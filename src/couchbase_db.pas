unit couchbase_db;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, couchbase_api, typinfo;

type
  TCouchbaseResponseFormat = (rfUnknown = 0, rfForeign = 1, rfJSON = 2,
                           rfRAW = 3, rfUTF8 = 4);
  TCouchbaseCRUDOperation = ( coUPSERT = LCB_UPSERT, coADD = LCB_ADD,
                          coREPLACE = LCB_REPLACE, coSET = LCB_SET,
                          coAPPEND = LCB_APPEND, coPREPEND = LCB_PREPEND );

  TCouchbaseCallbackType = ( ctDEFAULT = LCB_CALLBACK_DEFAULT, ctGET = LCB_CALLBACK_GET, ctSTORE = LCB_CALLBACK_STORE,
                         ctCOUNTER = LCB_CALLBACK_COUNTER, ctTOUCH = LCB_CALLBACK_TOUCH, ctREMOVE = LCB_CALLBACK_REMOVE,
                         ctUNLOCK = LCB_CALLBACK_UNLOCK, ctSTATS = LCB_CALLBACK_STATS, ctVERSIONS = LCB_CALLBACK_VERSIONS,
                         ctVERBOSITY = LCB_CALLBACK_VERBOSITY, ctFLUSH = LCB_CALLBACK_FLUSH, ctOBSERVE = LCB_CALLBACK_OBSERVE,
                         ctGETREPLICA = LCB_CALLBACK_GETREPLICA, ctENDURE = LCB_CALLBACK_ENDURE, ctHTTP = LCB_CALLBACK_HTTP,
                         ctCBFLUSH = LCB_CALLBACK_CBFLUSH, ctOBSEQNO = LCB_CALLBACK_OBSEQNO, ctSTOREDUR = LCB_CALLBACK_STOREDUR,
                         ctSDLOOKUP = LCB_CALLBACK_SDLOOKUP, ctSDMUTATE = LCB_CALLBACK_SDMUTATE, ct_MAX = LCB_CALLBACK__MAX);
  ECouchbaseException = class(Exception)
  end;


  { TCouchbaseResponse }

  TCouchbaseResponse = record
    Success: Boolean;
    Error: Integer;
    Key: String;
    Value: TBytes;
    Format: TCouchbaseResponseFormat;
    Flags: Integer;
    CAS: Integer;
    Operation: Integer;
    Counter: Integer;
  end;
  PCouchbaseResponse = ^TCouchbaseResponse;


  { TCouchbaseConnection }

  TCouchbaseConnection = class(TObject)
  private
    FConnected: Boolean;
    FLastErrorCode: lcb_error_t;
    FLastErrorDesc: String;
    FInstance: lcb_t;
    FLastResponse: TCouchbaseResponse;
    FOptions: lcb_create_st;
    //FCouchbaseCallbackProc: TCouchbaseCallbackProc;
    FValue: String;
    function IsSuccess(const ACallFuncResult: Lcb_error_t): Boolean;
    function Store(const AOperation: TCouchbaseCRUDOperation; const AKey: String; const AValue: String;
             const AFormat: TCouchbaseResponseFormat): boolean;
  protected
    procedure CallbackHandler(instance: lcb_t; callbackType: Integer; const resp: plcb_RESPBASE);
  public
    constructor Create(const AConnection: String; const AUsername: String = ''; const APassword: String = '');
    destructor Destroy; override;

    function Connect: Boolean;
    function Upsert(const AKey: String; const AValue: String): boolean;
    function Get(const AKey: String; out AValue: TBytes): boolean;
    function Add(const AKey: String; const AValue: String): boolean;
    function Append(const AKey: String; const AValue: String): boolean;
  published
    property LastErrorCode: Integer read FLastErrorCode;
    property LastErrorDesc: String read FLastErrorDesc;
    property Connected: Boolean read FConnected;

    property LastResponse: TCouchbaseResponse read FLastResponse;
    property Value: String read FValue write FValue;
  end;

procedure CallbackProc(instance: lcb_t; callbackType: Integer;
  const resp: plcb_RESPBASE); cdecl;

const
  SUCCESS = 'Success';

var
  Connection: TCouchbaseConnection;

implementation


procedure CallbackProc(instance: lcb_t; callbackType: Integer;
  const resp: plcb_RESPBASE); cdecl;
begin
  Connection.CallbackHandler(instance, callbackType, resp);
end;

{ TCouchbaseConnection }

function TCouchbaseConnection.IsSuccess(const ACallFuncResult: Lcb_error_t
  ): Boolean;
begin
  FLastErrorCode := ACallFuncResult;
  if FLastErrorCode = LCB_SUCCESS then begin
    FLastErrorDesc := SUCCESS;
    Result := True;
  end else begin
    FLastErrorDesc := String(lcb_strerror(@FInstance, FLastErrorCode));
    Result := False;
  end;
end;

function TCouchbaseConnection.Store(const AOperation: TCouchbaseCRUDOperation;
  const AKey: String; const AValue: String;
  const AFormat: TCouchbaseResponseFormat): boolean;
var
  Command: lcb_CMDSTORE;
begin
  FillChar(Command, SizeOf(Command), 0);
  Command.flags := lcb_U32(AFormat);
  Command.cmdbase.exptime := 0;
  Command.cmdbase.cas := 0;
  LCB_CMD_SET_KEY(Command.cmdbase, AKey, Length(AKey));
  LCB_CMD_SET_VALUE(Command, AValue, Length(AValue));
  Command.operation := lcb_storage_t(AOperation);
  if IsSuccess(lcb_store3(FInstance, nil, @Command)) then begin
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
  end;
end;

procedure TCouchbaseConnection.CallbackHandler(instance: lcb_t;
  callbackType: Integer; const resp: plcb_RESPBASE);
var
  vResp: lcb_RESPBASE;
  vCbRes: TCouchbaseResponse;
  PCbRes: PCouchbaseResponse;
  PRespGet: plcb_RESPGET;
  RespGet: lcb_RESPGET;
  val: String;
begin
  vResp := resp^;
  PCbRes:= vResp.cookie;
  vCbRes:= PCbRes^;
  vCbRes.Success:= vResp.rc = LCB_SUCCESS;
  SetLength(vCbRes.Key, vResp.nkey);
  Move(vResp.key^, vCbRes.Key[1], vResp.nkey);
  vCbRes.Flags := vResp.rflags;
  vCbRes.CAS := vResp.cas;
  PRespGet := plcb_RESPGET(resp);
  respGet := PRespGet^;
  vCbRes.Format := TCouchbaseResponseFormat(respGet.itmflags shr 24);
  SetLength(Val, respGet.nvalue);
  Move(respGet.value^, val[1], respGet.nvalue);
  WriteLn('CallbackProc           | ');
  WriteLn('=======================|');
  WriteLn('CbResult, ', PCbRes <> nil);
  WriteLn('callbackType: ', GetEnumName(typeInfo(TCouchbaseCallbackType), Ord(callbackType)));
  WriteLn('Key: ', vCbRes.Key);
  WriteLn('Version: ', vResp.version);
  WriteLn('Value: ', Val);
  //Connection.Value := value;
  WriteLn('Success: ', vCbRes.Success);
  WriteLn('Format: ', GetEnumName(typeInfo(TCouchbaseResponseFormat), Ord(vCbRes.Format)));
  WriteLn('-----------------------|');
  WriteLn('');
end;

constructor TCouchbaseConnection.Create(const AConnection: String;
  const AUsername: String; const APassword: String);
begin
  FInstance:= nil;
  //FCouchbaseCallbackProc:= @CallbackProc;
  FillChar(FOptions, SizeOf(FOptions), 0);
  FOptions.version := 3;
  FOptions.v3.connstr := PAnsiChar(AConnection);
  FOptions.v3.username := PAnsiChar(AUsername);
  FOptions.v3.passwd := PAnsiChar(APassword);
  if not IsSuccess(lcb_create(@FInstance, @FOptions)) then
     raise ECouchbaseException.Create('Error creating couchbase instance!.');
end;

destructor TCouchbaseConnection.Destroy;
begin
  if FInstance <> nil then
    FInstance := nil;
  inherited Destroy;
end;

function TCouchbaseConnection.Connect: Boolean;
var
  lValue: String;
  i: Integer;
begin
  result := false;
  if IsSuccess(lcb_connect(FInstance)) then begin
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
    Result := IsSuccess(lcb_get_bootstrap_status(FInstance));
    FConnected:= Result;
    if Result then begin
      lcb_install_callback3(FInstance, LCB_CALLBACK_GET, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_STORE, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_COUNTER, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_TOUCH, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_REMOVE, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_FLUSH, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_SDLOOKUP, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_SDMUTATE, @CallbackProc);
    end;
  end;
end;

function TCouchbaseConnection.Upsert(const AKey: String; const AValue: String
  ): boolean;
begin
  Result := Store(coSET, AKey, AValue, rfJSON);
end;

function TCouchbaseConnection.Get(const AKey: String; out AValue: TBytes
  ): boolean;
var
  Command: lcb_CMDGET;
  resp: TCouchbaseResponse;
  //PRes: PCouchbaseResponse;
  //PResBase: plcb_RESPBASE;
  ////resBase: lcb_RESPBASE;
  //ResGet: plcb_RESPGET;
  //value: String;
begin
  Result := False;
  //res.Init;
  FillChar(Command, SizeOf(Command), 0);
  LCB_CMD_SET_KEY(Command.cmdbase, AKey, Length(AKey));
  if IsSuccess(lcb_get3(FInstance, @resp, @Command)) then begin
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
    Result := IsSuccess(lcb_get_bootstrap_status(FInstance));

    WriteLn('Success get: ', resp.Key);
    WriteLn('Success callback: ', value);
    //WriteLn('Success: ', retVal^.status = LCB_SUCCESS);
    //SetLength(value, retVal^.nValue);
    //Move(retVal^.Value, value[1], retVal^.nValue);
    //WriteLn('value: ', Value);

    //PResBase := plcb_RESPBASE(@res);
    //res.Success:= PResBase^.rc = LCB_SUCCESS;
    //SetLength(res.Key, PResBase^.nkey);
    //Move(PResBase^.key^, res.Key[1], PResBase^.nkey);
    //res.Flags:= PResBase^.rflags;
    //res.CAS:= PResBase^.cas;
    //ResGet := plcb_RESPGET(PResBase);
    //res.Format := TCouchbaseResponseFormat(ResGet^.itmflags shr 24);
    //SetLength(value, ResGet^.nvalue);
    //Move(ResGet^.value^, value[1], ResGet^.nvalue);

    //WriteLn('res.Error: ', res.Error);
    //WriteLn('res.Format: ', res.Format);
    //WriteLn('res.Succes: ', res.Success);
    //WriteLn('res.Key: ', res.Key);
    //WriteLn('res.Value: ', Value);
    //WriteLn('res.CAS: ', res.CAS);
    //WriteLn('res.Counter: ', res.Counter);
    //WriteLn('res.Flags: ', res.Flags);
    //WriteLn('res.Key: ', res.Key);
    //AValue:= res.Value;
    Result := True;
  end;
end;

function TCouchbaseConnection.Add(const AKey: String; const AValue: String
  ): boolean;
begin
  Result := Store(coADD, AKey, AValue, rfJSON);
end;

function TCouchbaseConnection.Append(const AKey: String; const AValue: String
  ): boolean;
begin
  Result := Store(coAPPEND, AKey, AValue, rfJSON);
end;

end.

