unit couchbase_db;

{$mode objfpc}{$H+}

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

  TCouchbaseResult = record
    Success: Boolean;
    Error: Integer;
    Key: String;
    Value: String;
    Format: TCouchbaseResponseFormat;
    Flags: Integer;
    CAS: Integer;
    Operation: Integer;
    Counter: Integer;
  end;
  PCouchbaseResult = ^TCouchbaseResult;

  { TCouchbaseConnection }

  TCouchbaseConnection = class(TObject)
  private
    FLastErrorCode: lcb_error_t;
    FLastErrorDesc: String;
    FInstance: lcb_t;
    FOptions: lcb_create_st;
    function IsSuccess(const ACallFuncResult: Lcb_error_t): Boolean;
    function Store(const AOperation: TCouchbaseCRUDOperation; const AKey: String; const AValue: String;
             const AFormat: TCouchbaseResponseFormat): TCouchbaseResult;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AConnection: String; const AUsername: String = ''; const APassword: String = ''): Boolean;
    function Upsert(const AKey: String; const AValue: String): TCouchbaseResult;
    function Get(const AKey: String; out AValue: String): TCouchbaseResult;
  published
    property LastErrorCode: Integer read FLastErrorCode;
    property LastErrorDesc: String read FLastErrorDesc;
  end;

procedure CallbackProc(instance: lcb_t; callbackType: Integer;
  const resp: plcb_RESPBASE); cdecl;

implementation

procedure CallbackProc(instance: lcb_t; callbackType: Integer;
  const resp: plcb_RESPBASE); cdecl;
var
  vResp: lcb_RESPBASE;
  vCbRes: TCouchbaseResult;
  PCbRes: PCouchbaseResult;
begin
  vResp := resp^;
  PCbRes:= vResp.cookie;
  vCbRes:= PCbRes^;
  WriteLn('callbackType: ', GetEnumName(typeInfo(TCouchbaseCallbackType), Ord(callbackType)));
  WriteLn('Version: ', vResp.version);
  WriteLn('Value: ', vCbRes.Value);
  WriteLn('Success: ', vCbRes.Success);
  WriteLn('Format: ', GetEnumName(typeInfo(TCouchbaseResponseFormat), Ord(vCbRes.Format)));
end;

{ TCouchbaseConnection }

function TCouchbaseConnection.IsSuccess(const ACallFuncResult: Lcb_error_t
  ): Boolean;
begin
  FLastErrorCode := ACallFuncResult;
  if FLastErrorCode = LCB_SUCCESS then begin
    FLastErrorDesc := 'Success';
    Result := True;
  end else begin
    FLastErrorDesc := String(lcb_strerror(nil, FLastErrorCode));
    Result := False;
  end;
end;

function TCouchbaseConnection.Store(const AOperation: TCouchbaseCRUDOperation;
  const AKey: String; const AValue: String;
  const AFormat: TCouchbaseResponseFormat): TCouchbaseResult;
var
  Command: lcb_CMDSTORE;
begin
  FillChar(Command, SizeOf(Command), 0);
  Command.flags := lcb_U32(AFormat);
  Command.cmdbase.exptime := 60;
  //Command.cmdbase.cas :=
  LCB_CMD_SET_KEY(Command.cmdbase, AKey, Length(AKey));
  LCB_CMD_SET_VALUE(Command, AValue, Length(AValue));
  Command.operation := lcb_storage_t(AOperation);
  if IsSuccess(lcb_store3(FInstance, @Result, @Command)) then begin
    WriteLn('Result: ', Result.Value, Result.Success);
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
  end;
end;

constructor TCouchbaseConnection.Create;
begin
  FInstance:= nil;
end;

destructor TCouchbaseConnection.Destroy;
begin
  if FInstance <> nil then
    FInstance := nil;
  inherited Destroy;
end;

function TCouchbaseConnection.Connect(const AConnection: String;
  const AUsername: String; const APassword: String): Boolean;
var
   lValue: String;
begin
  result := false;
  FillChar(FOptions, SizeOf(FOptions), 0);
  FOptions.version := 3;
  FOptions.v3.connstr := PAnsiChar(AConnection);
  FOptions.v3.username := PAnsiChar(AUsername);
  FOptions.v3.passwd := PAnsiChar(APassword);

  if IsSuccess(lcb_create(@FInstance, @FOptions)) then
    if IsSuccess(lcb_connect(FInstance)) then begin
      lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
      Result := IsSuccess(lcb_get_bootstrap_status(FInstance));
      if Result then begin
        lcb_install_callback3(FInstance, LCB_CALLBACK_GET, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_STORE, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_COUNTER, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_TOUCH, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_REMOVE, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_FLUSH, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_STATS, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_SDLOOKUP, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_SDMUTATE, @CallbackProc);
        // testing purpose
        //Store(coSET, 'key', '{"test":"valueTest4"}', rfJSON);
        Upsert('keyupsert', '{"testUpsert":"valueUpsert"}');
        Get('keyupsert', lValue);
        WriteLn('value of keyupsert: ', lValue);
      end;
    end;
end;

function TCouchbaseConnection.Upsert(const AKey: String; const AValue: String
  ): TCouchbaseResult;
begin
  WriteLn('Finstance: ', FInstance <> nil);
  Result := Store(coSET, AKey, AValue, rfJSON);
end;

function TCouchbaseConnection.Get(const AKey: String; out AValue: String
  ): TCouchbaseResult;
var
  Command: lcb_CMDGET;
begin
  FillChar(Command, SizeOf(Command), 0);
  WriteLn('1');
  LCB_CMD_SET_KEY(Command.cmdbase, AKey, Length(AKey));
  WriteLn('2 ');
  if IsSuccess(lcb_get3(FInstance, nil, @Command)) then
  begin
    WriteLn('3');
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
    WriteLn('4');
    if Result.Success then
      AValue := Result.Value;
    WriteLn('5');
  end;
end;

end.

