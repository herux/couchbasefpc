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

  { TCouchbaseConnection }

  TCouchbaseConnection = class(TObject)
  private
    FConnected: Boolean;
    FLastErrorCode: lcb_error_t;
    FLastErrorDesc: String;
    FInstance: lcb_t;
    FOptions: lcb_create_st;
    FCallbackProc: Tlcb_RESPCALLBACK_proc;
    FCallbackBootstrap: Tlcb_bootstrap_callback_proc;
    function IsSuccess(const ACallFuncResult: Lcb_error_t): Boolean;
    function Store(const AOperation: TCouchbaseCRUDOperation; const AKey: String; const AValue: String;
             const AFormat: TCouchbaseResponseFormat): boolean;
  public
    constructor Create(const AConnection: String; const AUsername: String = ''; const APassword: String = '');
    destructor Destroy; override;

    function Connect: Boolean;
    function Upsert(const AKey: String; const AValue: String): boolean;
    function Get(const AKey: String; out AValue: String): boolean;
    function Add(const AKey: String; const AValue: String): boolean;
    function Append(const AKey: String; const AValue: String): boolean;
  published
    property LastErrorCode: Integer read FLastErrorCode;
    property LastErrorDesc: String read FLastErrorDesc;
    property Connected: Boolean read FConnected;
  end;

//procedure CallbackProc(instance: lcb_t; callbackType: Integer;
//  const resp: plcb_RESPBASE); cdecl;

const
  SUCCESS = 'Success';

implementation

//procedure CallbackProc(instance: lcb_t; callbackType: Integer;
//  const resp: plcb_RESPBASE); cdecl;
//var
//  vResp: lcb_RESPBASE;
//  vCbRes: TCouchbaseResult;
//  PCbRes: PCouchbaseResult;
//begin
//  vResp := resp^;
//  PCbRes:= vResp.cookie;
//  vCbRes:= PCbRes^;
//  WriteLn('CallbackProc           | ');
//  WriteLn('=======================|');
//  WriteLn('callbackType: ', GetEnumName(typeInfo(TCouchbaseCallbackType), Ord(callbackType)));
//  WriteLn('Version: ', vResp.version);
//  WriteLn('Value: ', vCbRes.Value);
//  WriteLn('Success: ', vCbRes.Success);
//  WriteLn('Format: ', GetEnumName(typeInfo(TCouchbaseResponseFormat), Ord(vCbRes.Format)));
//  WriteLn('-----------------------|');
//  WriteLn('');
//end;

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
   WriteLn(' IsSucces?: ', Result, ' FLastErrorDesc: ', FLastErrorDesc);
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

constructor TCouchbaseConnection.Create(const AConnection: String;
  const AUsername: String; const APassword: String);
begin
  FInstance:= nil;
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
      //lcb_install_callback3(FInstance, LCB_CALLBACK_GET, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_STORE, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_COUNTER, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_TOUCH, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_REMOVE, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_FLUSH, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_GET, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_SDLOOKUP, @CallbackProc);
      //lcb_install_callback3(FInstance, LCB_CALLBACK_SDMUTATE, @CallbackProc);

      //for i:= 0 to 4 do begin
         //testing purpose
        //Store(coSET, 'key'+IntToStr(i), '{"test'+IntToStr(i)+'":"valueTest'+IntToStr(i)+'"}', rfJSON);
        //res := Upsert('keyupsert_'+IntToStr(i), '{"testUpsert":"valueUpsert"}');
        //WriteLn('res.Value: ', res.Value);
        //Get('keyupsert', lValue);
        //WriteLn('value of keyupsert: ', lValue);
      //end;
    end;
  end;
end;

function TCouchbaseConnection.Upsert(const AKey: String; const AValue: String
  ): boolean;
begin
  Result := Store(coSET, AKey, AValue, rfJSON);
end;

function TCouchbaseConnection.Get(const AKey: String; out AValue: String
  ): boolean;
var
  Command: lcb_CMDGET;
  res: TBytes;
begin SetLength(res, 1000);
  Result := False;
  FillChar(Command, SizeOf(Command), 0);
  LCB_CMD_SET_KEY(Command.cmdbase, AKey, Length(AKey));
  if IsSuccess(lcb_get3(FInstance, @res, @Command)) then begin
    lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
    AValue:= chr(res[0]);
    WriteLn('res: ', AValue);
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

