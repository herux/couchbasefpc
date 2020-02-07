unit couchbase_db;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, couchbase_api;

type

  { TCouchbaseConnection }

  TCouchbaseConnection = class(TObject)
  private
    FLastErrorCode: lcb_error_t;
    FLastErrorDesc: String;
    FInstance: lcb_t;
    FOptions: lcb_create_st;
    function IsSuccess(const ACallFuncResult: Lcb_error_t): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AConnection: String; const AUsername: String = ''; const APassword: String = ''): Boolean;
  public
    property LastErrorCode: Integer read FLastErrorCode;
  end;

procedure CallbackProc(instance: lcb_t; cbtype: Integer;
  const resp: plcb_RESPBASE); cdecl;

implementation

procedure CallbackProc(instance: lcb_t; cbtype: Integer;
  const resp: plcb_RESPBASE); cdecl;
begin
   WriteLn('cbtype: ', cbtype);
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
  scmd: lcb_CMDSTORE;
  gcmd: lcb_CMDGET;
begin
  result := false;
  FillChar(FOptions, SizeOf(FOptions), 0);
  FOptions.version := 3;
  FOptions.v3.connstr := PAnsiChar(AConnection);
  FOptions.v3.username := PAnsiChar(AUsername);
  FOptions.v3.passwd := PAnsiChar(APassword);
  WriteLn('couchbase handle: ', CouchbaseHandle);
  WriteLn('lcb_create Result: ', BoolToStr(lcb_create <> nil, true));

  if IsSuccess(lcb_create(@FInstance, @FOptions)) then
    if IsSuccess(lcb_connect(FInstance)) then begin
      lcb_wait3(FInstance, LCB_WAIT_NOCHECK);
      Result := IsSuccess(lcb_get_bootstrap_status(FInstance));
      WriteLn('lcb_get_bootstrap_status: ', Result, ' Error: ', FLastErrorDesc);
      if Result then begin
        lcb_install_callback3(FInstance, LCB_CALLBACK_GET, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_STORE, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_COUNTER, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_TOUCH, @CallbackProc);
        lcb_install_callback3(FInstance, LCB_CALLBACK_REMOVE, @CallbackProc);

        // testing purpose
        // key
        scmd.cmdbase.key.&type := LCB_KV_COPY;
        scmd.cmdbase.key.contig.bytes := PAnsiChar('key');
        scmd.cmdbase.key.contig.nbytes := Length('key');
        // value
        scmd.value.vtype := LCB_KV_COPY;
        scmd.value.contig.bytes := PAnsiChar('value');
        scmd.value.contig.nbytes := Length('value');
        scmd.operation:= LCB_SET;
        // set store
        if IsSuccess(lcb_store3(FInstance, nil, @scmd)) then
           lcb_wait3(FInstance, LCB_WAIT_NOCHECK);

        gcmd.cmdbase.key.&type:= LCB_KV_COPY;
        gcmd.cmdbase.key.contig.bytes := PAnsiChar('key');
        scmd.cmdbase.key.contig.nbytes := Length('key');
        if IsSuccess(lcb_get3(FInstance, nil, @gcmd)) then
           lcb_wait3(FInstance, LCB_WAIT_NOCHECK);

        //{ flush/stats callbacks }
        //lcb_install_callback3(FInstance, LCB_CALLBACK_FLUSH, ResponseFlushCallback);
        //lcb_install_callback3(FInstance, LCB_CALLBACK_STATS, ResponseStatsCallback);
        //
        //{ json subdoc callbacks }
        //lcb_install_callback3(FInstance, LCB_CALLBACK_SDLOOKUP, ResponseSubDocCallback);
        //lcb_install_callback3(FInstance, LCB_CALLBACK_SDMUTATE, ResponseSubDocCallback);
      end;
    end;
end;

end.

