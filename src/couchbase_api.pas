unit couchbase_api;

interface

uses
    dynlibs, sysutils;

const
  LCB_SUCCESS = $00;

type
  lcb_t = Pointer{plcb_st};
  plcb_t = ^lcb_t;
  Lcb_type_t = Integer;
  lcb_create_st0 = record
    host: PAnsiChar;
    user: PAnsiChar;
    passwd: PAnsiChar;
    bucket: PAnsiChar;
    io: Pointer{plcb_io_opt_st};
  end;

  lcb_create_st1 = record
    host: PAnsiChar;
    user: PAnsiChar;
    passwd: PAnsiChar;
    bucket: PAnsiChar;
    io: Pointer{plcb_io_opt_st};
    &type: lcb_type_t;
  end;

  lcb_create_st2 = record
    host: PAnsiChar;
    user: PAnsiChar;
    passwd: PAnsiChar;
    bucket: PAnsiChar;
    io: Pointer{plcb_io_opt_st};
    &type: lcb_type_t;
    mchosts: PAnsiChar;
    transports: Pointer{plcb_config_transport_t};
  end;

  lcb_create_st3 = record
    connstr: PAnsiChar;
    username: PAnsiChar;
    passwd: PAnsiChar;
    _pad_bucket: Pointer;
    io: Pointer{plcb_io_opt_st};
    &type: Lcb_type_t;
  end;
  plcb_create_st3 = ^lcb_create_st3;

  lcb_create_st = record
    version: Integer;
    case Integer of
      0: (v0: lcb_create_st0);
      1: (v1: lcb_create_st1);
      2: (v2: lcb_create_st2);
      3: (v3: lcb_create_st3);
  end;
  plcb_create_st = ^lcb_create_st;
  Lcb_error_t = Integer;
  plcb_error_t = ^lcb_error_t;

  Tlcb_create_func = function(instance: plcb_t; const options: plcb_create_st): Lcb_error_t; cdecl;
  Tlcb_destroy_func = procedure(instance: lcb_t); cdecl;
  Tlcb_connect_func= function(instance: lcb_t): Lcb_error_t; cdecl;
  Tlcb_bootstrap_callback_func = procedure(instance: lcb_t; err: Lcb_error_t); cdecl;
  Tlcb_set_bootstrap_callback_func = function(instance: lcb_t; callback: Tlcb_bootstrap_callback_func): Tlcb_bootstrap_callback_func; cdecl;
  Tlcb_get_bootstrap_status_func = function(instance: lcb_t): Lcb_error_t; cdecl;

var
  lcb_create: Tlcb_create_func;
  lcb_destroy: Tlcb_destroy_func;
  lcb_connect: Tlcb_connect_func;
  lcb_set_bootstrap_callback: Tlcb_set_bootstrap_callback_func;
  lcb_get_bootstrap_status: Tlcb_get_bootstrap_status_func;


  CouchbaseHandle: HMODULE;

implementation

const
  {$IFDEF darwin}
  COUCHBASE_LIBRARY = 'libcouchbase.dylib';
  {$ENDIF}
  //{$IFDEF windows}
  //COUCHBASE_LIBRARY = 'libcouchbase.dll';
  //{$ENDIF}
  //{$IFDEF linux}
  //COUCHBASE_LIBRARY = 'libcouchbase.so';
  //{$ENDIF}

function GetProcedure(const AProcName: String): Pointer;
begin
  Result := GetProcAddress(CouchbaseHandle, PChar(AProcName));
  if (Result = nil) then
    raise Exception.CreateFmt('%s is not found', [AProcName]);
end;

procedure LoadCouchbase;
var
  p: Pointer;
begin
  if (CouchbaseHandle <> 0) then Exit;

  CouchbaseHandle := LoadLibrary(COUCHBASE_LIBRARY);
  if (CouchbaseHandle = 0) then begin
    raise Exception.CreateFmt('Load %s failed', [COUCHBASE_LIBRARY]);
    Exit;
  end;

  lcb_create := Tlcb_create_func(GetProcedure('lcb_create'));
  lcb_connect := Tlcb_connect_func(GetProcedure('lcb_connect'));
  lcb_set_bootstrap_callback := Tlcb_set_bootstrap_callback_func(GetProcedure('lcb_set_bootstrap_callback'));
  lcb_get_bootstrap_status := Tlcb_get_bootstrap_status_func(GetProcedure('lcb_get_bootstrap_status'));
end;

procedure UnloadCouchbase;
begin
  if (CouchbaseHandle = 0) then Exit;
  FreeLibrary(CouchbaseHandle);
  CouchbaseHandle := 0;
end;

initialization
    LoadCouchbase;

finalization
    UnloadCouchbase;

end.
