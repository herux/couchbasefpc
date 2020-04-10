unit couchbase_api;

interface

uses
    dynlibs, sysutils;

const
  { KV buffer types }
  LCB_KV_COPY = 0;
  LCB_KV_CONTIG = 1;
  LCB_KV_IOV = 2;
  LCB_KV_VBID = 3;
  LCB_KV_IOVCOPY = 4;

  LCB_SUCCESS = $00;

  { Operations }
  LCB_UPSERT = 0;
  LCB_ADD = 1;
  LCB_REPLACE = 2;
  LCB_SET = 3;
  LCB_APPEND = 4;
  LCB_PREPEND = 5;

  LCB_WAIT_DEFAULT = 0;
  LCB_WAIT_NOCHECK = 1;

  LCB_CALLBACK_DEFAULT = 0;
  LCB_CALLBACK_GET = LCB_CALLBACK_DEFAULT + 1;
  LCB_CALLBACK_STORE = LCB_CALLBACK_GET + 1;
  LCB_CALLBACK_COUNTER = LCB_CALLBACK_STORE + 1;
  LCB_CALLBACK_TOUCH = LCB_CALLBACK_COUNTER + 1;
  LCB_CALLBACK_REMOVE = LCB_CALLBACK_TOUCH + 1;
  LCB_CALLBACK_UNLOCK = LCB_CALLBACK_REMOVE + 1;
  LCB_CALLBACK_STATS = LCB_CALLBACK_UNLOCK + 1;
  LCB_CALLBACK_VERSIONS = LCB_CALLBACK_STATS + 1;
  LCB_CALLBACK_VERBOSITY = LCB_CALLBACK_VERSIONS + 1;
  LCB_CALLBACK_FLUSH = LCB_CALLBACK_VERBOSITY + 1;
  LCB_CALLBACK_OBSERVE = LCB_CALLBACK_FLUSH + 1;
  LCB_CALLBACK_GETREPLICA = LCB_CALLBACK_OBSERVE + 1;
  LCB_CALLBACK_ENDURE = LCB_CALLBACK_GETREPLICA + 1;
  LCB_CALLBACK_HTTP = LCB_CALLBACK_ENDURE + 1;
  LCB_CALLBACK_CBFLUSH = LCB_CALLBACK_HTTP + 1;
  LCB_CALLBACK_OBSEQNO = LCB_CALLBACK_CBFLUSH + 1;
  LCB_CALLBACK_STOREDUR = LCB_CALLBACK_OBSEQNO + 1;
  LCB_CALLBACK_SDLOOKUP = LCB_CALLBACK_STOREDUR + 1;
  LCB_CALLBACK_SDMUTATE = LCB_CALLBACK_SDLOOKUP + 1;
  LCB_CALLBACK__MAX = LCB_CALLBACK_SDMUTATE + 1;


type
  lcb_t = Pointer;
  plcb_t = ^lcb_t;
  Lcb_type_t = Integer;
  lcb_create_st0 = record
    host: PAnsiChar;
    user: PAnsiChar;
    passwd: PAnsiChar;
    bucket: PAnsiChar;
    io: Pointer;
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

  lcb_error = NativeInt;

  size_t = NativeUInt;
  ssize_t = NativeInt;
  time_t = NativeInt;

  lcb_int64_t = int64;
  lcb_int32_t = int32;
  lcb_size_t = size_t;
  lcb_ssize_t = ssize_t;
  lcb_vbucket_t = uint16;
  lcb_uint8_t = uint8;
  lcb_uint16_t = uint16;
  lcb_uint32_t = uint32;
  lcb_cas_t = uint64;
  lcb_uint64_t = uint64;
  lcb_time_t = time_t;

  lcb_S64 = lcb_int64_t;
  lcb_U64 = lcb_uint64_t;
  lcb_U32 = lcb_uint32_t;
  lcb_S32 = lcb_int32_t;
  lcb_U16 = lcb_uint16_t;
  lcb_U8 = lcb_uint8_t;
  lcb_SECS = lcb_time_t;
  plcb_U16 = ^lcb_U16;
  plcb_U32 = ^lcb_U32;

  lcb_datatype_t = lcb_U8;
  lcb_size = size_t;
  lcb_storage_t = Integer;
  lcb_cas = lcb_cas_t;

  lcb_RESPBASE = record
    cookie: Pointer;
    key: Pointer;
    nkey: lcb_size;
    cas: lcb_cas;
    rc: lcb_error_t;
    version: lcb_U16;
    rflags: lcb_U16;
  end;
  plcb_RESPBASE = ^lcb_RESPBASE;

  lcb_CONTIGBUF = record
    bytes: Pointer;
    nbytes: lcb_size_t;
  end;

  lcb_KVBUFTYPE = Integer;

  lcb_KEYBUF = record
    &type: lcb_KVBUFTYPE;
    contig: lcb_CONTIGBUF;
  end;
  plcb_KEYBUF = ^lcb_KEYBUF;

  lcb_FRAGBUF = record
    iov: Pointer{lcb_IOV};
    niov: UInt32;
    total_length: UInt32;
  end;

  lcb_VALBUF = record
    vtype: lcb_KVBUFTYPE;
    case Integer of
      0: (contig: lcb_CONTIGBUF);
      1: (multi: lcb_FRAGBUF);
  end;

  lcb_CMDBASE = record
    cmdflags: lcb_U32;
    exptime: lcb_U32;
    cas: lcb_U64;
    key: lcb_KEYBUF;
    _hashkey: lcb_KEYBUF;
  end;
  plcb_CMDBASE = ^lcb_CMDBASE;

  lcb_CMDSTORE = record
    cmdbase: lcb_CMDBASE;
    value: lcb_VALBUF;
    flags: lcb_U32;
    datatype: lcb_datatype_t;
    operation: lcb_storage_t;
  end;
  plcb_CMDSTORE = ^lcb_CMDSTORE;

  lcb_CMDGET = record
    cmdbase: lcb_CMDBASE;
    lock: Integer;
  end;
  plcb_CMDGET = ^lcb_CMDGET;

  lcb_RESPSTORE = record
    respbase: lcb_RESPBASE;
    op: lcb_storage_t;
  end;
  plcb_RESPSTORE = ^lcb_RESPSTORE;

  lcb_RESPGET = record
    respbase: lcb_RESPBASE;
    value: Pointer;
    nvalue: lcb_size;
    bufh: Pointer;
    datatype: lcb_datatype_t;
    itmflags: lcb_U32;
  end;
  plcb_RESPGET = ^lcb_RESPGET;

  lcb_RESPCOUNTER = record
    respbase: lcb_RESPBASE;
    value: lcb_U64;
  end;
  plcb_RESPCOUNTER = ^lcb_RESPCOUNTER;

  lcb_RESPSERVERFIELDS = record
    server: PAnsiChar;
  end;

  lcb_RESPSERVERBASE = packed record
    respbase: lcb_RESPBASE;
    respserverfields: lcb_RESPSERVERFIELDS;
  end;
  plcb_RESPSERVERBASE = ^lcb_RESPSERVERBASE;

  lcb_RESPFLUSH = record
    respserverbase: lcb_RESPSERVERBASE;
  end;
  plcb_RESPFLUSH = ^lcb_RESPFLUSH;

  lcb_RESPSTATS = record
    respserverbase: lcb_RESPSERVERBASE;
    value: PAnsiChar;
    nvalue: lcb_SIZE;
  end;
  plcb_RESPSTATS = ^lcb_RESPSTATS;

  Tlcb_create_func = function(instance: plcb_t; const options: plcb_create_st): Lcb_error_t; cdecl;
  Tlcb_destroy_proc = procedure(instance: lcb_t); cdecl;
  Tlcb_connect_func= function(instance: lcb_t): Lcb_error_t; cdecl;
  Tlcb_bootstrap_callback_proc = procedure(instance: lcb_t; err: Lcb_error_t); cdecl;
  Tlcb_set_bootstrap_callback_func = function(instance: lcb_t; callback: Tlcb_bootstrap_callback_proc): Tlcb_bootstrap_callback_proc; cdecl;
  Tlcb_get_bootstrap_status_func = function(instance: lcb_t): Lcb_error_t; cdecl;
  Tlcb_strerror_func = function(instance: lcb_t; error: lcb_error_t): PAnsiChar; cdecl;
  Tlcb_wait3_proc = procedure(instance: lcb_t; flags: Integer); cdecl;
  Tlcb_RESPCALLBACK_proc = procedure(instance: lcb_t; cbtype: Integer; const resp: plcb_RESPBASE); cdecl;
  Tlcb_install_callback3_func = function(instance: lcb_t; cbtype: Integer; cb: Tlcb_RESPCALLBACK_proc): Tlcb_RESPCALLBACK_proc; cdecl;
  Tlcb_set_get_callback_proc = procedure(instance: lcb_t; cb: Tlcb_RESPCALLBACK_proc); cdecl;
  // crud
  Tlcb_store3_func = function(instance: lcb_t; const cookie: Pointer; const cmd: plcb_CMDSTORE): Lcb_error_t; cdecl;
  Tlcb_get3_func = function(instance: lcb_t; const cookie: Pointer; const cmd: plcb_CMDGET): Lcb_error_t; cdecl;



var
  lcb_create: Tlcb_create_func;
  lcb_destroy: Tlcb_destroy_proc;
  lcb_connect: Tlcb_connect_func;
  lcb_set_bootstrap_callback: Tlcb_set_bootstrap_callback_func;
  lcb_get_bootstrap_status: Tlcb_get_bootstrap_status_func;
  lcb_strerror: Tlcb_strerror_func;
  lcb_wait3: Tlcb_wait3_proc;
  lcb_install_callback3: Tlcb_install_callback3_func;
  lcb_set_get_callback: Tlcb_set_get_callback_proc;
  lcb_store3: Tlcb_store3_func;
  lcb_get3: Tlcb_get3_func;

  CouchbaseHandle: {$IFDEF DARWIN} TLibHandle {$ELSE} THandle {$ENDIF} = 0;

procedure LCB_CMD_SET_KEY(var cmdbase: lcb_CMDBASE; AKey: String; Keylen: Integer);
procedure LCB_CMD_SET_VALUE(var scmd: lcb_CMDSTORE; AValue: String; Valuelen: Integer);

implementation

const
  {$IFDEF DARWIN}
  COUCHBASE_LIBRARY = 'libcouchbase.dylib';
  {$ENDIF}
  {$IFDEF WINDOWS}
  COUCHBASE_LIBRARY = 'libcouchbase.dll';
  {$ENDIF}
  {$IFDEF linux}
  COUCHBASE_LIBRARY = 'libcouchbase.so';
  {$ENDIF}


function LoadCouchbaseFunc(const AProcName: String): Pointer;
begin
   {$IFDEF UNIX}
  Result := GetProcAddress(CouchbaseHandle, AnsiString(AProcName));
   {$ELSE}
   Result := {Windows.}GetProcAddress(CouchbaseHandle, PChar(AProcName));
   {$ENDIF}
  if (Result = nil) then
    raise Exception.CreateFmt('method %s is not found', [AProcName]);
end;

procedure LoadCouchbase;
begin
  if (CouchbaseHandle = 0) then
    CouchbaseHandle := LoadLibrary(PAnsiChar(COUCHBASE_LIBRARY));

  if (CouchbaseHandle = 0) then begin
    raise Exception.CreateFmt('Load %s failed', [COUCHBASE_LIBRARY]);
    Exit;
  end;

  lcb_create := Tlcb_create_func(LoadCouchbaseFunc('lcb_create'));
  lcb_connect := Tlcb_connect_func(LoadCouchbaseFunc('lcb_connect'));
  lcb_set_bootstrap_callback := Tlcb_set_bootstrap_callback_func(LoadCouchbaseFunc('lcb_set_bootstrap_callback'));
  lcb_get_bootstrap_status := Tlcb_get_bootstrap_status_func(LoadCouchbaseFunc('lcb_get_bootstrap_status'));
  lcb_wait3 := Tlcb_wait3_proc(LoadCouchbaseFunc('lcb_wait3'));
  lcb_install_callback3 := Tlcb_install_callback3_func(LoadCouchbaseFunc('lcb_install_callback3'));
  lcb_set_get_callback := Tlcb_set_get_callback_proc(LoadCouchbaseFunc('lcb_set_get_callback'));
  // crud
  lcb_store3 := Tlcb_store3_func(LoadCouchbaseFunc('lcb_store3'));
  lcb_get3 := Tlcb_get3_func(LoadCouchbaseFunc('lcb_get3'));
  // error function
  lcb_strerror :=  Tlcb_strerror_func(LoadCouchbaseFunc('lcb_strerror'));
end;

procedure UnloadCouchbase;
begin
  if (CouchbaseHandle = 0) then Exit;
  FreeLibrary(CouchbaseHandle);
  CouchbaseHandle := 0;
end;

procedure LCB_CMD_SET_KEY(var cmdbase: lcb_CMDBASE; AKey: String;
  Keylen: Integer);
begin
  cmdbase.key.&type := LCB_KV_COPY;
  cmdbase.key.contig.bytes := PAnsiChar(AKey);
  cmdbase.key.contig.nbytes := Keylen;
end;

procedure LCB_CMD_SET_VALUE(var scmd: lcb_CMDSTORE; AValue: String;
  Valuelen: Integer);
begin
  scmd.value.vtype := LCB_KV_COPY;
  scmd.value.contig.bytes := PAnsiChar(AValue);
  scmd.value.contig.nbytes := Valuelen;
end;

initialization
    LoadCouchbase;

finalization
    UnloadCouchbase;

end.
