unit couchbase_db;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, couchbase_api;

type

  { TCouchbaseConnection }

  TCouchbaseConnection = class(TObject)
  private
    FLastErrorCode: Integer;
    FInstance: lcb_t;
    FOptions: lcb_create_st;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AConnection: String; const AUsername: String = ''; const APassword: String = ''): Boolean;
  public
    property LastErrorCode: Integer read FLastErrorCode;
  end;

implementation

{ TCouchbaseConnection }

constructor TCouchbaseConnection.Create;
begin
  FInstance:= nil;
end;

destructor TCouchbaseConnection.Destroy;
begin
  if FInstance <> nil then
  begin
    lcb_destroy(FInstance);
    FInstance := nil;
  end;
  inherited Destroy;
end;

function TCouchbaseConnection.Connect(const AConnection: String;
  const AUsername: String; const APassword: String): Boolean;
begin
  FillChar(FOptions, SizeOf(FOptions), 0);
  FOptions.version := 3;
  FOptions.v3.connstr := PAnsiChar(AConnection);
  FOptions.v3.username := PAnsiChar(AUsername);
  FOptions.v3.passwd := PAnsiChar(APassword);
  //FLastErrorCode := lcb_create(@FInstance, @FOptions);
  //if FLastErrorCode = LCB_SUCCESS then begin
  //   WriteLn('Create couchbase connection instance success');
  //end else begin
  //   WriteLn('Create couchbase connection instance Error');
  //end;
end;

end.

