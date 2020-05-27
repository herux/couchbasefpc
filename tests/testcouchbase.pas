unit testcouchbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, couchbase_api, couchbase_db;

type

  { TTestCouchbase }

  TTestCouchbase= class(TTestCase)
  strict private
    //class var cbCon: TCouchbaseConnection;
    class var FExampleJson: String;
    class destructor Destroy;
  public
    class constructor Create;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
    procedure TestUpsert;
    procedure TestGet;
  end;

implementation

procedure TTestCouchbase.TestConnect;
begin
  if not Connection.Connect then
       Fail('Connection to couchbase failed, error: ' + Connection.LastErrorDesc);
end;

procedure TTestCouchbase.TestUpsert;
begin
  Connection.Upsert('TestUpsert', '{"TestUpsert":"TestUpsert"}');
end;

procedure TTestCouchbase.TestGet;
var
  outVal: String;
  s: String;
begin
  if not Connection.Get('TestUpsert', outVal) then
     Fail('Error get function, for key ');
end;

class destructor TTestCouchbase.Destroy;
begin
  Connection.Free;
end;

class constructor TTestCouchbase.Create;
begin
  Connection := TCouchbaseConnection.Create('couchbase://localhost/testbucket', 'usercb', 'sa_karep_mu');
end;

procedure TTestCouchbase.SetUp;
begin

end;

procedure TTestCouchbase.TearDown;
begin

end;

initialization

  RegisterTest(TTestCouchbase);
end.

