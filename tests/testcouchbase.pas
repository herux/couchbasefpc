unit testcouchbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, couchbase_api, couchbase_db;

type

  { TTestCouchbase }

  TTestCouchbase= class(TTestCase)
  strict private
    class var cbCon: TCouchbaseConnection;
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
  if not cbCon.Connect then
       Fail('Connection to couchbase failed, error: ' + cbCon.LastErrorDesc);
end;

procedure TTestCouchbase.TestUpsert;
begin
  cbCon.Upsert('TestUpsert', '{"TestUpsert":"TestUpsert"}');
end;

procedure TTestCouchbase.TestGet;
var
  outVal: String;
begin
  if not cbCon.Get('TestUpsert', outVal) then
     Fail('Error get function, for key ' + outVal);
end;

class destructor TTestCouchbase.Destroy;
begin
  cbCon.Free;
end;

class constructor TTestCouchbase.Create;
begin
  cbCon := TCouchbaseConnection.Create('couchbase://localhost/testbucket', 'usercb', 'sa_karep_mu');
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

