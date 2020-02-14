unit testcouchbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, couchbase_api, couchbase_db;

type

  { TTestCouchbase }

  TTestCouchbase= class(TTestCase)
  private
    cbCon: TCouchbaseConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
    procedure TestUpsert;
  end;

implementation

procedure TTestCouchbase.TestConnect;
var
  lValue: String;
  cbRes: TCouchbaseResult;
begin
  if not cbCon.Connect('couchbase://localhost/testbucket', 'usercb', 'sa_karep_mu') then
       Fail('Connection to couchbase failed, error: ' + cbCon.LastErrorDesc);

  cbRes := cbCon.Upsert('test', '{"ok":"eko3"}');
  WriteLn('success: ', cbRes.Success);
  //cbCon.Get('afterTestConnect', lValue);
  //WriteLn(lValue);
end;

procedure TTestCouchbase.TestUpsert;
begin
  cbCon.Upsert('TestUpsert1', '{"TestUpsert":"TestUpsert"}');
end;

procedure TTestCouchbase.SetUp;
begin
  cbCon := TCouchbaseConnection.Create;
end;

procedure TTestCouchbase.TearDown;
begin
  cbCon.Free;
  cbCon := Nil;
end;

initialization

  RegisterTest(TTestCouchbase);
end.

