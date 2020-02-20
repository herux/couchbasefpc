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
    //procedure TestUpsert;
  end;

implementation

procedure TTestCouchbase.TestConnect;
var
  lValue: String;
  cbRes: TCouchbaseResult;
begin
  if not cbCon.Connect then
       Fail('Connection to couchbase failed, error: ' + cbCon.LastErrorDesc);
end;

//procedure TTestCouchbase.TestUpsert;
//begin
//  WriteLn('cbCon: ', cbCon <> nil);
//  cbCon.Upsert('TestUpsert', '{"TestUpsert":"TestUpsert"}');
//end;

procedure TTestCouchbase.SetUp;
begin
  cbCon := TCouchbaseConnection.Create('couchbase://localhost/testbucket', 'usercb', 'sa_karep_mu');
end;

procedure TTestCouchbase.TearDown;
begin
  cbCon.Free;
  cbCon := Nil;
end;

initialization

  RegisterTest(TTestCouchbase);
end.

