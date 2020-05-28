unit testcouchbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, couchbase_api, couchbase_db;

type

  { TTestCouchbase }

  TTestCouchbase= class(TTestCase)
  strict private
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
    procedure TestAdd;
    //procedure TestAppend;
    //procedure TestPrepend;
    //procedure TestReplace;
    //procedure TestRemove;
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

procedure TTestCouchbase.TestAdd;
begin
  if not Connection.Add('TestAdd', '{"TestAdd":"TestAdd"}') then
     Fail('Error add function, ' +  Connection.LastErrorDesc);
end;

//procedure TTestCouchbase.TestAppend;
//begin
//  if not Connection.Append('TestAppend', '{"TestAppend":"TestAppend"}') then
//     Fail('Error append function, for key ');
//end;

//procedure TTestCouchbase.TestPrepend;
//begin
//  if not Connection.Prepend('TestPrepend', '{"TestPrepend":"TestPrepend"}') then
//     Fail('Error prepend function, for key ');
//end;
//
//procedure TTestCouchbase.TestReplace;
//begin
//  if not Connection.Remove('TestPrepend') then
//     Fail('Error remove function, for key ');
//end;
//
//procedure TTestCouchbase.TestRemove;
//begin
//
//end;

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

