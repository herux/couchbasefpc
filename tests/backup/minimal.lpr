program minimal;

{$DEFINE OSX}

uses
   couchbase_api, couchbase_db, testcase1;

var
  cbCon: TCouchbaseConnection;
begin
  cbCon := TCouchbaseConnection.Create;
  try
    cbCon.Connect('couchbase://localhost/testbucket', 'Administrator', 'suckarep');
  finally
    cbCon.Free;
  end;
end.

