program minimal;

{$DEFINE OSX}

uses
   couchbase_api, couchbase_db;

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

