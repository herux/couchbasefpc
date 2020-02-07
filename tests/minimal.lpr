program minimal;

{$DEFINE OSX}

uses
   couchbase_api, couchbase_db;

var
  cbCon: TCouchbaseConnection;
begin
  cbCon := TCouchbaseConnection.Create;
  try
    cbCon.Connect('couchbase://localhost/testbucket', 'Administrator', 'sa_karep_mu');
  finally
    cbCon.Free;
  end;
end.

