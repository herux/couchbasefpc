program minimal;

{$DEFINE OSX}

uses
   couchbase_api, couchbase_db;

var
  cbCon: TCouchbaseConnection;
begin
  cbCon := TCouchbaseConnection.Create('couchbase://localhost/testbucket', 'usercb', 'sa_karep_mu');
  try
    cbCon.Connect;
    cbCon.Upsert('testMinimalKey', '{"ok1":"yes1"}');
    //cbCon.Add('testMinimalKeyAdd', '{"ok2":"yes2"}');
    //cbCon.Append('testMinimalKeyAdd', '{"ok3":"yes3"}');
  finally
    cbCon.Free;
  end;
end.

