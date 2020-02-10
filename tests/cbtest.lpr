program cbtest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, testcouchbase;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='cbtest';
  Application.Run;
  Application.Free;
end.
