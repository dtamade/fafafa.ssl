program test_winssl_lib_catch;
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Windows;

var
  OldExitProc: Pointer;

procedure MyExitProc;
begin
  ExitProc := OldExitProc;
  if ExitCode <> 0 then
    WriteLn('Exit with code: ', ExitCode);
end;

begin
  OldExitProc := ExitProc;
  ExitProc := @MyExitProc;
  
  WriteLn('=== Before loading winssl.lib ===');
end.
