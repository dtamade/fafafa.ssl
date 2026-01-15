{**
 * 测试 SyncObjs 模块
 *}

program test_syncobjs_only;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, SyncObjs;

var
  LCS: TCriticalSection;
begin
  WriteLn('Testing SyncObjs...');
  LCS := TCriticalSection.Create;
  try
    LCS.Enter;
    WriteLn('Critical section entered');
    LCS.Leave;
    WriteLn('Critical section left');
    WriteLn('SUCCESS');
  finally
    LCS.Free;
  end;
  Halt(0);
end.
