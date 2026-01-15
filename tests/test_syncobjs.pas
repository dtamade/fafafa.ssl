{**
 * Test SyncObjs import
 *}
program test_syncobjs;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  SyncObjs;

var
  CS: TCriticalSection;

begin
  WriteLn('Step 1: Program started');
  CS := TCriticalSection.Create;
  try
    WriteLn('Step 2: SyncObjs loaded successfully');
  finally
    CS.Free;
  end;
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
