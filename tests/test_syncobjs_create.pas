{**
 * 诊断测试: 创建 SyncObjs 对象
 *}

program test_syncobjs_create;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  SyncObjs;

var
  LCS: TCriticalSection;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Creating TCriticalSection...');
  Flush(Output);
  
  try
    LCS := TCriticalSection.Create;
    WriteLn('Step 3: TCriticalSection created');
    LCS.Free;
  except
    on E: Exception do
      WriteLn('Step 3: Create FAILED: ', E.Message);
  end;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
