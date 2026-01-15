{**
 * 最小化 WinSSL 测试 - 诊断启动问题
 *}

program test_winssl_minimal;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils;

begin
  WriteLn('Step 1: Program started');
  try
    WriteLn('Step 2: Basic test passed');
    WriteLn('SUCCESS');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
