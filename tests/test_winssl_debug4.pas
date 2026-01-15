{**
 * 详细诊断测试 - 捕获初始化异常
 *}

program test_winssl_debug4;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory;

var
  LAvailable: TSSLLibraryTypes;
begin
  WriteLn('Debug: Starting test...');
  try
    WriteLn('Debug: Checking available libraries...');
    LAvailable := TSSLFactory.GetAvailableLibraries;
    WriteLn('Debug: Got available libraries');
    WriteLn('SUCCESS');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
