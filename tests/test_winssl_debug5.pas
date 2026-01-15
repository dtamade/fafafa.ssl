{**
 * 详细诊断测试 - 捕获初始化异常 (with winssl.lib)
 *}

program test_winssl_debug5;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

var
  LAvailable: TSSLLibraryTypes;
begin
  WriteLn('Debug: Starting test...');
  try
    WriteLn('Debug: Checking available libraries...');
    LAvailable := TSSLFactory.GetAvailableLibraries;
    WriteLn('Debug: Got available libraries');
    if sslWinSSL in LAvailable then
      WriteLn('Debug: WinSSL is available')
    else
      WriteLn('Debug: WinSSL is NOT available');
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
