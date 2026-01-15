{**
 * 测试 WinSSL 自动注册
 *}

program test_winssl_autoregister;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,              // 需要导入 factory 以使用 TSSLFactory
  fafafa.ssl.winssl.autoregister;  // 自动注册 WinSSL

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
