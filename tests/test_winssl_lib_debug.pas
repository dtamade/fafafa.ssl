program test_winssl_lib_debug;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory;

// 手动注册 WinSSL 而不是通过 initialization
procedure ManualRegisterWinSSL;
begin
  WriteLn('Attempting to register WinSSL...');
  try
    // 这里我们不能直接调用 RegisterLibrary 因为需要 TWinSSLLibrary 类
    // 但我们可以测试 factory 是否正常工作
    WriteLn('  Factory initialized: ', TSSLFactory.GetAutoInitialize);
    WriteLn('  Available libraries: ', TSSLFactory.GetAvailableLibraries <> []);
  except
    on E: Exception do
      WriteLn('  ERROR: ', E.ClassName, ' - ', E.Message);
  end;
end;

begin
  WriteLn('=== WinSSL Lib Debug Test ===');
  ManualRegisterWinSSL;
  WriteLn('SUCCESS: Test completed');
end.
