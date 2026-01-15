{**
 * 诊断测试: 逐步导入 WinSSL 模块
 *}

program test_winssl_step_import;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;  // 只导入 lib，不自动注册

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: fafafa.ssl.winssl.lib loaded (no auto-register)');
  WriteLn('Step 3: Calling RegisterWinSSLBackend...');
  
  try
    RegisterWinSSLBackend;
    WriteLn('Step 4: RegisterWinSSLBackend completed');
  except
    on E: Exception do
      WriteLn('Step 4: RegisterWinSSLBackend FAILED: ', E.Message);
  end;
  
  WriteLn('SUCCESS');
end.
