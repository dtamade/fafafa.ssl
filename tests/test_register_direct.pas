{**
 * 诊断测试: 直接调用 TSSLFactory.RegisterLibrary
 *}

program test_register_direct;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Calling TSSLFactory.RegisterLibrary...');
  Flush(Output);
  
  try
    TSSLFactory.RegisterLibrary(sslWinSSL, TWinSSLLibrary,
      'Windows Schannel (Native SSL/TLS)', 200);
    WriteLn('Step 3: RegisterLibrary completed');
  except
    on E: Exception do
      WriteLn('Step 3: RegisterLibrary FAILED: ', E.Message);
  end;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
