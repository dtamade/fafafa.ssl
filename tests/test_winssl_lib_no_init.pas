{**
 * Test WinSSL lib functionality without initialization section
 * This is a copy of fafafa.ssl.winssl.lib but without the initialization
 *}
program test_winssl_lib_no_init;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.factory;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: All modules loaded successfully');
  
  // Now manually try to register WinSSL
  WriteLn('Step 3: About to register WinSSL backend...');
  // TSSLFactory.RegisterLibrary(sslWinSSL, TWinSSLLibrary, 'Windows Schannel', 200);
  
  WriteLn('Step 4: About to exit normally');
  Halt(0);
end.
