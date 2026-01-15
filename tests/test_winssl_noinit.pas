{**
 * Test WinSSL lib without auto-initialization
 *}
program test_winssl_noinit;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib.noinit;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: About to manually register WinSSL backend...');
  RegisterWinSSLBackend;
  WriteLn('Step 3: WinSSL backend registered successfully');
  WriteLn('Step 4: About to exit normally');
  Halt(0);
end.
