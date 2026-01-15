{**
 * Test the test lib unit
 *}
program test_winssl_lib_test;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib.test;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: About to register test WinSSL backend...');
  RegisterTestWinSSLBackend;
  WriteLn('Step 3: Test WinSSL backend registered successfully');
  WriteLn('Step 4: About to exit normally');
  Halt(0);
end.
