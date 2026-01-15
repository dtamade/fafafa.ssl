{**
 * Test WinSSL connection only
 *}
program test_winssl_connection_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: WinSSL connection loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
