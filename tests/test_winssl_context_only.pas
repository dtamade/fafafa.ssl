{**
 * Test WinSSL context only
 *}
program test_winssl_context_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: WinSSL context loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
