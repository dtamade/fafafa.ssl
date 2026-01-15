{**
 * Minimal test to isolate WinSSL startup crash
 *}
program test_minimal_winssl;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;  // Test: All three without lib

begin
  WriteLn('Step 1: Program started with all three WinSSL modules');
  WriteLn('Step 2: About to exit normally');
  Halt(0);
end.
