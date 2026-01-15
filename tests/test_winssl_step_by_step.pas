{**
 * Step-by-step test to isolate WinSSL crash
 * Uncomment each module one by one to find the culprit
 *}
program test_winssl_step_by_step;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  // Step 1: Base modules (should work)
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  // Step 2: Factory (should work after our fixes)
  fafafa.ssl.factory,
  // Step 3: WinSSL implementation modules
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  // Step 4: WinSSL lib (the problematic one)
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: All modules loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
