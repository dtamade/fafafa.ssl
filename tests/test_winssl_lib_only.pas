{**
 * Minimal test to isolate WinSSL lib crash
 * This test ONLY imports fafafa.ssl.winssl.lib to see if it crashes
 *}
program test_winssl_lib_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.lib;  // ONLY import the lib module

begin
  WriteLn('Step 1: Program started with fafafa.ssl.winssl.lib');
  WriteLn('Step 2: If you see this, the initialization section ran successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
