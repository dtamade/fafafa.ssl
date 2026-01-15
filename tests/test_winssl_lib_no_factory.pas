{**
 * Test WinSSL lib without factory to see if factory is the issue
 *}
program test_winssl_lib_no_factory;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.winssl.lib;  // This imports factory in implementation

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: All modules loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
