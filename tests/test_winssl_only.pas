program test_winssl_only;
{$mode objfpc}{$H+}
uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('=== WinSSL Only Test ===');
  WriteLn('SUCCESS: Program started with WinSSL');
end.
