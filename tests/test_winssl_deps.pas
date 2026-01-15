program test_winssl_deps;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base;

begin
  WriteLn('=== WinSSL Deps Test ===');
  WriteLn('SUCCESS: winssl.base loaded');
end.
