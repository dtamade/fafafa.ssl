program test_winssl_api;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api;

begin
  WriteLn('=== WinSSL API Test ===');
  WriteLn('SUCCESS: winssl.api loaded');
end.
