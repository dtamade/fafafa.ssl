program test_winssl_api_only;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api;

begin
  WriteLn('=== WinSSL API Only Test ===');
  WriteLn('SUCCESS: winssl.api loaded');
end.
