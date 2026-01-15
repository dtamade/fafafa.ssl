program test_winssl_utils;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

begin
  WriteLn('=== WinSSL Utils Test ===');
  WriteLn('SUCCESS: winssl.utils loaded');
end.
