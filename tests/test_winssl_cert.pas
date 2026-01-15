program test_winssl_cert;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.certificate;

begin
  WriteLn('=== WinSSL Certificate Test ===');
  WriteLn('SUCCESS: winssl.certificate loaded');
end.
