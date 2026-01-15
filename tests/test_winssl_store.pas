program test_winssl_store;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('=== WinSSL CertStore Test ===');
  WriteLn('SUCCESS: winssl.certstore loaded');
end.
