program test_winssl_no_factory;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('=== WinSSL No Factory Test ===');
  WriteLn('SUCCESS: All winssl units loaded without factory');
end.
