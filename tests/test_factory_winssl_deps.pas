program test_factory_winssl_deps;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('=== Factory + WinSSL Deps Test ===');
  WriteLn('SUCCESS: Factory and winssl deps loaded');
end.
