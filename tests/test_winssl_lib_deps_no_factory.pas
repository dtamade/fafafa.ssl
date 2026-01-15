program test_winssl_lib_deps_no_factory;
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
  WriteLn('=== WinSSL Lib Deps (No Factory) Test ===');
  WriteLn('SUCCESS: All winssl.lib deps loaded');
end.
