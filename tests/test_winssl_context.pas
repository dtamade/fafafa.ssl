program test_winssl_context;
{$mode objfpc}{$H+}
uses
  SysUtils, Windows,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context;

begin
  WriteLn('=== WinSSL Context Test ===');
  WriteLn('SUCCESS: winssl.context loaded');
end.
