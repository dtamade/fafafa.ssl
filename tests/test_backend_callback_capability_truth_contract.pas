program test_backend_callback_capability_truth_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.winssl.lib
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  ;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure CheckBackendCapability(ABackend: TSSLLibraryType; AExpected: Boolean);
var
  LLib: ISSLLibrary;
  LActual: Boolean;
begin
  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ABackend], ' backend not available on this platform');
    Exit;
  end;

  LLib := TSSLFactory.GetLibrary(ABackend);
  Require(LLib <> nil, SSL_LIBRARY_NAMES[ABackend] + ' library should be creatable when available');

  LActual := LLib.GetCapabilities.SupportsCallbacks;
  Require(LActual = AExpected,
    Format('%s SupportsCallbacks mismatch: expected=%s actual=%s',
      [SSL_LIBRARY_NAMES[ABackend], BoolToStr(AExpected, True), BoolToStr(LActual, True)]));

  WriteLn('[PASS] ', SSL_LIBRARY_NAMES[ABackend], ' SupportsCallbacks = ',
    BoolToStr(LActual, True));
end;

begin
  WriteLn('Testing backend callback capability truth contract');
  WriteLn('=================================================');

  CheckBackendCapability(sslOpenSSL, True);
  CheckBackendCapability(sslWinSSL, True);
  CheckBackendCapability(sslFreePascal, False);
  CheckBackendCapability(sslWolfSSL, False);
  CheckBackendCapability(sslMbedTLS, False);

  WriteLn('=================================================');
  WriteLn('✅ backend callback capability truth contract verified');
end.
