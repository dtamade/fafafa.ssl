program test_backend_callback_capability_truth_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.openssl.api.core
  , fafafa.ssl.openssl.api.ssl
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.openssl.api.core
  , fafafa.ssl.openssl.api.ssl
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

procedure CheckOpenSSLBackendCapability;
var
  LLib: ISSLLibrary;
  LActual: Boolean;
  LExpected: Boolean;
begin
  if not TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
  begin
    WriteLn('[SKIP] OpenSSL backend not available on this platform');
    Exit;
  end;

  LLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(LLib <> nil, 'OpenSSL library should be creatable when available');

  LExpected := OpenSSLPublishedContextCallbackSurfaceReady;
  LActual := LLib.GetCapabilities.SupportsCallbacks;
  Require(LActual = LExpected,
    Format('OpenSSL SupportsCallbacks mismatch: expected=%s actual=%s',
      [BoolToStr(LExpected, True), BoolToStr(LActual, True)]));

  WriteLn('[PASS] OpenSSL SupportsCallbacks = ', BoolToStr(LActual, True));
end;

procedure CheckOpenSSLCallbackRuntimeGateContract;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LOrigPasswordUserdata: TSSL_CTX_set_default_passwd_cb_userdata;
begin
  if not TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
  begin
    WriteLn('[SKIP] OpenSSL backend not available on this platform');
    Exit;
  end;

  LLib := TOpenSSLLibrary.Create as ISSLLibrary;
  Require(LLib.Initialize,
    'OpenSSL probe library should initialize for callback capability runtime gate contract');

  LOrigPasswordUserdata := SSL_CTX_set_default_passwd_cb_userdata;
  if not Assigned(LOrigPasswordUserdata) then
  begin
    WriteLn('[SKIP] OpenSSL build does not export password callback userdata helper');
    Exit;
  end;

  SSL_CTX_set_default_passwd_cb_userdata := nil;
  try
    LCaps := LLib.GetCapabilities;
    Require(not LCaps.SupportsCallbacks,
      'OpenSSL must stop publishing SupportsCallbacks when the password callback userdata helper is missing');
  finally
    SSL_CTX_set_default_passwd_cb_userdata := LOrigPasswordUserdata;
  end;

  WriteLn('[PASS] OpenSSL runtime callback gate clears SupportsCallbacks when helper surface is incomplete');
end;

begin
  WriteLn('Testing backend callback capability truth contract');
  WriteLn('=================================================');

  CheckOpenSSLBackendCapability;
  CheckBackendCapability(sslWinSSL, True);
  CheckBackendCapability(sslFreePascal, False);
  CheckBackendCapability(sslWolfSSL, False);
  CheckBackendCapability(sslMbedTLS, False);
  CheckOpenSSLCallbackRuntimeGateContract;

  WriteLn('=================================================');
  WriteLn('✅ backend callback capability truth contract verified');
end.
