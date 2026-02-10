program test_freepascal_backend_basic;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.factory,
  fafafa.ssl.base;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('❌ ', AMessage);
    Halt(1);
  end;
end;

var
  LAvailable: Boolean;
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LCaps: TSSLBackendCapabilities;
begin
  WriteLn('Testing FreePascal backend registration and creation...');

  LAvailable := TSSLFactory.IsLibraryAvailable(sslFreePascal);
  AssertTrue(LAvailable, 'sslFreePascal should be available');

  LLib := TSSLFactory.GetLibrary(sslFreePascal);
  AssertTrue(LLib <> nil, 'GetLibrary(sslFreePascal) should return library instance');
  AssertTrue(LLib.GetLibraryType = sslFreePascal, 'Library type mismatch');

  LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LCtx <> nil, 'CreateContext should return context');
  AssertTrue(LCtx.GetContextType = sslCtxClient, 'Context type mismatch');

  LCaps := LLib.GetCapabilities;
  AssertTrue(IsKeyExchangeSupported(LCaps, sslKexECDHE_RSA),
    'FreePascal backend should advertise ECDHE_RSA');
  AssertTrue(IsKeyExchangeSupported(LCaps, sslKexECDHE_ECDSA),
    'FreePascal backend should advertise ECDHE_ECDSA once pure ECDSA signer is available');
  AssertTrue(not LCaps.RequiresExternalLibrary,
    'FreePascal backend should not require external TLS library');
  AssertTrue(Pos('ECDSA', UpperCase(LCaps.KnownIssues)) > 0,
    'FreePascal capability KnownIssues should mention ECDSA CertificateVerify support scope');

  WriteLn('✅ FreePascal backend basic checks passed');
end.
