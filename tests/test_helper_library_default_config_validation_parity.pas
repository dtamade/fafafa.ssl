program test_helper_library_default_config_validation_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.context;

{$I helpers/test_fake_default_backend_fixture.inc}
{$I helpers/test_backend_store_fake_fixture.inc}

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure InitBaseConfig(out AConfig: TSSLConfig; ALibType: TSSLLibraryType);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := ALibType;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
end;

function MismatchedLibraryType(AOwner: TSSLLibraryType): TSSLLibraryType;
begin
  if AOwner = sslFreePascal then
    Result := sslMbedTLS
  else
    Result := sslFreePascal;
end;

procedure ExpectLibraryDefaultConfigError(const AName: string; ALib: ISSLLibrary;
  const AConfig: TSSLConfig; const AExpectedText: string);
var
  LRaised: Boolean;
begin
  LRaised := False;
  try
    ALib.SetDefaultConfig(AConfig);
  except
    on E: ESSLConfigurationException do
    begin
      LRaised := True;
      Require(Pos(AExpectedText, E.Message) > 0,
        AName + ' should mention ' + AExpectedText + ', actual=' + E.Message);
    end;
  end;

  Require(LRaised, AName + ' should raise ESSLConfigurationException');
end;

procedure CheckLibrary(const AName: string; ALib: ISSLLibrary; AOwnerLibType: TSSLLibraryType);
var
  LConfig: TSSLConfig;
begin
  Require(ALib <> nil, AName + ' library should not be nil');

  InitBaseConfig(LConfig, AOwnerLibType);
  LConfig.HandshakeTimeout := 100;
  ExpectLibraryDefaultConfigError(AName + ' HandshakeTimeout', ALib, LConfig, 'HandshakeTimeout');

  InitBaseConfig(LConfig, AOwnerLibType);
  LConfig.CertificateFile := '/tmp/request-only-cert.pem';
  ExpectLibraryDefaultConfigError(AName + ' CertificateFile', ALib, LConfig, 'CertificateFile');

  InitBaseConfig(LConfig, AOwnerLibType);
  LConfig.LibraryType := MismatchedLibraryType(AOwnerLibType);
  LConfig.ContextType := sslCtxServer;
  LConfig.SessionTimeout := 456;
  ALib.SetDefaultConfig(LConfig);
  Require(ALib.GetDefaultConfig.LibraryType = AOwnerLibType,
    AName + ' should normalize LibraryType back to owner');
  Require(ALib.GetDefaultConfig.ContextType = sslCtxClient,
    AName + ' should normalize ContextType back to stable client baseline');
  Require(ALib.GetDefaultConfig.SessionTimeout = 456,
    AName + ' should preserve non-owner defaults while normalizing owner fields');
end;

procedure TestDefaultFixture;
var
  LLib: ISSLLibrary;
begin
  WriteLn('--- default helper fixture');
  RegisterTestDefaultFakeLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    CheckLibrary('default helper fixture', LLib, sslFreePascal);
  finally
    CleanupTestDefaultFakeLibrary;
  end;
end;

procedure TestBackendStoreFixture;
var
  LDefaultLib: ISSLLibrary;
  LExplicitLib: ISSLLibrary;
begin
  WriteLn('--- backend store helper fixture');
  RegisterFakeLibraries;
  try
    LDefaultLib := TSSLFactory.GetLibrary(sslMbedTLS);
    LExplicitLib := TSSLFactory.GetLibrary(sslFreePascal);
    CheckLibrary('backend store default helper fixture', LDefaultLib, sslMbedTLS);
    CheckLibrary('backend store explicit helper fixture', LExplicitLib, sslFreePascal);
  finally
    CleanupFakeLibraries;
  end;
end;

begin
  WriteLn('fafafa.ssl - helper library default config validation parity');
  TestDefaultFixture;
  TestBackendStoreFixture;
  WriteLn('[PASS] helper library default config validation parity');
end.
