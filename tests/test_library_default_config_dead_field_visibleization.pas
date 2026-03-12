program test_library_default_config_dead_field_visibleization;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.freepascal.lib;

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
    Result := sslOpenSSL
  else
    Result := sslFreePascal;
end;

procedure ExpectLibraryDefaultConfigError(ALibType: TSSLLibraryType; const AName: string;
  const AConfig: TSSLConfig; const AExpectedText: string);
var
  LLib: ISSLLibrary;
  LRaised: Boolean;
begin
  LLib := TSSLFactory.GetLibraryInstance(ALibType);
  Require(LLib <> nil, AName + ' should acquire library instance');

  LRaised := False;
  try
    LLib.SetDefaultConfig(AConfig);
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

procedure TestBackendCase(ALibType: TSSLLibraryType);
var
  LConfig: TSSLConfig;
  LLib: ISSLLibrary;
  LSaved: TSSLConfig;
  LName: string;
begin
  LName := SSL_LIBRARY_NAMES[ALibType];
  WriteLn('--- Backend: ', LName);

  TSSLFactory.ReleaseLibrary(ALibType);
  LLib := TSSLFactory.GetLibraryInstance(ALibType);
  Require(LLib <> nil, LName + ' library instance should not be nil');
  LSaved := LLib.GetDefaultConfig;

  InitBaseConfig(LConfig, ALibType);
  LConfig.HandshakeTimeout := 100;
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default HandshakeTimeout', LConfig, 'HandshakeTimeout');

  InitBaseConfig(LConfig, ALibType);
  LConfig.BufferSize := 8192;
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default BufferSize', LConfig, 'BufferSize');

  InitBaseConfig(LConfig, ALibType);
  LConfig.CertificateFile := '/tmp/request-only-cert.pem';
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default CertificateFile', LConfig, 'CertificateFile');

  InitBaseConfig(LConfig, ALibType);
  LConfig.PrivateKeyFile := '/tmp/request-only-key.pem';
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default PrivateKeyFile', LConfig, 'PrivateKeyFile');

  InitBaseConfig(LConfig, ALibType);
  LConfig.PrivateKeyPassword := 'secret';
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default PrivateKeyPassword', LConfig, 'PrivateKeyPassword');

  InitBaseConfig(LConfig, ALibType);
  LConfig.CAFile := '/tmp/request-only-ca.pem';
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default CAFile', LConfig, 'CAFile');

  InitBaseConfig(LConfig, ALibType);
  LConfig.CAPath := '/tmp/request-only-ca-dir';
  ExpectLibraryDefaultConfigError(ALibType, LName + ' default CAPath', LConfig, 'CAPath');

  InitBaseConfig(LConfig, ALibType);
  LConfig.LibraryType := MismatchedLibraryType(ALibType);
  LConfig.ContextType := sslCtxServer;
  LConfig.SessionTimeout := 456;
  LLib.SetDefaultConfig(LConfig);
  Require(LLib.GetDefaultConfig.LibraryType = ALibType,
    LName + ' should normalize default LibraryType back to backend owner');
  Require(LLib.GetDefaultConfig.ContextType = sslCtxClient,
    LName + ' should normalize default ContextType back to stable client baseline');
  Require(LLib.GetDefaultConfig.SessionTimeout = 456,
    LName + ' should preserve non-owner default fields while normalizing owner fields');

  InitBaseConfig(LConfig, ALibType);
  LConfig.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
  LConfig.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  LLib.SetDefaultConfig(LConfig);
  Require(LLib.GetDefaultConfig.BufferSize = SSL_DEFAULT_BUFFER_SIZE,
    LName + ' should still accept default BufferSize');
  Require(LLib.GetDefaultConfig.HandshakeTimeout = SSL_DEFAULT_HANDSHAKE_TIMEOUT,
    LName + ' should still accept default HandshakeTimeout');

  LLib.SetDefaultConfig(LSaved);
end;

procedure TryBackendCase(ALibType: TSSLLibraryType);
begin
  try
    TestBackendCase(ALibType);
  except
    on E: Exception do
      WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ALibType], ': ', E.Message);
  end;
end;

begin
  WriteLn('fafafa.ssl - library default config dead-field visibleization');
  TryBackendCase(sslFreePascal);
  TryBackendCase(sslOpenSSL);
  {$IFDEF ENABLE_MBEDTLS}
  TryBackendCase(sslMbedTLS);
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  TryBackendCase(sslWolfSSL);
  {$ENDIF}
  {$IFDEF WINDOWS}
  TryBackendCase(sslWinSSL);
  {$ENDIF}
  WriteLn('[PASS] library default config dead-field visibleization');
end.
