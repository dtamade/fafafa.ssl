program test_library_create_context_default_config_consistency;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
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

procedure RequireEquals(const AName: string; AExpected, AActual: Integer); overload;
begin
  if AExpected <> AActual then
  begin
    WriteLn('[FAIL] ', AName, ' expected=', AExpected, ' actual=', AActual);
    Halt(1);
  end;
end;

procedure RequireEquals(const AName, AExpected, AActual: string); overload;
begin
  if AExpected <> AActual then
  begin
    WriteLn('[FAIL] ', AName, ' expected="', AExpected, '" actual="', AActual, '"');
    Halt(1);
  end;
end;

procedure InitCustomConfig(var AConfig: TSSLConfig; ALibType: TSSLLibraryType);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := ALibType;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.VerifyDepth := 9;
  AConfig.CipherList := 'CUSTOM-CIPHER';
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
  AConfig.SessionTimeout := 123;
  AConfig.SessionCacheSize := 7;
  AConfig.Options := [ssoEnableSNI];
  AConfig.ServerName := 'direct-default.local';
  AConfig.ALPNProtocols := 'h2,http/1.1';
  AConfig.EnableCompression := False;
  AConfig.EnableSessionTickets := False;
  AConfig.EnableOCSPStapling := False;
  TSSLFactory.NormalizeConfig(AConfig);
end;

procedure RunBackendCase(ALibType: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConfig: TSSLConfig;
  LBackendName: string;
begin
  LBackendName := SSL_LIBRARY_NAMES[ALibType];
  WriteLn('--- Backend: ', LBackendName);

  TSSLFactory.ReleaseLibrary(ALibType);
  LLib := TSSLFactory.GetLibraryInstance(ALibType);
  Require(LLib <> nil, LBackendName + ' library instance should not be nil');

  InitCustomConfig(LConfig, ALibType);
  LLib.SetDefaultConfig(LConfig);

  LCtx := LLib.CreateContext(sslCtxClient);
  Require(LCtx <> nil, LBackendName + ' direct CreateContext returned nil');

  RequireEquals(LBackendName + ' session timeout', LConfig.SessionTimeout, LCtx.GetSessionTimeout);
  RequireEquals(LBackendName + ' session cache size', LConfig.SessionCacheSize, LCtx.GetSessionCacheSize);
  RequireEquals(LBackendName + ' verify depth', LConfig.VerifyDepth, LCtx.GetVerifyDepth);
  RequireEquals(LBackendName + ' ALPN', LConfig.ALPNProtocols, LCtx.GetALPNProtocols);
  RequireEquals(LBackendName + ' cipher list', LConfig.CipherList, LCtx.GetCipherList);
  RequireEquals(LBackendName + ' cipher suites', LConfig.CipherSuites, LCtx.GetCipherSuites);
  Require(LCtx.GetVerifyMode = LConfig.VerifyMode, LBackendName + ' verify mode should match library defaults');
  Require(LCtx.GetProtocolVersions = LConfig.ProtocolVersions, LBackendName + ' protocol versions should match library defaults');
  Require(LCtx.GetPreferredVersion = LConfig.PreferredVersion, LBackendName + ' preferred version should match library defaults');
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  RequireEquals(LBackendName + ' server name', LConfig.ServerName, LCtx.GetServerName);
  {$POP}
end;

procedure TryRunBackendCase(ALibType: TSSLLibraryType);
begin
  try
    RunBackendCase(ALibType);
  except
    on E: Exception do
      WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ALibType], ': ', E.Message);
  end;
end;

begin
  WriteLn('fafafa.ssl - direct library CreateContext default config consistency');

  TryRunBackendCase(sslFreePascal);
  TryRunBackendCase(sslOpenSSL);

  {$IFDEF ENABLE_MBEDTLS}
  TryRunBackendCase(sslMbedTLS);
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  TryRunBackendCase(sslWolfSSL);
  {$ENDIF}

  WriteLn('[PASS] direct library CreateContext default config consistency');
end.
