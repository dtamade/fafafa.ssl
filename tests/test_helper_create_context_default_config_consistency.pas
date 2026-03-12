program test_helper_create_context_default_config_consistency;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
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
  AConfig.ServerName := 'helper-default.local';
  AConfig.ALPNProtocols := 'h2,http/1.1';
  TSSLFactory.NormalizeConfig(AConfig);
end;

procedure CheckLibrary(const AName: string; ALib: ISSLLibrary; ALibType: TSSLLibraryType);
var
  LConfig: TSSLConfig;
  LSnapshot: TSSLConfig;
  LCtx: ISSLContext;
  LServerCtx: ISSLContext;
begin
  Require(ALib <> nil, AName + ' library should not be nil');

  InitCustomConfig(LConfig, ALibType);
  ALib.SetDefaultConfig(LConfig);
  LSnapshot := ALib.GetDefaultConfig;

  LCtx := ALib.CreateContext(sslCtxClient);
  Require(LCtx <> nil, AName + ' CreateContext returned nil');

  RequireEquals(AName + ' session timeout', LConfig.SessionTimeout, LCtx.GetSessionTimeout);
  RequireEquals(AName + ' session cache size', LConfig.SessionCacheSize, LCtx.GetSessionCacheSize);
  RequireEquals(AName + ' verify depth', LConfig.VerifyDepth, LCtx.GetVerifyDepth);
  RequireEquals(AName + ' ALPN', LConfig.ALPNProtocols, LCtx.GetALPNProtocols);
  RequireEquals(AName + ' cipher list', LConfig.CipherList, LCtx.GetCipherList);
  RequireEquals(AName + ' cipher suites', LConfig.CipherSuites, LCtx.GetCipherSuites);
  Require(LCtx.GetVerifyMode = LConfig.VerifyMode, AName + ' verify mode should match helper defaults');
  Require(LCtx.GetProtocolVersions = LConfig.ProtocolVersions, AName + ' protocol versions should match helper defaults');
  Require(LCtx.GetPreferredVersion = LConfig.PreferredVersion, AName + ' preferred version should match helper defaults');

  LServerCtx := ALib.CreateContext(sslCtxServer);
  Require(LServerCtx <> nil, AName + ' CreateContext(server) returned nil');
  Require(LServerCtx.GetContextType = sslCtxServer, AName + ' server context type should be server');
  RequireEquals(AName + ' server context session timeout', LConfig.SessionTimeout, LServerCtx.GetSessionTimeout);
  RequireEquals(AName + ' server context ALPN', LConfig.ALPNProtocols, LServerCtx.GetALPNProtocols);

  Require(ALib.GetDefaultConfig.ContextType = LSnapshot.ContextType,
    AName + ' CreateContext(server) should not mutate default ContextType snapshot');
  Require(ALib.GetDefaultConfig.LibraryType = LSnapshot.LibraryType,
    AName + ' CreateContext(server) should not mutate default LibraryType snapshot');
  RequireEquals(AName + ' default snapshot session timeout after server create',
    LSnapshot.SessionTimeout,
    ALib.GetDefaultConfig.SessionTimeout);
  RequireEquals(AName + ' default snapshot ALPN after server create',
    LSnapshot.ALPNProtocols,
    ALib.GetDefaultConfig.ALPNProtocols);
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
  WriteLn('fafafa.ssl - helper CreateContext default config consistency');
  TestDefaultFixture;
  TestBackendStoreFixture;
  WriteLn('[PASS] helper CreateContext default config consistency');
end.
