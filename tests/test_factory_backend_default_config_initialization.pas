program test_factory_backend_default_config_initialization;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.freepascal.lib;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure RequireConfigMatches(const ABackend: string;
  const AExpected, AActual: TSSLConfig);
begin
  Require(AActual.LibraryType = AExpected.LibraryType,
    ABackend + ' default LibraryType should preserve constructor baseline');
  Require(AActual.ContextType = AExpected.ContextType,
    ABackend + ' default ContextType should preserve constructor baseline');
  Require(AActual.VerifyDepth = AExpected.VerifyDepth,
    ABackend + ' default VerifyDepth should preserve constructor baseline');
  Require(AActual.SessionTimeout = AExpected.SessionTimeout,
    ABackend + ' default SessionTimeout should preserve constructor baseline');
  Require(AActual.SessionCacheSize = AExpected.SessionCacheSize,
    ABackend + ' default SessionCacheSize should preserve constructor baseline');
  Require(AActual.ProtocolVersions = AExpected.ProtocolVersions,
    ABackend + ' default ProtocolVersions should preserve constructor baseline');
  Require(AActual.LogLevel = AExpected.LogLevel,
    ABackend + ' default LogLevel should preserve constructor baseline');
end;

type
  TLibraryFactoryFunc = function: ISSLLibrary;

procedure CheckBackend(const ABackend: string; ALibType: TSSLLibraryType;
  ADirectFactory: TLibraryFactoryFunc);
var
  LDirect: ISSLLibrary;
  LFactory: ISSLLibrary;
  LExpected: TSSLConfig;
  LActual: TSSLConfig;
begin
  if not TSSLFactory.IsLibraryAvailable(ALibType) then
  begin
    WriteLn('[SKIP] ', ABackend, ' not available');
    Exit;
  end;

  TSSLFactory.ReleaseLibrary(ALibType);

  LDirect := ADirectFactory();
  LExpected := LDirect.GetDefaultConfig;

  LFactory := TSSLFactory.GetLibrary(ALibType);
  LActual := LFactory.GetDefaultConfig;

  RequireConfigMatches(ABackend, LExpected, LActual);
  WriteLn('[PASS] ', ABackend, ' default config initialization');
end;

begin
  CheckBackend('OpenSSL', sslOpenSSL, @CreateOpenSSLLibrary);
  CheckBackend('MbedTLS', sslMbedTLS, @CreateMbedTLSLibrary);
  CheckBackend('WolfSSL', sslWolfSSL, @CreateWolfSSLLibrary);
  CheckBackend('FreePascal', sslFreePascal, @CreateFreePascalSSLLibrary);
end.
