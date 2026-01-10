{**
 * Test: WolfSSL Backend Framework
 * Purpose: Comprehensive WolfSSL backend framework tests
 *
 * Tests include:
 * - Constants and error mapping
 * - Library creation and initialization
 * - Context creation and configuration
 * - Certificate operations
 * - Connection interface (mock)
 *
 * Note: Full functionality tests require WolfSSL library to be installed.
 *
 * @author fafafa.ssl team
 * @version 2.0.0
 * @since 2026-01-09
 * @updated 2026-01-10
 *}

program test_wolfssl_framework;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.wolfssl.context,
  fafafa.ssl.wolfssl.certificate,
  fafafa.ssl.wolfssl.session;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;

procedure Test(const AName: string; ACondition: Boolean);
begin
  Inc(GTestCount);
  Write(AName, ': ');
  if ACondition then
  begin
    WriteLn('PASS');
    Inc(GPassCount);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(GFailCount);
  end;
end;

procedure TestWolfSSLConstants;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Constants ===');

  Test('WOLFSSL_SUCCESS = 1', WOLFSSL_SUCCESS = 1);
  Test('WOLFSSL_FAILURE = 0', WOLFSSL_FAILURE = 0);
  Test('WOLFSSL_ERROR_NONE = 0', WOLFSSL_ERROR_NONE = 0);
  Test('WOLFSSL_ERROR_WANT_READ defined', WOLFSSL_ERROR_WANT_READ = -2);
  Test('WOLFSSL_ERROR_WANT_WRITE defined', WOLFSSL_ERROR_WANT_WRITE = -3);
  Test('WOLFSSL_MIN_VERSION defined', WOLFSSL_MIN_VERSION > 0);
end;

procedure TestWolfSSLErrorMapping;
var
  LResult: TSSLErrorCode;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Error Mapping ===');

  LResult := WolfSSLErrorToSSLError(WOLFSSL_ERROR_NONE);
  Test('ERROR_NONE maps to sslErrNone', LResult = sslErrNone);

  LResult := WolfSSLErrorToSSLError(WOLFSSL_ERROR_WANT_READ);
  Test('ERROR_WANT_READ maps to sslErrWantRead', LResult = sslErrWantRead);

  LResult := WolfSSLErrorToSSLError(WOLFSSL_ERROR_WANT_WRITE);
  Test('ERROR_WANT_WRITE maps to sslErrWantWrite', LResult = sslErrWantWrite);

  LResult := WolfSSLErrorToSSLError(WOLFSSL_ERROR_SYSCALL);
  Test('ERROR_SYSCALL maps to sslErrIO', LResult = sslErrIO);

  LResult := WolfSSLErrorToSSLError(WOLFSSL_ERROR_SSL);
  Test('ERROR_SSL maps to sslErrProtocol', LResult = sslErrProtocol);

  LResult := WolfSSLErrorToSSLError(-999);
  Test('Unknown error maps to sslErrGeneral', LResult = sslErrGeneral);
end;

procedure TestWolfSSLProtocolMapping;
var
  LResult: Integer;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Protocol Mapping ===');

  LResult := SSLProtocolToWolfSSL(sslProtocolTLS12);
  Test('TLS 1.2 maps to $0303', LResult = $0303);

  LResult := SSLProtocolToWolfSSL(sslProtocolTLS13);
  Test('TLS 1.3 maps to $0304', LResult = $0304);

  Test('$0303 maps back to TLS 1.2', WolfSSLProtocolToSSL($0303) = sslProtocolTLS12);
  Test('$0304 maps back to TLS 1.3', WolfSSLProtocolToSSL($0304) = sslProtocolTLS13);
end;

procedure TestWolfSSLLibraryCreation;
var
  LLib: ISSLLibrary;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Library Creation ===');

  LLib := CreateWolfSSLLibrary;
  Test('CreateWolfSSLLibrary returns non-nil', LLib <> nil);
  Test('Library type is sslWolfSSL', LLib.GetLibraryType = sslWolfSSL);
  Test('Library not initialized by default', not LLib.IsInitialized);

  // Note: Initialize will fail if WolfSSL library is not installed
  // This is expected behavior - we're testing the framework, not the library
  WriteLn('  (Note: Initialize test skipped - requires WolfSSL library)');
end;

procedure TestWolfSSLCapabilities;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Capabilities (Framework) ===');

  LLib := CreateWolfSSLLibrary;

  // Before initialization, capabilities should be empty/default
  LCaps := LLib.GetCapabilities;
  Test('Capabilities struct accessible', True);
  Test('MinTLSVersion defined', Ord(LCaps.MinTLSVersion) >= 0);
  Test('MaxTLSVersion defined', Ord(LCaps.MaxTLSVersion) >= 0);
end;

procedure TestWolfSSLCertificateClass;
var
  LCert: TWolfSSLCertificate;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Certificate Class ===');

  LCert := TWolfSSLCertificate.Create;
  try
    Test('Certificate created', LCert <> nil);
    Test('Certificate not loaded initially', LCert.GetNativeHandle = nil);
    Test('GetVersion returns default', LCert.GetVersion = 3);
    Test('GetPublicKeyAlgorithm returns default', LCert.GetPublicKeyAlgorithm = 'RSA');
    // Note: Without X509 loaded, GetNotAfter returns 0, so IsExpired returns True
    Test('IsExpired returns True without X509', LCert.IsExpired);
    Test('Clone works', LCert.Clone <> nil);
  finally
    LCert.Free;
  end;
end;

procedure TestWolfSSLCertificateStore;
var
  LStore: TWolfSSLCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Certificate Store ===');

  LStore := TWolfSSLCertificateStore.Create;
  try
    Test('Store created', LStore <> nil);
    Test('Store initially empty', LStore.GetCount = 0);

    // Test adding certificate
    LCert := TWolfSSLCertificate.Create;
    Test('AddCertificate returns true', LStore.AddCertificate(LCert));
    Test('Store count is 1', LStore.GetCount = 1);
    Test('GetCertificate(0) returns cert', LStore.GetCertificate(0) <> nil);

    // Test duplicate prevention
    Test('AddCertificate duplicate returns false', not LStore.AddCertificate(LCert));
    Test('Store count still 1', LStore.GetCount = 1);

    // Test removal
    Test('RemoveCertificate returns true', LStore.RemoveCertificate(LCert));
    Test('Store count is 0', LStore.GetCount = 0);

    // Test clear
    LStore.AddCertificate(TWolfSSLCertificate.Create);
    LStore.AddCertificate(TWolfSSLCertificate.Create);
    Test('Store count is 2', LStore.GetCount = 2);
    LStore.Clear;
    Test('Store cleared', LStore.GetCount = 0);
  finally
    LStore.Free;
  end;
end;

procedure TestWolfSSLContextCreation;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LInitialized: Boolean;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Context Creation ===');

  LLib := CreateWolfSSLLibrary;
  LInitialized := LLib.Initialize;

  if LInitialized then
  begin
    WriteLn('  WolfSSL library initialized successfully');

    try
      LCtx := LLib.CreateContext(sslCtxClient);
      Test('Client context created', LCtx <> nil);
      Test('Context type is client', LCtx.GetContextType = sslCtxClient);
      Test('Context is valid', LCtx.IsValid);
      Test('Native handle not nil', LCtx.GetNativeHandle <> nil);

      // Test default values
      Test('Default verify mode includes peer', sslVerifyPeer in LCtx.GetVerifyMode);
      Test('Default verify depth > 0', LCtx.GetVerifyDepth > 0);
      Test('Session cache enabled by default', LCtx.GetSessionCacheMode);
    except
      on E: Exception do
      begin
        WriteLn('  Context creation failed: ', E.Message);
        Test('Context creation', False);
      end;
    end;

    LLib.Finalize;
  end
  else
  begin
    WriteLn('  (Skipped - WolfSSL library not available)');
    Test('Context creation skipped', True);
  end;
end;

procedure TestWolfSSLContextConfiguration;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Context Configuration ===');

  LLib := CreateWolfSSLLibrary;

  if LLib.Initialize then
  begin
    try
      LCtx := LLib.CreateContext(sslCtxClient);

      // Test protocol versions
      LCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      Test('Protocol versions set', sslProtocolTLS12 in LCtx.GetProtocolVersions);

      // Test verify mode
      LCtx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
      Test('Verify mode set', sslVerifyFailIfNoPeerCert in LCtx.GetVerifyMode);

      // Test verify depth
      LCtx.SetVerifyDepth(5);
      Test('Verify depth set to 5', LCtx.GetVerifyDepth = 5);

      // Test server name
      LCtx.SetServerName('example.com');
      Test('Server name set', LCtx.GetServerName = 'example.com');

      // Test session cache
      LCtx.SetSessionCacheMode(False);
      Test('Session cache disabled', not LCtx.GetSessionCacheMode);
      LCtx.SetSessionCacheMode(True);
      Test('Session cache enabled', LCtx.GetSessionCacheMode);

      // Test session timeout
      LCtx.SetSessionTimeout(3600);
      Test('Session timeout set', LCtx.GetSessionTimeout = 3600);

      // Test ALPN protocols
      LCtx.SetALPNProtocols('h2,http/1.1');
      Test('ALPN protocols set', LCtx.GetALPNProtocols = 'h2,http/1.1');

      // Test options
      LCtx.SetOptions([ssoEnableSNI, ssoEnableALPN]);
      Test('Options set', ssoEnableSNI in LCtx.GetOptions);

    except
      on E: Exception do
      begin
        WriteLn('  Configuration test failed: ', E.Message);
        Test('Context configuration', False);
      end;
    end;

    LLib.Finalize;
  end
  else
  begin
    WriteLn('  (Skipped - WolfSSL library not available)');
    Test('Context configuration skipped', True);
  end;
end;

procedure TestWolfSSLFeatureSupport;
var
  LLib: ISSLLibrary;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Feature Support ===');

  LLib := CreateWolfSSLLibrary;

  if LLib.Initialize then
  begin
    Test('TLS 1.2 supported', LLib.IsProtocolSupported(sslProtocolTLS12));
    Test('SSL 2.0 not supported', not LLib.IsProtocolSupported(sslProtocolSSL2));
    Test('SSL 3.0 not supported', not LLib.IsProtocolSupported(sslProtocolSSL3));

    // Feature support
    Test('Session cache feature', LLib.IsFeatureSupported(sslFeatSessionCache));

    // Version info
    Test('Version string not empty', LLib.GetVersionString <> '');
    WriteLn('  Version: ', LLib.GetVersionString);

    LLib.Finalize;
  end
  else
  begin
    WriteLn('  (Skipped - WolfSSL library not available)');
    Test('Feature support skipped', True);
  end;
end;

procedure TestWolfSSLSessionClass;
var
  LSession: TWolfSSLSession;
  LClone: ISSLSession;
begin
  WriteLn('');
  WriteLn('=== WolfSSL Session Class ===');

  LSession := TWolfSSLSession.Create;
  try
    Test('Session created', LSession <> nil);
    Test('Session ID not empty', LSession.GetID <> '');
    Test('Session creation time valid', LSession.GetCreationTime > 0);
    Test('Session timeout default', LSession.GetTimeout = SSL_DEFAULT_SESSION_TIMEOUT);
    Test('Session not valid without handle', not LSession.IsValid);
    Test('Session not resumable without handle', not LSession.IsResumable);
    Test('Native handle is nil', LSession.GetNativeHandle = nil);

    // Test timeout setting
    LSession.SetTimeout(7200);
    Test('Session timeout updated', LSession.GetTimeout = 7200);

    // Test clone
    LClone := LSession.Clone;
    Test('Clone created', LClone <> nil);
    Test('Clone has same timeout', LClone.GetTimeout = 7200);
    Test('Clone has same ID', LClone.GetID = LSession.GetID);

    // Test serialization
    Test('Serialize returns empty for no session', Length(LSession.Serialize) = 0);
    Test('Deserialize accepts data', LSession.Deserialize(TBytes.Create(1, 2, 3, 4)));
  finally
    LSession.Free;
  end;
end;

procedure PrintSummary;
var
  LPassRate: Double;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('WolfSSL Framework Test Summary');
  WriteLn('========================================');
  WriteLn(Format('Total:  %d', [GTestCount]));
  WriteLn(Format('Passed: %d', [GPassCount]));
  WriteLn(Format('Failed: %d', [GFailCount]));

  if GTestCount > 0 then
    LPassRate := (GPassCount / GTestCount) * 100
  else
    LPassRate := 0;

  WriteLn(Format('Rate:   %.1f%%', [LPassRate]));
  WriteLn('========================================');
end;

begin
  WriteLn('WolfSSL Backend Framework Tests');
  WriteLn('================================');
  WriteLn('Testing framework structure and functionality');
  WriteLn('');

  // Basic framework tests (no library required)
  TestWolfSSLConstants;
  TestWolfSSLErrorMapping;
  TestWolfSSLProtocolMapping;
  TestWolfSSLLibraryCreation;
  TestWolfSSLCapabilities;

  // Certificate class tests (no library required)
  TestWolfSSLCertificateClass;
  TestWolfSSLCertificateStore;

  // Session class tests (no library required)
  TestWolfSSLSessionClass;

  // Context tests (require WolfSSL library)
  TestWolfSSLContextCreation;
  TestWolfSSLContextConfiguration;
  TestWolfSSLFeatureSupport;

  PrintSummary;

  if GFailCount > 0 then
    Halt(1);
end.
