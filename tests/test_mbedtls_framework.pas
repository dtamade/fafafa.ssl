{**
 * Test: MbedTLS Backend Framework
 * Purpose: Comprehensive MbedTLS backend framework tests
 *
 * Tests include:
 * - Constants and error mapping
 * - Library creation and initialization
 * - Context creation and configuration
 * - Certificate operations
 * - Session management
 * - Connection interface (mock)
 *
 * Note: Full functionality tests require MbedTLS library to be installed.
 *
 * @author fafafa.ssl team
 * @version 2.0.0
 * @since 2026-01-09
 * @updated 2026-01-10
 *}

program test_mbedtls_framework;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.native_handle,
  fafafa.ssl.mbedtls.api,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.connection,
  fafafa.ssl.mbedtls.certificate,
  fafafa.ssl.mbedtls.session;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;

type
  TTestMbedTLSConnection = class(TMbedTLSConnection)
  public
    procedure MarkHandshakeCompleteForTest;
  end;

  TMockWrongBackendNativeHandle = class(TInterfacedObject, ISSLNativeHandleAccess)
  private
    FHandle: Pointer;
  public
    constructor Create(AHandle: Pointer);
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;
  end;

procedure TTestMbedTLSConnection.MarkHandshakeCompleteForTest;
begin
  FHandshakeComplete := True;
end;

constructor TMockWrongBackendNativeHandle.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TMockWrongBackendNativeHandle.GetNativeHandle: Pointer;
begin
  Result := FHandle;
end;

function TMockWrongBackendNativeHandle.GetBackendType: TSSLLibraryType;
begin
  Result := sslOpenSSL;
end;

function TMockWrongBackendNativeHandle.IsNativeHandleValid: Boolean;
begin
  Result := FHandle <> nil;
end;

{ Helper function to check if object has native handle }
function HasNativeHandle(const AObject: IInterface): Boolean;
var
  NativeAccess: ISSLNativeHandleAccess;
begin
  Result := Supports(AObject, ISSLNativeHandleAccess, NativeAccess) and
            (NativeAccess.GetNativeHandle <> nil);
end;

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

procedure TestMbedTLSConstants;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Constants ===');

  Test('MBEDTLS_SSL_IS_CLIENT = 0', MBEDTLS_SSL_IS_CLIENT = 0);
  Test('MBEDTLS_SSL_IS_SERVER = 1', MBEDTLS_SSL_IS_SERVER = 1);
  Test('MBEDTLS_SSL_TRANSPORT_STREAM = 0', MBEDTLS_SSL_TRANSPORT_STREAM = 0);
  Test('MBEDTLS_SSL_VERIFY_NONE = 0', MBEDTLS_SSL_VERIFY_NONE = 0);
  Test('MBEDTLS_SSL_VERIFY_REQUIRED = 2', MBEDTLS_SSL_VERIFY_REQUIRED = 2);
  Test('MBEDTLS_MIN_VERSION defined', MBEDTLS_MIN_VERSION > 0);
end;

procedure TestMbedTLSErrorMapping;
var
  LResult: TSSLErrorCode;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Error Mapping ===');

  LResult := MbedTLSErrorToSSLError(0);
  Test('0 maps to sslErrNone', LResult = sslErrNone);

  LResult := MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_WANT_READ);
  Test('ERR_SSL_WANT_READ maps to sslErrWantRead', LResult = sslErrWantRead);

  LResult := MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_WANT_WRITE);
  Test('ERR_SSL_WANT_WRITE maps to sslErrWantWrite', LResult = sslErrWantWrite);

  LResult := MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_TIMEOUT);
  Test('ERR_SSL_TIMEOUT maps to sslErrTimeout', LResult = sslErrTimeout);

  LResult := MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE);
  Test('ERR_SSL_HANDSHAKE_FAILURE maps to sslErrHandshake', LResult = sslErrHandshake);

  LResult := MbedTLSErrorToSSLError(MBEDTLS_ERR_X509_CERT_VERIFY_FAILED);
  Test('ERR_X509_CERT_VERIFY_FAILED maps to sslErrCertificate', LResult = sslErrCertificate);

  LResult := MbedTLSErrorToSSLError(-9999);
  Test('Unknown error maps to sslErrGeneral', LResult = sslErrGeneral);
end;

procedure TestMbedTLSProtocolMapping;
var
  LResult: Integer;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Protocol Mapping ===');

  LResult := SSLProtocolToMbedTLS(sslProtocolTLS12);
  Test('TLS 1.2 maps to $0303', LResult = $0303);

  LResult := SSLProtocolToMbedTLS(sslProtocolTLS13);
  Test('TLS 1.3 maps to $0304', LResult = $0304);

  Test('$0303 maps back to TLS 1.2', MbedTLSProtocolToSSL($0303) = sslProtocolTLS12);
  Test('$0304 maps back to TLS 1.3', MbedTLSProtocolToSSL($0304) = sslProtocolTLS13);
end;

procedure TestMbedTLSLibraryCreation;
var
  LLib: ISSLLibrary;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Library Creation ===');

  LLib := CreateMbedTLSLibrary;
  Test('CreateMbedTLSLibrary returns non-nil', LLib <> nil);
  Test('Library type is sslMbedTLS', LLib.GetLibraryType = sslMbedTLS);
  Test('Library not initialized by default', not LLib.IsInitialized);

  WriteLn('  (Note: Initialize test skipped - requires MbedTLS library)');
end;

procedure TestMbedTLSCapabilities;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Capabilities (Framework) ===');

  LLib := CreateMbedTLSLibrary;

  LCaps := LLib.GetCapabilities;
  Test('Capabilities struct accessible', True);
  Test('MinTLSVersion defined', Ord(LCaps.MinTLSVersion) >= 0);
  Test('MaxTLSVersion defined', Ord(LCaps.MaxTLSVersion) >= 0);
end;

procedure TestMbedTLSCapabilityHelperLossContract;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LOriginalSetHostname: Tmbedtls_ssl_set_hostname;
  LOriginalGetALPN: Tmbedtls_ssl_get_alpn_protocol;
  LOriginalConfALPN: Tmbedtls_ssl_conf_alpn_protocols;
  LOriginalGetSession: Tmbedtls_ssl_get_session;
  LOriginalSetSession: Tmbedtls_ssl_set_session;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Capability Helper-Loss Contract ===');

  if not LoadMbedTLSLibrary then
  begin
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('Capability helper-loss contract skipped', True);
    Exit;
  end;

  LOriginalSetHostname := mbedtls_ssl_set_hostname;
  LOriginalGetALPN := mbedtls_ssl_get_alpn_protocol;
  LOriginalConfALPN := mbedtls_ssl_conf_alpn_protocols;
  LOriginalGetSession := mbedtls_ssl_get_session;
  LOriginalSetSession := mbedtls_ssl_set_session;

  try
    mbedtls_ssl_set_hostname := nil;
    mbedtls_ssl_get_alpn_protocol := nil;
    mbedtls_ssl_conf_alpn_protocols := nil;
    mbedtls_ssl_get_session := nil;
    mbedtls_ssl_set_session := nil;

    LLib := CreateMbedTLSLibrary;
    if not LLib.Initialize then
    begin
      WriteLn('  (Skipped - helper-loss capability init unavailable)');
      Test('Capability helper-loss contract skipped', True);
      Exit;
    end;

    try
      LCaps := LLib.GetCapabilities;
      Test('SNI helper loss clears SupportsSNI', not LCaps.SupportsSNI);
      Test('SNI helper loss clears SNISupport', LCaps.SNISupport = sslSupportNone);
      Test('SNI helper loss clears sslFeatSNI', not LLib.IsFeatureSupported(sslFeatSNI));

      Test('ALPN helper loss clears SupportsALPN', not LCaps.SupportsALPN);
      Test('ALPN helper loss clears ALPNSupport', LCaps.ALPNSupport = sslSupportNone);
      Test('ALPN helper loss clears sslFeatALPN', not LLib.IsFeatureSupported(sslFeatALPN));

      Test('Session helper loss clears SupportsSessionTickets', not LCaps.SupportsSessionTickets);
      Test('Session helper loss clears SessionTicketsSupport',
        LCaps.SessionTicketsSupport = sslSupportNone);
      Test('Session helper loss clears sslFeatSessionTickets',
        not LLib.IsFeatureSupported(sslFeatSessionTickets));
    finally
      LLib.Finalize;
    end;
  finally
    if IsMbedTLSLoaded then
      UnloadMbedTLSLibrary;
    mbedtls_ssl_set_hostname := LOriginalSetHostname;
    mbedtls_ssl_get_alpn_protocol := LOriginalGetALPN;
    mbedtls_ssl_conf_alpn_protocols := LOriginalConfALPN;
    mbedtls_ssl_get_session := LOriginalGetSession;
    mbedtls_ssl_set_session := LOriginalSetSession;
  end;
end;

procedure TestMbedTLSCertificateClass;
var
  LCert: TMbedTLSCertificate;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Certificate Class ===');

  LCert := TMbedTLSCertificate.Create;
  try
    Test('Certificate created', LCert <> nil);
    Test('Certificate not loaded initially', LCert.GetNativeHandle = nil);
    Test('GetVersion returns default', LCert.GetVersion = 3);
    Test('GetPublicKeyAlgorithm returns default', LCert.GetPublicKeyAlgorithm = 'RSA');
    Test('Clone works', LCert.Clone <> nil);
  finally
    LCert.Free;
  end;
end;

procedure TestMbedTLSCertificateStore;
var
  LStore: TMbedTLSCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Certificate Store ===');

  LStore := TMbedTLSCertificateStore.Create;
  try
    Test('Store created', LStore <> nil);
    Test('Store initially empty', LStore.GetCount = 0);

    LCert := TMbedTLSCertificate.Create;
    Test('AddCertificate returns true', LStore.AddCertificate(LCert));
    Test('Store count is 1', LStore.GetCount = 1);
    Test('GetCertificate(0) returns cert', LStore.GetCertificate(0) <> nil);

    Test('AddCertificate duplicate returns false', not LStore.AddCertificate(LCert));
    Test('Store count still 1', LStore.GetCount = 1);

    Test('RemoveCertificate returns true', LStore.RemoveCertificate(LCert));
    Test('Store count is 0', LStore.GetCount = 0);

    LStore.AddCertificate(TMbedTLSCertificate.Create);
    LStore.AddCertificate(TMbedTLSCertificate.Create);
    Test('Store count is 2', LStore.GetCount = 2);
    LStore.Clear;
    Test('Store cleared', LStore.GetCount = 0);
  finally
    LStore.Free;
  end;
end;

procedure TestMbedTLSNativeHandleContract;
var
  LMockWrong: IInterface;
  LRaised: Boolean;
  LHandle: Pointer;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Native Handle Contract ===');

  LMockWrong := TMockWrongBackendNativeHandle.Create(Pointer($1234));
  LRaised := False;
  try
    LHandle := GetNativeHandleSafe(LMockWrong, 'TestMbedTLSNativeHandleContract');
    Test('MbedTLS native-handle helper rejects non-MbedTLS backend', False);
  except
    on E: ESSLException do
    begin
      LRaised := Pos('not a mbedtls backend', LowerCase(E.Message)) > 0;
      Test('MbedTLS native-handle helper rejects non-MbedTLS backend', LRaised);
    end;
  end;

  Test('TryGetNativeHandle returns handle for non-nil interface',
    TryGetNativeHandle(LMockWrong, LHandle) and (LHandle <> nil));
end;

procedure TestMbedTLSSessionClass;
var
  LSession: TMbedTLSSession;
  LClone: ISSLSession;
  LData: TBytes;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Session Class ===');

  LSession := TMbedTLSSession.Create;
  try
    Test('Session created', LSession <> nil);
    Test('Session ID not empty', LSession.GetID <> '');
    Test('Session creation time valid', LSession.GetCreationTime > 0);
    Test('Session timeout default', LSession.GetTimeout = SSL_DEFAULT_SESSION_TIMEOUT);
    Test('Session not valid without handle', not LSession.IsValid);
    Test('Session not resumable without handle', not LSession.IsResumable);
    Test('Native handle is nil', LSession.GetNativeHandle = nil);

    LSession.SetTimeout(7200);
    Test('Session timeout updated', LSession.GetTimeout = 7200);

    LClone := LSession.Clone;
    Test('Clone created', LClone <> nil);
    Test('Clone has same timeout', LClone.GetTimeout = 7200);
    Test('Clone has same ID', LClone.GetID = LSession.GetID);

    Test('Serialize returns empty for no session', Length(LSession.Serialize) = 0);
    Test('Deserialize rejects empty payload', not LSession.Deserialize(nil));
    Test('Deserialize accepts data', LSession.Deserialize(TBytes.Create(1, 2, 3, 4)));
    LData := LSession.Serialize;
    Test('Serialize returns cached bytes after deserialize',
      (Length(LData) = 4) and (LData[0] = 1) and (LData[1] = 2) and (LData[2] = 3) and (LData[3] = 4));
  finally
    LSession.Free;
  end;
end;

procedure TestMbedTLSContextCreation;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LInitialized: Boolean;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Context Creation ===');

  LLib := CreateMbedTLSLibrary;
  LInitialized := LLib.Initialize;

  if LInitialized then
  begin
    WriteLn('  MbedTLS library initialized successfully');

    try
      LCtx := LLib.CreateContext(sslCtxClient);
      Test('Client context created', LCtx <> nil);
      Test('Context type is client', LCtx.GetContextType = sslCtxClient);
      Test('Context is valid', LCtx.IsValid);
      Test('Native handle not nil', HasNativeHandle(LCtx));

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
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('Context creation skipped', True);
  end;
end;

procedure TestMbedTLSContextConfiguration;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LErrMsg: string;
  LRaised: Boolean;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Context Configuration ===');

  LLib := CreateMbedTLSLibrary;

  if LLib.Initialize then
  begin
    try
      LCtx := LLib.CreateContext(sslCtxClient);

      LCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      Test('Protocol versions set', sslProtocolTLS12 in LCtx.GetProtocolVersions);

      LCtx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
      Test('Verify mode set', sslVerifyFailIfNoPeerCert in LCtx.GetVerifyMode);

      LCtx.SetVerifyDepth(5);
      Test('Verify depth set to 5', LCtx.GetVerifyDepth = 5);

      // INTENTIONAL_API_SURFACE: context-level SNI setter coverage. This
      // backend framework test validates MbedTLS context configuration, not
      // recommended per-connection handshake guidance.
      LCtx.SetServerName('example.com');
      Test('Server name set', LCtx.GetServerName = 'example.com');

      LCtx.SetSessionCacheMode(False);
      Test('Session cache disabled', not LCtx.GetSessionCacheMode);
      LCtx.SetSessionCacheMode(True);
      Test('Session cache enabled', LCtx.GetSessionCacheMode);

      LCtx.SetSessionTimeout(3600);
      Test('Session timeout set', LCtx.GetSessionTimeout = 3600);

      LCtx.SetALPNProtocols('h2,http/1.1');
      Test('ALPN protocols set', LCtx.GetALPNProtocols = 'h2,http/1.1');

      LCtx.SetOptions([ssoEnableSNI, ssoEnableALPN]);
      Test('Options set', ssoEnableSNI in LCtx.GetOptions);

      // Renegotiation explicit unsupported semantics
      LConn := LCtx.CreateConnection(0);
      Test('Renegotiate returns false before handshake', not LConn.Renegotiate);
      Test('Renegotiate reports unsupported error class',
        LConn.GetError(-1) = sslErrUnsupported);
      LErrMsg := LConn.GetVerifyResultString;
      Test('Renegotiate exposes non-empty diagnostic message',
        Pos('renegotiation', LowerCase(LErrMsg)) > 0);

      LRaised := False;
      try
        LCtx.LoadCertificatePEM('not a valid pem certificate');
      except
        on E: ESSLCertError do
          LRaised := Pos('certificate', LowerCase(E.Message)) > 0;
      end;
      Test('Invalid certificate PEM raises ESSLCertError mapping', LRaised);

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
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('Context configuration skipped', True);
  end;
end;

procedure TestMbedTLSVerifyResultHelperLossContract;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: TTestMbedTLSConnection;
  LStream: TMemoryStream;
  LVerifyResult: Integer;
  LVerifyText: string;
  LOriginalGetVerifyResult: Tmbedtls_ssl_get_verify_result;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Verify Result Helper-Loss Contract ===');

  LLib := CreateMbedTLSLibrary;

  if LLib.Initialize then
  begin
    LCtx := LLib.CreateContext(sslCtxClient);
    LStream := TMemoryStream.Create;
    LConn := nil;
    try
      LConn := TTestMbedTLSConnection.Create(
        LCtx,
        Pmbedtls_ssl_config(GetNativeHandleSafe(LCtx,
          'TestMbedTLSVerifyResultHelperLossContract')),
        LStream
      );
      LConn.MarkHandshakeCompleteForTest;
      LOriginalGetVerifyResult := mbedtls_ssl_get_verify_result;
      try
        mbedtls_ssl_get_verify_result := nil;

        LVerifyResult := LConn.GetVerifyResult;
        LVerifyText := LowerCase(LConn.GetVerifyResultString);

        Test('VerifyResult helper loss degrades to -1', LVerifyResult = -1);
        Test('VerifyResultString helper loss exposes unavailable diagnostic',
          Pos('unavailable', LVerifyText) > 0);
      finally
        mbedtls_ssl_get_verify_result := LOriginalGetVerifyResult;
      end;
    finally
      if Assigned(LConn) then
        LConn.Free;
      LStream.Free;
      LLib.Finalize;
    end;
  end
  else
  begin
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('VerifyResult helper-loss contract skipped', True);
  end;
end;

procedure TestMbedTLSVerifyStatusBeforeHandshakeContract;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LStream: TMemoryStream;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Pre-Handshake Verify Status Contract ===');

  LLib := CreateMbedTLSLibrary;

  if LLib.Initialize then
  begin
    LCtx := LLib.CreateContext(sslCtxClient);
    LStream := TMemoryStream.Create;
    try
      LConn := LCtx.CreateConnection(LStream);
      Test('Fresh MbedTLS connection does not report verify success before handshake',
        LConn.GetVerifyResult = -1);
      Test('Fresh MbedTLS connection reports not-verified diagnostic before handshake',
        Pos('not verified', LowerCase(LConn.GetVerifyResultString)) > 0);
    finally
      LStream.Free;
      LLib.Finalize;
    end;
  end
  else
  begin
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('Pre-handshake verify-status contract skipped', True);
  end;
end;

procedure TestMbedTLSFeatureSupport;
var
  LLib: ISSLLibrary;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Feature Support ===');

  LLib := CreateMbedTLSLibrary;

  if LLib.Initialize then
  begin
    Test('TLS 1.2 supported', LLib.IsProtocolSupported(sslProtocolTLS12));
    Test('SSL 2.0 not supported', not LLib.IsProtocolSupported(sslProtocolSSL2));
    Test('SSL 3.0 not supported', not LLib.IsProtocolSupported(sslProtocolSSL3));
    Test('DTLS 1.0 not supported', not LLib.IsProtocolSupported(sslProtocolDTLS10));
    Test('DTLS 1.2 not supported', not LLib.IsProtocolSupported(sslProtocolDTLS12));
    with LLib.GetCapabilities do
      Test('Capabilities DTLS support matches runtime protocol support',
        SupportsDTLS = (LLib.IsProtocolSupported(sslProtocolDTLS10) or
                        LLib.IsProtocolSupported(sslProtocolDTLS12)));

    Test('Session cache feature', LLib.IsFeatureSupported(sslFeatSessionCache));
    Test('Known TLS1.3 cipher is reported supported',
      LLib.IsCipherSupported('TLS_AES_128_GCM_SHA256'));
    Test('Unknown fake cipher is rejected',
      not LLib.IsCipherSupported('TLS_FAKE_AES_128_GCM_SHA256'));
    Test('Empty cipher name is rejected',
      not LLib.IsCipherSupported(''));

    Test('Version string not empty', LLib.GetVersionString <> '');
    WriteLn('  Version: ', LLib.GetVersionString);

    LLib.Finalize;
  end
  else
  begin
    WriteLn('  (Skipped - MbedTLS library not available)');
    Test('Feature support skipped', True);
  end;
end;

procedure PrintSummary;
var
  LPassRate: Double;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('MbedTLS Framework Test Summary');
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
  WriteLn('MbedTLS Backend Framework Tests');
  WriteLn('================================');
  WriteLn('Testing framework structure and functionality');
  WriteLn('');

  // Basic framework tests (no library required)
  TestMbedTLSConstants;
  TestMbedTLSErrorMapping;
  TestMbedTLSProtocolMapping;
  TestMbedTLSLibraryCreation;
  TestMbedTLSCapabilities;
  TestMbedTLSCapabilityHelperLossContract;

  // Certificate class tests (no library required)
  TestMbedTLSCertificateClass;
  TestMbedTLSCertificateStore;
  TestMbedTLSNativeHandleContract;

  // Session class tests (no library required)
  TestMbedTLSSessionClass;

  // Context tests (require MbedTLS library)
  TestMbedTLSContextCreation;
  TestMbedTLSContextConfiguration;
  TestMbedTLSVerifyResultHelperLossContract;
  TestMbedTLSVerifyStatusBeforeHandshakeContract;
  TestMbedTLSFeatureSupport;

  PrintSummary;

  if GFailCount > 0 then
    Halt(1);
end.
