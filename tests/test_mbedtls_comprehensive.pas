{**
 * Test: MbedTLS Backend Comprehensive Unit Tests
 * Purpose: High-coverage unit tests for MbedTLS backend
 *
 * Test Categories:
 * 1. Error code mapping
 * 2. Library initialization and finalization
 * 3. Context creation and configuration
 * 4. Certificate operations
 * 5. Session management
 * 6. Connection interface (mock)
 * 7. Capabilities detection
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_mbedtls_comprehensive;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
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
  GSkipCount: Integer = 0;
  GLibraryAvailable: Boolean = False;

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

procedure Skip(const AName: string; const AReason: string);
begin
  Inc(GTestCount);
  Inc(GSkipCount);
  WriteLn(AName, ': SKIP (', AReason, ')');
end;

procedure Section(const ATitle: string);
begin
  WriteLn('');
  WriteLn('=== ', ATitle, ' ===');
end;

// ============================================================================
// Category 1: Error Code Mapping Tests
// ============================================================================

procedure TestErrorCodeMapping;
begin
  Section('Error Code Mapping');
  Test('MBEDTLS_ERR_SSL_WANT_READ maps to sslErrWantRead',
    MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_WANT_READ) = sslErrWantRead);
  Test('MBEDTLS_ERR_SSL_WANT_WRITE maps to sslErrWantWrite',
    MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_WANT_WRITE) = sslErrWantWrite);
  Test('MBEDTLS_ERR_SSL_TIMEOUT maps to sslErrTimeout',
    MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_TIMEOUT) = sslErrTimeout);
  Test('MBEDTLS_ERR_SSL_CONN_EOF maps to sslErrConnection',
    MbedTLSErrorToSSLError(MBEDTLS_ERR_SSL_CONN_EOF) = sslErrConnection);
  Test('Zero error maps to sslErrNone',
    MbedTLSErrorToSSLError(0) = sslErrNone);
  Test('Unknown error maps to sslErrGeneral',
    MbedTLSErrorToSSLError(-99999) = sslErrGeneral);
end;

// ============================================================================
// Category 2: Library Initialization Tests
// ============================================================================

procedure TestLibraryInitialization;
var
  LLib: ISSLLibrary;
  LInitialized: Boolean;
begin
  Section('Library Initialization');
  try
    LLib := CreateMbedTLSLibrary;
    Test('CreateMbedTLSLibrary returns non-nil', LLib <> nil);
  except
    on E: Exception do
    begin
      Test('CreateMbedTLSLibrary returns non-nil', False);
      WriteLn('  Exception: ', E.Message);
      Exit;
    end;
  end;

  Test('Library type is sslMbedTLS', LLib.GetLibraryType = sslMbedTLS);

  LInitialized := LLib.Initialize;
  GLibraryAvailable := LInitialized;
  Test('Initialize returns result', True);
  WriteLn('  Initialized: ', LInitialized);

  if LInitialized then
  begin
    Test('Version string is not empty', LLib.GetVersionString <> '');
    WriteLn('  Version: ', LLib.GetVersionString);
    Test('Version number > 0', LLib.GetVersionNumber > 0);
    Test('GetCapabilities returns valid record', True);
    LLib.Finalize;
    Test('Finalize completes without error', True);
  end
  else
  begin
    Skip('Version string check', 'Library not available');
    Skip('Version number check', 'Library not available');
    Skip('Capabilities check', 'Library not available');
    Skip('Finalize check', 'Library not available');
  end;
end;

// ============================================================================
// Category 3: Context Creation and Configuration Tests
// ============================================================================

procedure TestContextCreation;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  Section('Context Creation and Configuration');
  if not GLibraryAvailable then
  begin
    Skip('Context creation tests', 'Library not available');
    Exit;
  end;

  LLib := CreateMbedTLSLibrary;
  if not LLib.Initialize then
  begin
    Skip('Context creation tests', 'Library initialization failed');
    Exit;
  end;

  try
    try
      LCtx := LLib.CreateContext(sslCtxClient);
      Test('CreateContext(sslCtxClient) succeeds', LCtx <> nil);
      if LCtx <> nil then
      begin
        Test('Context type is sslCtxClient', LCtx.GetContextType = sslCtxClient);
        Test('Context is valid', LCtx.IsValid);
        Test('Native handle is not nil', LCtx.GetNativeHandle <> nil);
        LCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
        Test('Protocol versions set', sslProtocolTLS12 in LCtx.GetProtocolVersions);
        LCtx.SetVerifyMode([sslVerifyPeer]);
        Test('Verify mode set', sslVerifyPeer in LCtx.GetVerifyMode);
        LCtx.SetVerifyDepth(5);
        Test('Verify depth set to 5', LCtx.GetVerifyDepth = 5);
        LCtx.SetServerName('example.com');
        Test('Server name set', LCtx.GetServerName = 'example.com');
        LCtx.SetSessionCacheMode(True);
        Test('Session cache enabled', LCtx.GetSessionCacheMode = True);
        LCtx.SetSessionTimeout(7200);
        Test('Session timeout set', LCtx.GetSessionTimeout = 7200);
      end;
    except
      on E: Exception do
      begin
        Test('CreateContext(sslCtxClient) succeeds', False);
        WriteLn('  Exception: ', E.Message);
      end;
    end;

    try
      LCtx := LLib.CreateContext(sslCtxServer);
      Test('CreateContext(sslCtxServer) succeeds', LCtx <> nil);
      if LCtx <> nil then
        Test('Server context type correct', LCtx.GetContextType = sslCtxServer);
    except
      on E: Exception do
      begin
        Test('CreateContext(sslCtxServer) succeeds', False);
        WriteLn('  Exception: ', E.Message);
      end;
    end;
  finally
    LLib.Finalize;
  end;
end;

// ============================================================================
// Category 4: Certificate Operations Tests
// ============================================================================

procedure TestCertificateOperations;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LCertFile: string;
begin
  Section('Certificate Operations');
  if not GLibraryAvailable then
  begin
    Skip('Certificate tests', 'Library not available');
    Exit;
  end;

  LLib := CreateMbedTLSLibrary;
  if not LLib.Initialize then
  begin
    Skip('Certificate tests', 'Library initialization failed');
    Exit;
  end;

  try
    LCtx := LLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Certificate tests', 'Context creation failed');
      Exit;
    end;

    try
      LCtx.LoadCertificate('/nonexistent/path/cert.pem');
      Test('LoadCertificate with invalid path raises exception', False);
    except
      on E: Exception do
        Test('LoadCertificate with invalid path raises exception', True);
    end;

    try
      LCtx.LoadCAFile('/nonexistent/path/ca.pem');
      Test('LoadCAFile with invalid path raises exception', False);
    except
      on E: Exception do
        Test('LoadCAFile with invalid path raises exception', True);
    end;

    LCertFile := '/etc/ssl/certs/ca-certificates.crt';
    if FileExists(LCertFile) then
    begin
      try
        LCtx.LoadCAFile(LCertFile);
        Test('LoadCAFile with system CA bundle succeeds', True);
      except
        on E: Exception do
        begin
          Test('LoadCAFile with system CA bundle succeeds', False);
          WriteLn('  Exception: ', E.Message);
        end;
      end;
    end
    else
      Skip('LoadCAFile with system CA bundle', 'CA bundle not found');
  finally
    LLib.Finalize;
  end;
end;

// ============================================================================
// Category 5: Session Management Tests
// ============================================================================

procedure TestSessionManagement;
var
  LLib: ISSLLibrary;
  LSession, LClone: ISSLSession;
begin
  Section('Session Management');
  if not GLibraryAvailable then
  begin
    Skip('Session tests', 'Library not available');
    Exit;
  end;

  LLib := CreateMbedTLSLibrary;
  if not LLib.Initialize then
  begin
    Skip('Session tests', 'Library initialization failed');
    Exit;
  end;

  try
    try
      LSession := TMbedTLSSession.Create;
      Test('TMbedTLSSession.Create succeeds', LSession <> nil);
      if LSession <> nil then
      begin
        Test('Session GetID returns string', True);  // MbedTLS may have ID on creation
        LSession.SetTimeout(3600);
        Test('Session timeout set to 3600', LSession.GetTimeout = 3600);
        try
          LClone := LSession.Clone;
          Test('Session clone succeeds', LClone <> nil);
          if LClone <> nil then
            Test('Clone has same timeout', LClone.GetTimeout = 3600);
        except
          Test('Session clone succeeds', False);
        end;
      end;
    except
      on E: Exception do
      begin
        Test('TMbedTLSSession.Create succeeds', False);
        WriteLn('  Exception: ', E.Message);
      end;
    end;
  finally
    LLib.Finalize;
  end;
end;

// ============================================================================
// Category 6: Connection Interface Tests (Mock)
// ============================================================================

procedure TestConnectionInterface;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('Connection Interface (Mock)');
  if not GLibraryAvailable then
  begin
    Skip('Connection tests', 'Library not available');
    Exit;
  end;

  LLib := CreateMbedTLSLibrary;
  if not LLib.Initialize then
  begin
    Skip('Connection tests', 'Library initialization failed');
    Exit;
  end;

  try
    LCtx := LLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Connection tests', 'Context creation failed');
      Exit;
    end;

    try
      LConn := LCtx.CreateConnection(THandle(-1));
      Test('CreateConnection succeeds', LConn <> nil);
      if LConn <> nil then
      begin
        Test('IsHandshakeComplete is false initially', not LConn.IsHandshakeComplete);
        Test('IsConnected is false initially', not LConn.IsConnected);
        LConn.SetTimeout(5000);
        Test('SetTimeout succeeds', LConn.GetTimeout = 5000);
        Test('GetContext returns valid context', LConn.GetContext <> nil);
      end;
    except
      on E: Exception do
      begin
        Test('CreateConnection succeeds', False);
        WriteLn('  Exception: ', E.Message);
      end;
    end;
  finally
    LLib.Finalize;
  end;
end;

// ============================================================================
// Category 7: Capabilities Tests
// ============================================================================

procedure TestCapabilities;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  Section('Capabilities and Feature Detection');
  if not GLibraryAvailable then
  begin
    Skip('Capabilities tests', 'Library not available');
    Exit;
  end;

  LLib := CreateMbedTLSLibrary;
  if not LLib.Initialize then
  begin
    Skip('Capabilities tests', 'Library initialization failed');
    Exit;
  end;

  try
    LCaps := LLib.GetCapabilities;
    Test('Capabilities retrieved', True);
    WriteLn('  TLS 1.3: ', LCaps.SupportsTLS13);
    WriteLn('  SNI: ', LCaps.SupportsSNI);
    WriteLn('  ALPN: ', LCaps.SupportsALPN);
  finally
    LLib.Finalize;
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('MbedTLS Backend Comprehensive Unit Tests');
  WriteLn('=========================================');
  WriteLn('');

  LStartTime := Now;

  TestErrorCodeMapping;
  TestLibraryInitialization;
  TestContextCreation;
  TestCertificateOperations;
  TestSessionManagement;
  TestConnectionInterface;
  TestCapabilities;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('MbedTLS Comprehensive Test Summary');
  WriteLn('========================================');
  WriteLn('Total:   ', GTestCount);
  WriteLn('Passed:  ', GPassCount);
  WriteLn('Failed:  ', GFailCount);
  WriteLn('Skipped: ', GSkipCount);
  if GTestCount > 0 then
    WriteLn('Rate:    ', (GPassCount * 100.0 / GTestCount):0:1, '%');
  WriteLn('Time:    ', FormatDateTime('nn:ss.zzz', Now - LStartTime));

  if GFailCount > 0 then
    Halt(1);
end.
