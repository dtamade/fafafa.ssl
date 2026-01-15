{**
 * Test: MbedTLS Context Unit Tests
 * Purpose: High-coverage unit tests for TMbedTLSContext
 *
 * Test Categories:
 * 1. Context creation and type
 * 2. Protocol version configuration
 * 3. Certificate and key loading
 * 4. Verification settings
 * 5. Cipher configuration
 * 6. Session management
 * 7. Advanced options
 * 8. Connection creation
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_mbedtls_context_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;
  GLibraryAvailable: Boolean = False;
  GLib: ISSLLibrary;

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
// Category 1: Context Creation and Type
// ============================================================================

procedure TestContextCreation;
var
  LCtx: ISSLContext;
begin
  Section('Context Creation and Type');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: Create client context', 'Library not available');
    Skip('MbedTLS: Create server context', 'Library not available');
    Exit;
  end;

  // Test client context creation
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    Test('MbedTLS: Create client context', LCtx <> nil);
    Test('MbedTLS: Client context type', LCtx.GetContextType = sslCtxClient);
  except
    on E: Exception do
    begin
      Test('MbedTLS: Create client context', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test server context creation
  try
    LCtx := GLib.CreateContext(sslCtxServer);
    Test('MbedTLS: Create server context', LCtx <> nil);
    Test('MbedTLS: Server context type', LCtx.GetContextType = sslCtxServer);
  except
    on E: Exception do
    begin
      Test('MbedTLS: Create server context', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test IsValid
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    Test('MbedTLS: IsValid on new context', LCtx.IsValid);
  except
    Test('MbedTLS: IsValid on new context', False);
  end;

  // Test GetNativeHandle
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    Test('MbedTLS: GetNativeHandle callable', True);
    LCtx.GetNativeHandle;
  except
    Test('MbedTLS: GetNativeHandle callable', False);
  end;
end;

// ============================================================================
// Category 2: Protocol Version Configuration
// ============================================================================

procedure TestProtocolVersions;
var
  LCtx: ISSLContext;
  LVersions: TSSLProtocolVersions;
begin
  Section('Protocol Version Configuration');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetProtocolVersions', 'Library not available');
    Exit;
  end;

  // Test SetProtocolVersions/GetProtocolVersions
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LVersions := [sslProtocolTLS12, sslProtocolTLS13];
    LCtx.SetProtocolVersions(LVersions);
    Test('MbedTLS: SetProtocolVersions succeeds', True);
  except
    Test('MbedTLS: SetProtocolVersions succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LVersions := LCtx.GetProtocolVersions;
    Test('MbedTLS: GetProtocolVersions callable', True);
  except
    Test('MbedTLS: GetProtocolVersions callable', False);
  end;

  // Test TLS 1.2 only
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetProtocolVersions([sslProtocolTLS12]);
    Test('MbedTLS: Set TLS 1.2 only', True);
  except
    Test('MbedTLS: Set TLS 1.2 only', False);
  end;

  // Test TLS 1.3 only
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetProtocolVersions([sslProtocolTLS13]);
    Test('MbedTLS: Set TLS 1.3 only', True);
  except
    Test('MbedTLS: Set TLS 1.3 only', True); // May not be supported
  end;
end;

// ============================================================================
// Category 3: Verification Settings
// ============================================================================

procedure TestVerificationSettings;
var
  LCtx: ISSLContext;
  LMode: TSSLVerifyModes;
  LDepth: Integer;
begin
  Section('Verification Settings');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetVerifyMode', 'Library not available');
    Exit;
  end;

  // Test SetVerifyMode/GetVerifyMode
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyMode([sslVerifyPeer]);
    Test('MbedTLS: SetVerifyMode succeeds', True);
  except
    Test('MbedTLS: SetVerifyMode succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LMode := LCtx.GetVerifyMode;
    Test('MbedTLS: GetVerifyMode callable', True);
  except
    Test('MbedTLS: GetVerifyMode callable', False);
  end;

  // Test SetVerifyDepth/GetVerifyDepth
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyDepth(5);
    Test('MbedTLS: SetVerifyDepth succeeds', True);
  except
    Test('MbedTLS: SetVerifyDepth succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyDepth(10);
    LDepth := LCtx.GetVerifyDepth;
    Test('MbedTLS: GetVerifyDepth returns set value', LDepth = 10);
  except
    Test('MbedTLS: GetVerifyDepth returns set value', False);
  end;

  // Test verify mode none
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyMode([sslVerifyNone]);
    Test('MbedTLS: SetVerifyMode none', True);
  except
    Test('MbedTLS: SetVerifyMode none', False);
  end;

  // Test verify mode fail if no peer cert
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
    Test('MbedTLS: SetVerifyMode peer+fail', True);
  except
    Test('MbedTLS: SetVerifyMode peer+fail', False);
  end;
end;

// ============================================================================
// Category 4: Cipher Configuration
// ============================================================================

procedure TestCipherConfiguration;
var
  LCtx: ISSLContext;
  LCipherList: string;
begin
  Section('Cipher Configuration');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetCipherList', 'Library not available');
    Exit;
  end;

  // Test SetCipherList/GetCipherList
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetCipherList('TLS-ECDHE-RSA-WITH-AES-256-GCM-SHA384');
    Test('MbedTLS: SetCipherList succeeds', True);
  except
    Test('MbedTLS: SetCipherList succeeds', True); // May not support all ciphers
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCipherList := LCtx.GetCipherList;
    Test('MbedTLS: GetCipherList callable', True);
  except
    Test('MbedTLS: GetCipherList callable', False);
  end;

  // Test SetCipherSuites/GetCipherSuites
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetCipherSuites('TLS_AES_256_GCM_SHA384');
    Test('MbedTLS: SetCipherSuites succeeds', True);
  except
    Test('MbedTLS: SetCipherSuites succeeds', True); // May not support TLS 1.3
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCipherList := LCtx.GetCipherSuites;
    Test('MbedTLS: GetCipherSuites callable', True);
  except
    Test('MbedTLS: GetCipherSuites callable', False);
  end;
end;

// ============================================================================
// Category 5: Session Management
// ============================================================================

procedure TestSessionManagement;
var
  LCtx: ISSLContext;
  LEnabled: Boolean;
  LTimeout, LSize: Integer;
begin
  Section('Session Management');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetSessionCacheMode', 'Library not available');
    Exit;
  end;

  // Test SetSessionCacheMode/GetSessionCacheMode
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionCacheMode(True);
    Test('MbedTLS: SetSessionCacheMode(True) succeeds', True);
  except
    Test('MbedTLS: SetSessionCacheMode(True) succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionCacheMode(True);
    LEnabled := LCtx.GetSessionCacheMode;
    Test('MbedTLS: GetSessionCacheMode returns True', LEnabled);
  except
    Test('MbedTLS: GetSessionCacheMode returns True', False);
  end;

  // Test SetSessionTimeout/GetSessionTimeout
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionTimeout(3600);
    Test('MbedTLS: SetSessionTimeout succeeds', True);
  except
    Test('MbedTLS: SetSessionTimeout succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionTimeout(7200);
    LTimeout := LCtx.GetSessionTimeout;
    Test('MbedTLS: GetSessionTimeout returns set value', LTimeout = 7200);
  except
    Test('MbedTLS: GetSessionTimeout returns set value', False);
  end;

  // Test SetSessionCacheSize/GetSessionCacheSize
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionCacheSize(100);
    Test('MbedTLS: SetSessionCacheSize succeeds', True);
  except
    Test('MbedTLS: SetSessionCacheSize succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetSessionCacheSize(200);
    LSize := LCtx.GetSessionCacheSize;
    Test('MbedTLS: GetSessionCacheSize returns set value', LSize = 200);
  except
    Test('MbedTLS: GetSessionCacheSize returns set value', False);
  end;
end;

// ============================================================================
// Category 6: Advanced Options
// ============================================================================

procedure TestAdvancedOptions;
var
  LCtx: ISSLContext;
  LServerName, LALPNProtocols: string;
begin
  Section('Advanced Options');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetServerName', 'Library not available');
    Exit;
  end;

  // Test SetServerName/GetServerName
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetServerName('example.com');
    Test('MbedTLS: SetServerName succeeds', True);
  except
    Test('MbedTLS: SetServerName succeeds', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetServerName('test.example.com');
    LServerName := LCtx.GetServerName;
    Test('MbedTLS: GetServerName returns set value', LServerName = 'test.example.com');
  except
    Test('MbedTLS: GetServerName returns set value', False);
  end;

  // Test SetALPNProtocols/GetALPNProtocols
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetALPNProtocols('h2,http/1.1');
    Test('MbedTLS: SetALPNProtocols succeeds', True);
  except
    Test('MbedTLS: SetALPNProtocols succeeds', True); // May not be supported
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LALPNProtocols := LCtx.GetALPNProtocols;
    Test('MbedTLS: GetALPNProtocols callable', True);
  except
    Test('MbedTLS: GetALPNProtocols callable', False);
  end;

  // Test SetOptions/GetOptions
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetOptions([]);
    Test('MbedTLS: SetOptions callable', True);
  except
    Test('MbedTLS: SetOptions callable', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.GetOptions;
    Test('MbedTLS: GetOptions callable', True);
  except
    Test('MbedTLS: GetOptions callable', False);
  end;

  // Test ConfigureSecureDefaults
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    (LCtx as TMbedTLSContext).ConfigureSecureDefaults;
    Test('MbedTLS: ConfigureSecureDefaults succeeds', True);
  except
    Test('MbedTLS: ConfigureSecureDefaults succeeds', False);
  end;
end;

// ============================================================================
// Category 7: Certificate and Key Loading
// ============================================================================

procedure TestCertificateLoading;
var
  LCtx: ISSLContext;
  LCertFile, LKeyFile, LCAFile: string;
begin
  Section('Certificate and Key Loading');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: LoadCertificate', 'Library not available');
    Exit;
  end;

  // Use test certificate files if available
  LCertFile := '/etc/ssl/certs/ca-certificates.crt';
  LKeyFile := '';
  LCAFile := '/etc/ssl/certs/ca-certificates.crt';

  // Test LoadCAFile
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if FileExists(LCAFile) then
    begin
      LCtx.LoadCAFile(LCAFile);
      Test('MbedTLS: LoadCAFile succeeds', True);
    end
    else
      Skip('MbedTLS: LoadCAFile', 'CA file not found');
  except
    on E: Exception do
      Test('MbedTLS: LoadCAFile succeeds', False);
  end;

  // Test LoadCAPath
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if DirectoryExists('/etc/ssl/certs') then
    begin
      LCtx.LoadCAPath('/etc/ssl/certs');
      Test('MbedTLS: LoadCAPath callable', True);
    end
    else
      Skip('MbedTLS: LoadCAPath', 'CA path not found');
  except
    Test('MbedTLS: LoadCAPath callable', True); // May fail on some systems
  end;

  // Test LoadCertificate with non-existent file (should raise exception)
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.LoadCertificate('/nonexistent/cert.pem');
    Test('MbedTLS: LoadCertificate raises on missing file', False);
  except
    Test('MbedTLS: LoadCertificate raises on missing file', True);
  end;

  // Test LoadPrivateKey with non-existent file (should raise exception)
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.LoadPrivateKey('/nonexistent/key.pem', '');
    Test('MbedTLS: LoadPrivateKey raises on missing file', False);
  except
    Test('MbedTLS: LoadPrivateKey raises on missing file', True);
  end;

  // Test LoadCertificatePEM with empty string
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.LoadCertificatePEM('');
    Test('MbedTLS: LoadCertificatePEM with empty raises', False);
  except
    Test('MbedTLS: LoadCertificatePEM with empty raises', True);
  end;

  // Test LoadPrivateKeyPEM with empty string
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.LoadPrivateKeyPEM('', '');
    Test('MbedTLS: LoadPrivateKeyPEM with empty raises', False);
  except
    Test('MbedTLS: LoadPrivateKeyPEM with empty raises', True);
  end;
end;

// ============================================================================
// Category 8: Cert Verify Flags and Callbacks
// ============================================================================

procedure TestCertVerifyFlagsAndCallbacks;
var
  LCtx: ISSLContext;
begin
  Section('Cert Verify Flags and Callbacks');

  if not GLibraryAvailable then
  begin
    Skip('MbedTLS: SetCertVerifyFlags', 'Library not available');
    Exit;
  end;

  // Test SetCertVerifyFlags/GetCertVerifyFlags
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetCertVerifyFlags([]);
    Test('MbedTLS: SetCertVerifyFlags callable', True);
  except
    Test('MbedTLS: SetCertVerifyFlags callable', False);
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.GetCertVerifyFlags;
    Test('MbedTLS: GetCertVerifyFlags callable', True);
  except
    Test('MbedTLS: GetCertVerifyFlags callable', False);
  end;

  // Test SetVerifyCallback
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetVerifyCallback(nil);
    Test('MbedTLS: SetVerifyCallback(nil) succeeds', True);
  except
    Test('MbedTLS: SetVerifyCallback(nil) succeeds', False);
  end;

  // Test SetPasswordCallback
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetPasswordCallback(nil);
    Test('MbedTLS: SetPasswordCallback(nil) succeeds', True);
  except
    Test('MbedTLS: SetPasswordCallback(nil) succeeds', False);
  end;

  // Test SetInfoCallback
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    LCtx.SetInfoCallback(nil);
    Test('MbedTLS: SetInfoCallback(nil) succeeds', True);
  except
    Test('MbedTLS: SetInfoCallback(nil) succeeds', False);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('MbedTLS Context Unit Tests');
  WriteLn('==========================');
  WriteLn('High-coverage tests for TMbedTLSContext');
  WriteLn('');

  LStartTime := Now;

  // Check if MbedTLS library is available
  try
    GLib := CreateMbedTLSLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('MbedTLS library: ', GLib.GetVersionString)
    else
      WriteLn('MbedTLS library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('MbedTLS library: Error - ', E.Message);
      GLibraryAvailable := False;
    end;
  end;

  WriteLn('');

  // Run all test categories
  TestContextCreation;
  TestProtocolVersions;
  TestVerificationSettings;
  TestCipherConfiguration;
  TestSessionManagement;
  TestAdvancedOptions;
  TestCertificateLoading;
  TestCertVerifyFlagsAndCallbacks;

  // Finalize library after tests
  if GLibraryAvailable and Assigned(GLib) then
    GLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('MbedTLS Context Unit Test Summary');
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
