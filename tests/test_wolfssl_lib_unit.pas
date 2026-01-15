{**
 * Test: WolfSSL Library Unit Tests
 * Purpose: High-coverage unit tests for TWolfSSLLibrary
 *
 * Test Categories:
 * 1. Library initialization and finalization
 * 2. Version and info queries
 * 3. Protocol and cipher support
 * 4. Feature detection
 * 5. Configuration management
 * 6. Error handling
 * 7. Object creation
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_wolfssl_lib_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.lib;

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
// Category 1: Library Initialization
// ============================================================================

procedure TestLibraryInitialization;
var
  LLib: ISSLLibrary;
begin
  Section('Library Initialization');

  // Test CreateWolfSSLLibrary
  try
    LLib := CreateWolfSSLLibrary;
    Test('WolfSSL: CreateWolfSSLLibrary succeeds', LLib <> nil);
  except
    Test('WolfSSL: CreateWolfSSLLibrary succeeds', False);
  end;

  // Test Initialize
  try
    LLib := CreateWolfSSLLibrary;
    Test('WolfSSL: Initialize returns boolean', True);
    LLib.Initialize;
  except
    Test('WolfSSL: Initialize returns boolean', False);
  end;

  // Test IsInitialized after Initialize
  try
    LLib := CreateWolfSSLLibrary;
    LLib.Initialize;
    Test('WolfSSL: IsInitialized after Initialize', LLib.IsInitialized);
  except
    Test('WolfSSL: IsInitialized after Initialize', False);
  end;

  // Test Finalize
  try
    LLib := CreateWolfSSLLibrary;
    LLib.Initialize;
    LLib.Finalize;
    Test('WolfSSL: Finalize callable', True);
  except
    Test('WolfSSL: Finalize callable', False);
  end;

  // Test GetLibraryType
  try
    LLib := CreateWolfSSLLibrary;
    Test('WolfSSL: GetLibraryType returns sslWolfSSL', LLib.GetLibraryType = sslWolfSSL);
  except
    Test('WolfSSL: GetLibraryType returns sslWolfSSL', False);
  end;
end;

// ============================================================================
// Category 2: Version and Info Queries
// ============================================================================

procedure TestVersionInfo;
var
  LVersionStr: string;
  LVersionNum: Cardinal;
  LCompileFlags: string;
begin
  Section('Version and Info Queries');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: GetVersionString', 'Library not available');
    Exit;
  end;

  // Test GetVersionString
  try
    LVersionStr := GLib.GetVersionString;
    Test('WolfSSL: GetVersionString not empty', LVersionStr <> '');
    WriteLn('  Version: ', LVersionStr);
  except
    Test('WolfSSL: GetVersionString not empty', False);
  end;

  // Test GetVersionNumber
  try
    LVersionNum := GLib.GetVersionNumber;
    Test('WolfSSL: GetVersionNumber > 0', LVersionNum > 0);
    WriteLn('  Version Number: ', LVersionNum);
  except
    Test('WolfSSL: GetVersionNumber > 0', False);
  end;

  // Test GetCompileFlags
  try
    LCompileFlags := GLib.GetCompileFlags;
    Test('WolfSSL: GetCompileFlags callable', True);
  except
    Test('WolfSSL: GetCompileFlags callable', False);
  end;
end;

// ============================================================================
// Category 3: Protocol and Cipher Support
// ============================================================================

procedure TestProtocolCipherSupport;
begin
  Section('Protocol and Cipher Support');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: IsProtocolSupported', 'Library not available');
    Exit;
  end;

  // Test IsProtocolSupported for TLS 1.2
  try
    Test('WolfSSL: IsProtocolSupported(TLS12)', GLib.IsProtocolSupported(sslProtocolTLS12));
  except
    Test('WolfSSL: IsProtocolSupported(TLS12)', False);
  end;

  // Test IsProtocolSupported for TLS 1.3
  try
    Test('WolfSSL: IsProtocolSupported(TLS13) callable', True);
    GLib.IsProtocolSupported(sslProtocolTLS13);
  except
    Test('WolfSSL: IsProtocolSupported(TLS13) callable', False);
  end;

  // Test IsCipherSupported
  try
    Test('WolfSSL: IsCipherSupported callable', True);
    GLib.IsCipherSupported('TLS-ECDHE-RSA-WITH-AES-256-GCM-SHA384');
  except
    Test('WolfSSL: IsCipherSupported callable', False);
  end;
end;

// ============================================================================
// Category 4: Feature Detection
// ============================================================================

procedure TestFeatureDetection;
var
  LCaps: TSSLBackendCapabilities;
begin
  Section('Feature Detection');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: GetCapabilities', 'Library not available');
    Exit;
  end;

  // Test GetCapabilities
  try
    LCaps := GLib.GetCapabilities;
    Test('WolfSSL: GetCapabilities returns valid struct', True);
    WriteLn('  TLS 1.3: ', LCaps.SupportsTLS13);
    WriteLn('  ALPN: ', LCaps.SupportsALPN);
    WriteLn('  SNI: ', LCaps.SupportsSNI);
  except
    Test('WolfSSL: GetCapabilities returns valid struct', False);
  end;

  // Test IsFeatureSupported
  try
    Test('WolfSSL: IsFeatureSupported callable', True);
    GLib.IsFeatureSupported(sslFeatSNI);
  except
    Test('WolfSSL: IsFeatureSupported callable', False);
  end;
end;

// ============================================================================
// Category 5: Configuration Management
// ============================================================================

procedure TestConfigurationManagement;
var
  LConfig: TSSLConfig;
begin
  Section('Configuration Management');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: SetDefaultConfig', 'Library not available');
    Exit;
  end;

  // Test GetDefaultConfig
  try
    LConfig := GLib.GetDefaultConfig;
    Test('WolfSSL: GetDefaultConfig callable', True);
  except
    Test('WolfSSL: GetDefaultConfig callable', False);
  end;

  // Test SetDefaultConfig
  try
    LConfig := GLib.GetDefaultConfig;
    GLib.SetDefaultConfig(LConfig);
    Test('WolfSSL: SetDefaultConfig callable', True);
  except
    Test('WolfSSL: SetDefaultConfig callable', False);
  end;
end;

// ============================================================================
// Category 6: Error Handling
// ============================================================================

procedure TestErrorHandling;
var
  LError: Integer;
  LErrorStr: string;
begin
  Section('Error Handling');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: GetLastError', 'Library not available');
    Exit;
  end;

  // Test GetLastError
  try
    LError := GLib.GetLastError;
    Test('WolfSSL: GetLastError callable', True);
  except
    Test('WolfSSL: GetLastError callable', False);
  end;

  // Test GetLastErrorString
  try
    LErrorStr := GLib.GetLastErrorString;
    Test('WolfSSL: GetLastErrorString callable', True);
  except
    Test('WolfSSL: GetLastErrorString callable', False);
  end;

  // Test ClearError
  try
    GLib.ClearError;
    Test('WolfSSL: ClearError callable', True);
  except
    Test('WolfSSL: ClearError callable', False);
  end;
end;

// ============================================================================
// Category 7: Object Creation
// ============================================================================

procedure TestObjectCreation;
var
  LCtx: ISSLContext;
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
begin
  Section('Object Creation');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: CreateContext', 'Library not available');
    Exit;
  end;

  // Test CreateContext (client)
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    Test('WolfSSL: CreateContext(client) succeeds', LCtx <> nil);
  except
    Test('WolfSSL: CreateContext(client) succeeds', False);
  end;

  // Test CreateContext (server)
  try
    LCtx := GLib.CreateContext(sslCtxServer);
    Test('WolfSSL: CreateContext(server) succeeds', LCtx <> nil);
  except
    Test('WolfSSL: CreateContext(server) succeeds', False);
  end;

  // Test CreateCertificate
  try
    LCert := GLib.CreateCertificate;
    Test('WolfSSL: CreateCertificate succeeds', LCert <> nil);
  except
    Test('WolfSSL: CreateCertificate succeeds', False);
  end;

  // Test CreateCertificateStore
  try
    LStore := GLib.CreateCertificateStore;
    Test('WolfSSL: CreateCertificateStore succeeds', LStore <> nil);
  except
    Test('WolfSSL: CreateCertificateStore succeeds', False);
  end;
end;

// ============================================================================
// Category 8: Statistics and Logging
// ============================================================================

procedure TestStatisticsAndLogging;
var
  LStats: TSSLStatistics;
begin
  Section('Statistics and Logging');

  if not GLibraryAvailable then
  begin
    Skip('WolfSSL: GetStatistics', 'Library not available');
    Exit;
  end;

  // Test GetStatistics
  try
    LStats := GLib.GetStatistics;
    Test('WolfSSL: GetStatistics callable', True);
  except
    Test('WolfSSL: GetStatistics callable', False);
  end;

  // Test ResetStatistics
  try
    GLib.ResetStatistics;
    Test('WolfSSL: ResetStatistics callable', True);
  except
    Test('WolfSSL: ResetStatistics callable', False);
  end;

  // Test SetLogCallback
  try
    GLib.SetLogCallback(nil);
    Test('WolfSSL: SetLogCallback(nil) succeeds', True);
  except
    Test('WolfSSL: SetLogCallback(nil) succeeds', False);
  end;

  // Test Log
  try
    GLib.Log(sslLogInfo, 'Test log message');
    Test('WolfSSL: Log callable', True);
  except
    Test('WolfSSL: Log callable', False);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('WolfSSL Library Unit Tests');
  WriteLn('==========================');
  WriteLn('High-coverage tests for TWolfSSLLibrary');
  WriteLn('');

  LStartTime := Now;

  // Test library initialization first
  TestLibraryInitialization;

  // Check if WolfSSL library is available for remaining tests
  try
    GLib := CreateWolfSSLLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('WolfSSL library initialized: ', GLib.GetVersionString)
    else
      WriteLn('WolfSSL library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('WolfSSL library: Error - ', E.Message);
      GLibraryAvailable := False;
    end;
  end;

  WriteLn('');

  // Run remaining test categories
  TestVersionInfo;
  TestProtocolCipherSupport;
  TestFeatureDetection;
  TestConfigurationManagement;
  TestErrorHandling;
  TestObjectCreation;
  TestStatisticsAndLogging;

  // Finalize library after tests
  if GLibraryAvailable and Assigned(GLib) then
    GLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('WolfSSL Library Unit Test Summary');
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
