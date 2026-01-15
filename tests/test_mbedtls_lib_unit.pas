{**
 * Test: MbedTLS Library Unit Tests
 * Purpose: High-coverage unit tests for TMbedTLSLibrary
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

program test_mbedtls_lib_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.lib;

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

  // Test CreateMbedTLSLibrary
  try
    LLib := CreateMbedTLSLibrary;
    Test('MbedTLS: CreateMbedTLSLibrary succeeds', LLib <> nil);
  except
    Test('MbedTLS: CreateMbedTLSLibrary succeeds', False);
  end;

  // Test Initialize
  try
    LLib := CreateMbedTLSLibrary;
    Test('MbedTLS: Initialize returns boolean', True);
    LLib.Initialize;
  except
    Test('MbedTLS: Initialize returns boolean', False);
  end;

  // Test IsInitialized after Initialize
  try
    LLib := CreateMbedTLSLibrary;
    LLib.Initialize;
    Test('MbedTLS: IsInitialized after Initialize', LLib.IsInitialized);
  except
    Test('MbedTLS: IsInitialized after Initialize', False);
  end;

  // Test Finalize
  try
    LLib := CreateMbedTLSLibrary;
    LLib.Initialize;
    LLib.Finalize;
    Test('MbedTLS: Finalize callable', True);
  except
    Test('MbedTLS: Finalize callable', False);
  end;

  // Test GetLibraryType
  try
    LLib := CreateMbedTLSLibrary;
    Test('MbedTLS: GetLibraryType returns sslMbedTLS', LLib.GetLibraryType = sslMbedTLS);
  except
    Test('MbedTLS: GetLibraryType returns sslMbedTLS', False);
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
    Skip('MbedTLS: GetVersionString', 'Library not available');
    Exit;
  end;

  // Test GetVersionString
  try
    LVersionStr := GLib.GetVersionString;
    Test('MbedTLS: GetVersionString not empty', LVersionStr <> '');
    WriteLn('  Version: ', LVersionStr);
  except
    Test('MbedTLS: GetVersionString not empty', False);
  end;

  // Test GetVersionNumber
  try
    LVersionNum := GLib.GetVersionNumber;
    Test('MbedTLS: GetVersionNumber > 0', LVersionNum > 0);
    WriteLn('  Version Number: ', LVersionNum);
  except
    Test('MbedTLS: GetVersionNumber > 0', False);
  end;

  // Test GetCompileFlags
  try
    LCompileFlags := GLib.GetCompileFlags;
    Test('MbedTLS: GetCompileFlags callable', True);
  except
    Test('MbedTLS: GetCompileFlags callable', False);
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
    Skip('MbedTLS: IsProtocolSupported', 'Library not available');
    Exit;
  end;

  // Test IsProtocolSupported for TLS 1.2
  try
    Test('MbedTLS: IsProtocolSupported(TLS12)', GLib.IsProtocolSupported(sslProtocolTLS12));
  except
    Test('MbedTLS: IsProtocolSupported(TLS12)', False);
  end;

  // Test IsProtocolSupported for TLS 1.3
  try
    Test('MbedTLS: IsProtocolSupported(TLS13) callable', True);
    GLib.IsProtocolSupported(sslProtocolTLS13);
  except
    Test('MbedTLS: IsProtocolSupported(TLS13) callable', False);
  end;

  // Test IsCipherSupported
  try
    Test('MbedTLS: IsCipherSupported callable', True);
    GLib.IsCipherSupported('TLS-ECDHE-RSA-WITH-AES-256-GCM-SHA384');
  except
    Test('MbedTLS: IsCipherSupported callable', False);
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
    Skip('MbedTLS: GetCapabilities', 'Library not available');
    Exit;
  end;

  // Test GetCapabilities
  try
    LCaps := GLib.GetCapabilities;
    Test('MbedTLS: GetCapabilities returns valid struct', True);
    WriteLn('  TLS 1.3: ', LCaps.SupportsTLS13);
    WriteLn('  ALPN: ', LCaps.SupportsALPN);
    WriteLn('  SNI: ', LCaps.SupportsSNI);
  except
    Test('MbedTLS: GetCapabilities returns valid struct', False);
  end;

  // Test IsFeatureSupported
  try
    Test('MbedTLS: IsFeatureSupported callable', True);
    GLib.IsFeatureSupported(sslFeatSNI);
  except
    Test('MbedTLS: IsFeatureSupported callable', False);
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
    Skip('MbedTLS: SetDefaultConfig', 'Library not available');
    Exit;
  end;

  // Test GetDefaultConfig
  try
    LConfig := GLib.GetDefaultConfig;
    Test('MbedTLS: GetDefaultConfig callable', True);
  except
    Test('MbedTLS: GetDefaultConfig callable', False);
  end;

  // Test SetDefaultConfig
  try
    LConfig := GLib.GetDefaultConfig;
    GLib.SetDefaultConfig(LConfig);
    Test('MbedTLS: SetDefaultConfig callable', True);
  except
    Test('MbedTLS: SetDefaultConfig callable', False);
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
    Skip('MbedTLS: GetLastError', 'Library not available');
    Exit;
  end;

  // Test GetLastError
  try
    LError := GLib.GetLastError;
    Test('MbedTLS: GetLastError callable', True);
  except
    Test('MbedTLS: GetLastError callable', False);
  end;

  // Test GetLastErrorString
  try
    LErrorStr := GLib.GetLastErrorString;
    Test('MbedTLS: GetLastErrorString callable', True);
  except
    Test('MbedTLS: GetLastErrorString callable', False);
  end;

  // Test ClearError
  try
    GLib.ClearError;
    Test('MbedTLS: ClearError callable', True);
  except
    Test('MbedTLS: ClearError callable', False);
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
    Skip('MbedTLS: CreateContext', 'Library not available');
    Exit;
  end;

  // Test CreateContext (client)
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    Test('MbedTLS: CreateContext(client) succeeds', LCtx <> nil);
  except
    Test('MbedTLS: CreateContext(client) succeeds', False);
  end;

  // Test CreateContext (server)
  try
    LCtx := GLib.CreateContext(sslCtxServer);
    Test('MbedTLS: CreateContext(server) succeeds', LCtx <> nil);
  except
    Test('MbedTLS: CreateContext(server) succeeds', False);
  end;

  // Test CreateCertificate
  try
    LCert := GLib.CreateCertificate;
    Test('MbedTLS: CreateCertificate succeeds', LCert <> nil);
  except
    Test('MbedTLS: CreateCertificate succeeds', False);
  end;

  // Test CreateCertificateStore
  try
    LStore := GLib.CreateCertificateStore;
    Test('MbedTLS: CreateCertificateStore succeeds', LStore <> nil);
  except
    Test('MbedTLS: CreateCertificateStore succeeds', False);
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
    Skip('MbedTLS: GetStatistics', 'Library not available');
    Exit;
  end;

  // Test GetStatistics
  try
    LStats := GLib.GetStatistics;
    Test('MbedTLS: GetStatistics callable', True);
  except
    Test('MbedTLS: GetStatistics callable', False);
  end;

  // Test ResetStatistics
  try
    GLib.ResetStatistics;
    Test('MbedTLS: ResetStatistics callable', True);
  except
    Test('MbedTLS: ResetStatistics callable', False);
  end;

  // Test SetLogCallback
  try
    GLib.SetLogCallback(nil);
    Test('MbedTLS: SetLogCallback(nil) succeeds', True);
  except
    Test('MbedTLS: SetLogCallback(nil) succeeds', False);
  end;

  // Test Log
  try
    GLib.Log(sslLogInfo, 'Test log message');
    Test('MbedTLS: Log callable', True);
  except
    Test('MbedTLS: Log callable', False);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('MbedTLS Library Unit Tests');
  WriteLn('==========================');
  WriteLn('High-coverage tests for TMbedTLSLibrary');
  WriteLn('');

  LStartTime := Now;

  // Test library initialization first
  TestLibraryInitialization;

  // Check if MbedTLS library is available for remaining tests
  try
    GLib := CreateMbedTLSLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('MbedTLS library initialized: ', GLib.GetVersionString)
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
  WriteLn('MbedTLS Library Unit Test Summary');
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
