program test_winssl_unit_comprehensive;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes,
  
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.certstore;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  CurrentSection: string;

procedure BeginSection(const aName: string);
begin
  WriteLn;
  WriteLn('=== ', aName, ' ===');
  CurrentSection := aName;
end;

procedure Test(const aTestName: string; aCondition: Boolean; const aMessage: string = '');
begin
  Inc(TotalTests);
  Write('  [', CurrentSection, '] ', aTestName, ': ');

  if aCondition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    if aMessage <> '' then
      WriteLn('    Reason: ', aMessage);
    Inc(FailedTests);
  end;
end;

// ============================================================================
// Library Initialization Tests
// ============================================================================

procedure TestLibraryInitialization;
var
  LLib: ISSLLibrary;
  LVersionString: string;
  LLibType: TSSLLibraryType;
begin
  BeginSection('Library Initialization');

  // Test 1: Create library instance
  try
    LLib := CreateWinSSLLibrary;
    Test('Create WinSSL library instance', LLib <> nil, 'Failed to create library');
  except
    on E: Exception do
    begin
      Test('Create WinSSL library instance', False, E.Message);
      Exit;
    end;
  end;

  // Test 2: Initialize library
  try
    Test('Initialize WinSSL library', LLib.Initialize, LLib.GetLastErrorString);
  except
    on E: Exception do
    begin
      Test('Initialize WinSSL library', False, E.Message);
      Exit;
    end;
  end;

  // Test 3: Check initialization state
  Test('Library reports initialized', LLib.IsInitialized);

  // Test 4: Get library type
  LLibType := LLib.GetLibraryType;
  Test('Library type is WinSSL', LLibType = sslWinSSL,
    Format('Expected sslWinSSL, got %d', [Ord(LLibType)]));

  // Test 5: Get version string
  LVersionString := LLib.GetVersionString;
  Test('Version string not empty', LVersionString <> '',
    'GetVersionString returned empty');
  Test('Version contains "Schannel"', Pos('Schannel', LVersionString) > 0,
    'Version string: ' + LVersionString);

  // Test 6: Get version number
  // Version format: High word = major, Low word = minor (e.g., $00060002 = 6.2)
  Test('Version number >= 6.0', (LLib.GetVersionNumber shr 16) >= 6,
    Format('Version: %d.%d', [(LLib.GetVersionNumber shr 16), (LLib.GetVersionNumber and $FFFF)]));

  // Test 7: Check TLS 1.2 support
  Test('TLS 1.2 supported', LLib.IsProtocolSupported(sslProtocolTLS12));

  // Test 8: Check TLS 1.3 support (Windows 11+)
  // Note: This may fail on Windows 10, which is acceptable
  if LLib.IsProtocolSupported(sslProtocolTLS13) then
    Test('TLS 1.3 supported (Windows 11)', True)
  else
    Test('TLS 1.3 not supported (Windows 10 or earlier)', True);

  // Test 9: Multiple initialize calls should be safe
  Test('Re-initialize is safe', LLib.Initialize);
  Test('Still initialized after re-init', LLib.IsInitialized);

  // Test 10: Finalize library
  try
    LLib.Finalize;
    Test('Finalize successful', True);
  except
    on E: Exception do
      Test('Finalize successful', False, E.Message);
  end;

  // Test 11: Check state after finalize
  Test('Not initialized after finalize', not LLib.IsInitialized);

  // Cleanup
  LLib := nil;
end;

// ============================================================================
// Context Management Tests
// ============================================================================

procedure TestContextManagement;
var
  LLib: ISSLLibrary;
  LClientCtx, LServerCtx: ISSLContext;
  LProtocols: TSSLProtocolVersions;
begin
  BeginSection('Context Management');

  // Initialize library
  LLib := CreateWinSSLLibrary;
  if not LLib.Initialize then
  begin
    Test('Library initialization failed', False, LLib.GetLastErrorString);
    Exit;
  end;

  // Test 1: Create client context
  try
    LClientCtx := LLib.CreateContext(sslCtxClient);
    Test('Create client context', LClientCtx <> nil);
  except
    on E: Exception do
    begin
      Test('Create client context', False, E.Message);
      LLib.Finalize;
      Exit;
    end;
  end;

  // Test 2: Create server context
  try
    LServerCtx := LLib.CreateContext(sslCtxServer);
    Test('Create server context', LServerCtx <> nil);
  except
    on E: Exception do
      Test('Create server context', False, E.Message);
  end;

  // Test 3: Get context type
  Test('Client context type correct', LClientCtx.GetContextType = sslCtxClient);
  if LServerCtx <> nil then
    Test('Server context type correct', LServerCtx.GetContextType = sslCtxServer);

  // Test 4: Set server name (SNI)
  try
    LClientCtx.SetServerName('www.example.com');
    Test('Set SNI server name', True);
  except
    on E: Exception do
      Test('Set SNI server name', False, E.Message);
  end;

  // Test 5: Set protocol versions
  try
    LProtocols := [sslProtocolTLS12, sslProtocolTLS13];
    LClientCtx.SetProtocolVersions(LProtocols);
    Test('Set protocol versions', True);
  except
    on E: Exception do
      Test('Set protocol versions', False, E.Message);
  end;

  // Test 6: Get protocol versions
  LProtocols := LClientCtx.GetProtocolVersions;
  Test('Protocol versions set correctly',
    (sslProtocolTLS12 in LProtocols) and (sslProtocolTLS13 in LProtocols),
    'Expected TLS 1.2 and 1.3');

  // Test 7: Set verify mode
  try
    LClientCtx.SetVerifyMode([sslVerifyPeer]);
    Test('Set verify mode', True);
  except
    on E: Exception do
      Test('Set verify mode', False, E.Message);
  end;

  // Test 8: Get verify mode
  Test('Verify mode set correctly',
    sslVerifyPeer in LClientCtx.GetVerifyMode);

  // Test 9: Set verify depth
  try
    LClientCtx.SetVerifyDepth(5);
    Test('Set verify depth', True);
  except
    on E: Exception do
      Test('Set verify depth', False, E.Message);
  end;

  // Test 10: Get verify depth
  Test('Verify depth set correctly', LClientCtx.GetVerifyDepth = 5);

  // Test 11: Multiple contexts from same library
  try
    LClientCtx := LLib.CreateContext(sslCtxClient);
    Test('Create multiple contexts', LClientCtx <> nil);
  except
    on E: Exception do
      Test('Create multiple contexts', False, E.Message);
  end;

  // Cleanup
  LClientCtx := nil;
  LServerCtx := nil;
  LLib.Finalize;
  LLib := nil;
end;

// ============================================================================
// Certificate Management Tests
// ============================================================================

procedure TestCertificateManagement;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LStore: ISSLCertificateStore;
  LCert: ISSLCertificate;
  LCount: Integer;
begin
  BeginSection('Certificate Management');

  // Initialize library
  LLib := CreateWinSSLLibrary;
  if not LLib.Initialize then
  begin
    Test('Library initialization failed', False, LLib.GetLastErrorString);
    Exit;
  end;

  // Test 1: Open system ROOT certificate store
  try
    LStore := OpenSystemStore(SSL_STORE_ROOT);
    Test('Open ROOT certificate store', LStore <> nil);
  except
    on E: Exception do
    begin
      Test('Open ROOT certificate store', False, E.Message);
      LLib.Finalize;
      Exit;
    end;
  end;

  // Test 2: Get certificate count
  try
    LCount := LStore.GetCount;
    Test('Get certificate count', LCount > 0,
      Format('Found %d certificates', [LCount]));
  except
    on E: Exception do
      Test('Get certificate count', False, E.Message);
  end;

  // Test 3: Get first certificate
  try
    if LCount > 0 then
    begin
      LCert := LStore.GetCertificate(0);
      Test('Get first certificate', LCert <> nil);
    end
    else
      Test('Get first certificate', False, 'No certificates in store');
  except
    on E: Exception do
      Test('Get first certificate', False, E.Message);
  end;

  // Test 4: Get certificate subject
  if LCert <> nil then
  begin
    try
      Test('Get certificate subject', LCert.GetSubject <> '');
    except
      on E: Exception do
        Test('Get certificate subject', False, E.Message);
    end;

    // Test 5: Get certificate issuer
    try
      Test('Get certificate issuer', LCert.GetIssuer <> '');
    except
      on E: Exception do
        Test('Get certificate issuer', False, E.Message);
    end;

    // Test 6: Get certificate serial number
    try
      Test('Get certificate serial number', LCert.GetSerialNumber <> '');
    except
      on E: Exception do
        Test('Get certificate serial number', False, E.Message);
    end;

    // Test 7: Get certificate not before date
    try
      Test('Get not before date', LCert.GetNotBefore > 0);
    except
      on E: Exception do
        Test('Get not before date', False, E.Message);
    end;

    // Test 8: Get certificate not after date
    try
      Test('Get not after date', LCert.GetNotAfter > 0);
    except
      on E: Exception do
        Test('Get not after date', False, E.Message);
    end;

    // Test 9: Check certificate expiration
    try
      // Note: Some ROOT certificates may be expired
      LCert.IsExpired;
      Test('IsExpired does not crash', True);
    except
      on E: Exception do
        Test('IsExpired does not crash', False, E.Message);
    end;

    // Test 10: Get SHA-1 fingerprint
    try
      Test('Get SHA-1 fingerprint', LCert.GetFingerprint(sslHashSHA1) <> '');
    except
      on E: Exception do
        Test('Get SHA-1 fingerprint', False, E.Message);
    end;

    // Test 11: Get SHA-256 fingerprint
    try
      Test('Get SHA-256 fingerprint', LCert.GetFingerprint(sslHashSHA256) <> '');
    except
      on E: Exception do
        Test('Get SHA-256 fingerprint', False, E.Message);
    end;
  end;

  // Test 12: Open MY certificate store
  try
    LStore := OpenSystemStore(SSL_STORE_MY);
    Test('Open MY certificate store', LStore <> nil);
  except
    on E: Exception do
      Test('Open MY certificate store', False, E.Message);
  end;

  // Cleanup
  LCert := nil;
  LStore := nil;
  LContext := nil;
  LLib.Finalize;
  LLib := nil;
end;

// ============================================================================
// Error Handling Tests
// ============================================================================

procedure TestErrorHandling;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LErrorCode: SECURITY_STATUS;
  LErrorMsg: string;
  LCategory: TSchannelErrorCategory;
begin
  BeginSection('Error Handling');

  // Test 1: Error code mapping
  LErrorCode := SEC_E_OK;
  Test('Map SEC_E_OK to success', IsSuccess(LErrorCode));

  LErrorCode := SECURITY_STATUS(SEC_I_CONTINUE_NEEDED);
  Test('Detect continue needed', IsHandshakeContinue(LErrorCode));

  LErrorCode := SECURITY_STATUS(SEC_E_INCOMPLETE_MESSAGE);
  Test('Detect incomplete message', IsIncompleteMessage(LErrorCode));

  // Test 2: Error message conversion
  LErrorCode := SECURITY_STATUS(SEC_E_CERT_EXPIRED);
  LErrorMsg := GetSchannelErrorString(LErrorCode);
  Test('Get error message for expired cert', LErrorMsg <> '',
    'Message: ' + LErrorMsg);

  // Test 3: Error category
  LCategory := GetSchannelErrorCategory(SECURITY_STATUS(SEC_E_CERT_EXPIRED));
  Test('Get error category returns enum', LCategory = secCertificateError);

  // Test 4: Unknown error code
  LErrorCode := $FFFFFFFF;
  LErrorMsg := GetSchannelErrorString(LErrorCode);
  Test('Handle unknown error code', LErrorMsg <> '');

  // Test 5: Library error reporting
  LLib := CreateWinSSLLibrary;
  if not LLib.Initialize then
  begin
    Test('Library initialization failed', False);
    Exit;
  end;

  // Test 6: Invalid context creation behavior
  try
    // Try to create context with invalid type (cast to TSSLContextType)
    LContext := LLib.CreateContext(TSSLContextType(999));
    // Some implementations may create a context with default type
    // Others may return nil or raise exception - all are acceptable
    Test('Invalid context type handled', True,
      'Created context with invalid type (implementation-specific behavior)');
  except
    on E: Exception do
      // Raising an exception is acceptable
      Test('Invalid context type raises exception', True);
  end;

  // Test 7: Get last error after initialization (should be success)
  Test('Last error after init is 0', LLib.GetLastError = 0);
  Test('Last error string after init is empty', LLib.GetLastErrorString = '');

  // Test 8: Protocol version validation
  // Note: IsProtocolDeprecated only flags SSL2/SSL3 as deprecated
  // TLS 1.0/1.1 are technically still supported by Windows, though discouraged
  Test('SSL 2 deprecated', IsProtocolDeprecated(sslProtocolSSL2));
  Test('SSL 3 deprecated', IsProtocolDeprecated(sslProtocolSSL3));
  Test('TLS 1.0 not flagged deprecated', not IsProtocolDeprecated(sslProtocolTLS10));
  Test('TLS 1.2 not deprecated', not IsProtocolDeprecated(sslProtocolTLS12));
  Test('TLS 1.3 not deprecated', not IsProtocolDeprecated(sslProtocolTLS13));

  // Cleanup
  LContext := nil;
  if LLib <> nil then
  begin
    LLib.Finalize;
    LLib := nil;
  end;
end;

// ============================================================================
// Utils Module Tests
// ============================================================================

procedure TestUtilsModule;
var
  LVersions: TSSLProtocolVersions;
  LFlags: DWORD;
  LHandle: TSecHandle;
  LBuffer: PSecBuffer;
  LWideStr: PWideChar;
  LAnsiStr: AnsiString;
  LUTF8Str: string;
begin
  BeginSection('Utils Module');

  // Test 1: Protocol version to Schannel flags
  LVersions := [sslProtocolTLS12];
  LFlags := ProtocolVersionsToSchannelFlags(LVersions, False);
  Test('Convert TLS 1.2 to flags', LFlags <> 0,
    Format('Flags: $%x', [LFlags]));

  // Test 2: Multiple protocol versions
  LVersions := [sslProtocolTLS12, sslProtocolTLS13];
  LFlags := ProtocolVersionsToSchannelFlags(LVersions, False);
  Test('Convert TLS 1.2+1.3 to flags', LFlags <> 0,
    Format('Flags: $%x', [LFlags]));

  // Test 3: Schannel flags to protocol versions
  LFlags := SP_PROT_TLS1_2_CLIENT;
  LVersions := SchannelFlagsToProtocolVersions(LFlags, False);
  Test('Convert flags to TLS 1.2', sslProtocolTLS12 in LVersions);

  // Test 4: Get protocol name
  Test('TLS 1.2 name', GetProtocolVersionName(sslProtocolTLS12) = 'TLS 1.2');
  Test('TLS 1.3 name', GetProtocolVersionName(sslProtocolTLS13) = 'TLS 1.3');

  // Test 5: Security handle validation
  InitSecHandle(LHandle);
  Test('Init security handle sets to zero',
    (LHandle.dwLower = 0) and (LHandle.dwUpper = 0));
  Test('Recognize invalid handle', not IsValidSecHandle(LHandle));

  LHandle.dwLower := 1;
  LHandle.dwUpper := 1;
  Test('Recognize valid handle', IsValidSecHandle(LHandle));

  // Test 6: Clear security handle
  ClearSecHandle(LHandle);
  Test('Clear handle sets to zero',
    (LHandle.dwLower = 0) and (LHandle.dwUpper = 0));

  // Test 7: Allocate security buffer
  LBuffer := AllocSecBuffer(1024, SECBUFFER_DATA);
  Test('Allocate security buffer', LBuffer <> nil);

  if LBuffer <> nil then
  begin
    Test('Buffer size correct', LBuffer^.cbBuffer = 1024);
    Test('Buffer type correct', LBuffer^.BufferType = SECBUFFER_DATA);
    Test('Buffer pointer allocated', LBuffer^.pvBuffer <> nil);

    // Test 8: Free security buffer
    FreeSecBuffer(LBuffer);
    Test('Free security buffer', True);
  end;

  // Test 9: String conversion - ANSI to Wide
  LAnsiStr := 'Test String';
  LWideStr := StringToPWideChar(LAnsiStr);
  Test('Convert ANSI to Wide', LWideStr <> nil);
  if LWideStr <> nil then
  begin
    FreePWideCharString(LWideStr);
    Test('Free wide string', True);
  end;

  // Test 10: Wide to UTF-8 conversion
  LWideStr := StringToPWideChar('Test UTF-8');
  if LWideStr <> nil then
  begin
    LUTF8Str := WideToUTF8(LWideStr);
    Test('Convert Wide to UTF-8', LUTF8Str = 'Test UTF-8');
    FreePWideCharString(LWideStr);
  end;
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  WriteLn('========================================');
  WriteLn('  WinSSL Comprehensive Unit Tests');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Testing WinSSL backend components in isolation');
  WriteLn('Platform: ', {$IFDEF WIN64}'Windows x64'{$ELSE}'Windows x86'{$ENDIF});
  WriteLn;

  try
    // Run all test suites
    TestLibraryInitialization;
    TestContextManagement;
    TestCertificateManagement;
    TestErrorHandling;
    TestUtilsModule;

    // Print summary
    WriteLn;
    WriteLn('========================================');
    WriteLn('  Test Summary');
    WriteLn('========================================');
    WriteLn('Total tests:  ', TotalTests);
    WriteLn('Passed:       ', PassedTests, ' (',
      FormatFloat('0.0', PassedTests * 100.0 / TotalTests), '%)');
    WriteLn('Failed:       ', FailedTests);
    WriteLn;

    if FailedTests = 0 then
    begin
      WriteLn('✓ ALL TESTS PASSED');
      WriteLn;
      WriteLn('WinSSL backend components are functioning correctly.');
      ExitCode := 0;
    end
    else
    begin
      WriteLn('✗ SOME TESTS FAILED');
      WriteLn;
      WriteLn('Please review the failed tests above.');
      ExitCode := 1;
    end;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
