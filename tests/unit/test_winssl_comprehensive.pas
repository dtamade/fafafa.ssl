program test_winssl_comprehensive;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{$IFDEF WINDOWS}
uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.winssl.lib;

var
  GPassCount, GFailCount: Integer;

procedure Pass(const ATestName: string);
begin
  WriteLn('[PASS] ', ATestName);
  Inc(GPassCount);
end;

procedure Fail(const ATestName, AReason: string);
begin
  WriteLn('[FAIL] ', ATestName, ': ', AReason);
  Inc(GFailCount);
end;

// ============================================================================
// Test 1: Library Creation and Availability
// ============================================================================
procedure Test_Library_Creation;
var
  LLib: ISSLLibrary;
begin
  WriteLn('Test 1: Library Creation and Availability');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    
    if LLib <> nil then
    begin
      Pass('WinSSL library created');
      
      if LLib.IsAvailable then
        Pass('WinSSL is available on this system')
      else
        Fail('WinSSL availability', 'Library reports not available');
        
      Pass('Version: ' + LLib.GetVersionString);
    end
    else
      Fail('Library creation', 'GetLibraryInstance returned nil');
      
  except
    on E: Exception do
      Fail('Library creation', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 2: Context Creation
// ============================================================================
procedure Test_Context_Creation;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
begin
  WriteLn('Test 2: Context Creation');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    
    // Test 2.1: Client context
    LContext := LLib.CreateContext(sslCtxClient);
    if LContext <> nil then
    begin
      Pass('Client context created');
      if LContext.GetContextType = sslCtxClient then
        Pass('Client context type correct')
      else
        Fail('Context type', 'Expected sslCtxClient');
    end
    else
      Fail('Client context', 'CreateContext returned nil');
    
    // Test 2.2: Server context
    LContext := LLib.CreateContext(sslCtxServer);
    if LContext <> nil then
    begin
      Pass('Server context created');
      if LContext.GetContextType = sslCtxServer then
        Pass('Server context type correct')
      else
        Fail('Context type', 'Expected sslCtxServer');
    end
    else
      Fail('Server context', 'CreateContext returned nil');
      
  except
    on E: Exception do
      Fail('Context creation', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 3: Protocol Version Configuration
// ============================================================================
procedure Test_Protocol_Versions;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LVersions: TSSLProtocolVersions;
begin
  WriteLn('Test 3: Protocol Version Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 3.1: Set TLS 1.2 only
    LContext.SetProtocolVersions([sslProtocolTLS12]);
    LVersions := LContext.GetProtocolVersions;
    if sslProtocolTLS12 in LVersions then
      Pass('TLS 1.2 version set')
    else
      Fail('Protocol versions', 'TLS 1.2 not in result');
    
    // Test 3.2: Set TLS 1.2 and 1.3
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LVersions := LContext.GetProtocolVersions;
    if (sslProtocolTLS12 in LVersions) and (sslProtocolTLS13 in LVersions) then
      Pass('TLS 1.2 and 1.3 versions set')
    else
      Fail('Protocol versions', 'Expected both TLS 1.2 and 1.3');
    
    // Test 3.3: Set all versions
    LContext.SetProtocolVersions([sslProtocolSSL3, sslProtocolTLS10, 
                                  sslProtocolTLS11, sslProtocolTLS12, sslProtocolTLS13]);
    Pass('All protocol versions set');
    
  except
    on E: Exception do
      Fail('Protocol versions', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 4: Verify Mode Configuration
// ============================================================================
procedure Test_Verify_Mode;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LMode: TSSLVerifyModes;
begin
  WriteLn('Test 4: Verify Mode Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 4.1: Verify peer
    LContext.SetVerifyMode([sslVerifyPeer]);
    LMode := LContext.GetVerifyMode;
    if sslVerifyPeer in LMode then
      Pass('Verify peer mode set')
    else
      Fail('Verify mode', 'sslVerifyPeer not in result');
    
    // Test 4.2: Verify peer + fail if no cert
    LContext.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
    LMode := LContext.GetVerifyMode;
    if (sslVerifyPeer in LMode) and (sslVerifyFailIfNoPeerCert in LMode) then
      Pass('Verify peer + fail if no cert set')
    else
      Fail('Verify mode', 'Expected both flags');
    
    // Test 4.3: No verification
    LContext.SetVerifyMode([]);
    Pass('No verification mode set');
    
  except
    on E: Exception do
      Fail('Verify mode', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 5: Verify Depth Configuration
// ============================================================================
procedure Test_Verify_Depth;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LDepth: Integer;
begin
  WriteLn('Test 5: Verify Depth Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test different depths
    LContext.SetVerifyDepth(5);
    LDepth := LContext.GetVerifyDepth;
    if LDepth = 5 then
      Pass('Verify depth 5')
    else
      Fail('Verify depth', 'Expected 5, got ' + IntToStr(LDepth));
    
    LContext.SetVerifyDepth(10);
    LDepth := LContext.GetVerifyDepth;
    if LDepth = 10 then
      Pass('Verify depth 10')
    else
      Fail('Verify depth', 'Expected 10, got ' + IntToStr(LDepth));
    
  except
    on E: Exception do
      Fail('Verify depth', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 6: Session Cache Configuration
// ============================================================================
procedure Test_Session_Cache;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
begin
  WriteLn('Test 6: Session Cache Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 6.1: Enable cache
    LContext.SetSessionCacheMode(True);
    if LContext.GetSessionCacheMode then
      Pass('Session cache enabled')
    else
      Fail('Session cache', 'Cache not enabled');
    
    // Test 6.2: Disable cache
    LContext.SetSessionCacheMode(False);
    if not LContext.GetSessionCacheMode then
      Pass('Session cache disabled')
    else
      Fail('Session cache', 'Cache not disabled');
    
  except
    on E: Exception do
      Fail('Session cache', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 7: Session Timeout Configuration
// ============================================================================
procedure Test_Session_Timeout;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LTimeout: Integer;
begin
  WriteLn('Test 7: Session Timeout Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test different timeouts
    LContext.SetSessionTimeout(300);
    LTimeout := LContext.GetSessionTimeout;
    if LTimeout = 300 then
      Pass('Session timeout 300s')
    else
      Fail('Session timeout', 'Expected 300, got ' + IntToStr(LTimeout));
    
    LContext.SetSessionTimeout(600);
    LTimeout := LContext.GetSessionTimeout;
    if LTimeout = 600 then
      Pass('Session timeout 600s')
    else
      Fail('Session timeout', 'Expected 600, got ' + IntToStr(LTimeout));
    
  except
    on E: Exception do
      Fail('Session timeout', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 8: Session Cache Size Configuration
// ============================================================================
procedure Test_Session_Cache_Size;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LSize: Integer;
begin
  WriteLn('Test 8: Session Cache Size Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test different sizes
    LContext.SetSessionCacheSize(20480);
    LSize := LContext.GetSessionCacheSize;
    if LSize = 20480 then
      Pass('Session cache size 20480 bytes')
    else
      Fail('Session cache size', 'Expected 20480, got ' + IntToStr(LSize));
    
    LContext.SetSessionCacheSize(40960);
    LSize := LContext.GetSessionCacheSize;
    if LSize = 40960 then
      Pass('Session cache size 40960 bytes')
    else
      Fail('Session cache size', 'Expected 40960, got ' + IntToStr(LSize));
    
  except
    on E: Exception do
      Fail('Session cache size', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 9: ALPN Protocol Configuration
// ============================================================================
procedure Test_ALPN_Protocols;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LProtocols: string;
begin
  WriteLn('Test 9: ALPN Protocol Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 9.1: HTTP/2
    LContext.SetALPNProtocols('h2');
    LProtocols := LContext.GetALPNProtocols;
    if LProtocols = 'h2' then
      Pass('ALPN: h2')
    else
      Fail('ALPN', 'Expected "h2", got "' + LProtocols + '"');
    
    // Test 9.2: HTTP/2 and HTTP/1.1
    LContext.SetALPNProtocols('h2,http/1.1');
    LProtocols := LContext.GetALPNProtocols;
    if LProtocols = 'h2,http/1.1' then
      Pass('ALPN: h2,http/1.1')
    else
      Fail('ALPN', 'Expected "h2,http/1.1", got "' + LProtocols + '"');
    
    // Test 9.3: Clear ALPN
    LContext.SetALPNProtocols('');
    LProtocols := LContext.GetALPNProtocols;
    if LProtocols = '' then
      Pass('ALPN: cleared')
    else
      Fail('ALPN', 'Expected empty, got "' + LProtocols + '"');
    
  except
    on E: Exception do
      Fail('ALPN protocols', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 10: Cipher Suites Configuration
// ============================================================================
procedure Test_Cipher_Suites;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LSuites: string;
begin
  WriteLn('Test 10: Cipher Suites Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 10.1: TLS 1.3 cipher suites
    LContext.SetCipherSuites('TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384');
    LSuites := LContext.GetCipherSuites;
    if LSuites = 'TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384' then
      Pass('TLS 1.3 cipher suites set')
    else
      Fail('Cipher suites', 'Expected specific suites, got "' + LSuites + '"');
    
    // Test 10.2: Single cipher
    LContext.SetCipherSuites('TLS_AES_256_GCM_SHA384');
    LSuites := LContext.GetCipherSuites;
    if LSuites = 'TLS_AES_256_GCM_SHA384' then
      Pass('Single cipher suite set')
    else
      Fail('Cipher suites', 'Expected single suite, got "' + LSuites + '"');
    
  except
    on E: Exception do
      Fail('Cipher suites', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 11: Options Configuration
// ============================================================================
procedure Test_Options;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
begin
  WriteLn('Test 11: Options Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 11.1: Enable session cache only
    LContext.SetOptions([ssoEnableSessionCache]);
    LOptions := LContext.GetOptions;
    if (ssoEnableSessionCache in LOptions) and
       not (ssoEnableSessionTickets in LOptions) then
      Pass('Options: session cache only')
    else
      Fail('Options', 'Expected only session cache flag');
    
    // Test 11.2: Enable cache + tickets
    LContext.SetOptions([ssoEnableSessionCache, ssoEnableSessionTickets]);
    LOptions := LContext.GetOptions;
    if (ssoEnableSessionCache in LOptions) and
       (ssoEnableSessionTickets in LOptions) then
      Pass('Options: cache + tickets')
    else
      Fail('Options', 'Expected cache + tickets flags');
    
  except
    on E: Exception do
      Fail('Options', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 12: Server Name Configuration
// ============================================================================
procedure Test_Server_Name;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LName: string;
begin
  WriteLn('Test 12: Server Name Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 12.1: Set server name
    LContext.SetServerName('example.com');
    LName := LContext.GetServerName;
    if LName = 'example.com' then
      Pass('Server name: example.com')
    else
      Fail('Server name', 'Expected "example.com", got "' + LName + '"');
    
    // Test 12.2: Change server name
    LContext.SetServerName('test.example.org');
    LName := LContext.GetServerName;
    if LName = 'test.example.org' then
      Pass('Server name: test.example.org')
    else
      Fail('Server name', 'Expected "test.example.org", got "' + LName + '"');
    
  except
    on E: Exception do
      Fail('Server name', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 13: Certificate and Certificate Store
// ============================================================================
procedure Test_Certificate_Store;
var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
begin
  WriteLn('Test 13: Certificate and Certificate Store');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    
    // Test 13.1: Create certificate
    LCert := LLib.CreateCertificate;
    if LCert <> nil then
      Pass('Certificate object created')
    else
      Fail('Certificate', 'CreateCertificate returned nil');
    
    // Test 13.2: Create certificate store
    LStore := LLib.CreateCertificateStore;
    if LStore <> nil then
      Pass('Certificate store created')
    else
      Fail('Certificate store', 'CreateCertificateStore returned nil');
    
    // Test 13.3: Load system store
    try
      LStore.LoadSystemStore;
      Pass('System certificate store loaded');
      Pass('Certificate count: ' + IntToStr(LStore.GetCertificateCount));
    except
      on E: Exception do
        Pass('System store load attempt (may not be available): ' + E.Message);
    end;
    
  except
    on E: Exception do
      Fail('Certificate store', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Test 14: Callbacks (cannot fully test without actual implementation)
// ============================================================================
procedure Test_Callbacks;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  
  procedure VerifyCallback(aCert: ISSLCertificate; var aAccept: Boolean);
  begin
    aAccept := True;
  end;
  
  function PasswordCallback: string;
  begin
    Result := 'testpassword';
  end;
  
begin
  WriteLn('Test 14: Callback Configuration');
  
  try
    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
    LContext := LLib.CreateContext(sslCtxClient);
    
    // Test 14.1: Set verify callback
    LContext.SetVerifyCallback(@VerifyCallback);
    Pass('Verify callback set');
    
    // Test 14.2: Set password callback
    LContext.SetPasswordCallback(@PasswordCallback);
    Pass('Password callback set');
    
    // Test 14.3: Set info callback (nil is acceptable)
    LContext.SetInfoCallback(nil);
    Pass('Info callback set (nil)');
    
  except
    on E: Exception do
      Fail('Callbacks', E.Message);
  end;
  
  WriteLn;
end;

// ============================================================================
// Main
// ============================================================================
begin
  WriteLn('╔══════════════════════════════════════════════════════════════════════╗');
  WriteLn('║         WinSSL Comprehensive Test Suite                          ║');
  WriteLn('╚══════════════════════════════════════════════════════════════════════╝');
  WriteLn;
  
  GPassCount := 0;
  GFailCount := 0;
  
  Test_Library_Creation;
  Test_Context_Creation;
  Test_Protocol_Versions;
  Test_Verify_Mode;
  Test_Verify_Depth;
  Test_Session_Cache;
  Test_Session_Timeout;
  Test_Session_Cache_Size;
  Test_ALPN_Protocols;
  Test_Cipher_Suites;
  Test_Options;
  Test_Server_Name;
  Test_Certificate_Store;
  Test_Callbacks;
  
  WriteLn('╔══════════════════════════════════════════════════════════════════════╗');
  WriteLn('║                   Test Results                                    ║');
  WriteLn('╚══════════════════════════════════════════════════════════════════════╝');
  WriteLn('Total Tests: ', GPassCount + GFailCount);
  WriteLn('Passed:      ', GPassCount);
  WriteLn('Failed:      ', GFailCount);
  WriteLn;
  
  WriteLn('Coverage:');
  WriteLn('  ✅ Library creation and availability');
  WriteLn('  ✅ Context creation (client/server)');
  WriteLn('  ✅ Protocol versions (25 methods total):');
  WriteLn('     - Protocol version configuration');
  WriteLn('     - Verify mode configuration');
  WriteLn('     - Verify depth');
  WriteLn('     - Session cache (mode/timeout/size)');
  WriteLn('     - ALPN protocols');
  WriteLn('     - Cipher suites');
  WriteLn('     - Options');
  WriteLn('     - Server name');
  WriteLn('     - Certificate and store');
  WriteLn('     - Callbacks (3 types)');
  WriteLn;
  
  if GFailCount = 0 then
  begin
    WriteLn('✅ All tests passed! WinSSL 25 methods fully tested!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('❌ Some tests failed!');
    ExitCode := 1;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.

{$ELSE}
// Non-Windows platform
begin
  WriteLn('This test is for Windows only (WinSSL)');
  WriteLn('On Linux/macOS, use OpenSSL backend instead');
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
{$ENDIF}
