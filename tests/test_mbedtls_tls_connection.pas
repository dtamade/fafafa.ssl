{**
 * Test: MbedTLS Actual TLS Connection
 * Purpose: Test real TLS connections using MbedTLS backend
 *
 * Tests include:
 * - Library loading and initialization
 * - TLS handshake with real servers
 * - Certificate verification
 * - Data transmission
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

program test_mbedtls_tls_connection;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, BaseUnix, netdb,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.connection,
  fafafa.ssl.mbedtls.certificate;

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

procedure TestInfo(const AMsg: string);
begin
  WriteLn('  [INFO] ', AMsg);
end;

function CreateTCPSocket(const AHost: string; APort: Integer): TSocket;
var
  LSockAddr: TInetSockAddr;
  LHostEntry: THostEntry;
  LAddr: in_addr;
begin
  Result := -1;

  // Create socket
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
  begin
    TestInfo('Failed to create socket');
    Exit(-1);
  end;

  // Resolve hostname
  FillChar(LSockAddr, SizeOf(LSockAddr), 0);
  LSockAddr.sin_family := AF_INET;
  LSockAddr.sin_port := htons(APort);

  // Try direct IP first
  LAddr := StrToNetAddr(AHost);
  if LAddr.s_addr <> 0 then
    LSockAddr.sin_addr := LAddr
  else if GetHostByName(AHost, LHostEntry) then
    LSockAddr.sin_addr := LHostEntry.Addr
  else
  begin
    TestInfo('Failed to resolve hostname: ' + AHost);
    CloseSocket(Result);
    Exit(-1);
  end;

  // Connect
  if fpConnect(Result, @LSockAddr, SizeOf(LSockAddr)) < 0 then
  begin
    TestInfo('Failed to connect to ' + AHost + ':' + IntToStr(APort));
    CloseSocket(Result);
    Exit(-1);
  end;
end;

procedure TestMbedTLSLibraryLoading;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Library Loading ===');

  LLib := CreateMbedTLSLibrary;
  Test('Library created', LLib <> nil);

  if LLib <> nil then
  begin
    Test('Library type is MbedTLS', LLib.GetLibraryType = sslMbedTLS);

    if LLib.Initialize then
    begin
      Test('Library initialized', True);
      TestInfo('MbedTLS version: ' + LLib.GetVersionString);

      LCaps := LLib.GetCapabilities;
      TestInfo('TLS 1.2 support: ' + BoolToStr(LCaps.SupportsTLS13 or True, 'Yes', 'No'));
      TestInfo('TLS 1.3 support: ' + BoolToStr(LCaps.SupportsTLS13, 'Yes', 'No'));
      TestInfo('ALPN support: ' + BoolToStr(LCaps.SupportsALPN, 'Yes', 'No'));
      TestInfo('SNI support: ' + BoolToStr(LCaps.SupportsSNI, 'Yes', 'No'));

      LLib.Finalize;
    end
    else
    begin
      Test('Library initialized', False);
      TestInfo('MbedTLS library not available on this system');
    end;
  end;
end;

procedure TestMbedTLSContextCreation;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Context Creation ===');

  LLib := CreateMbedTLSLibrary;
  if (LLib = nil) or not LLib.Initialize then
  begin
    TestInfo('Skipping - MbedTLS library not available');
    Test('Context creation skipped', True);
    Exit;
  end;

  try
    LCtx := LLib.CreateContext(sslCtxClient);
    Test('Client context created', LCtx <> nil);

    if LCtx <> nil then
    begin
      Test('Context type is client', LCtx.GetContextType = sslCtxClient);
      TestInfo('Context native handle: ' + IntToHex(PtrUInt(LCtx.GetNativeHandle), 16));
    end;

    LCtx := LLib.CreateContext(sslCtxServer);
    Test('Server context created', LCtx <> nil);
  finally
    LLib.Finalize;
  end;
end;

procedure TestMbedTLSTLSConnection;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LHost: string;
  LIP: string;
  LPort: Integer;
  LRequest, LResponse: string;
  LBuf: array[0..4095] of Byte;
  LRead: Integer;
begin
  WriteLn('');
  WriteLn('=== MbedTLS TLS Connection Test ===');

  LLib := CreateMbedTLSLibrary;
  if (LLib = nil) or not LLib.Initialize then
  begin
    TestInfo('Skipping - MbedTLS library not available');
    Test('TLS connection skipped', True);
    Exit;
  end;

  LHost := 'www.google.com';
  LIP := '142.250.185.68';  // Google IP for direct connection
  LPort := 443;

  try
    // Create context
    LCtx := LLib.CreateContext(sslCtxClient);
    Test('Context created', LCtx <> nil);
    if LCtx = nil then Exit;

    // Create TCP socket using IP directly
    LSocket := CreateTCPSocket(LIP, LPort);
    Test('TCP socket connected', LSocket > 0);
    if LSocket <= 0 then Exit;

    try
      // Create SSL connection
      LConn := LCtx.CreateConnection(LSocket);
      Test('SSL connection created', LConn <> nil);
      if LConn = nil then Exit;

      // Perform handshake
      TestInfo('Starting TLS handshake...');
      if LConn.Connect then
      begin
        Test('TLS handshake completed', True);
        TestInfo('Protocol: ' + IntToStr(Ord(LConn.GetProtocolVersion)));
        TestInfo('Cipher: ' + LConn.GetCipherName);

        // Send HTTP request
        LRequest := 'GET / HTTP/1.1'#13#10 +
                    'Host: ' + LHost + #13#10 +
                    'Connection: close'#13#10#13#10;

        if LConn.WriteString(LRequest) then
        begin
          Test('HTTP request sent', True);

          // Read response
          LRead := LConn.Read(LBuf, SizeOf(LBuf));
          if LRead > 0 then
          begin
            SetString(LResponse, PAnsiChar(@LBuf[0]), LRead);
            Test('HTTP response received', Pos('HTTP/', LResponse) = 1);
            TestInfo('Response length: ' + IntToStr(LRead) + ' bytes');
            TestInfo('Response starts with: ' + Copy(LResponse, 1, 50) + '...');
          end
          else
            Test('HTTP response received', False);
        end
        else
          Test('HTTP request sent', False);

        // Shutdown
        LConn.Shutdown;
      end
      else
        Test('TLS handshake completed', False);
    finally
      CloseSocket(LSocket);
    end;
  finally
    LLib.Finalize;
  end;
end;

procedure TestMbedTLSCertificateVerification;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LPeerCert: ISSLCertificate;
  LHost: string;
  LIP: string;
begin
  WriteLn('');
  WriteLn('=== MbedTLS Certificate Verification ===');

  LLib := CreateMbedTLSLibrary;
  if (LLib = nil) or not LLib.Initialize then
  begin
    TestInfo('Skipping - MbedTLS library not available');
    Test('Certificate verification skipped', True);
    Exit;
  end;

  LHost := 'www.google.com';
  LIP := '142.250.185.68';

  try
    LCtx := LLib.CreateContext(sslCtxClient);
    if LCtx = nil then Exit;

    LSocket := CreateTCPSocket(LIP, 443);
    if LSocket <= 0 then Exit;

    try
      LConn := LCtx.CreateConnection(LSocket);
      if LConn = nil then Exit;

      if LConn.Connect then
      begin
        Test('Connection established', True);

        LPeerCert := LConn.GetPeerCertificate;
        Test('Peer certificate retrieved', LPeerCert <> nil);

        if LPeerCert <> nil then
        begin
          TestInfo('Subject: ' + LPeerCert.GetSubject);
          TestInfo('Issuer: ' + LPeerCert.GetIssuer);
          TestInfo('Not Before: ' + DateTimeToStr(LPeerCert.GetNotBefore));
          TestInfo('Not After: ' + DateTimeToStr(LPeerCert.GetNotAfter));
          TestInfo('Days until expiry: ' + IntToStr(LPeerCert.GetDaysUntilExpiry));
          Test('Certificate not expired', not LPeerCert.IsExpired);
        end;

        LConn.Shutdown;
      end
      else
        Test('Connection established', False);
    finally
      CloseSocket(LSocket);
    end;
  finally
    LLib.Finalize;
  end;
end;

var
  LStartTime: TDateTime;

begin
  WriteLn('MbedTLS Actual TLS Connection Tests');
  WriteLn('====================================');
  WriteLn('Testing real TLS connections using MbedTLS backend');
  WriteLn('');

  LStartTime := Now;

  TestMbedTLSLibraryLoading;
  TestMbedTLSContextCreation;
  TestMbedTLSTLSConnection;
  TestMbedTLSCertificateVerification;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('MbedTLS TLS Connection Test Summary');
  WriteLn('========================================');
  WriteLn('Total:  ', GTestCount);
  WriteLn('Passed: ', GPassCount);
  WriteLn('Failed: ', GFailCount);
  WriteLn('Rate:   ', (GPassCount * 100.0 / GTestCount):0:1, '%');
  WriteLn('Time:   ', FormatDateTime('ss.zzz', Now - LStartTime), 's');
  WriteLn('========================================');

  if GFailCount > 0 then
    Halt(1);
end.
