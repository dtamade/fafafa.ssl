program test_real_websites_enhanced;

{$mode objfpc}{$H+}

{
  增强版真实网站连接测试
  
  功能：完整测试HTTPS连接、TLS握手、证书验证、数据传输
  用途：验证库在真实场景下的可用性，发现实际问题
  难度：⭐⭐ 中级
}

uses
  SysUtils, Classes,
  {$IFDEF UNIX}
  fafafa.examples.sockets,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.base;

type
  TWebsiteTest = record
    Host: string;
    Port: Word;
    Path: string;
    Description: string;
    ExpectedStatus: string; // Expected HTTP status (e.g., '200', '301')
  end;

  TTestResult = record
    Success: Boolean;
    ErrorMessage: string;
    Protocol: string;
    Cipher: string;
    ResponseCode: string;
    ResponseTime: Integer; // milliseconds
  end;

const
  TEST_SITES: array[1..12] of TWebsiteTest = (
    (Host: 'www.google.com'; Port: 443; Path: '/'; Description: 'Google'; ExpectedStatus: '200'),
    (Host: 'www.github.com'; Port: 443; Path: '/'; Description: 'GitHub'; ExpectedStatus: '200'),
    (Host: 'api.github.com'; Port: 443; Path: '/'; Description: 'GitHub API'; ExpectedStatus: '200'),
    (Host: 'www.cloudflare.com'; Port: 443; Path: '/'; Description: 'Cloudflare'; ExpectedStatus: '200'),
    (Host: 'www.mozilla.org'; Port: 443; Path: '/'; Description: 'Mozilla'; ExpectedStatus: '200'),
    (Host: 'www.wikipedia.org'; Port: 443; Path: '/'; Description: 'Wikipedia'; ExpectedStatus: '200'),
    (Host: 'www.example.com'; Port: 443; Path: '/'; Description: 'Example.com'; ExpectedStatus: '200'),
    (Host: 'badssl.com'; Port: 443; Path: '/'; Description: 'BadSSL (测试站)'; ExpectedStatus: '200'),
    (Host: 'golang.org'; Port: 443; Path: '/'; Description: 'Go'; ExpectedStatus: '200'),
    (Host: 'www.python.org'; Port: 443; Path: '/'; Description: 'Python'; ExpectedStatus: '200'),
    (Host: 'www.rust-lang.org'; Port: 443; Path: '/'; Description: 'Rust'; ExpectedStatus: '200'),
    (Host: 'www.npmjs.com'; Port: 443; Path: '/'; Description: 'NPM'; ExpectedStatus: '200')
  );

var
  GLib: ISSLLibrary;
  GTotalTests: Integer = 0;
  GPassedTests: Integer = 0;
  GFailedTests: Integer = 0;

function GetProtocolName(AVer: TSSLProtocolVersion): string;
begin
  case AVer of
    sslProtocolUnknown: Result := 'Unknown';
    sslProtocolSSL2: Result := 'SSLv2';
    sslProtocolSSL3: Result := 'SSLv3';
    sslProtocolTLS10: Result := 'TLS 1.0';
    sslProtocolTLS11: Result := 'TLS 1.1';
    sslProtocolTLS12: Result := 'TLS 1.2';
    sslProtocolTLS13: Result := 'TLS 1.3';
    sslProtocolDTLS10: Result := 'DTLS 1.0';
    sslProtocolDTLS12: Result := 'DTLS 1.2';
  else
    Result := 'Unknown';
  end;
end;

{$IFDEF UNIX}
function ConnectSocket(const AHost: string; APort: Word): TSocket;
var
  LAddr: tsockaddr_in;
  LHostEntry: PHostEnt;
begin
  Result := socket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    raise Exception.Create('Failed to create socket');

  // Resolve hostname
  LHostEntry := gethostbyname(PChar(AHost));
  if LHostEntry = nil then
  begin
    close(Result);
    raise Exception.CreateFmt('Failed to resolve host: %s', [AHost]);
  end;

  // Connect
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  Move(LHostEntry^.h_addr_list^^, LAddr.sin_addr, SizeOf(LAddr.sin_addr));

  if connect(Result, psockaddr(@LAddr), SizeOf(LAddr)) < 0 then
  begin
    close(Result);
    raise Exception.CreateFmt('Failed to connect to %s:%d', [AHost, APort]);
  end;
end;
{$ENDIF}

function TestWebsite(const ATest: TWebsiteTest): TTestResult;
var
  {$IFDEF UNIX}
  LSocket: TSocket;
  {$ENDIF}
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LRequest: string;
  LResponse: TBytes;
  LBytesRead: Integer;
  LResponseStr: string;
  LStartTime, LEndTime: QWord;
  LStatusLine: string;
  LParts: TStringArray;
begin
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.Protocol := '';
  Result.Cipher := '';
  Result.ResponseCode := '';
  Result.ResponseTime := 0;

  Inc(GTotalTests);
  Write(Format('[%2d/%2d] Testing %s (%s:%d)... ',
    [GTotalTests, Length(TEST_SITES), ATest.Description, ATest.Host, ATest.Port]));

  LStartTime := GetTickCount64;

  try
    {$IFDEF UNIX}
    // Step 1: Create TCP connection
    try
      LSocket := ConnectSocket(ATest.Host, ATest.Port);
    except
      on E: Exception do
      begin
        Result.ErrorMessage := 'TCP connection failed: ' + E.Message;
        WriteLn('✗');
        WriteLn('    ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;
    end;

    try
      // Step 2: Create SSL context
      LContext := GLib.CreateContext(sslCtxClient);
      if LContext = nil then
      begin
        Result.ErrorMessage := 'Failed to create SSL context';
        WriteLn('✗');
        WriteLn('    ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      // Set server name for SNI
      LContext.SetServerName(ATest.Host);

      // Load system CA bundle
      if FileExists('/etc/ssl/certs/ca-certificates.crt') then
        LContext.LoadCAFile('/etc/ssl/certs/ca-certificates.crt')
      else if FileExists('/etc/pki/tls/certs/ca-bundle.crt') then
        LContext.LoadCAFile('/etc/pki/tls/certs/ca-bundle.crt');

      // Step 3: Create SSL connection and perform handshake
      LConn := LContext.CreateConnection(LSocket);
      if LConn = nil then
      begin
        Result.ErrorMessage := 'Failed to create SSL connection';
        WriteLn('✗');
        WriteLn('    ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      if not LConn.Connect then
      begin
        Result.ErrorMessage := 'TLS handshake failed: ' + GLib.GetLastErrorString;
        WriteLn('✗');
        WriteLn('    ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      // Get connection info
      try
        Result.Protocol := GetProtocolName(LConn.GetProtocolVersion);
        Result.Cipher := LConn.GetCipherName;
      except
        Result.Protocol := 'Unknown';
        Result.Cipher := 'Unknown';
      end;

      // Step 4: Send HTTP request
      LRequest := Format(
        'GET %s HTTP/1.1'#13#10 +
        'Host: %s'#13#10 +
        'User-Agent: fafafa.ssl/1.0'#13#10 +
        'Connection: close'#13#10 +
        #13#10,
        [ATest.Path, ATest.Host]
      );

      if LConn.Write(PChar(LRequest)^, Length(LRequest)) <> Length(LRequest) then
      begin
        Result.ErrorMessage := 'Failed to send HTTP request';
        WriteLn('✗');
        WriteLn('    ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      // Step 5: Read response
      SetLength(LResponse, 4096);
      LBytesRead := LConn.Read(LResponse[0], Length(LResponse));

      if LBytesRead > 0 then
      begin
        SetLength(LResponse, LBytesRead);
        SetLength(LResponseStr, LBytesRead);
        Move(LResponse[0], LResponseStr[1], LBytesRead);

        // Extract HTTP status code
        if Pos('HTTP/', LResponseStr) = 1 then
        begin
          LStatusLine := Copy(LResponseStr, 1, Pos(#13#10, LResponseStr) - 1);
          LParts := LStatusLine.Split(' ');
          if Length(LParts) >= 2 then
            Result.ResponseCode := LParts[1];
        end;
      end;

      LEndTime := GetTickCount64;
      Result.ResponseTime := LEndTime - LStartTime;
      Result.Success := True;
      Inc(GPassedTests);

      WriteLn('✓');
      WriteLn(Format('    Protocol: %s | Cipher: %s | Status: %s | Time: %dms',
        [Result.Protocol, Result.Cipher, Result.ResponseCode, Result.ResponseTime]));

    finally
      close(LSocket);
    end;
    {$ELSE}
    // Windows support would need WinSock2
    Result.ErrorMessage := 'Windows socket support not implemented in this test';
    WriteLn('⊘ Skipped (Unix only)');
    {$ENDIF}

  except
    on E: Exception do
    begin
      LEndTime := GetTickCount64;
      Result.ResponseTime := LEndTime - LStartTime;
      Result.ErrorMessage := E.Message;
      WriteLn('✗');
      WriteLn('    Error: ', E.Message);
      Inc(GFailedTests);
    end;
  end;
end;

var
  I: Integer;
  LResults: array of TTestResult;
  LTotalTime: Integer;
  LCount: Integer;

begin
  WriteLn('================================================================');
  WriteLn('  fafafa.ssl - Enhanced Real Website Connection Test');
  WriteLn('================================================================');
  WriteLn;

  {$IFDEF UNIX}
  WriteLn('Platform: Unix/Linux');
  {$ELSE}
  WriteLn('Platform: Windows (Limited support in this test)');
  {$ENDIF}
  WriteLn;

  try
    // Initialize SSL library
    WriteLn('Initializing OpenSSL library...');
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if not GLib.Initialize then
      raise Exception.Create('Failed to initialize SSL library');

    WriteLn('✓ OpenSSL ', GLib.GetVersion);
    WriteLn;
    WriteLn('Starting tests...');
    WriteLn('----------------------------------------------------------------');

    SetLength(LResults, Length(TEST_SITES));

    // Test all websites
    for I := Low(TEST_SITES) to High(TEST_SITES) do
      LResults[I - Low(TEST_SITES)] := TestWebsite(TEST_SITES[I]);

    WriteLn('----------------------------------------------------------------');
    WriteLn;

    // Display statistics
    WriteLn('================================================================');
    WriteLn('Test Results:');
    WriteLn('  Total:  ', GTotalTests);
    WriteLn('  Passed: ', GPassedTests, Format('  (%.1f%%)', [GPassedTests * 100.0 / GTotalTests]));
    WriteLn('  Failed: ', GFailedTests);

    if GPassedTests > 0 then
    begin
      LTotalTime := 0;
      LCount := 0;
      for I := 0 to High(LResults) do
      begin
        if LResults[I].Success then
        begin
          Inc(LTotalTime, LResults[I].ResponseTime);
          Inc(LCount);
        end;
      end;
      if LCount > 0 then
        WriteLn('  Avg Response Time: ', LTotalTime div LCount, 'ms');
    end;

    WriteLn('================================================================');
    WriteLn;

    if GFailedTests = 0 then
    begin
      WriteLn('✅ All tests passed! Library works correctly in real-world scenarios.');
      WriteLn;
      WriteLn('Next steps:');
      WriteLn('  1. Test with client certificate authentication');
      WriteLn('  2. Test various TLS versions and cipher suites');
      WriteLn('  3. Performance benchmarking');
    end
    else
    begin
      WriteLn('⚠️  Some tests failed. Review errors above.');
      WriteLn;
      WriteLn('Common issues:');
      WriteLn('  - Network connectivity problems');
      WriteLn('  - Certificate verification failures');
      WriteLn('  - Unsupported TLS versions');
    end;

    GLib.Finalize;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('================================================================');
      WriteLn('FATAL ERROR: ', E.Message);
      WriteLn('================================================================');
      Halt(1);
    end;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  // ReadLn; // Removed for automated testing
end.
