{******************************************************************************}
{  WinSSL vs OpenSSL Backend Comparison Tests                                  }
{  Migrated to use TSimpleTestRunner framework (P1-2.2)                        }
{******************************************************************************}

program test_backend_comparison;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}Windows, WinSock2, md5,{$ENDIF}
  {$IFNDEF WINDOWS}BaseUnix,{$ENDIF}
  SysUtils, Classes,

  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;
  CurrentSection: string;

procedure BeginSection(const aName: string);
begin
  CurrentSection := aName;
  WriteLn;
  WriteLn('=== ', aName, ' ===');
end;

procedure Test(const aName: string; aCondition: Boolean; const aDetails: string = '');
begin
  Runner.Check('[' + CurrentSection + '] ' + aName, aCondition, aDetails);
end;

{$IFDEF WINDOWS}
function InitWinsock: Boolean;
var
  LWSAData: TWSAData;
begin
  Result := WSAStartup(MAKEWORD(2, 2), LWSAData) = 0;
end;

procedure CleanupWinsock;
begin
  WSACleanup;
end;

function ConnectToHost(const aHost: string; aPort: Word; out aSocket: TSocket): Boolean;
var
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LInAddr: TInAddr;
  LTimeout: Integer;
begin
  Result := False;
  aSocket := INVALID_SOCKET;

  aSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aSocket = INVALID_SOCKET then
    Exit;

  LTimeout := 10000;
  setsockopt(aSocket, SOL_SOCKET, SO_RCVTIMEO, @LTimeout, SizeOf(LTimeout));
  setsockopt(aSocket, SOL_SOCKET, SO_SNDTIMEO, @LTimeout, SizeOf(LTimeout));

  LHostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if LHostEnt = nil then
  begin
    closesocket(aSocket);
    aSocket := INVALID_SOCKET;
    Exit;
  end;

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(aPort);
  Move(LHostEnt^.h_addr_list^^, LInAddr, SizeOf(LInAddr));
  LAddr.sin_addr := LInAddr;

  Result := connect(aSocket, @LAddr, SizeOf(LAddr)) = 0;
  if not Result then
  begin
    closesocket(aSocket);
    aSocket := INVALID_SOCKET;
  end;
end;

procedure CloseSSLSocket(aSocket: TSocket);
begin
  if aSocket <> INVALID_SOCKET then
    closesocket(aSocket);
end;
{$ELSE}
// Linux stubs - this test is Windows-only
type
  TSocket = Integer;
const
  INVALID_SOCKET = -1;

function InitWinsock: Boolean;
begin
  Result := True;  // No-op on Linux
end;

procedure CleanupWinsock;
begin
  // No-op on Linux
end;

function ConnectToHost(const aHost: string; aPort: Word; out aSocket: TSocket): Boolean;
begin
  Result := False;  // Not implemented on Linux
  aSocket := INVALID_SOCKET;
end;

procedure CloseSSLSocket(aSocket: TSocket);
begin
  if aSocket <> INVALID_SOCKET then
    fpClose(aSocket);
end;
{$ENDIF}

function FetchHTTPS(aLibType: TSSLLibraryType; const aHost, aPath: string;
  out aResponse: string): Boolean;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
begin
  Result := False;
  aResponse := '';
  LSocket := INVALID_SOCKET;

  try
    // Create library
    LLib := CreateSSLLibrary(aLibType);
    if not LLib.Initialize then
      Exit;

    // Create context
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetServerName(aHost);
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.SetVerifyMode([]);

    // Connect TCP
    if not ConnectToHost(aHost, 443, LSocket) then
      Exit;

    // Create SSL connection
    LConn := LContext.CreateConnection(LSocket);
    if not LConn.Connect then
      Exit;

    // Send request
    LRequest := 'GET ' + aPath + ' HTTP/1.1'#13#10 +
                'Host: ' + aHost + #13#10 +
                'Connection: close'#13#10 +
                'User-Agent: BackendComparison/1.0'#13#10 +
                #13#10;

    if not LConn.WriteString(LRequest) then
      Exit;

    // Receive response
    repeat
      LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
      if LBytesRead > 0 then
      begin
        SetLength(aResponse, Length(aResponse) + LBytesRead);
        Move(LBuffer[0], aResponse[Length(aResponse) - LBytesRead + 1], LBytesRead);
        Result := True;
      end;
    until LBytesRead <= 0;

    LConn.Shutdown;

  finally
    if LSocket <> INVALID_SOCKET then
      {$IFDEF WINDOWS}CloseSSLSocket(LSocket){$ELSE}fpClose(LSocket){$ENDIF};
  end;
end;

{$IFDEF WINDOWS}
function CalculateMD5(const aData: string): string;
var
  LDigest: TMD5Digest;
  i: Integer;
begin
  LDigest := MD5String(aData);
  Result := '';
  for i := 0 to 15 do
    Result := Result + IntToHex(LDigest[i], 2);
  Result := LowerCase(Result);
end;
{$ELSE}
function CalculateMD5(const aData: string): string;
begin
  Result := ''; // MD5 not available on Linux without additional unit
end;
{$ENDIF}

procedure TestBasicFunctionality;
var
  LWinSSL, LOpenSSL: ISSLLibrary;
  LWinSSLVersion, LOpenSSLVersion: string;
begin
  BeginSection('基础功能对比');

  // Test 1: Library creation
  LWinSSL := CreateSSLLibrary(sslWinSSL);
  Test('WinSSL 库创建', LWinSSL <> nil);

  LOpenSSL := CreateSSLLibrary(sslOpenSSL);
  Test('OpenSSL 库创建', LOpenSSL <> nil);

  // Test 2: Initialization
  Test('WinSSL 初始化', LWinSSL.Initialize);
  Test('OpenSSL 初始化', LOpenSSL.Initialize);

  // Test 3: Library type
  Test('WinSSL 类型正确', LWinSSL.GetLibraryType = sslWinSSL);
  Test('OpenSSL 类型正确', LOpenSSL.GetLibraryType = sslOpenSSL);

  // Test 4: Version strings (should be different but valid)
  LWinSSLVersion := LWinSSL.GetVersionString;
  LOpenSSLVersion := LOpenSSL.GetVersionString;
  Test('WinSSL 版本字符串有效', Length(LWinSSLVersion) > 0);
  Test('OpenSSL 版本字符串有效', Length(LOpenSSLVersion) > 0);
  Test('版本字符串不同', LWinSSLVersion <> LOpenSSLVersion,
    Format('WinSSL: %s, OpenSSL: %s', [LWinSSLVersion, LOpenSSLVersion]));

  // Test 5: Protocol support (both should support TLS 1.2)
  Test('WinSSL 支持 TLS 1.2', LWinSSL.IsProtocolSupported(sslProtocolTLS12));
  Test('OpenSSL 支持 TLS 1.2', LOpenSSL.IsProtocolSupported(sslProtocolTLS12));

  // Test 6: Context creation
  Test('WinSSL 可创建客户端上下文',
    LWinSSL.CreateContext(sslCtxClient) <> nil);
  Test('OpenSSL 可创建客户端上下文',
    LOpenSSL.CreateContext(sslCtxClient) <> nil);
end;

procedure TestTLSHandshakeComparison;
var
  LWinSSLLib, LOpenSSLLib: ISSLLibrary;
  LWinSSLCtx, LOpenSSLCtx: ISSLContext;
  LWinSSLConn, LOpenSSLConn: ISSLConnection;
  LWinSSLSocket, LOpenSSLSocket: TSocket;
  LWinSSLProto, LOpenSSLProto: TSSLProtocolVersion;
  LWinSSLCipher, LOpenSSLCipher: string;
begin
  BeginSection('TLS 握手对比');

  LWinSSLSocket := INVALID_SOCKET;
  LOpenSSLSocket := INVALID_SOCKET;

  try
    // WinSSL handshake
    LWinSSLLib := CreateSSLLibrary(sslWinSSL);
    if LWinSSLLib.Initialize then
    begin
      LWinSSLCtx := LWinSSLLib.CreateContext(sslCtxClient);
      LWinSSLCtx.SetServerName('www.google.com');
      LWinSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LWinSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 443, LWinSSLSocket) then
      begin
        LWinSSLConn := LWinSSLCtx.CreateConnection(LWinSSLSocket);
        Test('WinSSL 握手成功', LWinSSLConn.Connect);

        if LWinSSLConn.IsConnected then
        begin
          LWinSSLProto := LWinSSLConn.GetProtocolVersion;
          LWinSSLCipher := LWinSSLConn.GetCipherName;
          Test('WinSSL 协议版本有效',
            LWinSSLProto in [sslProtocolTLS12, sslProtocolTLS13]);
          Test('WinSSL 密码套件有效', Length(LWinSSLCipher) > 0);
          LWinSSLConn.Shutdown;
        end;

        CloseSSLSocket(LWinSSLSocket);
        LWinSSLSocket := INVALID_SOCKET;
      end;
    end;

    // OpenSSL handshake
    LOpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    if LOpenSSLLib.Initialize then
    begin
      LOpenSSLCtx := LOpenSSLLib.CreateContext(sslCtxClient);
      LOpenSSLCtx.SetServerName('www.google.com');
      LOpenSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LOpenSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 443, LOpenSSLSocket) then
      begin
        LOpenSSLConn := LOpenSSLCtx.CreateConnection(LOpenSSLSocket);
        Test('OpenSSL 握手成功', LOpenSSLConn.Connect);

        if LOpenSSLConn.IsConnected then
        begin
          LOpenSSLProto := LOpenSSLConn.GetProtocolVersion;
          LOpenSSLCipher := LOpenSSLConn.GetCipherName;
          Test('OpenSSL 协议版本有效',
            LOpenSSLProto in [sslProtocolTLS12, sslProtocolTLS13]);
          Test('OpenSSL 密码套件有效', Length(LOpenSSLCipher) > 0);
          LOpenSSLConn.Shutdown;
        end;

        CloseSSLSocket(LOpenSSLSocket);
        LOpenSSLSocket := INVALID_SOCKET;
      end;
    end;

    // Compare results
    Test('协议版本兼容',
      (LWinSSLProto in [sslProtocolTLS12, sslProtocolTLS13]) and
      (LOpenSSLProto in [sslProtocolTLS12, sslProtocolTLS13]),
      Format('WinSSL: %d, OpenSSL: %d', [Ord(LWinSSLProto), Ord(LOpenSSLProto)]));

    Test('密码套件都有效',
      (Length(LWinSSLCipher) > 0) and (Length(LOpenSSLCipher) > 0),
      Format('WinSSL: %s, OpenSSL: %s', [LWinSSLCipher, LOpenSSLCipher]));

  finally
    if LWinSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LWinSSLSocket);
    if LOpenSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LOpenSSLSocket);
  end;
end;

procedure TestDataTransferComparison;
var
  LWinSSLData, LOpenSSLData: string;
  LWinSSLHash, LOpenSSLHash: string;
  LWinSSLSuccess, LOpenSSLSuccess: Boolean;
begin
  BeginSection('数据传输对比');

  // Test small data transfer (robots.txt)
  LWinSSLSuccess := FetchHTTPS(sslWinSSL, 'www.google.com', '/robots.txt', LWinSSLData);
  Test('WinSSL 小数据传输', LWinSSLSuccess and (Length(LWinSSLData) > 0),
    Format('大小: %d bytes', [Length(LWinSSLData)]));

  LOpenSSLSuccess := FetchHTTPS(sslOpenSSL, 'www.google.com', '/robots.txt', LOpenSSLData);
  Test('OpenSSL 小数据传输', LOpenSSLSuccess and (Length(LOpenSSLData) > 0),
    Format('大小: %d bytes', [Length(LOpenSSLData)]));

  // Compare data integrity
  if LWinSSLSuccess and LOpenSSLSuccess then
  begin
    LWinSSLHash := CalculateMD5(LWinSSLData);
    LOpenSSLHash := CalculateMD5(LOpenSSLData);

    Test('数据完整性一致 (MD5)', LWinSSLHash = LOpenSSLHash,
      Format('WinSSL: %s, OpenSSL: %s', [LWinSSLHash, LOpenSSLHash]));

    Test('数据长度相同', Length(LWinSSLData) = Length(LOpenSSLData),
      Format('WinSSL: %d, OpenSSL: %d', [Length(LWinSSLData), Length(LOpenSSLData)]));
  end;

  // Test medium data transfer (main page)
  LWinSSLSuccess := FetchHTTPS(sslWinSSL, 'www.cloudflare.com', '/', LWinSSLData);
  Test('WinSSL 中等数据传输', LWinSSLSuccess and (Length(LWinSSLData) > 1024),
    Format('大小: %d bytes', [Length(LWinSSLData)]));

  LOpenSSLSuccess := FetchHTTPS(sslOpenSSL, 'www.cloudflare.com', '/', LOpenSSLData);
  Test('OpenSSL 中等数据传输', LOpenSSLSuccess and (Length(LOpenSSLData) > 1024),
    Format('大小: %d bytes', [Length(LOpenSSLData)]));

  // Compare medium data integrity
  if LWinSSLSuccess and LOpenSSLSuccess then
  begin
    LWinSSLHash := CalculateMD5(LWinSSLData);
    LOpenSSLHash := CalculateMD5(LOpenSSLData);

    Test('中等数据完整性一致 (MD5)', LWinSSLHash = LOpenSSLHash,
      Format('WinSSL MD5: %s...', [Copy(LWinSSLHash, 1, 16)]));
  end;
end;

procedure TestCertificateHandling;
var
  LWinSSLLib, LOpenSSLLib: ISSLLibrary;
  LWinSSLCtx, LOpenSSLCtx: ISSLContext;
  LWinSSLConn, LOpenSSLConn: ISSLConnection;
  LWinSSLSocket, LOpenSSLSocket: TSocket;
  LWinSSLCert, LOpenSSLCert: ISSLCertificate;
  LWinSSLSubject, LOpenSSLSubject: string;
begin
  BeginSection('证书处理对比');

  LWinSSLSocket := INVALID_SOCKET;
  LOpenSSLSocket := INVALID_SOCKET;

  try
    // WinSSL certificate handling
    LWinSSLLib := CreateSSLLibrary(sslWinSSL);
    if LWinSSLLib.Initialize then
    begin
      LWinSSLCtx := LWinSSLLib.CreateContext(sslCtxClient);
      LWinSSLCtx.SetServerName('www.google.com');
      LWinSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LWinSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 443, LWinSSLSocket) then
      begin
        LWinSSLConn := LWinSSLCtx.CreateConnection(LWinSSLSocket);
        if LWinSSLConn.Connect then
        begin
          LWinSSLCert := LWinSSLConn.GetPeerCertificate;
          Test('WinSSL 可获取对端证书', LWinSSLCert <> nil);

          if LWinSSLCert <> nil then
          begin
            LWinSSLSubject := LWinSSLCert.GetSubject;
            Test('WinSSL 证书 Subject 有效',
              (Length(LWinSSLSubject) > 0) and (Pos('google', LowerCase(LWinSSLSubject)) > 0));
            Test('WinSSL 证书指纹有效',
              Length(LWinSSLCert.GetFingerprintSHA256) > 0);
          end;

          LWinSSLConn.Shutdown;
        end;

        CloseSSLSocket(LWinSSLSocket);
        LWinSSLSocket := INVALID_SOCKET;
      end;
    end;

    // OpenSSL certificate handling
    LOpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    if LOpenSSLLib.Initialize then
    begin
      LOpenSSLCtx := LOpenSSLLib.CreateContext(sslCtxClient);
      LOpenSSLCtx.SetServerName('www.google.com');
      LOpenSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LOpenSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 443, LOpenSSLSocket) then
      begin
        LOpenSSLConn := LOpenSSLCtx.CreateConnection(LOpenSSLSocket);
        if LOpenSSLConn.Connect then
        begin
          LOpenSSLCert := LOpenSSLConn.GetPeerCertificate;
          Test('OpenSSL 可获取对端证书', LOpenSSLCert <> nil);

          if LOpenSSLCert <> nil then
          begin
            LOpenSSLSubject := LOpenSSLCert.GetSubject;
            Test('OpenSSL 证书 Subject 有效',
              (Length(LOpenSSLSubject) > 0) and (Pos('google', LowerCase(LOpenSSLSubject)) > 0));
            Test('OpenSSL 证书指纹有效',
              Length(LOpenSSLCert.GetFingerprintSHA256) > 0);
          end;

          LOpenSSLConn.Shutdown;
        end;

        CloseSSLSocket(LOpenSSLSocket);
        LOpenSSLSocket := INVALID_SOCKET;
      end;
    end;

    // Compare certificate info
    Test('证书 Subject 都包含 google',
      (Pos('google', LowerCase(LWinSSLSubject)) > 0) and
      (Pos('google', LowerCase(LOpenSSLSubject)) > 0));

  finally
    if LWinSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LWinSSLSocket);
    if LOpenSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LOpenSSLSocket);
  end;
end;

procedure TestErrorHandling;
var
  LWinSSLLib, LOpenSSLLib: ISSLLibrary;
  LWinSSLCtx, LOpenSSLCtx: ISSLContext;
  LWinSSLConn, LOpenSSLConn: ISSLConnection;
  LWinSSLSocket, LOpenSSLSocket: TSocket;
begin
  BeginSection('错误处理对比');

  LWinSSLSocket := INVALID_SOCKET;
  LOpenSSLSocket := INVALID_SOCKET;

  try
    // Test invalid connection (HTTP port for HTTPS)
    LWinSSLLib := CreateSSLLibrary(sslWinSSL);
    if LWinSSLLib.Initialize then
    begin
      LWinSSLCtx := LWinSSLLib.CreateContext(sslCtxClient);
      LWinSSLCtx.SetServerName('www.google.com');
      LWinSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LWinSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 80, LWinSSLSocket) then
      begin
        LWinSSLConn := LWinSSLCtx.CreateConnection(LWinSSLSocket);
        Test('WinSSL HTTP 端口握手失败（预期）', not LWinSSLConn.Connect);
        CloseSSLSocket(LWinSSLSocket);
        LWinSSLSocket := INVALID_SOCKET;
      end;
    end;

    LOpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    if LOpenSSLLib.Initialize then
    begin
      LOpenSSLCtx := LOpenSSLLib.CreateContext(sslCtxClient);
      LOpenSSLCtx.SetServerName('www.google.com');
      LOpenSSLCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LOpenSSLCtx.SetVerifyMode([]);

      if ConnectToHost('www.google.com', 80, LOpenSSLSocket) then
      begin
        LOpenSSLConn := LOpenSSLCtx.CreateConnection(LOpenSSLSocket);
        Test('OpenSSL HTTP 端口握手失败（预期）', not LOpenSSLConn.Connect);
        CloseSSLSocket(LOpenSSLSocket);
        LOpenSSLSocket := INVALID_SOCKET;
      end;
    end;

    // Test deprecated protocol (SSL 3.0)
    LWinSSLCtx := LWinSSLLib.CreateContext(sslCtxClient);
    LWinSSLCtx.SetServerName('www.google.com');
    LWinSSLCtx.SetProtocolVersions([sslProtocolSSL3]);
    LWinSSLCtx.SetVerifyMode([]);

    if ConnectToHost('www.google.com', 443, LWinSSLSocket) then
    begin
      LWinSSLConn := LWinSSLCtx.CreateConnection(LWinSSLSocket);
      Test('WinSSL SSL3 握手失败（预期）', not LWinSSLConn.Connect);
      CloseSSLSocket(LWinSSLSocket);
      LWinSSLSocket := INVALID_SOCKET;
    end;

    LOpenSSLCtx := LOpenSSLLib.CreateContext(sslCtxClient);
    LOpenSSLCtx.SetServerName('www.google.com');
    LOpenSSLCtx.SetProtocolVersions([sslProtocolSSL3]);
    LOpenSSLCtx.SetVerifyMode([]);

    if ConnectToHost('www.google.com', 443, LOpenSSLSocket) then
    begin
      LOpenSSLConn := LOpenSSLCtx.CreateConnection(LOpenSSLSocket);
      Test('OpenSSL SSL3 握手失败（预期）', not LOpenSSLConn.Connect);
      CloseSSLSocket(LOpenSSLSocket);
      LOpenSSLSocket := INVALID_SOCKET;
    end;

  finally
    if LWinSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LWinSSLSocket);
    if LOpenSSLSocket <> INVALID_SOCKET then
      CloseSSLSocket(LOpenSSLSocket);
  end;
end;

{$IFNDEF WINDOWS}
procedure TestOpenSSLOnLinux;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  Runner.Check('OpenSSL library creation', CreateSSLLibrary(sslOpenSSL) <> nil);
  LLib := CreateSSLLibrary(sslOpenSSL);
  Runner.Check('OpenSSL initialization', LLib.Initialize);
  Runner.Check('OpenSSL type correct', LLib.GetLibraryType = sslOpenSSL);
  Runner.Check('OpenSSL version string', Length(LLib.GetVersionString) > 0);
  Runner.Check('OpenSSL supports TLS 1.2', LLib.IsProtocolSupported(sslProtocolTLS12));
  LCtx := LLib.CreateContext(sslCtxClient);
  Runner.Check('OpenSSL create client context', LCtx <> nil);
end;
{$ENDIF}

begin
  WriteLn('=========================================');
  WriteLn('WinSSL vs OpenSSL Backend Comparison Tests');
  WriteLn('=========================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);
    WriteLn('Test Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    {$IFDEF WINDOWS}
    if not InitWinsock then
    begin
      WriteLn('ERROR: Winsock init failed');
      Halt(1);
    end;

    try
      TestBasicFunctionality;
      TestTLSHandshakeComparison;
      TestDataTransferComparison;
      TestCertificateHandling;
      TestErrorHandling;
    finally
      CleanupWinsock;
    end;
    {$ELSE}
    WriteLn('');
    WriteLn('Note: Full backend comparison requires Windows platform');
    WriteLn('Running OpenSSL-only tests on Linux...');
    WriteLn('');

    // Test OpenSSL functionality only on Linux
    TestOpenSSLOnLinux;

    Runner.Check('WinSSL tests skipped (Linux)', True, 'Full comparison requires Windows');
    {$ENDIF}

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
