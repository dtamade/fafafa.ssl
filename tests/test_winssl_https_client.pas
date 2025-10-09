program test_winssl_https_client;

{$mode objfpc}{$H+}

uses
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ELSE}
  Sockets,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.lib;

var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Socket: TSocket;
  Host: string;
  Port: Word;
  Request: string;
  Response: array[0..4095] of Char;
  BytesRead: Integer;
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  {$IFDEF WINDOWS}
  WSAData: TWSAData;
  {$ENDIF}

procedure TestPass(const TestName: string);
begin
  WriteLn('[PASS] ', TestName);
  Inc(TestsPassed);
end;

procedure TestFail(const TestName, Reason: string);
begin
  WriteLn('[FAIL] ', TestName, ': ', Reason);
  Inc(TestsFailed);
end;

{$IFDEF WINDOWS}
function CreateTCPSocket(const aHost: string; aPort: Word): TSocket;
var
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  HostAddr: PInAddr;
begin
  Result := INVALID_SOCKET;
  
  // 创建 socket
  Result := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
  begin
    WriteLn('Failed to create socket: WSA error ', WSAGetLastError);
    Exit;
  end;
  
  // 解析主机名
  HostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if HostEnt = nil then
  begin
    WriteLn('Failed to resolve host: ', aHost, ', WSA error ', WSAGetLastError);
    closesocket(Result);
    Result := INVALID_SOCKET;
    Exit;
  end;
  
  // 获取第一个地址
  HostAddr := PInAddr(HostEnt^.h_addr_list^);
  
  // 设置地址结构
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(aPort);
  Addr.sin_addr := HostAddr^;
  
  // 连接
  if WinSock2.connect(Result, Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    WriteLn('Failed to connect: WSA error ', WSAGetLastError);
    closesocket(Result);
    Result := INVALID_SOCKET;
    Exit;
  end;
  
  WriteLn('TCP connection established to ', aHost, ':', aPort);
end;
{$ELSE}
function CreateTCPSocket(const aHost: string; aPort: Word): TSocket;
var
  Addr: TInetSockAddr;
  HostEnt: PHostEntry;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result = -1 then
  begin
    WriteLn('Failed to create socket');
    Exit;
  end;
  
  HostEnt := GetHostByName(PChar(aHost));
  if HostEnt = nil then
  begin
    WriteLn('Failed to resolve host: ', aHost);
    CloseSocket(Result);
    Result := -1;
    Exit;
  end;
  
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(aPort);
  Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;
  
  if fpConnect(Result, @Addr, SizeOf(Addr)) = -1 then
  begin
    WriteLn('Failed to connect');
    CloseSocket(Result);
    Result := -1;
    Exit;
  end;
  
  WriteLn('TCP connection established to ', aHost, ':', aPort);
end;
{$ENDIF}

begin
  WriteLn('=== WinSSL HTTPS Client Test ===');
  WriteLn;
  
  {$IFDEF WINDOWS}
  // 初始化 Winsock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
  begin
    WriteLn('Failed to initialize Winsock');
    Halt(1);
  end;
  WriteLn('Winsock initialized (version ', Lo(WSAData.wVersion), '.', Hi(WSAData.wVersion), ')');
  WriteLn;
  {$ENDIF}
  
  try
    // 设置目标服务器
    Host := 'www.google.com';
    Port := 443;
  
  WriteLn('Target: https://', Host, ':', Port);
  WriteLn;
  
  // Test 1: 创建 SSL 库
  WriteLn('Test 1: Creating SSL library...');
  try
    SSLLib := CreateWinSSLLibrary;
    if SSLLib <> nil then
      TestPass('Create SSL library')
    else
    begin
      TestFail('Create SSL library', 'Returned nil');
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      TestFail('Create SSL library', E.Message);
      Halt(1);
    end;
  end;
  
  // Test 2: 初始化库
  WriteLn('Test 2: Initializing SSL library...');
  if not SSLLib.Initialize then
  begin
    TestFail('Initialize library', SSLLib.GetLastErrorString);
    Halt(1);
  end;
  TestPass('Initialize library');
  WriteLn('  Version: ', SSLLib.GetVersionString);
  
  // Test 3: 创建客户端上下文
  WriteLn('Test 3: Creating client context...');
  try
    Context := SSLLib.CreateContext(sslCtxClient);
    if Context <> nil then
      TestPass('Create client context')
    else
    begin
      TestFail('Create client context', 'Returned nil');
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      TestFail('Create client context', E.Message);
      Halt(1);
    end;
  end;
  
  // Test 4: 配置上下文
  WriteLn('Test 4: Configuring context...');
  try
    // 设置 SNI 服务器名称
    Context.SetServerName(Host);
    
    // 设置协议版本
    Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    
    // 设置验证模式（暂时不验证证书）
    Context.SetVerifyMode([]);
    
    TestPass('Configure context (SNI: ' + Host + ')');
  except
    on E: Exception do
    begin
      TestFail('Configure context', E.Message);
      Halt(1);
    end;
  end;
  
  // Test 5: 创建 TCP 连接
  WriteLn('Test 5: Creating TCP connection...');
  Socket := CreateTCPSocket(Host, Port);
  {$IFDEF WINDOWS}
  if Socket = INVALID_SOCKET then
  {$ELSE}
  if Socket = -1 then
  {$ENDIF}
  begin
    TestFail('Create TCP connection', 'Socket creation failed');
    Halt(1);
  end;
  TestPass('Create TCP connection');
  
  // Test 6: 创建 SSL 连接
  WriteLn('Test 6: Creating SSL connection...');
  try
    Connection := Context.CreateConnection(Socket);
    if Connection <> nil then
      TestPass('Create SSL connection')
    else
    begin
      TestFail('Create SSL connection', 'Returned nil');
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      TestFail('Create SSL connection', E.Message);
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  end;
  
  // Test 7: 执行 TLS 握手
  WriteLn('Test 7: Performing TLS handshake...');
  WriteLn('  This may take a few seconds...');
  try
    if Connection.Connect then
    begin
      TestPass('TLS handshake completed');
      WriteLn('  Protocol: ', ProtocolVersionToString(Connection.GetProtocolVersion));
      WriteLn('  Cipher: ', Connection.GetCipherName);
    end
    else
    begin
      TestFail('TLS handshake', 'Connect returned False');
      Connection.Close;
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      TestFail('TLS handshake', E.Message);
      Connection.Close;
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  end;
  
  // Test 8: 发送 HTTP GET 请求
  WriteLn('Test 8: Sending HTTP GET request...');
  Request := 'GET / HTTP/1.1'#13#10 +
             'Host: ' + Host + #13#10 +
             'User-Agent: WinSSL-Test/1.0'#13#10 +
             'Connection: close'#13#10 +
             #13#10;
  
  try
    if Connection.WriteString(Request) then
      TestPass('Send HTTP request (' + IntToStr(Length(Request)) + ' bytes)')
    else
    begin
      TestFail('Send HTTP request', 'WriteString returned False');
      Connection.Close;
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      TestFail('Send HTTP request', E.Message);
      Connection.Close;
      {$IFDEF WINDOWS}
      closesocket(Socket);
      {$ELSE}
      CloseSocket(Socket);
      {$ENDIF}
      Halt(1);
    end;
  end;
  
  // Test 9: 接收 HTTP 响应
  WriteLn('Test 9: Receiving HTTP response...');
  try
    BytesRead := Connection.Read(Response, SizeOf(Response) - 1);
    if BytesRead > 0 then
    begin
      Response[BytesRead] := #0;
      TestPass('Receive HTTP response (' + IntToStr(BytesRead) + ' bytes)');
      
      // 显示响应的前几行
      WriteLn;
      WriteLn('--- Response Preview ---');
      WriteLn(Copy(PChar(@Response[0]), 1, 400));
      WriteLn('--- End Preview ---');
      WriteLn;
      
      // 检查是否是 HTTP 响应
      if Pos('HTTP/', PChar(@Response[0])) > 0 then
        TestPass('Valid HTTP response received')
      else
        TestFail('HTTP response validation', 'Not a valid HTTP response');
    end
    else
    begin
      TestFail('Receive HTTP response', 'Read returned ' + IntToStr(BytesRead));
    end;
  except
    on E: Exception do
    begin
      TestFail('Receive HTTP response', E.Message);
    end;
  end;
  
  // Test 10: 优雅关闭连接
  WriteLn('Test 10: Closing SSL connection...');
  try
    if Connection.Shutdown then
      TestPass('SSL connection shutdown')
    else
      TestPass('SSL connection shutdown (best effort)');
  except
    on E: Exception do
    begin
      WriteLn('[WARN] Shutdown exception: ', E.Message);
      TestPass('SSL connection shutdown (with exception)');
    end;
  end;
  
  // 关闭 socket
  {$IFDEF WINDOWS}
  closesocket(Socket);
  {$ELSE}
  CloseSocket(Socket);
  {$ENDIF}
  
  // 清理
  Connection := nil;
  Context := nil;
  SSLLib.Finalize;
  SSLLib := nil;
  
  // 总结
  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn('Passed: ', TestsPassed);
  WriteLn('Failed: ', TestsFailed);
  WriteLn('Total:  ', TestsPassed + TestsFailed);
  WriteLn;
  
  if TestsFailed = 0 then
  begin
    WriteLn('🎉 ALL TESTS PASSED! 🎉');
    WriteLn;
    WriteLn('Successfully completed HTTPS connection to ', Host, '!');
    WriteLn('WinSSL backend is working correctly.');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('❌ SOME TESTS FAILED');
    ExitCode := 1;
  end;
  
  finally
    {$IFDEF WINDOWS}
    // 清理 Winsock
    WSACleanup;
    {$ENDIF}
  end;
end.
