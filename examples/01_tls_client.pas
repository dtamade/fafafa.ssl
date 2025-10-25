program tls_client;

{$mode objfpc}{$H+}

{ ============================================================================
  示例 1: TLS 客户端连接
  
  功能：演示如何创建一个简单的 TLS 客户端，连接到 HTTPS 服务器
  用途：学习基本的 TLS 连接建立和数据传输
  
  编译：fpc -Fusrc -Fusrc\openssl 01_tls_client.pas
  运行：01_tls_client.exe
  ============================================================================ }

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}WinSock2{$ELSE}Sockets{$ENDIF},
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

const
  SERVER_HOST = 'www.example.com';
  SERVER_PORT = 443;
  
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LRequest, LResponse: string;
  
function ConnectToServer(const aHost: string; aPort: Word): TSocket;
var
  LAddr: TSockAddr;
  LHostEnt: PHostEnt;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
    raise Exception.Create('Failed to create socket');
  
  LHostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if LHostEnt = nil then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Failed to resolve host: %s', [aHost]);
  end;
  
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(aPort);
  LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;
  
  if connect(Result, LAddr, SizeOf(LAddr)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Failed to connect to %s:%d', [aHost, aPort]);
  end;
end;

begin
  WriteLn('================================================================================');
  WriteLn('  示例 1: TLS 客户端连接');
  WriteLn('  连接到: ', SERVER_HOST, ':', SERVER_PORT);
  WriteLn('================================================================================');
  WriteLn;
  
  try
    // 1. 初始化 SSL 库
    WriteLn('[1/6] 初始化 SSL 库...');
    LLib := CreateOpenSSLLibrary;
    if not LLib.Initialize then
      raise Exception.Create('Failed to initialize SSL library');
    WriteLn('      ✓ SSL 库初始化成功');
    WriteLn('      版本: ', LLib.GetVersionString);
    WriteLn;
    
    try
      // 2. 创建客户端上下文
      WriteLn('[2/6] 创建 SSL 上下文...');
      LContext := LLib.CreateContext(sslCtxClient);
      WriteLn('      ✓ 上下文创建成功');
      WriteLn;
      
      // 3. 配置 TLS 参数
      WriteLn('[3/6] 配置 TLS 参数...');
      LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LContext.SetVerifyMode([sslVerifyPeer]);
      WriteLn('      ✓ 协议版本: TLS 1.2 / 1.3');
      WriteLn('      ✓ 证书验证: 已启用');
      WriteLn;
      
      // 4. 建立 TCP 连接
      WriteLn('[4/6] 建立 TCP 连接...');
      {$IFDEF MSWINDOWS}
      var LWSAData: TWSAData;
      WSAStartup(MAKEWORD(2, 2), LWSAData);
      {$ENDIF}
      
      LSocket := ConnectToServer(SERVER_HOST, SERVER_PORT);
      WriteLn('      ✓ TCP 连接已建立');
      WriteLn;
      
      // 5. 执行 TLS 握手
      WriteLn('[5/6] 执行 TLS 握手...');
      LConn := LContext.CreateConnection(LSocket);
      
      if LConn.Connect then
      begin
        WriteLn('      ✓ TLS 握手成功');
        WriteLn('      协议: ', GetProtocolName(LConn.GetProtocolVersion));
        WriteLn('      密码套件: ', LConn.GetCipherName);
        WriteLn('      密钥强度: ', LConn.GetCipherBits, ' bits');
        WriteLn;
        
        // 验证服务器证书
        var LCert := LConn.GetPeerCertificate;
        if LCert <> nil then
        begin
          WriteLn('      服务器证书:');
          WriteLn('        主题: ', LCert.GetSubject);
          WriteLn('        颁发者: ', LCert.GetIssuer);
          WriteLn('        有效期至: ', DateTimeToStr(LCert.GetNotAfter));
          
          if LCert.VerifyHostname(SERVER_HOST) then
            WriteLn('        主机名验证: ✓ 通过')
          else
            WriteLn('        主机名验证: ✗ 失败');
        end;
        WriteLn;
        
        // 6. 发送 HTTPS 请求
        WriteLn('[6/6] 发送 HTTPS 请求...');
        LRequest := 'GET / HTTP/1.1'#13#10 +
                    'Host: ' + SERVER_HOST + #13#10 +
                    'User-Agent: fafafa.ssl-example/1.0'#13#10 +
                    'Connection: close'#13#10 +
                    #13#10;
        
        LConn.WriteString(LRequest);
        WriteLn('      ✓ 请求已发送 (', Length(LRequest), ' 字节)');
        WriteLn;
        
        // 接收响应
        WriteLn('      接收响应...');
        LResponse := LConn.ReadString;
        
        if Length(LResponse) > 0 then
        begin
          WriteLn('      ✓ 收到响应 (', Length(LResponse), ' 字节)');
          WriteLn;
          WriteLn('      响应头部:');
          WriteLn('      ', '─' * 70);
          
          // 只显示前10行
          var LLines := LResponse.Split([#13#10]);
          for var i := 0 to Min(9, High(LLines)) do
            if LLines[i] <> '' then
              WriteLn('      ', LLines[i]);
          
          if Length(LLines) > 10 then
            WriteLn('      ... (', Length(LLines) - 10, ' 行已省略)');
          
          WriteLn('      ', '─' * 70);
        end;
        WriteLn;
        
        // 关闭连接
        WriteLn('      关闭连接...');
        LConn.Shutdown;
        WriteLn('      ✓ 连接已关闭');
      end
      else
      begin
        WriteLn('      ✗ TLS 握手失败');
        WriteLn('      错误: ', LLib.GetLastErrorString);
      end;
      
      closesocket(LSocket);
      {$IFDEF MSWINDOWS}
      WSACleanup;
      {$ENDIF}
      
    finally
      LLib.Finalize;
    end;
    
    WriteLn;
    WriteLn('================================================================================');
    WriteLn('  示例执行完成！');
    WriteLn('================================================================================');
    WriteLn;
    WriteLn('💡 学到的知识：');
    WriteLn('  1. 如何初始化 SSL 库');
    WriteLn('  2. 如何创建和配置 SSL 上下文');
    WriteLn('  3. 如何建立 TCP 连接');
    WriteLn('  4. 如何执行 TLS 握手');
    WriteLn('  5. 如何验证服务器证书');
    WriteLn('  6. 如何通过 TLS 发送和接收数据');
    WriteLn;
    WriteLn('📚 下一步：');
    WriteLn('  - 查看示例 2: TLS 服务器 (02_tls_server.pas)');
    WriteLn('  - 阅读 docs/USER_GUIDE.md 了解更多用法');
    WriteLn;
    
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('================================================================================');
      WriteLn('  ✗ 错误: ', E.Message);
      WriteLn('================================================================================');
      WriteLn;
      WriteLn('🔧 故障排除：');
      WriteLn('  1. 确保 OpenSSL 已安装且可访问');
      WriteLn('  2. 检查网络连接');
      WriteLn('  3. 确认服务器地址和端口正确');
      WriteLn('  4. 查看 docs/TROUBLESHOOTING.md 获取更多帮助');
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.

