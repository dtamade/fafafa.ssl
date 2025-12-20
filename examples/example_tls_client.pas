program example_tls_client;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

uses
  SysUtils, Classes, WinSock2,
  // SSL 库接口
  fafafa.ssl.base,
  fafafa.ssl.factory;

var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Socket: TSocket;
  SockAddr: TSockAddrIn;
  HostEnt: PHostEnt;
  WSAData: TWSAData;
  
procedure InitWinSock;
begin
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    raise Exception.Create('WSAStartup 失败');
end;

procedure CleanupWinSock;
begin
  WSACleanup;
end;

function ConnectToHost(const Host: string; Port: Word): TSocket;
var
  HostAnsi: AnsiString;
begin
  Result := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
    raise Exception.Create('创建 socket 失败');
  
  HostAnsi := AnsiString(Host);
  HostEnt := gethostbyname(PAnsiChar(HostAnsi));
  if HostEnt = nil then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('无法解析主机名: %s', [Host]);
  end;
  
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(Port);
  SockAddr.sin_addr.S_addr := PInAddr(HostEnt^.h_addr_list^)^.S_addr;
  
  if connect(Result, TSockAddr(SockAddr), SizeOf(SockAddr)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('连接到 %s:%d 失败', [Host, Port]);
  end;
end;

procedure TestHTTPSConnection(const Host: string; Port: Word = 443);
var
  Request, Response: string;
  BytesRead, TotalBytes: Integer;
  Buffer: array[0..4095] of Byte;
  Certificate: ISSLCertificate;
begin
  WriteLn('========================================');
  WriteLn('测试 HTTPS 连接: ', Host, ':', Port);
  WriteLn('========================================');
  WriteLn;
  
  try
    // 1. 建立 TCP 连接
    Write('1. 建立 TCP 连接... ');
    Socket := ConnectToHost(Host, Port);
    WriteLn('成功');
    
    try
      // 2. 创建 SSL 库实例
      Write('2. 创建 SSL 库实例... ');
      SSLLib := TSSLFactory.GetInstance('OpenSSL');
      if not Assigned(SSLLib) then
        raise Exception.Create('无法创建 OpenSSL 库实例');
      WriteLn('成功');
      
      // 3. 初始化 SSL 库
      Write('3. 初始化 SSL 库... ');
      SSLLib.Initialize;
      WriteLn('成功');
      
      // 4. 创建 SSL 上下文
      Write('4. 创建 SSL 上下文... ');
      Context := SSLLib.CreateContext(TSSLMethod.TLS_Client);
      if not Assigned(Context) then
        raise Exception.Create('无法创建 SSL 上下文');
      
      // 设置选项
      Context.SetOptions([
        TSSLOption.NoSSLv2,
        TSSLOption.NoSSLv3,
        TSSLOption.NoCompression
      ]);
      
      // 设置验证模式（这里简化处理，实际应用应验证证书）
      Context.SetVerifyMode(TSSLVerifyMode.None);
      WriteLn('成功');
      
      // 5. 创建 SSL 连接
      Write('5. 创建 SSL 连接... ');
      Connection := Context.CreateConnection;
      if not Assigned(Connection) then
        raise Exception.Create('无法创建 SSL 连接');
      WriteLn('成功');
      
      // 6. 关联 socket
      Write('6. 关联 socket... ');
      Connection.SetSocket(Socket);
      WriteLn('成功');
      
      // 7. 设置 SNI
      Write('7. 设置 SNI... ');
      Connection.SetHostName(Host);
      WriteLn('成功');
      
      // 8. 执行 SSL 握手
      Write('8. 执行 SSL 握手... ');
      if not Connection.Connect then
        raise Exception.Create('SSL 握手失败: ' + Connection.GetLastError);
      WriteLn('成功');
      
      // 9. 获取服务器证书信息
      WriteLn;
      WriteLn('9. 服务器证书信息:');
      Certificate := Connection.GetPeerCertificate;
      if Assigned(Certificate) then
      begin
        WriteLn('   主题: ', Certificate.GetSubject);
        WriteLn('   颁发者: ', Certificate.GetIssuer);
        WriteLn('   有效期从: ', DateTimeToStr(Certificate.GetNotBefore));
        WriteLn('   有效期至: ', DateTimeToStr(Certificate.GetNotAfter));
        WriteLn('   序列号: ', Certificate.GetSerialNumber);
        
        // 显示 SAN
        var SANs := Certificate.GetSubjectAltNames;
        if Length(SANs) > 0 then
        begin
          WriteLn('   备用名称:');
          for var SAN in SANs do
            WriteLn('     - ', SAN);
        end;
      end
      else
        WriteLn('   无法获取证书');
      
      // 10. 显示连接信息
      WriteLn;
      WriteLn('10. 连接信息:');
      WriteLn('    协议版本: ', Connection.GetProtocolVersion);
      WriteLn('    加密套件: ', Connection.GetCipherName);
      
      // 11. 发送 HTTP 请求
      WriteLn;
      WriteLn('11. 发送 HTTP GET 请求...');
      Request := Format('GET / HTTP/1.1' + #13#10 +
                       'Host: %s' + #13#10 +
                       'User-Agent: Pascal-SSL-Client/1.0' + #13#10 +
                       'Accept: */*' + #13#10 +
                       'Connection: close' + #13#10 +
                       #13#10, [Host]);
      
      var RequestBytes := TEncoding.UTF8.GetBytes(Request);
      var BytesSent := Connection.Write(RequestBytes[0], Length(RequestBytes));
      WriteLn('    发送 ', BytesSent, ' 字节');
      
      // 12. 接收响应
      WriteLn;
      WriteLn('12. 接收 HTTP 响应:');
      Response := '';
      TotalBytes := 0;
      
      repeat
        BytesRead := Connection.Read(Buffer[0], SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          Inc(TotalBytes, BytesRead);
          SetLength(Response, Length(Response) + BytesRead);
          Move(Buffer[0], Response[Length(Response) - BytesRead + 1], BytesRead);
        end;
      until BytesRead <= 0;
      
      WriteLn('    接收 ', TotalBytes, ' 字节');
      
      // 显示响应头
      WriteLn;
      WriteLn('13. HTTP 响应头:');
      var HeaderEnd := Pos(#13#10#13#10, Response);
      if HeaderEnd > 0 then
      begin
        var Headers := Copy(Response, 1, HeaderEnd - 1);
        var Lines := Headers.Split([#13#10]);
        var LineCount := 0;
        for var Line in Lines do
        begin
          Inc(LineCount);
          WriteLn('    ', Line);
          if LineCount >= 10 then
          begin
            WriteLn('    ... (更多)');
            Break;
          end;
        end;
      end;
      
      // 14. 关闭连接
      WriteLn;
      Write('14. 关闭 SSL 连接... ');
      Connection.Disconnect;
      WriteLn('成功');
      
    finally
      if Socket <> INVALID_SOCKET then
        closesocket(Socket);
    end;
    
    WriteLn;
    WriteLn('测试成功完成！');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('错误: ', E.Message);
      raise;
    end;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('用法: ', ExtractFileName(ParamStr(0)), ' [主机名] [端口]');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' www.google.com');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' www.example.com 443');
  WriteLn;
  WriteLn('默认: www.example.com:443');
end;

var
  Host: string;
  Port: Word;
  
begin
  WriteLn('========================================');
  WriteLn('     OpenSSL TLS 客户端示例');
  WriteLn('========================================');
  WriteLn;
  
  // 解析命令行参数
  if ParamCount >= 1 then
  begin
    Host := ParamStr(1);
    if ParamCount >= 2 then
      Port := StrToIntDef(ParamStr(2), 443)
    else
      Port := 443;
  end
  else
  begin
    // 使用默认值
    Host := 'www.example.com';
    Port := 443;
    WriteLn('使用默认主机: ', Host, ':', Port);
    WriteLn('(使用 --help 查看更多选项)');
    WriteLn;
  end;
  
  // 检查帮助
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin
    ShowUsage;
    Exit;
  end;
  
  try
    // 初始化 WinSock
    InitWinSock;
    try
      // 注册 OpenSSL 后端
      WriteLn('注册 SSL 后端...');
      // 这里应该调用注册函数，如果有的话
      // RegisterOpenSSLBackend;
      
      // 执行测试
      TestHTTPSConnection(Host, Port);
      
    finally
      CleanupWinSock;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('发生错误: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn;
  WriteLn('按 Enter 退出...');
  ReadLn;
end.