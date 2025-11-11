program simple_ssl_connection;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ELSE}
  BaseUnix, Unix, Sockets,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf;

{
  这个示例展示 fafafa.ssl 的正确使用方式：
  
  1. 用户自己创建socket（使用系统API或任何网络库）
  2. 将socket传入 fafafa.ssl
  3. fafafa.ssl 只负责SSL/TLS加密
  
  这遵循业界最佳实践（OpenSSL、mbedTLS等都是这样设计的）
}

// 辅助函数：创建TCP连接（用户也可以用Synapse、Indy等网络库）
function CreateTCPConnection(const aHost: string; aPort: Word): THandle;
{$IFDEF WINDOWS}
var
  WSAData: TWSAData;
  Sock: TSocket;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
begin
  Result := INVALID_HANDLE_VALUE;
  
  // 初始化WinSock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    raise Exception.Create('WSAStartup failed');
    
  // 创建socket
  Sock := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Sock = INVALID_SOCKET then
    raise Exception.Create('Socket creation failed');
    
  // 解析主机名
  HostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if HostEnt = nil then
  begin
    closesocket(Sock);
    raise Exception.CreateFmt('Cannot resolve host: %s', [aHost]);
  end;
  
  // 连接
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(aPort);
  Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;
  
  if WinSock2.connect(Sock, Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(Sock);
    raise Exception.CreateFmt('Cannot connect to %s:%d', [aHost, aPort]);
  end;
  
  Result := Sock;
end;
{$ELSE}
var
  Sock: cint;
  Addr: TInetSockAddr;
  HostEnt: PHostEnt;
begin
  Result := -1;
  
  // 创建socket
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    raise Exception.Create('Socket creation failed');
    
  // 解析主机名
  HostEnt := gethostbyname(PChar(aHost));
  if HostEnt = nil then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Cannot resolve host: %s', [aHost]);
  end;
  
  // 连接
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(aPort);
  Move(HostEnt^.h_addr_list^^, Addr.sin_addr, SizeOf(Addr.sin_addr));
  
  if fpConnect(Sock, @Addr, SizeOf(Addr)) <> 0 then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Cannot connect to %s:%d', [aHost, aPort]);
  end;
  
  Result := Sock;
end;
{$ENDIF}

procedure CloseSocket(aSocket: THandle);
begin
  {$IFDEF WINDOWS}
  if aSocket <> INVALID_HANDLE_VALUE then
    closesocket(aSocket);
  {$ELSE}
  if aSocket >= 0 then
    fpClose(aSocket);
  {$ENDIF}
end;

procedure SimpleSSLConnection;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LSocket: THandle;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
  LResponse: string;
begin
  WriteLn('╔══════════════════════════════════════════════════════════════╗');
  WriteLn('║   简单SSL/TLS连接示例                                       ║');
  WriteLn('║   展示如何结合socket和fafafa.ssl                            ║');
  WriteLn('╚══════════════════════════════════════════════════════════════╝');
  WriteLn;
  
  LSocket := {$IFDEF WINDOWS}INVALID_HANDLE_VALUE{$ELSE}-1{$ENDIF};
  
  try
    WriteLn('[1/6] 创建SSL上下文...');
    LContext := TSSLFactory.CreateContext(sslCtxClient);
    LContext.SetVerifyMode([sslVerifyPeer]);
    WriteLn('      ✓ SSL上下文已创建');
    WriteLn;
    
    WriteLn('[2/6] 创建TCP连接...');
    WriteLn('      目标: example.com:443');
    WriteLn('      注意: 我们用系统API创建socket');
    WriteLn('            （也可以用Synapse、Indy等网络库）');
    LSocket := CreateTCPConnection('example.com', 443);
    WriteLn('      ✓ TCP连接已建立 (Socket Handle: ', LSocket, ')');
    WriteLn;
    
    WriteLn('[3/6] 将socket传入SSL库...');
    WriteLn('      fafafa.ssl 不创建socket，只接收已有的socket');
    LConnection := LContext.CreateConnection(LSocket);
    LConnection.SetHostname('example.com');  // 设置SNI
    WriteLn('      ✓ SSL连接对象已创建');
    WriteLn;
    
    WriteLn('[4/6] 执行SSL握手...');
    if not LConnection.Connect then
      raise Exception.Create('SSL握手失败');
    WriteLn('      ✓ SSL握手成功');
    WriteLn('      协议: ', Ord(LConnection.GetProtocol));
    WriteLn('      密码套件: ', LConnection.GetCipher);
    WriteLn;
    
    WriteLn('[5/6] 发送数据（HTTP请求）...');
    // fafafa.ssl只负责SSL/TLS加密
    // 应用层协议（HTTP/SMTP/FTP等）由用户自己实现
    LRequest := 
      'GET / HTTP/1.1'#13#10 +
      'Host: example.com'#13#10 +
      'User-Agent: fafafa.ssl-example/1.0'#13#10 +
      'Connection: close'#13#10 +
      #13#10;
    
    if LConnection.Write(@LRequest[1], Length(LRequest)) <> Length(LRequest) then
      raise Exception.Create('写入数据失败');
    WriteLn('      ✓ 请求已发送 (', Length(LRequest), ' 字节)');
    WriteLn;
    
    WriteLn('[6/6] 接收响应...');
    LResponse := '';
    repeat
      LBytesRead := LConnection.Read(@LBuffer[0], SizeOf(LBuffer));
      if LBytesRead > 0 then
      begin
        SetString(LResponse, PAnsiChar(@LBuffer[0]), LBytesRead);
        Write(LResponse);
      end;
    until LBytesRead <= 0;
    WriteLn;
    WriteLn('      ✓ 响应已接收');
    WriteLn;
    
    WriteLn('[完成] 连接已关闭');
    WriteLn;
    WriteLn('════════════════════════════════════════════════════════════');
    WriteLn('总结：');
    WriteLn('  1. 用户创建socket（任何方式）');
    WriteLn('  2. 传入fafafa.ssl进行SSL/TLS加密');
    WriteLn('  3. fafafa.ssl专注于加密，不管理网络');
    WriteLn('  4. 这是OpenSSL/mbedTLS等的标准模式');
    WriteLn('════════════════════════════════════════════════════════════');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ 错误: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  // 清理socket（用户负责）
  if LSocket <> {$IFDEF WINDOWS}INVALID_HANDLE_VALUE{$ELSE}-1{$ENDIF} then
    CloseSocket(LSocket);
end;

begin
  WriteLn;
  SimpleSSLConnection;
  WriteLn;
  WriteLn('按回车键退出...');
  ReadLn;
end.
