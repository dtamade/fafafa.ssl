program test_real_https_connection;

{$mode objfpc}{$H+}{$J-}

{**
 * 真实网络 HTTPS 连接测试
 *
 * P1-1: 真实网络集成测试
 *
 * 测试内容:
 * - 连接真实 HTTPS 服务器
 * - TLS 握手验证
 * - 证书验证
 * - HTTP/1.1 请求/响应
 * - TLS 1.2/1.3 协议测试
 * - SNI 和 ALPN 功能
 *
 * 注意: 此测试需要网络连接
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  {$IFDEF UNIX}
  ctypes, BaseUnix,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.sockets,
  fafafa.ssl.openssl.backed;

var
  Total, Passed, Failed, Skipped: Integer;
  Section: string;

procedure BeginSection(const aName: string);
begin
  Section := aName;
  WriteLn;
  WriteLn('=== ', aName, ' ===');
end;

procedure Check(const aName: string; ok: Boolean; const details: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', aName, ': ');
  if ok then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if details <> '' then
      WriteLn('    ', details);
  end;
end;

procedure Skip(const aName: string; const reason: string);
begin
  Inc(Total);
  Inc(Skipped);
  WriteLn('  [', Section, '] ', aName, ': SKIP');
  WriteLn('    ', reason);
end;

{ 创建 TCP 连接 }
function CreateTCPSocket: TSocket;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
end;

{ 解析主机名 }
function ResolveHostname(const AHostname: string): Cardinal;
var
  HostEnt: PHostEnt;
  HostnameA: AnsiString;
begin
  Result := INADDR_NONE;
  HostnameA := AnsiString(AHostname);

  // 首先尝试作为 IP 地址解析
  Result := inet_addr(PAnsiChar(HostnameA));
  if Result <> INADDR_NONE then
    Exit;

  // 通过 DNS 解析
  HostEnt := gethostbyname(PAnsiChar(HostnameA));
  if (HostEnt <> nil) and (HostEnt^.h_addr_list <> nil) and
     (HostEnt^.h_addr_list^ <> nil) then
    Result := PCardinal(HostEnt^.h_addr_list^)^;
end;

{ 连接到服务器 }
function ConnectToHost(ASocket: TSocket; const AHostname: string; APort: Word): Boolean;
var
  Addr: TSockAddrIn;
  IP: Cardinal;
begin
  Result := False;

  IP := ResolveHostname(AHostname);
  if IP = INADDR_NONE then
    Exit;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr.s_addr := IP;

  Result := connect(ASocket, @Addr, SizeOf(Addr)) = 0;
end;

{ 设置 socket 超时 }
procedure SetSocketTimeout(ASocket: TSocket; ATimeoutMs: Integer);
var
  Timeout: TTimeVal;
begin
  Timeout.tv_sec := ATimeoutMs div 1000;
  Timeout.tv_usec := (ATimeoutMs mod 1000) * 1000;
  setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  setsockopt(ASocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
end;

{ 测试连接单个 HTTPS 站点 }
function TestHTTPSConnection(const AHostname: string; APort: Word = 443): Boolean;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  Request, Response: string;
  Buffer: array[0..4095] of Byte;
  BytesRead, BytesWritten: Integer;
  ResponseStr: AnsiString;
  RequestBytes: TBytes;
begin
  Result := False;
  Sock := INVALID_SOCKET;

  try
    // 1. 创建 TCP socket
    Sock := CreateTCPSocket;
    if Sock = INVALID_SOCKET then
    begin
      WriteLn('    无法创建 socket');
      Exit;
    end;

    // 设置超时 (10秒)
    SetSocketTimeout(Sock, 10000);

    // 2. TCP 连接
    if not ConnectToHost(Sock, AHostname, APort) then
    begin
      WriteLn('    TCP 连接失败: ', AHostname);
      Exit;
    end;

    // 3. 初始化 SSL
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      WriteLn('    OpenSSL 初始化失败');
      Exit;
    end;

    // 4. 创建 SSL 上下文
    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      WriteLn('    创建 SSL 上下文失败');
      Exit;
    end;

    // 配置 TLS
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName(AHostname);
    // 使用系统 CA 证书进行验证，如果不可用则跳过
    if FileExists('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
      Ctx.SetVerifyMode([sslVerifyPeer]);
    end
    else
      Ctx.SetVerifyMode([]);  // 跳过验证
    Ctx.SetALPNProtocols('http/1.1');

    // 5. 创建 SSL 连接
    Conn := Ctx.CreateConnection(Sock);
    if Conn = nil then
    begin
      WriteLn('    创建 SSL 连接失败');
      Exit;
    end;

    // 6. TLS 握手
    if not Conn.Connect then
    begin
      WriteLn('    TLS 握手失败: ', Conn.GetStateString);
      Exit;
    end;

    // 7. 发送 HTTP 请求
    Request := 'GET / HTTP/1.1' + #13#10 +
               'Host: ' + AHostname + #13#10 +
               'User-Agent: fafafa.ssl-test/1.0' + #13#10 +
               'Connection: close' + #13#10 +
               #13#10;

    RequestBytes := TEncoding.UTF8.GetBytes(Request);
    BytesWritten := Conn.Write(RequestBytes[0], Length(RequestBytes));
    if BytesWritten <= 0 then
    begin
      WriteLn('    发送请求失败');
      Exit;
    end;

    // 8. 读取响应
    BytesRead := Conn.Read(Buffer, SizeOf(Buffer));
    if BytesRead <= 0 then
    begin
      WriteLn('    读取响应失败');
      Exit;
    end;

    SetString(ResponseStr, PAnsiChar(@Buffer[0]), BytesRead);
    Response := string(ResponseStr);

    // 9. 验证响应
    Result := (Pos('HTTP/', Response) = 1);

    if Result then
    begin
      // 提取状态码
      if Pos('200', Response) > 0 then
        WriteLn('    状态: 200 OK')
      else if Pos('301', Response) > 0 then
        WriteLn('    状态: 301 Redirect')
      else if Pos('302', Response) > 0 then
        WriteLn('    状态: 302 Redirect')
      else if Pos('403', Response) > 0 then
        WriteLn('    状态: 403 Forbidden (但 TLS 成功)')
      else
        WriteLn('    状态: 其他 HTTP 响应');

      // 显示协议信息
      WriteLn('    协议: ', ProtocolVersionToString(Conn.GetProtocolVersion));
      WriteLn('    密码: ', Conn.GetCipherName);
    end;

  except
    on E: Exception do
    begin
      WriteLn('    异常: ', E.Message);
      Result := False;
    end;
  end;

  // 清理
  if Sock <> INVALID_SOCKET then
    close(Sock);
end;

{ 测试 TLS 版本协商 }
procedure TestTLSVersionNegotiation;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  Protocol: string;
  ProtoVer: TSSLProtocolVersion;
begin
  BeginSection('TLS 版本协商');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    Skip('TLS 1.3 协商', '无法创建 socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.google.com', 443) then
  begin
    close(Sock);
    Skip('TLS 1.3 协商', 'TCP 连接失败');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Skip('TLS 1.3 协商', 'OpenSSL 初始化失败');
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS13]);  // 仅 TLS 1.3
    Ctx.SetServerName('www.google.com');
    Ctx.SetVerifyMode([]);  // 跳过证书验证以简化测试

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      ProtoVer := Conn.GetProtocolVersion;
      Protocol := ProtocolVersionToString(ProtoVer);
      Check('TLS 1.3 协商', ProtoVer = sslProtocolTLS13, '实际协议: ' + Protocol);
    end
    else
      Check('TLS 1.3 协商', False, 'TLS 握手失败');

  except
    on E: Exception do
      Check('TLS 1.3 协商', False, E.Message);
  end;

  close(Sock);
end;

{ 测试证书验证 }
procedure TestCertificateVerification;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  CertInfo: TSSLCertificateInfo;
  PeerCert: ISSLCertificate;
begin
  BeginSection('证书验证');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    Skip('获取服务器证书', '无法创建 socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.github.com', 443) then
  begin
    close(Sock);
    Skip('获取服务器证书', 'TCP 连接失败');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.github.com');
    Ctx.SetVerifyMode([]);  // 先不验证，只获取证书

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      Check('TLS 握手成功', True);

      PeerCert := Conn.GetPeerCertificate;
      if PeerCert <> nil then
      begin
        CertInfo := PeerCert.GetInfo;
        Check('获取证书主题', CertInfo.Subject <> '', '主题: ' + CertInfo.Subject);
        Check('获取证书颁发者', CertInfo.Issuer <> '', '颁发者: ' + CertInfo.Issuer);
        Check('证书有效期', CertInfo.NotAfter > Now, '有效至: ' + DateTimeToStr(CertInfo.NotAfter));
      end
      else
        Check('获取服务器证书', False, '无法获取对端证书');
    end
    else
      Check('TLS 握手成功', False, Conn.GetStateString);

  except
    on E: Exception do
      Check('获取服务器证书', False, E.Message);
  end;

  close(Sock);
end;

{ 测试多个知名网站 }
procedure TestKnownWebsites;
const
  Websites: array[0..4] of string = (
    'www.google.com',
    'www.github.com',
    'www.cloudflare.com',
    'www.microsoft.com',
    'www.amazon.com'
  );
var
  I: Integer;
  SuccessCount: Integer;
begin
  BeginSection('知名网站连接测试');

  SuccessCount := 0;
  for I := Low(Websites) to High(Websites) do
  begin
    Write('  测试 ', Websites[I], '... ');
    if TestHTTPSConnection(Websites[I]) then
    begin
      Inc(SuccessCount);
      Inc(Passed);
    end
    else
      Inc(Failed);
    Inc(Total);
  end;

  WriteLn;
  WriteLn('  成功连接: ', SuccessCount, '/', Length(Websites));
end;

{ 测试 SNI (Server Name Indication) }
procedure TestSNI;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
begin
  BeginSection('SNI 功能测试');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    Skip('SNI 设置', '无法创建 socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  // Cloudflare 使用 SNI
  if not ConnectToHost(Sock, 'www.cloudflare.com', 443) then
  begin
    close(Sock);
    Skip('SNI 设置', 'TCP 连接失败');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.cloudflare.com');  // SNI
    Ctx.SetVerifyMode([]);

    Check('设置 SNI', Ctx.GetServerName = 'www.cloudflare.com');

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
      Check('SNI 握手成功', True)
    else
      Check('SNI 握手成功', False, Conn.GetStateString);

  except
    on E: Exception do
      Check('SNI 功能', False, E.Message);
  end;

  close(Sock);
end;

{ 测试 ALPN }
procedure TestALPN;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  NegotiatedProtocol: string;
begin
  BeginSection('ALPN 协议协商');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    Skip('ALPN 协商', '无法创建 socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.google.com', 443) then
  begin
    close(Sock);
    Skip('ALPN 协商', 'TCP 连接失败');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.google.com');
    Ctx.SetALPNProtocols('h2,http/1.1');  // 优先 HTTP/2
    Ctx.SetVerifyMode([]);

    Check('设置 ALPN 协议', Ctx.GetALPNProtocols = 'h2,http/1.1');

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      Check('ALPN 握手成功', True);
      NegotiatedProtocol := Conn.GetSelectedALPNProtocol;
      Check('ALPN 协商结果', NegotiatedProtocol <> '', '协商协议: ' + NegotiatedProtocol);
    end
    else
      Check('ALPN 握手成功', False, Conn.GetStateString);

  except
    on E: Exception do
      Check('ALPN 功能', False, E.Message);
  end;

  close(Sock);
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('真实网络 HTTPS 测试摘要');
  WriteLn('================================================================');
  WriteLn('总计: ', Total);
  WriteLn('通过: ', Passed);
  WriteLn('失败: ', Failed);
  WriteLn('跳过: ', Skipped);
  WriteLn;

  if Failed = 0 then
    WriteLn('结果: 全部测试通过')
  else
    WriteLn('结果: 存在失败的测试');

  WriteLn('================================================================');
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;
  Skipped := 0;

  WriteLn('================================================================');
  WriteLn('真实网络 HTTPS 连接测试套件');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  测试真实 HTTPS 网站连接，验证 TLS 握手、证书和协议功能。');
  WriteLn('  注意: 此测试需要网络连接。');
  WriteLn;

  try
    TestKnownWebsites;
    TestTLSVersionNegotiation;
    TestCertificateVerification;
    TestSNI;
    TestALPN;

    PrintSummary;

    if Failed > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn('CRITICAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
