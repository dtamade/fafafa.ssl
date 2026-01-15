{******************************************************************************}
{  Real Network HTTPS Connection Tests                                         }
{  Migrated to use TSimpleTestRunner framework (P1-2.2)                        }
{******************************************************************************}

program test_real_https_connection;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  ctypes, BaseUnix,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.sockets,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

{ Create TCP socket }
function CreateTCPSocket: TSocket;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
end;

{ Resolve hostname }
function ResolveHostname(const AHostname: string): Cardinal;
var
  HostEnt: PHostEnt;
  HostnameA: AnsiString;
begin
  Result := INADDR_NONE;
  HostnameA := AnsiString(AHostname);

  Result := inet_addr(PAnsiChar(HostnameA));
  if Result <> INADDR_NONE then
    Exit;

  HostEnt := gethostbyname(PAnsiChar(HostnameA));
  if (HostEnt <> nil) and (HostEnt^.h_addr_list <> nil) and
     (HostEnt^.h_addr_list^ <> nil) then
    Result := PCardinal(HostEnt^.h_addr_list^)^;
end;

{ Connect to server }
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

{ Set socket timeout }
procedure SetSocketTimeout(ASocket: TSocket; ATimeoutMs: Integer);
var
  Timeout: TTimeVal;
begin
  Timeout.tv_sec := ATimeoutMs div 1000;
  Timeout.tv_usec := (ATimeoutMs mod 1000) * 1000;
  setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  setsockopt(ASocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
end;

{ Test single HTTPS site connection }
function TestHTTPSConnection(const AHostname: string; APort: Word = 443): Boolean;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  Request: string;
  Buffer: array[0..4095] of Byte;
  BytesRead, BytesWritten: Integer;
  ResponseStr: AnsiString;
  RequestBytes: TBytes;
begin
  Result := False;
  Sock := INVALID_SOCKET;

  try
    Sock := CreateTCPSocket;
    if Sock = INVALID_SOCKET then
    begin
      WriteLn('    Cannot create socket');
      Exit;
    end;

    SetSocketTimeout(Sock, 10000);

    if not ConnectToHost(Sock, AHostname, APort) then
    begin
      WriteLn('    TCP connect failed: ', AHostname);
      Exit;
    end;

    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      WriteLn('    OpenSSL init failed');
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      WriteLn('    Create SSL context failed');
      Exit;
    end;

    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName(AHostname);
    if FileExists('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
      Ctx.SetVerifyMode([sslVerifyPeer]);
    end
    else
      Ctx.SetVerifyMode([]);
    Ctx.SetALPNProtocols('http/1.1');

    Conn := Ctx.CreateConnection(Sock);
    if Conn = nil then
    begin
      WriteLn('    Create SSL connection failed');
      Exit;
    end;

    if not Conn.Connect then
    begin
      WriteLn('    TLS handshake failed: ', Conn.GetStateString);
      Exit;
    end;

    Request := 'GET / HTTP/1.1' + #13#10 +
               'Host: ' + AHostname + #13#10 +
               'User-Agent: fafafa.ssl-test/1.0' + #13#10 +
               'Connection: close' + #13#10 +
               #13#10;

    RequestBytes := TEncoding.UTF8.GetBytes(Request);
    BytesWritten := Conn.Write(RequestBytes[0], Length(RequestBytes));
    if BytesWritten <= 0 then
    begin
      WriteLn('    Send request failed');
      Exit;
    end;

    BytesRead := Conn.Read(Buffer, SizeOf(Buffer));
    if BytesRead <= 0 then
    begin
      WriteLn('    Read response failed');
      Exit;
    end;

    SetString(ResponseStr, PAnsiChar(@Buffer[0]), BytesRead);

    Result := (Pos('HTTP/', string(ResponseStr)) = 1);

    if Result then
    begin
      if Pos('200', string(ResponseStr)) > 0 then
        WriteLn('    Status: 200 OK')
      else if Pos('301', string(ResponseStr)) > 0 then
        WriteLn('    Status: 301 Redirect')
      else if Pos('302', string(ResponseStr)) > 0 then
        WriteLn('    Status: 302 Redirect')
      else if Pos('403', string(ResponseStr)) > 0 then
        WriteLn('    Status: 403 Forbidden (TLS success)')
      else
        WriteLn('    Status: Other HTTP response');

      WriteLn('    Protocol: ', ProtocolVersionToString(Conn.GetProtocolVersion));
      WriteLn('    Cipher: ', Conn.GetCipherName);
    end;

  except
    on E: Exception do
    begin
      WriteLn('    Exception: ', E.Message);
      Result := False;
    end;
  end;

  if Sock <> INVALID_SOCKET then
    close(Sock);
end;

{ Test TLS version negotiation }
procedure TestTLSVersionNegotiation;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  Protocol: string;
  ProtoVer: TSSLProtocolVersion;
begin
  WriteLn;
  WriteLn('=== TLS Version Negotiation ===');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('[SKIP] TLS 1.3 negotiation - Cannot create socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.google.com', 443) then
  begin
    close(Sock);
    WriteLn('[SKIP] TLS 1.3 negotiation - TCP connect failed');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      WriteLn('[SKIP] TLS 1.3 negotiation - OpenSSL init failed');
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS13]);
    Ctx.SetServerName('www.google.com');
    Ctx.SetVerifyMode([]);

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      ProtoVer := Conn.GetProtocolVersion;
      Protocol := ProtocolVersionToString(ProtoVer);
      Runner.Check('TLS 1.3 negotiation', ProtoVer = sslProtocolTLS13, 'Actual: ' + Protocol);
    end
    else
      Runner.Check('TLS 1.3 negotiation', False, 'TLS handshake failed');

  except
    on E: Exception do
      Runner.Check('TLS 1.3 negotiation', False, E.Message);
  end;

  close(Sock);
end;

{ Test certificate verification }
procedure TestCertificateVerification;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  CertInfo: TSSLCertificateInfo;
  PeerCert: ISSLCertificate;
begin
  WriteLn;
  WriteLn('=== Certificate Verification ===');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('[SKIP] Get server certificate - Cannot create socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.github.com', 443) then
  begin
    close(Sock);
    WriteLn('[SKIP] Get server certificate - TCP connect failed');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.github.com');
    Ctx.SetVerifyMode([]);

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      Runner.Check('TLS handshake success', True);

      PeerCert := Conn.GetPeerCertificate;
      if PeerCert <> nil then
      begin
        CertInfo := PeerCert.GetInfo;
        Runner.Check('Get certificate subject', CertInfo.Subject <> '', 'Subject: ' + CertInfo.Subject);
        Runner.Check('Get certificate issuer', CertInfo.Issuer <> '', 'Issuer: ' + CertInfo.Issuer);
        Runner.Check('Certificate validity', CertInfo.NotAfter > Now, 'Valid until: ' + DateTimeToStr(CertInfo.NotAfter));
      end
      else
        Runner.Check('Get server certificate', False, 'Cannot get peer certificate');
    end
    else
      Runner.Check('TLS handshake success', False, Conn.GetStateString);

  except
    on E: Exception do
      Runner.Check('Get server certificate', False, E.Message);
  end;

  close(Sock);
end;

{ Test multiple known websites }
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
  WriteLn;
  WriteLn('=== Known Website Connection Tests ===');

  SuccessCount := 0;
  for I := Low(Websites) to High(Websites) do
  begin
    Write('  Testing ', Websites[I], '... ');
    if TestHTTPSConnection(Websites[I]) then
    begin
      Inc(SuccessCount);
      Runner.Check(Websites[I], True);
    end
    else
      Runner.Check(Websites[I], False, 'Connection failed');
  end;

  WriteLn;
  WriteLn('  Successful: ', SuccessCount, '/', Length(Websites));
end;

{ Test SNI (Server Name Indication) }
procedure TestSNI;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
begin
  WriteLn;
  WriteLn('=== SNI Function Tests ===');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('[SKIP] SNI setup - Cannot create socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.cloudflare.com', 443) then
  begin
    close(Sock);
    WriteLn('[SKIP] SNI setup - TCP connect failed');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.cloudflare.com');
    Ctx.SetVerifyMode([]);

    Runner.Check('Set SNI', Ctx.GetServerName = 'www.cloudflare.com');

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
      Runner.Check('SNI handshake success', True)
    else
      Runner.Check('SNI handshake success', False, Conn.GetStateString);

  except
    on E: Exception do
      Runner.Check('SNI function', False, E.Message);
  end;

  close(Sock);
end;

{ Test ALPN }
procedure TestALPN;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  NegotiatedProtocol: string;
begin
  WriteLn;
  WriteLn('=== ALPN Protocol Negotiation ===');

  Sock := CreateTCPSocket;
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('[SKIP] ALPN negotiation - Cannot create socket');
    Exit;
  end;

  SetSocketTimeout(Sock, 10000);

  if not ConnectToHost(Sock, 'www.google.com', 443) then
  begin
    close(Sock);
    WriteLn('[SKIP] ALPN negotiation - TCP connect failed');
    Exit;
  end;

  try
    Lib := TOpenSSLLibrary.Create;
    Lib.Initialize;

    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.google.com');
    Ctx.SetALPNProtocols('h2,http/1.1');
    Ctx.SetVerifyMode([]);

    Runner.Check('Set ALPN protocols', Ctx.GetALPNProtocols = 'h2,http/1.1');

    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      Runner.Check('ALPN handshake success', True);
      NegotiatedProtocol := Conn.GetSelectedALPNProtocol;
      Runner.Check('ALPN negotiation result', NegotiatedProtocol <> '', 'Protocol: ' + NegotiatedProtocol);
    end
    else
      Runner.Check('ALPN handshake success', False, Conn.GetStateString);

  except
    on E: Exception do
      Runner.Check('ALPN function', False, E.Message);
  end;

  close(Sock);
end;

begin
  WriteLn('Real Network HTTPS Connection Test Suite');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Note: This test requires network connectivity.');
  WriteLn;

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestKnownWebsites;
    TestTLSVersionNegotiation;
    TestCertificateVerification;
    TestSNI;
    TestALPN;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
