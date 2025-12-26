program test_e2e_scenarios;

{******************************************************************************}
{  End-to-End (E2E) Test Scenarios                                             }
{  Migrated to use TSimpleTestRunner framework (P1-2.2)                        }
{******************************************************************************}

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  {$IFDEF UNIX}
  ctypes,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

const
  TEST_HOST = 'badssl.com';
  TEST_PORT = 443;

var
  Runner: TSimpleTestRunner;
  GLib: ISSLLibrary;

// Socket connection helpers
{$IFDEF UNIX}
const
  AF_INET = 2;
  SOCK_STREAM = 1;
  IPPROTO_TCP = 6;
  INVALID_SOCKET = -1;

type
  TSocket = cint;
  tsockaddr_in = record
    sin_family: cushort;
    sin_port: cushort;
    sin_addr: record s_addr: cuint; end;
    sin_zero: array[0..7] of char;
  end;
  PHostEnt = ^THostEnt;
  THostEnt = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: cint;
    h_length: cint;
    h_addr_list: PPChar;
  end;

function socket(domain, atype, protocol: cint): cint; cdecl; external 'c';
function connect(sockfd: cint; addr: Pointer; addrlen: cuint): cint; cdecl; external 'c';
function close(fd: cint): cint; cdecl; external 'c';
function htons(hostshort: cushort): cushort; cdecl; external 'c';
function gethostbyname(name: PChar): PHostEnt; cdecl; external 'c';

function ConnectSocket(const Host: string; Port: Word): TSocket;
var
  S: TSocket;
  Addr: tsockaddr_in;
  HE: PHostEnt;
begin
  Result := INVALID_SOCKET;
  S := socket(AF_INET, SOCK_STREAM, 0);
  if S < 0 then Exit;

  HE := gethostbyname(PChar(Host));
  if HE = nil then
  begin
    close(S);
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Move(HE^.h_addr_list^^, Addr.sin_addr, SizeOf(Addr.sin_addr));

  if connect(S, @Addr, SizeOf(Addr)) < 0 then
  begin
    close(S);
    Exit;
  end;
  Result := S;
end;

procedure CloseSocket(S: TSocket);
begin
  if S <> INVALID_SOCKET then close(S);
end;
{$ELSE}
// Windows stub
type TSocket = THandle;
const INVALID_SOCKET = THandle(-1);
function ConnectSocket(const Host: string; Port: Word): TSocket; begin Result := INVALID_SOCKET; end;
procedure CloseSocket(S: TSocket); begin end;
{$ENDIF}

procedure TestSessionResumption;
var
  Ctx: ISSLContext;
  Conn1, Conn2: ISSLConnection;
  Sock1, Sock2: TSocket;
  Sess: ISSLSession;
begin
  WriteLn;
  WriteLn('=== Session Resumption Tests ===');

  Ctx := GLib.CreateContext(sslCtxClient);
  Ctx.SetSessionCacheMode(True);
  Ctx.SetServerName('www.cloudflare.com');

  // Load system CA bundle
  if FileExists('/etc/ssl/certs/ca-certificates.crt') then
    Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt')
  else if FileExists('/etc/pki/tls/certs/ca-bundle.crt') then
    Ctx.LoadCAFile('/etc/pki/tls/certs/ca-bundle.crt');

  // First connection
  Sock1 := ConnectSocket('www.cloudflare.com', 443);
  if Sock1 = INVALID_SOCKET then
  begin
    Runner.Check('Session Resumption - Connect 1', False, 'Socket failed');
    Exit;
  end;

  try
    Conn1 := Ctx.CreateConnection(Sock1);
    if Conn1.Connect then
    begin
      Runner.Check('Session Resumption - Handshake 1', True);
      // Extract session for reuse
      Sess := Conn1.GetSession;
      Runner.Check('Session Resumption - Extract Session', Sess <> nil);
    end
    else
    begin
      Runner.Check('Session Resumption - Handshake 1', False, GLib.GetLastErrorString);
      Exit;
    end;
  finally
    CloseSocket(Sock1);
  end;

  // Second connection (reuse session)
  if Sess = nil then Exit;

  Sock2 := ConnectSocket('www.cloudflare.com', 443);
  if Sock2 = INVALID_SOCKET then Exit;

  try
    Conn2 := Ctx.CreateConnection(Sock2);
    Conn2.SetSession(Sess);  // Set session before Connect
    if Conn2.Connect then
    begin
      Runner.Check('Session Resumption - Handshake 2', True);
      Runner.Check('Session Resumption - Reused', Conn2.IsSessionReused,
        Format('Was reused: %s', [BoolToStr(Conn2.IsSessionReused, True)]));
    end
    else
      Runner.Check('Session Resumption - Handshake 2', False, GLib.GetLastErrorString);
  finally
    CloseSocket(Sock2);
  end;
end;

procedure TestLargeDataTransfer;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Sock: TSocket;
  Req: string;
  Resp: TBytes;
  TotalRead: Integer;
  BytesRead: Integer;
begin
  WriteLn;
  WriteLn('=== Large Data Transfer Tests ===');

  Ctx := GLib.CreateContext(sslCtxClient);
  Ctx.SetServerName('www.cloudflare.com');

  // Load system CA bundle
  if FileExists('/etc/ssl/certs/ca-certificates.crt') then
    Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt')
  else if FileExists('/etc/pki/tls/certs/ca-bundle.crt') then
    Ctx.LoadCAFile('/etc/pki/tls/certs/ca-bundle.crt');

  Sock := ConnectSocket('www.cloudflare.com', 443);
  if Sock = INVALID_SOCKET then
  begin
    Runner.Check('Large Data - Connect', False);
    Exit;
  end;

  try
    Conn := Ctx.CreateConnection(Sock);
    if Conn.Connect then
    begin
      Runner.Check('Large Data - Handshake', True);

      Req := 'GET / HTTP/1.1'#13#10'Host: www.cloudflare.com'#13#10'Connection: close'#13#10#13#10;
      Conn.WriteString(Req);

      TotalRead := 0;
      SetLength(Resp, 4096);
      repeat
        BytesRead := Conn.Read(Resp[0], Length(Resp));
        if BytesRead > 0 then
          Inc(TotalRead, BytesRead);
      until BytesRead <= 0;

      Runner.Check('Large Data - Transfer', TotalRead > 10000, Format('Read %d bytes', [TotalRead]));
    end
    else
      Runner.Check('Large Data - Handshake', False, GLib.GetLastErrorString);
  finally
    CloseSocket(Sock);
  end;
end;

procedure TestClientCertAuth;
begin
  WriteLn;
  WriteLn('=== Client Certificate Auth Tests ===');
  // Would need valid client certificates to test
  Runner.Check('Client Cert Auth - Skipped (No certs)', True);
end;

begin
  WriteLn('End-to-End Scenarios');
  WriteLn('====================');
  WriteLn;

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    try
      GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
      if not GLib.Initialize then
      begin
        WriteLn('ERROR: Failed to initialize SSL library');
        Halt(1);
      end;

      WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

      TestSessionResumption;
      TestLargeDataTransfer;
      TestClientCertAuth;

    except
      on E: Exception do
        WriteLn('FATAL: ', E.Message);
    end;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
