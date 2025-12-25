program test_cross_backend_errors_contract;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,

  fafafa.ssl.base,
  {$IFNDEF WINDOWS}
  sockets,
  BaseUnix, Unix,
  fafafa.ssl.openssl.backed,
  {$ENDIF}
  {$IFDEF WINDOWS}fafafa.ssl.winssl.lib,{$ENDIF}
  fafafa.ssl.openssl.api;

{$IFDEF WINDOWS}
const
  CERT_E_EXPIRED = $800B0101;
  CERT_E_CN_NO_MATCH = $800B010F;
  CERT_E_INVALID_NAME = $800B0114;
{$ELSE}
const
  CERT_E_EXPIRED = $800B0101;
  CERT_E_CN_NO_MATCH = $800B010F;
  CERT_E_INVALID_NAME = $800B0114;

type
  TInetSockAddr = record
    sin_family: cushort;
    sin_port: cushort;
    sin_addr: in_addr;
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

function gethostbyname(name: PChar): PHostEnt; cdecl; external 'c';
{$ENDIF}

type
  TErrorKind = (ErrNone, ErrExpired, ErrHostname, ErrOther);

function NormalizeError(aSideIsWin: Boolean; aCode: Integer; const aStr: string): TErrorKind;
begin
  Result := ErrOther;
  if aSideIsWin then
  begin
    // Windows 常见错误码
    if aCode = CERT_E_EXPIRED then Exit(ErrExpired);
    if (aCode = CERT_E_CN_NO_MATCH) or (aCode = CERT_E_INVALID_NAME) then Exit(ErrHostname);
  end
  else
  begin
    // OpenSSL 错误字符串包含关键词（简化归一化）
    if Pos('expired', LowerCase(aStr)) > 0 then Exit(ErrExpired);
    if (Pos('host', LowerCase(aStr)) > 0) or (Pos('name', LowerCase(aStr)) > 0) then Exit(ErrHostname);
  end;
end;

procedure Check(const Name: string; ok: Boolean; const details: string = '');
begin
  if ok then WriteLn('[PASS] ', Name) else begin WriteLn('[FAIL] ', Name); if details <> '' then WriteLn('       ', details); Halt(1); end;
end;

{$IFDEF WINDOWS}
function ConnectTCP(const H: string; Port: Word; out Sock: THandle): Boolean;
var A: TSockAddrIn; WSA: TWSAData; HE: PHostEnt; InA: TInAddr; Tm: Integer;
begin
  Result := False; Sock := INVALID_HANDLE_VALUE; if WSAStartup(MAKEWORD(2,2), WSA) <> 0 then Exit;
  Sock := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); if Sock = INVALID_SOCKET then Exit;
  Tm := 10000; setsockopt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Tm, SizeOf(Tm)); setsockopt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Tm, SizeOf(Tm));
  HE := gethostbyname(PAnsiChar(AnsiString(H))); if HE = nil then begin closesocket(Sock); Sock := INVALID_SOCKET; Exit; end;
  FillChar(A, SizeOf(A), 0); A.sin_family := AF_INET; A.sin_port := htons(Port); Move(HE^.h_addr_list^^, InA, SizeOf(InA)); A.sin_addr := InA;
  Result := connect(Sock, @A, SizeOf(A)) = 0; if not Result then begin closesocket(Sock); Sock := INVALID_SOCKET; end;
end;
{$ELSE}
function ConnectTCP(const H: string; Port: Word; out Sock: THandle): Boolean;
var A: TInetSockAddr; HE: PHostEnt; Sfd: cint;
begin
  Result := False; Sock := THandle(-1);
  Sfd := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sfd < 0 then Exit;
  HE := gethostbyname(PChar(H));
  if HE = nil then begin fpClose(Sfd); Exit; end;
  FillChar(A, SizeOf(A), 0);
  A.sin_family := AF_INET;
  A.sin_port := htons(Port);
  Move(HE^.h_addr_list^^, A.sin_addr, SizeOf(A.sin_addr));
  if fpConnect(Sfd, @A, SizeOf(A)) = 0 then
  begin
    Sock := Sfd; Result := True;
  end
  else
    fpClose(Sfd);
end;
{$ENDIF}

procedure Probe(const Host: string; out Code: Integer; out Str: string; SideIsWin: Boolean);
var Lib: ISSLLibrary; Ctx: ISSLContext; Conn: ISSLConnection; S: THandle; ok: Boolean;
begin
  Code := 0; Str := '';
  Lib := {$IFDEF WINDOWS}CreateWinSSLLibrary{$ELSE}TOpenSSLLibrary.Create{$ENDIF};
  if (Lib = nil) or (not Lib.Initialize) then Exit;
  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx = nil then Exit;
  Ctx.SetServerName(Host);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  if not ConnectTCP(Host, 443, S) then Exit;
  try
    Conn := Ctx.CreateConnection(S);
    if Conn = nil then Exit;
    ok := Conn.Connect;
    if not ok then Exit;
    Code := Conn.GetVerifyResult;
    Str := Conn.GetVerifyResultString;
    Conn.Shutdown;
  finally
    {$IFDEF WINDOWS} if S <> INVALID_HANDLE_VALUE then closesocket(S) {$ELSE} if S <> THandle(-1) then fpClose(S) {$ENDIF};
  end;
end;

var
  c: Integer; s: string; k: TErrorKind;
  failCode: Integer; failStr: string;
  Lib: ISSLLibrary; Ctx: ISSLContext; Conn: ISSLConnection;
  Sfd, SockRefused, SockNX: THandle; ok: Boolean;
begin
  if GetEnvironmentVariable('FAFAFA_RUN_NETWORK_TESTS') <> '1' then
  begin
    WriteLn('SKIPPED (FAFAFA_RUN_NETWORK_TESTS!=1)');
    Halt(0);
  end;

  // 过期证书
  Probe('expired.badssl.com', c, s, {$IFDEF WINDOWS}True{$ELSE}False{$ENDIF});
  Check('expired.badssl.com 非零校验结果', c <> 0, s);
  k := NormalizeError({$IFDEF WINDOWS}True{$ELSE}False{$ENDIF}, c, s);
  Check('expired 归一化为 ErrExpired', k = ErrExpired, s);

  // 主机名不匹配
  Probe('wrong.host.badssl.com', c, s, {$IFDEF WINDOWS}True{$ELSE}False{$ENDIF});
  Check('wrong.host 非零校验结果', c <> 0, s);
  k := NormalizeError({$IFDEF WINDOWS}True{$ELSE}False{$ENDIF}, c, s);
  Check('hostname 归一化为 ErrHostname', k = ErrHostname, s);

  // 错误端口（HTTP 80）— 握手应失败
  Lib := {$IFDEF WINDOWS}CreateWinSSLLibrary{$ELSE}TOpenSSLLibrary.Create{$ENDIF};
  if (Lib <> nil) and Lib.Initialize then
  begin
    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx <> nil then
    begin
      Ctx.SetServerName('www.google.com');
      Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      if ConnectTCP('www.google.com', 80, Sfd) then
      begin
        try
          Conn := Ctx.CreateConnection(Sfd);
          ok := (Conn <> nil) and Conn.Connect;
          Check('HTTP:80 握手应失败', not ok);
        finally
          {$IFDEF WINDOWS} if Sfd <> INVALID_HANDLE_VALUE then closesocket(Sfd) {$ELSE} if Sfd <> THandle(-1) then fpClose(Sfd) {$ENDIF};
        end;
      end;
    end;
  end;

  // 连接拒绝（本地环回：端口1通常拒绝）
  if ConnectTCP('127.0.0.1', 1, SockRefused) then
  begin
    {$IFDEF WINDOWS} closesocket(SockRefused) {$ELSE} fpClose(SockRefused) {$ENDIF};
    Check('127.0.0.1:1 预期连接拒绝', False, '意外连接成功');
  end
  else
    Check('127.0.0.1:1 连接被拒绝（预期）', True);

  // 不存在的域名（NXDOMAIN）
  if ConnectTCP('nonexistent.invalid', 443, SockNX) then
  begin
    {$IFDEF WINDOWS} closesocket(SockNX) {$ELSE} fpClose(SockNX) {$ENDIF};
    Check('nonexistent.invalid 应解析失败', False, '意外连接成功');
  end
  else
    Check('nonexistent.invalid 解析/连接失败（预期）', True);
end.


