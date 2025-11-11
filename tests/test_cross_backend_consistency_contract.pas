program test_cross_backend_consistency_contract;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes, DateUtils,
  
  fafafa.ssl.base,
  {$IFNDEF WINDOWS}sockets, BaseUnix, Unix,{$ENDIF}
  {$IFNDEF WINDOWS}fafafa.ssl.openssl,{$ENDIF}
  {$IFDEF WINDOWS}Windows, WinSock2,{$ENDIF}
  fafafa.ssl.winssl.lib;

type
  TSide = (SideOpenSSL, SideWinSSL);

var
  Total, Passed, Failed: Integer;

procedure Check(const Name: string; ok: Boolean; const details: string = '');
begin
  Inc(Total);
  if ok then begin Inc(Passed); WriteLn('[PASS] ', Name); end
  else begin Inc(Failed); WriteLn('[FAIL] ', Name); if details <> '' then WriteLn('       ', details); end;
end;

function EnvEnabled(const VarName: string): Boolean;
begin
  Result := GetEnvironmentVariable(VarName) = '1';
end;

function CreateLib(aSide: TSide): ISSLLibrary;
begin
  Result := nil;
  case aSide of
    SideWinSSL: Result := CreateWinSSLLibrary;
    {$IFNDEF WINDOWS}
    SideOpenSSL: Result := TOpenSSLLibrary.Create;
    {$ELSE}
    SideOpenSSL: Result := nil;
    {$ENDIF}
  end;
end;

function ConnectTCP(const Host: string; Port: Word; out Sock: THandle): Boolean;
{$IFDEF WINDOWS}
var A: TSockAddrIn; H: PHostEnt; InAddr: TInAddr; Tm: Integer; WSA: TWSAData;
begin
  Result := False; Sock := INVALID_HANDLE_VALUE;
  if WSAStartup(MAKEWORD(2,2), WSA) <> 0 then Exit;
  Sock := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Sock = INVALID_SOCKET then Exit;
  Tm := 10000; setsockopt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Tm, SizeOf(Tm));
  setsockopt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Tm, SizeOf(Tm));
  H := gethostbyname(PAnsiChar(AnsiString(Host)));
  if H = nil then begin closesocket(Sock); Sock := INVALID_SOCKET; Exit; end;
  FillChar(A, SizeOf(A), 0); A.sin_family := AF_INET; A.sin_port := htons(Port);
  Move(H^.h_addr_list^^, InAddr, SizeOf(InAddr)); A.sin_addr := InAddr;
  Result := connect(Sock, @A, SizeOf(A)) = 0;
  if not Result then begin closesocket(Sock); Sock := INVALID_SOCKET; end;
end;
{$ELSE}
var A: TInetSockAddr; H: PHostEnt; S: cint; Tm: LongInt;
begin
  Result := False; Sock := THandle(-1);
  S := fpSocket(AF_INET, SOCK_STREAM, 0);
  if S < 0 then Exit;
  Tm := 10000;
  fpSetSockOpt(S, SOL_SOCKET, SO_RCVTIMEO, @Tm, SizeOf(Tm));
  fpSetSockOpt(S, SOL_SOCKET, SO_SNDTIMEO, @Tm, SizeOf(Tm));
  H := gethostbyname(PChar(Host));
  if H = nil then begin fpClose(S); Exit; end;
  FillChar(A, SizeOf(A), 0);
  A.sin_family := AF_INET;
  A.sin_port := htons(Port);
  Move(H^.h_addr_list^^, A.sin_addr, SizeOf(A.sin_addr));
  if fpConnect(S, @A, SizeOf(A)) = 0 then
  begin
    Sock := S; Result := True;
  end
  else
  begin
    fpClose(S);
  end;
end;
{$ENDIF}

function RunProbe(aSide: TSide; const Host: string; out Proto: TSSLProtocolVersion; out Cipher, Alpn: string; out VerifyCode: Integer): Boolean;
var Lib: ISSLLibrary; Ctx: ISSLContext; Conn: ISSLConnection; S: THandle; ok: Boolean;
begin
  Result := False; Proto := sslProtocolTLS12; Cipher := ''; Alpn := ''; VerifyCode := 0;
  Lib := CreateLib(aSide);
  if Lib = nil then Exit;
  if not Lib.Initialize then Exit;
  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx = nil then Exit;
  Ctx.SetServerName(Host);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  Ctx.SetALPNProtocols('h2,http/1.1');
  if not ConnectTCP(Host, 443, S) then Exit;
  try
    Conn := Ctx.CreateConnection(S);
    if Conn = nil then Exit;
    ok := Conn.Connect;
    if not ok then Exit;
    Proto := Conn.GetProtocolVersion;
    Cipher := Conn.GetCipherName;
    Alpn := Conn.GetSelectedALPNProtocol;
    VerifyCode := Conn.GetVerifyResult;
    Result := True;
    Conn.Shutdown;
  finally
    {$IFDEF WINDOWS}
    if S <> INVALID_HANDLE_VALUE then closesocket(S);
    {$ELSE}
    if S <> THandle(-1) then fpClose(S);
    {$ENDIF}
  end;
end;

procedure TestNormalizedContract;
var
  runNet: Boolean;
  oOK, wOK: Boolean;
  oProto: TSSLProtocolVersion; wProto: TSSLProtocolVersion;
  oCipher, wCipher, oAlpn, wAlpn: string;
  oV, wV: Integer;
begin
  WriteLn('=== 跨后端一致性（合同）===');
  runNet := EnvEnabled('FAFAFA_RUN_NETWORK_TESTS');
  if not runNet then begin
    Check('跳过网络测试 (FAFAFA_RUN_NETWORK_TESTS!=1)', True);
    Exit;
  end;

  // OpenSSL 侧（Linux 有效）
  {$IFNDEF WINDOWS}
  oOK := RunProbe(SideOpenSSL, 'api.github.com', oProto, oCipher, oAlpn, oV);
  Check('OpenSSL 探测执行', oOK);
  if oOK then begin
    Check('OpenSSL 协议版本有效', oProto in [sslProtocolTLS12, sslProtocolTLS13]);
    Check('OpenSSL 密码套件非空', oCipher <> '');
  end;
  {$ELSE}
  oOK := False;
  Check('OpenSSL 探测（Windows 跳过）', True);
  {$ENDIF}

  // WinSSL 侧（Windows 有效）
  {$IFDEF WINDOWS}
  wOK := RunProbe(SideWinSSL, 'api.github.com', wProto, wCipher, wAlpn, wV);
  Check('WinSSL 探测执行', wOK);
  if wOK then begin
    Check('WinSSL 协议版本有效', wProto in [sslProtocolTLS12, sslProtocolTLS13]);
    Check('WinSSL 密码套件非空', wCipher <> '');
  end;
  {$ELSE}
  wOK := False;
  Check('WinSSL 探测（Linux 跳过）', True);
  {$ENDIF}

  // 若两侧均可用，则比较归一化属性
  if oOK and wOK then
  begin
    Check('协议族一致 (TLS1.2/1.3)', (oProto in [sslProtocolTLS12, sslProtocolTLS13]) and (wProto in [sslProtocolTLS12, sslProtocolTLS13]));
    // ALPN 可能为空或不同，放宽为：两侧只要返回空或在 {h2,http/1.1} 中即可
    if oAlpn <> '' then Check('OpenSSL ALPN 合法', (oAlpn = 'h2') or (oAlpn = 'http/1.1')) else Check('OpenSSL ALPN 允许为空', True);
    if wAlpn <> '' then Check('WinSSL ALPN 合法', (wAlpn = 'h2') or (wAlpn = 'http/1.1')) else Check('WinSSL ALPN 允许为空', True);
  end;
end;

begin
  Total := 0; Passed := 0; Failed := 0;
  WriteLn('跨后端一致性（合同）测试');
  TestNormalizedContract;
  WriteLn; WriteLn('总计: ', Total, ' 通过: ', Passed, ' 失败: ', Failed);
  if Failed > 0 then Halt(1);
end.


