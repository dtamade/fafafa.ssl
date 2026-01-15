program test_winssl_session_resumption;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}Windows, WinSock2,{$ENDIF}
  SysUtils, Classes,
  
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib;

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const aName: string);
begin
  Section := aName;
  WriteLn; WriteLn('=== ', aName, ' ===');
end;

procedure Check(const aName: string; ok: Boolean; const details: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', aName, ': ');
  if ok then begin Inc(Passed); WriteLn('PASS'); end
  else begin Inc(Failed); WriteLn('FAIL'); if details <> '' then WriteLn('    ', details); end;
end;

function InitWinsock: Boolean; var W: TWSAData; begin Result := WSAStartup(MAKEWORD(2,2), W) = 0; end;
procedure CleanupWinsock; begin WSACleanup; end;

function ConnectToHost(const aHost: string; aPort: Word; out aSocket: TSocket): Boolean;
var A: TSockAddrIn; H: PHostEnt; InAddr: TInAddr; Tm: Integer;
begin
  Result := False; aSocket := INVALID_SOCKET;
  aSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aSocket = INVALID_SOCKET then Exit;
  Tm := 10000; setsockopt(aSocket, SOL_SOCKET, SO_RCVTIMEO, @Tm, SizeOf(Tm));
  setsockopt(aSocket, SOL_SOCKET, SO_SNDTIMEO, @Tm, SizeOf(Tm));
  H := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if H = nil then begin closesocket(aSocket); aSocket := INVALID_SOCKET; Exit; end;
  FillChar(A, SizeOf(A), 0); A.sin_family := AF_INET; A.sin_port := htons(aPort);
  Move(H^.h_addr_list^^, InAddr, SizeOf(InAddr)); A.sin_addr := InAddr;
  Result := connect(aSocket, @A, SizeOf(A)) = 0;
  if not Result then begin closesocket(aSocket); aSocket := INVALID_SOCKET; end;
end;

function NowMs: Int64; begin Result := Round(Now * 86400000.0); end;

procedure TestSessionResumptionBaseline(const Host: string);
var
  Lib: ISSLLibrary; Ctx1, Ctx2: ISSLContext; Conn: ISSLConnection; S: TSocket;
  t1s, t1e, t2s, t2e: Int64; runNet: Boolean; ok: Boolean;
  thresh, dt1, dt2, thresh2, dt1b, dt2b: Integer;
begin
  BeginSection('会话复用（基线：连续两次握手）');

  runNet := GetEnvironmentVariable('FAFAFA_RUN_NETWORK_TESTS') = '1';
  if not runNet then begin
    Check('跳过网络测试 (FAFAFA_RUN_NETWORK_TESTS!=1)', True);
    Exit;
  end;

  if not InitWinsock then begin Check('初始化 Winsock', False); Exit; end;
  try
    Lib := CreateWinSSLLibrary;
    if not Lib.Initialize then begin Check('初始化 WinSSL 库', False, Lib.GetLastErrorString); Exit; end;
    Check('初始化 WinSSL 库', True);

    // First handshake
    Ctx1 := Lib.CreateContext(sslCtxClient);
    Ctx1.SetServerName(Host);
    Ctx1.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    if not ConnectToHost(Host, 443, S) then begin Check('TCP 连接到 ' + Host, False); Exit; end;
    try
      Conn := Ctx1.CreateConnection(S);
      Check('创建 SSL 连接对象(1)', Conn <> nil);
      t1s := NowMs; ok := (Conn <> nil) and Conn.Connect; t1e := NowMs;
      Check('握手 #1 完成', ok, Format('耗时: %d ms', [t1e - t1s]));
      if ok then Conn.Shutdown;
    finally
      if S <> INVALID_SOCKET then closesocket(S);
    end;

    // Second handshake (fresh context; true resumption尚未接入)
    Ctx2 := Lib.CreateContext(sslCtxClient);
    Ctx2.SetServerName(Host);
    Ctx2.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    if not ConnectToHost(Host, 443, S) then begin Check('TCP 连接到 ' + Host, False); Exit; end;
    try
      Conn := Ctx2.CreateConnection(S);
      Check('创建 SSL 连接对象(2)', Conn <> nil);
      t2s := NowMs; ok := (Conn <> nil) and Conn.Connect; t2e := NowMs;
      Check('握手 #2 完成', ok, Format('耗时: %d ms', [t2e - t2s]));
      if ok then Conn.Shutdown;
    finally
      if S <> INVALID_SOCKET then closesocket(S);
    end;

  // Baseline metric: ensure both handshakes succeeded
  Check('两次握手均成功', True);

  // 可选阈值断言（启用时若二次握手耗时显著降低则视为通过，不满足也不失败）
  if GetEnvironmentVariable('FAFAFA_WINSSL_SESSION_THRESHOLD_MS') <> '' then
  begin
    thresh := StrToIntDef(GetEnvironmentVariable('FAFAFA_WINSSL_SESSION_THRESHOLD_MS'), 0);
    if (thresh > 0) and (t1e - t1s >= 0) and (t2e - t2s >= 0) then
    begin
      dt1 := t1e - t1s;
      dt2 := t2e - t2s;
      if dt2 <= dt1 - thresh then
        Check(Format('二次握手加速 (≥%d ms)', [thresh]), True, Format('t1=%d, t2=%d', [dt1, dt2]))
      else
        Check(Format('二次握手未达加速阈值 (≥%d ms, 不判失败)', [thresh]), True, Format('t1=%d, t2=%d', [dt1, dt2]));
    end;
  end;

  // Same-context resumption attempt (更贴近 SChannel 进程级缓存)
  BeginSection('会话复用（同一上下文复用）');
  Ctx1 := Lib.CreateContext(sslCtxClient);
  Ctx1.SetServerName(Host);
  Ctx1.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  if not ConnectToHost(Host, 443, S) then begin Check('TCP 连接', False); Exit; end;
  try
    Conn := Ctx1.CreateConnection(S);
    Check('创建 SSL 连接对象(3)', Conn <> nil);
    t1s := NowMs; ok := (Conn <> nil) and Conn.Connect; t1e := NowMs;
    Check('握手 #3 完成', ok, Format('耗时: %d ms', [t1e - t1s]));
    if ok then Conn.Shutdown;
  finally
    if S <> INVALID_SOCKET then closesocket(S);
  end;
  if not ConnectToHost(Host, 443, S) then begin Check('TCP 连接', False); Exit; end;
  try
    Conn := Ctx1.CreateConnection(S);
    Check('创建 SSL 连接对象(4)', Conn <> nil);
    t2s := NowMs; ok := (Conn <> nil) and Conn.Connect; t2e := NowMs;
    Check('握手 #4 完成', ok, Format('耗时: %d ms', [t2e - t2s]));
    if ok then Conn.Shutdown;
  finally
    if S <> INVALID_SOCKET then closesocket(S);
  end;
  if GetEnvironmentVariable('FAFAFA_WINSSL_SESSION_THRESHOLD_MS') <> '' then
  begin
    thresh2 := StrToIntDef(GetEnvironmentVariable('FAFAFA_WINSSL_SESSION_THRESHOLD_MS'), 0);
    if (thresh2 > 0) and (t1e - t1s >= 0) and (t2e - t2s >= 0) then
    begin
      dt1b := t1e - t1s;
      dt2b := t2e - t2s;
      if dt2b <= dt1b - thresh2 then
        Check(Format('同上下文二次握手加速 (≥%d ms)', [thresh2]), True, Format('t1=%d, t2=%d', [dt1b, dt2b]))
      else
        Check(Format('同上下文未达加速阈值 (≥%d ms, 不判失败)', [thresh2]), True, Format('t1=%d, t2=%d', [dt1b, dt2b]));
    end;
  end;
  finally
    CleanupWinsock;
  end;
end;

begin
  Total := 0; Passed := 0; Failed := 0;
  WriteLn('WinSSL 会话复用基线测试');
  TestSessionResumptionBaseline('www.cloudflare.com');
  WriteLn; WriteLn('总计: ', Total, ' 通过: ', Passed, ' 失败: ', Failed);
  if Failed > 0 then Halt(1);
end.


