program test_winssl_session_resumption;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib;

var
  Total, Passed, Failed: Integer;
  Section: string;

function ResolveSessionHost: string;
begin
  Result := Trim(GetEnvironmentVariable('FAFAFA_WINSSL_SESSION_HOST'));
  if Result = '' then
    Result := 'www.cloudflare.com';
end;

procedure BeginSection(const AName: string);
begin
  Section := AName;
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Check(const AName: string; AOk: Boolean; const ADetails: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', AName, ': ');
  if AOk then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if ADetails <> '' then
      WriteLn('    ', ADetails);
  end;
end;

procedure EmitResumeMarker(const AMarker: string);
begin
  WriteLn('[WINSSL-SESSION-RESUME] ', AMarker);
end;

function BoolText(AValue: Boolean): string;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function EnvEnabled(const AName: string): Boolean;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(GetEnvironmentVariable(AName)));
  Result := (LValue = '1') or (LValue = 'true') or
    (LValue = 'yes') or (LValue = 'on');
end;

function EnvInt(const AName: string; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(Trim(GetEnvironmentVariable(AName)), ADefault);
end;

function InitWinsock: Boolean;
var
  LWSAData: TWSAData;
begin
  Result := WSAStartup(MAKEWORD(2, 2), LWSAData) = 0;
end;

procedure CleanupWinsock;
begin
  WSACleanup;
end;

function ConnectToHost(const AHost: string; APort: Word; out ASocket: TSocket): Boolean;
var
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LInAddr: TInAddr;
  LTimeout: Integer;
begin
  Result := False;
  ASocket := INVALID_SOCKET;

  ASocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if ASocket = INVALID_SOCKET then
    Exit;

  LTimeout := 10000;
  setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO, @LTimeout, SizeOf(LTimeout));
  setsockopt(ASocket, SOL_SOCKET, SO_SNDTIMEO, @LTimeout, SizeOf(LTimeout));

  LHostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
  if LHostEnt = nil then
  begin
    closesocket(ASocket);
    ASocket := INVALID_SOCKET;
    Exit;
  end;

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  Move(LHostEnt^.h_addr_list^^, LInAddr, SizeOf(LInAddr));
  LAddr.sin_addr := LInAddr;

  Result := connect(ASocket, @LAddr, SizeOf(LAddr)) = 0;
  if not Result then
  begin
    closesocket(ASocket);
    ASocket := INVALID_SOCKET;
  end;
end;

procedure ValidateReuseTruth(const ALabel: string; const AConn: ISSLConnection;
  out AReused: Boolean);
var
  LResumption: ISSLSessionResumption;
  LInfo: TSSLConnectionInfo;
  LPerf: TSSLPerformanceMetrics;
begin
  AReused := AConn.IsSessionReused;
  LInfo := AConn.GetConnectionInfo;
  LPerf := AConn.GetPerformanceMetrics;

  Check(ALabel + ' exposes ISSLSessionResumption',
    Supports(AConn, ISSLSessionResumption, LResumption));
  if Supports(AConn, ISSLSessionResumption, LResumption) then
    Check(ALabel + ' optional/core reuse truth aligns',
      LResumption.IsSessionReused = AReused,
      Format('optional=%s core=%s',
        [BoolText(LResumption.IsSessionReused), BoolText(AReused)]));

  Check(ALabel + ' connection info mirrors reuse truth',
    LInfo.IsResumed = AReused,
    Format('info=%s core=%s',
      [BoolText(LInfo.IsResumed), BoolText(AReused)]));

  Check(ALabel + ' performance metrics mirror reuse truth',
    LPerf.SessionReused = AReused,
    Format('perf=%s core=%s',
      [BoolText(LPerf.SessionReused), BoolText(AReused)]));

  EmitResumeMarker(Format(
    'signal label=%s reused=%s info_resumed=%s perf_reused=%s',
    [ALabel, BoolText(AReused), BoolText(LInfo.IsResumed),
     BoolText(LPerf.SessionReused)]));
end;

procedure TestSameContextResumptionTruth(const AHost: string);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LResumption1, LResumptionN: ISSLSessionResumption;
  LSession: ISSLSession;
  LSocket: TSocket;
  LRunNet: Boolean;
  LRequireReuse: Boolean;
  LObservedReuse: Boolean;
  LSessionConfigured: Boolean;
  LAttemptCount: Integer;
  LAttempt: Integer;
  LOk: Boolean;
  LReused: Boolean;
  LInitError: string;
begin
  BeginSection('WinSSL session resumption truth');

  LRunNet := EnvEnabled('FAFAFA_RUN_NETWORK_TESTS');
  if not LRunNet then
  begin
    Check('skip network test (FAFAFA_RUN_NETWORK_TESTS!=1)', True);
    EmitResumeMarker('summary skipped=true reason=network_gate');
    Exit;
  end;

  LAttemptCount := EnvInt('FAFAFA_WINSSL_SESSION_ATTEMPTS', 4);
  if LAttemptCount < 1 then
    LAttemptCount := 1;
  LRequireReuse := EnvEnabled('FAFAFA_WINSSL_REQUIRE_REUSE');
  LObservedReuse := False;
  LSessionConfigured := False;
  LSession := nil;

  if not InitWinsock then
  begin
    Check('initialize Winsock', False);
    EmitResumeMarker('summary skipped=false phase=init_winsock status=fail');
    Exit;
  end;

  try
    LLib := CreateWinSSLLibrary;
    LOk := (LLib <> nil) and LLib.Initialize;
    if LOk then
      LInitError := ''
    else if LLib <> nil then
      LInitError := LLib.GetLastErrorString
    else
      LInitError := 'library instance is nil';
    Check('initialize WinSSL library', LOk, LInitError);
    if not LOk then
    begin
      EmitResumeMarker('summary skipped=false phase=initialize_library status=fail');
      Exit;
    end;

    LCtx := LLib.CreateContext(sslCtxClient);
    Check('create client context', LCtx <> nil);
    if LCtx = nil then
    begin
      EmitResumeMarker('summary skipped=false phase=create_context status=fail');
      Exit;
    end;

    // Prefer TLS 1.2 here because classic session-ID/ticket reconnects are
    // more stable across public servers and CI runners.
    LCtx.SetProtocolVersions([sslProtocolTLS12]);

    LSocket := INVALID_SOCKET;
    if not ConnectToHost(AHost, 443, LSocket) then
    begin
      Check('TCP connect for initial handshake', False, AHost);
      EmitResumeMarker('summary skipped=false phase=initial_tcp_connect status=fail');
      Exit;
    end;

    try
      LConn := LCtx.CreateConnection(LSocket);
      Check('create SSL connection for initial handshake', LConn <> nil);
      if LConn = nil then
      begin
        EmitResumeMarker('summary skipped=false phase=initial_create_connection status=fail');
        Exit;
      end;

      Check('initial connection exposes ISSLSessionResumption',
        Supports(LConn, ISSLSessionResumption, LResumption1));
      if not Supports(LConn, ISSLSessionResumption, LResumption1) then
      begin
        EmitResumeMarker('summary skipped=false phase=initial_owner_surface status=fail');
        Exit;
      end;

      (LConn as ISSLClientConnection).SetServerName(AHost);
      LOk := LConn.Connect;
      Check('initial handshake completes', LOk, AHost);
      if not LOk then
      begin
        EmitResumeMarker('summary skipped=false phase=initial_handshake status=fail');
        Exit;
      end;

      ValidateReuseTruth('initial_handshake', LConn, LReused);
      Check('initial handshake must not report reuse', not LReused,
        'fresh handshake unexpectedly reported session reuse');

      LSession := LResumption1.GetSession;
      LSessionConfigured := LSession <> nil;
      Check('initial handshake captures session metadata', LSessionConfigured);
      if LSessionConfigured then
        Check('captured session metadata is resumable',
          LSession.IsResumable,
          'captured session should remain resumable for the next attempt');

      LConn.Shutdown;
    finally
      if LSocket <> INVALID_SOCKET then
        closesocket(LSocket);
    end;

    for LAttempt := 1 to LAttemptCount do
    begin
      LSocket := INVALID_SOCKET;
      if not ConnectToHost(AHost, 443, LSocket) then
      begin
        Check(Format('TCP connect for resumed attempt #%d', [LAttempt]), False, AHost);
        Continue;
      end;

      try
        LConn := LCtx.CreateConnection(LSocket);
        Check(Format('create SSL connection for resumed attempt #%d', [LAttempt]),
          LConn <> nil);
        if LConn = nil then
          Continue;

        Check(Format('resumed attempt #%d exposes ISSLSessionResumption', [LAttempt]),
          Supports(LConn, ISSLSessionResumption, LResumptionN));
        if not Supports(LConn, ISSLSessionResumption, LResumptionN) then
          Continue;

        if LSessionConfigured then
          LResumptionN.SetSession(LSession);

        Check(Format('pre-handshake attempt #%d does not preclaim reuse', [LAttempt]),
          not LResumptionN.IsSessionReused,
          'reuse state must remain false until the handshake actually completes');

        (LConn as ISSLClientConnection).SetServerName(AHost);
        LOk := LConn.Connect;
        Check(Format('same-context resumed attempt #%d completes', [LAttempt]), LOk, AHost);
        if not LOk then
          Continue;

        ValidateReuseTruth(Format('same_context_attempt_%d', [LAttempt]), LConn, LReused);
        EmitResumeMarker(Format('attempt index=%d reused=%s session_configured=%s',
          [LAttempt, BoolText(LReused), BoolText(LSessionConfigured)]));
        if LReused then
          LObservedReuse := True;

        LConn.Shutdown;
      finally
        if LSocket <> INVALID_SOCKET then
          closesocket(LSocket);
      end;

      if LObservedReuse then
        Break;
    end;

    EmitResumeMarker(Format(
      'summary host=%s attempts=%d observed_reuse=%s require_reuse=%s session_configured=%s',
      [AHost, LAttemptCount, BoolText(LObservedReuse), BoolText(LRequireReuse),
       BoolText(LSessionConfigured)]));

    if LRequireReuse then
      Check('same-context reconnect eventually observes session reuse',
        LObservedReuse,
        Format('host=%s attempts=%d', [AHost, LAttemptCount]))
    else
      Check('same-context reconnect evidence recorded', True,
        Format('observed_reuse=%s attempts=%d',
          [BoolText(LObservedReuse), LAttemptCount]));
  finally
    CleanupWinsock;
  end;
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('WinSSL session resumption runtime truth test');
  TestSameContextResumptionTruth(ResolveSessionHost);

  WriteLn;
  WriteLn('总计: ', Total, ' 通过: ', Passed, ' 失败: ', Failed);
  if Failed > 0 then
    Halt(1);
end.
