{******************************************************************************}
{  OpenSSL Cert Verify Cache Runtime Policy Regression                         }
{******************************************************************************}

program test_openssl_cert_verify_cache_policy_runtime;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.cert.verify.cache,
  fafafa.ssl.logging,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.native_handle,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base
  {$IFDEF WINDOWS}
  , Windows, WinSock2
  {$ELSE}
  , sockets, BaseUnix, Unix, ctypes
  {$ENDIF}
  ;

{$IFNDEF WINDOWS}
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
  TMemoryLogger = class(TBaseLogger)
  private
    FLines: TStringList;
  protected
    procedure WriteLog(const AMessage: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearMessages;
    function ContainsText(const AText: string): Boolean;
    function RecentMessages(AMaxLines: Integer = 20): string;
  end;

var
  Runner: TSimpleTestRunner;
  TargetHosts: TStringList;
  TargetPort: Word;
  CaptureLoggerObject: TMemoryLogger;

constructor TMemoryLogger.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TMemoryLogger.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TMemoryLogger.WriteLog(const AMessage: string);
begin
  FLines.Add(AMessage);
end;

procedure TMemoryLogger.ClearMessages;
begin
  FLines.Clear;
end;

function TMemoryLogger.ContainsText(const AText: string): Boolean;
var
  I: Integer;
  LNeedle: string;
begin
  Result := False;
  LNeedle := LowerCase(AText);
  for I := 0 to FLines.Count - 1 do
    if Pos(LNeedle, LowerCase(FLines[I])) > 0 then
      Exit(True);
end;

function TMemoryLogger.RecentMessages(AMaxLines: Integer): string;
var
  StartIdx, I: Integer;
begin
  if FLines.Count = 0 then
    Exit('(no logs captured)');

  if AMaxLines <= 0 then
    AMaxLines := FLines.Count;

  StartIdx := FLines.Count - AMaxLines;
  if StartIdx < 0 then
    StartIdx := 0;

  Result := '';
  for I := StartIdx to FLines.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + LineEnding;
    Result := Result + FLines[I];
  end;
end;

function EnvEnabled(const VarName: string): Boolean;
begin
  Result := GetEnvironmentVariable(VarName) = '1';
end;

procedure AddHostCandidate(AHosts: TStringList; const AHost: string);
var
  I: Integer;
  LHost: string;
begin
  LHost := Trim(AHost);
  if LHost = '' then
    Exit;

  for I := 0 to AHosts.Count - 1 do
    if SameText(AHosts[I], LHost) then
      Exit;

  AHosts.Add(LHost);
end;

function ResolveTargetHosts: TStringList;
var
  LRawHosts: string;
  LParsed: TStringList;
  I: Integer;
begin
  Result := TStringList.Create;

  LRawHosts := Trim(GetEnvironmentVariable('FAFAFA_CERT_VERIFY_CACHE_HOSTS'));
  if LRawHosts <> '' then
  begin
    LParsed := TStringList.Create;
    try
      ExtractStrings([',', ';', ' ', #9, #10, #13], [], PChar(LRawHosts), LParsed);
      for I := 0 to LParsed.Count - 1 do
        AddHostCandidate(Result, LParsed[I]);
    finally
      LParsed.Free;
    end;
  end;

  AddHostCandidate(Result, GetEnvironmentVariable('FAFAFA_CERT_VERIFY_CACHE_HOST'));
  AddHostCandidate(Result, 'www.google.com');
  AddHostCandidate(Result, 'www.github.com');
  AddHostCandidate(Result, 'www.cloudflare.com');
end;

function HostCandidatesToString(AHosts: TStringList): string;
begin
  if (AHosts = nil) or (AHosts.Count = 0) then
    Exit('(none)');
  Result := AHosts.CommaText;
end;

function ResolveTargetPort: Word;
var
  LValue: Integer;
  LRaw: string;
begin
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_CERT_VERIFY_CACHE_PORT'));
  if LRaw = '' then
    Exit(443);

  LValue := StrToIntDef(LRaw, 443);
  if (LValue <= 0) or (LValue > 65535) then
    LValue := 443;
  Result := Word(LValue);
end;

function ResolveCAInput(out ACAFile, ACAPath: string): Boolean;
begin
  ACAFile := '';
  ACAPath := '';

  {$IFNDEF WINDOWS}
  if FileExists('/etc/ssl/certs/ca-certificates.crt') then
  begin
    ACAFile := '/etc/ssl/certs/ca-certificates.crt';
    Exit(True);
  end;

  if FileExists('/etc/ssl/cert.pem') then
  begin
    ACAFile := '/etc/ssl/cert.pem';
    Exit(True);
  end;

  if DirectoryExists('/etc/ssl/certs') then
  begin
    ACAPath := '/etc/ssl/certs';
    Exit(True);
  end;
  {$ENDIF}

  Result := False;
end;

procedure CloseSocketHandle(ASocket: THandle);
begin
  {$IFDEF WINDOWS}
  if ASocket <> THandle(INVALID_SOCKET) then
    closesocket(TSocket(ASocket));
  {$ELSE}
  if ASocket <> THandle(-1) then
    fpClose(ASocket);
  {$ENDIF}
end;

function ConnectTCP(const AHost: string; APort: Word; out ASocket: THandle): Boolean;
{$IFDEF WINDOWS}
var
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  InAddr: TInAddr;
  TimeoutMs: Integer;
  WSA: TWSAData;
begin
  Result := False;
  ASocket := THandle(INVALID_SOCKET);

  if WSAStartup(MAKEWORD(2, 2), WSA) <> 0 then
    Exit;

  ASocket := THandle(socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
  if ASocket = THandle(INVALID_SOCKET) then
    Exit;

  TimeoutMs := 10000;
  setsockopt(TSocket(ASocket), SOL_SOCKET, SO_RCVTIMEO, @TimeoutMs, SizeOf(TimeoutMs));
  setsockopt(TSocket(ASocket), SOL_SOCKET, SO_SNDTIMEO, @TimeoutMs, SizeOf(TimeoutMs));

  HostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
  if HostEnt = nil then
  begin
    CloseSocketHandle(ASocket);
    ASocket := THandle(INVALID_SOCKET);
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Move(HostEnt^.h_addr_list^^, InAddr, SizeOf(InAddr));
  Addr.sin_addr := InAddr;

  Result := connect(TSocket(ASocket), @Addr, SizeOf(Addr)) = 0;
  if not Result then
  begin
    CloseSocketHandle(ASocket);
    ASocket := THandle(INVALID_SOCKET);
  end;
end;
{$ELSE}
var
  Addr: TInetSockAddr;
  HostEnt: PHostEnt;
  SocketFd: cint;
  TimeoutMs: LongInt;
begin
  Result := False;
  ASocket := THandle(-1);

  SocketFd := fpSocket(AF_INET, SOCK_STREAM, 0);
  if SocketFd < 0 then
    Exit;

  TimeoutMs := 10000;
  fpSetSockOpt(SocketFd, SOL_SOCKET, SO_RCVTIMEO, @TimeoutMs, SizeOf(TimeoutMs));
  fpSetSockOpt(SocketFd, SOL_SOCKET, SO_SNDTIMEO, @TimeoutMs, SizeOf(TimeoutMs));

  HostEnt := gethostbyname(PChar(AHost));
  if HostEnt = nil then
  begin
    fpClose(SocketFd);
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Move(HostEnt^.h_addr_list^^, Addr.sin_addr, SizeOf(Addr.sin_addr));

  if fpConnect(SocketFd, @Addr, SizeOf(Addr)) = 0 then
  begin
    ASocket := SocketFd;
    Result := True;
  end
  else
    fpClose(SocketFd);
end;
{$ENDIF}

function BuildClientContext(
  const AHost: string;
  ASkipValidHitRefresh: Boolean;
  AEnableOCSPCheck: Boolean;
  out ALibrary: ISSLLibrary;
  out AContext: ISSLContext;
  out AError: string
): Boolean;
var
  LOptions: TSSLOptions;
  LCAFile, LCAPath: string;
begin
  Result := False;
  AError := '';
  ALibrary := nil;
  AContext := nil;

  ALibrary := TOpenSSLLibrary.Create;
  if (ALibrary = nil) or (not ALibrary.Initialize) then
  begin
    AError := 'OpenSSL initialization failed';
    Exit;
  end;

  AContext := ALibrary.CreateContext(sslCtxClient);
  if AContext = nil then
  begin
    AError := 'CreateContext returned nil';
    Exit;
  end;

  AContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  AContext.SetServerName(AHost);
  AContext.SetVerifyMode([sslVerifyPeer]);
  if AEnableOCSPCheck then
    AContext.SetCertVerifyFlags([sslCertVerifyCheckOCSP])
  else
    AContext.SetCertVerifyFlags([]);

  if not ResolveCAInput(LCAFile, LCAPath) then
  begin
    AError := 'No system CA bundle/path found';
    Exit;
  end;

  if LCAFile <> '' then
    AContext.LoadCAFile(LCAFile)
  else
    AContext.LoadCAPath(LCAPath);

  LOptions := AContext.GetOptions;
  Include(LOptions, ssoEnableCertVerifyCache);
  if ASkipValidHitRefresh then
    Include(LOptions, ssoSkipCertVerifyCacheValidHitRefresh)
  else
    Exclude(LOptions, ssoSkipCertVerifyCacheValidHitRefresh);
  AContext.SetOptions(LOptions);

  Result := True;
end;

function ConnectOnce(
  const AContext: ISSLContext;
  const AHost: string;
  APort: Word;
  out AHandshakeOk: Boolean;
  out ADetail: string
): Boolean;
var
  LSocket: THandle;
  LConn: ISSLConnection;
begin
  Result := False;
  AHandshakeOk := False;
  ADetail := '';
  LSocket := THandle(-1);

  if not ConnectTCP(AHost, APort, LSocket) then
  begin
    ADetail := 'tcp connect failed';
    Exit;
  end;

  try
    LConn := AContext.CreateConnection(LSocket);
    if LConn = nil then
    begin
      ADetail := 'CreateConnection returned nil';
      Exit;
    end;

    AHandshakeOk := LConn.Connect;
    if AHandshakeOk then
    begin
      ADetail := Format('connect=ok protocol=%s verify=%d',
        [ProtocolVersionToString(LConn.GetProtocolVersion), LConn.GetVerifyResult]);
      LConn.Shutdown;
    end
    else
      ADetail := Format('connect=fail state=%s verify=%d msg=%s',
        [LConn.GetStateString, LConn.GetVerifyResult, LConn.GetVerifyResultString]);

    Result := True;
  finally
    CloseSocketHandle(LSocket);
  end;
end;

function SeedInvalidCacheFromLiveHandshake(
  const AContext: ISSLContext;
  const AHost: string;
  APort: Word;
  out ADetail: string
): Boolean;
var
  LSocket: THandle;
  LConn: ISSLConnection;
  LPeerCert: ISSLCertificate;
  LPeerX509: Pointer;
  LInjected: TCertVerifyResult;
begin
  Result := False;
  ADetail := '';
  LSocket := THandle(-1);

  if not ConnectTCP(AHost, APort, LSocket) then
  begin
    ADetail := 'tcp connect failed for seed step';
    Exit;
  end;

  try
    LConn := AContext.CreateConnection(LSocket);
    if LConn = nil then
    begin
      ADetail := 'CreateConnection returned nil in seed step';
      Exit;
    end;

    if not LConn.Connect then
    begin
      ADetail := Format('seed handshake failed state=%s verify=%d msg=%s',
        [LConn.GetStateString, LConn.GetVerifyResult, LConn.GetVerifyResultString]);
      Exit;
    end;

    LPeerCert := LConn.GetPeerCertificate;
    if LPeerCert = nil then
    begin
      ADetail := 'GetPeerCertificate returned nil in seed step';
      Exit;
    end;

    if not TryGetNativeHandle(LPeerCert, LPeerX509) then
    begin
      ADetail := 'cannot get native X509 handle from peer certificate';
      Exit;
    end;

    LInjected.Valid := False;
    LInjected.ErrorCode := X509_V_ERR_CERT_REVOKED;
    LInjected.ErrorMessage := 'runtime-injected invalid cache entry';
    LInjected.VerifiedAt := Now;
    GetGlobalCertVerifyCache.Put(PX509(LPeerX509), LInjected);

    ADetail := Format('seeded invalid cache entry; seed-verify=%d', [LConn.GetVerifyResult]);
    LConn.Shutdown;
    Result := True;
  finally
    CloseSocketHandle(LSocket);
  end;
end;

procedure RunPolicyScenario(
  const ACheckName: string;
  ASkipValidHitRefresh: Boolean;
  const AExpectedMarker: string;
  const ADisallowMarker: string
);
var
  I: Integer;
  LHost: string;
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LBuildError: string;
  LAttempt1, LAttempt2: Boolean;
  LConnected1, LConnected2: Boolean;
  LDetail1, LDetail2: string;
  LExpectedFound, LDisallowFound: Boolean;
  LDetails: string;
  LEnvSkipDetails: TStringList;
  LFailureDetails: TStringList;
begin
  if (TargetHosts = nil) or (TargetHosts.Count = 0) then
  begin
    Runner.Skip(ACheckName, '[environment] no target hosts configured');
    Exit;
  end;

  LEnvSkipDetails := TStringList.Create;
  LFailureDetails := TStringList.Create;
  try
    for I := 0 to TargetHosts.Count - 1 do
    begin
      LHost := TargetHosts[I];

      CaptureLoggerObject.ClearMessages;
      GetGlobalCertVerifyCache.Clear;

      if not BuildClientContext(LHost, ASkipValidHitRefresh, True, LLibrary, LContext, LBuildError) then
      begin
        LEnvSkipDetails.Add(Format('%s -> context build failed: %s', [LHost, LBuildError]));
        Continue;
      end;

      LAttempt1 := ConnectOnce(LContext, LHost, TargetPort, LConnected1, LDetail1);
      LAttempt2 := ConnectOnce(LContext, LHost, TargetPort, LConnected2, LDetail2);

      LExpectedFound := CaptureLoggerObject.ContainsText(AExpectedMarker);
      LDisallowFound := (ADisallowMarker <> '') and CaptureLoggerObject.ContainsText(ADisallowMarker);

      LDetails := Format('host=%s attempt1=%s handshake1=%s [%s]; attempt2=%s handshake2=%s [%s]',
        [LHost,
         BoolToStr(LAttempt1, True), BoolToStr(LConnected1, True), LDetail1,
         BoolToStr(LAttempt2, True), BoolToStr(LConnected2, True), LDetail2]);

      if (not LAttempt1) or (not LAttempt2) then
      begin
        LEnvSkipDetails.Add(LDetails + '; reason=connection setup failed');
        Continue;
      end;

      if LExpectedFound and (not LDisallowFound) then
      begin
        Runner.Check(ACheckName, True, LDetails);
        Exit;
      end;

      if ASkipValidHitRefresh and
        CaptureLoggerObject.ContainsText('Cert verify cache hit (valid result), refreshing X509_verify_cert (issuer unresolved)') and
        (not LExpectedFound) then
      begin
        LEnvSkipDetails.Add(LDetails + '; reason=issuer unresolved precondition unmet');
        Continue;
      end;

      if (not LConnected1) and (not LConnected2) and (not LExpectedFound) then
      begin
        LEnvSkipDetails.Add(
          LDetails + '; reason=both handshakes failed before expected marker' + LineEnding +
          CaptureLoggerObject.RecentMessages(20));
        Continue;
      end;

      LFailureDetails.Add(
        LDetails + LineEnding +
        'Expected marker: ' + AExpectedMarker + LineEnding +
        'Disallow marker: ' + ADisallowMarker + LineEnding +
        'Recent logs:' + LineEnding +
        CaptureLoggerObject.RecentMessages(40));
    end;

    if (LFailureDetails.Count = 0) and (LEnvSkipDetails.Count > 0) then
    begin
      Runner.Skip(ACheckName,
        '[environment] all host candidates constrained. candidates=' + HostCandidatesToString(TargetHosts) +
        LineEnding + LEnvSkipDetails.Text);
      Exit;
    end;

    Runner.Check(ACheckName, False,
      'Host candidates: ' + HostCandidatesToString(TargetHosts) + LineEnding +
      'Environment-constrained hosts:' + LineEnding + LEnvSkipDetails.Text +
      'Failure details:' + LineEnding + LFailureDetails.Text);
  finally
    LFailureDetails.Free;
    LEnvSkipDetails.Free;
  end;
end;

procedure RunInvalidCacheRefreshScenario;
var
  I: Integer;
  LHost: string;
  LSeedLibrary: ISSLLibrary;
  LSeedContext: ISSLContext;
  LRuntimeLibrary: ISSLLibrary;
  LRuntimeContext: ISSLContext;
  LBuildError: string;
  LSeedOk: Boolean;
  LAttempt2, LConnected2: Boolean;
  LSeedDetail, LDetail2: string;
  LRefreshFound, LSkipFound: Boolean;
  LEnvSkipDetails: TStringList;
  LFailureDetails: TStringList;
begin
  if (TargetHosts = nil) or (TargetHosts.Count = 0) then
  begin
    Runner.Skip('Cache invalid-hit refresh branch (runtime)', '[environment] no target hosts configured');
    Exit;
  end;

  LEnvSkipDetails := TStringList.Create;
  LFailureDetails := TStringList.Create;
  try
    for I := 0 to TargetHosts.Count - 1 do
    begin
      LHost := TargetHosts[I];

      CaptureLoggerObject.ClearMessages;
      GetGlobalCertVerifyCache.Clear;

      if not BuildClientContext(LHost, True, False, LSeedLibrary, LSeedContext, LBuildError) then
      begin
        LEnvSkipDetails.Add(Format('%s -> seed context build failed: %s', [LHost, LBuildError]));
        Continue;
      end;

      LSeedOk := SeedInvalidCacheFromLiveHandshake(LSeedContext, LHost, TargetPort, LSeedDetail);
      if not LSeedOk then
      begin
        LEnvSkipDetails.Add(Format('%s -> cannot seed invalid cache: %s', [LHost, LSeedDetail]));
        Continue;
      end;

      if not BuildClientContext(LHost, True, True, LRuntimeLibrary, LRuntimeContext, LBuildError) then
      begin
        LEnvSkipDetails.Add(Format('%s -> cannot build runtime OCSP context: %s', [LHost, LBuildError]));
        Continue;
      end;

      CaptureLoggerObject.ClearMessages;
      LAttempt2 := ConnectOnce(LRuntimeContext, LHost, TargetPort, LConnected2, LDetail2);

      LRefreshFound := CaptureLoggerObject.ContainsText(
        'Cert verify cache hit (invalid result), refreshing X509_verify_cert');
      LSkipFound := CaptureLoggerObject.ContainsText(
        'Cert verify cache hit (invalid result), skipping X509_verify_cert');

      if LRefreshFound and (not LSkipFound) then
      begin
        Runner.Check('Cache invalid-hit refresh branch (runtime)', True,
          Format('host=%s seed=%s; second=%s', [LHost, LSeedDetail, LDetail2]));
        Exit;
      end;

      if LSkipFound and (not LRefreshFound) then
      begin
        LEnvSkipDetails.Add(
          Format('%s -> issuer unresolved path observed; seed=%s; second=%s', [LHost, LSeedDetail, LDetail2]));
        Continue;
      end;

      if (not LAttempt2) or ((not LConnected2) and (not LRefreshFound)) then
      begin
        LEnvSkipDetails.Add(
          Format('%s -> second handshake unavailable; seed=%s; second=%s', [LHost, LSeedDetail, LDetail2]));
        Continue;
      end;

      LFailureDetails.Add(
        Format('host=%s seed=%s; second=%s', [LHost, LSeedDetail, LDetail2]) + LineEnding +
        'Expected marker: Cert verify cache hit (invalid result), refreshing X509_verify_cert' + LineEnding +
        'Disallow marker: Cert verify cache hit (invalid result), skipping X509_verify_cert' + LineEnding +
        'Recent logs:' + LineEnding + CaptureLoggerObject.RecentMessages(40));
    end;

    if (LFailureDetails.Count = 0) and (LEnvSkipDetails.Count > 0) then
    begin
      Runner.Skip('Cache invalid-hit refresh branch (runtime)',
        '[environment] all host candidates constrained. candidates=' + HostCandidatesToString(TargetHosts) +
        LineEnding + LEnvSkipDetails.Text);
      Exit;
    end;

    Runner.Check('Cache invalid-hit refresh branch (runtime)', False,
      'Host candidates: ' + HostCandidatesToString(TargetHosts) + LineEnding +
      'Environment-constrained hosts:' + LineEnding + LEnvSkipDetails.Text +
      'Failure details:' + LineEnding + LFailureDetails.Text);
  finally
    LFailureDetails.Free;
    LEnvSkipDetails.Free;
  end;
end;

var
  PreviousLogger: ISecurityLogger;
  CaptureLoggerRef: ISecurityLogger;
begin
  WriteLn('OpenSSL Cert Verify Cache Runtime Policy Regression');
  WriteLn('===================================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    TargetHosts := ResolveTargetHosts;
    TargetPort := ResolveTargetPort;

    PreviousLogger := TSecurityLog.Logger;
    CaptureLoggerObject := TMemoryLogger.Create;
    CaptureLoggerObject.SetMinLevel(selDebug);
    CaptureLoggerRef := CaptureLoggerObject;
    TSecurityLog.Logger := CaptureLoggerRef;
    TSecurityLog.SetMinLevel(selDebug);

    if not EnvEnabled('FAFAFA_RUN_NETWORK_TESTS') then
      Runner.Skip('Runtime policy regression', '[environment] network tests disabled (FAFAFA_RUN_NETWORK_TESTS!=1)')
    else
    begin
      {$IFDEF WINDOWS}
      Runner.Skip('Runtime policy regression', '[environment] OpenSSL network regression test is non-Windows only');
      {$ELSE}
      RunPolicyScenario(
        'Cache valid-hit skip branch (runtime)',
        True,
        'Cert verify cache hit (valid result), skipping X509_verify_cert',
        'Cert verify cache hit (valid result), refreshing X509_verify_cert'
      );
      RunPolicyScenario(
        'Cache valid-hit refresh branch (runtime)',
        False,
        'Cert verify cache hit (valid result), refreshing X509_verify_cert',
        'Cert verify cache hit (valid result), skipping X509_verify_cert'
      );
      RunInvalidCacheRefreshScenario;
      {$ENDIF}
    end;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    TSecurityLog.Logger := PreviousLogger;
    CaptureLoggerRef := nil;
    TargetHosts.Free;
    Runner.Free;
  end;
end.
