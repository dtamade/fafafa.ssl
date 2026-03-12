program test_freepascal_tls12_resumption_runtime;

{$mode ObjFPC}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.tls,
  fafafa.ssl.context.builder
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

procedure Fail(const AMessage: string);
begin
  WriteLn('❌ ', AMessage);
  Halt(1);
end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    Fail(AMessage);
end;

function EnvEnabled(const AName: string): Boolean;
begin
  Result := GetEnvironmentVariable(AName) = '1';
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
  I: Integer;
  LParsed: TStringList;
  LRawHosts: string;
begin
  Result := TStringList.Create;

  LRawHosts := Trim(GetEnvironmentVariable('FAFAFA_TLS12_RESUMPTION_HOSTS'));
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

  if Result.Count = 0 then
    AddHostCandidate(Result, GetEnvironmentVariable('FAFAFA_TLS12_RESUMPTION_HOST'));
end;

function HostCandidatesToString(AHosts: TStringList): string;
begin
  if (AHosts = nil) or (AHosts.Count = 0) then
    Exit('(none)');
  Result := AHosts.CommaText;
end;

function ResolvePort: Word;
var
  LRaw: string;
  LPort: Integer;
begin
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_TLS12_RESUMPTION_PORT'));
  if LRaw = '' then
    Exit(443);
  LPort := StrToIntDef(LRaw, 443);
  if (LPort <= 0) or (LPort > 65535) then
    LPort := 443;
  Result := Word(LPort);
end;

function RequireReuse: Boolean;
begin
  Result := GetEnvironmentVariable('FAFAFA_TLS12_RESUMPTION_REQUIRE_REUSE') = '1';
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
  WSAData: TWSAData;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  InAddr: TInAddr;
begin
  Result := False;
  ASocket := THandle(INVALID_SOCKET);
  if WSAStartup($0202, WSAData) <> 0 then
    Exit;
  try
    HostEnt := GetHostByName(PAnsiChar(AnsiString(AHost)));
    if HostEnt = nil then
      Exit;
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(APort);
    Move(HostEnt^.h_addr_list^^, InAddr, SizeOf(InAddr));
    Addr.sin_addr := InAddr;
    ASocket := THandle(socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
    if ASocket = THandle(INVALID_SOCKET) then
      Exit;
    if WinSock2.connect(TSocket(ASocket), Addr, SizeOf(Addr)) <> 0 then
      Exit;
    Result := True;
  finally
    if not Result then
      CloseSocketHandle(ASocket);
    WSACleanup;
  end;
end;
{$ELSE}
var
  SockAddr: TInetSockAddr;
  HostEnt: PHostEnt;
  Sock: cint;
begin
  Result := False;
  ASocket := THandle(-1);
  HostEnt := gethostbyname(PChar(AHost));
  if HostEnt = nil then
    Exit;
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    Exit;
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);
  Move(HostEnt^.h_addr_list^^, SockAddr.sin_addr, HostEnt^.h_length);
  if fpConnect(Sock, @SockAddr, SizeOf(SockAddr)) <> 0 then
  begin
    fpClose(Sock);
    Exit;
  end;
  ASocket := THandle(Sock);
  Result := True;
end;
{$ENDIF}

function ConnectOnce(const AHost: string; APort: Word; ASessionIn: ISSLSession;
  out ASessionOut: ISSLSession; out AReused: Boolean): Boolean;
var
  LSocket: THandle;
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LStream: TSSLStream;
  LReq: RawByteString;
  LBuf: array[0..4095] of Byte;
  LRead: Integer;
begin
  Result := False;
  ASessionOut := nil;
  AReused := False;
  LSocket := THandle(-1);

  AssertTrue(ConnectTCP(AHost, APort, LSocket),
    Format('TCP connect should succeed for %s:%d', [AHost, APort]));
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithTLS12
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;
    AssertTrue(LContext <> nil, 'TLS1.2 resumption runtime context should be created');

    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    if ASessionIn <> nil then
      LConnector := LConnector.WithSession(ASessionIn);

    LStream := LConnector.ConnectSocket(LSocket, AHost);
    try
      LReq := 'HEAD / HTTP/1.0'#13#10 +
        'Host: ' + AHost + #13#10#13#10;
      LStream.Write(LReq[1], Length(LReq));
      LRead := LStream.Read(LBuf[0], SizeOf(LBuf));
      AssertTrue(LRead > 0, 'TLS1.2 resumption runtime should read a response');

      ASessionOut := LStream.Connection.GetSession;
      AReused := LStream.Connection.IsSessionReused;
      Result := True;
    finally
      LStream.Free;
    end;
  finally
    CloseSocketHandle(LSocket);
  end;
end;

procedure RunTLS12ResumptionRuntimeForHost(const AHost: string; APort: Word);
var
  LSession1, LSession2: ISSLSession;
  LReused1, LReused2: Boolean;
begin
  AssertTrue(ConnectOnce(AHost, APort, nil, LSession1, LReused1),
    'First TLS1.2 resumption runtime connect should succeed');
  AssertTrue(LSession1 <> nil, 'First TLS1.2 resumption runtime connect should produce session');
  AssertTrue(LSession1.IsResumable, 'First TLS1.2 resumption runtime session should be resumable');
  AssertTrue(not LReused1, 'First TLS1.2 resumption runtime connect should not be reused');

  AssertTrue(ConnectOnce(AHost, APort, LSession1, LSession2, LReused2),
    'Second TLS1.2 resumption runtime connect should succeed');
  AssertTrue(LSession2 <> nil, 'Second TLS1.2 resumption runtime connect should produce session');
  AssertTrue(LSession2.IsResumable, 'Second TLS1.2 resumption runtime session should remain resumable');
  if RequireReuse then
    AssertTrue(LReused2, 'Second TLS1.2 resumption runtime connect should be reused');

  WriteLn('✅ TLS1.2 resumption runtime passed for ', AHost, ':', APort,
    ' reused=', BoolToStr(LReused2, True),
    ' require-reuse=', BoolToStr(RequireReuse, True));
end;

procedure RunTLS12ResumptionRuntime;
var
  LHosts: TStringList;
  LPort: Word;
  I: Integer;
begin
  if not EnvEnabled('FAFAFA_RUN_NETWORK_TESTS') then
  begin
    WriteLn('ℹ️  Skip: network tests disabled (set FAFAFA_RUN_NETWORK_TESTS=1)');
    Exit;
  end;

  LHosts := ResolveTargetHosts;
  try
    if LHosts.Count = 0 then
    begin
      WriteLn('ℹ️  Skip: no TLS1.2 resumption hosts configured ',
        '(set FAFAFA_TLS12_RESUMPTION_HOSTS or FAFAFA_TLS12_RESUMPTION_HOST)');
      Exit;
    end;

    LPort := ResolvePort;
    WriteLn('ℹ️  TLS1.2 resumption host matrix: ', HostCandidatesToString(LHosts),
      ' port=', LPort, ' require-reuse=', BoolToStr(RequireReuse, True));
    for I := 0 to LHosts.Count - 1 do
      RunTLS12ResumptionRuntimeForHost(LHosts[I], LPort);
  finally
    LHosts.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal TLS1.2 resumption runtime...');
  RunTLS12ResumptionRuntime;
end.
