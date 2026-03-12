program test_freepascal_tls12_system_roots_runtime;

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

  LRawHosts := Trim(GetEnvironmentVariable('FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS'));
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
    AddHostCandidate(Result, GetEnvironmentVariable('FAFAFA_TLS12_SYSTEM_ROOTS_HOST'));
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
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_TLS12_SYSTEM_ROOTS_PORT'));
  if LRaw = '' then
    Exit(443);
  LPort := StrToIntDef(LRaw, 443);
  if (LPort <= 0) or (LPort > 65535) then
    LPort := 443;
  Result := Word(LPort);
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

procedure RunTLS12SystemRootsRuntimeForHost(const AHost: string; APort: Word);
var
  LSocket: THandle;
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LStream: TSSLStream;
begin
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
    AssertTrue(LContext <> nil, 'TLS1.2 system-roots context should be created');

    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    LStream := LConnector.ConnectSocket(LSocket, AHost);
    try
      AssertTrue(LStream <> nil, 'TLS1.2 system-roots handshake should create TLS stream');
      AssertTrue(LStream.Connection.GetProtocolVersion = sslProtocolTLS12,
        'TLS1.2 system-roots handshake should negotiate TLS1.2');
      AssertTrue(LStream.Connection.GetVerifyResult = 0,
        'TLS1.2 system-roots handshake should verify successfully');
      WriteLn('✅ TLS1.2 system-roots runtime passed for ', AHost, ':', APort,
        ' cipher=', LStream.Connection.GetCipherName);
    finally
      LStream.Free;
    end;
  finally
    CloseSocketHandle(LSocket);
  end;
end;

procedure RunTLS12SystemRootsRuntime;
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
      WriteLn('ℹ️  Skip: no TLS1.2 system-roots hosts configured ',
        '(set FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS or FAFAFA_TLS12_SYSTEM_ROOTS_HOST)');
      Exit;
    end;

    LPort := ResolvePort;
    WriteLn('ℹ️  TLS1.2 system-roots host matrix: ', HostCandidatesToString(LHosts), ' port=', LPort);
    for I := 0 to LHosts.Count - 1 do
      RunTLS12SystemRootsRuntimeForHost(LHosts[I], LPort);
  finally
    LHosts.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal TLS1.2 system roots runtime...');
  RunTLS12SystemRootsRuntime;
end.
