program test_freepascal_alpn_runtime;

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

procedure AddUniqueValue(AValues: TStringList; const AValue: string);
var
  I: Integer;
  LValue: string;
begin
  LValue := Trim(AValue);
  if LValue = '' then
    Exit;

  for I := 0 to AValues.Count - 1 do
    if SameText(AValues[I], LValue) then
      Exit;

  AValues.Add(LValue);
end;

function ResolveTargetHosts: TStringList;
var
  I: Integer;
  LParsed: TStringList;
  LRawHosts: string;
begin
  Result := TStringList.Create;

  LRawHosts := Trim(GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_HOSTS'));
  if LRawHosts <> '' then
  begin
    LParsed := TStringList.Create;
    try
      ExtractStrings([',', ';', ' ', #9, #10, #13], [], PChar(LRawHosts), LParsed);
      for I := 0 to LParsed.Count - 1 do
        AddUniqueValue(Result, LParsed[I]);
    finally
      LParsed.Free;
    end;
  end;

  if Result.Count = 0 then
    AddUniqueValue(Result, GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_HOST'));
end;

function ResolveALPNProtocols: string;
begin
  Result := Trim(GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_PROTOCOLS'));
  if Result = '' then
    Result := 'h2,http/1.1';
end;

function ResolveExpectedALPNProtocol: string;
begin
  Result := Trim(GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_EXPECTED_PROTOCOL'));
end;

function AllowEmptyALPNSelection: Boolean;
begin
  Result := GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_ALLOW_EMPTY') = '1';
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
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_ALPN_RUNTIME_PORT'));
  if LRaw = '' then
    Exit(443);

  LPort := StrToIntDef(LRaw, 443);
  if (LPort <= 0) or (LPort > 65535) then
    LPort := 443;
  Result := Word(LPort);
end;

function SplitCSV(const AValue: string): TStringList;
begin
  Result := TStringList.Create;
  ExtractStrings([',', ';', ' ', #9, #10, #13], [], PChar(AValue), Result);
end;

function IsOfferedProtocol(const ASelected, AOffered: string): Boolean;
var
  I: Integer;
  LList: TStringList;
begin
  Result := False;
  LList := SplitCSV(AOffered);
  try
    for I := 0 to LList.Count - 1 do
      if SameText(Trim(LList[I]), Trim(ASelected)) then
        Exit(True);
  finally
    LList.Free;
  end;
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

procedure RunALPNRuntimeForHost(
  const AHost: string;
  APort: Word;
  const AALPNProtocols: string;
  const AExpectedALPNProtocol: string
);
var
  LSocket: THandle;
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LStream: TSSLStream;
  LSelectedALPN: string;
  LInfo: TSSLConnectionInfo;
begin
  LSocket := THandle(-1);

  AssertTrue(ConnectTCP(AHost, APort, LSocket),
    Format('TCP connect should succeed for %s:%d', [AHost, APort]));
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithVerifyPeer
      .WithSystemRoots
      .WithALPN(AALPNProtocols)
      .BuildClient;
    AssertTrue(LContext <> nil, 'ALPN runtime context should be created');

    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    LStream := LConnector.ConnectSocket(LSocket, AHost);
    try
      AssertTrue(LStream <> nil, 'ALPN runtime handshake should create TLS stream');
      AssertTrue(LStream.Connection.GetVerifyResult = 0,
        'ALPN runtime handshake should verify successfully');

      LSelectedALPN := LStream.Connection.GetSelectedALPNProtocol;
      if not AllowEmptyALPNSelection then
        AssertTrue(LSelectedALPN <> '',
          'ALPN runtime handshake should negotiate a non-empty ALPN protocol');
      AssertTrue(IsOfferedProtocol(LSelectedALPN, AALPNProtocols),
        'ALPN runtime handshake should negotiate one of the offered protocols');
      if AExpectedALPNProtocol <> '' then
        AssertTrue(SameText(LSelectedALPN, AExpectedALPNProtocol),
          'ALPN runtime handshake should negotiate the expected protocol');

      LInfo := LStream.Connection.GetConnectionInfo;
      AssertTrue(LInfo.ALPNProtocol = LSelectedALPN,
        'Connection info should mirror the negotiated ALPN protocol');

      WriteLn(
        '✅ FreePascal ALPN runtime passed for ', AHost, ':', APort,
        ' protocol=', ProtocolVersionToString(LStream.Connection.GetProtocolVersion),
        ' cipher=', LStream.Connection.GetCipherName,
        ' alpn=', LSelectedALPN
      );
    finally
      LStream.Free;
    end;
  finally
    CloseSocketHandle(LSocket);
  end;
end;

procedure RunALPNRuntime;
var
  LHosts: TStringList;
  LPort: Word;
  LALPNProtocols: string;
  LExpectedALPNProtocol: string;
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
      WriteLn('ℹ️  Skip: no ALPN runtime hosts configured ',
        '(set FAFAFA_ALPN_RUNTIME_HOSTS or FAFAFA_ALPN_RUNTIME_HOST)');
      Exit;
    end;

    LPort := ResolvePort;
    LALPNProtocols := ResolveALPNProtocols;
    LExpectedALPNProtocol := ResolveExpectedALPNProtocol;
    WriteLn('ℹ️  ALPN runtime host matrix: ', HostCandidatesToString(LHosts),
      ' port=', LPort, ' offered=', LALPNProtocols,
      ' expected=', LExpectedALPNProtocol,
      ' allow-empty=', BoolToStr(AllowEmptyALPNSelection, True));
    for I := 0 to LHosts.Count - 1 do
      RunALPNRuntimeForHost(LHosts[I], LPort, LALPNProtocols, LExpectedALPNProtocol);
    WriteLn('✅ FreePascal ALPN runtime matrix passed for ',
      HostCandidatesToString(LHosts), ' offered=', LALPNProtocols,
      ' expected=', LExpectedALPNProtocol,
      ' allow-empty=', BoolToStr(AllowEmptyALPNSelection, True));
  finally
    LHosts.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal ALPN runtime...');
  RunALPNRuntime;
end.
