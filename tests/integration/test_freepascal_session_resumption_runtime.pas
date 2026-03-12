program test_freepascal_session_resumption_runtime;

{$mode ObjFPC}{$H+}{$J-}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.tls,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

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

function ResolveHost: string;
begin
  Result := Trim(GetEnvironmentVariable('FAFAFA_SESSION_RESUMPTION_HOST'));
  if Result = '' then
    Result := 'www.google.com';
end;

function ResolvePort: Word;
var
  LRaw: string;
  LPort: Integer;
begin
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_SESSION_RESUMPTION_PORT'));
  if LRaw = '' then
    Exit(443);
  LPort := StrToIntDef(LRaw, 443);
  if (LPort <= 0) or (LPort > 65535) then
    LPort := 443;
  Result := Word(LPort);
end;

function ConnectOnce(const AHost: string; APort: Word; ASessionIn: ISSLSession;
  out ASessionOut: ISSLSession; out AReused: Boolean; out AError: string): Boolean;
var
  LSock: TSocketHandle;
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConnection: ISSLClientConnection;
  LRequest: RawByteString;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LAttempt: Integer;
begin
  Result := False;
  ASessionOut := nil;
  AReused := False;
  AError := '';

  LSock := ConnectTCP(AHost, APort);
  AssertTrue(LSock <> INVALID_SOCKET,
    Format('TCP connect should succeed for %s:%d', [AHost, APort]));

  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;
    AssertTrue(LContext <> nil, 'PurePascal builder should create client context');

    LConnection := LContext.CreateConnection(THandle(LSock));
    AssertTrue(LConnection <> nil, 'PurePascal client connection should be created');
    AssertTrue(Supports(LConnection, ISSLClientConnection, LClientConnection),
      'PurePascal client connection should expose ISSLClientConnection');
    LClientConnection.SetServerName(AHost);

    if ASessionIn <> nil then
      LConnection.SetSession(ASessionIn);

    if not LConnection.Connect then
    begin
      AError := LConnection.GetVerifyResultString;
      Exit(False);
    end;

    AReused := LConnection.IsSessionReused;

    LRequest := 'HEAD / HTTP/1.1'#13#10 +
      'Host: ' + AHost + #13#10 +
      'Connection: close'#13#10#13#10;
    if Length(LRequest) > 0 then
      LConnection.Write(LRequest[1], Length(LRequest));

    for LAttempt := 1 to 5 do
    begin
      LRead := LConnection.Read(LBuffer[0], SizeOf(LBuffer));
      if LRead < 0 then
      begin
        AError := LConnection.GetVerifyResultString;
        Exit(False);
      end;

      ASessionOut := LConnection.GetSession;
      if (ASessionOut <> nil) and ASessionOut.IsResumable then
        Break;
      if LRead = 0 then
        Break;
    end;

    if ASessionOut = nil then
      ASessionOut := LConnection.GetSession;
    AReused := LConnection.IsSessionReused;
    Result := True;
  finally
    CloseSocket(LSock);
  end;
end;

procedure RunSessionResumptionRuntime;
var
  LHost: string;
  LPort: Word;
  LSession1, LSession2: ISSLSession;
  LReused1, LReused2: Boolean;
  LError1, LError2: string;
  LNetErr: string;
begin
  if not EnvEnabled('FAFAFA_RUN_NETWORK_TESTS') then
  begin
    WriteLn('ℹ️  Skip: network tests disabled (set FAFAFA_RUN_NETWORK_TESTS=1)');
    Exit;
  end;

  if not InitNetwork(LNetErr) then
    Fail('Network init failed: ' + LNetErr);

  try
    LHost := ResolveHost;
    LPort := ResolvePort;

    AssertTrue(ConnectOnce(LHost, LPort, nil, LSession1, LReused1, LError1),
      'First runtime handshake should succeed: ' + LError1);
    AssertTrue(not LReused1, 'First runtime handshake should not be marked reused');
    AssertTrue(LSession1 <> nil, 'First runtime handshake should expose session snapshot');
    AssertTrue(LSession1.IsResumable,
      'First runtime handshake should expose resumable session');

    AssertTrue(ConnectOnce(LHost, LPort, LSession1, LSession2, LReused2, LError2),
      'Second runtime handshake should succeed: ' + LError2);
    AssertTrue(LReused2,
      'Second runtime handshake should be marked reused after session resumption');
    AssertTrue(LSession2 <> nil, 'Second runtime handshake should expose session snapshot');
    AssertTrue(LSession2.IsResumable,
      'Second runtime handshake should keep exposing resumable session');

    WriteLn('✅ FreePascal runtime session resumption captured for ', LHost, ':', LPort);
  finally
    CleanupNetwork;
  end;
end;

begin
  WriteLn('Testing FreePascal session resumption runtime...');
  RunSessionResumptionRuntime;
end.
