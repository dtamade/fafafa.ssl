program test_freepascal_session_ticket_runtime;

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
  Result := Trim(GetEnvironmentVariable('FAFAFA_SESSION_TICKET_HOST'));
  if Result = '' then
    Result := 'www.google.com';
end;

function ResolvePort: Word;
var
  LRaw: string;
  LPort: Integer;
begin
  LRaw := Trim(GetEnvironmentVariable('FAFAFA_SESSION_TICKET_PORT'));
  if LRaw = '' then
    Exit(443);
  LPort := StrToIntDef(LRaw, 443);
  if (LPort <= 0) or (LPort > 65535) then
    LPort := 443;
  Result := Word(LPort);
end;

procedure RunSessionTicketRuntime;
var
  LHost: string;
  LPort: Word;
  LSock: TSocketHandle;
  LContext: ISSLContext;
  LTLS: TSSLConnector;
  LStream: TSSLStream;
  LRequest: RawByteString;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LAttempt: Integer;
  LSession: ISSLSession;
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
    LSock := ConnectTCP(LHost, LPort);
    AssertTrue(LSock <> INVALID_SOCKET,
      Format('TCP connect should succeed for %s:%d', [LHost, LPort]));

    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;
    AssertTrue(LContext <> nil, 'PurePascal builder should create client context');

    LTLS := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    LStream := LTLS.ConnectSocket(THandle(LSock), LHost);
    try
      LRequest := 'HEAD / HTTP/1.1'#13#10 +
        'Host: ' + LHost + #13#10 +
        'Connection: close'#13#10#13#10;
      LStream.Write(LRequest[1], Length(LRequest));

      LSession := nil;
      for LAttempt := 1 to 5 do
      begin
        LRead := LStream.Connection.Read(LBuffer[0], SizeOf(LBuffer));
        if LRead < 0 then
        begin
          LSession := LStream.Connection.GetSession;
          if (LSession <> nil) and LSession.IsResumable then
            Break;
          Fail('Runtime session ticket read failed: ' +
            LStream.Connection.GetVerifyResultString);
        end;
        if LRead = 0 then
          Break;

        LSession := LStream.Connection.GetSession;
        if (LSession <> nil) and LSession.IsResumable then
          Break;
      end;

      AssertTrue(LSession <> nil, 'Runtime session ticket path should expose session snapshot');
      AssertTrue(LSession.IsResumable,
        'Runtime session ticket path should expose resumable session after reading post-handshake data');
      WriteLn('✅ FreePascal runtime session ticket captured for ', LHost, ':', LPort);
    finally
      LStream.Free;
    end;
  finally
    CleanupNetwork;
  end;
end;

begin
  WriteLn('Testing FreePascal session ticket runtime...');
  RunSessionTicketRuntime;
end.
