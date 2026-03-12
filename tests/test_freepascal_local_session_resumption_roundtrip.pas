program test_freepascal_local_session_resumption_roundtrip;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$DEFINE USE_CTHREADS}{$ENDIF}

uses
  {$IFDEF USE_CTHREADS}
  CThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2,
  {$ELSE}
  BaseUnix, Sockets,
  {$ENDIF}
  SysUtils, Classes,
  SyncObjs,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.examples.tcp;

const
  SERVER_CERT_FILE = 'tests/certificate/test_certs/signer_cert.pem';
  SERVER_KEY_FILE = 'tests/certificate/test_certs/signer_key.pem';

type
  TServerThread = class(TThread)
  private
    FPort: Word;
    FResumeSession: ISSLSession;
    FAccepted: Boolean;
    FReused: Boolean;
    FError: string;
    FSessionResumable: Boolean;
    FReadyEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word; AResumeSession: ISSLSession);
    destructor Destroy; override;
    function WaitUntilReady(ATimeoutMS: Cardinal): Boolean;
    property Accepted: Boolean read FAccepted;
    property Reused: Boolean read FReused;
    property ErrorText: string read FError;
    property SessionResumable: Boolean read FSessionResumable;
    property Port: Word read FPort;
  end;

function GetBoundPort(ASocket: TSocketHandle): Word;
var
  {$IFDEF MSWINDOWS}
  LAddr: TSockAddr;
  LAddrLen: Integer;
  {$ELSE}
  LAddr: TInetSockAddr;
  LAddrLen: TSockLen;
  {$ENDIF}
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddrLen := SizeOf(LAddr);
  {$IFDEF MSWINDOWS}
  if getsockname(ASocket, LAddr, LAddrLen) <> 0 then
    raise Exception.Create('Unable to query bound socket port');
  Result := ntohs(LAddr.sin_port);
  {$ELSE}
  if fpGetSockName(ASocket, @LAddr, @LAddrLen) <> 0 then
    raise Exception.Create('Unable to query bound socket port');
  Result := ntohs(LAddr.sin_port);
  {$ENDIF}
end;

constructor TServerThread.Create(APort: Word; AResumeSession: ISSLSession);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPort := APort;
  FResumeSession := AResumeSession;
  FAccepted := False;
  FReused := False;
  FError := '';
  FSessionResumable := False;
  FReadyEvent := TEvent.Create(nil, True, False, '');
end;

destructor TServerThread.Destroy;
begin
  FReadyEvent.Free;
  inherited Destroy;
end;

function TServerThread.WaitUntilReady(ATimeoutMS: Cardinal): Boolean;
begin
  Result := FReadyEvent.WaitFor(ATimeoutMS) = wrSignaled;
end;

procedure TServerThread.Execute;
var
  LNetErr: string;
  LCtx: ISSLContext;
  LListenSock, LClientSock: TSocketHandle;
  LConn: ISSLConnection;
  LSession: ISSLSession;
  LPayload: RawByteString;
begin
  try
    try
      if not InitNetwork(LNetErr) then
      begin
        FError := 'Network init failed: ' + LNetErr;
        Exit;
      end;

      LCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
      if LCtx = nil then
      begin
        FError := 'CreateContext returned nil';
        Exit;
      end;

      LCtx.SetPreferredVersion(sslProtocolTLS13);
      LCtx.SetVerifyMode([]);
      LCtx.LoadCertificate(SERVER_CERT_FILE);
      LCtx.LoadPrivateKey(SERVER_KEY_FILE);
      if not (ssoEnableSessionTickets in LCtx.GetOptions) then
      begin
        FError := 'Server context is missing ssoEnableSessionTickets';
        Exit;
      end;

      LListenSock := ListenTCP(FPort, '0.0.0.0');
      try
        FPort := GetBoundPort(LListenSock);
        FReadyEvent.SetEvent;
        LClientSock := AcceptConnection(LListenSock);
        try
          LConn := LCtx.CreateConnection(THandle(LClientSock));
          if LConn = nil then
          begin
            FError := 'CreateConnection returned nil';
            Exit;
          end;

          if FResumeSession <> nil then
            LConn.SetSession(FResumeSession);

          if not LConn.Accept then
          begin
            FError := 'Accept failed: ' + LConn.GetVerifyResultString;
            Exit;
          end;

          FAccepted := True;
          FReused := LConn.IsSessionReused;
          LSession := LConn.GetSession;
          FSessionResumable := (LSession <> nil) and LSession.IsResumable;

          LPayload := 'OK';
          if Length(LPayload) > 0 then
            LConn.Write(LPayload[1], Length(LPayload));

          LConn.Shutdown;
        finally
          CloseSocket(LClientSock);
        end;
      finally
        CloseSocket(LListenSock);
      end;
    except
      on E: Exception do
        FError := E.ClassName + ': ' + E.Message;
    end;
  finally
    FReadyEvent.SetEvent;
  end;
end;

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

function ConnectLocalClient(APort: Word; ASession: ISSLSession; out ASessionOut: ISSLSession;
  out AReused: Boolean; out AError: string): Boolean;
var
  LNetErr: string;
  LCtx: ISSLContext;
  LSock: TSocketHandle;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LInfo: TSSLConnectionInfo;
  LBuffer: array[0..255] of Byte;
  LRead: Integer;
  LAttempt: Integer;
  LTotalRead: Integer;
begin
  ASessionOut := nil;
  AReused := False;
  AError := '';
  Result := False;
  LTotalRead := 0;

  if not InitNetwork(LNetErr) then
    Fail('Network init failed: ' + LNetErr);

  LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LCtx <> nil, 'Client context should be created');
  LCtx.SetPreferredVersion(sslProtocolTLS13);
  LCtx.SetVerifyMode([]);

  LSock := ConnectTCP('127.0.0.1', APort);
  try
    LConn := LCtx.CreateConnection(THandle(LSock));
    AssertTrue(LConn <> nil, 'Client connection should be created');
    AssertTrue(Supports(LConn, ISSLClientConnection, LClientConn),
      'Client connection should expose ISSLClientConnection');
    LClientConn.SetServerName('localhost');
    if ASession <> nil then
      LConn.SetSession(ASession);

    if not LConn.Connect then
    begin
      AError := LConn.GetVerifyResultString;
      Exit(False);
    end;

    AReused := LConn.IsSessionReused;
    LInfo := LConn.GetConnectionInfo;
    if LInfo.IsResumed <> AReused then
    begin
      AError := Format(
        'connection_info_resumed_mismatch info=%s reused=%s state="%s"',
        [BoolToStr(LInfo.IsResumed, True), BoolToStr(AReused, True), LConn.GetStateString]
      );
      Exit(False);
    end;

    for LAttempt := 1 to 5 do
    begin
      LRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
      if LRead > 0 then
        Inc(LTotalRead, LRead);
      if LRead <= 0 then
        Break;
      ASessionOut := LConn.GetSession;
      if (ASessionOut <> nil) and ASessionOut.IsResumable then
        Break;
    end;

    ASessionOut := LConn.GetSession;
    if (ASessionOut <> nil) and (not ASessionOut.IsResumable) then
      AError := Format(
        'session_not_resumable totalRead=%d reused=%s sessionId="%s" cipher="%s" lastError="%s"',
        [
          LTotalRead,
          BoolToStr(AReused, True),
          ASessionOut.GetID,
          ASessionOut.GetCipherName,
          LConn.GetVerifyResultString
        ]
      )
    else if (ASessionOut = nil) then
      AError := Format(
        'session_missing totalRead=%d reused=%s lastError="%s"',
        [LTotalRead, BoolToStr(AReused, True), LConn.GetVerifyResultString]
      );
    Result := True;
    LConn.Shutdown;
  finally
    CloseSocket(LSock);
  end;
end;

procedure TestLocalResumptionRoundtrip;
var
  LServer1, LServer2: TServerThread;
  LSession1, LSession2: ISSLSession;
  LClientReused: Boolean;
  LClientError: string;
begin
  LServer1 := TServerThread.Create(0, nil);
  try
    LServer1.Start;
    AssertTrue(LServer1.WaitUntilReady(5000),
      'First local server should become ready');
    AssertTrue(LServer1.ErrorText = '',
      'First local server setup should succeed: ' + LServer1.ErrorText);
    AssertTrue(LServer1.Port <> 0,
      'First local server should expose assigned port');
    if not ConnectLocalClient(LServer1.Port, nil, LSession1, LClientReused, LClientError) then
    begin
      LServer1.WaitFor;
      Fail(Format(
        'First local client handshake should succeed: client="%s" serverAccepted=%s serverSessionResumable=%s serverError="%s"',
        [
          LClientError,
          BoolToStr(LServer1.Accepted, True),
          BoolToStr(LServer1.SessionResumable, True),
          LServer1.ErrorText
        ]
      ));
    end;
    LServer1.WaitFor;
    AssertTrue(LServer1.Accepted, 'First server accept should succeed: ' + LServer1.ErrorText);
    AssertTrue(not LClientReused, 'First handshake should not be marked reused');
    AssertTrue(LSession1 <> nil, 'First handshake should expose session snapshot');
    AssertTrue(LSession1.IsResumable,
      Format(
        'First handshake should expose resumable session after ticket processing: client="%s" serverSessionResumable=%s',
        [LClientError, BoolToStr(LServer1.SessionResumable, True)]
      ));
  finally
    LServer1.Free;
  end;

  LServer2 := TServerThread.Create(0, LSession1);
  try
    LServer2.Start;
    AssertTrue(LServer2.WaitUntilReady(5000),
      'Second local server should become ready');
    AssertTrue(LServer2.ErrorText = '',
      'Second local server setup should succeed: ' + LServer2.ErrorText);
    AssertTrue(LServer2.Port <> 0,
      'Second local server should expose assigned port');
    if not ConnectLocalClient(LServer2.Port, LSession1, LSession2, LClientReused, LClientError) then
    begin
      LServer2.WaitFor;
      Fail(Format(
        'Second local client handshake should succeed: client="%s" serverAccepted=%s serverSessionResumable=%s serverError="%s"',
        [
          LClientError,
          BoolToStr(LServer2.Accepted, True),
          BoolToStr(LServer2.SessionResumable, True),
          LServer2.ErrorText
        ]
      ));
    end;
    LServer2.WaitFor;
    AssertTrue(LServer2.Accepted, 'Second server accept should succeed: ' + LServer2.ErrorText);
    AssertTrue(
      LClientReused,
      Format(
        'Second local client handshake should be marked reused: client="%s" serverReused=%s serverError="%s"',
        [LClientError, BoolToStr(LServer2.Reused, True), LServer2.ErrorText]
      )
    );
    AssertTrue(LServer2.Reused, 'Second local server handshake should be marked reused');
  finally
    LServer2.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal local session resumption roundtrip...');
  TestLocalResumptionRoundtrip;
  WriteLn('✅ FreePascal local session resumption roundtrip checks passed');
end.
