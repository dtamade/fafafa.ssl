program test_freepascal_client_trust_source_runtime_matrix;

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
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder,
  fafafa.examples.tcp;

type
  TServerThread = class(TThread)
  private
    FPort: Word;
    FCertificatePEM: string;
    FPrivateKeyPEM: string;
    FAccepted: Boolean;
    FError: string;
    FReadyEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word; const ACertificatePEM, APrivateKeyPEM: string);
    destructor Destroy; override;
    function WaitUntilReady(ATimeoutMS: Cardinal): Boolean;
    property Accepted: Boolean read FAccepted;
    property ErrorText: string read FError;
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

constructor TServerThread.Create(APort: Word; const ACertificatePEM, APrivateKeyPEM: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPort := APort;
  FCertificatePEM := ACertificatePEM;
  FPrivateKeyPEM := APrivateKeyPEM;
  FAccepted := False;
  FError := '';
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
      LCtx.LoadCertificatePEM(FCertificatePEM);
      LCtx.LoadPrivateKeyPEM(FPrivateKeyPEM);

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

          if not LConn.Accept then
          begin
            FError := 'Accept failed: ' + LConn.GetVerifyResultString;
            Exit;
          end;

          FAccepted := True;
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

procedure WriteTextFile(const AFileName, AText: string);
begin
  with TStringList.Create do
  try
    Text := AText;
    SaveToFile(AFileName);
  finally
    Free;
  end;
end;

function CreateTrustedStore(const AServerCertPEM: string): ISSLCertificateStore;
var
  LTrustedCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  AssertTrue(Result <> nil, 'Trusted store should be created');

  LTrustedCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LTrustedCert <> nil, 'Trusted certificate instance should be created');
  AssertTrue(LTrustedCert.LoadFromPEM(AServerCertPEM),
    'Trusted certificate should load from PEM');
  AssertTrue(Result.AddCertificate(LTrustedCert),
    'Trusted store should accept generated certificate');
end;

function ConnectClient(
  APort: Word;
  const AServerName: string;
  ATrustStore: ISSLCertificateStore;
  const ACAFile, ACAPath: string;
  out AError: string
): Boolean;
var
  LNetErr: string;
  LCtx: ISSLContext;
  LSock: TSocketHandle;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LBuffer: array[0..15] of Byte;
begin
  Result := False;
  AError := '';

  if not InitNetwork(LNetErr) then
    Fail('Network init failed: ' + LNetErr);

  LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LCtx <> nil, 'Client context should be created');
  LCtx.SetPreferredVersion(sslProtocolTLS13);
  LCtx.SetVerifyMode([sslVerifyPeer]);
  if ATrustStore <> nil then
    LCtx.SetCertificateStore(ATrustStore);
  if ACAFile <> '' then
    LCtx.LoadCAFile(ACAFile);
  if ACAPath <> '' then
    LCtx.LoadCAPath(ACAPath);

  LSock := ConnectTCP('127.0.0.1', APort);
  try
    LConn := LCtx.CreateConnection(THandle(LSock));
    AssertTrue(LConn <> nil, 'Client connection should be created');
    AssertTrue(Supports(LConn, ISSLClientConnection, LClientConn),
      'Client connection should expose ISSLClientConnection');
    LClientConn.SetServerName(AServerName);

    if not LConn.Connect then
    begin
      AError := LConn.GetVerifyResultString;
      Exit(False);
    end;

    AssertTrue(LConn.GetVerifyResult = 0,
      'Successful trust-source runtime handshake should report verify success');
    AssertTrue(LConn.GetVerifyResultString = 'Verification passed',
      'Successful trust-source runtime handshake should report verification passed');
    AssertTrue(LConn.Read(LBuffer[0], SizeOf(LBuffer)) = 2,
      'Runtime matrix client should read OK payload');
    Result := True;
  finally
    CloseSocket(LSock);
  end;
end;

procedure TestTrustSourceRuntimeMatrix;
var
  LKeyPair: IKeyPairWithCertificate;
  LServerCertPEM: string;
  LServerKeyPEM: string;
  LServer: TServerThread;
  LError: string;
  LTempDir: string;
  LCAFile: string;
begin
  LKeyPair := TCertificate.CreateServerCert('alt.example.com', ['alt.example.com']);
  AssertTrue(LKeyPair <> nil, 'Server key pair should be created');
  LKeyPair.SaveToPEM(LServerCertPEM, LServerKeyPEM);

  LServer := TServerThread.Create(0, LServerCertPEM, LServerKeyPEM);
  try
    LServer.Start;
    AssertTrue(LServer.WaitUntilReady(3000), 'Server should become ready for negative trust case');
    AssertTrue(not ConnectClient(LServer.Port, 'alt.example.com', nil, '', '', LError),
      'Client without trust source should fail verification');
    LServer.WaitFor;
    AssertTrue(Pos('certificate', LowerCase(LError)) > 0,
      'No-trust runtime failure should mention certificate/trust');
  finally
    LServer.Free;
  end;

  LServer := TServerThread.Create(0, LServerCertPEM, LServerKeyPEM);
  try
    LServer.Start;
    AssertTrue(LServer.WaitUntilReady(3000), 'Server should become ready for trust-store case');
    AssertTrue(ConnectClient(LServer.Port, 'alt.example.com', CreateTrustedStore(LServerCertPEM), '', '', LError),
      'Explicit trust store should succeed on runtime socket path: ' + LError);
    LServer.WaitFor;
    AssertTrue(LServer.Accepted, 'Server should accept trust-store client: ' + LServer.ErrorText);
  finally
    LServer.Free;
  end;

  LTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'fafafa_fp_client_trust_runtime_' + IntToStr(Int64(GetTickCount64));
  AssertTrue(ForceDirectories(LTempDir), 'Temporary CA directory should be created');
  LCAFile := IncludeTrailingPathDelimiter(LTempDir) + 'server_ca.pem';
  WriteTextFile(LCAFile, LServerCertPEM);

  LServer := TServerThread.Create(0, LServerCertPEM, LServerKeyPEM);
  try
    LServer.Start;
    AssertTrue(LServer.WaitUntilReady(3000), 'Server should become ready for CA file case');
    AssertTrue(ConnectClient(LServer.Port, 'alt.example.com', nil, LCAFile, '', LError),
      'LoadCAFile should succeed on runtime socket path: ' + LError);
    LServer.WaitFor;
    AssertTrue(LServer.Accepted, 'Server should accept CA-file client: ' + LServer.ErrorText);
  finally
    LServer.Free;
  end;

  LServer := TServerThread.Create(0, LServerCertPEM, LServerKeyPEM);
  try
    LServer.Start;
    AssertTrue(LServer.WaitUntilReady(3000), 'Server should become ready for CA path case');
    AssertTrue(ConnectClient(LServer.Port, 'alt.example.com', nil, '', LTempDir, LError),
      'LoadCAPath should succeed on runtime socket path: ' + LError);
    LServer.WaitFor;
    AssertTrue(LServer.Accepted, 'Server should accept CA-path client: ' + LServer.ErrorText);
  finally
    LServer.Free;
  end;

  DeleteFile(LCAFile);
  RemoveDir(LTempDir);
end;

begin
  WriteLn('Testing FreePascal client trust source runtime matrix...');
  TestTrustSourceRuntimeMatrix;
  WriteLn('✅ FreePascal client trust source runtime matrix checks passed');
end.
