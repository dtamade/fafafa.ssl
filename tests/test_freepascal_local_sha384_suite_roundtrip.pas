program test_freepascal_local_sha384_suite_roundtrip;

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
  fafafa.ssl.tls13.wire,
  fafafa.examples.tcp;

const
  SERVER_CERT_FILE = 'tests/certificate/test_certs/signer_cert.pem';
  SERVER_KEY_FILE = 'tests/certificate/test_certs/signer_key.pem';
  SHA384_SUITE = 'TLS_AES_256_GCM_SHA384';

type
  TServerThread = class(TThread)
  private
    FPort: Word;
    FAccepted: Boolean;
    FCipherName: string;
    FError: string;
    FReadyEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word);
    destructor Destroy; override;
    function WaitUntilReady(ATimeoutMS: Cardinal): Boolean;
    property Accepted: Boolean read FAccepted;
    property CipherName: string read FCipherName;
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

constructor TServerThread.Create(APort: Word);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPort := APort;
  FAccepted := False;
  FCipherName := '';
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
      LCtx.SetCipherSuites(SHA384_SUITE);
      LCtx.LoadCertificate(SERVER_CERT_FILE);
      LCtx.LoadPrivateKey(SERVER_KEY_FILE);

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
          FCipherName := LConn.GetCipherName;

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

procedure TestLocalSHA384SuiteRoundtrip;
var
  LServer: TServerThread;
  LNetErr: string;
  LCtx: ISSLContext;
  LSock: TSocketHandle;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LInfo: TSSLConnectionInfo;
  LBuffer: array[0..15] of Byte;
  LRead: Integer;
begin
  if not InitNetwork(LNetErr) then
    Fail('Network init failed: ' + LNetErr);

  LServer := TServerThread.Create(0);
  try
    LServer.Start;
    AssertTrue(LServer.WaitUntilReady(3000), 'Server thread should become ready');

    LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    AssertTrue(LCtx <> nil, 'Client context should be created');
    LCtx.SetPreferredVersion(sslProtocolTLS13);
    LCtx.SetVerifyMode([]);
    LCtx.SetCipherSuites(SHA384_SUITE);

    LSock := ConnectTCP('127.0.0.1', LServer.Port);
    try
      LConn := LCtx.CreateConnection(THandle(LSock));
      AssertTrue(LConn <> nil, 'Client connection should be created');
      AssertTrue(Supports(LConn, ISSLClientConnection, LClientConn),
        'Client connection should expose ISSLClientConnection');
      LClientConn.SetServerName('localhost');

      if not LConn.Connect then
      begin
        LServer.WaitFor;
        Fail(
          'Local SHA384 suite handshake should complete: client="' +
          LConn.GetVerifyResultString + '" server="' + LServer.ErrorText + '"'
        );
      end;
      AssertTrue(LConn.GetCipherName = SHA384_SUITE,
        'Client should negotiate TLS_AES_256_GCM_SHA384');
      LInfo := LConn.GetConnectionInfo;
      AssertTrue(LInfo.ProtocolVersion = sslProtocolTLS13,
        'Connection info should report TLS 1.3');
      AssertTrue(LInfo.CipherSuite = SHA384_SUITE,
        'Connection info should report negotiated SHA384 suite');
      AssertTrue(LInfo.CipherSuiteId = TLS13_CIPHER_AES_256_GCM_SHA384,
        'Connection info should expose TLS_AES_256_GCM_SHA384 cipher suite id');
      AssertTrue(LInfo.KeySize = 32,
        'Connection info should expose AES-256 key size');
      AssertTrue(LInfo.MacSize = 16,
        'Connection info should expose TLS 1.3 AEAD tag size');
      AssertTrue(LInfo.KeyExchange = sslKexECDHE_RSA,
        'Connection info should expose ECDHE_RSA key exchange');
      AssertTrue(LInfo.Cipher = sslCipherAES256GCM,
        'Connection info should expose AES256GCM cipher enum');
      AssertTrue(LInfo.Hash = sslHashSHA384,
        'Connection info should expose SHA384 hash enum');
      AssertTrue(not LInfo.IsResumed,
        'Initial SHA384 suite handshake should not report resumed session');
      AssertTrue(LInfo.SessionId <> '',
        'Connection info should expose session id after handshake');
      AssertTrue(LInfo.PeerCertificate.Subject <> '',
        'Connection info should expose peer certificate subject');

      LRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
      AssertTrue(LRead = 2, 'Client should read the OK payload');
      AssertTrue((LBuffer[0] = Ord('O')) and (LBuffer[1] = Ord('K')),
        'Client should receive OK payload');
    finally
      CloseSocket(LSock);
    end;

    LServer.WaitFor;
    AssertTrue(LServer.Accepted, 'Server should accept SHA384 suite connection');
    AssertTrue(LServer.ErrorText = '', 'Server should not report error: ' + LServer.ErrorText);
    AssertTrue(LServer.CipherName = SHA384_SUITE,
      'Server should negotiate TLS_AES_256_GCM_SHA384');
  finally
    LServer.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal local SHA384 suite roundtrip...');
  TestLocalSHA384SuiteRoundtrip;
  WriteLn('✅ FreePascal local SHA384 suite roundtrip checks passed');
end.
