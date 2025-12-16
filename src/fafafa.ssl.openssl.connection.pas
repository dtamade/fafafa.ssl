{
  fafafa.ssl.openssl.connection - OpenSSL 连接实现
  
  版本: 1.0 (简化版，核心功能完整)
  创建: 2025-11-02
}

unit fafafa.ssl.openssl.connection;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,  // 新增：类型化异常
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.certificate,
  fafafa.ssl.openssl.session,
  fafafa.ssl.logging;

type
  TOpenSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: ISSLContext;
    FSocket: THandle;
    FStream: TStream;
    FSSL: PSSL;
    FBioRead: PBIO;
    FBioWrite: PBIO;
    FConnected: Boolean;
    FBlocking: Boolean;
    FTimeout: Integer;
    function HasStreamTransport: Boolean;
    function PumpStreamToBIO: Integer;
    function PumpBIOToStream: Integer;
    function InternalHandshake(aIsClient: Boolean): Boolean;
  public
    constructor Create(aContext: ISSLContext; aSocket: THandle); overload;
    constructor Create(aContext: ISSLContext; aStream: TStream); overload;
    destructor Destroy; override;
    
    function Connect: Boolean;
    function Accept: Boolean;
    function Shutdown: Boolean;
    procedure Close;
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    function ReadString(out aStr: string): Boolean;
    function WriteString(const aStr: string): Boolean;
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(aRet: Integer): TSSLErrorCode;
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    function GetSession: ISSLSession;
    procedure SetSession(aSession: ISSLSession);
    function IsSessionReused: Boolean;
    function GetSelectedALPNProtocol: string;
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    procedure SetTimeout(aTimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(aBlocking: Boolean);
    function GetBlocking: Boolean;
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

implementation

constructor TOpenSSLConnection.Create(aContext: ISSLContext; aSocket: THandle);
var
  Ctx: PSSL_CTX;
begin
  inherited Create;
  FContext := aContext;
  FSocket := aSocket;
  FStream := nil;
  FBioRead := nil;
  FBioWrite := nil;
  FConnected := False;
  FBlocking := True;
  FTimeout := 30000;
  
  Ctx := PSSL_CTX(aContext.GetNativeHandle);
  if Ctx = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Invalid SSL context (GetNativeHandle returned nil)',
      sslErrInvalidParam,
      'TOpenSSLConnection.Create'
    );
  
  FSSL := SSL_new(Ctx);
  if FSSL = nil then
    RaiseSSLInitError(
      'Failed to create SSL object',
      'TOpenSSLConnection.Create'
    );
  
  // Apply SNI if configured
  if (aContext.GetServerName <> '') and Assigned(SSL_set_tlsext_host_name) then
    SSL_set_tlsext_host_name(FSSL, PAnsiChar(aContext.GetServerName));
    
  SSL_set_fd(FSSL, aSocket);
end;

constructor TOpenSSLConnection.Create(aContext: ISSLContext; aStream: TStream);
var
  Ctx: PSSL_CTX;
begin
  inherited Create;
  FContext := aContext;
  FSocket := THandle(-1);
  FStream := aStream;
  FBioRead := nil;
  FBioWrite := nil;
  FConnected := False;
  FBlocking := True;
  FTimeout := 30000;
  
  Ctx := PSSL_CTX(aContext.GetNativeHandle);
  if Ctx = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Invalid SSL context (GetNativeHandle returned nil)',
      sslErrInvalidParam,
      'TOpenSSLConnection.Create'
    );
  
  FSSL := SSL_new(Ctx);
  if FSSL = nil then
    RaiseSSLInitError(
      'Failed to create SSL object',
      'TOpenSSLConnection.Create'
    );

  // Apply SNI if configured
  if (aContext.GetServerName <> '') and Assigned(SSL_set_tlsext_host_name) then
    SSL_set_tlsext_host_name(FSSL, PAnsiChar(aContext.GetServerName));

  // Ensure BIO API is available
  if not IsOpenSSLBIOLoaded then
    LoadOpenSSLBIO;

  if (not Assigned(BIO_new)) or (not Assigned(BIO_s_mem)) or
     (not Assigned(SSL_set_bio)) then
    raise ESSLInitializationException.CreateWithContext(
      'OpenSSL BIO API not available (functions not loaded)',
      sslErrFunctionNotFound,
      'TOpenSSLConnection.Create'
    );

  // Create separate memory BIOs for incoming and outgoing encrypted data
  FBioRead := BIO_new(BIO_s_mem());
  if FBioRead = nil then
    raise ESSLResourceException.CreateWithContext(
      'Failed to create read BIO for TLS connection',
      sslErrMemory,
      'TOpenSSLConnection.Create'
    );

  FBioWrite := BIO_new(BIO_s_mem());
  if FBioWrite = nil then
  begin
    BIO_free(FBioRead);
    FBioRead := nil;
    raise ESSLResourceException.CreateWithContext(
      'Failed to create write BIO for TLS connection',
      sslErrMemory,
      'TOpenSSLConnection.Create'
    );
  end;

  // Attach BIOs to SSL; SSL takes ownership and will free them in SSL_free
  SSL_set_bio(FSSL, FBioRead, FBioWrite);
end;

destructor TOpenSSLConnection.Destroy;
begin
  if FConnected then
    Shutdown;
  if FSSL <> nil then
    SSL_free(FSSL);
  inherited Destroy;
end;

function TOpenSSLConnection.Connect: Boolean;
var
  Ret: Integer;
begin
  if FSSL = nil then Exit(False);

  // For stream-based transport, run an internal blocking handshake
  if HasStreamTransport then
  begin
    Result := InternalHandshake(True);
    Exit;
  end;
  
  Ret := SSL_connect(FSSL);
  FConnected := (Ret = 1);
  Result := FConnected;
end;

function TOpenSSLConnection.Accept: Boolean;
var
  Ret: Integer;
begin
  if FSSL = nil then Exit(False);
  
  // For stream-based transport, run an internal blocking handshake
  if HasStreamTransport then
  begin
    Result := InternalHandshake(False);
    Exit;
  end;
  
  Ret := SSL_accept(FSSL);
  FConnected := (Ret = 1);
  Result := FConnected;
end;

function TOpenSSLConnection.Shutdown: Boolean;
begin
  if FSSL <> nil then
    SSL_shutdown(FSSL);
  FConnected := False;
  Result := True;
end;

procedure TOpenSSLConnection.Close;
begin
  Shutdown;
end;

function TOpenSSLConnection.DoHandshake: TSSLHandshakeState;
var
  LRole: string;
begin
  if FContext.GetContextType = sslCtxClient then
    LRole := 'Client'
  else
    LRole := 'Server';
    
  TSecurityLog.Info('OpenSSL', Format('Starting handshake (%s)', [LRole]));

  if FContext.GetContextType = sslCtxClient then
  begin
    if Connect then
    begin
      Result := sslHsCompleted;
      TSecurityLog.Info('OpenSSL', Format('Handshake completed (%s). Cipher: %s', [LRole, GetCipherName]));
    end
    else
    begin
      Result := sslHsFailed;
      TSecurityLog.Error('OpenSSL', Format('Handshake failed (%s)', [LRole]));
    end;
  end
  else
  begin
    if Accept then
    begin
      Result := sslHsCompleted;
      TSecurityLog.Info('OpenSSL', Format('Handshake completed (%s). Cipher: %s', [LRole, GetCipherName]));
    end
    else
    begin
      Result := sslHsFailed;
      TSecurityLog.Error('OpenSSL', Format('Handshake failed (%s)', [LRole]));
    end;
  end;
end;

function TOpenSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FConnected and (FSSL <> nil) and (SSL_is_init_finished(FSSL) = 1);
end;

function TOpenSSLConnection.Renegotiate: Boolean;
var
  Ret: Integer;
begin
  Result := False;
  
  if (FSSL = nil) or not FConnected then
    Exit;
  
  if not Assigned(SSL_renegotiate) then
    Exit;
  
  // 发起重协商
  Ret := SSL_renegotiate(FSSL);
  if Ret <> 1 then
    Exit;
  
  // 执行握手以完成重协商
  Ret := SSL_do_handshake(FSSL);
  Result := (Ret = 1);
end;

function TOpenSSLConnection.Read(var aBuffer; aCount: Integer): Integer;
var
  LRet, LErr: Integer;
begin
  Result := -1;
  if FSSL = nil then Exit;

  // Stream-based blocking read using BIO <-> TStream bridge
  if HasStreamTransport then
  begin
    // Ensure handshake completed
    if not FConnected then
    begin
      if not InternalHandshake(FContext.GetContextType = sslCtxClient) then
        Exit;
    end;

    while True do
    begin
      LRet := SSL_read(FSSL, @aBuffer, aCount);
      if LRet > 0 then
      begin
        Result := LRet;
        Exit;
      end;

      LErr := SSL_get_error(FSSL, LRet);
      case LErr of
        SSL_ERROR_WANT_READ:
          begin
            PumpBIOToStream;
            if PumpStreamToBIO <= 0 then
            begin
              Result := -1;
              Exit;
            end;
          end;
        SSL_ERROR_WANT_WRITE:
          begin
            if PumpBIOToStream <= 0 then
            begin
              Result := -1;
              Exit;
            end;
          end;
        SSL_ERROR_ZERO_RETURN:
          begin
            // Clean shutdown from peer
            PumpBIOToStream;
            Result := 0;
            Exit;
          end;
      else
        Result := -1;
        Exit;
      end;
    end;
  end
  else
  begin
    if not FConnected then Exit(-1);
    Result := SSL_read(FSSL, @aBuffer, aCount);
  end;
end;

function TOpenSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
var
  LRet, LErr: Integer;
begin
  Result := -1;
  if FSSL = nil then Exit;

  // Stream-based blocking write using BIO <-> TStream bridge
  if HasStreamTransport then
  begin
    // Ensure handshake completed
    if not FConnected then
    begin
      if not InternalHandshake(FContext.GetContextType = sslCtxClient) then
        Exit;
    end;

    while True do
    begin
      LRet := SSL_write(FSSL, @aBuffer, aCount);
      if LRet > 0 then
      begin
        // Flush any pending encrypted data to the underlying stream
        PumpBIOToStream;
        Result := LRet;
        Exit;
      end;

      LErr := SSL_get_error(FSSL, LRet);
      case LErr of
        SSL_ERROR_WANT_READ:
          begin
            // Peer expects us to read more encrypted data before continuing
            PumpBIOToStream;
            if PumpStreamToBIO <= 0 then
            begin
              Result := -1;
              Exit;
            end;
          end;
        SSL_ERROR_WANT_WRITE:
          begin
            if PumpBIOToStream <= 0 then
            begin
              Result := -1;
              Exit;
            end;
          end;
        SSL_ERROR_ZERO_RETURN:
          begin
            PumpBIOToStream;
            Result := 0;
            Exit;
          end;
      else
        Result := -1;
        Exit;
      end;
    end;
  end
  else
  begin
    if not FConnected then Exit(-1);
    Result := SSL_write(FSSL, @aBuffer, aCount);
  end;
end;

function TOpenSSLConnection.ReadString(out aStr: string): Boolean;
var
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, SizeOf(Buffer));
  Result := BytesRead > 0;
  if Result then
    SetString(aStr, PChar(@Buffer[0]), BytesRead);
end;

function TOpenSSLConnection.WriteString(const aStr: string): Boolean;
begin
  Result := Write(PChar(aStr)^, Length(aStr)) = Length(aStr);
end;

function TOpenSSLConnection.WantRead: Boolean;
begin
  if FSSL = nil then Exit(False);
  Result := (SSL_want(FSSL) = SSL_READING);
end;

function TOpenSSLConnection.WantWrite: Boolean;
begin
  if FSSL = nil then Exit(False);
  Result := (SSL_want(FSSL) = SSL_WRITING);
end;

function TOpenSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
var
  Err: Integer;
begin
  if FSSL = nil then
  begin
    Result := sslErrNone;
    Exit;
  end;
  
  // 与 WinSSL 路径保持一致：非负返回值一律视为无错误
  if aRet >= 0 then
  begin
    Result := sslErrNone;
    Exit;
  end;
  
  if not Assigned(SSL_get_error) then
  begin
    Result := sslErrOther;
    Exit;
  end;
  
  Err := SSL_get_error(FSSL, aRet);
  case Err of
    SSL_ERROR_NONE: Result := sslErrNone;
    SSL_ERROR_WANT_READ: Result := sslErrWantRead;
    SSL_ERROR_WANT_WRITE: Result := sslErrWantWrite;
  else
    Result := sslErrOther;
  end;
end;

function TOpenSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
var
  Cipher: PSSL_CIPHER;
  CipherName: PAnsiChar;
  AlgBits: Integer;
  ServerNamePtr: PAnsiChar;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if FSSL = nil then
    Exit;
  
  // Protocol version
  Result.ProtocolVersion := GetProtocolVersion;
  
  // Cipher suite information
  Cipher := SSL_get_current_cipher(FSSL);
  if Cipher <> nil then
  begin
    // Cipher suite name
    CipherName := SSL_CIPHER_get_name(Cipher);
    if CipherName <> nil then
      Result.CipherSuite := string(CipherName);
    
    // Key size
    if Assigned(SSL_CIPHER_get_bits) then
    begin
      AlgBits := 0;
      Result.KeySize := SSL_CIPHER_get_bits(Cipher, @AlgBits);
    end;
  end;
  
  // Session resumed flag
  Result.IsResumed := IsSessionReused;
  
  // Server name (SNI)
  if Assigned(SSL_get_servername) then
  begin
    ServerNamePtr := SSL_get_servername(FSSL, TLSEXT_NAMETYPE_host_name);
    if ServerNamePtr <> nil then
      Result.ServerName := string(ServerNamePtr);
  end;
  
  // ALPN protocol
  Result.ALPNProtocol := GetSelectedALPNProtocol;
end;

function TOpenSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
var
  Ver: Integer;
begin
  Result := sslProtocolTLS12;
  if FSSL = nil then Exit;
  
  Ver := SSL_version(FSSL);
  case Ver of
    TLS1_VERSION: Result := sslProtocolTLS10;
    TLS1_1_VERSION: Result := sslProtocolTLS11;
    TLS1_2_VERSION: Result := sslProtocolTLS12;
    TLS1_3_VERSION: Result := sslProtocolTLS13;
  end;
end;

function TOpenSSLConnection.GetCipherName: string;
var
  Cipher: PSSL_CIPHER;
  Name: PAnsiChar;
begin
  Result := '';
  if FSSL = nil then Exit;
  
  Cipher := SSL_get_current_cipher(FSSL);
  if Cipher <> nil then
  begin
    Name := SSL_CIPHER_get_name(Cipher);
    if Name <> nil then
      Result := string(Name);
  end;
end;

function TOpenSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  X509Cert: PX509;
begin
  Result := nil;
  
  if FSSL = nil then
    Exit;
  
  X509Cert := SSL_get_peer_certificate(FSSL);
  if X509Cert = nil then
    Exit;
  
  // 创建证书对象（SSL_get_peer_certificate已增加引用计数）
  Result := TOpenSSLCertificate.Create(X509Cert, True);
end;

function TOpenSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
var
  Chain: PSTACK_OF_X509;
  Count, I: Integer;
  X509Cert: PX509;
begin
  SetLength(Result, 0);
  
  if FSSL = nil then
    Exit;
  
  Chain := SSL_get_peer_cert_chain(FSSL);
  if Chain = nil then
    Exit;
  
  Count := sk_X509_num(Chain);
  if Count <= 0 then
    Exit;
  
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    X509Cert := sk_X509_value(Chain, I);
    if X509Cert <> nil then
    begin
      // sk_X509_value不增加引用计数，需要手动增加
      X509_up_ref(X509Cert);
      Result[I] := TOpenSSLCertificate.Create(X509Cert, True);
    end;
  end;
end;

function TOpenSSLConnection.GetVerifyResult: Integer;
begin
  if FSSL = nil then Exit(-1);
  Result := SSL_get_verify_result(FSSL);
end;

function TOpenSSLConnection.GetVerifyResultString: string;
var
  Res: Integer;
begin
  Res := GetVerifyResult;
  if Res = X509_V_OK then
    Result := 'OK'
  else
    Result := Format('Error: %d', [Res]);
end;

function TOpenSSLConnection.GetSession: ISSLSession;
var
  Sess: PSSL_SESSION;
begin
  Result := nil;
  
  if FSSL = nil then
    Exit;
  
  if not Assigned(SSL_get1_session) then
    Exit;
  
  // 使用 SSL_get1_session（增加引用计数）
  Sess := SSL_get1_session(FSSL);
  if Sess = nil then
    Exit;
  
  Result := TOpenSSLSession.Create(Sess, True);
end;

procedure TOpenSSLConnection.SetSession(aSession: ISSLSession);
var
  Sess: PSSL_SESSION;
begin
  if (FSSL = nil) or (aSession = nil) then
    Exit;
  
  if not Assigned(SSL_set_session) then
    Exit;
  
  Sess := PSSL_SESSION(aSession.GetNativeHandle);
  if Sess = nil then
    Exit;
  
  SSL_set_session(FSSL, Sess);
end;

function TOpenSSLConnection.IsSessionReused: Boolean;
begin
  if FSSL = nil then Exit(False);
  Result := (SSL_session_reused(FSSL) = 1);
end;

function TOpenSSLConnection.GetSelectedALPNProtocol: string;
var
  Data: PByte;
  Len: Cardinal;
begin
  Result := '';
  if (FSSL = nil) or not Assigned(SSL_get0_alpn_selected) then Exit;
  
  SSL_get0_alpn_selected(FSSL, @Data, @Len);
  if (Data <> nil) and (Len > 0) then
    SetString(Result, PAnsiChar(Data), Len);
end;

function TOpenSSLConnection.IsConnected: Boolean;
begin
  Result := FConnected;
end;

function TOpenSSLConnection.GetState: string;
begin
  if FSSL = nil then
    Exit('not_initialized');
  if not Assigned(SSL_state_string) then
    Exit('unknown');
  Result := string(SSL_state_string(FSSL));
end;

function TOpenSSLConnection.GetStateString: string;
begin
  if FSSL = nil then
    Exit('Not initialized');
  if not Assigned(SSL_state_string_long) then
    Exit('Unknown state');
  Result := string(SSL_state_string_long(FSSL));
end;

procedure TOpenSSLConnection.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TOpenSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TOpenSSLConnection.SetBlocking(aBlocking: Boolean);
begin
  FBlocking := aBlocking;
end;

function TOpenSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TOpenSSLConnection.GetNativeHandle: Pointer;
begin
  Result := FSSL;
end;

function TOpenSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext;
end;

function TOpenSSLConnection.HasStreamTransport: Boolean;
begin
  Result := (FStream <> nil) and (FSocket = THandle(-1));
end;

function TOpenSSLConnection.PumpStreamToBIO: Integer;
const
  BUF_SIZE = 8192;
var
  LBuffer: array[0..BUF_SIZE - 1] of Byte;
  LRead, LOffset, LWritten: Integer;
begin
  Result := 0;

  if (not HasStreamTransport) or (FBioRead = nil) or
     (not Assigned(BIO_write)) or (FStream = nil) then
    Exit;

  // Blocking read from underlying stream (encrypted data from peer)
  LRead := FStream.Read(LBuffer[0], BUF_SIZE);
  if LRead <= 0 then
    Exit;

  LOffset := 0;
  while LOffset < LRead do
  begin
    LWritten := BIO_write(FBioRead, @LBuffer[LOffset], LRead - LOffset);
    if LWritten <= 0 then
      Break;
    Inc(LOffset, LWritten);
  end;

  Result := LOffset;
end;

function TOpenSSLConnection.PumpBIOToStream: Integer;
const
  BUF_SIZE = 8192;
var
  LBuffer: array[0..BUF_SIZE - 1] of Byte;
  LPending, LToRead, LRead: Integer;
begin
  Result := 0;

  if (not HasStreamTransport) or (FBioWrite = nil) or
     (not Assigned(BIO_pending)) or (not Assigned(BIO_read)) or
     (FStream = nil) then
    Exit;

  // Drain all pending encrypted data from SSL's write BIO to the underlying stream
  while True do
  begin
    LPending := BIO_pending(FBioWrite);
    if LPending <= 0 then
      Break;

    if LPending > BUF_SIZE then
      LToRead := BUF_SIZE
    else
      LToRead := LPending;

    LRead := BIO_read(FBioWrite, @LBuffer[0], LToRead);
    if LRead <= 0 then
      Break;

    FStream.WriteBuffer(LBuffer[0], LRead);
    Inc(Result, LRead);
  end;
end;

function TOpenSSLConnection.InternalHandshake(aIsClient: Boolean): Boolean;
var
  LRet, LErr: Integer;
begin
  Result := False;

  if (FSSL = nil) or (not HasStreamTransport) then
    Exit;

  // Set initial handshake state explicitly for stream-based connections
  if aIsClient then
  begin
    if Assigned(SSL_set_connect_state) then
      SSL_set_connect_state(FSSL);
  end
  else
  begin
    if Assigned(SSL_set_accept_state) then
      SSL_set_accept_state(FSSL);
  end;

  while True do
  begin
    LRet := SSL_do_handshake(FSSL);
    if LRet = 1 then
    begin
      FConnected := True;
      Result := True;
      // Flush any handshake data still buffered
      PumpBIOToStream;
      Exit;
    end;

    LErr := SSL_get_error(FSSL, LRet);
    case LErr of
      SSL_ERROR_WANT_READ:
        begin
          PumpBIOToStream;
          if PumpStreamToBIO <= 0 then
            Exit(False);
        end;
      SSL_ERROR_WANT_WRITE:
        begin
          if PumpBIOToStream <= 0 then
            Exit(False);
        end;
      SSL_ERROR_ZERO_RETURN:
        begin
          PumpBIOToStream;
          Exit(False);
        end;
    else
      PumpBIOToStream;
      Exit(False);
    end;
  end;
end;

end.
