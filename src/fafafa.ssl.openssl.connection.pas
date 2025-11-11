{
  fafafa.ssl.openssl.connection - OpenSSL 连接实现
  
  版本: 1.0 (简化版，核心功能完整)
  创建: 2025-11-02
}

unit fafafa.ssl.openssl.connection;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Sockets,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.certificate,
  fafafa.ssl.openssl.session;

type
  TOpenSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: ISSLContext;
    FSocket: THandle;
    FSSL: PSSL;
    FConnected: Boolean;
    FBlocking: Boolean;
    FTimeout: Integer;
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
  FConnected := False;
  FBlocking := True;
  FTimeout := 30000;
  
  Ctx := PSSL_CTX(aContext.GetNativeHandle);
  if Ctx = nil then
    raise Exception.Create('Invalid SSL context');
  
  FSSL := SSL_new(Ctx);
  if FSSL = nil then
    raise Exception.Create('Failed to create SSL');
  
  SSL_set_fd(FSSL, aSocket);
end;

constructor TOpenSSLConnection.Create(aContext: ISSLContext; aStream: TStream);
begin
  // TODO: Stream support
  raise Exception.Create('Stream-based connections not yet implemented');
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
  
  Ret := SSL_connect(FSSL);
  FConnected := (Ret = 1);
  Result := FConnected;
end;

function TOpenSSLConnection.Accept: Boolean;
var
  Ret: Integer;
begin
  if FSSL = nil then Exit(False);
  
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
begin
  if FContext.GetContextType = sslCtxClient then
  begin
    if Connect then
      Result := sslHsCompleted
    else
      Result := sslHsFailed;
  end
  else
  begin
    if Accept then
      Result := sslHsCompleted
    else
      Result := sslHsFailed;
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
begin
  if (FSSL = nil) or not FConnected then Exit(-1);
  Result := SSL_read(FSSL, @aBuffer, aCount);
end;

function TOpenSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
begin
  if (FSSL = nil) or not FConnected then Exit(-1);
  Result := SSL_write(FSSL, @aBuffer, aCount);
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
  if FSSL = nil then Exit(sslErrNone);
  Err := SSL_get_error(FSSL, aRet);
  case Err of
    SSL_ERROR_NONE: Result := sslErrNone;
    SSL_ERROR_WANT_READ: Result := sslErrWantRead;
    SSL_ERROR_WANT_WRITE: Result := sslErrWantWrite;
    else Result := sslErrOther;
  end;
end;

function TOpenSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  // TODO: Fill connection info
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
  if FSSL = nil then Exit('not_initialized');
  Result := string(SSL_state_string(FSSL));
end;

function TOpenSSLConnection.GetStateString: string;
begin
  if FSSL = nil then Exit('Not initialized');
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

end.
