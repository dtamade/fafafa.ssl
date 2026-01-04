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
  fafafa.ssl.errors,
  fafafa.ssl.ocsp,
  fafafa.ssl.x509,
  fafafa.ssl.openssl.errors,  // Phase 3.1 - OpenSSL-specific error handling
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.x509.chain,
  fafafa.ssl.openssl.certificate,
  fafafa.ssl.openssl.session,
  fafafa.ssl.logging;

type
  TOpenSSLConnection = class(TInterfacedObject, ISSLConnection, ISSLClientConnection)
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
    FServerName: string;
    function HasStreamTransport: Boolean;
    function PumpStreamToBIO: Integer;
    function PumpBIOToStream: Integer;
    function InternalHandshake(AIsClient: Boolean): Boolean;
    function ValidatePostHandshake(AIsClient: Boolean): Boolean;
  public
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;
    destructor Destroy; override;

    { ISSLClientConnection }
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;

    { ISSLConnection }
    function Connect: Boolean;
    function Accept: Boolean;
    function Shutdown: Boolean;
    procedure Close;
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    function Read(var ABuffer; ACount: Integer): Integer;
    function Write(const ABuffer; ACount: Integer): Integer;
    function ReadString(out AStr: string): Boolean;
    function WriteString(const AStr: string): Boolean;
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(ARet: Integer): TSSLErrorCode;
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    function GetSession: ISSLSession;
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;
    function GetSelectedALPNProtocol: string;
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
    function GetBlocking: Boolean;
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

implementation

const
  // P3-2: 统一定义缓冲区大小常量
  SSL_STRING_BUFFER_SIZE = 4096;   // ReadString 缓冲区大小
  SSL_IO_BUFFER_SIZE = 8192;       // PumpStreamToBIO/PumpBIOToStream 缓冲区大小

constructor TOpenSSLConnection.Create(AContext: ISSLContext; ASocket: THandle);
var
  Ctx: PSSL_CTX;
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FStream := nil;
  FBioRead := nil;
  FBioWrite := nil;
  FConnected := False;
  FBlocking := True;
  FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;  // P3-1: 使用常量替代魔法数字

  Ctx := PSSL_CTX(AContext.GetNativeHandle);
  if Ctx = nil then
    RaiseInvalidParameter('SSL context (GetNativeHandle returned nil)');

  FSSL := SSL_new(Ctx);
  if FSSL = nil then
    RaiseSSLInitError(
      'Failed to create SSL object',
      'TOpenSSLConnection.Create'
    );

  // Initialize per-connection server name from context default (backward compatibility)
  FServerName := '';
  if (AContext.GetServerName <> '') then
    SetServerName(AContext.GetServerName);

  SSL_set_fd(FSSL, ASocket);
end;

constructor TOpenSSLConnection.Create(AContext: ISSLContext; AStream: TStream);
var
  Ctx: PSSL_CTX;
begin
  inherited Create;
  FContext := AContext;
  FSocket := THandle(-1);
  FStream := AStream;
  FBioRead := nil;
  FBioWrite := nil;
  FConnected := False;
  FBlocking := True;
  FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;  // P3-1: 使用常量替代魔法数字
  
  Ctx := PSSL_CTX(AContext.GetNativeHandle);
  if Ctx = nil then
    RaiseInvalidParameter('SSL context (GetNativeHandle returned nil)');
  
  FSSL := SSL_new(Ctx);
  if FSSL = nil then
    RaiseSSLInitError(
      'Failed to create SSL object',
      'TOpenSSLConnection.Create'
    );

  // Initialize per-connection server name from context default (backward compatibility)
  FServerName := '';
  if (AContext.GetServerName <> '') then
    SetServerName(AContext.GetServerName);

  // Ensure BIO API is available
  if not IsOpenSSLBIOLoaded then
    LoadOpenSSLBIO;

  if (not Assigned(BIO_new)) or (not Assigned(BIO_s_mem)) or
    (not Assigned(SSL_set_bio)) then
    RaiseFunctionNotAvailable('OpenSSL BIO API (BIO_new/BIO_s_mem/SSL_set_bio)');

  // Create separate memory BIOs for incoming and outgoing encrypted data
  FBioRead := BIO_new(BIO_s_mem());
  if FBioRead = nil then
    RaiseMemoryError('create read BIO');

  FBioWrite := BIO_new(BIO_s_mem());
  if FBioWrite = nil then
  begin
    BIO_free(FBioRead);
    FBioRead := nil;
    RaiseMemoryError('create write BIO');
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

procedure TOpenSSLConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;

  // Apply SNI on the underlying SSL handle (must be set before handshake)
  if (FServerName <> '') and Assigned(SSL_set_tlsext_host_name) and Assigned(FSSL) then
    SSL_set_tlsext_host_name(FSSL, PAnsiChar(AnsiString(FServerName)));
end;

function TOpenSSLConnection.GetServerName: string;
begin
  Result := FServerName;
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
  if FConnected then
  begin
    // Strategy A: fail closed if validation fails
    if not ValidatePostHandshake(True) then
    begin
      FConnected := False;
      Result := False;
      Exit;
    end;
  end;
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
  if FConnected then
  begin
    // Strategy A: fail closed if validation fails
    if not ValidatePostHandshake(False) then
    begin
      FConnected := False;
      Result := False;
      Exit;
    end;
  end;
  Result := FConnected;
end;

function TOpenSSLConnection.ValidatePostHandshake(AIsClient: Boolean): Boolean;
var
  VerifyModes: TSSLVerifyModes;
  VerifyFlags: TSSLCertVerifyFlags;
  RequirePeerCert: Boolean;
  PeerCert: ISSLCertificate;
  PeerX509: PX509;
  VerifyRes: Integer;
  Host: string;
  HostNormalized: string;
  HostA: AnsiString;
  IsIP: Boolean;
  OCSPUrl: string;
  IssuerX509: PX509;
  IssuerNeedsFree: Boolean;
  VerifyStore: PX509_STORE;
  OCSPStatus: Integer;
  OCSPTimeoutSec: Integer;
  PeerDER: TBytes;
  ParsedCert: TX509Certificate;
  PeerChain: PSTACK_OF_X509;
  StoreCtx: PX509_STORE_CTX;
  VerifiedChain: PSTACK_OF_X509;

  function NormalizeHostForVerify(const S: string): string;
  var
    LHost: string;
    P, PEnd: SizeInt;
    PortPart: string;
    I: Integer;
  begin
    LHost := Trim(S);

    // Strip IPv6 brackets: [::1]
    if (LHost <> '') and (LHost[1] = '[') then
    begin
      PEnd := Pos(']', LHost);
      if PEnd > 0 then
        LHost := Copy(LHost, 2, PEnd - 2);
    end;

    // Strip zone id: fe80::1%eth0
    P := Pos('%', LHost);
    if P > 0 then
      LHost := Copy(LHost, 1, P - 1);

    // Strip port for the host:port case (not valid for plain IPv6 without brackets)
    if (Pos(':', LHost) > 0) and (Pos(':', LHost) = LastDelimiter(':', LHost)) then
    begin
      P := Pos(':', LHost);
      PortPart := Copy(LHost, P + 1, Length(LHost) - P);
      if PortPart <> '' then
      begin
        for I := 1 to Length(PortPart) do
          if not (PortPart[I] in ['0'..'9']) then
          begin
            PortPart := '';
            Break;
          end;
        if PortPart <> '' then
          LHost := Copy(LHost, 1, P - 1);
      end;
    end;

    Result := LHost;
  end;

  function IsValidIPv4(const S: string): Boolean;
  var
    Parts: TStringArray;
    Part: string;
    Value: Integer;
    I: Integer;
  begin
    Result := False;
    Parts := S.Split(['.']);
    if Length(Parts) <> 4 then
      Exit;

    for Part in Parts do
    begin
      if Part = '' then
        Exit;

      for I := 1 to Length(Part) do
        if not (Part[I] in ['0'..'9']) then
          Exit;

      if not TryStrToInt(Part, Value) then
        Exit;
      if (Value < 0) or (Value > 255) then
        Exit;
    end;

    Result := True;
  end;

begin
  Result := True;

  if (FSSL = nil) or (FContext = nil) then
    Exit(False);

  VerifyModes := FContext.GetVerifyMode;
  if not (sslVerifyPeer in VerifyModes) then
    Exit(True);

  if not Assigned(SSL_get_verify_result) then
    Exit(False);

  // Decide whether a peer certificate is required (align with WinSSL logic)
  RequirePeerCert := AIsClient or (sslVerifyFailIfNoPeerCert in VerifyModes);

  // Ensure X509 helpers are available before we materialize certificates
  if not Assigned(X509_free) then
    LoadOpenSSLX509;

  PeerCert := GetPeerCertificate;
  if PeerCert = nil then
  begin
    if RequirePeerCert then
    begin
      if Assigned(SSL_set_verify_result) then
        SSL_set_verify_result(FSSL, X509_V_ERR_APPLICATION_VERIFICATION);
      Result := False;
    end;
    Exit;
  end;

  PeerX509 := PX509(PeerCert.GetNativeHandle);
  if PeerX509 = nil then
  begin
    if Assigned(SSL_set_verify_result) then
      SSL_set_verify_result(FSSL, X509_V_ERR_APPLICATION_VERIFICATION);
    Exit(False);
  end;

  VerifyRes := SSL_get_verify_result(FSSL);
  if VerifyRes <> X509_V_OK then
    Exit(False);

  VerifyFlags := FContext.GetCertVerifyFlags;

  // OCSP revocation checking (fail closed when requested)
  if sslCertVerifyCheckOCSP in VerifyFlags then
  begin
    IssuerX509 := nil;
    IssuerNeedsFree := False;

    try
      // Ensure OCSP APIs are loaded
      if not TOpenSSLLoader.IsModuleLoaded(osmOCSP) then
        LoadOpenSSLOCSP(GetCryptoLibHandle);

      if not TOpenSSLLoader.IsModuleLoaded(osmOCSP) then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_OCSP_VERIFY_FAILED);
        Exit(False);
      end;

      // Extract responder URL from certificate AIA (pure-pascal parser)
      OCSPUrl := '';
      try
        PeerDER := PeerCert.SaveToDER;
        if Length(PeerDER) > 0 then
        begin
          ParsedCert := TX509Certificate.Create;
          try
            ParsedCert.LoadFromDER(PeerDER);
            OCSPUrl := GetOCSPURLFromCertificate(ParsedCert);
          finally
            ParsedCert.Free;
          end;
        end;
      except
        OCSPUrl := '';
      end;

      if OCSPUrl = '' then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_OCSP_VERIFY_NEEDED);
        Exit(False);
      end;

      // Try to obtain issuer cert from the peer-provided chain first
      PeerChain := nil;
      if Assigned(SSL_get_peer_cert_chain) then
        PeerChain := SSL_get_peer_cert_chain(FSSL);

      if PeerChain <> nil then
        IssuerX509 := FindIssuerX509InChain(PeerX509, PeerChain);

      // Fall back to verified chain building via X509_STORE if needed
      VerifyStore := nil;
      if Assigned(SSL_CTX_get_cert_store) then
        VerifyStore := SSL_CTX_get_cert_store(PSSL_CTX(FContext.GetNativeHandle));

      if (IssuerX509 = nil) and (VerifyStore <> nil) and
        Assigned(X509_STORE_CTX_new) and Assigned(X509_STORE_CTX_free) and
        Assigned(X509_STORE_CTX_init) and Assigned(X509_verify_cert) and
        Assigned(X509_STORE_CTX_get0_chain) then
      begin
        StoreCtx := X509_STORE_CTX_new();
        if StoreCtx <> nil then
        try
          PeerChain := nil;
          if Assigned(SSL_get_peer_cert_chain) then
            PeerChain := SSL_get_peer_cert_chain(FSSL);

          if X509_STORE_CTX_init(StoreCtx, VerifyStore, PeerX509, PeerChain) = 1 then
          begin
            if X509_verify_cert(StoreCtx) = 1 then
            begin
              VerifiedChain := X509_STORE_CTX_get0_chain(StoreCtx);
              if VerifiedChain <> nil then
              begin
                IssuerX509 := FindIssuerX509InChain(PeerX509, VerifiedChain);
                if (IssuerX509 <> nil) and Assigned(X509_up_ref) then
                begin
                  X509_up_ref(IssuerX509);
                  IssuerNeedsFree := True;
                end;
              end;
            end;
          end;
        finally
          X509_STORE_CTX_free(StoreCtx);
        end;
      end;

      if IssuerX509 = nil then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT);
        Exit(False);
      end;

      // Map connection timeout (ms) to OCSP timeout (seconds)
      OCSPTimeoutSec := 10;
      if FTimeout > 0 then
      begin
        OCSPTimeoutSec := FTimeout div 1000;
        if OCSPTimeoutSec <= 0 then
          OCSPTimeoutSec := 1;
      end;

      // Perform OCSP check (supports http/https responders)
      OCSPStatus := CheckCertificateStatus(PeerX509, IssuerX509, OCSPUrl, OCSPTimeoutSec, VerifyStore);
      case OCSPStatus of
        V_OCSP_CERTSTATUS_GOOD:
          ; // OK
        V_OCSP_CERTSTATUS_REVOKED:
          begin
            if Assigned(SSL_set_verify_result) then
              SSL_set_verify_result(FSSL, X509_V_ERR_CERT_REVOKED);
            Exit(False);
          end;
        V_OCSP_CERTSTATUS_UNKNOWN:
          begin
            if Assigned(SSL_set_verify_result) then
              SSL_set_verify_result(FSSL, X509_V_ERR_OCSP_CERT_UNKNOWN);
            Exit(False);
          end;
      else
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_OCSP_VERIFY_FAILED);
        Exit(False);
      end;

    finally
      if IssuerNeedsFree and (IssuerX509 <> nil) and Assigned(X509_free) then
        X509_free(IssuerX509);
    end;
  end;

  // Hostname verification: client-side only
  if AIsClient and not (sslCertVerifyIgnoreHostname in VerifyFlags) then
  begin
    Host := FServerName;
    HostNormalized := NormalizeHostForVerify(Host);

    if HostNormalized = '' then
    begin
      if Assigned(SSL_set_verify_result) then
        SSL_set_verify_result(FSSL, X509_V_ERR_HOSTNAME_MISMATCH);
      Exit(False);
    end;

    // Ensure hostname helpers are loaded
    if (not Assigned(X509_check_host)) and (not Assigned(X509_check_ip_asc)) then
      LoadOpenSSLX509;

    IsIP := IsValidIPv4(HostNormalized) or (Pos(':', HostNormalized) > 0);
    HostA := AnsiString(HostNormalized);

    if IsIP then
    begin
      if not Assigned(X509_check_ip_asc) then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_APPLICATION_VERIFICATION);
        Exit(False);
      end;

      if X509_check_ip_asc(PeerX509, PAnsiChar(HostA), 0) <> 1 then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_IP_ADDRESS_MISMATCH);
        Exit(False);
      end;
    end
    else
    begin
      if not Assigned(X509_check_host) then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_APPLICATION_VERIFICATION);
        Exit(False);
      end;

      if X509_check_host(PeerX509, PAnsiChar(HostA), Length(HostA), 0, nil) <> 1 then
      begin
        if Assigned(SSL_set_verify_result) then
          SSL_set_verify_result(FSSL, X509_V_ERR_HOSTNAME_MISMATCH);
        Exit(False);
      end;
    end;
  end;

  Result := True;
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

function TOpenSSLConnection.Read(var ABuffer; ACount: Integer): Integer;
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
      LRet := SSL_read(FSSL, @ABuffer, ACount);
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
    Result := SSL_read(FSSL, @ABuffer, ACount);
  end;
end;

function TOpenSSLConnection.Write(const ABuffer; ACount: Integer): Integer;
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
      LRet := SSL_write(FSSL, @ABuffer, ACount);
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
    Result := SSL_write(FSSL, @ABuffer, ACount);
  end;
end;

function TOpenSSLConnection.ReadString(out AStr: string): Boolean;
var
  Buffer: array[0..SSL_STRING_BUFFER_SIZE - 1] of Char;  // P3-2: 使用常量
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, SizeOf(Buffer));
  Result := BytesRead > 0;
  if Result then
    SetString(AStr, PChar(@Buffer[0]), BytesRead);
end;

function TOpenSSLConnection.WriteString(const AStr: string): Boolean;
begin
  Result := Write(PChar(AStr)^, Length(AStr)) = Length(AStr);
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

function TOpenSSLConnection.GetError(ARet: Integer): TSSLErrorCode;
var
  Err: Integer;
begin
  if FSSL = nil then
  begin
    Result := sslErrNone;
    Exit;
  end;
  
  // 与 WinSSL 路径保持一致：非负返回值一律视为无错误
  if ARet >= 0 then
  begin
    Result := sslErrNone;
    Exit;
  end;
  
  if not Assigned(SSL_get_error) then
  begin
    Result := sslErrOther;
    Exit;
  end;
  
  Err := SSL_get_error(FSSL, ARet);
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
  // P3-19: 添加完整的协议版本处理以保持与 WinSSL 一致
  case Ver of
    SSL2_VERSION: Result := sslProtocolSSL2;
    SSL3_VERSION: Result := sslProtocolSSL3;
    TLS1_VERSION: Result := sslProtocolTLS10;
    TLS1_1_VERSION: Result := sslProtocolTLS11;
    TLS1_2_VERSION: Result := sslProtocolTLS12;
    TLS1_3_VERSION: Result := sslProtocolTLS13;
  else
    Result := sslProtocolTLS12;  // 未知版本时使用安全默认值
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
  ErrStr: PAnsiChar;
begin
  Res := GetVerifyResult;
  if Res = X509_V_OK then
  begin
    Result := 'OK';
    Exit;
  end;

  if Res < 0 then
  begin
    Result := Format('Error: %d', [Res]);
    Exit;
  end;

  ErrStr := nil;
  if Assigned(X509_verify_cert_error_string) then
    ErrStr := X509_verify_cert_error_string(Res);

  if ErrStr <> nil then
    Result := string(ErrStr)
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

procedure TOpenSSLConnection.SetSession(ASession: ISSLSession);
var
  Sess: PSSL_SESSION;
begin
  if (FSSL = nil) or (ASession = nil) then
    Exit;
  
  if not Assigned(SSL_set_session) then
    Exit;
  
  Sess := PSSL_SESSION(ASession.GetNativeHandle);
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

procedure TOpenSSLConnection.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TOpenSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TOpenSSLConnection.SetBlocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
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
var
  LBuffer: array[0..SSL_IO_BUFFER_SIZE - 1] of Byte;  // P3-2: 使用模块级常量
  LRead, LOffset, LWritten: Integer;
begin
  Result := 0;

  if (not HasStreamTransport) or (FBioRead = nil) or
    (not Assigned(BIO_write)) or (FStream = nil) then
    Exit;

  // Blocking read from underlying stream (encrypted data from peer)
  LRead := FStream.Read(LBuffer[0], SSL_IO_BUFFER_SIZE);
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
var
  LBuffer: array[0..SSL_IO_BUFFER_SIZE - 1] of Byte;  // P3-2: 使用模块级常量
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

    if LPending > SSL_IO_BUFFER_SIZE then
      LToRead := SSL_IO_BUFFER_SIZE
    else
      LToRead := LPending;

    LRead := BIO_read(FBioWrite, @LBuffer[0], LToRead);
    if LRead <= 0 then
      Break;

    FStream.WriteBuffer(LBuffer[0], LRead);
    Inc(Result, LRead);
  end;
end;

function TOpenSSLConnection.InternalHandshake(AIsClient: Boolean): Boolean;
var
  LRet, LErr: Integer;
begin
  Result := False;

  if (FSSL = nil) or (not HasStreamTransport) then
    Exit;

  // Set initial handshake state explicitly for stream-based connections
  if AIsClient then
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
      // Flush any handshake data still buffered
      PumpBIOToStream;

      // Strategy A: fail closed if validation fails
      if not ValidatePostHandshake(AIsClient) then
      begin
        FConnected := False;
        Result := False;
        Exit;
      end;

      Result := True;
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
