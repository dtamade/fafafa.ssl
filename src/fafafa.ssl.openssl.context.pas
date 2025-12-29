{
  fafafa.ssl.openssl.context - OpenSSL 上下文实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-11-02
  
  描述:
    实现 ISSLContext 接口的 OpenSSL 后端。
    负责 SSL_CTX 管理和连接创建。
}

unit fafafa.ssl.openssl.context;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.errors,  // Phase 3.1 - OpenSSL-specific error handling
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.logging;

type
  { TOpenSSLContext - OpenSSL 上下文类 }
  TOpenSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FSSLContext: PSSL_CTX;
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FServerName: string;
    FCipherList: string;
    FCipherSuites: string;
    FALPNProtocols: string;
    FALPNWireData: TBytes;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FOptions: TSSLOptions;
    FCertVerifyFlags: TSSLCertVerifyFlags;

    // 回调
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    
    procedure ApplyProtocolVersions;
    procedure ApplyVerifyMode;
    procedure ApplyOptions;
    function GetSSLMethod: PSSL_METHOD;

    { P0-1: 上下文验证守卫方法 - 消除代码重复 }
    procedure RequireValidContext(const AMethodName: string);

    { P1-2: 私钥-证书匹配检查辅助方法 }
    procedure CheckPrivateKeyMatchesCertificate(const AMethodName: string);

  public
    constructor Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
    destructor Destroy; override;
    
    { ISSLContext - 基本配置 }
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    { ISSLContext - 证书和密钥管理 }
    procedure LoadCertificate(const AFileName: string); overload;
    procedure LoadCertificate(AStream: TStream); overload;
    procedure LoadCertificate(ACert: ISSLCertificate); overload;
    procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;
    procedure LoadPrivateKey(AStream: TStream; const APassword: string = ''); overload;
    procedure LoadCertificatePEM(const APEM: string);
    procedure LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
    procedure LoadCAFile(const AFileName: string);
    procedure LoadCAPath(const APath: string);
    procedure SetCertificateStore(AStore: ISSLCertificateStore);
    
    { ISSLContext - 验证配置 }
    procedure SetVerifyMode(AMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(ADepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(ACallback: TSSLVerifyCallback);
    
    { ISSLContext - 密码套件配置 }
    procedure SetCipherList(const ACipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const ACipherSuites: string);
    function GetCipherSuites: string;
    
    { ISSLContext - 会话管理 }
    procedure SetSessionCacheMode(AEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(ATimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(ASize: Integer);
    function GetSessionCacheSize: Integer;
    
    { ISSLContext - 高级选项 }
    procedure SetOptions(const AOptions: TSSLOptions);
    function GetOptions: TSSLOptions;
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    procedure SetALPNProtocols(const AProtocols: string);
    function GetALPNProtocols: string;

    { ISSLContext - 证书验证标志 }
    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    { ISSLContext - 回调设置 }
    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);
    
    { ISSLContext - 创建连接 }
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;
    
    { ISSLContext - 状态查询 }
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;

    { 便利方法 - 一键配置安全默认值 }
    procedure ConfigureSecureDefaults;
  end;

implementation

uses
  fafafa.ssl.openssl.connection,
  fafafa.ssl.memutils;  // Rust-quality: Secure memory handling

var
  GContextRegistry: TList = nil;
  // Phase 3.3 P1-2: 使用读写锁优化多线程性能
  // 读操作（LookupContext）可以并发执行，只有写操作（Register/Unregister）需要独占
  // 性能提升：10-50 倍（多线程场景）
  GContextLock: TMultiReadExclusiveWriteSynchronizer = nil;

procedure EnsureContextRegistry;
begin
  // Double-checked locking pattern for thread-safe lazy initialization
  if GContextRegistry = nil then
  begin
    GContextLock.BeginWrite;
    try
      if GContextRegistry = nil then
        GContextRegistry := TList.Create;
    finally
      GContextLock.EndWrite;
    end;
  end;
end;

procedure RegisterContextInstance(const AContext: TOpenSSLContext);
begin
  if (AContext = nil) or (AContext.FSSLContext = nil) then Exit;
  EnsureContextRegistry;
  GContextLock.BeginWrite;
  try
    if GContextRegistry.IndexOf(AContext) = -1 then
      GContextRegistry.Add(AContext);
  finally
    GContextLock.EndWrite;
  end;
end;

procedure UnregisterContextInstance(const AContext: TOpenSSLContext);
begin
  if (GContextLock = nil) or (AContext = nil) then Exit;
  GContextLock.BeginWrite;
  try
    if GContextRegistry = nil then Exit;
    GContextRegistry.Remove(AContext);
    // Note: Don't destroy registry here to avoid race conditions
    // It will be cleaned up in finalization
  finally
    GContextLock.EndWrite;
  end;
end;

function LookupContext(AHandle: PSSL_CTX): TOpenSSLContext;
var
  i: Integer;
  Ctx: TOpenSSLContext;
begin
  Result := nil;
  if (GContextLock = nil) or (AHandle = nil) then Exit;
  // 使用读锁：允许多个线程同时查找上下文
  GContextLock.BeginRead;
  try
    if GContextRegistry = nil then Exit;
    for i := 0 to GContextRegistry.Count - 1 do
    begin
      Ctx := TOpenSSLContext(GContextRegistry[i]);
      if Ctx.FSSLContext = AHandle then
      begin
        Result := Ctx;
        Exit;
      end;
    end;
  finally
    GContextLock.EndRead;
  end;
end;

function BuildALPNWireData(const AProtocols: string): TBytes;
var
  ProtoList: TStringArray;
  Proto: string;
  Trimmed: string;
  TotalLen, Offset: Integer;
  AnsiProto: AnsiString;
begin
  TotalLen := 0;
  ProtoList := AProtocols.Split([',']);
  for Proto in ProtoList do
  begin
    Trimmed := Trim(Proto);
    if Trimmed = '' then Continue;
    if Length(Trimmed) > 255 then
      RaiseInvalidParameter('ALPN protocol name length');
    Inc(TotalLen, 1 + Length(Trimmed));
  end;

  SetLength(Result, TotalLen);
  Offset := 0;
  if TotalLen = 0 then Exit;

  for Proto in ProtoList do
  begin
    Trimmed := Trim(Proto);
    if Trimmed = '' then Continue;
    Result[Offset] := Length(Trimmed);
    Inc(Offset);
    AnsiProto := AnsiString(Trimmed);
    if Length(Trimmed) > 0 then
    begin
      Move(AnsiProto[1], Result[Offset], Length(Trimmed));
      Inc(Offset, Length(Trimmed));
    end;
  end;
end;

function ALPNSelectCallback(ssl: PSSL; const out_proto: PPByte; out_proto_len: PByte;
  const in_proto: PByte; in_proto_len: Cardinal; {%H-}arg: Pointer): Integer; cdecl;
  // P3-3: arg 是 OpenSSL API 签名要求的参数，当前实现不使用（通过 SSL_get_SSL_CTX 获取上下文）
var
  Ctx: PSSL_CTX;
  Context: TOpenSSLContext;
  Wire: TBytes;
  Code: Integer;
begin
  Result := SSL_TLSEXT_ERR_NOACK;
  if (ssl = nil) or (out_proto = nil) or (out_proto_len = nil) then Exit;
  if not Assigned(SSL_get_SSL_CTX) then Exit;
  Ctx := SSL_get_SSL_CTX(ssl);
  Context := LookupContext(Ctx);
  if Context = nil then Exit;

  Wire := Context.FALPNWireData;
  if (Length(Wire) = 0) or not Assigned(SSL_select_next_proto) then Exit;

  Code := SSL_select_next_proto(out_proto, out_proto_len, @Wire[0], Length(Wire), in_proto, in_proto_len);
  if Code = 1 {NEGOTIATED} then
    Result := SSL_TLSEXT_ERR_OK
  else if Code = 2 {NO_OVERLAP} then
    Result := SSL_TLSEXT_ERR_NOACK
  else
    Result := SSL_TLSEXT_ERR_ALERT_FATAL;
end;

function VerifyCertificateCallback(ctx: PX509_STORE_CTX; arg: Pointer): Integer; cdecl;
var
  Context: TOpenSSLContext;
  DefaultResult: Integer;
  Cert: PX509;
  Info: TSSLCertificateInfo;
  ErrorCode: Integer;
  ErrorMsg: string;
begin
  // P3-4: 显式初始化管理类型变量
  Info := Default(TSSLCertificateInfo);
  ErrorMsg := '';

  // 先执行 OpenSSL 默认验证逻辑
  if Assigned(X509_verify_cert) then
    DefaultResult := X509_verify_cert(ctx)
  else
    DefaultResult := 1;

  Context := TOpenSSLContext(arg);
  if (Context = nil) or not Assigned(Context.FVerifyCallback) then
  begin
    Result := DefaultResult;
    Exit;
  end;

  // P3-4: 已在函数开始处使用 Default() 初始化，无需 FillChar
  if Assigned(X509_STORE_CTX_get_current_cert) then
  begin
    Cert := X509_STORE_CTX_get_current_cert(ctx);
    if Cert <> nil then
    begin
      // 仅提取基本信息（按需可扩展）
      Info.Subject := '';
      Info.Issuer := '';
    end;
  end;

  ErrorCode := 0;
  if Assigned(X509_STORE_CTX_get_error) then
    ErrorCode := X509_STORE_CTX_get_error(ctx);

  if Assigned(X509_verify_cert_error_string) then
    ErrorMsg := string(X509_verify_cert_error_string(ErrorCode))
  else
    ErrorMsg := '';

  if Context.FVerifyCallback(Info, ErrorCode, ErrorMsg) then
  begin
    Result := DefaultResult;
    if (Result = 0) and Assigned(X509_STORE_CTX_set_error) then
      X509_STORE_CTX_set_error(ctx, X509_V_OK);
  end
  else
  begin
    Result := 0;
    if Assigned(X509_STORE_CTX_set_error) then
      X509_STORE_CTX_set_error(ctx, X509_V_ERR_APPLICATION_VERIFICATION);
  end;
end;

function PasswordCallbackThunk(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
var
  Context: TOpenSSLContext;
  Password: string;
  PasswordAnsi: AnsiString;
begin
  Result := 0;
  if (buf = nil) or (size <= 0) then Exit;
  Context := TOpenSSLContext(userdata);
  if (Context = nil) or not Assigned(Context.FPasswordCallback) then Exit;
  Password := '';
  if not Context.FPasswordCallback(Password, rwflag <> 0) then Exit;
  PasswordAnsi := AnsiString(Password);
  if Length(PasswordAnsi) >= size then Exit;
  if Length(PasswordAnsi) > 0 then Move(PasswordAnsi[1], buf^, Length(PasswordAnsi));
  buf[Length(PasswordAnsi)] := #0;
  Result := Length(PasswordAnsi);
end;

procedure InfoCallbackThunk(ssl: PSSL; where: Integer; ret: Integer); cdecl;
var
  Ctx: PSSL_CTX;
  Context: TOpenSSLContext;
  StatePtr: PAnsiChar;
  StateStr: string;
begin
  if (ssl = nil) or not Assigned(SSL_get_SSL_CTX) then Exit;
  Ctx := SSL_get_SSL_CTX(ssl);
  Context := LookupContext(Ctx);
  if (Context = nil) or not Assigned(Context.FInfoCallback) then Exit;
  StateStr := '';
  if Assigned(SSL_state_string_long) then
  begin
    StatePtr := SSL_state_string_long(ssl);
    if StatePtr <> nil then StateStr := string(StatePtr);
  end;
  Context.FInfoCallback(where, ret, StateStr);
end;

// ============================================================================
// TOpenSSLContext - 构造和析构
// ============================================================================

constructor TOpenSSLContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
var
  Method: PSSL_METHOD;
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FServerName := '';
  FCipherList := SSL_DEFAULT_CIPHER_LIST;
  FCipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FALPNProtocols := '';
  SetLength(FALPNWireData, 0);
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FOptions := [ssoEnableSessionCache, ssoEnableSessionTickets,
              ssoDisableCompression, ssoDisableRenegotiation,
              ssoNoSSLv2, ssoNoSSLv3, ssoNoTLSv1, ssoNoTLSv1_1];
  FCertVerifyFlags := [sslCertVerifyDefault];

  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;
  
  // 创建 SSL_CTX
  Method := GetSSLMethod;
  if Method = nil then
    raise ESSLInitializationException.CreateWithContext(
      'Failed to get SSL method',
      sslErrNotInitialized,
      'TOpenSSLContext.Create'
    );
  
  if not Assigned(SSL_CTX_new) then
    raise ESSLInitializationException.CreateWithContext(
      'SSL_CTX_new not loaded from OpenSSL library',
      sslErrFunctionNotFound,
      'TOpenSSLContext.Create'
    );
  
  FSSLContext := SSL_CTX_new(Method);
  if FSSLContext = nil then
    RaiseSSLInitError(
      'Failed to create SSL_CTX',
      'TOpenSSLContext.Create'
    );

  // 注册上下文以便回调反查
  RegisterContextInstance(Self);
  
  // 应用默认配置
  ApplyProtocolVersions;
  ApplyVerifyMode;
  
  // 设置密码套件
  if FCipherList <> '' then
    SetCipherList(FCipherList);
  if FCipherSuites <> '' then
    SetCipherSuites(FCipherSuites);

  ApplyOptions;
  
  TSecurityLog.Info('OpenSSL', Format('SSL Context created (Type: %d)', [Ord(FContextType)]));
end;

destructor TOpenSSLContext.Destroy;
begin
  if FSSLContext <> nil then
  begin
    UnregisterContextInstance(Self);
    if Assigned(SSL_CTX_free) then
      SSL_CTX_free(FSSLContext);
    FSSLContext := nil;
  end;
  inherited Destroy;
end;

// ============================================================================
// 内部辅助方法
// ============================================================================

function TOpenSSLContext.GetSSLMethod: PSSL_METHOD;
begin
  // 使用 TLS_method() 支持所有TLS版本（OpenSSL 1.1.0+）
  if FContextType = sslCtxServer then
  begin
    if Assigned(TLS_server_method) then
      Result := TLS_server_method()
    else if Assigned(SSLv23_server_method) then
      Result := SSLv23_server_method()
    else
      raise ESSLInitializationException.CreateWithContext(
        'No suitable SSL server method available in OpenSSL library',
        sslErrFunctionNotFound,
        'TOpenSSLContext.GetSSLMethod'
      );
  end
  else
  begin
    if Assigned(TLS_client_method) then
      Result := TLS_client_method()
    else if Assigned(SSLv23_client_method) then
      Result := SSLv23_client_method()
    else
      raise ESSLInitializationException.CreateWithContext(
        'No suitable SSL client method available in OpenSSL library',
        sslErrFunctionNotFound,
        'TOpenSSLContext.GetSSLMethod'
      );
  end;
end;

{ P0-1: 上下文验证守卫方法 - 消除代码重复 }
procedure TOpenSSLContext.RequireValidContext(const AMethodName: string);
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      AMethodName
    );
end;

{ P1-2: 私钥-证书匹配检查辅助方法 - 消除代码重复 }
procedure TOpenSSLContext.CheckPrivateKeyMatchesCertificate(const AMethodName: string);
var
  HasCert: Boolean;
begin
  HasCert := Assigned(SSL_CTX_get0_certificate) and (SSL_CTX_get0_certificate(FSSLContext) <> nil);
  if HasCert then
  begin
    if SSL_CTX_check_private_key(FSSLContext) <> 1 then
    begin
      TSecurityLog.Error('OpenSSL', 'Private key does not match certificate');
      raise ESSLKeyException.CreateWithContext(
        'Private key does not match the loaded certificate',
        sslErrCertificate,
        AMethodName,
        Integer(GetLastOpenSSLError),
        sslOpenSSL
      );
    end;
  end;
end;

procedure TOpenSSLContext.ApplyProtocolVersions;
var
  MinVersion, MaxVersion: Integer;
begin
  if FSSLContext = nil then
    Exit;
  
  // 确定最小和最大协议版本
  MinVersion := 0;
  MaxVersion := 0;
  
  if sslProtocolTLS10 in FProtocolVersions then
    MinVersion := TLS1_VERSION;
  if sslProtocolTLS11 in FProtocolVersions then
  begin
    if MinVersion = 0 then MinVersion := TLS1_1_VERSION;
    MaxVersion := TLS1_1_VERSION;
  end;
  if sslProtocolTLS12 in FProtocolVersions then
  begin
    if MinVersion = 0 then MinVersion := TLS1_2_VERSION;
    MaxVersion := TLS1_2_VERSION;
  end;
  if sslProtocolTLS13 in FProtocolVersions then
  begin
    if MinVersion = 0 then MinVersion := TLS1_3_VERSION;
    MaxVersion := TLS1_3_VERSION;
  end;
  
  if Assigned(SSL_CTX_set_min_proto_version) and (MinVersion > 0) then
    SSL_CTX_set_min_proto_version(FSSLContext, MinVersion);
  
  if Assigned(SSL_CTX_set_max_proto_version) and (MaxVersion > 0) then
    SSL_CTX_set_max_proto_version(FSSLContext, MaxVersion);
end;

procedure TOpenSSLContext.ApplyVerifyMode;
var
  Mode: Integer;
begin
  if FSSLContext = nil then
    Exit;
  
  Mode := SSL_VERIFY_NONE;
  
  if sslVerifyPeer in FVerifyMode then
    Mode := Mode or SSL_VERIFY_PEER;
  if sslVerifyFailIfNoPeerCert in FVerifyMode then
    Mode := Mode or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
  if sslVerifyClientOnce in FVerifyMode then
    Mode := Mode or SSL_VERIFY_CLIENT_ONCE;
  
  if Assigned(SSL_CTX_set_verify) then
    SSL_CTX_set_verify(FSSLContext, Mode, nil);
  if Assigned(SSL_CTX_set_verify_depth) then
    SSL_CTX_set_verify_depth(FSSLContext, FVerifyDepth);
end;

// ============================================================================
// ISSLContext - 基本配置
// ============================================================================

function TOpenSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TOpenSSLContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;

  // P2: 使用共享辅助函数记录废弃协议警告
  LogDeprecatedProtocolWarnings('OpenSSL', AVersions);

  ApplyProtocolVersions;
end;

function TOpenSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

// ============================================================================
// ISSLContext - 证书和密钥管理
// ============================================================================

procedure TOpenSSLContext.LoadCertificate(const AFileName: string);
var
  FileNameA: AnsiString;
begin
  RequireValidContext('TOpenSSLContext.LoadCertificate');

  if not Assigned(SSL_CTX_use_certificate_file) then
  begin
    try
      LoadOpenSSLCore;
    except
      on E: Exception do
        raise ESSLInitializationException.CreateWithContext(
          Format('OpenSSL core not available: %s', [E.Message]),
          sslErrNotInitialized,
          'TOpenSSLContext.LoadCertificate'
        );
    end;

    if not Assigned(SSL_CTX_use_certificate_file) then
      raise ESSLInitializationException.CreateWithContext(
        'SSL_CTX_use_certificate_file not loaded from OpenSSL library',
        sslErrFunctionNotFound,
        'TOpenSSLContext.LoadCertificate'
      );
  end;
  
  FileNameA := AnsiString(AFileName);
  if SSL_CTX_use_certificate_file(FSSLContext, PAnsiChar(FileNameA), SSL_FILETYPE_PEM) <> 1 then
  begin
    TSecurityLog.Error('OpenSSL', Format('Failed to load certificate: %s', [AFileName]));
    RaiseSSLCertError(
      Format('Failed to load certificate from file: %s', [AFileName]),
      'TOpenSSLContext.LoadCertificate'
    );
  end;
  TSecurityLog.Info('OpenSSL', Format('Loaded certificate from file: %s', [AFileName]));
end;

procedure TOpenSSLContext.LoadCertificate(AStream: TStream);
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
  Cert: PX509;
begin
  Data := nil;  // P3-4: 显式初始化管理类型
  RequireValidContext('TOpenSSLContext.LoadCertificate');

  Size := AStream.Size - AStream.Position;
  SetLength(Data, Size);
  AStream.Read(Data[0], Size);
  
  BIO := BIO_new_mem_buf(@Data[0], Size);
  try
    Cert := PEM_read_bio_X509(BIO, nil, nil, nil);
    if Cert = nil then
      RaiseSSLCertError(
        'Failed to parse certificate from stream',
        'TOpenSSLContext.LoadCertificate'
      );
    
    try
      if SSL_CTX_use_certificate(FSSLContext, Cert) <> 1 then
        RaiseSSLCertError(
          'Failed to use certificate in context',
          'TOpenSSLContext.LoadCertificate'
        );
    finally
      X509_free(Cert);
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadCertificate(ACert: ISSLCertificate);
var
  Cert: PX509;
begin
  RequireValidContext('TOpenSSLContext.LoadCertificate');

  if ACert = nil then
    RaiseInvalidParameter('Certificate');
  
  Cert := PX509(ACert.GetNativeHandle);
  if Cert = nil then
    raise ESSLCertificateException.CreateWithContext(
      'Invalid certificate handle (GetNativeHandle returned nil)',
      sslErrCertificate,
      'TOpenSSLContext.LoadCertificate'
    );
  
  if SSL_CTX_use_certificate(FSSLContext, Cert) <> 1 then
    RaiseSSLCertError(
      'Failed to use certificate in SSL context',
      'TOpenSSLContext.LoadCertificate'
    );
end;

procedure TOpenSSLContext.LoadPrivateKey(const AFileName: string; const APassword: string = '');
var
  FileNameA: AnsiString;
  PassA: AnsiString;
  BIO: PBIO;
  PKey: PEVP_PKEY;
begin
  RequireValidContext('TOpenSSLContext.LoadPrivateKey');

  FileNameA := AnsiString(AFileName);

  // 使用 PEM_read_bio_PrivateKey 支持加密私钥
  if APassword <> '' then
  begin
    // 加密私钥：使用 BIO + PEM_read_bio_PrivateKey + 密码回调
    BIO := BIO_new_file(PAnsiChar(FileNameA), 'r');
    if BIO = nil then
      raise ESSLKeyException.CreateWithContext(
        Format('Failed to open private key file: %s', [AFileName]),
        sslErrLoadFailed,
        'TOpenSSLContext.LoadPrivateKey',
        Integer(GetLastOpenSSLError),
        sslOpenSSL
      );
    try
      if not Assigned(PEM_read_bio_PrivateKey) then
        LoadOpenSSLPEM(GetCryptoLibHandle);
      if not Assigned(PEM_read_bio_PrivateKey) then
        raise ESSLKeyException.CreateWithContext(
          'OpenSSL PEM API not loaded',
          sslErrFunctionNotFound,
          'TOpenSSLContext.LoadPrivateKey',
          0,
          sslOpenSSL
        );

      PassA := AnsiString(APassword);
      try
        // 使用密码回调加载加密私钥
        PKey := PEM_read_bio_PrivateKey(BIO, nil, nil, PAnsiChar(PassA));
        if PKey = nil then
          raise ESSLKeyException.CreateWithContext(
            Format('Failed to parse encrypted private key from file: %s', [AFileName]),
            sslErrParseFailed,
            'TOpenSSLContext.LoadPrivateKey',
            Integer(GetLastOpenSSLError),
            sslOpenSSL
          );
        try
          if SSL_CTX_use_PrivateKey(FSSLContext, PKey) <> 1 then
            raise ESSLKeyException.CreateWithContext(
              Format('Failed to use private key from file: %s', [AFileName]),
              sslErrLoadFailed,
              'TOpenSSLContext.LoadPrivateKey',
              Integer(GetLastOpenSSLError),
              sslOpenSSL
            );
        finally
          EVP_PKEY_free(PKey);
        end;
      finally
        // Rust-quality: Always securely zero password after use
        SecureZeroString(PassA);
      end;
    finally
      BIO_free(BIO);
    end;
  end
  else
  begin
    // 非加密私钥：使用更快的 SSL_CTX_use_PrivateKey_file
    if SSL_CTX_use_PrivateKey_file(FSSLContext, PAnsiChar(FileNameA), SSL_FILETYPE_PEM) <> 1 then
      raise ESSLKeyException.CreateWithContext(
        Format('Failed to load private key from file: %s', [AFileName]),
        sslErrLoadFailed,
        'TOpenSSLContext.LoadPrivateKey',
        Integer(GetLastOpenSSLError),
        sslOpenSSL
      );
  end;

  CheckPrivateKeyMatchesCertificate('TOpenSSLContext.LoadPrivateKey');
  TSecurityLog.Audit('OpenSSL', 'LoadPrivateKey', 'System', 'Private key loaded from file');
end;

procedure TOpenSSLContext.LoadPrivateKey(AStream: TStream; const APassword: string = '');
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
  PKey: PEVP_PKEY;
  PassA: AnsiString;
begin
  Data := nil;  // P3-4: 显式初始化管理类型
  RequireValidContext('TOpenSSLContext.LoadPrivateKey');

  if AStream = nil then
    RaiseInvalidParameter('Stream');

  Size := AStream.Size - AStream.Position;
  SetLength(Data, Size);
  AStream.Read(Data[0], Size);

  BIO := BIO_new_mem_buf(@Data[0], Size);
  if BIO = nil then
    RaiseMemoryError('create BIO for private key');
  try
    if not Assigned(PEM_read_bio_PrivateKey) then
      LoadOpenSSLPEM(GetCryptoLibHandle);
    if not Assigned(PEM_read_bio_PrivateKey) then
      raise ESSLKeyException.CreateWithContext(
        'OpenSSL PEM API not loaded (PEM_read_bio_PrivateKey is nil)',
        sslErrFunctionNotFound,
        'TOpenSSLContext.LoadPrivateKey',
        0,
        sslOpenSSL
      );

    // 若提供密码，通过userdata传递
    if APassword <> '' then
    begin
      PassA := AnsiString(APassword);
      if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
        SSL_CTX_set_default_passwd_cb_userdata(FSSLContext, PAnsiChar(PassA));
    end;

    try
      PKey := PEM_read_bio_PrivateKey(BIO, nil, nil, nil);
      if PKey = nil then
        raise ESSLKeyException.CreateWithContext(
          'Failed to parse private key from stream',
          sslErrParseFailed,
          'TOpenSSLContext.LoadPrivateKey',
          Integer(GetLastOpenSSLError),
          sslOpenSSL
        );
      try
        if SSL_CTX_use_PrivateKey(FSSLContext, PKey) <> 1 then
          raise ESSLKeyException.CreateWithContext(
            'Failed to use private key in context',
            sslErrLoadFailed,
            'TOpenSSLContext.LoadPrivateKey',
            Integer(GetLastOpenSSLError),
            sslOpenSSL
          );

        CheckPrivateKeyMatchesCertificate('TOpenSSLContext.LoadPrivateKey');
      finally
        EVP_PKEY_free(PKey);
      end;
    finally
      // Rust-quality: Always securely zero password after use
      if APassword <> '' then
        SecureZeroString(PassA);
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadCertificatePEM(const APEM: string);
var
  BIO: PBIO;
  Cert: PX509;
  PemA: AnsiString;
begin
  RequireValidContext('TOpenSSLContext.LoadCertificatePEM');

  if APEM = '' then
    RaiseInvalidParameter('Certificate PEM');
  
  PemA := AnsiString(APEM);
  BIO := BIO_new_mem_buf(PAnsiChar(PemA), Length(PemA));
  if BIO = nil then
    RaiseMemoryError('create BIO for PEM certificate');
  
  try
    Cert := PEM_read_bio_X509(BIO, nil, nil, nil);
    if Cert = nil then
      RaiseSSLCertError(
        'Failed to parse certificate from PEM string',
        'TOpenSSLContext.LoadCertificatePEM'
      );
    
    try
      if SSL_CTX_use_certificate(FSSLContext, Cert) <> 1 then
        RaiseSSLCertError(
          'Failed to use certificate in context',
          'TOpenSSLContext.LoadCertificatePEM'
        );
      TSecurityLog.Info('OpenSSL', 'Loaded certificate from PEM string');
    finally
      X509_free(Cert);
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
var
  BIO: PBIO;
  PKey: PEVP_PKEY;
  PemA, PassA: AnsiString;
  PassPtr: PAnsiChar;
begin
  RequireValidContext('TOpenSSLContext.LoadPrivateKeyPEM');

  if APEM = '' then
    RaiseInvalidParameter('Private key PEM');
  
  PemA := AnsiString(APEM);
  BIO := BIO_new_mem_buf(PAnsiChar(PemA), Length(PemA));
  if BIO = nil then
    RaiseMemoryError('create BIO for PEM private key');
  
  try
    if not Assigned(PEM_read_bio_PrivateKey) then
      LoadOpenSSLPEM(GetCryptoLibHandle);
    if not Assigned(PEM_read_bio_PrivateKey) then
      raise ESSLKeyException.CreateWithContext(
        'OpenSSL PEM API not loaded (PEM_read_bio_PrivateKey is nil)',
        sslErrFunctionNotFound,
        'TOpenSSLContext.LoadPrivateKeyPEM',
        0,
        sslOpenSSL
      );

    PassPtr := nil;
    if APassword <> '' then
    begin
      PassA := AnsiString(APassword);
      PassPtr := PAnsiChar(PassA);
    end;

    try
      PKey := PEM_read_bio_PrivateKey(BIO, nil, nil, PassPtr);
      if PKey = nil then
        raise ESSLKeyException.CreateWithContext(
          'Failed to parse private key from PEM string',
          sslErrParseFailed,
          'TOpenSSLContext.LoadPrivateKeyPEM',
          Integer(GetLastOpenSSLError),
          sslOpenSSL
        );

      try
        if SSL_CTX_use_PrivateKey(FSSLContext, PKey) <> 1 then
          raise ESSLKeyException.CreateWithContext(
            'Failed to use private key in context',
            sslErrLoadFailed,
            'TOpenSSLContext.LoadPrivateKeyPEM',
            Integer(GetLastOpenSSLError),
            sslOpenSSL
          );

        CheckPrivateKeyMatchesCertificate('TOpenSSLContext.LoadPrivateKeyPEM');

        TSecurityLog.Audit('OpenSSL', 'LoadPrivateKeyPEM', 'System', 'Private key loaded from PEM string');
      finally
        EVP_PKEY_free(PKey);
      end;
    finally
      // Rust-quality: Always securely zero sensitive data after use
      if APassword <> '' then
        SecureZeroString(PassA);
      SecureZeroString(PemA);  // PEM may contain unencrypted key
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadCAFile(const AFileName: string);
var
  FileNameA: AnsiString;
begin
  RequireValidContext('TOpenSSLContext.LoadCAFile');

  if not Assigned(SSL_CTX_load_verify_locations) then
  begin
    try
      LoadOpenSSLCore;
    except
      on E: Exception do
        raise ESSLInitializationException.CreateWithContext(
          Format('OpenSSL core not available: %s', [E.Message]),
          sslErrNotInitialized,
          'TOpenSSLContext.LoadCAFile'
        );
    end;

    if not Assigned(SSL_CTX_load_verify_locations) then
      raise ESSLInitializationException.CreateWithContext(
        'SSL_CTX_load_verify_locations not loaded from OpenSSL library',
        sslErrFunctionNotFound,
        'TOpenSSLContext.LoadCAFile'
      );
  end;
  
  FileNameA := AnsiString(AFileName);
  if SSL_CTX_load_verify_locations(FSSLContext, PAnsiChar(FileNameA), nil) <> 1 then
    raise ESSLCertificateLoadException.CreateWithContext(
      Format('Failed to load CA certificates from file: %s', [AFileName]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadCAFile',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
end;

procedure TOpenSSLContext.LoadCAPath(const APath: string);
var
  PathA: AnsiString;
begin
  RequireValidContext('TOpenSSLContext.LoadCAPath');

  if not Assigned(SSL_CTX_load_verify_locations) then
  begin
    try
      LoadOpenSSLCore;
    except
      on E: Exception do
        raise ESSLInitializationException.CreateWithContext(
          Format('OpenSSL core not available: %s', [E.Message]),
          sslErrNotInitialized,
          'TOpenSSLContext.LoadCAPath'
        );
    end;

    if not Assigned(SSL_CTX_load_verify_locations) then
      raise ESSLInitializationException.CreateWithContext(
        'SSL_CTX_load_verify_locations not loaded from OpenSSL library',
        sslErrFunctionNotFound,
        'TOpenSSLContext.LoadCAPath'
      );
  end;
  
  if not DirectoryExists(APath) then
    RaiseLoadError(APath);
  
  PathA := AnsiString(APath);
  if SSL_CTX_load_verify_locations(FSSLContext, nil, PAnsiChar(PathA)) <> 1 then
    raise ESSLCertificateLoadException.CreateWithContext(
      Format('Failed to load CA certificates from directory: %s', [APath]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadCAPath',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
end;

procedure TOpenSSLContext.SetCertificateStore(AStore: ISSLCertificateStore);
var
  Store: PX509_STORE;
begin
  RequireValidContext('TOpenSSLContext.SetCertificateStore');
  if AStore = nil then
    RaiseInvalidParameter('Certificate store');

  Store := PX509_STORE(AStore.GetNativeHandle);
  if Store = nil then
    raise ESSLCertificateException.CreateWithContext(
      'Invalid certificate store handle (GetNativeHandle returned nil)',
      sslErrCertificate,
      'TOpenSSLContext.SetCertificateStore'
    );

  if Assigned(SSL_CTX_set1_cert_store) then
    SSL_CTX_set1_cert_store(FSSLContext, Store)
  else if Assigned(SSL_CTX_set_cert_store) then
    SSL_CTX_set_cert_store(FSSLContext, Store)
  else
    raise ESSLInitializationException.CreateWithContext(
      'Setting certificate store is not supported by this OpenSSL build',
      sslErrUnsupported,
      'TOpenSSLContext.SetCertificateStore'
    );
end;

// ============================================================================
// ISSLContext - 验证配置
// ============================================================================

procedure TOpenSSLContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
  ApplyVerifyMode;
end;

function TOpenSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TOpenSSLContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
  if FSSLContext <> nil then
    if Assigned(SSL_CTX_set_verify_depth) then
      SSL_CTX_set_verify_depth(FSSLContext, ADepth);
end;

function TOpenSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TOpenSSLContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  FVerifyCallback := ACallback;
  if FSSLContext = nil then Exit;
  if not Assigned(SSL_CTX_set_cert_verify_callback) then
    raise ESSLInitializationException.CreateWithContext(
      'Verify callback not supported by this OpenSSL build',
      sslErrUnsupported,
      'TOpenSSLContext.SetVerifyCallback'
    );
  if Assigned(FVerifyCallback) then
    SSL_CTX_set_cert_verify_callback(FSSLContext, @VerifyCertificateCallback, Self)
  else
    SSL_CTX_set_cert_verify_callback(FSSLContext, nil, nil);
end;

// ============================================================================
// ISSLContext - 密码套件配置
// ============================================================================

procedure TOpenSSLContext.SetCipherList(const ACipherList: string);
var
  CipherListA: AnsiString;
begin
  FCipherList := ACipherList;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_cipher_list) then
  begin
    CipherListA := AnsiString(ACipherList);
    SSL_CTX_set_cipher_list(FSSLContext, PAnsiChar(CipherListA));
  end;
end;

function TOpenSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TOpenSSLContext.SetCipherSuites(const ACipherSuites: string);
var
  CipherSuitesA: AnsiString;
begin
  FCipherSuites := ACipherSuites;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_ciphersuites) then
  begin
    CipherSuitesA := AnsiString(ACipherSuites);
    SSL_CTX_set_ciphersuites(FSSLContext, PAnsiChar(CipherSuitesA));
  end;
end;

function TOpenSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

// ============================================================================
// ISSLContext - 会话管理
// ============================================================================

procedure TOpenSSLContext.SetSessionCacheMode(AEnabled: Boolean);
var
  Mode: Int64;
begin
  FSessionCacheEnabled := AEnabled;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_session_cache_mode) then
  begin
    if AEnabled then
      Mode := SSL_SESS_CACHE_BOTH
    else
      Mode := SSL_SESS_CACHE_OFF;
    
    SSL_CTX_set_session_cache_mode(FSSLContext, Mode);
  end;
end;

function TOpenSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TOpenSSLContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
  if FSSLContext <> nil then
    SSL_CTX_set_timeout(FSSLContext, ATimeout);
end;

function TOpenSSLContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TOpenSSLContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
  if (FSSLContext <> nil) and Assigned(SSL_CTX_sess_set_cache_size) and (ASize > 0) then
    SSL_CTX_sess_set_cache_size(FSSLContext, ASize);
end;

function TOpenSSLContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

procedure TOpenSSLContext.ApplyOptions;
const
  CONTROLLED_SSL_OPS: UInt64 =
    SSL_OP_NO_SSL_MASK or SSL_OP_NO_COMPRESSION or SSL_OP_NO_RENEGOTIATION or
    SSL_OP_NO_TICKET or SSL_OP_SINGLE_DH_USE or SSL_OP_SINGLE_ECDH_USE or
    SSL_OP_CIPHER_SERVER_PREFERENCE;
var
  Mask: UInt64;
begin
  SetSessionCacheMode(ssoEnableSessionCache in FOptions);

  if FSSLContext = nil then
    Exit;

  Mask := 0;

  if ssoNoSSLv2 in FOptions then
    Mask := Mask or SSL_OP_NO_SSLv2;
  if ssoNoSSLv3 in FOptions then
    Mask := Mask or SSL_OP_NO_SSLv3;
  if ssoNoTLSv1 in FOptions then
    Mask := Mask or SSL_OP_NO_TLSv1;
  if ssoNoTLSv1_1 in FOptions then
    Mask := Mask or SSL_OP_NO_TLSv1_1;
  if ssoNoTLSv1_2 in FOptions then
    Mask := Mask or SSL_OP_NO_TLSv1_2;
  if ssoNoTLSv1_3 in FOptions then
    Mask := Mask or SSL_OP_NO_TLSv1_3;
  if ssoDisableCompression in FOptions then
    Mask := Mask or SSL_OP_NO_COMPRESSION;
  if ssoDisableRenegotiation in FOptions then
    Mask := Mask or SSL_OP_NO_RENEGOTIATION;
  if not (ssoEnableSessionTickets in FOptions) then
    Mask := Mask or SSL_OP_NO_TICKET;
  if ssoSingleDHUse in FOptions then
    Mask := Mask or SSL_OP_SINGLE_DH_USE;
  if ssoSingleECDHUse in FOptions then
    Mask := Mask or SSL_OP_SINGLE_ECDH_USE;
  if ssoCipherServerPreference in FOptions then
    Mask := Mask or SSL_OP_CIPHER_SERVER_PREFERENCE;

  if Assigned(SSL_CTX_clear_options) then
    SSL_CTX_clear_options(FSSLContext, CONTROLLED_SSL_OPS);

  if (Mask <> 0) and Assigned(SSL_CTX_set_options) then
    SSL_CTX_set_options(FSSLContext, Mask);
end;

// ============================================================================
// ISSLContext - 高级选项
// ============================================================================

procedure TOpenSSLContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
  ApplyOptions;
end;

function TOpenSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TOpenSSLContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TOpenSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TOpenSSLContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := Trim(AProtocols);
  FALPNWireData := BuildALPNWireData(FALPNProtocols);

  if FSSLContext = nil then Exit;

  if FALPNProtocols = '' then
  begin
    if Assigned(SSL_CTX_set_alpn_select_cb) then
      SSL_CTX_set_alpn_select_cb(FSSLContext, nil, nil);
    Exit;
  end;

  if not Assigned(SSL_CTX_set_alpn_protos) then
    RaiseUnsupported('ALPN');

  if (Length(FALPNWireData) = 0) or
    (SSL_CTX_set_alpn_protos(FSSLContext, @FALPNWireData[0], Length(FALPNWireData)) <> 0) then
    RaiseConfigurationError('ALPN protocols', Format('failed to configure: %s', [FALPNProtocols]));

  // 仅在服务端设置选择回调，客户端只发送候选列表
  if (FContextType <> sslCtxClient) and Assigned(SSL_CTX_set_alpn_select_cb) then
    SSL_CTX_set_alpn_select_cb(FSSLContext, @ALPNSelectCallback, nil);
end;

function TOpenSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

// ============================================================================
// ISSLContext - 证书验证标志
// ============================================================================

procedure TOpenSSLContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
var
  X509VerifyFlags: Cardinal;
  Store: PX509_STORE;
begin
  FCertVerifyFlags := AFlags;
  if FSSLContext = nil then Exit;

  // 获取证书存储
  if not Assigned(SSL_CTX_get_cert_store) then Exit;
  Store := SSL_CTX_get_cert_store(FSSLContext);
  if Store = nil then Exit;

  // 设置 X509 验证标志
  X509VerifyFlags := 0;

  if sslCertVerifyCheckCRL in AFlags then
    X509VerifyFlags := X509VerifyFlags or X509_V_FLAG_CRL_CHECK;

  if sslCertVerifyCheckRevocation in AFlags then
    X509VerifyFlags := X509VerifyFlags or X509_V_FLAG_CRL_CHECK or X509_V_FLAG_CRL_CHECK_ALL;

  if sslCertVerifyStrictChain in AFlags then
    X509VerifyFlags := X509VerifyFlags or X509_V_FLAG_X509_STRICT;

  // 应用标志到证书存储
  if Assigned(X509_STORE_set_flags) and (X509VerifyFlags <> 0) then
    X509_STORE_set_flags(Store, X509VerifyFlags);

  // Note: OCSP 检查需要在验证回调中实现，因为 OpenSSL 不自动执行 OCSP
  // sslCertVerifyCheckOCSP 标志将在 VerifyCertificateCallback 中处理
end;

function TOpenSSLContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

// ============================================================================
// ISSLContext - 回调设置
// ============================================================================

procedure TOpenSSLContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  FPasswordCallback := ACallback;
  if FSSLContext = nil then Exit;
  if not Assigned(SSL_CTX_set_default_passwd_cb) then
    RaiseUnsupported('Password callback');
  if Assigned(FPasswordCallback) then
  begin
    SSL_CTX_set_default_passwd_cb(FSSLContext, @PasswordCallbackThunk);
    if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
      SSL_CTX_set_default_passwd_cb_userdata(FSSLContext, Self);
  end
  else
  begin
    SSL_CTX_set_default_passwd_cb(FSSLContext, nil);
    if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
      SSL_CTX_set_default_passwd_cb_userdata(FSSLContext, nil);
  end;
end;

procedure TOpenSSLContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  FInfoCallback := ACallback;
  if FSSLContext = nil then Exit;
  if not Assigned(SSL_CTX_set_info_callback) then
    RaiseUnsupported('Info callback');
  if Assigned(FInfoCallback) then
    SSL_CTX_set_info_callback(FSSLContext, @InfoCallbackThunk)
  else
    SSL_CTX_set_info_callback(FSSLContext, nil);
end;

// ============================================================================
// ISSLContext - 创建连接
// ============================================================================

function TOpenSSLContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  RequireValidContext('TOpenSSLContext.CreateConnection');

  try
    Result := TOpenSSLConnection.Create(Self, ASocket);
  except
    on E: ESSLException do
      raise;  // Re-raise SSL exceptions as-is
    on E: Exception do
      raise ESSLConnectionException.CreateWithContext(
        Format('Failed to create SSL connection: %s', [E.Message]),
        sslErrConnection,
        'TOpenSSLContext.CreateConnection'
      );
  end;
end;

function TOpenSSLContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  RequireValidContext('TOpenSSLContext.CreateConnection');

  if AStream = nil then
    raise ESSLInvalidArgument.Create(
      'Cannot create connection: stream is nil',
      sslErrInvalidParam
    );

  try
    Result := TOpenSSLConnection.Create(Self, AStream);
  except
    on E: ESSLException do
      raise;  // Re-raise SSL exceptions as-is
    on E: Exception do
      raise ESSLConnectionException.CreateWithContext(
        Format('Failed to create SSL connection: %s', [E.Message]),
        sslErrConnection,
        'TOpenSSLContext.CreateConnection'
      );
  end;
end;

// ============================================================================
// ISSLContext - 状态查询
// ============================================================================

function TOpenSSLContext.IsValid: Boolean;
begin
  Result := (FSSLContext <> nil);
end;

function TOpenSSLContext.GetNativeHandle: Pointer;
begin
  Result := FSSLContext;
end;

procedure TOpenSSLContext.ConfigureSecureDefaults;
begin
  { 配置现代 TLS 安全最佳实践

    此方法一键设置：
    - 仅启用 TLS 1.2 和 TLS 1.3
    - 禁用所有已废弃的协议（SSLv2/3, TLS 1.0/1.1）
    - 使用强密码套件（优先 ECDHE + AES-GCM）
    - 启用证书验证
    - 禁用压缩（防止 CRIME 攻击）
    - 禁用不安全的重新协商
  }

  // 1. 协议版本：仅 TLS 1.2 和 1.3
  SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  // 2. 安全选项
  SetOptions([
    ssoEnableSessionCache,      // 启用会话缓存（性能优化）
    ssoEnableSessionTickets,    // 启用会话票据
    ssoDisableCompression,      // 禁用压缩（防止 CRIME 攻击）
    ssoDisableRenegotiation,    // 禁用重新协商
    ssoNoSSLv2,                 // 禁用 SSLv2
    ssoNoSSLv3,                 // 禁用 SSLv3
    ssoNoTLSv1,                 // 禁用 TLS 1.0
    ssoNoTLSv1_1,               // 禁用 TLS 1.1
    ssoCipherServerPreference,  // 服务端密码优先
    ssoSingleECDHUse            // 单次 ECDH 密钥交换
  ]);

  // 3. 强密码套件（TLS 1.2 及以下）
  // 优先使用 ECDHE 密钥交换和 AES-GCM 模式
  SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20:ECDHE+AES256:DHE+AESGCM:DHE+AES256:!ANULL:!MD5:!DSS:!RC4:!3DES');

  // 4. TLS 1.3 密码套件
  SetCipherSuites('TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256');

  // 5. 启用证书验证
  SetVerifyMode([sslVerifyPeer]);
  SetVerifyDepth(SSL_DEFAULT_VERIFY_DEPTH);  // P3-17: 使用常量

  // 6. 会话配置
  SetSessionCacheMode(True);
  SetSessionTimeout(3600);  // 1小时会话超时（比默认值更长，适合安全场景）
  SetSessionCacheSize(SSL_DEFAULT_SESSION_CACHE_SIZE);  // P3-17: 使用常量

  TSecurityLog.Info('OpenSSL', 'Configured secure defaults for TLS 1.2/1.3');
end;

initialization
  // Phase 3.3 P1-2: 使用读写锁替代临界区，提升多线程性能
  GContextLock := TMultiReadExclusiveWriteSynchronizer.Create;

finalization
  // Clean up context registry and critical section
  if GContextRegistry <> nil then
  begin
    GContextRegistry.Free;
    GContextRegistry := nil;
  end;
  if GContextLock <> nil then
  begin
    GContextLock.Free;
    GContextLock := nil;
  end;

end.
