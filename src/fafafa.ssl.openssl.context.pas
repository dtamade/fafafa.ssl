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
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,  // 新增：类型化异常
  fafafa.ssl.openssl.types,
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
    
    // 回调
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    
    procedure ApplyProtocolVersions;
    procedure ApplyVerifyMode;
    procedure ApplyOptions;
    function GetSSLMethod: PSSL_METHOD;
    
  public
    constructor Create(aLibrary: ISSLLibrary; aType: TSSLContextType);
    destructor Destroy; override;
    
    { ISSLContext - 基本配置 }
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    { ISSLContext - 证书和密钥管理 }
    procedure LoadCertificate(const aFileName: string); overload;
    procedure LoadCertificate(aStream: TStream); overload;
    procedure LoadCertificate(aCert: ISSLCertificate); overload;
    procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
    procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
    procedure LoadCertificatePEM(const aPEM: string);
    procedure LoadPrivateKeyPEM(const aPEM: string; const aPassword: string = '');
    procedure LoadCAFile(const aFileName: string);
    procedure LoadCAPath(const aPath: string);
    procedure SetCertificateStore(aStore: ISSLCertificateStore);
    
    { ISSLContext - 验证配置 }
    procedure SetVerifyMode(aMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(aDepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
    
    { ISSLContext - 密码套件配置 }
    procedure SetCipherList(const aCipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const aCipherSuites: string);
    function GetCipherSuites: string;
    
    { ISSLContext - 会话管理 }
    procedure SetSessionCacheMode(aEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(aTimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(aSize: Integer);
    function GetSessionCacheSize: Integer;
    
    { ISSLContext - 高级选项 }
    procedure SetOptions(const aOptions: TSSLOptions);
    function GetOptions: TSSLOptions;
    procedure SetServerName(const aServerName: string);
    function GetServerName: string;
    procedure SetALPNProtocols(const aProtocols: string);
    function GetALPNProtocols: string;
    
    { ISSLContext - 回调设置 }
    procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
    procedure SetInfoCallback(aCallback: TSSLInfoCallback);
    
    { ISSLContext - 创建连接 }
    function CreateConnection(aSocket: THandle): ISSLConnection; overload;
    function CreateConnection(aStream: TStream): ISSLConnection; overload;
    
    { ISSLContext - 状态查询 }
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;
  end;

implementation

uses
  fafafa.ssl.openssl.connection;

var
  GContextRegistry: TList = nil;

procedure EnsureContextRegistry;
begin
  if GContextRegistry = nil then
    GContextRegistry := TList.Create;
end;

procedure RegisterContextInstance(const AContext: TOpenSSLContext);
begin
  if (AContext = nil) or (AContext.FSSLContext = nil) then Exit;
  EnsureContextRegistry;
  if GContextRegistry.IndexOf(AContext) = -1 then
    GContextRegistry.Add(AContext);
end;

procedure UnregisterContextInstance(const AContext: TOpenSSLContext);
begin
  if (GContextRegistry = nil) or (AContext = nil) then Exit;
  GContextRegistry.Remove(AContext);
  if GContextRegistry.Count = 0 then
  begin
    GContextRegistry.Free;
    GContextRegistry := nil;
  end;
end;

function LookupContext(AHandle: PSSL_CTX): TOpenSSLContext;
var
  i: Integer;
  Ctx: TOpenSSLContext;
begin
  Result := nil;
  if (GContextRegistry = nil) or (AHandle = nil) then Exit;
  
  for i := 0 to GContextRegistry.Count - 1 do
  begin
    Ctx := TOpenSSLContext(GContextRegistry[i]);
    if Ctx.FSSLContext = AHandle then
    begin
      Result := Ctx;
      Exit;
    end;
  end;
end;

function BuildALPNWireData(const aProtocols: string): TBytes;
var
  ProtoList: TStringArray;
  Proto: string;
  Trimmed: string;
  TotalLen, Offset: Integer;
  AnsiProto: AnsiString;
begin
  TotalLen := 0;
  ProtoList := aProtocols.Split([',']);
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
  const in_proto: PByte; in_proto_len: Cardinal; arg: Pointer): Integer; cdecl;
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

  FillChar(Info, SizeOf(Info), 0);
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

constructor TOpenSSLContext.Create(aLibrary: ISSLLibrary; aType: TSSLContextType);
var
  Method: PSSL_METHOD;
begin
  inherited Create;
  FLibrary := aLibrary;
  FContextType := aType;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 9;
  FServerName := '';
  FCipherList := 'HIGH:!aNULL:!MD5:!RC4';
  FCipherSuites := 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256';
  FALPNProtocols := '';
  SetLength(FALPNWireData, 0);
  FSessionCacheEnabled := True;
  FSessionTimeout := 300;
  FSessionCacheSize := 20480;
  FOptions := [ssoEnableSessionCache, ssoEnableSessionTickets,
               ssoDisableCompression, ssoDisableRenegotiation,
               ssoNoSSLv2, ssoNoSSLv3];
  
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

procedure TOpenSSLContext.SetProtocolVersions(aVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := aVersions;
  ApplyProtocolVersions;
end;

function TOpenSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

// ============================================================================
// ISSLContext - 证书和密钥管理
// ============================================================================

procedure TOpenSSLContext.LoadCertificate(const aFileName: string);
var
  FileNameA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCertificate'
    );
  
  FileNameA := AnsiString(aFileName);
  if SSL_CTX_use_certificate_file(FSSLContext, PAnsiChar(FileNameA), SSL_FILETYPE_PEM) <> 1 then
  begin
    TSecurityLog.Error('OpenSSL', Format('Failed to load certificate: %s', [aFileName]));
    RaiseSSLCertError(
      Format('Failed to load certificate from file: %s', [aFileName]),
      'TOpenSSLContext.LoadCertificate'
    );
  end;
  TSecurityLog.Info('OpenSSL', Format('Loaded certificate from file: %s', [aFileName]));
end;

procedure TOpenSSLContext.LoadCertificate(aStream: TStream);
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
  Cert: PX509;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCertificate'
    );
  
  Size := aStream.Size - aStream.Position;
  SetLength(Data, Size);
  aStream.Read(Data[0], Size);
  
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

procedure TOpenSSLContext.LoadCertificate(aCert: ISSLCertificate);
var
  Cert: PX509;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCertificate'
    );
  
  if aCert = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Certificate parameter is nil',
      sslErrInvalidParam,
      'TOpenSSLContext.LoadCertificate'
    );
  
  Cert := PX509(aCert.GetNativeHandle);
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

procedure TOpenSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string = '');
var
  FileNameA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadPrivateKey'
    );
  
  FileNameA := AnsiString(aFileName);
  if SSL_CTX_use_PrivateKey_file(FSSLContext, PAnsiChar(FileNameA), SSL_FILETYPE_PEM) <> 1 then
    raise ESSLKeyException.CreateWithContext(
      Format('Failed to load private key from file: %s', [aFileName]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadPrivateKey',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
  
  // 验证私钥和证书是否匹配
  if SSL_CTX_check_private_key(FSSLContext) <> 1 then
  begin
    TSecurityLog.Error('OpenSSL', 'Private key does not match certificate');
    raise ESSLKeyException.CreateWithContext(
      'Private key does not match the loaded certificate',
      sslErrCertificate,
      'TOpenSSLContext.LoadPrivateKey',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
  end;
  TSecurityLog.Audit('OpenSSL', 'LoadPrivateKey', 'System', 'Private key loaded from file');
end;

procedure TOpenSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string = '');
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
  PKey: PEVP_PKEY;
  PassA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadPrivateKey'
    );

  if aStream = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Stream parameter is nil',
      sslErrInvalidParam,
      'TOpenSSLContext.LoadPrivateKey'
    );

  Size := aStream.Size - aStream.Position;
  SetLength(Data, Size);
  aStream.Read(Data[0], Size);

  BIO := BIO_new_mem_buf(@Data[0], Size);
  if BIO = nil then
    raise ESSLResourceException.CreateWithContext(
      'Failed to create BIO for private key',
      sslErrMemory,
      'TOpenSSLContext.LoadPrivateKey'
    );
  try
    // 若提供密码，通过userdata传递
    if aPassword <> '' then
    begin
      PassA := AnsiString(aPassword);
      if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
        SSL_CTX_set_default_passwd_cb_userdata(FSSLContext, PAnsiChar(PassA));
    end;

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
      if SSL_CTX_check_private_key(FSSLContext) <> 1 then
        raise ESSLKeyException.CreateWithContext(
          'Private key does not match the loaded certificate',
          sslErrCertificate,
          'TOpenSSLContext.LoadPrivateKey',
          Integer(GetLastOpenSSLError),
          sslOpenSSL
        );
    finally
      EVP_PKEY_free(PKey);
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadCertificatePEM(const aPEM: string);
var
  BIO: PBIO;
  Cert: PX509;
  PemA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCertificatePEM'
    );
  
  if aPEM = '' then
    raise ESSLInvalidArgument.CreateWithContext(
      'PEM string is empty',
      sslErrInvalidParam,
      'TOpenSSLContext.LoadCertificatePEM'
    );
  
  PemA := AnsiString(aPEM);
  BIO := BIO_new_mem_buf(PAnsiChar(PemA), Length(PemA));
  if BIO = nil then
    raise ESSLResourceException.CreateWithContext(
      'Failed to create BIO for PEM certificate',
      sslErrMemory,
      'TOpenSSLContext.LoadCertificatePEM'
    );
  
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

procedure TOpenSSLContext.LoadPrivateKeyPEM(const aPEM: string; const aPassword: string = '');
var
  BIO: PBIO;
  PKey: PEVP_PKEY;
  PemA, PassA: AnsiString;
  PassPtr: PAnsiChar;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadPrivateKeyPEM'
    );
  
  if aPEM = '' then
    raise ESSLInvalidArgument.CreateWithContext(
      'PEM string is empty',
      sslErrInvalidParam,
      'TOpenSSLContext.LoadPrivateKeyPEM'
    );
  
  PemA := AnsiString(aPEM);
  BIO := BIO_new_mem_buf(PAnsiChar(PemA), Length(PemA));
  if BIO = nil then
    raise ESSLResourceException.CreateWithContext(
      'Failed to create BIO for PEM private key',
      sslErrMemory,
      'TOpenSSLContext.LoadPrivateKeyPEM'
    );
  
  try
    PassPtr := nil;
    if aPassword <> '' then
    begin
      PassA := AnsiString(aPassword);
      PassPtr := PAnsiChar(PassA);
    end;
    
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
      
      if SSL_CTX_check_private_key(FSSLContext) <> 1 then
        raise ESSLKeyException.CreateWithContext(
          'Private key does not match the loaded certificate',
          sslErrCertificate,
          'TOpenSSLContext.LoadPrivateKeyPEM',
          Integer(GetLastOpenSSLError),
          sslOpenSSL
        );
      
      TSecurityLog.Audit('OpenSSL', 'LoadPrivateKeyPEM', 'System', 'Private key loaded from PEM string');
    finally
      EVP_PKEY_free(PKey);
    end;
  finally
    BIO_free(BIO);
  end;
end;

procedure TOpenSSLContext.LoadCAFile(const aFileName: string);
var
  FileNameA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCAFile'
    );
  
  FileNameA := AnsiString(aFileName);
  if SSL_CTX_load_verify_locations(FSSLContext, PAnsiChar(FileNameA), nil) <> 1 then
    raise ESSLCertificateLoadException.CreateWithContext(
      Format('Failed to load CA certificates from file: %s', [aFileName]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadCAFile',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
end;

procedure TOpenSSLContext.LoadCAPath(const aPath: string);
var
  PathA: AnsiString;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.LoadCAPath'
    );
  
  if not DirectoryExists(aPath) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('CA certificates directory not found: %s', [aPath]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadCAPath'
    );
  
  PathA := AnsiString(aPath);
  if SSL_CTX_load_verify_locations(FSSLContext, nil, PAnsiChar(PathA)) <> 1 then
    raise ESSLCertificateLoadException.CreateWithContext(
      Format('Failed to load CA certificates from directory: %s', [aPath]),
      sslErrLoadFailed,
      'TOpenSSLContext.LoadCAPath',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
end;

procedure TOpenSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
var
  Store: PX509_STORE;
begin
  if FSSLContext = nil then
    raise ESSLInitializationException.CreateWithContext(
      'SSL context not initialized',
      sslErrNotInitialized,
      'TOpenSSLContext.SetCertificateStore'
    );
  if aStore = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Certificate store parameter is nil',
      sslErrInvalidParam,
      'TOpenSSLContext.SetCertificateStore'
    );

  Store := PX509_STORE(aStore.GetNativeHandle);
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

procedure TOpenSSLContext.SetVerifyMode(aMode: TSSLVerifyModes);
begin
  FVerifyMode := aMode;
  ApplyVerifyMode;
end;

function TOpenSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TOpenSSLContext.SetVerifyDepth(aDepth: Integer);
begin
  FVerifyDepth := aDepth;
  if FSSLContext <> nil then
    if Assigned(SSL_CTX_set_verify_depth) then
      SSL_CTX_set_verify_depth(FSSLContext, aDepth);
end;

function TOpenSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TOpenSSLContext.SetVerifyCallback(aCallback: TSSLVerifyCallback);
begin
  FVerifyCallback := aCallback;
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

procedure TOpenSSLContext.SetCipherList(const aCipherList: string);
var
  CipherListA: AnsiString;
begin
  FCipherList := aCipherList;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_cipher_list) then
  begin
    CipherListA := AnsiString(aCipherList);
    SSL_CTX_set_cipher_list(FSSLContext, PAnsiChar(CipherListA));
  end;
end;

function TOpenSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TOpenSSLContext.SetCipherSuites(const aCipherSuites: string);
var
  CipherSuitesA: AnsiString;
begin
  FCipherSuites := aCipherSuites;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_ciphersuites) then
  begin
    CipherSuitesA := AnsiString(aCipherSuites);
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

procedure TOpenSSLContext.SetSessionCacheMode(aEnabled: Boolean);
var
  Mode: Int64;
begin
  FSessionCacheEnabled := aEnabled;
  
  if (FSSLContext <> nil) and Assigned(SSL_CTX_set_session_cache_mode) then
  begin
    if aEnabled then
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

procedure TOpenSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  FSessionTimeout := aTimeout;
  if FSSLContext <> nil then
    SSL_CTX_set_timeout(FSSLContext, aTimeout);
end;

function TOpenSSLContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TOpenSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  FSessionCacheSize := aSize;
  if (FSSLContext <> nil) and Assigned(SSL_CTX_sess_set_cache_size) and (aSize > 0) then
    SSL_CTX_sess_set_cache_size(FSSLContext, aSize);
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

procedure TOpenSSLContext.SetOptions(const aOptions: TSSLOptions);
begin
  FOptions := aOptions;
  ApplyOptions;
end;

function TOpenSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TOpenSSLContext.SetServerName(const aServerName: string);
begin
  FServerName := aServerName;
end;

function TOpenSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TOpenSSLContext.SetALPNProtocols(const aProtocols: string);
begin
  FALPNProtocols := Trim(aProtocols);
  FALPNWireData := BuildALPNWireData(FALPNProtocols);

  if FSSLContext = nil then Exit;

  if FALPNProtocols = '' then
  begin
    if Assigned(SSL_CTX_set_alpn_select_cb) then
      SSL_CTX_set_alpn_select_cb(FSSLContext, nil, nil);
    Exit;
  end;

  if not Assigned(SSL_CTX_set_alpn_protos) then
    raise ESSLConfigurationException.Create('ALPN is not supported by the current OpenSSL build');

  if (Length(FALPNWireData) = 0) or
     (SSL_CTX_set_alpn_protos(FSSLContext, @FALPNWireData[0], Length(FALPNWireData)) <> 0) then
    raise ESSLConfigurationException.CreateFmt('Failed to configure ALPN protocols: %s', [FALPNProtocols]);

  // 仅在服务端设置选择回调，客户端只发送候选列表
  if (FContextType <> sslCtxClient) and Assigned(SSL_CTX_set_alpn_select_cb) then
    SSL_CTX_set_alpn_select_cb(FSSLContext, @ALPNSelectCallback, nil);
end;

function TOpenSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

// ============================================================================
// ISSLContext - 回调设置
// ============================================================================

procedure TOpenSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  FPasswordCallback := aCallback;
  if FSSLContext = nil then Exit;
  if not Assigned(SSL_CTX_set_default_passwd_cb) then
    raise ESSLConfigurationException.Create('Password callback not supported by OpenSSL');
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

procedure TOpenSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  FInfoCallback := aCallback;
  if FSSLContext = nil then Exit;
  if not Assigned(SSL_CTX_set_info_callback) then
    raise ESSLConfigurationException.Create('Info callback not supported by OpenSSL');
  if Assigned(FInfoCallback) then
    SSL_CTX_set_info_callback(FSSLContext, @InfoCallbackThunk)
  else
    SSL_CTX_set_info_callback(FSSLContext, nil);
end;

// ============================================================================
// ISSLContext - 创建连接
// ============================================================================

function TOpenSSLContext.CreateConnection(aSocket: THandle): ISSLConnection;
begin
  if FSSLContext = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  try
    Result := TOpenSSLConnection.Create(Self, aSocket);
  except
    Result := nil;
  end;
end;

function TOpenSSLContext.CreateConnection(aStream: TStream): ISSLConnection;
begin
  if FSSLContext = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  try
    Result := TOpenSSLConnection.Create(Self, aStream);
  except
    Result := nil;
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

end.
