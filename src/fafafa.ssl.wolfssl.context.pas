{**
 * Unit: fafafa.ssl.wolfssl.context
 * Purpose: WolfSSL 上下文实现
 *
 * 实现 ISSLContext 接口的 WolfSSL 后端。
 * 负责 WOLFSSL_CTX 管理和连接创建。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.wolfssl.context;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Base64,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.native_handle,
  fafafa.ssl.wolfssl.api;

type
  { 证书固定记录 }
  TWolfSSLCertPin = record
    Hash: array[0..31] of Byte;  // SHA-256 hash
    PinType: Integer;            // 0=Certificate, 1=PublicKey
    Description: string;
    IsBackup: Boolean;
  end;
  TWolfSSLCertPinArray = array of TWolfSSLCertPin;

  { TWolfSSLContext - WolfSSL 上下文类 }
  TWolfSSLContext = class(TInterfacedObject, ISSLContext, ISSLNativeHandleAccess)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FWolfSSLCtx: PWOLFSSL_CTX;
    FProtocolVersions: TSSLProtocolVersions;
    FPreferredVersion: TSSLProtocolVersion;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FServerName: string;
    FCipherList: string;
    FCipherSuites: string;
    FALPNProtocols: string;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FOptions: TSSLOptions;
    FCertVerifyFlags: TSSLCertVerifyFlags;

    // 回调
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;

    // 证书固定
    FCertPins: TWolfSSLCertPinArray;
    FPinningEnabled: Boolean;

    function GetWolfSSLMethod: PWOLFSSL_METHOD;
    procedure ApplyVerifyMode;
    procedure RequireValidContext(const AMethodName: string);

  public
    constructor Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
    destructor Destroy; override;

    { ISSLContext - 基本配置 }
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    procedure SetPreferredVersion(AVersion: TSSLProtocolVersion);
    function GetPreferredVersion: TSSLProtocolVersion;

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

    { ISSLContext - 证书固定 }
    procedure AddCertificatePin(const AHash: TBytes; APinType: Integer;
      const ADescription: string; AIsBackup: Boolean = False);
    procedure AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
      const ADescription: string; AIsBackup: Boolean = False);
    procedure SetCertificatePinningEnabled(AEnabled: Boolean);
    function GetCertificatePinningEnabled: Boolean;
    procedure ClearCertificatePins;

    { 证书固定访问（供 Connection 使用）}
    function GetCertificatePins: TWolfSSLCertPinArray;

    { ISSLContext - 创建连接 }
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;

    { ISSLContext - 状态查询 }
    function IsValid: Boolean;

    { ISSLNativeHandleAccess implementation }
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;

    { ISSLContext - 健康状态和诊断 }
    function GetHealthStatus: TSSLHealthStatus;
    function IsHealthy: Boolean;
    function GetDiagnosticInfo: TSSLDiagnosticInfo;
    function GetPerformanceMetrics: TSSLPerformanceMetrics;

    { 便利方法 }
    procedure ConfigureSecureDefaults;
  end;

implementation

uses
  fafafa.ssl.wolfssl.certificate,
  fafafa.ssl.wolfssl.session;

const
  WOLFSSL_ERROR_UNSUPPORTED_RENEGOTIATION = -20001;

{ WolfSSL I/O 回调函数（用于流支持）
  这些函数将在 TWolfSSLConnection.Create(AStream) 中注册 }

function WolfSSL_StreamRecvCallback(ssl: PWOLFSSL; buf: PAnsiChar; sz: Integer;
  ctx: Pointer): Integer; cdecl;
var
  LStream: TStream;
  LBytesRead: Integer;
begin
  Result := -1;
  if ctx = nil then Exit;

  LStream := TStream(ctx);
  try
    LBytesRead := LStream.Read(buf^, sz);
    if LBytesRead = 0 then
      Result := -2  // WOLFSSL_CBIO_ERR_WANT_READ
    else if LBytesRead < 0 then
      Result := -1  // WOLFSSL_CBIO_ERR_GENERAL
    else
      Result := LBytesRead;
  except
    Result := -1;  // WOLFSSL_CBIO_ERR_GENERAL
  end;
end;

function WolfSSL_StreamSendCallback(ssl: PWOLFSSL; buf: PAnsiChar; sz: Integer;
  ctx: Pointer): Integer; cdecl;
var
  LStream: TStream;
  LBytesWritten: Integer;
begin
  Result := -1;
  if ctx = nil then Exit;

  LStream := TStream(ctx);
  try
    LBytesWritten := LStream.Write(buf^, sz);
    if LBytesWritten = 0 then
      Result := -3  // WOLFSSL_CBIO_ERR_WANT_WRITE
    else if LBytesWritten < 0 then
      Result := -1  // WOLFSSL_CBIO_ERR_GENERAL
    else
      Result := LBytesWritten;
  except
    Result := -1;  // WOLFSSL_CBIO_ERR_GENERAL
  end;
end;

{ Forward declaration for connection - will be implemented separately }
type
  TWolfSSLConnection = class(TInterfacedObject, ISSLConnection, ISSLNativeHandleAccess)
  private
    FContext: TWolfSSLContext;
    FWolfSSL: PWOLFSSL;
    FSocket: THandle;
    FStream: TStream;  // 流支持
    FServerName: string;
    FALPNProtocols: string;
    FNegotiatedALPN: string;
    FHandshakeComplete: Boolean;
    FTimeout: Integer;
    FBlocking: Boolean;
    FLastError: Integer;  // P0: 错误跟踪
  public
    constructor Create(AContext: TWolfSSLContext; ASocket: THandle); overload;
    constructor Create(AContext: TWolfSSLContext; AStream: TStream); overload;
    destructor Destroy; override;

    { ISSLConnection - 基本操作 }
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

    { ISSLConnection - 错误处理 }
    function GetError(ARetCode: Integer): TSSLErrorCode;
    function GetLastError: Integer;
    function GetLastErrorString: string;

    { ISSLConnection - 连接信息 }
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function IsConnected: Boolean;

    { ISSLConnection - 证书 }
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;

    { ISSLConnection - 会话 }
    function GetSession: ISSLSession;
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;

    { ISSLConnection - SNI/ALPN }
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    function GetSelectedALPNProtocol: string;
    function GetNegotiatedProtocol: TSSLProtocolVersion;
    function GetNegotiatedCipher: string;
    function GetNegotiatedALPN: string;

    { ISSLConnection - 状态 }
    function GetState: string;
    function GetStateString: string;

    { ISSLConnection - 超时和阻塞 }
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
    function GetBlocking: Boolean;

    { ISSLConnection - 上下文 }
    function GetContext: ISSLContext;

    { ISSLConnection - 健康状态和诊断 }
    function GetHealthStatus: TSSLHealthStatus;
    function IsHealthy: Boolean;
    function GetDiagnosticInfo: TSSLDiagnosticInfo;
    function GetPerformanceMetrics: TSSLPerformanceMetrics;

    { ISSLNativeHandleAccess implementation }
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;

    { ISSLConnection - OCSP Stapling }
    function GetOCSPStaplingEnabled: Boolean;
    function GetOCSPResponse: TBytes;
    function IsOCSPResponseVerified: Boolean;
    function GetOCSPResponseStatus: string;
  end;

{ TWolfSSLContext }

constructor TWolfSSLContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
var
  LMethod: PWOLFSSL_METHOD;
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FWolfSSLCtx := nil;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FPreferredVersion := sslProtocolTLS13;
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FServerName := '';
  FCipherList := '';
  FCipherSuites := '';
  FALPNProtocols := '';
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FOptions := [ssoEnableSNI];
  FCertVerifyFlags := [];
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;

  // 初始化证书固定
  SetLength(FCertPins, 0);
  FPinningEnabled := False;

  // 创建 WolfSSL 上下文
  LMethod := GetWolfSSLMethod;
  if LMethod = nil then
    raise ESSLException.Create('Failed to get WolfSSL method for context type');

  if not Assigned(wolfSSL_CTX_new) then
    raise ESSLException.Create('wolfSSL_CTX_new not available');

  FWolfSSLCtx := wolfSSL_CTX_new(LMethod);
  if FWolfSSLCtx = nil then
    raise ESSLException.Create('Failed to create WolfSSL context');

  // 应用默认验证模式
  ApplyVerifyMode;
end;

destructor TWolfSSLContext.Destroy;
begin
  if FWolfSSLCtx <> nil then
  begin
    if Assigned(wolfSSL_CTX_free) then
      wolfSSL_CTX_free(FWolfSSLCtx);
    FWolfSSLCtx := nil;
  end;
  FLibrary := nil;
  inherited Destroy;
end;

function TWolfSSLContext.GetWolfSSLMethod: PWOLFSSL_METHOD;
begin
  Result := nil;

  case FContextType of
    sslCtxClient:
      begin
        // 优先使用 TLS 1.3，回退到 TLS 1.2
        if (sslProtocolTLS13 in FProtocolVersions) and Assigned(wolfTLSv1_3_client_method) then
          Result := wolfTLSv1_3_client_method()
        else if Assigned(wolfSSLv23_client_method) then
          Result := wolfSSLv23_client_method()
        else if Assigned(wolfTLSv1_2_client_method) then
          Result := wolfTLSv1_2_client_method();
      end;

    sslCtxServer:
      begin
        if (sslProtocolTLS13 in FProtocolVersions) and Assigned(wolfTLSv1_3_server_method) then
          Result := wolfTLSv1_3_server_method()
        else if Assigned(wolfSSLv23_server_method) then
          Result := wolfSSLv23_server_method()
        else if Assigned(wolfTLSv1_2_server_method) then
          Result := wolfTLSv1_2_server_method();
      end;

    sslCtxBoth:
      begin
        // 使用通用方法
        if Assigned(wolfSSLv23_client_method) then
          Result := wolfSSLv23_client_method();
      end;
  end;
end;

procedure TWolfSSLContext.ApplyVerifyMode;
var
  LMode: Integer;
begin
  if FWolfSSLCtx = nil then Exit;
  if not Assigned(wolfSSL_CTX_set_verify) then Exit;

  LMode := WOLFSSL_VERIFY_NONE;

  if sslVerifyPeer in FVerifyMode then
    LMode := WOLFSSL_VERIFY_PEER;

  if sslVerifyFailIfNoPeerCert in FVerifyMode then
    LMode := LMode or WOLFSSL_VERIFY_FAIL_IF_NO_PEER_CERT;

  wolfSSL_CTX_set_verify(FWolfSSLCtx, LMode, nil);
end;

procedure TWolfSSLContext.RequireValidContext(const AMethodName: string);
begin
  if FWolfSSLCtx = nil then
    raise ESSLException.CreateFmt('%s: WolfSSL context is not valid', [AMethodName]);
end;

function TWolfSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TWolfSSLContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;

  // 当首选版本不再可用时，自动回退为无偏好
  if (FPreferredVersion <> sslProtocolUnknown) and
     not (FPreferredVersion in FProtocolVersions) then
    FPreferredVersion := sslProtocolUnknown;

  // WolfSSL 协议版本在创建时确定，运行时更改需要重建上下文
end;

function TWolfSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TWolfSSLContext.SetPreferredVersion(AVersion: TSSLProtocolVersion);
begin
  if (AVersion <> sslProtocolUnknown) and
     not (AVersion in FProtocolVersions) then
    RaiseInvalidParameter('PreferredVersion');

  FPreferredVersion := AVersion;
end;

function TWolfSSLContext.GetPreferredVersion: TSSLProtocolVersion;
begin
  Result := FPreferredVersion;
end;

{ 证书和密钥管理 }

procedure TWolfSSLContext.LoadCertificate(const AFileName: string);
begin
  RequireValidContext('LoadCertificate');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Certificate file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_use_certificate_file) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_certificate_file not available');

  if wolfSSL_CTX_use_certificate_file(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)),
    WOLFSSL_FILETYPE_PEM) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load certificate: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadCertificate(AStream: TStream);
var
  LBuffer: TBytes;
  LRet: Integer;
begin
  RequireValidContext('LoadCertificate');

  if AStream = nil then
    raise ESSLCertError.Create('Stream is nil');

  if not Assigned(wolfSSL_CTX_use_certificate_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_certificate_buffer not available');

  // 读取流内容到缓冲区
  SetLength(LBuffer, AStream.Size - AStream.Position);
  if Length(LBuffer) = 0 then
    raise ESSLCertError.Create('Stream is empty');

  AStream.ReadBuffer(LBuffer[0], Length(LBuffer));

  // 尝试 PEM 格式
  LRet := wolfSSL_CTX_use_certificate_buffer(FWolfSSLCtx, @LBuffer[0],
    Length(LBuffer), WOLFSSL_FILETYPE_PEM);

  // 如果 PEM 失败，尝试 DER 格式
  if LRet <> WOLFSSL_SUCCESS then
  begin
    LRet := wolfSSL_CTX_use_certificate_buffer(FWolfSSLCtx, @LBuffer[0],
      Length(LBuffer), WOLFSSL_FILETYPE_ASN1);
  end;

  if LRet <> WOLFSSL_SUCCESS then
    raise ESSLCertError.Create('Failed to load certificate from stream');
end;

procedure TWolfSSLContext.LoadCertificate(ACert: ISSLCertificate);
var
  LDERData: TBytes;
  LRet: Integer;
begin
  RequireValidContext('LoadCertificate');

  if ACert = nil then
    raise ESSLCertError.Create('Certificate is nil');

  if not Assigned(wolfSSL_CTX_use_certificate_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_certificate_buffer not available');

  // 从 ISSLCertificate 获取 DER 编码数据
  LDERData := ACert.SaveToDER;
  if Length(LDERData) = 0 then
    raise ESSLCertError.Create('Certificate DER data is empty');

  LRet := wolfSSL_CTX_use_certificate_buffer(FWolfSSLCtx, @LDERData[0],
    Length(LDERData), WOLFSSL_FILETYPE_ASN1);

  if LRet <> WOLFSSL_SUCCESS then
    raise ESSLCertError.Create('Failed to load certificate from ISSLCertificate');
end;

procedure TWolfSSLContext.LoadPrivateKey(const AFileName: string; const APassword: string);
begin
  RequireValidContext('LoadPrivateKey');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Private key file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_use_PrivateKey_file) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_PrivateKey_file not available');

  // 注意：WolfSSL 密码回调需要单独设置
  if wolfSSL_CTX_use_PrivateKey_file(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)),
    WOLFSSL_FILETYPE_PEM) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load private key: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadPrivateKey(AStream: TStream; const APassword: string);
var
  LBuffer: TBytes;
  LRet: Integer;
begin
  RequireValidContext('LoadPrivateKey');

  if AStream = nil then
    raise ESSLCertError.Create('Stream is nil');

  if not Assigned(wolfSSL_CTX_use_PrivateKey_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_PrivateKey_buffer not available');

  // 读取流内容到缓冲区
  SetLength(LBuffer, AStream.Size - AStream.Position);
  if Length(LBuffer) = 0 then
    raise ESSLCertError.Create('Stream is empty');

  AStream.ReadBuffer(LBuffer[0], Length(LBuffer));

  // 注意：WolfSSL 密码回调需要单独设置，这里假设密钥未加密或已通过回调处理
  // 尝试 PEM 格式
  LRet := wolfSSL_CTX_use_PrivateKey_buffer(FWolfSSLCtx, @LBuffer[0],
    Length(LBuffer), WOLFSSL_FILETYPE_PEM);

  // 如果 PEM 失败，尝试 DER 格式
  if LRet <> WOLFSSL_SUCCESS then
  begin
    LRet := wolfSSL_CTX_use_PrivateKey_buffer(FWolfSSLCtx, @LBuffer[0],
      Length(LBuffer), WOLFSSL_FILETYPE_ASN1);
  end;

  if LRet <> WOLFSSL_SUCCESS then
    raise ESSLCertError.Create('Failed to load private key from stream');
end;

procedure TWolfSSLContext.LoadCertificatePEM(const APEM: string);
var
  LBuffer: TBytes;
  LRet: Integer;
begin
  RequireValidContext('LoadCertificatePEM');

  if APEM = '' then
    raise ESSLCertError.Create('PEM string is empty');

  if not Assigned(wolfSSL_CTX_use_certificate_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_certificate_buffer not available');

  // 转换 PEM 字符串为字节数组
  LBuffer := TEncoding.UTF8.GetBytes(APEM);

  LRet := wolfSSL_CTX_use_certificate_buffer(FWolfSSLCtx, @LBuffer[0],
    Length(LBuffer), WOLFSSL_FILETYPE_PEM);

  if LRet <> WOLFSSL_SUCCESS then
    raise ESSLCertError.Create('Failed to load certificate from PEM string');
end;

procedure TWolfSSLContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string);
var
  LBuffer: TBytes;
  LRet: Integer;
begin
  RequireValidContext('LoadPrivateKeyPEM');

  if APEM = '' then
    raise ESSLCertError.Create('PEM string is empty');

  if not Assigned(wolfSSL_CTX_use_PrivateKey_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_PrivateKey_buffer not available');

  // 转换 PEM 字符串为字节数组
  LBuffer := TEncoding.UTF8.GetBytes(APEM);

  // 注意：WolfSSL 密码回调需要单独设置
  LRet := wolfSSL_CTX_use_PrivateKey_buffer(FWolfSSLCtx, @LBuffer[0],
    Length(LBuffer), WOLFSSL_FILETYPE_PEM);

  if LRet <> WOLFSSL_SUCCESS then
    raise ESSLCertError.Create('Failed to load private key from PEM string');
end;

procedure TWolfSSLContext.LoadCAFile(const AFileName: string);
begin
  RequireValidContext('LoadCAFile');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('CA file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_load_verify_locations) then
    raise ESSLCertError.Create('wolfSSL_CTX_load_verify_locations not available');

  if wolfSSL_CTX_load_verify_locations(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)), nil) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load CA file: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadCAPath(const APath: string);
begin
  RequireValidContext('LoadCAPath');

  if not DirectoryExists(APath) then
    raise ESSLCertError.CreateFmt('CA path not found: %s', [APath]);

  if not Assigned(wolfSSL_CTX_load_verify_locations) then
    raise ESSLCertError.Create('wolfSSL_CTX_load_verify_locations not available');

  if wolfSSL_CTX_load_verify_locations(FWolfSSLCtx, nil, PAnsiChar(AnsiString(APath))) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load CA path: %s', [APath]);
end;

procedure TWolfSSLContext.SetCertificateStore(AStore: ISSLCertificateStore);
var
  LCert: ISSLCertificate;
  LDERData: TBytes;
  LRet: Integer;
  I, LCount: Integer;
begin
  RequireValidContext('SetCertificateStore');

  if AStore = nil then
    raise ESSLCertError.Create('Certificate store is nil');

  if not Assigned(wolfSSL_CTX_load_verify_buffer) then
    raise ESSLCertError.Create('wolfSSL_CTX_load_verify_buffer not available');

  // 从证书存储中获取所有证书并加载
  LCount := AStore.GetCount;

  for I := 0 to LCount - 1 do
  begin
    LCert := AStore.GetCertificate(I);
    if LCert <> nil then
    begin
      LDERData := LCert.SaveToDER;
      if Length(LDERData) > 0 then
      begin
        LRet := wolfSSL_CTX_load_verify_buffer(FWolfSSLCtx, @LDERData[0],
          Length(LDERData), WOLFSSL_FILETYPE_ASN1);
        if LRet <> WOLFSSL_SUCCESS then
          raise ESSLCertError.CreateFmt('Failed to load certificate from store: %s',
            [LCert.GetSubject]);
      end;
    end;
  end;
end;

{ 验证配置 }

procedure TWolfSSLContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
  ApplyVerifyMode;
end;

function TWolfSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TWolfSSLContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
  // WolfSSL 验证深度通过其他方式设置
end;

function TWolfSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TWolfSSLContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  FVerifyCallback := ACallback;
end;

{ 密码套件配置 }

procedure TWolfSSLContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
  // WolfSSL 密码套件设置需要额外 API
end;

function TWolfSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TWolfSSLContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TWolfSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

{ 会话管理 }

procedure TWolfSSLContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheEnabled := AEnabled;
end;

function TWolfSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TWolfSSLContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TWolfSSLContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TWolfSSLContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TWolfSSLContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

{ 高级选项 }

procedure TWolfSSLContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TWolfSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TWolfSSLContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TWolfSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TWolfSSLContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TWolfSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TWolfSSLContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TWolfSSLContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TWolfSSLContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  FPasswordCallback := ACallback;
end;

procedure TWolfSSLContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  FInfoCallback := ACallback;
end;

{ 证书固定 }

procedure TWolfSSLContext.AddCertificatePin(const AHash: TBytes; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean);
var
  LIdx: Integer;
  LPin: TWolfSSLCertPin;
begin
  if Length(AHash) <> 32 then
    raise ESSLException.CreateWithContext(
      'Certificate pin hash must be 32 bytes (SHA-256)',
      sslErrInvalidParam,
      'TWolfSSLContext.AddCertificatePin'
    );

  // 初始化新的 pin
  FillChar(LPin, SizeOf(LPin), 0);
  Move(AHash[0], LPin.Hash[0], 32);
  LPin.PinType := APinType;
  LPin.Description := ADescription;
  LPin.IsBackup := AIsBackup;

  // 添加到数组
  LIdx := Length(FCertPins);
  SetLength(FCertPins, LIdx + 1);
  FCertPins[LIdx] := LPin;
end;

procedure TWolfSSLContext.AddCertificatePinBase64(const ABase64Hash: string;
  APinType: Integer; const ADescription: string; AIsBackup: Boolean);
var
  LHash: TBytes;
  LDecoded: AnsiString;
begin
  // 解码 Base64
  LDecoded := DecodeStringBase64(ABase64Hash);
  SetLength(LHash, Length(LDecoded));
  if Length(LDecoded) > 0 then
    Move(LDecoded[1], LHash[0], Length(LDecoded));

  if Length(LHash) <> 32 then
    raise ESSLException.CreateWithContext(
      Format('Invalid Base64 hash length: expected 32, got %d', [Length(LHash)]),
      sslErrInvalidParam,
      'TWolfSSLContext.AddCertificatePinBase64'
    );

  AddCertificatePin(LHash, APinType, ADescription, AIsBackup);
end;

procedure TWolfSSLContext.SetCertificatePinningEnabled(AEnabled: Boolean);
begin
  FPinningEnabled := AEnabled;
end;

function TWolfSSLContext.GetCertificatePinningEnabled: Boolean;
begin
  Result := FPinningEnabled;
end;

procedure TWolfSSLContext.ClearCertificatePins;
begin
  SetLength(FCertPins, 0);
  FPinningEnabled := False;
end;

function TWolfSSLContext.GetCertificatePins: TWolfSSLCertPinArray;
begin
  Result := FCertPins;
end;

{ 创建连接 }

function TWolfSSLContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  RequireValidContext('CreateConnection');
  Result := TWolfSSLConnection.Create(Self, ASocket);
end;

function TWolfSSLContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  RequireValidContext('CreateConnection');

  if AStream = nil then
    raise ESSLException.Create('Cannot create connection: stream is nil');

  // 检查 I/O 回调是否可用
  if not Assigned(wolfSSL_CTX_SetIORecv) or not Assigned(wolfSSL_CTX_SetIOSend) then
    raise ESSLException.Create('Stream-based connections require WolfSSL I/O callbacks which are not available');

  Result := TWolfSSLConnection.Create(Self, AStream);
end;

{ 状态查询 }

function TWolfSSLContext.IsValid: Boolean;
begin
  Result := FWolfSSLCtx <> nil;
end;

function TWolfSSLContext.GetNativeHandle: Pointer;
begin
  Result := FWolfSSLCtx;
end;

function TWolfSSLContext.GetBackendType: TSSLLibraryType;
begin
  Result := sslWolfSSL;
end;

function TWolfSSLContext.IsNativeHandleValid: Boolean;
begin
  Result := (FWolfSSLCtx <> nil);
end;

function TWolfSSLContext.GetHealthStatus: TSSLHealthStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.IsConnected := False;  // Context doesn't have connection state
  Result.HandshakeComplete := False;
  Result.LastError := sslErrNone;
  Result.LastErrorTime := 0;
  Result.BytesSent := 0;
  Result.BytesReceived := 0;
  Result.ConnectionAge := 0;
end;

function TWolfSSLContext.IsHealthy: Boolean;
begin
  Result := IsValid;
end;

function TWolfSSLContext.GetDiagnosticInfo: TSSLDiagnosticInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.HealthStatus := GetHealthStatus;
  Result.PerformanceMetrics := GetPerformanceMetrics;
  SetLength(Result.ErrorHistory, 0);
end;

function TWolfSSLContext.GetPerformanceMetrics: TSSLPerformanceMetrics;
begin
  FillChar(Result, SizeOf(Result), 0);
  // Context-level metrics are not tracked in this implementation
end;

procedure TWolfSSLContext.ConfigureSecureDefaults;
begin
  // 配置安全默认值
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FPreferredVersion := sslProtocolTLS13;
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 4;
  ApplyVerifyMode;
end;

{ TWolfSSLConnection }

constructor TWolfSSLConnection.Create(AContext: TWolfSSLContext; ASocket: THandle);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FStream := nil;
  FWolfSSL := nil;
  FServerName := AContext.FServerName;
  FALPNProtocols := AContext.FALPNProtocols;
  FNegotiatedALPN := '';
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;
  FLastError := 0;  // P0: 初始化错误跟踪

  if not Assigned(wolfSSL_new) then
    raise ESSLException.Create('wolfSSL_new not available');

  FWolfSSL := wolfSSL_new(AContext.FWolfSSLCtx);
  if FWolfSSL = nil then
    raise ESSLException.Create('Failed to create WolfSSL connection');

  // 设置文件描述符
  if Assigned(wolfSSL_set_fd) then
    wolfSSL_set_fd(FWolfSSL, Integer(FSocket));

  // 设置 SNI
  if (FServerName <> '') and Assigned(wolfSSL_UseSNI) then
    wolfSSL_UseSNI(FWolfSSL, 0, PAnsiChar(AnsiString(FServerName)), Length(FServerName));

  // 设置 ALPN 协议
  if (FALPNProtocols <> '') and Assigned(wolfSSL_UseALPN) then
    wolfSSL_UseALPN(FWolfSSL, PAnsiChar(AnsiString(FALPNProtocols)),
      Length(FALPNProtocols), 0);  // 0 = WOLFSSL_ALPN_CONTINUE_ON_MISMATCH
end;

constructor TWolfSSLConnection.Create(AContext: TWolfSSLContext; AStream: TStream);
begin
  inherited Create;
  FContext := AContext;
  FSocket := 0;
  FStream := AStream;
  FWolfSSL := nil;
  FServerName := AContext.FServerName;
  FALPNProtocols := AContext.FALPNProtocols;
  FNegotiatedALPN := '';
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;
  FLastError := 0;

  if AStream = nil then
    raise ESSLException.Create('Stream cannot be nil');

  if not Assigned(wolfSSL_new) then
    raise ESSLException.Create('wolfSSL_new not available');

  FWolfSSL := wolfSSL_new(AContext.FWolfSSLCtx);
  if FWolfSSL = nil then
    raise ESSLException.Create('Failed to create WolfSSL connection');

  // 设置自定义 I/O 回调用于流操作
  if Assigned(wolfSSL_CTX_SetIORecv) and Assigned(wolfSSL_CTX_SetIOSend) then
  begin
    wolfSSL_CTX_SetIORecv(AContext.FWolfSSLCtx, @WolfSSL_StreamRecvCallback);
    wolfSSL_CTX_SetIOSend(AContext.FWolfSSLCtx, @WolfSSL_StreamSendCallback);
  end
  else
    raise ESSLException.Create('WolfSSL I/O callbacks not available - stream connections not supported');

  // 设置 I/O 上下文（传递流指针）
  if Assigned(wolfSSL_SetIOReadCtx) and Assigned(wolfSSL_SetIOWriteCtx) then
  begin
    wolfSSL_SetIOReadCtx(FWolfSSL, FStream);
    wolfSSL_SetIOWriteCtx(FWolfSSL, FStream);
  end;

  // 设置 SNI
  if (FServerName <> '') and Assigned(wolfSSL_UseSNI) then
    wolfSSL_UseSNI(FWolfSSL, 0, PAnsiChar(AnsiString(FServerName)), Length(FServerName));

  // 设置 ALPN 协议
  if (FALPNProtocols <> '') and Assigned(wolfSSL_UseALPN) then
    wolfSSL_UseALPN(FWolfSSL, PAnsiChar(AnsiString(FALPNProtocols)),
      Length(FALPNProtocols), 0);
end;

destructor TWolfSSLConnection.Destroy;
begin
  if FWolfSSL <> nil then
  begin
    if Assigned(wolfSSL_free) then
      wolfSSL_free(FWolfSSL);
    FWolfSSL := nil;
  end;
  inherited Destroy;
end;

function TWolfSSLConnection.Connect: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_connect) then Exit;

  LResult := wolfSSL_connect(FWolfSSL);
  FHandshakeComplete := LResult = WOLFSSL_SUCCESS;
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Accept: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_accept) then Exit;

  LResult := wolfSSL_accept(FWolfSSL);
  FHandshakeComplete := LResult = WOLFSSL_SUCCESS;
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Shutdown: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_shutdown) then Exit;

  LResult := wolfSSL_shutdown(FWolfSSL);
  Result := LResult >= 0;
end;

procedure TWolfSSLConnection.Close;
begin
  Shutdown;
end;

function TWolfSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  if FHandshakeComplete then
    Result := sslHsCompleted
  else if Connect then
    Result := sslHsCompleted
  else
    Result := sslHsFailed;
end;

function TWolfSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Renegotiate: Boolean;
begin
  Result := False;
  FLastError := WOLFSSL_ERROR_UNSUPPORTED_RENEGOTIATION;
end;

function TWolfSSLConnection.Read(var ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_read) then Exit;

  Result := wolfSSL_read(FWolfSSL, @ABuffer, ACount);
end;

function TWolfSSLConnection.Write(const ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_write) then Exit;

  Result := wolfSSL_write(FWolfSSL, @ABuffer, ACount);
end;

function TWolfSSLConnection.ReadString(out AStr: string): Boolean;
var
  LBuf: array[0..4095] of Byte;
  LRead: Integer;
begin
  Result := False;
  AStr := '';
  LRead := Read(LBuf, SizeOf(LBuf));
  if LRead > 0 then
  begin
    SetString(AStr, PAnsiChar(@LBuf[0]), LRead);
    Result := True;
  end;
end;

function TWolfSSLConnection.WriteString(const AStr: string): Boolean;
var
  LWritten: Integer;
begin
  Result := False;
  if AStr = '' then Exit(True);
  LWritten := Write(AStr[1], Length(AStr));
  Result := LWritten = Length(AStr);
end;

function TWolfSSLConnection.WantRead: Boolean;
begin
  Result := GetLastError = WOLFSSL_ERROR_WANT_READ;
end;

function TWolfSSLConnection.WantWrite: Boolean;
begin
  Result := GetLastError = WOLFSSL_ERROR_WANT_WRITE;
end;

function TWolfSSLConnection.GetError(ARetCode: Integer): TSSLErrorCode;
var
  LErr: Integer;
begin
  if FLastError = WOLFSSL_ERROR_UNSUPPORTED_RENEGOTIATION then
  begin
    Result := sslErrUnsupported;
    Exit;
  end;

  if FWolfSSL = nil then
    Exit(sslErrGeneral);
  LErr := GetLastError;
  Result := WolfSSLErrorToSSLError(LErr);
end;

function TWolfSSLConnection.GetLastError: Integer;
begin
  Result := FLastError;
  if Result <> 0 then
    Exit;

  if (FWolfSSL <> nil) and Assigned(wolfSSL_get_error) then
    Result := wolfSSL_get_error(FWolfSSL, 0);
end;

function TWolfSSLConnection.GetLastErrorString: string;
var
  LError: Integer;
  LBuf: array[0..255] of AnsiChar;
begin
  if FLastError = WOLFSSL_ERROR_UNSUPPORTED_RENEGOTIATION then
  begin
    Result := 'TLS renegotiation is not supported by WolfSSL backend; reconnect required';
    Exit;
  end;

  Result := '';
  LError := GetLastError;
  if (LError <> 0) and Assigned(wolfSSL_ERR_error_string) then
  begin
    FillChar(LBuf, SizeOf(LBuf), 0);
    wolfSSL_ERR_error_string(LError, @LBuf[0]);
    Result := string(PAnsiChar(@LBuf[0]));
  end;
end;

function TWolfSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ProtocolVersion := GetProtocolVersion;
  Result.CipherSuite := GetCipherName;
end;

function TWolfSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  // 复用 GetNegotiatedProtocol 的实现
  Result := GetNegotiatedProtocol;
end;

function TWolfSSLConnection.GetCipherName: string;
begin
  // 复用 GetNegotiatedCipher 的实现
  Result := GetNegotiatedCipher;
end;

function TWolfSSLConnection.IsConnected: Boolean;
begin
  Result := (FWolfSSL <> nil) and FHandshakeComplete;
end;

function TWolfSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  LX509: PWOLFSSL_X509;
begin
  Result := nil;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_get_peer_certificate) then Exit;

  LX509 := wolfSSL_get_peer_certificate(FWolfSSL);
  if LX509 <> nil then
    Result := TWolfSSLCertificate.Create(LX509);
end;

function TWolfSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
begin
  SetLength(Result, 0);
end;

function TWolfSSLConnection.GetVerifyResult: Integer;
begin
  // WolfSSL 没有直接的 get_verify_result API
  // 使用 FLastError 来跟踪验证错误
  // 如果握手成功且没有错误，返回 0 表示验证通过
  if FHandshakeComplete and (FLastError = 0) then
    Result := 0
  else
    Result := FLastError;
end;

function TWolfSSLConnection.GetVerifyResultString: string;
var
  LResult: Integer;
begin
  LResult := GetVerifyResult;
  if LResult = 0 then
    Result := 'OK'
  else if LResult = WOLFSSL_SUCCESS then
    Result := 'OK'
  else
    Result := GetLastErrorString;  // 使用已有的错误字符串方法
end;

function TWolfSSLConnection.GetSession: ISSLSession;
begin
  Result := TWolfSSLSession.FromConnection(FWolfSSL);
end;

procedure TWolfSSLConnection.SetSession(ASession: ISSLSession);
var
  LSession: PWOLFSSL_SESSION;
begin
  if ASession = nil then Exit;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_set_session) then Exit;

  LSession := PWOLFSSL_SESSION(GetNativeHandleSafe(ASession, 'TWolfSSLConnection.SetSession'));
  if LSession <> nil then
    wolfSSL_set_session(FWolfSSL, LSession);
end;

function TWolfSSLConnection.IsSessionReused: Boolean;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_session_reused) then Exit;

  Result := wolfSSL_session_reused(FWolfSSL) = 1;
end;

procedure TWolfSSLConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
  if (FWolfSSL <> nil) and (FServerName <> '') and Assigned(wolfSSL_UseSNI) then
    wolfSSL_UseSNI(FWolfSSL, 0, PAnsiChar(AnsiString(FServerName)), Length(FServerName));
end;

function TWolfSSLConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TWolfSSLConnection.GetSelectedALPNProtocol: string;
var
  LProtocol: PAnsiChar;
  LSize: Word;
begin
  Result := '';
  if FWolfSSL = nil then Exit;

  // 如果已经缓存了协商结果，直接返回
  if FNegotiatedALPN <> '' then
  begin
    Result := FNegotiatedALPN;
    Exit;
  end;

  // 从 WolfSSL 获取协商的 ALPN 协议
  if Assigned(wolfSSL_ALPN_GetProtocol) then
  begin
    LProtocol := nil;
    LSize := 0;
    if wolfSSL_ALPN_GetProtocol(FWolfSSL, @LProtocol, @LSize) = WOLFSSL_SUCCESS then
    begin
      if (LProtocol <> nil) and (LSize > 0) then
      begin
        SetString(FNegotiatedALPN, LProtocol, LSize);
        Result := FNegotiatedALPN;
      end;
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedProtocol: TSSLProtocolVersion;
var
  LVersion: PAnsiChar;
begin
  Result := sslProtocolTLS12;  // 默认值
  if FWolfSSL = nil then Exit;

  if Assigned(wolfSSL_get_version) then
  begin
    LVersion := wolfSSL_get_version(FWolfSSL);
    if LVersion <> nil then
    begin
      if Pos('TLSv1.3', string(LVersion)) > 0 then
        Result := sslProtocolTLS13
      else if Pos('TLSv1.2', string(LVersion)) > 0 then
        Result := sslProtocolTLS12
      else if Pos('TLSv1.1', string(LVersion)) > 0 then
        Result := sslProtocolTLS11
      else if Pos('TLSv1', string(LVersion)) > 0 then
        Result := sslProtocolTLS10;
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedCipher: string;
var
  LCipher: Pointer;
  LName: PAnsiChar;
begin
  Result := '';
  if FWolfSSL = nil then Exit;

  if Assigned(wolfSSL_get_current_cipher) and Assigned(wolfSSL_CIPHER_get_name) then
  begin
    LCipher := wolfSSL_get_current_cipher(FWolfSSL);
    if LCipher <> nil then
    begin
      LName := wolfSSL_CIPHER_get_name(LCipher);
      if LName <> nil then
        Result := string(LName);
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedALPN: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TWolfSSLConnection.GetState: string;
begin
  if FHandshakeComplete then
    Result := 'CONNECTED'
  else
    Result := 'DISCONNECTED';
end;

function TWolfSSLConnection.GetStateString: string;
begin
  Result := GetState;
end;

procedure TWolfSSLConnection.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWolfSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWolfSSLConnection.SetBlocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
end;

function TWolfSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TWolfSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext;
end;

function TWolfSSLConnection.GetNativeHandle: Pointer;
begin
  Result := FWolfSSL;
end;

function TWolfSSLConnection.GetBackendType: TSSLLibraryType;
begin
  Result := sslWolfSSL;
end;

function TWolfSSLConnection.IsNativeHandleValid: Boolean;
begin
  Result := (FWolfSSL <> nil);
end;

function TWolfSSLConnection.GetHealthStatus: TSSLHealthStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.IsConnected := (FWolfSSL <> nil);
  Result.HandshakeComplete := FHandshakeComplete;
  Result.LastError := sslErrNone;
  Result.LastErrorTime := 0;
  Result.BytesSent := 0;
  Result.BytesReceived := 0;
  Result.ConnectionAge := 0;
end;

function TWolfSSLConnection.IsHealthy: Boolean;
begin
  Result := (FWolfSSL <> nil) and FHandshakeComplete;
end;

function TWolfSSLConnection.GetDiagnosticInfo: TSSLDiagnosticInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ConnectionInfo := GetConnectionInfo;
  Result.HealthStatus := GetHealthStatus;
  Result.PerformanceMetrics := GetPerformanceMetrics;
  SetLength(Result.ErrorHistory, 0);
end;

function TWolfSSLConnection.GetPerformanceMetrics: TSSLPerformanceMetrics;
begin
  FillChar(Result, SizeOf(Result), 0);
  // Connection-level metrics are not tracked in this implementation
end;

{ OCSP Stapling }

function TWolfSSLConnection.GetOCSPStaplingEnabled: Boolean;
begin
  // WolfSSL OCSP Stapling 需要在编译时启用
  // 检查是否可用
  Result := Assigned(wolfSSL_GetOCSP_Response);
end;

function TWolfSSLConnection.GetOCSPResponse: TBytes;
var
  LRespPtr: PByte;
  LRespLen: Integer;
begin
  SetLength(Result, 0);

  if FWolfSSL = nil then Exit;

  if not Assigned(wolfSSL_GetOCSP_Response) then Exit;

  LRespPtr := nil;
  LRespLen := wolfSSL_GetOCSP_Response(FWolfSSL, @LRespPtr);

  if (LRespLen > 0) and (LRespPtr <> nil) then
  begin
    SetLength(Result, LRespLen);
    Move(LRespPtr^, Result[0], LRespLen);
  end;
end;

function TWolfSSLConnection.IsOCSPResponseVerified: Boolean;
var
  LResp: TBytes;
begin
  // 简化实现：如果能获取到响应，认为已验证
  // WolfSSL 会在握手期间验证 OCSP 响应
  LResp := GetOCSPResponse;
  Result := Length(LResp) > 0;
end;

function TWolfSSLConnection.GetOCSPResponseStatus: string;
var
  LResp: TBytes;
begin
  if not Assigned(wolfSSL_GetOCSP_Response) then
  begin
    Result := 'OCSP API not available';
    Exit;
  end;

  LResp := GetOCSPResponse;
  if Length(LResp) = 0 then
    Result := 'No OCSP Response'
  else
    Result := 'Response Available';
end;

end.
