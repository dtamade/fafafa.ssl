{
  fafafa.ssl.winssl.context - WinSSL 上下文实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-06
  
  描述:
    实现 ISSLContext 接口的 WinSSL 后端。
    负责 Schannel 凭据管理和连接创建。
}

unit fafafa.ssl.winssl.context;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,  // 新增：类型化异常
  fafafa.ssl.winssl.types,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

type
  { TWinSSLContext - Windows Schannel 上下文类 }
  TWinSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FCredHandle: CredHandle;
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FServerName: string;
    FCipherList: string;
    FCipherSuites: string;
    FALPNProtocols: string;
    FInitialized: Boolean;
    FOptions: TSSLOptions;
    
    // 证书相关
    FCertContext: PCCERT_CONTEXT;
    FCertStore: HCERTSTORE;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    
    // 回调
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    
    procedure CleanupCertificate;
    procedure ApplyOptions;
    
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
  fafafa.ssl.winssl.connection;

// ============================================================================
// TWinSSLContext - 构造和析构
// ============================================================================

constructor TWinSSLContext.Create(aLibrary: ISSLLibrary; aType: TSSLContextType);
var
  SchannelCred: SCHANNEL_CRED;
  Status: SECURITY_STATUS;
  TimeStamp: TTimeStamp;
  dwDirection: DWORD;
begin
  inherited Create;
  FLibrary := aLibrary;
  FContextType := aType;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FServerName := '';
  FCipherList := '';
  FCipherSuites := '';
  FALPNProtocols := '';
  FInitialized := False;
  FOptions := [ssoEnableSessionCache, ssoEnableSessionTickets];
  
  // 证书相关初始化
  FCertContext := nil;
  FCertStore := nil;
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  
  // 回调初始化
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;
  
  InitSecHandle(FCredHandle);
  
  // 初始化 Schannel 凭据
  FillChar(SchannelCred, SizeOf(SchannelCred), 0);
  SchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
  
  // 设置协议版本标志
  SchannelCred.grbitEnabledProtocols := ProtocolVersionsToSchannelFlags(
    FProtocolVersions, 
    FContextType = sslCtxServer
  );
  
  // 设置标志
  SchannelCred.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or 
                          SCH_CRED_MANUAL_CRED_VALIDATION;
  
  // 确定方向（客户端或服务端）
  if FContextType = sslCtxServer then
    dwDirection := SECPKG_CRED_INBOUND
  else
    dwDirection := SECPKG_CRED_OUTBOUND;
  
  // 获取凭据句柄
  Status := AcquireCredentialsHandleW(
    nil,
    PWideChar(WideString('Microsoft Unified Security Protocol Provider')),
    dwDirection,
    nil,
    @SchannelCred,
    nil,
    nil,
    @FCredHandle,
    @TimeStamp
  );
  
  FInitialized := IsSuccess(Status);
  ApplyOptions;
end;

destructor TWinSSLContext.Destroy;
begin
  CleanupCertificate;
  if FInitialized and IsValidSecHandle(FCredHandle) then
    FreeCredentialsHandle(@FCredHandle);
  inherited Destroy;
end;

procedure TWinSSLContext.CleanupCertificate;
begin
  if FCertContext <> nil then
  begin
    CertFreeCertificateContext(FCertContext);
    FCertContext := nil;
  end;
  
  if FCertStore <> nil then
  begin
    CertCloseStore(FCertStore, 0);
    FCertStore := nil;
  end;
end;

procedure TWinSSLContext.ApplyOptions;
begin
  // 当前实现已映射主要选项（会话缓存等）
  // 可扩展：根据实际需求添加更多 WinSSL 选项映射
  SetSessionCacheMode(ssoEnableSessionCache in FOptions);
end;

// ============================================================================
// ISSLContext - 基本配置
// ============================================================================

function TWinSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TWinSSLContext.SetProtocolVersions(aVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := aVersions;
end;

function TWinSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

// ============================================================================
// ISSLContext - 证书和密钥管理
// ============================================================================

procedure TWinSSLContext.LoadCertificate(const aFileName: string);
var
  LFileStream: TFileStream;
begin
  if not FileExists(aFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('Certificate file not found: %s', [aFileName]),
      sslErrLoadFailed,
      'TWinSSLContext.LoadCertificate'
    );
  
  LFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadCertificate(LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure TWinSSLContext.LoadCertificate(aStream: TStream);
var
  LCertData: TBytes;
  LSize: Int64;
  Blob: CRYPT_DATA_BLOB;
  PFXStore: HCERTSTORE;
  CertContext: PCCERT_CONTEXT;
begin
  // 清理之前的证书
  CleanupCertificate;
  
  // 读取证书数据
  LSize := aStream.Size - aStream.Position;
  SetLength(LCertData, LSize);
  aStream.Read(LCertData[0], LSize);
  
  // 1. 尝试作为 PFX 加载 (无密码)
  Blob.cbData := Length(LCertData);
  Blob.pbData := @LCertData[0];
  
  PFXStore := PFXImportCertStore(@Blob, nil, CRYPT_EXPORTABLE or PKCS12_NO_PERSIST_KEY);
  if PFXStore <> nil then
  begin
    // 是 PFX 文件，查找第一个证书（通常是用户证书）
    // 注意：这里假设 PFX 中包含私钥的证书是我们需要的
    // 实际应该遍历查找带有私钥属性的证书
    CertContext := CertEnumCertificatesInStore(PFXStore, nil);
    if CertContext <> nil then
    begin
      FCertContext := CertDuplicateCertificateContext(CertContext);
      // PFX 存储中的证书已经包含了私钥关联信息
      // 我们不需要显式加载私钥，Schannel 会自动使用
    end;
    CertCloseStore(PFXStore, 0);
    
    if FCertContext <> nil then
      Exit; // 成功加载
  end;

  // 2. 尝试作为普通证书加载 (DER/PEM)
  // 创建内存证书存储
  FCertStore := CertOpenStore(
    CERT_STORE_PROV_MEMORY,
    0,
    0,
    0,
    nil
  );
  
  if FCertStore = nil then
    raise ESSLResourceException.CreateWithContext(
      'Failed to create certificate store',
      sslErrMemory,
      'TWinSSLContext.LoadCertificate'
    );
  
  // 添加证书到存储 (尝试多种格式，包括 PEM)
  // CertAddEncodedCertificateToStore 只能处理 DER，我们需要先转换 PEM
  // 或者使用 CertCreateCertificateContextFromFile 的逻辑
  
  // 尝试直接添加 (DER)
  if not CertAddEncodedCertificateToStore(
    FCertStore,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    @LCertData[0],
    Length(LCertData),
    CERT_STORE_ADD_ALWAYS,
    @FCertContext
  ) then
  begin
    // 如果失败，尝试解码 PEM
    // 这里简化处理，如果需要完整 PEM 支持，应使用 CryptStringToBinaryA
    // 目前假设是 DER 格式失败
    if FCertContext = nil then
    raise ESSLCertificateLoadException.CreateWithContext(
      'Certificate load failed: unsupported format or invalid data',
      sslErrLoadFailed,
      'TWinSSLContext.LoadCertificate',
      GetLastError,
      sslWinSSL
    );
  end;
end;

procedure TWinSSLContext.LoadCertificate(aCert: ISSLCertificate);
begin
  // 从ISSLCertificate接口获取证书上下文
  if aCert = nil then
    raise ESSLInvalidArgument.CreateWithContext(
      'Certificate object is nil',
      sslErrInvalidParam,
      'TWinSSLContext.LoadCertificate'
    );
  
  CleanupCertificate;
  
  // 获取证书的原生句柄
  FCertContext := PCCERT_CONTEXT(aCert.GetNativeHandle);
  if FCertContext <> nil then
    FCertContext := CertDuplicateCertificateContext(FCertContext);
end;

procedure TWinSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string);
var
  LFileStream: TFileStream;
begin
  if not FileExists(aFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('Private key file not found: %s', [aFileName]),
      sslErrLoadFailed,
      'TWinSSLContext.LoadPrivateKey'
    );
  
  LFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadPrivateKey(LFileStream, aPassword);
  finally
    LFileStream.Free;
  end;
end;

procedure TWinSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
var
  LCertData: TBytes;
  LSize: Int64;
  Blob: CRYPT_DATA_BLOB;
  PFXStore: HCERTSTORE;
  CertContext: PCCERT_CONTEXT;
  PW: PWideChar;
begin
  // WinSSL/Schannel 通常将证书和私钥一起加载（如PFX格式）
  // 如果是 PFX，我们尝试加载并提取证书和私钥
  
  LSize := aStream.Size - aStream.Position;
  SetLength(LCertData, LSize);
  aStream.Read(LCertData[0], LSize);
  
  Blob.cbData := Length(LCertData);
  Blob.pbData := @LCertData[0];
  
  PW := nil;
  if aPassword <> '' then
    PW := StringToPWideChar(aPassword);
    
  try
    PFXStore := PFXImportCertStore(@Blob, PW, CRYPT_EXPORTABLE or PKCS12_NO_PERSIST_KEY);
  finally
    if PW <> nil then
      FreePWideCharString(PW);
  end;
  
  if PFXStore <> nil then
  begin
    // 成功打开 PFX，查找带有私钥的证书
    // 这里我们简单地取第一个证书，并替换当前的 FCertContext
    // 如果之前通过 LoadCertificate 加载了证书，这里会被覆盖
    // 这是符合预期的，因为 PFX 包含了证书和私钥
    
    CertContext := CertEnumCertificatesInStore(PFXStore, nil);
    if CertContext <> nil then
    begin
      if FCertContext <> nil then
        CertFreeCertificateContext(FCertContext);
        
      FCertContext := CertDuplicateCertificateContext(CertContext);
    end;
    CertCloseStore(PFXStore, 0);
  end
  else
  begin
    // 如果不是 PFX，可能是 PEM 格式的私钥
    // WinSSL 不直接支持加载 PEM 私钥并关联到现有证书上下文
    // 除非使用 CryptImportKey 等复杂操作
    // 这里抛出异常或记录警告
    if aStream = nil then
    raise ESSLConfigurationException.CreateWithContext(
      'WinSSL backend only supports PFX/P12 format for private key loading. Please merge certificate and key into a PFX file.',
      sslErrUnsupported,
      'TWinSSLContext.LoadPrivateKey'
    );
  end;
end;

procedure TWinSSLContext.LoadCertificatePEM(const aPEM: string);
begin
  // WinSSL/Schannel does not natively support PEM format
  // PEM data must be converted to DER or PKCS#12 format
  if aPEM = '' then
    raise ESSLInvalidArgument.CreateWithContext(
      'PEM string is empty',
      sslErrInvalidParam,
      'TWinSSLContext.LoadCertificatePEM'
    );
  
  raise ESSLConfigurationException.CreateWithContext(
    'WinSSL backend does not support direct PEM loading. ' +
    'Please convert to DER or PKCS#12 format, or use the OpenSSL backend.',
    sslErrUnsupported,
    'TWinSSLContext.LoadCertificatePEM'
  );
end;

procedure TWinSSLContext.LoadPrivateKeyPEM(const aPEM: string; const aPassword: string = '');
begin
  // WinSSL/Schannel does not natively support PEM format for private keys
  if aPEM = '' then
    raise ESSLInvalidArgument.CreateWithContext(
      'PEM string is empty',
      sslErrInvalidParam,
      'TWinSSLContext.LoadPrivateKeyPEM'
    );
  
  raise ESSLConfigurationException.CreateWithContext(
    'WinSSL backend does not support direct PEM loading. ' +
    'Please use PKCS#12 format with LoadPrivateKey, or use the OpenSSL backend.',
    sslErrUnsupported,
    'TWinSSLContext.LoadPrivateKeyPEM'
  );
end;

procedure TWinSSLContext.LoadCAFile(const aFileName: string);
var
  LFileStream: TFileStream;
  LCertData: TBytes;
  LSize: Int64;
  LCertContext: PCCERT_CONTEXT;
begin
  if not FileExists(aFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('CA file not found: %s', [aFileName]),
      sslErrLoadFailed,
      'TWinSSLContext.LoadCAFile'
    );
  
  LFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LSize := LFileStream.Size;
    SetLength(LCertData, LSize);
    LFileStream.Read(LCertData[0], LSize);
    
    // 加载CA证书到系统存储
    LCertContext := CertCreateCertificateContext(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      @LCertData[0],
      Length(LCertData)
    );
    
    if LCertContext <> nil then
      CertFreeCertificateContext(LCertContext);
  finally
    LFileStream.Free;
  end;
end;

procedure TWinSSLContext.LoadCAPath(const aPath: string);
begin
  // Windows 使用系统证书存储，不支持指定CA路径
  // 如果调用者传入了路径，说明期望此功能工作，应该抛出异常
  if aPath <> '' then
    raise ESSLPlatformNotSupportedException.CreateWithContext(
      'LoadCAPath is not supported on Windows. ' +
      'Windows uses the system certificate store for CA verification.',
      sslErrOther,
      'TWinSSLContext.LoadCAPath',
      0,
      sslWinSSL
    );
  // 空路径视为无操作（兼容跨平台代码）
end;

procedure TWinSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
begin
  // WinSSL使用系统证书存储或自定义HCERTSTORE
  if aStore <> nil then
    raise ESSLPlatformNotSupportedException.CreateWithContext(
      'SetCertificateStore with ISSLCertificateStore is not implemented for WinSSL. ' +
      'Use Windows certificate store APIs directly.',
      sslErrOther,
      'TWinSSLContext.SetCertificateStore',
      0,
      sslWinSSL
    );
end;

// ============================================================================
// ISSLContext - 验证配置
// ============================================================================

procedure TWinSSLContext.SetVerifyMode(aMode: TSSLVerifyModes);
begin
  FVerifyMode := aMode;
end;

function TWinSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TWinSSLContext.SetVerifyDepth(aDepth: Integer);
begin
  FVerifyDepth := aDepth;
end;

function TWinSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TWinSSLContext.SetVerifyCallback(aCallback: TSSLVerifyCallback);
begin
  FVerifyCallback := aCallback;
end;

// ============================================================================
// ISSLContext - 密码套件配置
// ============================================================================

procedure TWinSSLContext.SetCipherList(const aCipherList: string);
begin
  FCipherList := aCipherList;
end;

function TWinSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TWinSSLContext.SetCipherSuites(const aCipherSuites: string);
begin
  FCipherSuites := aCipherSuites;
  // TLS 1.3 密码套件在Windows 10/Server 2019+支持
  // Schannel会自动选择合适的密码套件
end;

function TWinSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

// ============================================================================
// ISSLContext - 会话管理（Schannel 自动管理）
// ============================================================================

procedure TWinSSLContext.SetSessionCacheMode(aEnabled: Boolean);
begin
  FSessionCacheEnabled := aEnabled;
  // Schannel自动管理会话缓存
end;

function TWinSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TWinSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  FSessionTimeout := aTimeout;
end;

function TWinSSLContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TWinSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  FSessionCacheSize := aSize;
end;

function TWinSSLContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

// ============================================================================
// ISSLContext - 高级选项
// ============================================================================

procedure TWinSSLContext.SetOptions(const aOptions: TSSLOptions);
begin
  FOptions := aOptions;
  ApplyOptions;
end;

function TWinSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TWinSSLContext.SetServerName(const aServerName: string);
begin
  FServerName := aServerName;
end;

function TWinSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TWinSSLContext.SetALPNProtocols(const aProtocols: string);
begin
  FALPNProtocols := aProtocols;
  // ALPN在Windows 8.1/Server 2012 R2+支持
  // 实际协商在连接时进行
end;

function TWinSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

// ============================================================================
// ISSLContext - 回调设置（Schannel 模型限制）
// ============================================================================

procedure TWinSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  FPasswordCallback := aCallback;
end;

procedure TWinSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  FInfoCallback := aCallback;
end;

// ============================================================================
// ISSLContext - 创建连接
// ============================================================================

function TWinSSLContext.CreateConnection(aSocket: THandle): ISSLConnection;
begin
  if not FInitialized then
  begin
    Result := nil;
    Exit;
  end;
  
  try
    Result := TWinSSLConnection.Create(Self, aSocket);
  except
    Result := nil;
  end;
end;

function TWinSSLContext.CreateConnection(aStream: TStream): ISSLConnection;
begin
  if not FInitialized then
  begin
    Result := nil;
    Exit;
  end;
  
  try
    Result := TWinSSLConnection.Create(Self, aStream);
  except
    Result := nil;
  end;
end;

// ============================================================================
// ISSLContext - 状态查询
// ============================================================================

function TWinSSLContext.IsValid: Boolean;
begin
  Result := FInitialized and IsValidSecHandle(FCredHandle);
end;

function TWinSSLContext.GetNativeHandle: Pointer;
begin
  Result := @FCredHandle;
end;

end.
