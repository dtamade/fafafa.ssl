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
    FOptions: Cardinal;
    
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
    procedure SetOptions(aOptions: Cardinal);
    function GetOptions: Cardinal;
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
  FVerifyDepth := 9;
  FServerName := '';
  FCipherList := '';
  FCipherSuites := '';
  FALPNProtocols := '';
  FInitialized := False;
  FOptions := 0;
  
  // 证书相关初始化
  FCertContext := nil;
  FCertStore := 0;
  FSessionCacheEnabled := True;
  FSessionTimeout := 300;
  FSessionCacheSize := 20480;
  
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
  
  if FCertStore <> 0 then
  begin
    CertCloseStore(FCertStore, 0);
    FCertStore := 0;
  end;
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
// ISSLContext - 证书和密钥管理（存根实现）
// ============================================================================

procedure TWinSSLContext.LoadCertificate(const aFileName: string);
var
  LFileStream: TFileStream;
begin
  if not FileExists(aFileName) then
    raise Exception.CreateFmt('证书文件不存在: %s', [aFileName]);
  
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
begin
  // 清理之前的证书
  CleanupCertificate;
  
  // 读取证书数据
  LSize := aStream.Size - aStream.Position;
  SetLength(LCertData, LSize);
  aStream.Read(LCertData[0], LSize);
  
  // 创建内存证书存储
  FCertStore := CertOpenStore(
    CERT_STORE_PROV_MEMORY,
    0,
    0,
    0,
    nil
  );
  
  if FCertStore = 0 then
    raise Exception.Create('无法创建证书存储');
  
  // 添加证书到存储 (尝试多种格式)
  if not CertAddEncodedCertificateToStore(
    FCertStore,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    @LCertData[0],
    Length(LCertData),
    CERT_STORE_ADD_ALWAYS,
    @FCertContext
  ) then
  begin
    // 如果失败，尝试作为PFX/P12格式
    // 这里简化处理，实际应该根据文件格式进行相应处理
    raise Exception.Create('证书加载失败');
  end;
end;

procedure TWinSSLContext.LoadCertificate(aCert: ISSLCertificate);
begin
  // 从ISSLCertificate接口获取证书上下文
  if aCert = nil then
    raise Exception.Create('证书对象为空');
  
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
    raise Exception.CreateFmt('私钥文件不存在: %s', [aFileName]);
  
  LFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadPrivateKey(LFileStream, aPassword);
  finally
    LFileStream.Free;
  end;
end;

procedure TWinSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
begin
  // WinSSL/Schannel 通常将证书和私钥一起加载（如PFX格式）
  // 这里简化处理，实际应该使用 CryptImportKey 等API
  // 目前假设证书中已包含私钥（如通过LoadCertificate加载PFX文件）
end;

procedure TWinSSLContext.LoadCAFile(const aFileName: string);
var
  LFileStream: TFileStream;
  LCertData: TBytes;
  LSize: Int64;
  LCertContext: PCCERT_CONTEXT;
begin
  if not FileExists(aFileName) then
    raise Exception.CreateFmt('CA文件不存在: %s', [aFileName]);
  
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
  // Windows 使用系统证书存储，不需要指定CA路径
  // 这个方法在WinSSL中作为空操作
end;

procedure TWinSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
begin
  // WinSSL使用系统证书存储或自定义HCERTSTORE
  // 这里可以从ISSLCertificateStore获取原生句柄
  if aStore <> nil then
  begin
    // 实现根据具体需求
  end;
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
// ISSLContext - 会话管理（存根实现）
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

procedure TWinSSLContext.SetOptions(aOptions: Cardinal);
begin
  FOptions := aOptions;
end;

function TWinSSLContext.GetOptions: Cardinal;
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
// ISSLContext - 回调设置（存根实现）
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
