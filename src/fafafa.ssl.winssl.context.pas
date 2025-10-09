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
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
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
    FInitialized: Boolean;
    
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
  FInitialized := False;
  
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
  if FInitialized and IsValidSecHandle(FCredHandle) then
    FreeCredentialsHandle(@FCredHandle);
  inherited Destroy;
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
begin
  // TODO: 实现证书加载
end;

procedure TWinSSLContext.LoadCertificate(aStream: TStream);
begin
  // TODO: 实现证书加载
end;

procedure TWinSSLContext.LoadCertificate(aCert: ISSLCertificate);
begin
  // TODO: 实现证书加载
end;

procedure TWinSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string);
begin
  // TODO: 实现私钥加载
end;

procedure TWinSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
begin
  // TODO: 实现私钥加载
end;

procedure TWinSSLContext.LoadCAFile(const aFileName: string);
begin
  // TODO: 实现 CA 文件加载
end;

procedure TWinSSLContext.LoadCAPath(const aPath: string);
begin
  // TODO: 实现 CA 路径加载
end;

procedure TWinSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
begin
  // TODO: 实现证书存储设置
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
  // TODO: 实现验证回调设置
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
  // TODO: 实现 TLS 1.3 密码套件设置
end;

function TWinSSLContext.GetCipherSuites: string;
begin
  // TODO: 实现 TLS 1.3 密码套件获取
  Result := '';
end;

// ============================================================================
// ISSLContext - 会话管理（存根实现）
// ============================================================================

procedure TWinSSLContext.SetSessionCacheMode(aEnabled: Boolean);
begin
  // TODO: 实现会话缓存模式设置
end;

function TWinSSLContext.GetSessionCacheMode: Boolean;
begin
  // TODO: 实现会话缓存模式获取
  Result := True;
end;

procedure TWinSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  // TODO: 实现会话超时设置
end;

function TWinSSLContext.GetSessionTimeout: Integer;
begin
  // TODO: 实现会话超时获取
  Result := 300;
end;

procedure TWinSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  // TODO: 实现会话缓存大小设置
end;

function TWinSSLContext.GetSessionCacheSize: Integer;
begin
  // TODO: 实现会话缓存大小获取
  Result := 1024;
end;

// ============================================================================
// ISSLContext - 高级选项
// ============================================================================

procedure TWinSSLContext.SetOptions(aOptions: Cardinal);
begin
  // TODO: 实现选项设置
end;

function TWinSSLContext.GetOptions: Cardinal;
begin
  // TODO: 实现选项获取
  Result := 0;
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
  // TODO: 实现 ALPN 协议设置
end;

function TWinSSLContext.GetALPNProtocols: string;
begin
  // TODO: 实现 ALPN 协议获取
  Result := '';
end;

// ============================================================================
// ISSLContext - 回调设置（存根实现）
// ============================================================================

procedure TWinSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  // TODO: 实现密码回调设置
end;

procedure TWinSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  // TODO: 实现信息回调设置
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
