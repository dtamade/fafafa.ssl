{
  fafafa.ssl.intf - SSL/TLS 库核心接口定义
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    定义 fafafa.ssl 库的所有抽象接口。
    所有后端实现必须实现这些接口以保证API的一致性。
}

unit fafafa.ssl.intf;

{$mode ObjFPC}{$H+}
{$INTERFACES CORBA}  // 使用CORBA接口模式以避免引用计数问题
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, fafafa.ssl.types;

type
  // 前向声明
  ISSLContext = interface;
  ISSLConnection = interface;
  ISSLCertificate = interface;
  ISSLCertificateStore = interface;
  ISSLSession = interface;
  ISSLLibrary = interface;
  
  // 数组类型
  TSSLCertificateArray = array of ISSLCertificate;

  // ============================================================================
  // ISSLLibrary - SSL库管理接口
  // ============================================================================
  
  { 
    SSL库管理接口
    负责库的初始化、清理、版本信息获取等全局操作
  }
  ISSLLibrary = interface
    ['{A0E8F4B1-7C3A-4D2E-9F5B-8C6D7E9A0B1C}']
    
    // 初始化和清理
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    
    // 版本信息
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    // 功能支持查询
    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(const aFeatureName: string): Boolean;
    
    // 库配置
    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    // 错误处理
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    // 统计信息
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    // 日志
    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
    
    // 工厂方法
    function CreateContext(aType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  // ============================================================================
  // ISSLContext - SSL上下文接口
  // ============================================================================
  
  {
    SSL上下文接口
    管理SSL配置和创建连接
  }
  ISSLContext = interface
    ['{B1F9E5C2-8D4B-5E3F-A06C-9D8E0F1A2B3D}']
    
    // 基本配置
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    // 证书和密钥管理
    procedure LoadCertificate(const aFileName: string); overload;
    procedure LoadCertificate(aStream: TStream); overload;
    procedure LoadCertificate(aCert: ISSLCertificate); overload;
    
    procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
    procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
    
    procedure LoadCAFile(const aFileName: string);
    procedure LoadCAPath(const aPath: string);
    procedure SetCertificateStore(aStore: ISSLCertificateStore);
    
    // 验证配置
    procedure SetVerifyMode(aMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(aDepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
    
    // 密码套件配置
    procedure SetCipherList(const aCipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const aCipherSuites: string); // TLS 1.3
    function GetCipherSuites: string;
    
    // 会话管理
    procedure SetSessionCacheMode(aEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(aTimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(aSize: Integer);
    function GetSessionCacheSize: Integer;
    
    // 高级选项
    procedure SetOptions(aOptions: Cardinal);
    function GetOptions: Cardinal;
    procedure SetServerName(const aServerName: string); // SNI
    function GetServerName: string;
    procedure SetALPNProtocols(const aProtocols: string);
    function GetALPNProtocols: string;
    
    // 回调设置
    procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
    procedure SetInfoCallback(aCallback: TSSLInfoCallback);
    
    // 创建连接
    function CreateConnection(aSocket: THandle): ISSLConnection; overload;
    function CreateConnection(aStream: TStream): ISSLConnection; overload;
    
    // 状态查询
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;
  end;

  // ============================================================================
  // ISSLConnection - SSL连接接口
  // ============================================================================
  
  {
    SSL连接接口
    管理单个SSL/TLS连接
  }
  ISSLConnection = interface
    ['{C2A9F6D3-9E5C-6F40-B17D-AE9F102B4C5E}']
    
    // 连接管理
    function Connect: Boolean;              // 客户端连接
    function Accept: Boolean;               // 服务端接受
    function Shutdown: Boolean;             // 优雅关闭
    procedure Close;                        // 强制关闭
    
    // 握手控制
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    
    // 数据传输
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    function ReadString(out aStr: string): Boolean;
    function WriteString(const aStr: string): Boolean;
    
    // 异步操作支持
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(aRet: Integer): TSSLErrorCode;
    
    // 连接信息
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    
    // 会话管理
    function GetSession: ISSLSession;
    procedure SetSession(aSession: ISSLSession);
    function IsSessionReused: Boolean;
    
    // ALPN/NPN
    function GetSelectedALPNProtocol: string;
    
    // 状态查询
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    
    // 选项设置
    procedure SetTimeout(aTimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(aBlocking: Boolean);
    function GetBlocking: Boolean;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

  // ============================================================================
  // ISSLCertificate - 证书接口
  // ============================================================================
  
  {
    证书管理接口
    处理X.509证书的加载、验证和信息提取
  }
  ISSLCertificate = interface
    ['{D3B0A7E4-AF6D-7051-C28E-BF0A213C5D6F}']
    
    // 加载和保存
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    
    // 证书信息
    function GetInfo: TSSLCertificateInfo;
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetPublicKey: string;
    function GetPublicKeyAlgorithm: string;
    function GetSignatureAlgorithm: string;
    function GetVersion: Integer;
    
    // 证书验证
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    
    // 证书扩展
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    
    // 指纹
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    // 证书链
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

  // ============================================================================
  // ISSLCertificateStore - 证书存储接口
  // ============================================================================
  
  {
    证书存储接口
    管理可信证书集合
  }
  ISSLCertificateStore = interface
    ['{E4C1B8F5-B07E-8162-D39F-C10B324D6E70}']
    
    // 证书管理
    function AddCertificate(aCert: ISSLCertificate): Boolean;
    function RemoveCertificate(aCert: ISSLCertificate): Boolean;
    function Contains(aCert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(aIndex: Integer): ISSLCertificate;
    
    // 加载证书
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromPath(const aPath: string): Boolean;
    function LoadSystemStore: Boolean; // 加载系统证书存储
    
    // 查找证书
    function FindBySubject(const aSubject: string): ISSLCertificate;
    function FindByIssuer(const aIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const aFingerprint: string): ISSLCertificate;
    
    // 验证
    function VerifyCertificate(aCert: ISSLCertificate): Boolean;
    function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
  end;

  // ============================================================================
  // ISSLSession - 会话接口
  // ============================================================================
  
  {
    SSL会话接口
    用于会话复用和管理
  }
  ISSLSession = interface
    ['{F5D2C9F6-C18F-9273-E40A-D21C435E7F81}']
    
    // 会话信息
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(aTimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    
    // 会话属性
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    
    // 序列化
    function Serialize: TBytes;
    function Deserialize(const aData: TBytes): Boolean;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;
  end;

  // ============================================================================
  // 辅助函数
  // ============================================================================
  
  function SSLErrorToString(aError: TSSLErrorCode): string;
  function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
  function LibraryTypeToString(aLibType: TSSLLibraryType): string;

implementation

function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  Result := SSL_ERROR_MESSAGES[aError];
end;

function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
begin
  Result := SSL_PROTOCOL_NAMES[aVersion];
end;

function LibraryTypeToString(aLibType: TSSLLibraryType): string;
begin
  Result := SSL_LIBRARY_NAMES[aLibType];
end;

end.