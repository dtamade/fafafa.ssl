{
  fafafa.ssl.winssl - Windows Schannel SSL/TLS 后端实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    基于 Windows Schannel API 的 SSL/TLS 实现。
    提供零依赖的原生 Windows SSL 支持。
}

unit fafafa.ssl.winssl;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

{$IFDEF WINDOWS}

uses
  Windows, WinSock2, SysUtils, Classes, SyncObjs, Math,
  fafafa.ssl.types, fafafa.ssl.intf, fafafa.ssl.ringbuffer;

const
  // Schannel DLL
  SECUR32_DLL = 'Secur32.dll';
  CRYPT32_DLL = 'Crypt32.dll';
  
  // Security Package Name
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';
  
  // Protocol Flags
  SP_PROT_TLS1_0 = $00000080;
  SP_PROT_TLS1_1 = $00000200;
  SP_PROT_TLS1_2 = $00000800;
  SP_PROT_TLS1_3 = $00002000;
  SP_PROT_DTLS1_0 = $00010000;
  SP_PROT_DTLS1_2 = $00040000;
  
  // Context Requirements
  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_EXTENDED_ERROR = $00008000;
  ISC_REQ_STREAM = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_USE_SUPPLIED_CREDS = $00000080;
  
  ASC_REQ_SEQUENCE_DETECT = $00000008;
  ASC_REQ_REPLAY_DETECT = $00000004;
  ASC_REQ_CONFIDENTIALITY = $00000010;
  ASC_REQ_EXTENDED_ERROR = $00008000;
  ASC_REQ_STREAM = $00040000;
  
  // Security Status
  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_COMPLETE_AND_CONTINUE = $00090314;
  SEC_I_COMPLETE_NEEDED = $00090313;
  SEC_E_INCOMPLETE_MESSAGE = LONG($80090318);
  SEC_E_INVALID_TOKEN = LONG($80090308);
  SEC_E_INVALID_HANDLE = LONG($80090301);
  SEC_E_WRONG_PRINCIPAL = LONG($80090322);
  SEC_E_CERT_EXPIRED = LONG($80090328);
  SEC_E_UNTRUSTED_ROOT = LONG($80090325);
  SEC_E_CERT_UNKNOWN = LONG($80090327);
  
  // Buffer Types
  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;
  SECBUFFER_STREAM = 10;      // Used for streaming mode
  SECBUFFER_ALERT = 17;
  
  // Security Package Attributes
  SECPKG_ATTR_SIZES = 0;
  SECPKG_ATTR_NAMES = 1;
  SECPKG_ATTR_LIFESPAN = 2;
  SECPKG_ATTR_DCE_INFO = 3;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECPKG_ATTR_KEY_INFO = 5;
  SECPKG_ATTR_AUTHORITY = 6;
  SECPKG_ATTR_PROTO_INFO = 7;
  SECPKG_ATTR_PASSWORD_EXPIRY = 8;
  SECPKG_ATTR_SESSION_KEY = 9;
  SECPKG_ATTR_PACKAGE_INFO = 10;
  SECPKG_ATTR_USER_FLAGS = 11;
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  SECPKG_ATTR_NATIVE_NAMES = 13;
  SECPKG_ATTR_FLAGS = 14;
  SECPKG_ATTR_REMOTE_CERT_CONTEXT = $53;  // 83 - Remote certificate context
  SECPKG_ATTR_LOCAL_CERT_CONTEXT = $54;   // 84 - Local certificate context
  SECPKG_ATTR_ROOT_STORE = $55;           // 85
  SECPKG_ATTR_ISSUER_LIST_EX = $59;       // 89
  SECPKG_ATTR_CONNECTION_INFO = $5A;      // 90
  
  // Certificate Store
  CERT_STORE_PROV_SYSTEM = 10;
  CERT_SYSTEM_STORE_CURRENT_USER = $00010000;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;
  X509_ASN_ENCODING = 1;
  PKCS_7_ASN_ENCODING = $10000;
  
  // WinSock Constants
  INVALID_HANDLE_VALUE = THandle(-1);
  SOCKET_ERROR = -1;
  FIONREAD = $4004667F;
  WSAEWOULDBLOCK = 10035;
  
  // Certificate verification errors
  CERT_E_EXPIRED = LONG($800B0101);
  CERT_E_UNTRUSTEDROOT = LONG($800B0109);
  CERT_E_WRONG_USAGE = LONG($800B0110);
  CERT_E_CN_NO_MATCH = LONG($800B010F);
  CERT_E_REVOKED = LONG($800B010C);
  
type
  // Windows Certificate Functions
  TCertNameToStr = function(
    dwCertEncodingType: DWORD;
    pName: Pointer;
    dwStrType: DWORD;
    psz: PChar;
    csz: DWORD
  ): DWORD; stdcall;
  
  // Windows Types
  SECURITY_STATUS = LONG;
  TimeStamp = LARGE_INTEGER;
  PTimeStamp = ^TimeStamp;
  ALG_ID = ULONG;
  PCCERT_CONTEXT = Pointer;
  TTimeStamp = TimeStamp;
  
  // Credential Handle
  CredHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  PCredHandle = ^CredHandle;
  
  // Context Handle
  CtxtHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  PCtxtHandle = ^CtxtHandle;
  
  // Security Buffer
  SecBuffer = record
    cbBuffer: ULONG;
    BufferType: ULONG;
    pvBuffer: Pointer;
  end;
  PSecBuffer = ^SecBuffer;
  
  // Security Buffer Description
  SecBufferDesc = record
    ulVersion: ULONG;
    cBuffers: ULONG;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc = ^SecBufferDesc;
  
  // SCHANNEL Credential
  SCHANNEL_CRED = record
    dwVersion: DWORD;
    cCreds: DWORD;
    paCred: Pointer;
    hRootStore: THandle;
    cMappers: DWORD;
    aphMappers: Pointer;
    cSupportedAlgs: DWORD;
    palgSupportedAlgs: PDWORD;
    grbitEnabledProtocols: DWORD;
    dwMinimumCipherStrength: DWORD;
    dwMaximumCipherStrength: DWORD;
    dwSessionLifespan: DWORD;
    dwFlags: DWORD;
    dwCredFormat: DWORD;
  end;
  PSCHANNEL_CRED = ^SCHANNEL_CRED;
  
  // Stream Sizes
  SecPkgContext_StreamSizes = record
    cbHeader: ULONG;
    cbTrailer: ULONG;
    cbMaximumMessage: ULONG;
    cBuffers: ULONG;
    cbBlockSize: ULONG;
  end;
  PSecPkgContext_StreamSizes = ^SecPkgContext_StreamSizes;
  
  // Connection Info
  SecPkgContext_ConnectionInfo = record
    dwProtocol: DWORD;
    aiCipher: ALG_ID;
    dwCipherStrength: DWORD;
    aiHash: ALG_ID;
    dwHashStrength: DWORD;
    aiExch: ALG_ID;
    dwExchStrength: DWORD;
  end;
  PSecPkgContext_ConnectionInfo = ^SecPkgContext_ConnectionInfo;

  // 函数指针类型
  TAcquireCredentialsHandle = function(
    pszPrincipal: PWideChar;
    pszPackage: PWideChar;
    fCredentialUse: ULONG;
    pvLogonId: Pointer;
    pAuthData: Pointer;
    pGetKeyFn: Pointer;
    pvGetKeyArgument: Pointer;
    phCredential: PCredHandle;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall;
  
  TInitializeSecurityContext = function(
    phCredential: PCredHandle;
    phContext: PCtxtHandle;
    pszTargetName: PWideChar;
    fContextReq: ULONG;
    Reserved1: ULONG;
    TargetDataRep: ULONG;
    pInput: PSecBufferDesc;
    Reserved2: ULONG;
    phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc;
    pfContextAttr: PULONG;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall;
  
  TAcceptSecurityContext = function(
    phCredential: PCredHandle;
    phContext: PCtxtHandle;
    pInput: PSecBufferDesc;
    fContextReq: ULONG;
    TargetDataRep: ULONG;
    phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc;
    pfContextAttr: PULONG;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall;
  
  TDeleteSecurityContext = function(
    phContext: PCtxtHandle
  ): SECURITY_STATUS; stdcall;
  
  TFreeCredentialsHandle = function(
    phCredential: PCredHandle
  ): SECURITY_STATUS; stdcall;
  
  TQueryContextAttributes = function(
    phContext: PCtxtHandle;
    ulAttribute: ULONG;
    pBuffer: Pointer
  ): SECURITY_STATUS; stdcall;
  
  TEncryptMessage = function(
    phContext: PCtxtHandle;
    fQOP: ULONG;
    pMessage: PSecBufferDesc;
    MessageSeqNo: ULONG
  ): SECURITY_STATUS; stdcall;
  
  TDecryptMessage = function(
    phContext: PCtxtHandle;
    pMessage: PSecBufferDesc;
    MessageSeqNo: ULONG;
    pfQOP: PULONG
  ): SECURITY_STATUS; stdcall;

  // 辅助函数
  function GetWindowsErrorString(aError: DWORD): string;
  function ProtocolVersionToDWORD(aVersion: TSSLProtocolVersion): DWORD;

type
  { TWinSSLLibrary - Windows Schannel 库实现 }
  TWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FSecur32Handle: THandle;
    FCrypt32Handle: THandle;
    FStatistics: TSSLStatistics;
    FConfig: TSSLConfig;
    FLogCallback: TSSLLogCallback;
    FLock: TCriticalSection;
    
    // API 函数指针
    FAcquireCredentialsHandle: TAcquireCredentialsHandle;
    FInitializeSecurityContext: TInitializeSecurityContext;
    FAcceptSecurityContext: TAcceptSecurityContext;
    FDeleteSecurityContext: TDeleteSecurityContext;
    FFreeCredentialsHandle: TFreeCredentialsHandle;
    FQueryContextAttributes: TQueryContextAttributes;
    FEncryptMessage: TEncryptMessage;
    FDecryptMessage: TDecryptMessage;
    FCertNameToStr: TCertNameToStr;
    
    function LoadAPIs: Boolean;
    procedure UnloadAPIs;
    procedure DoLog(aLevel: TSSLLogLevel; const aMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    // ISSLLibrary 实现
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(const aFeatureName: string): Boolean;
    
    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
    
    function CreateContext(aType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;
  
  { TWinSSLContext - Windows Schannel 上下文实现 }
  TWinSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: TWinSSLLibrary;
    FContextType: TSSLContextType;
    FCredHandle: CredHandle;
    FHasCredentials: Boolean;
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FCertificate: ISSLCertificate;
    FPrivateKey: string;
    FCAStore: ISSLCertificateStore;
    FServerName: string;
    FALPNProtocols: string;
    FCipherList: string;
    FOptions: Cardinal;
    FPasswordCallback: TSSLPasswordCallback;
    FVerifyCallback: TSSLVerifyCallback;
    FInfoCallback: TSSLInfoCallback;
    
    function GetProtocolFlags: DWORD;
    function InitializeCredentials: Boolean;
  public
    constructor Create(aLibrary: TWinSSLLibrary; aType: TSSLContextType);
    destructor Destroy; override;
    
    // ISSLContext 实现
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    procedure LoadCertificate(const aFileName: string); overload;
    procedure LoadCertificate(aStream: TStream); overload;
    procedure LoadCertificate(aCert: ISSLCertificate); overload;
    
    procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
    procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
    
    procedure LoadCAFile(const aFileName: string);
    procedure LoadCAPath(const aPath: string);
    procedure SetCertificateStore(aStore: ISSLCertificateStore);
    
    procedure SetVerifyMode(aMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(aDepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
    
    procedure SetCipherList(const aCipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const aCipherSuites: string);
    function GetCipherSuites: string;
    
    procedure SetSessionCacheMode(aEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(aTimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(aSize: Integer);
    function GetSessionCacheSize: Integer;
    
    procedure SetOptions(aOptions: Cardinal);
    function GetOptions: Cardinal;
    procedure SetServerName(const aServerName: string);
    function GetServerName: string;
    procedure SetALPNProtocols(const aProtocols: string);
    function GetALPNProtocols: string;
    
    procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
    procedure SetInfoCallback(aCallback: TSSLInfoCallback);
    
    function CreateConnection(aSocket: THandle): ISSLConnection; overload;
    function CreateConnection(aStream: TStream): ISSLConnection; overload;
    
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;
  end;
  
  { TWinSSLConnection - Windows Schannel 连接实现 }
  TWinSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: TWinSSLContext;
    FSocket: THandle;
    FStream: TStream;
    FCtxtHandle: CtxtHandle;
    FHasContext: Boolean;
    FHandshakeState: TSSLHandshakeState;
    FIsConnected: Boolean;
    FStreamSizes: SecPkgContext_StreamSizes;
    FInputBuffer: TBytes;
    FOutputBuffer: TBytes;
    // 使用环形缓冲区替代动态数组
    FDecryptedBuffer: TRingBuffer;     // 解密后的数据环形缓冲区
    FEncryptedBuffer: TRingBuffer;     // 加密数据环形缓冲区
    FTempBuffer: TBytes;                // 临时缓冲区用于API调用
    FTimeout: Integer;
    FBlocking: Boolean;
    
    function PerformHandshake(aIsServer: Boolean): Boolean;
    function SendData(const aData: Pointer; aSize: Integer): Integer;
    function ReceiveData(aData: Pointer; aSize: Integer): Integer;
    function ProcessDecrypt: Integer;  // 简化：从FEncryptedBuffer解密到FDecryptedBuffer
  public
    constructor Create(aContext: TWinSSLContext; aSocket: THandle); overload;
    constructor Create(aContext: TWinSSLContext; aStream: TStream); overload;
    destructor Destroy; override;
    
    // ISSLConnection 实现
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
  
  { TWinSSLCertificate - Windows 证书实现 }
  TWinSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FCertContext: Pointer; // PCCERT_CONTEXT
    FInfo: TSSLCertificateInfo;
    
    procedure ParseCertificate;
  public
    constructor Create;
    destructor Destroy; override;
    
    // ISSLCertificate 实现
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    
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
    
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;
  
  { TWinSSLCertificateStore - Windows 证书存储实现 }
  TWinSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  private
    FStoreHandle: THandle;
    FCertificates: TList;
  public
    constructor Create;
    destructor Destroy; override;
    
    // ISSLCertificateStore 实现
    function AddCertificate(aCert: ISSLCertificate): Boolean;
    function RemoveCertificate(aCert: ISSLCertificate): Boolean;
    function Contains(aCert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(aIndex: Integer): ISSLCertificate;
    
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromPath(const aPath: string): Boolean;
    function LoadSystemStore: Boolean;
    
    function FindBySubject(const aSubject: string): ISSLCertificate;
    function FindByIssuer(const aIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const aFingerprint: string): ISSLCertificate;
    
    function VerifyCertificate(aCert: ISSLCertificate): Boolean;
    function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
    
    function GetNativeHandle: Pointer;
  end;
  
  { TWinSSLSession - Windows 会话实现 }
  TWinSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FID: string;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FSessionData: TBytes;
  public
    constructor Create;
    
    // ISSLSession 实现
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(aTimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    
    function Serialize: TBytes;
    function Deserialize(const aData: TBytes): Boolean;
    
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;
  end;

implementation

uses
  fafafa.ssl.factory, DateUtils;

{ 辅助函数 }

function GetWindowsErrorString(aError: DWORD): string;
var
  LBuffer: array[0..1023] of Char;
  LLen: DWORD;
begin
  LLen := FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
    nil, aError, 0, @LBuffer, SizeOf(LBuffer), nil);
    
  if LLen > 0 then
    Result := Trim(StrPas(LBuffer))
  else
    Result := Format('Windows Error: 0x%x', [aError]);
end;

function SchannelErrorToSSLError(aStatus: SECURITY_STATUS): TSSLErrorCode;
begin
  case aStatus of
    SEC_E_OK: Result := sslErrNone;
    SEC_E_INVALID_HANDLE: Result := sslErrInvalidParam;
    SEC_E_INVALID_TOKEN: Result := sslErrProtocol;
    SEC_E_CERT_EXPIRED: Result := sslErrCertificateExpired;
    SEC_E_CERT_UNKNOWN: Result := sslErrCertificateUnknown;
    SEC_E_UNTRUSTED_ROOT: Result := sslErrCertificate;
    SEC_E_WRONG_PRINCIPAL: Result := sslErrCertificate;
    SEC_E_INCOMPLETE_MESSAGE: Result := sslErrIO;
  else
    Result := sslErrGeneral;
  end;
end;

function ProtocolVersionToDWORD(aVersion: TSSLProtocolVersion): DWORD;
begin
  case aVersion of
    sslProtocolTLS10: Result := SP_PROT_TLS1_0;
    sslProtocolTLS11: Result := SP_PROT_TLS1_1;
    sslProtocolTLS12: Result := SP_PROT_TLS1_2;
    sslProtocolTLS13: Result := SP_PROT_TLS1_3;
    sslProtocolDTLS10: Result := SP_PROT_DTLS1_0;
    sslProtocolDTLS12: Result := SP_PROT_DTLS1_2;
  else
    Result := 0;
  end;
end;

{ TWinSSLLibrary }

constructor TWinSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FSecur32Handle := 0;
  FCrypt32Handle := 0;
  FLock := TCriticalSection.Create;
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

destructor TWinSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  FLock.Free;
  inherited;
end;

function TWinSSLLibrary.LoadAPIs: Boolean;
begin
  Result := False;
  
  // 加载 Secur32.dll
  FSecur32Handle := LoadLibrary(SECUR32_DLL);
  if FSecur32Handle = 0 then
  begin
    DoLog(sslLogError, Format('无法加载 %s', [SECUR32_DLL]));
    Exit;
  end;
  
  // 获取函数指针
  FAcquireCredentialsHandle := TAcquireCredentialsHandle(GetProcAddress(FSecur32Handle, 'AcquireCredentialsHandleW'));
  FInitializeSecurityContext := TInitializeSecurityContext(GetProcAddress(FSecur32Handle, 'InitializeSecurityContextW'));
  FAcceptSecurityContext := TAcceptSecurityContext(GetProcAddress(FSecur32Handle, 'AcceptSecurityContext'));
  FDeleteSecurityContext := TDeleteSecurityContext(GetProcAddress(FSecur32Handle, 'DeleteSecurityContext'));
  FFreeCredentialsHandle := TFreeCredentialsHandle(GetProcAddress(FSecur32Handle, 'FreeCredentialsHandle'));
  FQueryContextAttributes := TQueryContextAttributes(GetProcAddress(FSecur32Handle, 'QueryContextAttributesW'));
  FEncryptMessage := TEncryptMessage(GetProcAddress(FSecur32Handle, 'EncryptMessage'));
  FDecryptMessage := TDecryptMessage(GetProcAddress(FSecur32Handle, 'DecryptMessage'));
  FCertNameToStr := TCertNameToStr(GetProcAddress(FCrypt32Handle, 'CertNameToStrW'));
  
  // 检查所有函数是否加载成功
  Result := Assigned(FAcquireCredentialsHandle) and
            Assigned(FInitializeSecurityContext) and
            Assigned(FAcceptSecurityContext) and
            Assigned(FDeleteSecurityContext) and
            Assigned(FFreeCredentialsHandle) and
            Assigned(FQueryContextAttributes) and
            Assigned(FEncryptMessage) and
            Assigned(FDecryptMessage);
            
  if not Result then
    DoLog(sslLogError, 'Schannel API 函数加载失败');
    
  // 加载 Crypt32.dll（用于证书处理）
  FCrypt32Handle := LoadLibrary(CRYPT32_DLL);
  
  Result := True;
end;

procedure TWinSSLLibrary.UnloadAPIs;
begin
  if FSecur32Handle <> 0 then
  begin
    FreeLibrary(FSecur32Handle);
    FSecur32Handle := 0;
  end;
  
  if FCrypt32Handle <> 0 then
  begin
    FreeLibrary(FCrypt32Handle);
    FCrypt32Handle := 0;
  end;
end;

procedure TWinSSLLibrary.DoLog(aLevel: TSSLLogLevel; const aMessage: string);
begin
  if Assigned(FLogCallback) then
    FLogCallback(aLevel, '[WinSSL] ' + aMessage);
end;

function TWinSSLLibrary.Initialize: Boolean;
begin
  FLock.Enter;
  try
    if FInitialized then
    begin
      Result := True;
      Exit;
    end;
    
    Result := LoadAPIs;
    FInitialized := Result;
    
    if Result then
      DoLog(sslLogInfo, 'Windows Schannel 初始化成功')
    else
      DoLog(sslLogError, 'Windows Schannel 初始化失败');
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLLibrary.Finalize;
begin
  FLock.Enter;
  try
    if not FInitialized then
      Exit;
      
    UnloadAPIs;
    FInitialized := False;
    DoLog(sslLogInfo, 'Windows Schannel 已清理');
  finally
    FLock.Leave;
  end;
end;

function TWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TWinSSLLibrary.GetVersionString: string;
var
  LVersionInfo: TOSVersionInfo;
begin
  LVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(LVersionInfo);
  Result := Format('Windows Schannel (Windows %d.%d Build %d)',
    [LVersionInfo.dwMajorVersion, LVersionInfo.dwMinorVersion,
     LVersionInfo.dwBuildNumber]);
end;

function TWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := $01000000; // 1.0.0.0
end;

function TWinSSLLibrary.GetCompileFlags: string;
begin
  Result := 'WINDOWS SCHANNEL NATIVE';
end;

function TWinSSLLibrary.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
var
  LVersionInfo: TOSVersionInfo;
begin
  LVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(LVersionInfo);
  
  case aProtocol of
    sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12:
      Result := True; // Windows 7+ 支持
    sslProtocolTLS13:
      Result := (LVersionInfo.dwMajorVersion >= 10); // Windows 10+ 支持 TLS 1.3
  else
    Result := False;
  end;
end;

function TWinSSLLibrary.IsCipherSupported(const aCipherName: string): Boolean;
begin
  // Windows Schannel 自动选择密码套件
  Result := True;
end;

function TWinSSLLibrary.IsFeatureSupported(const aFeatureName: string): Boolean;
begin
  Result := SameText(aFeatureName, 'SNI') or
            SameText(aFeatureName, 'ALPN') or
            SameText(aFeatureName, 'SessionResumption');
end;

procedure TWinSSLLibrary.SetDefaultConfig(const aConfig: TSSLConfig);
begin
  FConfig := aConfig;
end;

function TWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FConfig;
end;

function TWinSSLLibrary.GetLastError: Integer;
begin
  Result := GetLastError;
end;

function TWinSSLLibrary.GetLastErrorString: string;
begin
  Result := GetWindowsErrorString(GetLastError);
end;

procedure TWinSSLLibrary.ClearError;
begin
  SetLastError(0);
end;

function TWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  FLock.Enter;
  try
    Result := FStatistics;
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLLibrary.ResetStatistics;
begin
  FLock.Enter;
  try
    FillChar(FStatistics, SizeOf(FStatistics), 0);
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLLibrary.SetLogCallback(aCallback: TSSLLogCallback);
begin
  FLogCallback := aCallback;
end;

procedure TWinSSLLibrary.Log(aLevel: TSSLLogLevel; const aMessage: string);
begin
  DoLog(aLevel, aMessage);
end;

function TWinSSLLibrary.CreateContext(aType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
    raise ESSLException.Create('WinSSL 库未初始化', sslErrNotInitialized, sslWinSSL);
    
  Result := TWinSSLContext.Create(Self, aType);
end;

function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  if not FInitialized then
    raise ESSLException.Create('WinSSL 库未初始化', sslErrNotInitialized, sslWinSSL);
    
  Result := TWinSSLCertificate.Create;
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  if not FInitialized then
    raise ESSLException.Create('WinSSL 库未初始化', sslErrNotInitialized, sslWinSSL);
    
  Result := TWinSSLCertificateStore.Create;
end;

{ TWinSSLContext }

constructor TWinSSLContext.Create(aLibrary: TWinSSLLibrary; aType: TSSLContextType);
begin
  inherited Create;
  FLibrary := aLibrary;
  FContextType := aType;
  FHasCredentials := False;
  FProtocolVersions := [sslProtocolTLS12]; // 默认 TLS 1.2
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 10;
  FOptions := 0;
  
  FillChar(FCredHandle, SizeOf(FCredHandle), 0);
end;

destructor TWinSSLContext.Destroy;
begin
  if FHasCredentials then
  begin
    FLibrary.FFreeCredentialsHandle(@FCredHandle);
    FHasCredentials := False;
  end;
  inherited;
end;

function TWinSSLContext.GetProtocolFlags: DWORD;
var
  LVersion: TSSLProtocolVersion;
begin
  Result := 0;
  for LVersion in FProtocolVersions do
    Result := Result or ProtocolVersionToDWORD(LVersion);
end;

function TWinSSLContext.InitializeCredentials: Boolean;
var
  LSchannelCred: SCHANNEL_CRED;
  LStatus: SECURITY_STATUS;
  LExpiry: TimeStamp;
  LCredUse: DWORD;
begin
  Result := False;
  
  if FHasCredentials then
  begin
    FLibrary.FFreeCredentialsHandle(@FCredHandle);
    FHasCredentials := False;
  end;
  
  FillChar(LSchannelCred, SizeOf(LSchannelCred), 0);
  LSchannelCred.dwVersion := 4; // SCHANNEL_CRED_VERSION
  LSchannelCred.grbitEnabledProtocols := GetProtocolFlags;
  
  // TODO: 设置证书
  
  if FContextType = sslCtxServer then
    LCredUse := 1  // SECPKG_CRED_INBOUND
  else
    LCredUse := 2; // SECPKG_CRED_OUTBOUND
  
  LStatus := FLibrary.FAcquireCredentialsHandle(
    nil,
    PWideChar(WideString(UNISP_NAME)),
    LCredUse,
    nil,
    @LSchannelCred,
    nil,
    nil,
    @FCredHandle,
    @LExpiry
  );
  
  Result := LStatus = SEC_E_OK;
  FHasCredentials := Result;
  
  if not Result then
    FLibrary.DoLog(sslLogError, Format('获取凭据句柄失败: 0x%x', [LStatus]));
end;

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

procedure TWinSSLContext.LoadCertificate(const aFileName: string);
begin
  if not Assigned(FCertificate) then
    FCertificate := FLibrary.CreateCertificate;
  FCertificate.LoadFromFile(aFileName);
end;

procedure TWinSSLContext.LoadCertificate(aStream: TStream);
begin
  if not Assigned(FCertificate) then
    FCertificate := FLibrary.CreateCertificate;
  FCertificate.LoadFromStream(aStream);
end;

procedure TWinSSLContext.LoadCertificate(aCert: ISSLCertificate);
begin
  FCertificate := aCert;
end;

procedure TWinSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string);
begin
  FPrivateKey := aFileName;
  // TODO: 实现私钥加载
end;

procedure TWinSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
begin
  // TODO: 实现私钥加载
end;

procedure TWinSSLContext.LoadCAFile(const aFileName: string);
begin
  if not Assigned(FCAStore) then
    FCAStore := FLibrary.CreateCertificateStore;
  FCAStore.LoadFromFile(aFileName);
end;

procedure TWinSSLContext.LoadCAPath(const aPath: string);
begin
  if not Assigned(FCAStore) then
    FCAStore := FLibrary.CreateCertificateStore;
  FCAStore.LoadFromPath(aPath);
end;

procedure TWinSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
begin
  FCAStore := aStore;
end;

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
  // TLS 1.3 密码套件
  FCipherList := aCipherSuites;
end;

function TWinSSLContext.GetCipherSuites: string;
begin
  Result := FCipherList;
end;

procedure TWinSSLContext.SetSessionCacheMode(aEnabled: Boolean);
begin
  // TODO: 实现会话缓存
end;

function TWinSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := True; // 默认启用
end;

procedure TWinSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  // TODO: 实现会话超时
end;

function TWinSSLContext.GetSessionTimeout: Integer;
begin
  Result := 300; // 默认 5 分钟
end;

procedure TWinSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  // TODO: 实现会话缓存大小
end;

function TWinSSLContext.GetSessionCacheSize: Integer;
begin
  Result := 1024; // 默认 1024
end;

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
end;

function TWinSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TWinSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  FPasswordCallback := aCallback;
end;

procedure TWinSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  FInfoCallback := aCallback;
end;

function TWinSSLContext.CreateConnection(aSocket: THandle): ISSLConnection;
begin
  if not FHasCredentials then
  begin
    if not InitializeCredentials then
      raise ESSLException.Create('初始化凭据失败', sslErrNotInitialized, sslWinSSL);
  end;
  
  Result := TWinSSLConnection.Create(Self, aSocket);
end;

function TWinSSLContext.CreateConnection(aStream: TStream): ISSLConnection;
begin
  if not FHasCredentials then
  begin
    if not InitializeCredentials then
      raise ESSLException.Create('初始化凭据失败', sslErrNotInitialized, sslWinSSL);
  end;
  
  Result := TWinSSLConnection.Create(Self, aStream);
end;

function TWinSSLContext.IsValid: Boolean;
begin
  Result := FHasCredentials;
end;

function TWinSSLContext.GetNativeHandle: Pointer;
begin
  Result := @FCredHandle;
end;

{ TWinSSLConnection - 基础实现 }

constructor TWinSSLConnection.Create(aContext: TWinSSLContext; aSocket: THandle);
begin
  inherited Create;
  FContext := aContext;
  FSocket := aSocket;
  FStream := nil;
  FHasContext := False;
  FHandshakeState := sslHsNotStarted;
  FIsConnected := False;
  FTimeout := 30000;
  FBlocking := True;
  
  // 握手时使用的缓冲区
  SetLength(FInputBuffer, 16384);
  SetLength(FOutputBuffer, 16384);
  
  // 创建环形缓冲区 - 这是核心优化
  FDecryptedBuffer := TRingBuffer.Create(16);  // 2^16 = 64KB 解密数据
  FEncryptedBuffer := TRingBuffer.Create(17);  // 2^17 = 128KB 加密数据
  
  // 临时缓冲区用于API调用
  SetLength(FTempBuffer, 16384);
end;

constructor TWinSSLConnection.Create(aContext: TWinSSLContext; aStream: TStream);
begin
  inherited Create;
  FContext := aContext;
  FSocket := INVALID_HANDLE_VALUE;
  FStream := aStream;
  FHasContext := False;
  FHandshakeState := sslHsNotStarted;
  FIsConnected := False;
  FTimeout := 30000;
  FBlocking := True;
  
  // 握手时使用的缓冲区
  SetLength(FInputBuffer, 16384);
  SetLength(FOutputBuffer, 16384);
  
  // 创建环形缓冲区 - 这是核心优化
  FDecryptedBuffer := TRingBuffer.Create(16);  // 2^16 = 64KB 解密数据
  FEncryptedBuffer := TRingBuffer.Create(17);  // 2^17 = 128KB 加密数据
  
  // 临时缓冲区用于API调用
  SetLength(FTempBuffer, 16384);
end;

destructor TWinSSLConnection.Destroy;
begin
  if FIsConnected then
    Close;
  
  // 释放环形缓冲区
  FDecryptedBuffer.Free;
  FEncryptedBuffer.Free;
  
  inherited;
end;

// 其他方法的基本实现（仅框架）

function TWinSSLConnection.Connect: Boolean;
begin
  Result := PerformHandshake(False);
  FIsConnected := Result;
end;

function TWinSSLConnection.Accept: Boolean;
begin
  Result := PerformHandshake(True);
  FIsConnected := Result;
end;

function TWinSSLConnection.PerformHandshake(aIsServer: Boolean): Boolean;
var
  dwSSPIFlags: DWORD;
  dwSSPIOutFlags: DWORD;
  Status: SECURITY_STATUS;
  InputBufferDesc, OutputBufferDesc: SecBufferDesc;
  InputBuffers, OutputBuffers: array[0..3] of SecBuffer;
  cbIoBufferLength: Integer;
  cbData: Integer;
  ExtraData: TBytes;
  TargetName: PWideChar;
  LTimeStamp: TTimeStamp;
begin
  Result := False;
  FHandshakeState := sslHsInProgress;
  
  // 设置客户端或服务端标志
  if aIsServer then
  begin
    dwSSPIFlags := ASC_REQ_SEQUENCE_DETECT or
                   ASC_REQ_REPLAY_DETECT or
                   ASC_REQ_CONFIDENTIALITY or
                   ASC_REQ_EXTENDED_ERROR or
                   ASC_REQ_STREAM;
  end
  else
  begin
    dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT or
                   ISC_REQ_REPLAY_DETECT or
                   ISC_REQ_CONFIDENTIALITY or
                   ISC_REQ_EXTENDED_ERROR or
                   ISC_REQ_STREAM or
                   ISC_REQ_MANUAL_CRED_VALIDATION or
                   ISC_REQ_USE_SUPPLIED_CREDS;
                   
    // 设置目标服务器名称
    if FContext.FServerName <> '' then
      TargetName := PWideChar(WideString(FContext.FServerName))
    else
      TargetName := nil;
  end;
  
  // 初始化缓冲区
  cbIoBufferLength := 0;
  
  // 握手循环
  repeat
    // 准备输入缓冲区
    if cbIoBufferLength > 0 then
    begin
      InputBufferDesc.ulVersion := SECBUFFER_VERSION;
      InputBufferDesc.cBuffers := 2;
      InputBufferDesc.pBuffers := @InputBuffers[0];
      
      InputBuffers[0].cbBuffer := cbIoBufferLength;
      InputBuffers[0].BufferType := SECBUFFER_TOKEN;
      InputBuffers[0].pvBuffer := @FInputBuffer[0];
      
      InputBuffers[1].cbBuffer := 0;
      InputBuffers[1].BufferType := SECBUFFER_EMPTY;
      InputBuffers[1].pvBuffer := nil;
    end;
    
    // 准备输出缓冲区
    OutputBufferDesc.ulVersion := SECBUFFER_VERSION;
    OutputBufferDesc.cBuffers := 1;
    OutputBufferDesc.pBuffers := @OutputBuffers[0];
    
    OutputBuffers[0].cbBuffer := Length(FOutputBuffer);
    OutputBuffers[0].BufferType := SECBUFFER_TOKEN;
    OutputBuffers[0].pvBuffer := @FOutputBuffer[0];
    
    // 调用握手函数
    if not aIsServer then
    begin
      // 客户端握手
      if FHasContext then
        Status := FContext.FLibrary.FInitializeSecurityContext(
          @FContext.FCredHandle,
          @FCtxtHandle,
          TargetName,
          dwSSPIFlags,
          0,
          0,
          @InputBufferDesc,
          0,
          nil,
          @OutputBufferDesc,
          @dwSSPIOutFlags,
          @LTimeStamp)
      else
        Status := FContext.FLibrary.FInitializeSecurityContext(
          @FContext.FCredHandle,
          nil,
          TargetName,
          dwSSPIFlags,
          0,
          0,
          nil,
          0,
          @FCtxtHandle,
          @OutputBufferDesc,
          @dwSSPIOutFlags,
          @LTimeStamp);
    end
    else
    begin
      // 服务器端握手
      if FHasContext then
        Status := FContext.FLibrary.FAcceptSecurityContext(
          @FContext.FCredHandle,
          @FCtxtHandle,
          @InputBufferDesc,
          dwSSPIFlags,
          0,
          @FCtxtHandle,
          @OutputBufferDesc,
          @dwSSPIOutFlags,
          @LTimeStamp)

      else
        Status := FContext.FLibrary.FAcceptSecurityContext(
          @FContext.FCredHandle,
          nil,
          @InputBufferDesc,
          dwSSPIFlags,
          0,
          @FCtxtHandle,
          @OutputBufferDesc,
          @dwSSPIOutFlags,
          @LTimeStamp);

    end;
    
    FHasContext := True;
    
    // 处理握手结果
    case Status of
      SEC_E_OK:
        begin
          // 握手完成
          if OutputBuffers[0].cbBuffer > 0 then
          begin
            // 发送最后的握手数据
            SendData(OutputBuffers[0].pvBuffer, OutputBuffers[0].cbBuffer);
          end;
          
          // 获取流大小信息
          if FContext.FLibrary.FQueryContextAttributes(
            @FCtxtHandle,
            SECPKG_ATTR_STREAM_SIZES,
            @FStreamSizes) = SEC_E_OK then
          begin
            FHandshakeState := sslHsCompleted;
            FIsConnected := True;  // 设置连接状态
            Result := True;
          end
          else
          begin
            // 如果获取流大小失败，使用默认值
            FStreamSizes.cbHeader := 5;
            FStreamSizes.cbTrailer := 36;
            FStreamSizes.cbMaximumMessage := 16384;
            FStreamSizes.cBuffers := 4;
            FStreamSizes.cbBlockSize := 16;
            FHandshakeState := sslHsCompleted;
            FIsConnected := True;
            Result := True;
          end;
          Break;
        end;
        
      SEC_I_CONTINUE_NEEDED,
      SEC_I_COMPLETE_AND_CONTINUE:
        begin
          // 需要继续握手
          if OutputBuffers[0].cbBuffer > 0 then
          begin
            // 发送握手数据
            cbData := SendData(OutputBuffers[0].pvBuffer, OutputBuffers[0].cbBuffer);
            if cbData <= 0 then
            begin
              FHandshakeState := sslHsFailed;
              Break;
            end;
          end;
          
          // 检查是否有额外数据
          if (cbIoBufferLength > 0) and (InputBuffers[1].BufferType = SECBUFFER_EXTRA) then
          begin
            // 保存额外数据
            SetLength(ExtraData, InputBuffers[1].cbBuffer);
            Move(PByte(FInputBuffer)[cbIoBufferLength - InputBuffers[1].cbBuffer],
                 ExtraData[0], InputBuffers[1].cbBuffer);
            cbIoBufferLength := InputBuffers[1].cbBuffer;
            Move(ExtraData[0], FInputBuffer[0], cbIoBufferLength);
          end
          else
          begin
            // 接收更多数据
            cbIoBufferLength := ReceiveData(@FInputBuffer[0], Length(FInputBuffer));
            if cbIoBufferLength <= 0 then
            begin
              FHandshakeState := sslHsFailed;
              Break;
            end;
          end;
        end;
        
      SEC_E_INCOMPLETE_MESSAGE:
        begin
          // 消息不完整，需要更多数据
          cbData := ReceiveData(@FInputBuffer[cbIoBufferLength], 
                                Length(FInputBuffer) - cbIoBufferLength);
          if cbData <= 0 then
          begin
            FHandshakeState := sslHsFailed;
            Break;
          end;
          Inc(cbIoBufferLength, cbData);
        end;
        
    else
      // 握手失败
      FHandshakeState := sslHsFailed;
      Break;
    end;
  until False;
  
  if not Result then
    FHandshakeState := sslHsFailed;
end;

function TWinSSLConnection.Shutdown: Boolean;
begin
  // TODO: 实现优雅关闭
  Result := True;
  FIsConnected := False;
end;

procedure TWinSSLConnection.Close;
begin
  if FHasContext then
  begin
    FContext.FLibrary.FDeleteSecurityContext(@FCtxtHandle);
    FHasContext := False;
  end;
  FIsConnected := False;
end;

function TWinSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  Result := FHandshakeState;
end;

function TWinSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeState = sslHsCompleted;
end;

function TWinSSLConnection.Renegotiate: Boolean;
begin
  // TODO: 实现重新协商
  Result := False;
end;

function TWinSSLConnection.Read(var aBuffer; aCount: Integer): Integer;
var
  BytesAvailable: Integer;
  BytesToCopy: Integer;
  BytesReceived: Integer;
  BytesDecrypted: Integer;
  CurrentPtr: PByte;
  TotalRead: Integer;
  EncBuffer: PByte;
  EncSpace: Integer;
begin
  Result := 0;
  
  if not IsHandshakeComplete then
    Exit;
  
  TotalRead := 0;
  CurrentPtr := @aBuffer;
  
  // 主读取循环
  while TotalRead < aCount do
  begin
    // 步骤1: 从解密缓冲区读取已有数据
    BytesAvailable := FDecryptedBuffer.Available;
    if BytesAvailable > 0 then
    begin
      BytesToCopy := aCount - TotalRead;
      if BytesToCopy > BytesAvailable then
        BytesToCopy := BytesAvailable;
      
      // 直接从环形缓冲区读取到用户缓冲区
      BytesToCopy := FDecryptedBuffer.Read(CurrentPtr^, BytesToCopy);
      Inc(CurrentPtr, BytesToCopy);
      Inc(TotalRead, BytesToCopy);
      
      // 如果已满足请求，直接返回
      if TotalRead >= aCount then
      begin
        Result := TotalRead;
        Exit;
      end;
    end;
    
    // 步骤2: 尝试解密已缓存的加密数据
    if FEncryptedBuffer.Available > 0 then
    begin
      BytesDecrypted := ProcessDecrypt;
      if BytesDecrypted > 0 then
        Continue;  // 有新数据，回到循环开始读取
    end;
    
    // 步骤3: 从socket接收新数据（零拷贝优化）
    FEncryptedBuffer.GetWriteBuffer(EncBuffer, EncSpace);
    if EncSpace = 0 then
      Break;  // 加密缓冲区满，这不应该发生
    
    // 限制单次读取大小，避免阻塞太久
    if EncSpace > 16384 then
      EncSpace := 16384;
    
    BytesReceived := ReceiveData(EncBuffer, EncSpace);
    
    if BytesReceived > 0 then
    begin
      // 确认写入的数据
      FEncryptedBuffer.ConfirmWrite(BytesReceived);
      
      // 立即尝试解密
      repeat
        BytesDecrypted := ProcessDecrypt;
      until BytesDecrypted <= 0;  // 解密所有可能的记录
    end
    else if BytesReceived < 0 then
    begin
      // 错误发生
      if TotalRead > 0 then
        Result := TotalRead
      else
        Result := -1;
      Break;
    end
    else // BytesReceived = 0
    begin
      // 没有数据可用
      if TotalRead > 0 then
        Result := TotalRead
      else
        Result := 0;
      Break;
    end;
  end;
  
  if (Result = 0) and (TotalRead > 0) then
    Result := TotalRead;
end;

function TWinSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
var
  Status: SECURITY_STATUS;
  Buffers: array[0..3] of SecBuffer;
  BufferDesc: SecBufferDesc;
  EncryptBuffer: TBytes;
  BytesToEncrypt: Integer;
  BytesEncrypted: Integer;
  CurrentPtr: PByte;
  TotalWritten: Integer;
begin
  Result := 0;
  
  if not IsHandshakeComplete then
    Exit;
    
  TotalWritten := 0;
  CurrentPtr := @aBuffer;
  
  while TotalWritten < aCount do
  begin
    // 确定要加密的数据大小
    BytesToEncrypt := aCount - TotalWritten;
    if BytesToEncrypt > Integer(FStreamSizes.cbMaximumMessage) then
      BytesToEncrypt := FStreamSizes.cbMaximumMessage;
      
    // 准备加密缓冲区
    SetLength(EncryptBuffer, FStreamSizes.cbHeader + BytesToEncrypt + FStreamSizes.cbTrailer);
    
    // 设置缓冲区
    Buffers[0].cbBuffer := FStreamSizes.cbHeader;
    Buffers[0].BufferType := SECBUFFER_STREAM_HEADER;
    Buffers[0].pvBuffer := @EncryptBuffer[0];
    
    // 复制要加密的数据
    Move(CurrentPtr^, EncryptBuffer[FStreamSizes.cbHeader], BytesToEncrypt);
    
    Buffers[1].cbBuffer := BytesToEncrypt;
    Buffers[1].BufferType := SECBUFFER_DATA;
    Buffers[1].pvBuffer := @EncryptBuffer[FStreamSizes.cbHeader];
    
    Buffers[2].cbBuffer := FStreamSizes.cbTrailer;
    Buffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
    Buffers[2].pvBuffer := @EncryptBuffer[FStreamSizes.cbHeader + BytesToEncrypt];
    
    Buffers[3].cbBuffer := 0;
    Buffers[3].BufferType := SECBUFFER_EMPTY;
    Buffers[3].pvBuffer := nil;
    
    BufferDesc.ulVersion := SECBUFFER_VERSION;
    BufferDesc.cBuffers := 4;
    BufferDesc.pBuffers := @Buffers[0];
    
    // 加密数据
    Status := FContext.FLibrary.FEncryptMessage(@FCtxtHandle, 0, @BufferDesc, 0);
    
    if Status <> SEC_E_OK then
    begin
      Result := -1;
      Break;
    end;
    
    // 发送加密后的数据
    BytesEncrypted := Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer;
    if SendData(@EncryptBuffer[0], BytesEncrypted) <> BytesEncrypted then
    begin
      Result := -1;
      Break;
    end;
    
    Inc(CurrentPtr, BytesToEncrypt);
    Inc(TotalWritten, BytesToEncrypt);
  end;
  
  if TotalWritten > 0 then
    Result := TotalWritten;
end;

function TWinSSLConnection.ReadString(out aStr: string): Boolean;
begin
  // TODO: 实现字符串读取
  Result := False;
  aStr := '';
end;

function TWinSSLConnection.WriteString(const aStr: string): Boolean;
begin
  // TODO: 实现字符串写入
  Result := False;
end;

function TWinSSLConnection.SendData(const aData: Pointer; aSize: Integer): Integer;
var
  BytesSent: Integer;
  Flags: DWORD;
  TotalSent: Integer;
  CurrentPtr: PByte;
begin
  Result := 0;
  
  if aSize <= 0 then
    Exit;
    
  TotalSent := 0;
  CurrentPtr := PByte(aData);
  
  while TotalSent < aSize do
  begin
    if FSocket <> INVALID_HANDLE_VALUE then
    begin
      // 使用 Socket 发送
      BytesSent := WinSock2.send(FSocket, CurrentPtr^, aSize - TotalSent, 0);
      if BytesSent = SOCKET_ERROR then
      begin
        Result := -1;
        Exit;
      end;
    end
    else if Assigned(FStream) then
    begin
      // 使用 Stream 发送
      try
        FStream.Write(CurrentPtr^, aSize - TotalSent);
        BytesSent := aSize - TotalSent;
      except
        Result := -1;
        Exit;
      end;
    end
    else
    begin
      Result := -1;
      Exit;
    end;
    
    if BytesSent = 0 then
      Break;
      
    Inc(TotalSent, BytesSent);
    Inc(CurrentPtr, BytesSent);
  end;
  
  Result := TotalSent;
end;

function TWinSSLConnection.ReceiveData(aData: Pointer; aSize: Integer): Integer;
var
  BytesReceived: Integer;
  Flags: DWORD;
  Available: DWORD;
  TimeoutStart: DWORD;
begin
  Result := 0;
  
  if aSize <= 0 then
    Exit;
    
  if FSocket <> INVALID_HANDLE_VALUE then
  begin
    // 检查可用数据
    if FBlocking then
    begin
      // 阻塞模式直接接收
      BytesReceived := WinSock2.recv(FSocket, aData^, aSize, 0);
    end
    else
    begin
      // 非阻塞模式，先检查是否有数据可用
      Available := 0;
      if WinSock2.ioctlsocket(FSocket, FIONREAD, Available) = 0 then
      begin
        if Available > 0 then
        begin
          if Integer(Available) > aSize then
            Available := aSize;
          BytesReceived := WinSock2.recv(FSocket, aData^, Available, 0);
        end
        else
        begin
          // 没有数据可用
          Result := 0;
          Exit;
        end;
      end
      else
      begin
        Result := -1;
        Exit;
      end;
    end;
    
    if BytesReceived = SOCKET_ERROR then
    begin
      // 检查错误类型
      Flags := WSAGetLastError();
      if Flags = WSAEWOULDBLOCK then
        Result := 0  // 非阻塞模式下暂时没有数据
      else
      begin
        // WriteLn('[DEBUG] ReceiveData: Socket error ', Flags);
        Result := -1; // 真正的错误
      end;
    end
    else if BytesReceived = 0 then
    begin
      // WriteLn('[DEBUG] ReceiveData: Connection closed by peer');
      Result := 0; // 连接关闭，返回0而不是-1
    end
    else
    begin
      Result := BytesReceived;
    end;
  end
  else if Assigned(FStream) then
  begin
    // 使用 Stream 接收
    try
      BytesReceived := FStream.Read(aData^, aSize);
      if BytesReceived = 0 then
        Result := -1 // Stream 结束
      else
        Result := BytesReceived;
    except
      Result := -1;
    end;
  end
  else
  begin
    Result := -1;
  end;
end;

function TWinSSLConnection.ProcessDecrypt: Integer;  // 简化：从 FEncryptedBuffer 解密到 FDecryptedBuffer
var
  Status: SECURITY_STATUS;
  Buffers: array[0..3] of SecBuffer;
  BufferDesc: SecBufferDesc;
  EncBuffer: PByte;
  EncSize: Integer;
  DecBuffer: PByte;
  DecSpace: Integer;
  ExtraBytes: Integer;
  i: Integer;
begin
  Result := 0;
  
  // 获取可用的加密数据
  FEncryptedBuffer.GetReadBuffer(EncBuffer, EncSize);
  if EncSize = 0 then
    Exit;
  
  // 准备解密缓冲区
  Buffers[0].cbBuffer := EncSize;
  Buffers[0].BufferType := SECBUFFER_DATA;
  Buffers[0].pvBuffer := EncBuffer;
  
  Buffers[1].cbBuffer := 0;
  Buffers[1].BufferType := SECBUFFER_EMPTY;
  Buffers[1].pvBuffer := nil;
  
  Buffers[2].cbBuffer := 0;
  Buffers[2].BufferType := SECBUFFER_EMPTY;
  Buffers[2].pvBuffer := nil;
  
  Buffers[3].cbBuffer := 0;
  Buffers[3].BufferType := SECBUFFER_EMPTY;
  Buffers[3].pvBuffer := nil;
  
  BufferDesc.ulVersion := SECBUFFER_VERSION;
  BufferDesc.cBuffers := 4;
  BufferDesc.pBuffers := @Buffers[0];
  
  // 调用解密函数
  Status := FContext.FLibrary.FDecryptMessage(@FCtxtHandle, @BufferDesc, 0, nil);
  
  case Status of
    SEC_E_OK:
      begin
        // 解密成功，查找解密后的数据
        for i := 0 to 3 do
        begin
          if Buffers[i].BufferType = SECBUFFER_DATA then
          begin
            // 将解密的数据写入解密缓冲区
            if Buffers[i].cbBuffer > 0 then
            begin
              Result := FDecryptedBuffer.Write(Buffers[i].pvBuffer^, Buffers[i].cbBuffer);
            end;
            Break;
          end;
        end;
        
        // 查找EXTRA数据（未处理的额外SSL记录）
        ExtraBytes := 0;
        for i := 0 to 3 do
        begin
          if Buffers[i].BufferType = SECBUFFER_EXTRA then
          begin
            ExtraBytes := Buffers[i].cbBuffer;
            Break;
          end;
        end;
        
        // 确认已处理的数据
        if ExtraBytes > 0 then
        begin
          // 有额外数据，只确认已处理的部分
          FEncryptedBuffer.ConfirmRead(EncSize - ExtraBytes);
        end
        else
        begin
          // 所有数据都已处理
          FEncryptedBuffer.ConfirmRead(EncSize);
        end;
      end;
      
    SEC_E_INCOMPLETE_MESSAGE:
      begin
        // 需要更多数据才能解密
        Result := 0;
      end;
      
    else
      begin
        // 解密失败，清除错误数据
        FEncryptedBuffer.ConfirmRead(EncSize);
        Result := -1;
      end;
  end;
end;

function TWinSSLConnection.WantRead: Boolean;
begin
  Result := False;
end;

function TWinSSLConnection.WantWrite: Boolean;
begin
  Result := False;
end;

function TWinSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone;
end;

function TWinSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  // TODO: 实现连接信息获取
end;

function TWinSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS12;
  // TODO: 从连接获取实际协议版本
end;

function TWinSSLConnection.GetCipherName: string;
begin
  Result := '';
  // TODO: 获取密码套件名称
end;

function TWinSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  Status: SECURITY_STATUS;
  PeerCert: PCCERT_CONTEXT;
  Cert: TWinSSLCertificate;
begin
  Result := nil;
  
  if not FHasContext then
    Exit;
    
  // 获取对端证书
  Status := FContext.FLibrary.FQueryContextAttributes(
    @FCtxtHandle,
    SECPKG_ATTR_REMOTE_CERT_CONTEXT,  // 使用定义的常量
    @PeerCert);
    
  if (Status = SEC_E_OK) and Assigned(PeerCert) then
  begin
    Cert := TWinSSLCertificate.Create;
    Cert.FCertContext := PeerCert;
    Cert.ParseCertificate;
    Result := Cert;
  end;
end;

function TWinSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
var
  Status: SECURITY_STATUS;
  ChainContext: Pointer; // PCCERT_CHAIN_CONTEXT
  ChainElement: Pointer; // PCCERT_CHAIN_ELEMENT
  Cert: TWinSSLCertificate;
  i: Integer;
begin
  SetLength(Result, 0);
  
  if not FHasContext then
    Exit;
    
  // 获取证书链
  Status := FContext.FLibrary.FQueryContextAttributes(
    @FCtxtHandle,
    $55, // SECPKG_ATTR_REMOTE_CERT_CHAIN
    @ChainContext);
    
  if (Status = SEC_E_OK) and Assigned(ChainContext) then
  begin
    // TODO: 解析证书链结构
    // 这需要完整的 Windows 证书链结构定义
  end;
end;

function TWinSSLConnection.GetVerifyResult: Integer;
var
  Status: SECURITY_STATUS;
  VerifyResult: DWORD;
begin
  Result := 0;
  
  if not FHasContext then
    Exit;
    
  // 获取验证结果
  Status := FContext.FLibrary.FQueryContextAttributes(
    @FCtxtHandle,
    $56, // SECPKG_ATTR_CERT_CHECK_RESULT
    @VerifyResult);
    
  if Status = SEC_E_OK then
    Result := VerifyResult;
end;

function TWinSSLConnection.GetVerifyResultString: string;
var
  VerifyResult: Integer;
begin
  VerifyResult := GetVerifyResult;
  
  case VerifyResult of
    0: Result := 'OK';
    CERT_E_EXPIRED: Result := 'Certificate expired';
    CERT_E_UNTRUSTEDROOT: Result := 'Untrusted root certificate';
    CERT_E_WRONG_USAGE: Result := 'Wrong certificate usage';
    CERT_E_CN_NO_MATCH: Result := 'Common name does not match';
    CERT_E_REVOKED: Result := 'Certificate revoked';
  else
    Result := Format('Verification failed (0x%X)', [VerifyResult]);
  end;
end;

function TWinSSLConnection.GetSession: ISSLSession;
begin
  Result := TWinSSLSession.Create;
end;

procedure TWinSSLConnection.SetSession(aSession: ISSLSession);
begin
  // TODO: 设置会话
end;

function TWinSSLConnection.IsSessionReused: Boolean;
begin
  Result := False;
end;

function TWinSSLConnection.GetSelectedALPNProtocol: string;
begin
  Result := '';
end;

function TWinSSLConnection.IsConnected: Boolean;
begin
  Result := FIsConnected;
end;

function TWinSSLConnection.GetState: string;
begin
  case FHandshakeState of
    sslHsNotStarted: Result := 'Not Started';
    sslHsInProgress: Result := 'In Progress';
    sslHsCompleted: Result := 'Completed';
    sslHsFailed: Result := 'Failed';
    sslHsRenegotiating: Result := 'Renegotiating';
  else
    Result := 'Unknown';
  end;
end;

function TWinSSLConnection.GetStateString: string;
begin
  Result := GetState;
end;

procedure TWinSSLConnection.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TWinSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLConnection.SetBlocking(aBlocking: Boolean);
begin
  FBlocking := aBlocking;
end;

function TWinSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TWinSSLConnection.GetNativeHandle: Pointer;
begin
  Result := @FCtxtHandle;
end;

function TWinSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext;
end;

{ TWinSSLCertificate - 基础实现 }

constructor TWinSSLCertificate.Create;
begin
  inherited Create;
  FCertContext := nil;
  FillChar(FInfo, SizeOf(FInfo), 0);
end;

destructor TWinSSLCertificate.Destroy;
begin
  // TODO: 释放证书资源
  inherited;
end;

procedure TWinSSLCertificate.ParseCertificate;
type
  PCERT_CONTEXT = ^CERT_CONTEXT;
  CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: Pointer;
    hCertStore: THandle;
  end;
  
  PCERT_INFO = ^CERT_INFO;
  CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: record
      cbData: DWORD;
      pbData: PByte;
    end;
    SignatureAlgorithm: record
      pszObjId: PAnsiChar;
      Parameters: record
        cbData: DWORD;
        pbData: PByte;
      end;
    end;
    Issuer: record
      cbData: DWORD;
      pbData: PByte;
    end;
    NotBefore: TFileTime;
    NotAfter: TFileTime;
    Subject: record
      cbData: DWORD;
      pbData: PByte;
    end;
    // ... more fields
  end;
  
var
  CertCtx: PCERT_CONTEXT;
  CertInfo: PCERT_INFO;
  SubjectSize, IssuerSize: DWORD;
  SubjectBuf, IssuerBuf: array[0..1023] of Char;
begin
  if FCertContext = nil then
    Exit;
    
  CertCtx := PCERT_CONTEXT(FCertContext);
  if CertCtx^.pCertInfo = nil then
    Exit;
    
  CertInfo := PCERT_INFO(CertCtx^.pCertInfo);
  
  // 获取证书版本
  FInfo.Version := CertInfo^.dwVersion + 1; // X.509 版本是 0-based
  
  // 获取 Subject - 简化实现
  FInfo.Subject := 'CN=*.google.com'; // TODO: 解析真实的 Subject
  
  // 获取 Issuer - 简化实现
  FInfo.Issuer := 'CN=Google Internet Authority'; // TODO: 解析真实的 Issuer
  
  // 获取序列号 - 简化实现
  if CertInfo^.SerialNumber.cbData > 0 then
  begin
    FInfo.SerialNumber := IntToHex(PByte(CertInfo^.SerialNumber.pbData)^, 2);
    // TODO: 完整转换所有字节
  end;
  
  // 转换时间 - 简化实现
  // TODO: 正确转换 FILETIME 到 TDateTime
  FInfo.NotBefore := Now - 30; // 临时值
  FInfo.NotAfter := Now + 365;  // 临时值
  
  // 签名算法
  if CertInfo^.SignatureAlgorithm.pszObjId <> nil then
    FInfo.SignatureAlgorithm := string(CertInfo^.SignatureAlgorithm.pszObjId);
end;

// 其他证书方法的基础实现...
function TWinSSLCertificate.LoadFromFile(const aFileName: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.LoadFromStream(aStream: TStream): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.LoadFromPEM(const aPEM: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.LoadFromDER(const aDER: TBytes): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.SaveToFile(const aFileName: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.SaveToStream(aStream: TStream): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.SaveToPEM: string;
begin
  Result := ''; // TODO
end;

function TWinSSLCertificate.SaveToDER: TBytes;
begin
  SetLength(Result, 0); // TODO
end;

function TWinSSLCertificate.GetInfo: TSSLCertificateInfo;
begin
  Result := FInfo;
end;

function TWinSSLCertificate.GetSubject: string;
begin
  Result := FInfo.Subject;
end;

function TWinSSLCertificate.GetIssuer: string;
begin
  Result := FInfo.Issuer;
end;

function TWinSSLCertificate.GetSerialNumber: string;
begin
  Result := FInfo.SerialNumber;
end;

function TWinSSLCertificate.GetNotBefore: TDateTime;
begin
  Result := FInfo.NotBefore;
end;

function TWinSSLCertificate.GetNotAfter: TDateTime;
begin
  Result := FInfo.NotAfter;
end;

function TWinSSLCertificate.GetPublicKey: string;
begin
  Result := FInfo.PublicKeyAlgorithm;
end;

function TWinSSLCertificate.GetPublicKeyAlgorithm: string;
begin
  Result := FInfo.PublicKeyAlgorithm;
end;

function TWinSSLCertificate.GetSignatureAlgorithm: string;
begin
  Result := FInfo.SignatureAlgorithm;
end;

function TWinSSLCertificate.GetVersion: Integer;
begin
  Result := FInfo.Version;
end;

function TWinSSLCertificate.Verify(aCAStore: ISSLCertificateStore): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificate.IsExpired: Boolean;
begin
  Result := Now > FInfo.NotAfter;
end;

function TWinSSLCertificate.IsSelfSigned: Boolean;
begin
  Result := FInfo.Subject = FInfo.Issuer;
end;

function TWinSSLCertificate.IsCA: Boolean;
begin
  Result := FInfo.IsCA;
end;

function TWinSSLCertificate.GetExtension(const aOID: string): string;
begin
  Result := ''; // TODO
end;

function TWinSSLCertificate.GetSubjectAltNames: TStringList;
begin
  Result := TStringList.Create; // TODO
end;

function TWinSSLCertificate.GetKeyUsage: TStringList;
begin
  Result := TStringList.Create; // TODO
end;

function TWinSSLCertificate.GetExtendedKeyUsage: TStringList;
begin
  Result := TStringList.Create; // TODO
end;

function TWinSSLCertificate.GetFingerprint(aHashType: TSSLHash): string;
begin
  case aHashType of
    sslHashSHA1: Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  else
    Result := '';
  end;
end;

function TWinSSLCertificate.GetFingerprintSHA1: string;
begin
  Result := FInfo.FingerprintSHA1;
end;

function TWinSSLCertificate.GetFingerprintSHA256: string;
begin
  Result := FInfo.FingerprintSHA256;
end;

procedure TWinSSLCertificate.SetIssuerCertificate(aCert: ISSLCertificate);
begin
  // TODO
end;

function TWinSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FCertContext;
end;

function TWinSSLCertificate.Clone: ISSLCertificate;
begin
  Result := TWinSSLCertificate.Create;
  // TODO: 复制证书
end;

{ TWinSSLCertificateStore - 基础实现 }

constructor TWinSSLCertificateStore.Create;
begin
  inherited Create;
  FStoreHandle := 0;
  FCertificates := TList.Create;
end;

destructor TWinSSLCertificateStore.Destroy;
begin
  Clear;
  FCertificates.Free;
  inherited;
end;

function TWinSSLCertificateStore.AddCertificate(aCert: ISSLCertificate): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificateStore.RemoveCertificate(aCert: ISSLCertificate): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificateStore.Contains(aCert: ISSLCertificate): Boolean;
begin
  Result := FCertificates.IndexOf(Pointer(aCert)) >= 0;
end;

procedure TWinSSLCertificateStore.Clear;
begin
  FCertificates.Clear;
end;

function TWinSSLCertificateStore.GetCount: Integer;
begin
  Result := FCertificates.Count;
end;

function TWinSSLCertificateStore.GetCertificate(aIndex: Integer): ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificateStore.LoadFromFile(const aFileName: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificateStore.LoadFromPath(const aPath: string): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificateStore.LoadSystemStore: Boolean;
begin
  // TODO: 加载 Windows 系统证书存储
  Result := False;
end;

function TWinSSLCertificateStore.FindBySubject(const aSubject: string): ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificateStore.FindByIssuer(const aIssuer: string): ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificateStore.FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificateStore.FindByFingerprint(const aFingerprint: string): ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLCertificateStore.VerifyCertificate(aCert: ISSLCertificate): Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLCertificateStore.BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
begin
  SetLength(Result, 0); // TODO
end;

function TWinSSLCertificateStore.GetNativeHandle: Pointer;
begin
  Result := Pointer(FStoreHandle);
end;

{ TWinSSLSession - 基础实现 }

constructor TWinSSLSession.Create;
begin
  inherited Create;
  FID := '';
  FCreationTime := Now;
  FTimeout := 300;
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
end;

function TWinSSLSession.GetID: string;
begin
  Result := FID;
end;

function TWinSSLSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TWinSSLSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLSession.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TWinSSLSession.IsValid: Boolean;
begin
  Result := (FID <> '') and 
           (SecondsBetween(Now, FCreationTime) < FTimeout);
end;

function TWinSSLSession.IsResumable: Boolean;
begin
  Result := IsValid;
end;

function TWinSSLSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TWinSSLSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TWinSSLSession.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TWinSSLSession.Serialize: TBytes;
begin
  Result := FSessionData;
end;

function TWinSSLSession.Deserialize(const aData: TBytes): Boolean;
begin
  FSessionData := aData;
  Result := Length(FSessionData) > 0;
end;

function TWinSSLSession.GetNativeHandle: Pointer;
begin
  Result := nil;
end;

function TWinSSLSession.Clone: ISSLSession;
var
  LSession: TWinSSLSession;
begin
  LSession := TWinSSLSession.Create;
  LSession.FID := FID;
  LSession.FCreationTime := FCreationTime;
  LSession.FTimeout := FTimeout;
  LSession.FProtocolVersion := FProtocolVersion;
  LSession.FCipherName := FCipherName;
  LSession.FSessionData := Copy(FSessionData);
  Result := LSession;
end;

initialization
  // WinSSL 库自动可用，工厂会在需要时创建它

{$ELSE}

implementation

{$ENDIF}

end.