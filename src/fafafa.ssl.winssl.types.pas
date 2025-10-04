{
  fafafa.ssl.winssl.types - Windows Schannel API 类型定义
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-04
  
  描述:
    定义 Windows Schannel (WinSSL) 后端所需的所有类型、常量和结构体。
    这些定义来自 Windows SDK 的 Schannel.h, Sspi.h 和 Wincrypt.h。
}

unit fafafa.ssl.winssl.types;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils;

type
  // ============================================================================
  // 基础类型定义
  // ============================================================================
  
  ULONG_PTR = NativeUInt;
  SECURITY_STATUS = LONG;
  
  PTimeStamp = ^TTimeStamp;
  TTimeStamp = record
    LowPart: DWORD;
    HighPart: LONG;
  end;
  
  // ============================================================================
  // Security Handle 定义 (来自 Sspi.h)
  // ============================================================================
  
  PSecHandle = ^TSecHandle;
  TSecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  
  // 凭据句柄和上下文句柄
  CredHandle = TSecHandle;
  PCredHandle = ^CredHandle;
  
  CtxtHandle = TSecHandle;
  PCtxtHandle = ^CtxtHandle;
  
  // ============================================================================
  // Security Buffer 定义
  // ============================================================================
  
  PSecBuffer = ^TSecBuffer;
  TSecBuffer = record
    cbBuffer: ULONG;      // 缓冲区大小
    BufferType: ULONG;    // 缓冲区类型
    pvBuffer: Pointer;    // 缓冲区指针
  end;
  
  PSecBufferDesc = ^TSecBufferDesc;
  TSecBufferDesc = record
    ulVersion: ULONG;     // 版本号
    cBuffers: ULONG;      // 缓冲区数量
    pBuffers: PSecBuffer; // 缓冲区数组指针
  end;
  
  // ============================================================================
  // Schannel Credential 定义
  // ============================================================================
  
  // 证书上下文指针（来自 Wincrypt.h）
  HCERTSTORE = Pointer;
  HCRYPTPROV = ULONG_PTR;
  
  PCCERT_CONTEXT = ^CERT_CONTEXT;
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;
  CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: Pointer;
    hCertStore: HCERTSTORE;
  end;
  
  // Algorithm ID 类型
  PALG_ID = ^ALG_ID;
  ALG_ID = DWORD;
  
  // Schannel 凭据结构
  PSCHANNEL_CRED = ^SCHANNEL_CRED;
  SCHANNEL_CRED = record
    dwVersion: DWORD;                    // 总是 SCHANNEL_CRED_VERSION
    cCreds: DWORD;                       // 证书数量
    paCred: PPCCERT_CONTEXT;             // 证书数组
    hRootStore: HCERTSTORE;              // 根证书存储（可选）
    
    cMappers: DWORD;                     // 凭据映射器数量
    aphMappers: Pointer;                 // 凭据映射器数组
    
    cSupportedAlgs: DWORD;               // 支持的算法数量
    palgSupportedAlgs: PALG_ID;          // 算法数组
    
    grbitEnabledProtocols: DWORD;        // 启用的协议标志
    dwMinimumCipherStrength: DWORD;      // 最小加密强度
    dwMaximumCipherStrength: DWORD;      // 最大加密强度
    dwSessionLifespan: DWORD;            // 会话生命周期
    dwFlags: DWORD;                      // 标志
    dwCredFormat: DWORD;                 // 凭据格式
  end;
  
  // ============================================================================
  // Security Context Attributes
  // ============================================================================
  
  PSecPkgContext_StreamSizes = ^TSecPkgContext_StreamSizes;
  TSecPkgContext_StreamSizes = record
    cbHeader: ULONG;      // 流头大小
    cbTrailer: ULONG;     // 流尾大小
    cbMaximumMessage: ULONG; // 最大消息大小
    cBuffers: ULONG;      // 缓冲区数量
    cbBlockSize: ULONG;   // 块大小
  end;
  
  PSecPkgContext_ConnectionInfo = ^TSecPkgContext_ConnectionInfo;
  TSecPkgContext_ConnectionInfo = record
    dwProtocol: DWORD;    // 协议版本
    aiCipher: ALG_ID;     // 加密算法
    dwCipherStrength: DWORD; // 加密强度
    aiHash: ALG_ID;       // 哈希算法
    dwHashStrength: DWORD;   // 哈希强度
    aiExch: ALG_ID;       // 密钥交换算法
    dwExchStrength: DWORD;   // 交换强度
  end;
  
  // 证书链相关
  HCERTCHAINENGINE = Pointer;
  
  PCERT_CHAIN_PARA = ^CERT_CHAIN_PARA;
  CERT_CHAIN_PARA = record
    cbSize: DWORD;
    RequestedUsage: record
      dwType: DWORD;
      Usage: record
        cUsageIdentifier: DWORD;
        rgpszUsageIdentifier: Pointer;
      end;
    end;
  end;
  
  PCCERT_CHAIN_CONTEXT = Pointer;
  PPCCERT_CHAIN_CONTEXT = ^PCCERT_CHAIN_CONTEXT;
  
  // 证书名称 Blob
  PCERT_NAME_BLOB = ^CERT_NAME_BLOB;
  CERT_NAME_BLOB = record
    cbData: DWORD;
    pbData: PByte;
  end;
  
  // Windows 证书函数类型
  TCertNameToStr = function(
    dwCertEncodingType: DWORD;
    pName: Pointer;
    dwStrType: DWORD;
    psz: PChar;
    csz: DWORD
  ): DWORD; stdcall;
  
  // 不带 T 前缀的类型别名（保持与旧代码兼容）
  SecHandle = TSecHandle;
  SecBuffer = TSecBuffer;
  SecBufferDesc = TSecBufferDesc;
  SecPkgContext_StreamSizes = TSecPkgContext_StreamSizes;
  SecPkgContext_ConnectionInfo = TSecPkgContext_ConnectionInfo;
  TimeStamp = TTimeStamp;

// ============================================================================
// 常量定义
// ============================================================================

const
  // Schannel 凭据版本
  SCHANNEL_CRED_VERSION = 4;
  
  // 凭据用途标志
  SECPKG_CRED_INBOUND  = $00000001;
  SECPKG_CRED_OUTBOUND = $00000002;
  SECPKG_CRED_BOTH     = SECPKG_CRED_INBOUND or SECPKG_CRED_OUTBOUND;
  
  // Security Buffer 版本
  SECBUFFER_VERSION = 0;
  
  // Security Buffer 类型
  SECBUFFER_EMPTY              = 0;
  SECBUFFER_DATA               = 1;
  SECBUFFER_TOKEN              = 2;
  SECBUFFER_PKG_PARAMS         = 3;
  SECBUFFER_MISSING            = 4;
  SECBUFFER_EXTRA              = 5;
  SECBUFFER_STREAM_TRAILER     = 6;
  SECBUFFER_STREAM_HEADER      = 7;
  SECBUFFER_NEGOTIATION_INFO   = 8;
  SECBUFFER_PADDING            = 9;
  SECBUFFER_STREAM             = 10;
  SECBUFFER_MECHLIST           = 11;
  SECBUFFER_MECHLIST_SIGNATURE = 12;
  SECBUFFER_TARGET             = 13;
  SECBUFFER_CHANNEL_BINDINGS   = 14;
  SECBUFFER_CHANGE_PASS_RESPONSE = 15;
  SECBUFFER_TARGET_HOST        = 16;
  SECBUFFER_ALERT              = 17;
  
  // TLS/SSL 协议版本标志
  SP_PROT_TLS1_0_SERVER        = $00000040;
  SP_PROT_TLS1_0_CLIENT        = $00000080;
  SP_PROT_TLS1_0               = SP_PROT_TLS1_0_SERVER or SP_PROT_TLS1_0_CLIENT;
  
  SP_PROT_TLS1_1_SERVER        = $00000100;
  SP_PROT_TLS1_1_CLIENT        = $00000200;
  SP_PROT_TLS1_1               = SP_PROT_TLS1_1_SERVER or SP_PROT_TLS1_1_CLIENT;
  
  SP_PROT_TLS1_2_SERVER        = $00000400;
  SP_PROT_TLS1_2_CLIENT        = $00000800;
  SP_PROT_TLS1_2               = SP_PROT_TLS1_2_SERVER or SP_PROT_TLS1_2_CLIENT;
  
  SP_PROT_TLS1_3_SERVER        = $00001000;
  SP_PROT_TLS1_3_CLIENT        = $00002000;
  SP_PROT_TLS1_3               = SP_PROT_TLS1_3_SERVER or SP_PROT_TLS1_3_CLIENT;
  
  SP_PROT_DTLS_SERVER          = $00010000;
  SP_PROT_DTLS_CLIENT          = $00020000;
  SP_PROT_DTLS                 = SP_PROT_DTLS_SERVER or SP_PROT_DTLS_CLIENT;
  
  SP_PROT_DTLS1_0_SERVER       = $00004000;
  SP_PROT_DTLS1_0_CLIENT       = $00008000;
  SP_PROT_DTLS1_0              = SP_PROT_DTLS1_0_SERVER or SP_PROT_DTLS1_0_CLIENT;
  
  SP_PROT_DTLS1_2_SERVER       = $00040000;
  SP_PROT_DTLS1_2_CLIENT       = $00080000;
  SP_PROT_DTLS1_2              = SP_PROT_DTLS1_2_SERVER or SP_PROT_DTLS1_2_CLIENT;
  
  // 已废弃的协议（不推荐使用）
  SP_PROT_SSL2_SERVER          = $00000004;
  SP_PROT_SSL2_CLIENT          = $00000008;
  SP_PROT_SSL2                 = SP_PROT_SSL2_SERVER or SP_PROT_SSL2_CLIENT;
  
  SP_PROT_SSL3_SERVER          = $00000010;
  SP_PROT_SSL3_CLIENT          = $00000020;
  SP_PROT_SSL3                 = SP_PROT_SSL3_SERVER or SP_PROT_SSL3_CLIENT;
  
  // Schannel 凭据标志
  SCH_CRED_NO_SYSTEM_MAPPER           = $00000002;
  SCH_CRED_NO_SERVERNAME_CHECK        = $00000004;
  SCH_CRED_MANUAL_CRED_VALIDATION     = $00000008;
  SCH_CRED_NO_DEFAULT_CREDS           = $00000010;
  SCH_CRED_AUTO_CRED_VALIDATION       = $00000020;
  SCH_CRED_USE_DEFAULT_CREDS          = $00000040;
  SCH_CRED_DISABLE_RECONNECTS         = $00000080;
  SCH_CRED_REVOCATION_CHECK_END_CERT  = $00000100;
  SCH_CRED_REVOCATION_CHECK_CHAIN     = $00000200;
  SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $00000400;
  SCH_CRED_IGNORE_NO_REVOCATION_CHECK = $00000800;
  SCH_CRED_IGNORE_REVOCATION_OFFLINE  = $00001000;
  SCH_CRED_RESTRICTED_ROOTS           = $00002000;
  SCH_CRED_CACHE_ONLY_URL_RETRIEVAL   = $00008000;
  SCH_CRED_MEMORY_STORE_CERT          = $00010000;
  
  // InitializeSecurityContext 请求标志
  ISC_REQ_DELEGATE                = $00000001;
  ISC_REQ_MUTUAL_AUTH             = $00000002;
  ISC_REQ_REPLAY_DETECT           = $00000004;
  ISC_REQ_SEQUENCE_DETECT         = $00000008;
  ISC_REQ_CONFIDENTIALITY         = $00000010;
  ISC_REQ_USE_SESSION_KEY         = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS        = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS      = $00000080;
  ISC_REQ_ALLOCATE_MEMORY         = $00000100;
  ISC_REQ_USE_DCE_STYLE           = $00000200;
  ISC_REQ_DATAGRAM                = $00000400;
  ISC_REQ_CONNECTION              = $00000800;
  ISC_REQ_CALL_LEVEL              = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  ISC_REQ_EXTENDED_ERROR          = $00004000;
  ISC_REQ_STREAM                  = $00008000;
  ISC_REQ_INTEGRITY               = $00010000;
  ISC_REQ_IDENTIFY                = $00020000;
  ISC_REQ_NULL_SESSION            = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION  = $00080000;
  ISC_REQ_RESERVED1               = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT         = $00200000;
  
  // AcceptSecurityContext 请求标志
  ASC_REQ_DELEGATE                = $00000001;
  ASC_REQ_MUTUAL_AUTH             = $00000002;
  ASC_REQ_REPLAY_DETECT           = $00000004;
  ASC_REQ_SEQUENCE_DETECT         = $00000008;
  ASC_REQ_CONFIDENTIALITY         = $00000010;
  ASC_REQ_USE_SESSION_KEY         = $00000020;
  ASC_REQ_ALLOCATE_MEMORY         = $00000100;
  ASC_REQ_USE_DCE_STYLE           = $00000200;
  ASC_REQ_DATAGRAM                = $00000400;
  ASC_REQ_CONNECTION              = $00000800;
  ASC_REQ_CALL_LEVEL              = $00001000;
  ASC_REQ_EXTENDED_ERROR          = $00008000;
  ASC_REQ_STREAM                  = $00010000;
  ASC_REQ_INTEGRITY               = $00020000;
  ASC_REQ_LICENSING               = $00040000;
  ASC_REQ_IDENTIFY                = $00080000;
  ASC_REQ_ALLOW_NULL_SESSION      = $00100000;
  ASC_REQ_ALLOW_NON_USER_LOGONS   = $00200000;
  ASC_REQ_ALLOW_CONTEXT_REPLAY    = $00400000;
  ASC_REQ_FRAGMENT_TO_FIT         = $00800000;
  ASC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  
  // Security Status 返回值
  SEC_E_OK                        = SECURITY_STATUS(0);
  SEC_I_CONTINUE_NEEDED          = SECURITY_STATUS($00090312);
  SEC_I_COMPLETE_NEEDED          = SECURITY_STATUS($00090313);
  SEC_I_COMPLETE_AND_CONTINUE    = SECURITY_STATUS($00090314);
  SEC_I_LOCAL_LOGON              = SECURITY_STATUS($00090315);
  SEC_I_CONTEXT_EXPIRED          = SECURITY_STATUS($00090317);
  SEC_I_INCOMPLETE_CREDENTIALS   = SECURITY_STATUS($00090320);
  SEC_I_RENEGOTIATE              = SECURITY_STATUS($00090321);
  SEC_I_NO_LSA_CONTEXT           = SECURITY_STATUS($00090323);
  
  // 错误码
  SEC_E_INSUFFICIENT_MEMORY      = SECURITY_STATUS($80090300);
  SEC_E_INVALID_HANDLE           = SECURITY_STATUS($80090301);
  SEC_E_UNSUPPORTED_FUNCTION     = SECURITY_STATUS($80090302);
  SEC_E_TARGET_UNKNOWN           = SECURITY_STATUS($80090303);
  SEC_E_INTERNAL_ERROR           = SECURITY_STATUS($80090304);
  SEC_E_SECPKG_NOT_FOUND         = SECURITY_STATUS($80090305);
  SEC_E_NOT_OWNER                = SECURITY_STATUS($80090306);
  SEC_E_CANNOT_INSTALL           = SECURITY_STATUS($80090307);
  SEC_E_INVALID_TOKEN            = SECURITY_STATUS($80090308);
  SEC_E_CANNOT_PACK              = SECURITY_STATUS($80090309);
  SEC_E_QOP_NOT_SUPPORTED        = SECURITY_STATUS($8009030A);
  SEC_E_NO_IMPERSONATION         = SECURITY_STATUS($8009030B);
  SEC_E_LOGON_DENIED             = SECURITY_STATUS($8009030C);
  SEC_E_UNKNOWN_CREDENTIALS      = SECURITY_STATUS($8009030D);
  SEC_E_NO_CREDENTIALS           = SECURITY_STATUS($8009030E);
  SEC_E_MESSAGE_ALTERED          = SECURITY_STATUS($8009030F);
  SEC_E_OUT_OF_SEQUENCE          = SECURITY_STATUS($80090310);
  SEC_E_NO_AUTHENTICATING_AUTHORITY = SECURITY_STATUS($80090311);
  SEC_E_INCOMPLETE_MESSAGE       = SECURITY_STATUS($80090318);
  SEC_E_INCOMPLETE_CREDENTIALS   = SECURITY_STATUS($80090320);
  SEC_E_BUFFER_TOO_SMALL         = SECURITY_STATUS($80090321);
  SEC_E_WRONG_PRINCIPAL          = SECURITY_STATUS($80090322);
  SEC_E_TIME_SKEW                = SECURITY_STATUS($80090324);
  SEC_E_UNTRUSTED_ROOT           = SECURITY_STATUS($80090325);
  SEC_E_ILLEGAL_MESSAGE          = SECURITY_STATUS($80090326);
  SEC_E_CERT_UNKNOWN             = SECURITY_STATUS($80090327);
  SEC_E_CERT_EXPIRED             = SECURITY_STATUS($80090328);
  SEC_E_ENCRYPT_FAILURE          = SECURITY_STATUS($80090329);
  SEC_E_DECRYPT_FAILURE          = SECURITY_STATUS($80090330);
  SEC_E_ALGORITHM_MISMATCH       = SECURITY_STATUS($80090331);
  SEC_E_SECURITY_QOS_FAILED      = SECURITY_STATUS($80090332);
  SEC_E_UNFINISHED_CONTEXT_DELETED = SECURITY_STATUS($80090333);
  
  // QueryContextAttributes 属性
  SECPKG_ATTR_SIZES              = 0;
  SECPKG_ATTR_NAMES              = 1;
  SECPKG_ATTR_LIFESPAN           = 2;
  SECPKG_ATTR_DCE_INFO           = 3;
  SECPKG_ATTR_STREAM_SIZES       = 4;
  SECPKG_ATTR_KEY_INFO           = 5;
  SECPKG_ATTR_AUTHORITY          = 6;
  SECPKG_ATTR_PROTO_INFO         = 7;
  SECPKG_ATTR_PASSWORD_EXPIRY    = 8;
  SECPKG_ATTR_SESSION_KEY        = 9;
  SECPKG_ATTR_PACKAGE_INFO       = 10;
  SECPKG_ATTR_USER_FLAGS         = 11;
  SECPKG_ATTR_NEGOTIATION_INFO   = 12;
  SECPKG_ATTR_NATIVE_NAMES       = 13;
  SECPKG_ATTR_FLAGS              = 14;
  SECPKG_ATTR_USE_VALIDATED      = 15;
  SECPKG_ATTR_CREDENTIAL_NAME    = 16;
  SECPKG_ATTR_TARGET_INFORMATION = 17;
  SECPKG_ATTR_ACCESS_TOKEN       = 18;
  SECPKG_ATTR_TARGET             = 19;
  SECPKG_ATTR_AUTHENTICATION_ID  = 20;
  SECPKG_ATTR_LOGOFF_TIME        = 21;
  SECPKG_ATTR_NEGO_KEYS          = 22;
  SECPKG_ATTR_PROMPTING_NEEDED   = 24;
  SECPKG_ATTR_UNIQUE_BINDINGS    = 25;
  SECPKG_ATTR_ENDPOINT_BINDINGS  = 26;
  SECPKG_ATTR_CLIENT_SPECIFIED_TARGET = 27;
  SECPKG_ATTR_LAST_CLIENT_TOKEN_STATUS = 30;
  SECPKG_ATTR_NEGO_PKG_INFO      = 31;
  SECPKG_ATTR_NEGO_STATUS        = 32;
  SECPKG_ATTR_CONTEXT_DELETED    = 33;
  SECPKG_ATTR_DTLS_MTU           = 34;
  SECPKG_ATTR_DATAGRAM_SIZES     = SECPKG_ATTR_STREAM_SIZES;
  SECPKG_ATTR_SUBJECT_SECURITY_ATTRIBUTES = 128;
  SECPKG_ATTR_APPLICATION_PROTOCOL = 35;
  SECPKG_ATTR_NEGOTIATED_TLS_EXTENSIONS = 36;
  SECPKG_ATTR_IS_LOOPBACK        = 37;
  SECPKG_ATTR_REMOTE_CERT_CONTEXT = 83;
  
  // 证书编码类型
  X509_ASN_ENCODING              = $00000001;
  PKCS_7_ASN_ENCODING            = $00010000;
  
  // 证书存储标志
  CERT_STORE_PROV_SYSTEM         = LPCSTR(10);
  CERT_SYSTEM_STORE_CURRENT_USER = $00010000;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;
  
  // 证书查找类型
  CERT_FIND_SUBJECT_STR          = $00080007;
  CERT_FIND_ISSUER_STR           = $00080004;
  CERT_FIND_ANY                  = 0;
  
  // 数据表示
  SECURITY_NATIVE_DREP           = $00000010;
  SECURITY_NETWORK_DREP          = $00000000;
  
  // WinSock 常量
  INVALID_HANDLE_VALUE           = THandle(-1);
  SOCKET_ERROR                   = -1;
  FIONREAD                       = $4004667F;
  WSAEWOULDBLOCK                 = 10035;
  
  // 证书验证错误
  CERT_E_EXPIRED                 = LONG($800B0101);
  CERT_E_UNTRUSTEDROOT           = LONG($800B0109);
  CERT_E_WRONG_USAGE             = LONG($800B0110);
  CERT_E_CN_NO_MATCH             = LONG($800B010F);
  CERT_E_REVOKED                 = LONG($800B010C);
  
  // 证书名称类型
  CERT_NAME_EMAIL_TYPE           = 1;
  CERT_NAME_RDN_TYPE             = 2;
  CERT_NAME_ATTR_TYPE            = 3;
  CERT_NAME_SIMPLE_DISPLAY_TYPE  = 4;
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  CERT_NAME_DNS_TYPE             = 6;
  CERT_NAME_URL_TYPE             = 7;
  CERT_NAME_UPN_TYPE             = 8;
  
  // 证书名称标志
  CERT_NAME_ISSUER_FLAG          = $1;
  CERT_NAME_STR_COMMA_FLAG       = $04000000;
  CERT_NAME_STR_SEMICOLON_FLAG   = $40000000;
  CERT_NAME_STR_CRLF_FLAG        = $08000000;
  
  // 证书获取名称字符串标志
  CERT_SIMPLE_NAME_STR           = 1;
  CERT_OID_NAME_STR              = 2;
  CERT_X500_NAME_STR             = 3;
  
  // Security Package Name
  UNISP_NAME                     = 'Microsoft Unified Security Protocol Provider';
  UNISP_NAME_W                   = WideString('Microsoft Unified Security Protocol Provider');

// ============================================================================
// 辅助函数声明
// ============================================================================

// 检查 Security Status 是否成功
function SUCCEEDED(Status: SECURITY_STATUS): Boolean; inline;
function FAILED(Status: SECURITY_STATUS): Boolean; inline;

implementation

function SUCCEEDED(Status: SECURITY_STATUS): Boolean;
begin
  Result := Status >= 0;
end;

function FAILED(Status: SECURITY_STATUS): Boolean;
begin
  Result := Status < 0;
end;

end.
