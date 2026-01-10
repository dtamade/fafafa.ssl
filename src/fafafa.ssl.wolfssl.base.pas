{**
 * Unit: fafafa.ssl.wolfssl.base
 * Purpose: WolfSSL 后端基础类型和常量定义
 *
 * P2-7: WolfSSL 后端框架 - 最小子集实现
 *
 * WolfSSL 特点：
 * - OpenSSL 兼容 API（降低迁移成本）
 * - 轻量级，适合嵌入式
 * - 商业许可证（需注意）
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-09
 *}

unit fafafa.ssl.wolfssl.base;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base;

const
  { WolfSSL 库文件名 }
  {$IFDEF WINDOWS}
  WOLFSSL_LIB_NAME = 'wolfssl.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  WOLFSSL_LIB_NAME = 'libwolfssl.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  WOLFSSL_LIB_NAME = 'libwolfssl.dylib';
  {$ENDIF}

  { WolfSSL 版本要求 }
  WOLFSSL_MIN_VERSION = $05000000;  // 5.0.0 minimum

  { WolfSSL 方法常量 }
  WOLFSSL_CLIENT_METHOD = 1;
  WOLFSSL_SERVER_METHOD = 2;

  { WolfSSL 验证模式 }
  WOLFSSL_VERIFY_NONE = 0;
  WOLFSSL_VERIFY_PEER = 1;
  WOLFSSL_VERIFY_FAIL_IF_NO_PEER_CERT = 2;

  { WolfSSL 错误码 }
  WOLFSSL_SUCCESS = 1;
  WOLFSSL_FAILURE = 0;
  WOLFSSL_ERROR_NONE = 0;
  WOLFSSL_ERROR_WANT_READ = -2;
  WOLFSSL_ERROR_WANT_WRITE = -3;
  WOLFSSL_ERROR_SYSCALL = -5;
  WOLFSSL_ERROR_SSL = -85;

  { WolfSSL 文件格式 }
  WOLFSSL_FILETYPE_PEM = 1;
  WOLFSSL_FILETYPE_ASN1 = 2;
  WOLFSSL_FILETYPE_DEFAULT = 2;

type
  { WolfSSL 不透明指针类型 }
  PWOLFSSL_CTX = Pointer;
  PWOLFSSL = Pointer;
  PWOLFSSL_METHOD = Pointer;
  PWOLFSSL_X509 = Pointer;
  PPWOLFSSL_X509 = ^PWOLFSSL_X509;
  PWOLFSSL_X509_STORE = Pointer;
  PWOLFSSL_SESSION = Pointer;
  PPAnsiChar = ^PAnsiChar;

  { WolfSSL 后端状态 }
  TWolfSSLBackendState = (
    wssNotLoaded,      // 库未加载
    wssLoaded,         // 库已加载
    wssInitialized,    // 已初始化
    wssError           // 错误状态
  );

  { WolfSSL 能力检测结果 }
  TWolfSSLCapabilities = record
    HasTLS13: Boolean;
    HasALPN: Boolean;
    HasSNI: Boolean;
    HasOCSP: Boolean;
    HasSessionTickets: Boolean;
    HasECDHE: Boolean;
    HasChaCha20: Boolean;
    VersionString: string;
    VersionNumber: Cardinal;
  end;

{ 工具函数 }
function WolfSSLErrorToSSLError(AWolfSSLError: Integer): TSSLErrorCode;
function SSLProtocolToWolfSSL(AProtocol: TSSLProtocolVersion): Integer;
function WolfSSLProtocolToSSL(AWolfSSLProtocol: Integer): TSSLProtocolVersion;

implementation

function WolfSSLErrorToSSLError(AWolfSSLError: Integer): TSSLErrorCode;
begin
  case AWolfSSLError of
    WOLFSSL_ERROR_NONE: Result := sslErrNone;
    WOLFSSL_ERROR_WANT_READ: Result := sslErrWantRead;
    WOLFSSL_ERROR_WANT_WRITE: Result := sslErrWantWrite;
    WOLFSSL_ERROR_SYSCALL: Result := sslErrIO;        // 系统调用错误映射到 I/O 错误
    WOLFSSL_ERROR_SSL: Result := sslErrProtocol;      // SSL 错误映射到协议错误
  else
    Result := sslErrGeneral;
  end;
end;

function SSLProtocolToWolfSSL(AProtocol: TSSLProtocolVersion): Integer;
begin
  // WolfSSL 使用位掩码表示协议版本
  case AProtocol of
    sslProtocolTLS10: Result := $0301;
    sslProtocolTLS11: Result := $0302;
    sslProtocolTLS12: Result := $0303;
    sslProtocolTLS13: Result := $0304;
  else
    Result := 0;
  end;
end;

function WolfSSLProtocolToSSL(AWolfSSLProtocol: Integer): TSSLProtocolVersion;
begin
  case AWolfSSLProtocol of
    $0301: Result := sslProtocolTLS10;
    $0302: Result := sslProtocolTLS11;
    $0303: Result := sslProtocolTLS12;
    $0304: Result := sslProtocolTLS13;
  else
    Result := sslProtocolTLS12;  // 默认
  end;
end;

end.
