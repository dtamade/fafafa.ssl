{**
 * Unit: fafafa.ssl.wolfssl.api
 * Purpose: WolfSSL API 动态绑定
 *
 * P2-7: WolfSSL 后端框架 - API 绑定层
 *
 * 实现策略：
 * - 动态加载 WolfSSL 库
 * - 仅绑定 TLS 主链路所需的最小 API 子集
 * - 其他功能通过能力矩阵标记为不支持
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-09
 *}

unit fafafa.ssl.wolfssl.api;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, dynlibs,
  fafafa.ssl.wolfssl.base;

type
  { WolfSSL 函数类型定义 }

  // 初始化/清理
  Twolfssl_init = function: Integer; cdecl;
  Twolfssl_cleanup = function: Integer; cdecl;

  // 方法创建
  TwolfTLSv1_2_client_method = function: PWOLFSSL_METHOD; cdecl;
  TwolfTLSv1_2_server_method = function: PWOLFSSL_METHOD; cdecl;
  TwolfTLSv1_3_client_method = function: PWOLFSSL_METHOD; cdecl;
  TwolfTLSv1_3_server_method = function: PWOLFSSL_METHOD; cdecl;
  TwolfSSLv23_client_method = function: PWOLFSSL_METHOD; cdecl;
  TwolfSSLv23_server_method = function: PWOLFSSL_METHOD; cdecl;

  // 上下文管理
  TwolfSSL_CTX_new = function(method: PWOLFSSL_METHOD): PWOLFSSL_CTX; cdecl;
  TwolfSSL_CTX_free = procedure(ctx: PWOLFSSL_CTX); cdecl;

  // SSL 对象管理
  TwolfSSL_new = function(ctx: PWOLFSSL_CTX): PWOLFSSL; cdecl;
  TwolfSSL_free = procedure(ssl: PWOLFSSL); cdecl;

  // 连接管理
  TwolfSSL_set_fd = function(ssl: PWOLFSSL; fd: Integer): Integer; cdecl;
  TwolfSSL_connect = function(ssl: PWOLFSSL): Integer; cdecl;
  TwolfSSL_accept = function(ssl: PWOLFSSL): Integer; cdecl;
  TwolfSSL_shutdown = function(ssl: PWOLFSSL): Integer; cdecl;

  // 数据传输
  TwolfSSL_read = function(ssl: PWOLFSSL; buf: Pointer; sz: Integer): Integer; cdecl;
  TwolfSSL_write = function(ssl: PWOLFSSL; buf: Pointer; sz: Integer): Integer; cdecl;

  // 错误处理
  TwolfSSL_get_error = function(ssl: PWOLFSSL; ret: Integer): Integer; cdecl;
  TwolfSSL_ERR_error_string = function(err: Cardinal; buf: PAnsiChar): PAnsiChar; cdecl;

  // 证书加载
  TwolfSSL_CTX_use_certificate_file = function(ctx: PWOLFSSL_CTX;
    const filename: PAnsiChar; format: Integer): Integer; cdecl;
  TwolfSSL_CTX_use_PrivateKey_file = function(ctx: PWOLFSSL_CTX;
    const filename: PAnsiChar; format: Integer): Integer; cdecl;
  TwolfSSL_CTX_load_verify_locations = function(ctx: PWOLFSSL_CTX;
    const CAfile: PAnsiChar; const CApath: PAnsiChar): Integer; cdecl;

  // 验证模式
  TwolfSSL_CTX_set_verify = procedure(ctx: PWOLFSSL_CTX; mode: Integer;
    callback: Pointer); cdecl;

  // SNI 支持
  TwolfSSL_UseSNI = function(ssl: PWOLFSSL; sniType: Byte;
    const data: Pointer; size: Word): Integer; cdecl;

  // 版本信息
  TwolfSSL_lib_version = function: PAnsiChar; cdecl;

  // SAN 解析（返回 SAN 条目值；调用多次返回下一个，直到 nil）
  TwolfSSL_X509_get_next_altname = function(cert: PWOLFSSL_X509): PAnsiChar; cdecl;

  // X509 证书操作
  TwolfSSL_X509_load_certificate_file = function(const filename: PAnsiChar;
    format: Integer): PWOLFSSL_X509; cdecl;
  TwolfSSL_X509_free = procedure(x509: PWOLFSSL_X509); cdecl;
  TwolfSSL_X509_d2i = function(x509: PPWOLFSSL_X509; const data: Pointer;
    len: Integer): PWOLFSSL_X509; cdecl;
  TwolfSSL_X509_get_subject_name = function(x509: PWOLFSSL_X509): Pointer; cdecl;
  TwolfSSL_X509_get_issuer_name = function(x509: PWOLFSSL_X509): Pointer; cdecl;

  // X509 Store 操作
  TwolfSSL_X509_STORE_new = function: PWOLFSSL_X509_STORE; cdecl;
  TwolfSSL_X509_STORE_free = procedure(store: PWOLFSSL_X509_STORE); cdecl;
  TwolfSSL_X509_STORE_add_cert = function(store: PWOLFSSL_X509_STORE;
    x509: PWOLFSSL_X509): Integer; cdecl;

  // X509 证书信息提取 (新增)
  TwolfSSL_X509_NAME_oneline = function(name: Pointer; buf: PAnsiChar;
    size: Integer): PAnsiChar; cdecl;
  TwolfSSL_X509_get_serial_number = function(x509: PWOLFSSL_X509): Pointer; cdecl;
  TwolfSSL_X509_get_notBefore = function(x509: PWOLFSSL_X509): Pointer; cdecl;
  TwolfSSL_X509_get_notAfter = function(x509: PWOLFSSL_X509): Pointer; cdecl;
  TwolfSSL_X509_get_pubkey = function(x509: PWOLFSSL_X509): Pointer; cdecl;
  TwolfSSL_X509_get_version = function(x509: PWOLFSSL_X509): Integer; cdecl;

  // ASN1 时间转换
  TwolfSSL_ASN1_TIME_to_tm = function(s: Pointer; tm: Pointer): Integer; cdecl;

  // 会话管理 (新增)
  TwolfSSL_get_session = function(ssl: PWOLFSSL): PWOLFSSL_SESSION; cdecl;
  TwolfSSL_set_session = function(ssl: PWOLFSSL; session: PWOLFSSL_SESSION): Integer; cdecl;
  TwolfSSL_SESSION_free = procedure(session: PWOLFSSL_SESSION); cdecl;
  TwolfSSL_session_reused = function(ssl: PWOLFSSL): Integer; cdecl;

  // 会话序列化 (新增)
  TwolfSSL_i2d_SSL_SESSION = function(session: PWOLFSSL_SESSION; pp: PPByte): Integer; cdecl;
  TwolfSSL_d2i_SSL_SESSION = function(session: PPWOLFSSL_SESSION; const pp: PPByte; length: Integer): PWOLFSSL_SESSION; cdecl;

  // ALPN 支持 (新增)
  TwolfSSL_UseALPN = function(ssl: PWOLFSSL; const protocol_name_list: PAnsiChar;
    protocol_name_listSz: Cardinal; options: Integer): Integer; cdecl;
  TwolfSSL_ALPN_GetProtocol = function(ssl: PWOLFSSL; protocol_name: PPAnsiChar;
    size: PWord): Integer; cdecl;

  // 连接信息 (新增)
  TwolfSSL_get_current_cipher = function(ssl: PWOLFSSL): Pointer; cdecl;
  TwolfSSL_CIPHER_get_name = function(cipher: Pointer): PAnsiChar; cdecl;
  TwolfSSL_get_version = function(ssl: PWOLFSSL): PAnsiChar; cdecl;
  TwolfSSL_get_peer_certificate = function(ssl: PWOLFSSL): PWOLFSSL_X509; cdecl;

var
  { WolfSSL 函数指针 }
  wolfssl_init: Twolfssl_init = nil;
  wolfssl_cleanup: Twolfssl_cleanup = nil;

  wolfTLSv1_2_client_method: TwolfTLSv1_2_client_method = nil;
  wolfTLSv1_2_server_method: TwolfTLSv1_2_server_method = nil;
  wolfTLSv1_3_client_method: TwolfTLSv1_3_client_method = nil;
  wolfTLSv1_3_server_method: TwolfTLSv1_3_server_method = nil;
  wolfSSLv23_client_method: TwolfSSLv23_client_method = nil;
  wolfSSLv23_server_method: TwolfSSLv23_server_method = nil;

  wolfSSL_CTX_new: TwolfSSL_CTX_new = nil;
  wolfSSL_CTX_free: TwolfSSL_CTX_free = nil;

  wolfSSL_new: TwolfSSL_new = nil;
  wolfSSL_free: TwolfSSL_free = nil;

  wolfSSL_set_fd: TwolfSSL_set_fd = nil;
  wolfSSL_connect: TwolfSSL_connect = nil;
  wolfSSL_accept: TwolfSSL_accept = nil;
  wolfSSL_shutdown: TwolfSSL_shutdown = nil;

  wolfSSL_read: TwolfSSL_read = nil;
  wolfSSL_write: TwolfSSL_write = nil;

  wolfSSL_get_error: TwolfSSL_get_error = nil;
  wolfSSL_ERR_error_string: TwolfSSL_ERR_error_string = nil;

  wolfSSL_CTX_use_certificate_file: TwolfSSL_CTX_use_certificate_file = nil;
  wolfSSL_CTX_use_PrivateKey_file: TwolfSSL_CTX_use_PrivateKey_file = nil;
  wolfSSL_CTX_load_verify_locations: TwolfSSL_CTX_load_verify_locations = nil;

  wolfSSL_CTX_set_verify: TwolfSSL_CTX_set_verify = nil;
  wolfSSL_UseSNI: TwolfSSL_UseSNI = nil;
  wolfSSL_lib_version: TwolfSSL_lib_version = nil;

  // X509 函数
  wolfSSL_X509_load_certificate_file: TwolfSSL_X509_load_certificate_file = nil;
  wolfSSL_X509_get_next_altname: TwolfSSL_X509_get_next_altname = nil;
  wolfSSL_X509_free: TwolfSSL_X509_free = nil;
  wolfSSL_X509_d2i: TwolfSSL_X509_d2i = nil;
  wolfSSL_X509_get_subject_name: TwolfSSL_X509_get_subject_name = nil;
  wolfSSL_X509_get_issuer_name: TwolfSSL_X509_get_issuer_name = nil;
  wolfSSL_X509_STORE_new: TwolfSSL_X509_STORE_new = nil;
  wolfSSL_X509_STORE_free: TwolfSSL_X509_STORE_free = nil;
  wolfSSL_X509_STORE_add_cert: TwolfSSL_X509_STORE_add_cert = nil;

  // X509 证书信息提取 (新增)
  wolfSSL_X509_NAME_oneline: TwolfSSL_X509_NAME_oneline = nil;
  wolfSSL_X509_get_serial_number: TwolfSSL_X509_get_serial_number = nil;
  wolfSSL_X509_get_notBefore: TwolfSSL_X509_get_notBefore = nil;
  wolfSSL_X509_get_notAfter: TwolfSSL_X509_get_notAfter = nil;
  wolfSSL_X509_get_pubkey: TwolfSSL_X509_get_pubkey = nil;
  wolfSSL_X509_get_version: TwolfSSL_X509_get_version = nil;
  wolfSSL_ASN1_TIME_to_tm: TwolfSSL_ASN1_TIME_to_tm = nil;

  // 会话管理 (新增)
  wolfSSL_get_session: TwolfSSL_get_session = nil;
  wolfSSL_set_session: TwolfSSL_set_session = nil;
  wolfSSL_SESSION_free: TwolfSSL_SESSION_free = nil;
  wolfSSL_session_reused: TwolfSSL_session_reused = nil;

  // 会话序列化 (新增)
  wolfSSL_i2d_SSL_SESSION: TwolfSSL_i2d_SSL_SESSION = nil;
  wolfSSL_d2i_SSL_SESSION: TwolfSSL_d2i_SSL_SESSION = nil;

  // ALPN 支持 (新增)
  wolfSSL_UseALPN: TwolfSSL_UseALPN = nil;
  wolfSSL_ALPN_GetProtocol: TwolfSSL_ALPN_GetProtocol = nil;

  // 连接信息 (新增)
  wolfSSL_get_current_cipher: TwolfSSL_get_current_cipher = nil;
  wolfSSL_CIPHER_get_name: TwolfSSL_CIPHER_get_name = nil;
  wolfSSL_get_version: TwolfSSL_get_version = nil;
  wolfSSL_get_peer_certificate: TwolfSSL_get_peer_certificate = nil;

{ 库加载函数 }
function LoadWolfSSLLibrary: Boolean;
procedure UnloadWolfSSLLibrary;
function IsWolfSSLLoaded: Boolean;
function GetWolfSSLLibraryHandle: TLibHandle;

implementation

var
  GWolfSSLHandle: TLibHandle = NilHandle;
  GWolfSSLLoaded: Boolean = False;

function GetProc(const AProcName: string): Pointer;
begin
  Result := GetProcAddress(GWolfSSLHandle, AProcName);
end;

function LoadWolfSSLLibrary: Boolean;
begin
  Result := False;

  if GWolfSSLLoaded then
    Exit(True);

  GWolfSSLHandle := LoadLibrary(WOLFSSL_LIB_NAME);
  if GWolfSSLHandle = NilHandle then
    Exit(False);

  // 加载核心函数
  wolfssl_init := Twolfssl_init(GetProc('wolfSSL_Init'));
  wolfssl_cleanup := Twolfssl_cleanup(GetProc('wolfSSL_Cleanup'));

  // 方法函数
  wolfTLSv1_2_client_method := TwolfTLSv1_2_client_method(GetProc('wolfTLSv1_2_client_method'));
  wolfTLSv1_2_server_method := TwolfTLSv1_2_server_method(GetProc('wolfTLSv1_2_server_method'));
  wolfTLSv1_3_client_method := TwolfTLSv1_3_client_method(GetProc('wolfTLSv1_3_client_method'));
  wolfTLSv1_3_server_method := TwolfTLSv1_3_server_method(GetProc('wolfTLSv1_3_server_method'));
  wolfSSLv23_client_method := TwolfSSLv23_client_method(GetProc('wolfSSLv23_client_method'));
  wolfSSLv23_server_method := TwolfSSLv23_server_method(GetProc('wolfSSLv23_server_method'));

  // 上下文函数
  wolfSSL_CTX_new := TwolfSSL_CTX_new(GetProc('wolfSSL_CTX_new'));
  wolfSSL_CTX_free := TwolfSSL_CTX_free(GetProc('wolfSSL_CTX_free'));

  // SSL 对象函数
  wolfSSL_new := TwolfSSL_new(GetProc('wolfSSL_new'));
  wolfSSL_free := TwolfSSL_free(GetProc('wolfSSL_free'));

  // 连接函数
  wolfSSL_set_fd := TwolfSSL_set_fd(GetProc('wolfSSL_set_fd'));
  wolfSSL_connect := TwolfSSL_connect(GetProc('wolfSSL_connect'));
  wolfSSL_accept := TwolfSSL_accept(GetProc('wolfSSL_accept'));
  wolfSSL_shutdown := TwolfSSL_shutdown(GetProc('wolfSSL_shutdown'));

  // I/O 函数
  wolfSSL_read := TwolfSSL_read(GetProc('wolfSSL_read'));
  wolfSSL_write := TwolfSSL_write(GetProc('wolfSSL_write'));

  // 错误函数
  wolfSSL_get_error := TwolfSSL_get_error(GetProc('wolfSSL_get_error'));
  wolfSSL_ERR_error_string := TwolfSSL_ERR_error_string(GetProc('wolfSSL_ERR_error_string'));

  // 证书函数
  wolfSSL_CTX_use_certificate_file := TwolfSSL_CTX_use_certificate_file(
    GetProc('wolfSSL_CTX_use_certificate_file'));
  wolfSSL_CTX_use_PrivateKey_file := TwolfSSL_CTX_use_PrivateKey_file(
    GetProc('wolfSSL_CTX_use_PrivateKey_file'));
  wolfSSL_CTX_load_verify_locations := TwolfSSL_CTX_load_verify_locations(
    GetProc('wolfSSL_CTX_load_verify_locations'));

  // 验证函数
  wolfSSL_CTX_set_verify := TwolfSSL_CTX_set_verify(GetProc('wolfSSL_CTX_set_verify'));

  // SNI 函数
  wolfSSL_UseSNI := TwolfSSL_UseSNI(GetProc('wolfSSL_UseSNI'));

  // 版本函数
  wolfSSL_lib_version := TwolfSSL_lib_version(GetProc('wolfSSL_lib_version'));

  // X509 函数
  wolfSSL_X509_load_certificate_file := TwolfSSL_X509_load_certificate_file(
    GetProc('wolfSSL_X509_load_certificate_file'));
  wolfSSL_X509_get_next_altname := TwolfSSL_X509_get_next_altname(
    GetProc('wolfSSL_X509_get_next_altname'));
  wolfSSL_X509_free := TwolfSSL_X509_free(GetProc('wolfSSL_X509_free'));
  wolfSSL_X509_d2i := TwolfSSL_X509_d2i(GetProc('wolfSSL_d2i_X509'));
  wolfSSL_X509_get_subject_name := TwolfSSL_X509_get_subject_name(
    GetProc('wolfSSL_X509_get_subject_name'));
  wolfSSL_X509_get_issuer_name := TwolfSSL_X509_get_issuer_name(
    GetProc('wolfSSL_X509_get_issuer_name'));
  wolfSSL_X509_STORE_new := TwolfSSL_X509_STORE_new(GetProc('wolfSSL_X509_STORE_new'));
  wolfSSL_X509_STORE_free := TwolfSSL_X509_STORE_free(GetProc('wolfSSL_X509_STORE_free'));
  wolfSSL_X509_STORE_add_cert := TwolfSSL_X509_STORE_add_cert(
    GetProc('wolfSSL_X509_STORE_add_cert'));

  // X509 证书信息提取 (新增)
  wolfSSL_X509_NAME_oneline := TwolfSSL_X509_NAME_oneline(
    GetProc('wolfSSL_X509_NAME_oneline'));
  wolfSSL_X509_get_serial_number := TwolfSSL_X509_get_serial_number(
    GetProc('wolfSSL_X509_get_serial_number'));
  wolfSSL_X509_get_notBefore := TwolfSSL_X509_get_notBefore(
    GetProc('wolfSSL_X509_notBefore'));
  wolfSSL_X509_get_notAfter := TwolfSSL_X509_get_notAfter(
    GetProc('wolfSSL_X509_notAfter'));
  wolfSSL_X509_get_pubkey := TwolfSSL_X509_get_pubkey(
    GetProc('wolfSSL_X509_get_pubkey'));
  wolfSSL_X509_get_version := TwolfSSL_X509_get_version(
    GetProc('wolfSSL_X509_get_version'));
  wolfSSL_ASN1_TIME_to_tm := TwolfSSL_ASN1_TIME_to_tm(
    GetProc('wolfSSL_ASN1_TIME_to_tm'));

  // 会话管理 (新增)
  wolfSSL_get_session := TwolfSSL_get_session(GetProc('wolfSSL_get_session'));
  wolfSSL_set_session := TwolfSSL_set_session(GetProc('wolfSSL_set_session'));
  wolfSSL_SESSION_free := TwolfSSL_SESSION_free(GetProc('wolfSSL_SESSION_free'));
  wolfSSL_session_reused := TwolfSSL_session_reused(GetProc('wolfSSL_session_reused'));

  // 会话序列化 (新增)
  wolfSSL_i2d_SSL_SESSION := TwolfSSL_i2d_SSL_SESSION(GetProc('wolfSSL_i2d_SSL_SESSION'));
  wolfSSL_d2i_SSL_SESSION := TwolfSSL_d2i_SSL_SESSION(GetProc('wolfSSL_d2i_SSL_SESSION'));

  // ALPN 支持 (新增)
  wolfSSL_UseALPN := TwolfSSL_UseALPN(GetProc('wolfSSL_UseALPN'));
  wolfSSL_ALPN_GetProtocol := TwolfSSL_ALPN_GetProtocol(GetProc('wolfSSL_ALPN_GetProtocol'));

  // 连接信息 (新增)
  wolfSSL_get_current_cipher := TwolfSSL_get_current_cipher(
    GetProc('wolfSSL_get_current_cipher'));
  wolfSSL_CIPHER_get_name := TwolfSSL_CIPHER_get_name(
    GetProc('wolfSSL_CIPHER_get_name'));
  wolfSSL_get_version := TwolfSSL_get_version(GetProc('wolfSSL_get_version'));
  wolfSSL_get_peer_certificate := TwolfSSL_get_peer_certificate(
    GetProc('wolfSSL_get_peer_certificate'));

  // 验证必需函数
  if not Assigned(wolfssl_init) or
    not Assigned(wolfSSL_CTX_new) or
    not Assigned(wolfSSL_new) then
  begin
    UnloadWolfSSLLibrary;
    Exit(False);
  end;

  GWolfSSLLoaded := True;
  Result := True;
end;

procedure UnloadWolfSSLLibrary;
begin
  if GWolfSSLHandle <> NilHandle then
  begin
    FreeLibrary(GWolfSSLHandle);
    GWolfSSLHandle := NilHandle;
  end;

  // 清空所有函数指针
  wolfssl_init := nil;
  wolfssl_cleanup := nil;
  wolfTLSv1_2_client_method := nil;
  wolfTLSv1_2_server_method := nil;
  wolfTLSv1_3_client_method := nil;
  wolfTLSv1_3_server_method := nil;
  wolfSSLv23_client_method := nil;
  wolfSSLv23_server_method := nil;
  wolfSSL_CTX_new := nil;
  wolfSSL_CTX_free := nil;
  wolfSSL_new := nil;
  wolfSSL_free := nil;
  wolfSSL_set_fd := nil;
  wolfSSL_connect := nil;
  wolfSSL_accept := nil;
  wolfSSL_shutdown := nil;
  wolfSSL_read := nil;
  wolfSSL_write := nil;
  wolfSSL_get_error := nil;
  wolfSSL_ERR_error_string := nil;
  wolfSSL_CTX_use_certificate_file := nil;
  wolfSSL_CTX_use_PrivateKey_file := nil;
  wolfSSL_CTX_load_verify_locations := nil;
  wolfSSL_CTX_set_verify := nil;
  wolfSSL_UseSNI := nil;
  wolfSSL_lib_version := nil;

  // X509 函数
  wolfSSL_X509_load_certificate_file := nil;
  wolfSSL_X509_get_next_altname := nil;
  wolfSSL_X509_free := nil;
  wolfSSL_X509_d2i := nil;
  wolfSSL_X509_get_subject_name := nil;
  wolfSSL_X509_get_issuer_name := nil;
  wolfSSL_X509_STORE_new := nil;
  wolfSSL_X509_STORE_free := nil;
  wolfSSL_X509_STORE_add_cert := nil;

  // X509 证书信息提取
  wolfSSL_X509_NAME_oneline := nil;
  wolfSSL_X509_get_serial_number := nil;
  wolfSSL_X509_get_notBefore := nil;
  wolfSSL_X509_get_notAfter := nil;
  wolfSSL_X509_get_pubkey := nil;
  wolfSSL_X509_get_version := nil;
  wolfSSL_ASN1_TIME_to_tm := nil;

  // 会话管理
  wolfSSL_get_session := nil;
  wolfSSL_set_session := nil;
  wolfSSL_SESSION_free := nil;
  wolfSSL_session_reused := nil;

  // 会话序列化
  wolfSSL_i2d_SSL_SESSION := nil;
  wolfSSL_d2i_SSL_SESSION := nil;

  // ALPN 支持
  wolfSSL_UseALPN := nil;
  wolfSSL_ALPN_GetProtocol := nil;

  // 连接信息
  wolfSSL_get_current_cipher := nil;
  wolfSSL_CIPHER_get_name := nil;
  wolfSSL_get_version := nil;
  wolfSSL_get_peer_certificate := nil;

  GWolfSSLLoaded := False;
end;

function IsWolfSSLLoaded: Boolean;
begin
  Result := GWolfSSLLoaded;
end;

function GetWolfSSLLibraryHandle: TLibHandle;
begin
  Result := GWolfSSLHandle;
end;

end.
