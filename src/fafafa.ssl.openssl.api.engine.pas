unit fafafa.ssl.openssl.api.engine;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, dynlibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

type
  // Engine 类型定义
  ENGINE = Pointer;
  PENGINE = ^ENGINE;
  PPENGINE = ^PENGINE;
  ENGINE_CMD_DEFN = Pointer;
  PENGINE_CMD_DEFN = ^ENGINE_CMD_DEFN;
  ENGINE_GEN_FUNC_PTR = Pointer;
  ENGINE_CTRL_FUNC_PTR = Pointer;
  ENGINE_LOAD_KEY_PTR = Pointer;
  ENGINE_CIPHERS_PTR = Pointer;
  ENGINE_DIGESTS_PTR = Pointer;
  ENGINE_PKEY_METHS_PTR = Pointer;
  ENGINE_PKEY_ASN1_METHS_PTR = Pointer;
  
  // EVP PKEY 方法类型（如果types.pas中没有）
  EVP_PKEY_METHOD = Pointer;
  PEVP_PKEY_METHOD = ^EVP_PKEY_METHOD;
  EVP_PKEY_ASN1_METHOD = Pointer;
  PEVP_PKEY_ASN1_METHOD = ^EVP_PKEY_ASN1_METHOD;

  // UI 类型定义
  UI = Pointer;
  UI_METHOD = Pointer;
  UI_STRING = Pointer;

  // Engine 标志和命令
  const
    // Engine 标志
    ENGINE_METHOD_RSA         = $0001;
    ENGINE_METHOD_DSA         = $0002;
    ENGINE_METHOD_DH          = $0004;
    ENGINE_METHOD_RAND        = $0008;
    ENGINE_METHOD_CIPHERS     = $0040;
    ENGINE_METHOD_DIGESTS     = $0080;
    ENGINE_METHOD_PKEY_METHS  = $0200;
    ENGINE_METHOD_PKEY_ASN1_METHS = $0400;
    ENGINE_METHOD_EC          = $0800;
    ENGINE_METHOD_ALL         = $FFFF;
    ENGINE_METHOD_NONE        = $0000;

    // Engine 命令
    ENGINE_CMD_SO_PATH        = 100;
    ENGINE_CMD_ID             = 101;
    ENGINE_CMD_LOAD_CERT_CTRL = 102;
    ENGINE_CMD_LOAD_KEY_CTRL  = 103;
    ENGINE_CMD_LIST_ADD       = 104;
    ENGINE_CMD_LIST_REMOVE    = 105;
    ENGINE_CMD_MODULE_PATH    = 106;
    ENGINE_CMD_LOAD_PRIVKEY   = 107;
    ENGINE_CMD_LOAD_PUBKEY    = 108;
    ENGINE_CMD_DEBUG_MODE     = 109;
    ENGINE_CMD_DEBUG_FILE     = 110;
    ENGINE_CMD_LOCK_TYPE      = 111;

    // Engine 控制标志
    ENGINE_CTRL_SET_LOGSTREAM        = 1;
    ENGINE_CTRL_SET_PASSWORD_CALLBACK = 2;
    ENGINE_CTRL_HUP                   = 3;
    ENGINE_CTRL_SET_USER_INTERFACE   = 4;
    ENGINE_CTRL_SET_CALLBACK_DATA    = 5;
    ENGINE_CTRL_LOAD_CONFIGURATION   = 6;
    ENGINE_CTRL_LOAD_SECTION          = 7;
    ENGINE_CTRL_HAS_CTRL_FUNCTION    = 10;
    ENGINE_CTRL_GET_FIRST_CMD_TYPE   = 11;
    ENGINE_CTRL_GET_NEXT_CMD_TYPE    = 12;
    ENGINE_CTRL_GET_CMD_FROM_NAME    = 13;
    ENGINE_CTRL_GET_NAME_LEN_FROM_CMD = 14;
    ENGINE_CTRL_GET_NAME_FROM_CMD    = 15;
    ENGINE_CTRL_GET_DESC_LEN_FROM_CMD = 16;
    ENGINE_CTRL_GET_DESC_FROM_CMD    = 17;
    ENGINE_CTRL_GET_CMD_FLAGS        = 18;
    ENGINE_CTRL_CMD_STRING           = 19;
    ENGINE_CTRL_SET_RAND              = 20;

    // 密钥格式
    ENGINE_KEY_FORMAT_NONE   = 0;
    ENGINE_KEY_FORMAT_PEM    = 1;
    ENGINE_KEY_FORMAT_ASN1   = 2;

  // Engine 函数类型
  type
    // Engine 管理函数
    TENGINE_new = function(): PENGINE; cdecl;
    TENGINE_free = function(e: PENGINE): Integer; cdecl;
    TENGINE_up_ref = function(e: PENGINE): Integer; cdecl;
    TENGINE_by_id = function(const id: PAnsiChar): PENGINE; cdecl;
    TENGINE_by_dso = function(const dso: PAnsiChar; const id: PAnsiChar): PENGINE; cdecl;
    TENGINE_add = function(e: PENGINE): Integer; cdecl;
    TENGINE_remove = function(e: PENGINE): Integer; cdecl;
    TENGINE_get_first = function(): PENGINE; cdecl;
    TENGINE_get_last = function(): PENGINE; cdecl;
    TENGINE_get_next = function(e: PENGINE): PENGINE; cdecl;
    TENGINE_get_prev = function(e: PENGINE): PENGINE; cdecl;

    // Engine 初始化和完成
    TENGINE_init = function(e: PENGINE): Integer; cdecl;
    TENGINE_finish = function(e: PENGINE): Integer; cdecl;
    TENGINE_load_builtin_engines = procedure(); cdecl;
    TENGINE_cleanup = procedure(); cdecl;

    // Engine 控制
    TENGINE_ctrl = function(e: PENGINE; cmd: Integer; i: Integer; p: Pointer; f: Pointer): Integer; cdecl;
    TENGINE_ctrl_cmd = function(e: PENGINE; const cmd_name: PAnsiChar; i: Integer; 
      p: Pointer; f: Pointer; cmd_optional: Integer): Integer; cdecl;
    TENGINE_ctrl_cmd_string = function(e: PENGINE; const cmd_name: PAnsiChar; 
      const arg: PAnsiChar; cmd_optional: Integer): Integer; cdecl;

    // Engine 属性获取和设置
    TENGINE_get_id = function(const e: PENGINE): PAnsiChar; cdecl;
    TENGINE_get_name = function(const e: PENGINE): PAnsiChar; cdecl;
    TENGINE_set_id = function(e: PENGINE; const id: PAnsiChar): Integer; cdecl;
    TENGINE_set_name = function(e: PENGINE; const name: PAnsiChar): Integer; cdecl;
    TENGINE_get_flags = function(const e: PENGINE): Integer; cdecl;
    TENGINE_set_flags = function(e: PENGINE; flags: Integer): Integer; cdecl;
    TENGINE_get_cmd_defns = function(const e: PENGINE): PENGINE_CMD_DEFN; cdecl;
    TENGINE_set_cmd_defns = function(e: PENGINE; defns: PENGINE_CMD_DEFN): Integer; cdecl;

    // Engine 算法方法
    TENGINE_get_RSA = function(const e: PENGINE): PRSA_METHOD; cdecl;
    TENGINE_set_RSA = function(e: PENGINE; rsa: PRSA_METHOD): Integer; cdecl;
    TENGINE_get_DSA = function(const e: PENGINE): PDSA_METHOD; cdecl;
    TENGINE_set_DSA = function(e: PENGINE; dsa: PDSA_METHOD): Integer; cdecl;
    TENGINE_get_DH = function(const e: PENGINE): PDH_METHOD; cdecl;
    TENGINE_set_DH = function(e: PENGINE; dh: PDH_METHOD): Integer; cdecl;
    TENGINE_get_EC = function(const e: PENGINE): PEC_KEY_METHOD; cdecl;
    TENGINE_set_EC = function(e: PENGINE; ec: PEC_KEY_METHOD): Integer; cdecl;
    TENGINE_get_RAND = function(const e: PENGINE): PRAND_METHOD; cdecl;
    TENGINE_set_RAND = function(e: PENGINE; rand: PRAND_METHOD): Integer; cdecl;

    // Engine 密码和摘要
    TENGINE_get_cipher = function(e: PENGINE; nid: Integer): PEVP_CIPHER; cdecl;
    TENGINE_get_digest = function(e: PENGINE; nid: Integer): PEVP_MD; cdecl;
    TENGINE_get_cipher_engine = function(nid: Integer): PENGINE; cdecl;
    TENGINE_get_digest_engine = function(nid: Integer): PENGINE; cdecl;
    TENGINE_get_pkey_meth_engine = function(nid: Integer): PENGINE; cdecl;
    TENGINE_get_pkey_asn1_meth_engine = function(nid: Integer): PENGINE; cdecl;
    TENGINE_get_pkey_meth = function(e: PENGINE; nid: Integer): PEVP_PKEY_METHOD; cdecl;
    TENGINE_get_pkey_asn1_meth = function(e: PENGINE; nid: Integer): PEVP_PKEY_ASN1_METHOD; cdecl;
    TENGINE_get_pkey_asn1_meth_str = function(e: PENGINE; const str: PAnsiChar; len: Integer): PEVP_PKEY_ASN1_METHOD; cdecl;

    // Engine 默认设置
    TENGINE_get_default_RSA = function(): PENGINE; cdecl;
    TENGINE_get_default_DSA = function(): PENGINE; cdecl;
    TENGINE_get_default_DH = function(): PENGINE; cdecl;
    TENGINE_get_default_RAND = function(): PENGINE; cdecl;
    TENGINE_set_default_RSA = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default_DSA = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default_DH = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default_RAND = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default = function(e: PENGINE; flags: Cardinal): Integer; cdecl;
    TENGINE_set_default_ciphers = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default_digests = function(e: PENGINE): Integer; cdecl;
    TENGINE_set_default_string = function(const name: PAnsiChar): Integer; cdecl;

    // Engine 密钥加载
    TENGINE_load_private_key = function(e: PENGINE; const key_id: PAnsiChar; 
      ui_method: UI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl;
    TENGINE_load_public_key = function(e: PENGINE; const key_id: PAnsiChar; 
      ui_method: UI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl;
    TENGINE_load_ssl_client_cert = function(e: PENGINE; s: PSSL; ca_dn: PSTACK_OF_X509_NAME; 
      pcert: PPX509; pkey: PPEVP_PKEY; pother: PPSTACK_OF_X509; ui_method: UI_METHOD; 
      callback_data: Pointer): Integer; cdecl;

    // Engine 表和注册
    TENGINE_get_table_flags = function(): Cardinal; cdecl;
    TENGINE_set_table_flags = procedure(flags: Cardinal); cdecl;
    TENGINE_register_RSA = function(e: PENGINE): Integer; cdecl;
    TENGINE_unregister_RSA = procedure(e: PENGINE); cdecl;
    TENGINE_register_all_RSA = procedure(); cdecl;
    TENGINE_register_DSA = function(e: PENGINE): Integer; cdecl;
    TENGINE_unregister_DSA = procedure(e: PENGINE); cdecl;
    TENGINE_register_all_DSA = procedure(); cdecl;
    TENGINE_register_DH = function(e: PENGINE): Integer; cdecl;
    TENGINE_unregister_DH = procedure(e: PENGINE); cdecl;
    TENGINE_register_all_DH = procedure(); cdecl;
    TENGINE_register_RAND = function(e: PENGINE): Integer; cdecl;
    TENGINE_unregister_RAND = procedure(e: PENGINE); cdecl;
    TENGINE_register_all_RAND = procedure(); cdecl;
    TENGINE_register_all_ciphers = procedure(); cdecl;
    TENGINE_register_all_digests = procedure(); cdecl;
    TENGINE_register_all_pkey_meths = procedure(); cdecl;
    TENGINE_register_all_pkey_asn1_meths = procedure(); cdecl;

    // 动态引擎加载
    TENGINE_load_dynamic = function(): PENGINE; cdecl;
    TENGINE_load_openssl = function(): PENGINE; cdecl;
    TENGINE_load_cryptodev = function(): PENGINE; cdecl;
    TENGINE_load_rdrand = function(): PENGINE; cdecl;

var
  // Engine 管理函数
  ENGINE_new: TENGINE_new = nil;
  ENGINE_free: TENGINE_free = nil;
  ENGINE_up_ref: TENGINE_up_ref = nil;
  ENGINE_by_id: TENGINE_by_id = nil;
  ENGINE_by_dso: TENGINE_by_dso = nil;
  ENGINE_add: TENGINE_add = nil;
  ENGINE_remove: TENGINE_remove = nil;
  ENGINE_get_first: TENGINE_get_first = nil;
  ENGINE_get_last: TENGINE_get_last = nil;
  ENGINE_get_next: TENGINE_get_next = nil;
  ENGINE_get_prev: TENGINE_get_prev = nil;

  // Engine 初始化和完成
  ENGINE_init: TENGINE_init = nil;
  ENGINE_finish: TENGINE_finish = nil;
  ENGINE_load_builtin_engines: TENGINE_load_builtin_engines = nil;
  ENGINE_cleanup: TENGINE_cleanup = nil;

  // Engine 控制
  ENGINE_ctrl: TENGINE_ctrl = nil;
  ENGINE_ctrl_cmd: TENGINE_ctrl_cmd = nil;
  ENGINE_ctrl_cmd_string_func: TENGINE_ctrl_cmd_string = nil;

  // Engine 属性获取和设置
  ENGINE_get_id: TENGINE_get_id = nil;
  ENGINE_get_name: TENGINE_get_name = nil;
  ENGINE_set_id: TENGINE_set_id = nil;
  ENGINE_set_name: TENGINE_set_name = nil;
  ENGINE_get_flags: TENGINE_get_flags = nil;
  ENGINE_set_flags: TENGINE_set_flags = nil;
  ENGINE_get_cmd_defns: TENGINE_get_cmd_defns = nil;
  ENGINE_set_cmd_defns: TENGINE_set_cmd_defns = nil;

  // Engine 算法方法
  ENGINE_get_RSA: TENGINE_get_RSA = nil;
  ENGINE_set_RSA: TENGINE_set_RSA = nil;
  ENGINE_get_DSA: TENGINE_get_DSA = nil;
  ENGINE_set_DSA: TENGINE_set_DSA = nil;
  ENGINE_get_DH: TENGINE_get_DH = nil;
  ENGINE_set_DH: TENGINE_set_DH = nil;
  ENGINE_get_EC: TENGINE_get_EC = nil;
  ENGINE_set_EC: TENGINE_set_EC = nil;
  ENGINE_get_RAND: TENGINE_get_RAND = nil;
  ENGINE_set_RAND: TENGINE_set_RAND = nil;

  // Engine 密码和摘要
  ENGINE_get_cipher: TENGINE_get_cipher = nil;
  ENGINE_get_digest: TENGINE_get_digest = nil;
  ENGINE_get_cipher_engine: TENGINE_get_cipher_engine = nil;
  ENGINE_get_digest_engine: TENGINE_get_digest_engine = nil;
  ENGINE_get_pkey_meth_engine: TENGINE_get_pkey_meth_engine = nil;
  ENGINE_get_pkey_asn1_meth_engine: TENGINE_get_pkey_asn1_meth_engine = nil;
  ENGINE_get_pkey_meth: TENGINE_get_pkey_meth = nil;
  ENGINE_get_pkey_asn1_meth: TENGINE_get_pkey_asn1_meth = nil;
  ENGINE_get_pkey_asn1_meth_str: TENGINE_get_pkey_asn1_meth_str = nil;

  // Engine 默认设置
  ENGINE_get_default_RSA: TENGINE_get_default_RSA = nil;
  ENGINE_get_default_DSA: TENGINE_get_default_DSA = nil;
  ENGINE_get_default_DH: TENGINE_get_default_DH = nil;
  ENGINE_get_default_RAND: TENGINE_get_default_RAND = nil;
  ENGINE_set_default_RSA: TENGINE_set_default_RSA = nil;
  ENGINE_set_default_DSA: TENGINE_set_default_DSA = nil;
  ENGINE_set_default_DH: TENGINE_set_default_DH = nil;
  ENGINE_set_default_RAND: TENGINE_set_default_RAND = nil;
  ENGINE_set_default: TENGINE_set_default = nil;
  ENGINE_set_default_ciphers: TENGINE_set_default_ciphers = nil;
  ENGINE_set_default_digests: TENGINE_set_default_digests = nil;
  ENGINE_set_default_string: TENGINE_set_default_string = nil;

  // Engine 密钥加载
  ENGINE_load_private_key: TENGINE_load_private_key = nil;
  ENGINE_load_public_key: TENGINE_load_public_key = nil;
  ENGINE_load_ssl_client_cert: TENGINE_load_ssl_client_cert = nil;

  // Engine 表和注册
  ENGINE_get_table_flags: TENGINE_get_table_flags = nil;
  ENGINE_set_table_flags: TENGINE_set_table_flags = nil;
  ENGINE_register_RSA: TENGINE_register_RSA = nil;
  ENGINE_unregister_RSA: TENGINE_unregister_RSA = nil;
  ENGINE_register_all_RSA: TENGINE_register_all_RSA = nil;
  ENGINE_register_DSA: TENGINE_register_DSA = nil;
  ENGINE_unregister_DSA: TENGINE_unregister_DSA = nil;
  ENGINE_register_all_DSA: TENGINE_register_all_DSA = nil;
  ENGINE_register_DH: TENGINE_register_DH = nil;
  ENGINE_unregister_DH: TENGINE_unregister_DH = nil;
  ENGINE_register_all_DH: TENGINE_register_all_DH = nil;
  ENGINE_register_RAND: TENGINE_register_RAND = nil;
  ENGINE_unregister_RAND: TENGINE_unregister_RAND = nil;
  ENGINE_register_all_RAND: TENGINE_register_all_RAND = nil;
  ENGINE_register_all_ciphers: TENGINE_register_all_ciphers = nil;
  ENGINE_register_all_digests: TENGINE_register_all_digests = nil;
  ENGINE_register_all_pkey_meths: TENGINE_register_all_pkey_meths = nil;
  ENGINE_register_all_pkey_asn1_meths: TENGINE_register_all_pkey_asn1_meths = nil;

  // 动态引擎加载
  ENGINE_load_dynamic: TENGINE_load_dynamic = nil;
  ENGINE_load_openssl: TENGINE_load_openssl = nil;
  ENGINE_load_cryptodev: TENGINE_load_cryptodev = nil;
  ENGINE_load_rdrand: TENGINE_load_rdrand = nil;

// 加载和卸载函数
function LoadOpenSSLEngine(const ACryptoLib: THandle): Boolean;
procedure UnloadOpenSSLEngine;

// 辅助函数
function InitializeEngine(const AEngineID: string): PENGINE;
function LoadEnginePrivateKey(AEngine: PENGINE; const AKeyID: string): PEVP_PKEY;
function LoadEnginePublicKey(AEngine: PENGINE; const AKeyID: string): PEVP_PKEY;
function SetEngineAsDefault(AEngine: PENGINE; AFlags: Cardinal = ENGINE_METHOD_ALL): Boolean;
function ListAvailableEngines: TStringList;

implementation

var
  FEngineLoaded: Boolean = False;

function LoadOpenSSLEngine(const ACryptoLib: THandle): Boolean;
begin
  if FEngineLoaded then
    Exit(True);

  if ACryptoLib = 0 then
    Exit(False);

  // 加载 Engine 管理函数
  Pointer(ENGINE_new) := GetProcAddress(ACryptoLib, 'ENGINE_new');
  Pointer(ENGINE_free) := GetProcAddress(ACryptoLib, 'ENGINE_free');
  Pointer(ENGINE_up_ref) := GetProcAddress(ACryptoLib, 'ENGINE_up_ref');
  Pointer(ENGINE_by_id) := GetProcAddress(ACryptoLib, 'ENGINE_by_id');
  Pointer(ENGINE_by_dso) := GetProcAddress(ACryptoLib, 'ENGINE_by_dso');
  Pointer(ENGINE_add) := GetProcAddress(ACryptoLib, 'ENGINE_add');
  Pointer(ENGINE_remove) := GetProcAddress(ACryptoLib, 'ENGINE_remove');
  Pointer(ENGINE_get_first) := GetProcAddress(ACryptoLib, 'ENGINE_get_first');
  Pointer(ENGINE_get_last) := GetProcAddress(ACryptoLib, 'ENGINE_get_last');
  Pointer(ENGINE_get_next) := GetProcAddress(ACryptoLib, 'ENGINE_get_next');
  Pointer(ENGINE_get_prev) := GetProcAddress(ACryptoLib, 'ENGINE_get_prev');

  // 加载 Engine 初始化和完成
  Pointer(ENGINE_init) := GetProcAddress(ACryptoLib, 'ENGINE_init');
  Pointer(ENGINE_finish) := GetProcAddress(ACryptoLib, 'ENGINE_finish');
  Pointer(ENGINE_load_builtin_engines) := GetProcAddress(ACryptoLib, 'ENGINE_load_builtin_engines');
  Pointer(ENGINE_cleanup) := GetProcAddress(ACryptoLib, 'ENGINE_cleanup');

  // 加载 Engine 控制
  Pointer(ENGINE_ctrl) := GetProcAddress(ACryptoLib, 'ENGINE_ctrl');
  Pointer(ENGINE_ctrl_cmd) := GetProcAddress(ACryptoLib, 'ENGINE_ctrl_cmd');
  Pointer(ENGINE_ctrl_cmd_string_func) := GetProcAddress(ACryptoLib, 'ENGINE_ctrl_cmd_string');

  // 加载 Engine 属性获取和设置
  Pointer(ENGINE_get_id) := GetProcAddress(ACryptoLib, 'ENGINE_get_id');
  Pointer(ENGINE_get_name) := GetProcAddress(ACryptoLib, 'ENGINE_get_name');
  Pointer(ENGINE_set_id) := GetProcAddress(ACryptoLib, 'ENGINE_set_id');
  Pointer(ENGINE_set_name) := GetProcAddress(ACryptoLib, 'ENGINE_set_name');
  Pointer(ENGINE_get_flags) := GetProcAddress(ACryptoLib, 'ENGINE_get_flags');
  Pointer(ENGINE_set_flags) := GetProcAddress(ACryptoLib, 'ENGINE_set_flags');
  Pointer(ENGINE_get_cmd_defns) := GetProcAddress(ACryptoLib, 'ENGINE_get_cmd_defns');
  Pointer(ENGINE_set_cmd_defns) := GetProcAddress(ACryptoLib, 'ENGINE_set_cmd_defns');

  // 加载 Engine 算法方法
  Pointer(ENGINE_get_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_get_RSA');
  Pointer(ENGINE_set_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_set_RSA');
  Pointer(ENGINE_get_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_get_DSA');
  Pointer(ENGINE_set_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_set_DSA');
  Pointer(ENGINE_get_DH) := GetProcAddress(ACryptoLib, 'ENGINE_get_DH');
  Pointer(ENGINE_set_DH) := GetProcAddress(ACryptoLib, 'ENGINE_set_DH');
  Pointer(ENGINE_get_EC) := GetProcAddress(ACryptoLib, 'ENGINE_get_EC');
  Pointer(ENGINE_set_EC) := GetProcAddress(ACryptoLib, 'ENGINE_set_EC');
  Pointer(ENGINE_get_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_get_RAND');
  Pointer(ENGINE_set_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_set_RAND');

  // 加载 Engine 密码和摘要
  Pointer(ENGINE_get_cipher) := GetProcAddress(ACryptoLib, 'ENGINE_get_cipher');
  Pointer(ENGINE_get_digest) := GetProcAddress(ACryptoLib, 'ENGINE_get_digest');
  Pointer(ENGINE_get_cipher_engine) := GetProcAddress(ACryptoLib, 'ENGINE_get_cipher_engine');
  Pointer(ENGINE_get_digest_engine) := GetProcAddress(ACryptoLib, 'ENGINE_get_digest_engine');
  Pointer(ENGINE_get_pkey_meth_engine) := GetProcAddress(ACryptoLib, 'ENGINE_get_pkey_meth_engine');
  Pointer(ENGINE_get_pkey_asn1_meth_engine) := GetProcAddress(ACryptoLib, 'ENGINE_get_pkey_asn1_meth_engine');
  Pointer(ENGINE_get_pkey_meth) := GetProcAddress(ACryptoLib, 'ENGINE_get_pkey_meth');
  Pointer(ENGINE_get_pkey_asn1_meth) := GetProcAddress(ACryptoLib, 'ENGINE_get_pkey_asn1_meth');
  Pointer(ENGINE_get_pkey_asn1_meth_str) := GetProcAddress(ACryptoLib, 'ENGINE_get_pkey_asn1_meth_str');

  // 加载 Engine 默认设置
  Pointer(ENGINE_get_default_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_get_default_RSA');
  Pointer(ENGINE_get_default_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_get_default_DSA');
  Pointer(ENGINE_get_default_DH) := GetProcAddress(ACryptoLib, 'ENGINE_get_default_DH');
  Pointer(ENGINE_get_default_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_get_default_RAND');
  Pointer(ENGINE_set_default_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_RSA');
  Pointer(ENGINE_set_default_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_DSA');
  Pointer(ENGINE_set_default_DH) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_DH');
  Pointer(ENGINE_set_default_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_RAND');
  Pointer(ENGINE_set_default) := GetProcAddress(ACryptoLib, 'ENGINE_set_default');
  Pointer(ENGINE_set_default_ciphers) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_ciphers');
  Pointer(ENGINE_set_default_digests) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_digests');
  Pointer(ENGINE_set_default_string) := GetProcAddress(ACryptoLib, 'ENGINE_set_default_string');

  // 加载 Engine 密钥加载
  Pointer(ENGINE_load_private_key) := GetProcAddress(ACryptoLib, 'ENGINE_load_private_key');
  Pointer(ENGINE_load_public_key) := GetProcAddress(ACryptoLib, 'ENGINE_load_public_key');
  Pointer(ENGINE_load_ssl_client_cert) := GetProcAddress(ACryptoLib, 'ENGINE_load_ssl_client_cert');

  // 加载 Engine 表和注册
  Pointer(ENGINE_get_table_flags) := GetProcAddress(ACryptoLib, 'ENGINE_get_table_flags');
  Pointer(ENGINE_set_table_flags) := GetProcAddress(ACryptoLib, 'ENGINE_set_table_flags');
  Pointer(ENGINE_register_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_register_RSA');
  Pointer(ENGINE_unregister_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_unregister_RSA');
  Pointer(ENGINE_register_all_RSA) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_RSA');
  Pointer(ENGINE_register_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_register_DSA');
  Pointer(ENGINE_unregister_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_unregister_DSA');
  Pointer(ENGINE_register_all_DSA) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_DSA');
  Pointer(ENGINE_register_DH) := GetProcAddress(ACryptoLib, 'ENGINE_register_DH');
  Pointer(ENGINE_unregister_DH) := GetProcAddress(ACryptoLib, 'ENGINE_unregister_DH');
  Pointer(ENGINE_register_all_DH) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_DH');
  Pointer(ENGINE_register_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_register_RAND');
  Pointer(ENGINE_unregister_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_unregister_RAND');
  Pointer(ENGINE_register_all_RAND) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_RAND');
  Pointer(ENGINE_register_all_ciphers) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_ciphers');
  Pointer(ENGINE_register_all_digests) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_digests');
  Pointer(ENGINE_register_all_pkey_meths) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_pkey_meths');
  Pointer(ENGINE_register_all_pkey_asn1_meths) := GetProcAddress(ACryptoLib, 'ENGINE_register_all_pkey_asn1_meths');

  // 加载动态引擎
  Pointer(ENGINE_load_dynamic) := GetProcAddress(ACryptoLib, 'ENGINE_load_dynamic');
  Pointer(ENGINE_load_openssl) := GetProcAddress(ACryptoLib, 'ENGINE_load_openssl');
  Pointer(ENGINE_load_cryptodev) := GetProcAddress(ACryptoLib, 'ENGINE_load_cryptodev');
  Pointer(ENGINE_load_rdrand) := GetProcAddress(ACryptoLib, 'ENGINE_load_rdrand');

  // 注意: ENGINE 功能在某些 OpenSSL 版本中可能不存在
  FEngineLoaded := Assigned(ENGINE_by_id);
  Result := FEngineLoaded;
end;

procedure UnloadOpenSSLEngine;
begin
  if not FEngineLoaded then
    Exit;

  // 清理 Engine 管理函数
  ENGINE_new := nil;
  ENGINE_free := nil;
  ENGINE_up_ref := nil;
  ENGINE_by_id := nil;
  ENGINE_by_dso := nil;
  ENGINE_add := nil;
  ENGINE_remove := nil;
  ENGINE_get_first := nil;
  ENGINE_get_last := nil;
  ENGINE_get_next := nil;
  ENGINE_get_prev := nil;

  // 清理 Engine 初始化和完成
  ENGINE_init := nil;
  ENGINE_finish := nil;
  ENGINE_load_builtin_engines := nil;
  ENGINE_cleanup := nil;

  // 清理 Engine 控制
  ENGINE_ctrl := nil;
  ENGINE_ctrl_cmd := nil;
  ENGINE_ctrl_cmd_string_func := nil;

  // 清理 Engine 属性获取和设置
  ENGINE_get_id := nil;
  ENGINE_get_name := nil;
  ENGINE_set_id := nil;
  ENGINE_set_name := nil;
  ENGINE_get_flags := nil;
  ENGINE_set_flags := nil;
  ENGINE_get_cmd_defns := nil;
  ENGINE_set_cmd_defns := nil;

  // 清理 Engine 算法方法
  ENGINE_get_RSA := nil;
  ENGINE_set_RSA := nil;
  ENGINE_get_DSA := nil;
  ENGINE_set_DSA := nil;
  ENGINE_get_DH := nil;
  ENGINE_set_DH := nil;
  ENGINE_get_EC := nil;
  ENGINE_set_EC := nil;
  ENGINE_get_RAND := nil;
  ENGINE_set_RAND := nil;

  // 清理 Engine 密码和摘要
  ENGINE_get_cipher := nil;
  ENGINE_get_digest := nil;
  ENGINE_get_cipher_engine := nil;
  ENGINE_get_digest_engine := nil;
  ENGINE_get_pkey_meth_engine := nil;
  ENGINE_get_pkey_asn1_meth_engine := nil;
  ENGINE_get_pkey_meth := nil;
  ENGINE_get_pkey_asn1_meth := nil;
  ENGINE_get_pkey_asn1_meth_str := nil;

  // 清理 Engine 默认设置
  ENGINE_get_default_RSA := nil;
  ENGINE_get_default_DSA := nil;
  ENGINE_get_default_DH := nil;
  ENGINE_get_default_RAND := nil;
  ENGINE_set_default_RSA := nil;
  ENGINE_set_default_DSA := nil;
  ENGINE_set_default_DH := nil;
  ENGINE_set_default_RAND := nil;
  ENGINE_set_default := nil;
  ENGINE_set_default_ciphers := nil;
  ENGINE_set_default_digests := nil;
  ENGINE_set_default_string := nil;

  // 清理 Engine 密钥加载
  ENGINE_load_private_key := nil;
  ENGINE_load_public_key := nil;
  ENGINE_load_ssl_client_cert := nil;

  // 清理 Engine 表和注册
  ENGINE_get_table_flags := nil;
  ENGINE_set_table_flags := nil;
  ENGINE_register_RSA := nil;
  ENGINE_unregister_RSA := nil;
  ENGINE_register_all_RSA := nil;
  ENGINE_register_DSA := nil;
  ENGINE_unregister_DSA := nil;
  ENGINE_register_all_DSA := nil;
  ENGINE_register_DH := nil;
  ENGINE_unregister_DH := nil;
  ENGINE_register_all_DH := nil;
  ENGINE_register_RAND := nil;
  ENGINE_unregister_RAND := nil;
  ENGINE_register_all_RAND := nil;
  ENGINE_register_all_ciphers := nil;
  ENGINE_register_all_digests := nil;
  ENGINE_register_all_pkey_meths := nil;
  ENGINE_register_all_pkey_asn1_meths := nil;

  // 清理动态引擎
  ENGINE_load_dynamic := nil;
  ENGINE_load_openssl := nil;
  ENGINE_load_cryptodev := nil;
  ENGINE_load_rdrand := nil;

  FEngineLoaded := False;
end;

// 辅助函数实现
function InitializeEngine(const AEngineID: string): PENGINE;
begin
  Result := nil;
  if not FEngineLoaded then
    Exit;

  // 加载内建引擎
  if Assigned(ENGINE_load_builtin_engines) then
    ENGINE_load_builtin_engines();

  // 获取引擎
  Result := ENGINE_by_id(PAnsiChar(AnsiString(AEngineID)));
  if Result = nil then
    Exit;

  // 初始化引擎
  if ENGINE_init(Result) = 0 then
  begin
    ENGINE_free(Result);
    Result := nil;
  end;
end;

function LoadEnginePrivateKey(AEngine: PENGINE; const AKeyID: string): PEVP_PKEY;
begin
  Result := nil;
  if not FEngineLoaded or (AEngine = nil) then
    Exit;

  Result := ENGINE_load_private_key(AEngine, PAnsiChar(AnsiString(AKeyID)), nil, nil);
end;

function LoadEnginePublicKey(AEngine: PENGINE; const AKeyID: string): PEVP_PKEY;
begin
  Result := nil;
  if not FEngineLoaded or (AEngine = nil) then
    Exit;

  Result := ENGINE_load_public_key(AEngine, PAnsiChar(AnsiString(AKeyID)), nil, nil);
end;

function SetEngineAsDefault(AEngine: PENGINE; AFlags: Cardinal): Boolean;
begin
  Result := False;
  if not FEngineLoaded or (AEngine = nil) then
    Exit;

  Result := ENGINE_set_default(AEngine, AFlags) <> 0;
end;

function ListAvailableEngines: TStringList;
var
  Eng: PENGINE;
begin
  Result := TStringList.Create;
  if not FEngineLoaded then
    Exit;

  // 加载内建引擎
  if Assigned(ENGINE_load_builtin_engines) then
    ENGINE_load_builtin_engines();

  // 枚举所有引擎
  Eng := ENGINE_get_first();
  while Eng <> nil do
  begin
    Result.Add(string(ENGINE_get_id(Eng)));
    Eng := ENGINE_get_next(Eng);
  end;
end;

end.