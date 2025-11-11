unit fafafa.ssl.openssl.api.ssl;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

type
  { SSL Version and Protocol Functions }
  TSSL_CTX_set_min_proto_version = function(ctx: PSSL_CTX; version: Integer): Integer; cdecl;
  TSSL_CTX_set_max_proto_version = function(ctx: PSSL_CTX; version: Integer): Integer; cdecl;
  TSSL_CTX_get_min_proto_version = function(ctx: PSSL_CTX): Integer; cdecl;
  TSSL_CTX_get_max_proto_version = function(ctx: PSSL_CTX): Integer; cdecl;
  TSSL_set_min_proto_version = function(ssl: PSSL; version: Integer): Integer; cdecl;
  TSSL_set_max_proto_version = function(ssl: PSSL; version: Integer): Integer; cdecl;
  TSSL_get_min_proto_version = function(ssl: PSSL): Integer; cdecl;
  TSSL_get_max_proto_version = function(ssl: PSSL): Integer; cdecl;
  TSSL_version = function(const ssl: PSSL): Integer; cdecl;
  TSSL_client_version = function(const ssl: PSSL): Integer; cdecl;
  TSSL_is_dtls = function(const ssl: PSSL): Integer; cdecl;

  { SSL Options }
  TSSL_CTX_set_options = function(ctx: PSSL_CTX; options: UInt64): UInt64; cdecl;
  TSSL_CTX_clear_options = function(ctx: PSSL_CTX; options: UInt64): UInt64; cdecl;
  TSSL_CTX_get_options = function(ctx: PSSL_CTX): UInt64; cdecl;
  TSSL_set_options = function(ssl: PSSL; options: UInt64): UInt64; cdecl;
  TSSL_clear_options = function(ssl: PSSL; options: UInt64): UInt64; cdecl;
  TSSL_get_options = function(ssl: PSSL): UInt64; cdecl;

  { SSL Modes }
  TSSL_CTX_set_mode = function(ctx: PSSL_CTX; mode: UInt64): UInt64; cdecl;
  TSSL_CTX_clear_mode = function(ctx: PSSL_CTX; mode: UInt64): UInt64; cdecl;
  TSSL_CTX_get_mode = function(ctx: PSSL_CTX): UInt64; cdecl;
  TSSL_set_mode = function(ssl: PSSL; mode: UInt64): UInt64; cdecl;
  TSSL_clear_mode = function(ssl: PSSL; mode: UInt64): UInt64; cdecl;
  TSSL_get_mode = function(ssl: PSSL): UInt64; cdecl;

  { Cipher functions }
  TSSL_CTX_set_cipher_list = function(ctx: PSSL_CTX; const str: PAnsiChar): Integer; cdecl;
  TSSL_CTX_set_ciphersuites = function(ctx: PSSL_CTX; const str: PAnsiChar): Integer; cdecl;
  TSSL_set_cipher_list = function(ssl: PSSL; const str: PAnsiChar): Integer; cdecl;
  TSSL_set_ciphersuites = function(ssl: PSSL; const str: PAnsiChar): Integer; cdecl;
  TSSL_get_ciphers = function(const ssl: PSSL): PSTACK_OF_SSL_CIPHER; cdecl;
  TSSL_get_cipher_list = function(const ssl: PSSL; n: Integer): PAnsiChar; cdecl;
  TSSL_get_shared_ciphers = function(const ssl: PSSL; buf: PAnsiChar; len: Integer): PAnsiChar; cdecl;
  TSSL_get_current_cipher = function(const ssl: PSSL): PSSL_CIPHER; cdecl;
  TSSL_get_pending_cipher = function(const ssl: PSSL): PSSL_CIPHER; cdecl;
  TSSL_CIPHER_get_name = function(const c: PSSL_CIPHER): PAnsiChar; cdecl;
  TSSL_CIPHER_get_bits = function(const c: PSSL_CIPHER; alg_bits: PInteger): Integer; cdecl;
  TSSL_CIPHER_get_version = function(const c: PSSL_CIPHER): PAnsiChar; cdecl;
  TSSL_CIPHER_description = function(c: PSSL_CIPHER; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl;
  TSSL_CIPHER_get_id = function(const c: PSSL_CIPHER): UInt32; cdecl;

  { SNI (Server Name Indication) }
  TSSL_set_tlsext_host_name = function(ssl: PSSL; const name: PAnsiChar): Integer; cdecl;
  TSSL_get_servername = function(const ssl: PSSL; &type: Integer): PAnsiChar; cdecl;
  TSSL_get_servername_type = function(const ssl: PSSL): Integer; cdecl;
  TSSL_CTX_set_tlsext_servername_callback = function(ctx: PSSL_CTX; cb: Pointer): Integer; cdecl;
  TSSL_CTX_set_tlsext_servername_arg = function(ctx: PSSL_CTX; arg: Pointer): Integer; cdecl;

  { ALPN (Application Layer Protocol Negotiation) }
  TSSL_CTX_set_alpn_protos = function(ctx: PSSL_CTX; const protos: PByte; protos_len: Cardinal): Integer; cdecl;
  TSSL_set_alpn_protos = function(ssl: PSSL; const protos: PByte; protos_len: Cardinal): Integer; cdecl;
  TSSL_CTX_set_alpn_select_cb = procedure(ctx: PSSL_CTX; cb: Pointer; arg: Pointer); cdecl;
  TSSL_get0_alpn_selected = procedure(const ssl: PSSL; const data: PPByte; len: PCardinal); cdecl;
  TSSL_select_next_proto = function(&out: PPByte; outlen: PByte; const server: PByte; server_len: Cardinal; const client: PByte; client_len: Cardinal): Integer; cdecl;

  { Session tickets }
  TSSL_CTX_set_tlsext_ticket_key_cb = function(ctx: PSSL_CTX; cb: Pointer): Integer; cdecl;
  TSSL_set_session_ticket_ext = function(ssl: PSSL; ext_data: Pointer; ext_len: Integer): Integer; cdecl;
  TSSL_set_session_ticket_ext_cb = function(ssl: PSSL; cb: Pointer; arg: Pointer): Integer; cdecl;

  { PSK (Pre-Shared Key) }
  TSSL_CTX_use_psk_identity_hint = function(ctx: PSSL_CTX; const hint: PAnsiChar): Integer; cdecl;
  TSSL_use_psk_identity_hint = function(ssl: PSSL; const hint: PAnsiChar): Integer; cdecl;
  TSSL_CTX_set_psk_server_callback = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSSL_set_psk_server_callback = procedure(ssl: PSSL; cb: Pointer); cdecl;
  TSSL_CTX_set_psk_client_callback = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSSL_set_psk_client_callback = procedure(ssl: PSSL; cb: Pointer); cdecl;

  { Info callback }
  TSSL_CTX_set_info_callback = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSSL_CTX_get_info_callback = function(ctx: PSSL_CTX): Pointer; cdecl;
  TSSL_set_info_callback = procedure(ssl: PSSL; cb: Pointer); cdecl;
  TSSL_get_info_callback = function(const ssl: PSSL): Pointer; cdecl;
  TSSL_state_string = function(const ssl: PSSL): PAnsiChar; cdecl;
  TSSL_state_string_long = function(const ssl: PSSL): PAnsiChar; cdecl;

  { OCSP stapling }
  TSSL_CTX_set_tlsext_status_type = function(ctx: PSSL_CTX; &type: Integer): clong; cdecl;
  TSSL_CTX_get_tlsext_status_type = function(ctx: PSSL_CTX): clong; cdecl;
  TSSL_set_tlsext_status_type = function(ssl: PSSL; &type: Integer): clong; cdecl;
  TSSL_get_tlsext_status_type = function(ssl: PSSL): clong; cdecl;
  TSSL_set_tlsext_status_ocsp_resp = function(ssl: PSSL; resp: PByte; len: clong): clong; cdecl;
  TSSL_get_tlsext_status_ocsp_resp = function(ssl: PSSL; resp: PPByte): clong; cdecl;
  TSSL_CTX_set_tlsext_status_cb = function(ctx: PSSL_CTX; cb: Pointer): clong; cdecl;
  TSSL_CTX_set_tlsext_status_arg = function(ctx: PSSL_CTX; arg: Pointer): clong; cdecl;

  { Early data / 0-RTT }
  TSSL_CTX_set_max_early_data = function(ctx: PSSL_CTX; max_early_data: UInt32): Integer; cdecl;
  TSSL_CTX_get_max_early_data = function(const ctx: PSSL_CTX): UInt32; cdecl;
  TSSL_set_max_early_data = function(ssl: PSSL; max_early_data: UInt32): Integer; cdecl;
  TSSL_get_max_early_data = function(const ssl: PSSL): UInt32; cdecl;
  TSSL_get_early_data_status = function(const ssl: PSSL): Integer; cdecl;

  { KeyLog callback }
  TSSL_CTX_set_keylog_callback = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSSL_CTX_get_keylog_callback = function(ctx: PSSL_CTX): Pointer; cdecl;

  { Record padding }
  TSSL_CTX_set_record_padding_callback = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSSL_CTX_get_record_padding_callback = function(ctx: PSSL_CTX): Pointer; cdecl;
  TSSL_CTX_set_record_padding_callback_arg = procedure(ctx: PSSL_CTX; arg: Pointer); cdecl;
  TSSL_CTX_get_record_padding_callback_arg = function(ctx: PSSL_CTX): Pointer; cdecl;
  TSSL_CTX_set_block_padding = function(ctx: PSSL_CTX; block_size: size_t): Integer; cdecl;
  TSSL_set_record_padding_callback = procedure(ssl: PSSL; cb: Pointer); cdecl;
  TSSL_get_record_padding_callback = function(ssl: PSSL): Pointer; cdecl;
  TSSL_set_record_padding_callback_arg = procedure(ssl: PSSL; arg: Pointer); cdecl;
  TSSL_get_record_padding_callback_arg = function(ssl: PSSL): Pointer; cdecl;
  TSSL_set_block_padding = function(ssl: PSSL; block_size: size_t): Integer; cdecl;

  { QUIC support (OpenSSL 3.2+) }
  TSSL_get_stream_id = function(const ssl: PSSL): UInt64; cdecl;
  TSSL_get_stream_type = function(const ssl: PSSL): Integer; cdecl;
  TSSL_is_stream_local = function(const ssl: PSSL): Integer; cdecl;
  TSSL_new_stream = function(ssl: PSSL; flags: UInt64): PSSL; cdecl;
  TSSL_accept_stream = function(ssl: PSSL; flags: UInt64): PSSL; cdecl;
  TSSL_get_accept_stream_queue_len = function(const ssl: PSSL): size_t; cdecl;
  TSSL_set_default_stream_mode = function(ssl: PSSL; mode: UInt32): Integer; cdecl;
  TSSL_set_incoming_stream_policy = function(ssl: PSSL; policy: Integer; aarg: Pointer): Integer; cdecl;
  TSSL_get0_connection = function(const ssl: PSSL): PSSL; cdecl;
  TSSL_is_connection = function(const ssl: PSSL): Integer; cdecl;
  TSSL_get_stream_read_error_code = function(ssl: PSSL; app_error_code: PUInt64): Integer; cdecl;
  TSSL_get_stream_write_error_code = function(ssl: PSSL; app_error_code: PUInt64): Integer; cdecl;
  TSSL_get_conn_close_info = function(ssl: PSSL; info: Pointer; info_len: size_t): Integer; cdecl;

  { SSL_poll and async support }
  TSSL_poll = function(items: Pointer; num_items: size_t; stride: size_t; timeout: Pointer; result_count: Psize_t; flags: UInt32): Integer; cdecl;
  TSSL_set_async_callback = function(ssl: PSSL; callback: Pointer): Integer; cdecl;
  TSSL_set_async_callback_arg = function(ssl: PSSL; arg: Pointer): Integer; cdecl;
  TSSL_get_async_status = function(ssl: PSSL; status: PInteger): Integer; cdecl;

var
  { SSL Version and Protocol Functions }
  SSL_CTX_set_min_proto_version: TSSL_CTX_set_min_proto_version;
  SSL_CTX_set_max_proto_version: TSSL_CTX_set_max_proto_version;
  SSL_CTX_get_min_proto_version: TSSL_CTX_get_min_proto_version;
  SSL_CTX_get_max_proto_version: TSSL_CTX_get_max_proto_version;
  SSL_set_min_proto_version: TSSL_set_min_proto_version;
  SSL_set_max_proto_version: TSSL_set_max_proto_version;
  SSL_get_min_proto_version: TSSL_get_min_proto_version;
  SSL_get_max_proto_version: TSSL_get_max_proto_version;
  SSL_version: TSSL_version;
  SSL_client_version: TSSL_client_version;
  SSL_is_dtls: TSSL_is_dtls;

  { SSL Options }
  SSL_CTX_set_options: TSSL_CTX_set_options;
  SSL_CTX_clear_options: TSSL_CTX_clear_options;
  SSL_CTX_get_options: TSSL_CTX_get_options;
  SSL_set_options: TSSL_set_options;
  SSL_clear_options: TSSL_clear_options;
  SSL_get_options: TSSL_get_options;

  { SSL Modes }
  SSL_CTX_set_mode: TSSL_CTX_set_mode;
  SSL_CTX_clear_mode: TSSL_CTX_clear_mode;
  SSL_CTX_get_mode: TSSL_CTX_get_mode;
  SSL_set_mode: TSSL_set_mode;
  SSL_clear_mode: TSSL_clear_mode;
  SSL_get_mode: TSSL_get_mode;

  { Cipher functions }
  SSL_CTX_set_cipher_list: TSSL_CTX_set_cipher_list;
  SSL_CTX_set_ciphersuites: TSSL_CTX_set_ciphersuites;
  SSL_set_cipher_list: TSSL_set_cipher_list;
  SSL_set_ciphersuites: TSSL_set_ciphersuites;
  SSL_get_ciphers: TSSL_get_ciphers;
  SSL_get_cipher_list: TSSL_get_cipher_list;
  SSL_get_shared_ciphers: TSSL_get_shared_ciphers;
  SSL_get_current_cipher: TSSL_get_current_cipher;
  SSL_get_pending_cipher: TSSL_get_pending_cipher;
  SSL_CIPHER_get_name: TSSL_CIPHER_get_name;
  SSL_CIPHER_get_bits: TSSL_CIPHER_get_bits;
  SSL_CIPHER_get_version: TSSL_CIPHER_get_version;
  SSL_CIPHER_description: TSSL_CIPHER_description;
  SSL_CIPHER_get_id: TSSL_CIPHER_get_id;

  { SNI }
  SSL_set_tlsext_host_name: TSSL_set_tlsext_host_name;
  SSL_get_servername: TSSL_get_servername;
  SSL_get_servername_type: TSSL_get_servername_type;
  SSL_CTX_set_tlsext_servername_callback: TSSL_CTX_set_tlsext_servername_callback;
  SSL_CTX_set_tlsext_servername_arg: TSSL_CTX_set_tlsext_servername_arg;

  { ALPN }
  SSL_CTX_set_alpn_protos: TSSL_CTX_set_alpn_protos;
  SSL_set_alpn_protos: TSSL_set_alpn_protos;
  SSL_CTX_set_alpn_select_cb: TSSL_CTX_set_alpn_select_cb;
  SSL_get0_alpn_selected: TSSL_get0_alpn_selected;
  SSL_select_next_proto: TSSL_select_next_proto;

  { Session tickets }
  SSL_CTX_set_tlsext_ticket_key_cb: TSSL_CTX_set_tlsext_ticket_key_cb;
  SSL_set_session_ticket_ext: TSSL_set_session_ticket_ext;
  SSL_set_session_ticket_ext_cb: TSSL_set_session_ticket_ext_cb;

  { PSK }
  SSL_CTX_use_psk_identity_hint: TSSL_CTX_use_psk_identity_hint;
  SSL_use_psk_identity_hint: TSSL_use_psk_identity_hint;
  SSL_CTX_set_psk_server_callback: TSSL_CTX_set_psk_server_callback;
  SSL_set_psk_server_callback: TSSL_set_psk_server_callback;
  SSL_CTX_set_psk_client_callback: TSSL_CTX_set_psk_client_callback;
  SSL_set_psk_client_callback: TSSL_set_psk_client_callback;

  { Info callback }
  SSL_CTX_set_info_callback: TSSL_CTX_set_info_callback;
  SSL_CTX_get_info_callback: TSSL_CTX_get_info_callback;
  SSL_set_info_callback: TSSL_set_info_callback;
  SSL_get_info_callback: TSSL_get_info_callback;
  SSL_state_string: TSSL_state_string;
  SSL_state_string_long: TSSL_state_string_long;

  { OCSP stapling }
  SSL_CTX_set_tlsext_status_type: TSSL_CTX_set_tlsext_status_type;
  SSL_CTX_get_tlsext_status_type: TSSL_CTX_get_tlsext_status_type;
  SSL_set_tlsext_status_type: TSSL_set_tlsext_status_type;
  SSL_get_tlsext_status_type: TSSL_get_tlsext_status_type;
  SSL_set_tlsext_status_ocsp_resp: TSSL_set_tlsext_status_ocsp_resp;
  SSL_get_tlsext_status_ocsp_resp: TSSL_get_tlsext_status_ocsp_resp;
  SSL_CTX_set_tlsext_status_cb: TSSL_CTX_set_tlsext_status_cb;
  SSL_CTX_set_tlsext_status_arg: TSSL_CTX_set_tlsext_status_arg;

  { Early data / 0-RTT }
  SSL_CTX_set_max_early_data: TSSL_CTX_set_max_early_data;
  SSL_CTX_get_max_early_data: TSSL_CTX_get_max_early_data;
  SSL_set_max_early_data: TSSL_set_max_early_data;
  SSL_get_max_early_data: TSSL_get_max_early_data;
  SSL_get_early_data_status: TSSL_get_early_data_status;

  { KeyLog callback }
  SSL_CTX_set_keylog_callback: TSSL_CTX_set_keylog_callback;
  SSL_CTX_get_keylog_callback: TSSL_CTX_get_keylog_callback;

  { Record padding }
  SSL_CTX_set_record_padding_callback: TSSL_CTX_set_record_padding_callback;
  SSL_CTX_get_record_padding_callback: TSSL_CTX_get_record_padding_callback;
  SSL_CTX_set_record_padding_callback_arg: TSSL_CTX_set_record_padding_callback_arg;
  SSL_CTX_get_record_padding_callback_arg: TSSL_CTX_get_record_padding_callback_arg;
  SSL_CTX_set_block_padding: TSSL_CTX_set_block_padding;
  SSL_set_record_padding_callback: TSSL_set_record_padding_callback;
  SSL_get_record_padding_callback: TSSL_get_record_padding_callback;
  SSL_set_record_padding_callback_arg: TSSL_set_record_padding_callback_arg;
  SSL_get_record_padding_callback_arg: TSSL_get_record_padding_callback_arg;
  SSL_set_block_padding: TSSL_set_block_padding;

  { QUIC support }
  SSL_get_stream_id: TSSL_get_stream_id;
  SSL_get_stream_type: TSSL_get_stream_type;
  SSL_is_stream_local: TSSL_is_stream_local;
  SSL_new_stream: TSSL_new_stream;
  SSL_accept_stream: TSSL_accept_stream;
  SSL_get_accept_stream_queue_len: TSSL_get_accept_stream_queue_len;
  SSL_set_default_stream_mode: TSSL_set_default_stream_mode;
  SSL_set_incoming_stream_policy: TSSL_set_incoming_stream_policy;
  SSL_get0_connection: TSSL_get0_connection;
  SSL_is_connection: TSSL_is_connection;
  SSL_get_stream_read_error_code: TSSL_get_stream_read_error_code;
  SSL_get_stream_write_error_code: TSSL_get_stream_write_error_code;
  SSL_get_conn_close_info: TSSL_get_conn_close_info;

  { SSL_poll and async support }
  SSL_poll: TSSL_poll;
  SSL_set_async_callback: TSSL_set_async_callback;
  SSL_set_async_callback_arg: TSSL_set_async_callback_arg;
  SSL_get_async_status: TSSL_get_async_status;

function LoadOpenSSLSSL: Boolean;
procedure UnloadOpenSSLSSL;
function IsOpenSSLSSLLoaded: Boolean;

{ Helper function - SSL_set_tlsext_host_name is a macro in OpenSSL, not a real function }
function SSL_set_tlsext_host_name_impl(ssl: PSSL; const name: PAnsiChar): Integer; cdecl;

implementation

uses
  fafafa.ssl.openssl.api.core;

var
  GSSLLoaded: Boolean = False;

function LoadOpenSSLSSL: Boolean;
begin
  if GSSLLoaded then
    Exit(True);

  // Make sure core is loaded first
  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  if not IsOpenSSLCoreLoaded then
    Exit(False);

  // Load SSL protocol functions using helper function
  SSL_CTX_set_min_proto_version := TSSL_CTX_set_min_proto_version(GetSSLProcAddress('SSL_CTX_set_min_proto_version'));
  SSL_CTX_set_max_proto_version := TSSL_CTX_set_max_proto_version(GetSSLProcAddress('SSL_CTX_set_max_proto_version'));
  SSL_CTX_get_min_proto_version := TSSL_CTX_get_min_proto_version(GetSSLProcAddress('SSL_CTX_get_min_proto_version'));
  SSL_CTX_get_max_proto_version := TSSL_CTX_get_max_proto_version(GetSSLProcAddress('SSL_CTX_get_max_proto_version'));
  SSL_set_min_proto_version := TSSL_set_min_proto_version(GetSSLProcAddress('SSL_set_min_proto_version'));
  SSL_set_max_proto_version := TSSL_set_max_proto_version(GetSSLProcAddress('SSL_set_max_proto_version'));
  SSL_get_min_proto_version := TSSL_get_min_proto_version(GetSSLProcAddress('SSL_get_min_proto_version'));
  SSL_get_max_proto_version := TSSL_get_max_proto_version(GetSSLProcAddress('SSL_get_max_proto_version'));
  SSL_version := TSSL_version(GetSSLProcAddress('SSL_version'));
  SSL_client_version := TSSL_client_version(GetSSLProcAddress('SSL_client_version'));
  SSL_is_dtls := TSSL_is_dtls(GetSSLProcAddress('SSL_is_dtls'));
  
  // Load Options functions
  SSL_CTX_set_options := TSSL_CTX_set_options(GetSSLProcAddress('SSL_CTX_set_options'));
  SSL_CTX_clear_options := TSSL_CTX_clear_options(GetSSLProcAddress('SSL_CTX_clear_options'));
  SSL_CTX_get_options := TSSL_CTX_get_options(GetSSLProcAddress('SSL_CTX_get_options'));
  SSL_set_options := TSSL_set_options(GetSSLProcAddress('SSL_set_options'));
  SSL_clear_options := TSSL_clear_options(GetSSLProcAddress('SSL_clear_options'));
  SSL_get_options := TSSL_get_options(GetSSLProcAddress('SSL_get_options'));
  
  // Load Mode functions
  SSL_CTX_set_mode := TSSL_CTX_set_mode(GetSSLProcAddress('SSL_CTX_set_mode'));
  SSL_CTX_clear_mode := TSSL_CTX_clear_mode(GetSSLProcAddress('SSL_CTX_clear_mode'));
  SSL_CTX_get_mode := TSSL_CTX_get_mode(GetSSLProcAddress('SSL_CTX_get_mode'));
  SSL_set_mode := TSSL_set_mode(GetSSLProcAddress('SSL_set_mode'));
  SSL_clear_mode := TSSL_clear_mode(GetSSLProcAddress('SSL_clear_mode'));
  SSL_get_mode := TSSL_get_mode(GetSSLProcAddress('SSL_get_mode'));
  
  // Load Cipher functions
  SSL_CTX_set_cipher_list := TSSL_CTX_set_cipher_list(GetSSLProcAddress('SSL_CTX_set_cipher_list'));
  SSL_CTX_set_ciphersuites := TSSL_CTX_set_ciphersuites(GetSSLProcAddress('SSL_CTX_set_ciphersuites'));
  SSL_set_cipher_list := TSSL_set_cipher_list(GetSSLProcAddress('SSL_set_cipher_list'));
  SSL_set_ciphersuites := TSSL_set_ciphersuites(GetSSLProcAddress('SSL_set_ciphersuites'));
  SSL_get_ciphers := TSSL_get_ciphers(GetSSLProcAddress('SSL_get_ciphers'));
  SSL_get_cipher_list := TSSL_get_cipher_list(GetSSLProcAddress('SSL_get_cipher_list'));
  SSL_get_shared_ciphers := TSSL_get_shared_ciphers(GetSSLProcAddress('SSL_get_shared_ciphers'));
  SSL_get_current_cipher := TSSL_get_current_cipher(GetSSLProcAddress('SSL_get_current_cipher'));
  SSL_get_pending_cipher := TSSL_get_pending_cipher(GetSSLProcAddress('SSL_get_pending_cipher'));
  SSL_CIPHER_get_name := TSSL_CIPHER_get_name(GetSSLProcAddress('SSL_CIPHER_get_name'));
  SSL_CIPHER_get_bits := TSSL_CIPHER_get_bits(GetSSLProcAddress('SSL_CIPHER_get_bits'));
  SSL_CIPHER_get_version := TSSL_CIPHER_get_version(GetSSLProcAddress('SSL_CIPHER_get_version'));
  SSL_CIPHER_description := TSSL_CIPHER_description(GetSSLProcAddress('SSL_CIPHER_description'));
  SSL_CIPHER_get_id := TSSL_CIPHER_get_id(GetSSLProcAddress('SSL_CIPHER_get_id'));
  
  // Load SNI functions
  SSL_set_tlsext_host_name := TSSL_set_tlsext_host_name(GetSSLProcAddress('SSL_set_tlsext_host_name'));

  // If the function wasn't found (it's a macro), use our helper implementation
  if not Assigned(SSL_set_tlsext_host_name) then
    SSL_set_tlsext_host_name := @SSL_set_tlsext_host_name_impl;

  SSL_get_servername := TSSL_get_servername(GetSSLProcAddress('SSL_get_servername'));
  SSL_get_servername_type := TSSL_get_servername_type(GetSSLProcAddress('SSL_get_servername_type'));
  SSL_CTX_set_tlsext_servername_callback := TSSL_CTX_set_tlsext_servername_callback(GetSSLProcAddress('SSL_CTX_set_tlsext_servername_callback'));
  SSL_CTX_set_tlsext_servername_arg := TSSL_CTX_set_tlsext_servername_arg(GetSSLProcAddress('SSL_CTX_set_tlsext_servername_arg'));
  
  // Load ALPN functions
  SSL_CTX_set_alpn_protos := TSSL_CTX_set_alpn_protos(GetSSLProcAddress('SSL_CTX_set_alpn_protos'));
  SSL_set_alpn_protos := TSSL_set_alpn_protos(GetSSLProcAddress('SSL_set_alpn_protos'));
  SSL_CTX_set_alpn_select_cb := TSSL_CTX_set_alpn_select_cb(GetSSLProcAddress('SSL_CTX_set_alpn_select_cb'));
  SSL_get0_alpn_selected := TSSL_get0_alpn_selected(GetSSLProcAddress('SSL_get0_alpn_selected'));
  SSL_select_next_proto := TSSL_select_next_proto(GetSSLProcAddress('SSL_select_next_proto'));
  
  // Load other extension functions...
  // Many functions are optional and may not exist in older versions

  GSSLLoaded := True;
  Result := True;
end;

procedure UnloadOpenSSLSSL;
begin
  // Clear all function pointers
  SSL_CTX_set_min_proto_version := nil;
  SSL_CTX_set_max_proto_version := nil;
  // ... clear all other function pointers ...
  
  GSSLLoaded := False;
end;

function IsOpenSSLSSLLoaded: Boolean;
begin
  Result := GSSLLoaded;
end;

{ SSL_set_tlsext_host_name is a macro in OpenSSL:
  #define SSL_set_tlsext_host_name(s, name) \
      SSL_ctrl(s, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, (void *)name)
}
function SSL_set_tlsext_host_name_impl(ssl: PSSL; const name: PAnsiChar): Integer; cdecl;
const
  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
  TLSEXT_NAMETYPE_host_name = 0;
begin
  if Assigned(SSL_ctrl) then
    Result := Integer(SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME,
                              TLSEXT_NAMETYPE_host_name, Pointer(name)))
  else
    Result := 0;
end;

end.