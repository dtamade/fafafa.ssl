unit fafafa.ssl.openssl.api.provider;

interface

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.types;

type
  // Provider 类型定义
  POSSL_PROVIDER = ^OSSL_PROVIDER;
  OSSL_PROVIDER = record
    // Opaque structure
  end;

  POSSL_LIB_CTX = ^OSSL_LIB_CTX;
  OSSL_LIB_CTX = record
    // Opaque structure
  end;

  POSSL_PARAM = ^OSSL_PARAM;
  OSSL_PARAM = record
    key: PAnsiChar;
    data_type: Byte;
    data: Pointer;
    data_size: NativeUInt;
    return_size: NativeUInt;
  end;
  
  POSSL_PARAM_Array = ^TOSSL_PARAM_Array;
  TOSSL_PARAM_Array = array[0..MaxInt div SizeOf(OSSL_PARAM) - 1] of OSSL_PARAM;

  POSSL_ALGORITHM = ^OSSL_ALGORITHM;
  OSSL_ALGORITHM = record
    algorithm_names: PAnsiChar;
    property_definition: PAnsiChar;
    impl: Pointer;  // 'implementation' is a reserved keyword in Pascal
    algorithm_description: PAnsiChar;
  end;

  POSSL_DISPATCH = ^OSSL_DISPATCH;
  OSSL_DISPATCH = record
    function_id: Integer;
    fn: Pointer;
  end;

const
  // OSSL_PARAM data types
  OSSL_PARAM_END = 0;
  OSSL_PARAM_INTEGER = 1;
  OSSL_PARAM_UNSIGNED_INTEGER = 2;
  OSSL_PARAM_REAL = 3;
  OSSL_PARAM_UTF8_STRING = 4;
  OSSL_PARAM_OCTET_STRING = 5;
  OSSL_PARAM_UTF8_PTR = 6;
  OSSL_PARAM_OCTET_PTR = 7;
  
  // Provider operation bits
  OSSL_OP_DIGEST = 1;
  OSSL_OP_CIPHER = 2;
  OSSL_OP_MAC = 3;
  OSSL_OP_KDF = 4;
  OSSL_OP_RAND = 5;
  OSSL_OP_KEYMGMT = 10;
  OSSL_OP_KEYEXCH = 11;
  OSSL_OP_SIGNATURE = 12;
  OSSL_OP_ASYM_CIPHER = 13;
  OSSL_OP_KEM = 14;
  OSSL_OP_ENCODER = 20;
  OSSL_OP_DECODER = 21;
  OSSL_OP_STORE = 22;

type
  // 函数类型定义
  // Provider 管理
  TOSSL_PROVIDER_load = function(libctx: POSSL_LIB_CTX; 
    const name: PAnsiChar): POSSL_PROVIDER; cdecl;
  TOSSL_PROVIDER_try_load = function(libctx: POSSL_LIB_CTX; 
    const name: PAnsiChar; retain_fallbacks: Integer): POSSL_PROVIDER; cdecl;
  TOSSL_PROVIDER_unload = function(prov: POSSL_PROVIDER): Integer; cdecl;
  TOSSL_PROVIDER_available = function(libctx: POSSL_LIB_CTX; 
    const name: PAnsiChar): Integer; cdecl;
  TOSSL_PROVIDER_set_fallback = function(prov: POSSL_PROVIDER): Integer; cdecl;
  TOSSL_PROVIDER_do_all_cb = procedure(provider: POSSL_PROVIDER; cbdata: Pointer); cdecl;
  TOSSL_PROVIDER_do_all = function(libctx: POSSL_LIB_CTX; 
    cb: TOSSL_PROVIDER_do_all_cb; cbdata: Pointer): Integer; cdecl;
  TOSSL_PROVIDER_gettable_params = function(prov: POSSL_PROVIDER): POSSL_PARAM; cdecl;
  TOSSL_PROVIDER_get_params = function(prov: POSSL_PROVIDER; 
    params: POSSL_PARAM): Integer; cdecl;
  TOSSL_PROVIDER_query_operation = function(prov: POSSL_PROVIDER; 
    operation_id: Integer; var no_cache: Integer): POSSL_ALGORITHM; cdecl;
  TOSSL_PROVIDER_unquery_operation = procedure(prov: POSSL_PROVIDER; 
    operation_id: Integer; algs: POSSL_ALGORITHM); cdecl;
  TOSSL_PROVIDER_get_reason_strings = function(prov: POSSL_PROVIDER): Pointer; cdecl;
  TOSSL_PROVIDER_get0_dispatch = function(prov: POSSL_PROVIDER): POSSL_DISPATCH; cdecl;
  TOSSL_PROVIDER_self_test = function(prov: POSSL_PROVIDER): Integer; cdecl;
  TOSSL_PROVIDER_get_capabilities_cb = function(params: POSSL_PARAM; arg: Pointer): Integer; cdecl;
  TOSSL_PROVIDER_get_capabilities = function(prov: POSSL_PROVIDER; 
    const capability: PAnsiChar; 
    cb: TOSSL_PROVIDER_get_capabilities_cb; arg: Pointer): Integer; cdecl;
  TOSSL_PROVIDER_get0_name = function(prov: POSSL_PROVIDER): PAnsiChar; cdecl;
  
  // Library context 函数
  TOSSL_LIB_CTX_new = function(): POSSL_LIB_CTX; cdecl;
  TOSSL_LIB_CTX_new_from_dispatch = function(handle: Pointer; 
    in_: POSSL_DISPATCH): POSSL_LIB_CTX; cdecl;
  TOSSL_LIB_CTX_new_child = function(parent: POSSL_LIB_CTX; 
    in_: POSSL_DISPATCH): POSSL_LIB_CTX; cdecl;
  TOSSL_LIB_CTX_free = procedure(ctx: POSSL_LIB_CTX); cdecl;
  TOSSL_LIB_CTX_load_config = function(ctx: POSSL_LIB_CTX; 
    const config_file: PAnsiChar): Integer; cdecl;
  TOSSL_LIB_CTX_get0_global_default = function(): POSSL_LIB_CTX; cdecl;
  TOSSL_LIB_CTX_set0_default = function(libctx: POSSL_LIB_CTX): POSSL_LIB_CTX; cdecl;
  
  // OSSL_PARAM 辅助函数
  TOSSL_PARAM_construct_int = function(key: PAnsiChar; buf: PInteger): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_uint = function(key: PAnsiChar; buf: PCardinal): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_long = function(key: PAnsiChar; buf: PLongInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_ulong = function(key: PAnsiChar; buf: PLongWord): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_int32 = function(key: PAnsiChar; buf: PInt32): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_uint32 = function(key: PAnsiChar; buf: PUInt32): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_int64 = function(key: PAnsiChar; buf: PInt64): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_uint64 = function(key: PAnsiChar; buf: PUInt64): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_size_t = function(key: PAnsiChar; buf: PNativeUInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_time_t = function(key: PAnsiChar; buf: PInt64): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_double = function(key: PAnsiChar; buf: PDouble): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_utf8_string = function(key: PAnsiChar; buf: PAnsiChar; 
    bsize: NativeUInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_utf8_ptr = function(key: PAnsiChar; buf: PPAnsiChar; 
    bsize: NativeUInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_octet_string = function(key: PAnsiChar; buf: Pointer; 
    bsize: NativeUInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_octet_ptr = function(key: PAnsiChar; buf: PPointer; 
    bsize: NativeUInt): OSSL_PARAM; cdecl;
  TOSSL_PARAM_construct_end = function(): OSSL_PARAM; cdecl;

var
  // 函数指针
  OSSL_PROVIDER_load: TOSSL_PROVIDER_load = nil;
  OSSL_PROVIDER_try_load: TOSSL_PROVIDER_try_load = nil;
  OSSL_PROVIDER_unload: TOSSL_PROVIDER_unload = nil;
  OSSL_PROVIDER_available: TOSSL_PROVIDER_available = nil;
  OSSL_PROVIDER_set_fallback: TOSSL_PROVIDER_set_fallback = nil;
  OSSL_PROVIDER_do_all: TOSSL_PROVIDER_do_all = nil;
  OSSL_PROVIDER_gettable_params: TOSSL_PROVIDER_gettable_params = nil;
  OSSL_PROVIDER_get_params: TOSSL_PROVIDER_get_params = nil;
  OSSL_PROVIDER_query_operation: TOSSL_PROVIDER_query_operation = nil;
  OSSL_PROVIDER_unquery_operation: TOSSL_PROVIDER_unquery_operation = nil;
  OSSL_PROVIDER_get_reason_strings: TOSSL_PROVIDER_get_reason_strings = nil;
  OSSL_PROVIDER_get0_dispatch: TOSSL_PROVIDER_get0_dispatch = nil;
  OSSL_PROVIDER_self_test: TOSSL_PROVIDER_self_test = nil;
  OSSL_PROVIDER_get_capabilities: TOSSL_PROVIDER_get_capabilities = nil;
  OSSL_PROVIDER_get0_name: TOSSL_PROVIDER_get0_name = nil;
  OSSL_LIB_CTX_new: TOSSL_LIB_CTX_new = nil;
  OSSL_LIB_CTX_new_from_dispatch: TOSSL_LIB_CTX_new_from_dispatch = nil;
  OSSL_LIB_CTX_new_child: TOSSL_LIB_CTX_new_child = nil;
  OSSL_LIB_CTX_free: TOSSL_LIB_CTX_free = nil;
  OSSL_LIB_CTX_load_config: TOSSL_LIB_CTX_load_config = nil;
  OSSL_LIB_CTX_get0_global_default: TOSSL_LIB_CTX_get0_global_default = nil;
  OSSL_LIB_CTX_set0_default: TOSSL_LIB_CTX_set0_default = nil;
  OSSL_PARAM_construct_int: TOSSL_PARAM_construct_int = nil;
  OSSL_PARAM_construct_uint: TOSSL_PARAM_construct_uint = nil;
  OSSL_PARAM_construct_long: TOSSL_PARAM_construct_long = nil;
  OSSL_PARAM_construct_ulong: TOSSL_PARAM_construct_ulong = nil;
  OSSL_PARAM_construct_int32: TOSSL_PARAM_construct_int32 = nil;
  OSSL_PARAM_construct_uint32: TOSSL_PARAM_construct_uint32 = nil;
  OSSL_PARAM_construct_int64: TOSSL_PARAM_construct_int64 = nil;
  OSSL_PARAM_construct_uint64: TOSSL_PARAM_construct_uint64 = nil;
  OSSL_PARAM_construct_size_t: TOSSL_PARAM_construct_size_t = nil;
  OSSL_PARAM_construct_time_t: TOSSL_PARAM_construct_time_t = nil;
  OSSL_PARAM_construct_double: TOSSL_PARAM_construct_double = nil;
  OSSL_PARAM_construct_utf8_string: TOSSL_PARAM_construct_utf8_string = nil;
  OSSL_PARAM_construct_utf8_ptr: TOSSL_PARAM_construct_utf8_ptr = nil;
  OSSL_PARAM_construct_octet_string: TOSSL_PARAM_construct_octet_string = nil;
  OSSL_PARAM_construct_octet_ptr: TOSSL_PARAM_construct_octet_ptr = nil;
  OSSL_PARAM_construct_end: TOSSL_PARAM_construct_end = nil;

// 辅助函数
function LoadProvider(const Name: string; LibCtx: POSSL_LIB_CTX = nil): POSSL_PROVIDER;
function UnloadProvider(Provider: POSSL_PROVIDER): Boolean;
function IsProviderAvailable(const Name: string; LibCtx: POSSL_LIB_CTX = nil): Boolean;
function GetProviderName(Provider: POSSL_PROVIDER): string;
function CreateLibraryContext: POSSL_LIB_CTX;
procedure FreeLibraryContext(var LibCtx: POSSL_LIB_CTX);
function LoadConfigForContext(LibCtx: POSSL_LIB_CTX; const ConfigFile: string): Boolean;
function SelfTestProvider(Provider: POSSL_PROVIDER): Boolean;

// 模块加载和卸载
procedure LoadProviderModule(ALibCrypto: THandle);
procedure UnloadProviderModule;

implementation

procedure LoadProviderModule(ALibCrypto: THandle);
begin
  if ALibCrypto = 0 then Exit;
  
  // Provider functions
  OSSL_PROVIDER_load := TOSSL_PROVIDER_load(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_load'));
  OSSL_PROVIDER_try_load := TOSSL_PROVIDER_try_load(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_try_load'));
  OSSL_PROVIDER_unload := TOSSL_PROVIDER_unload(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_unload'));
  OSSL_PROVIDER_available := TOSSL_PROVIDER_available(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_available'));
  OSSL_PROVIDER_set_fallback := TOSSL_PROVIDER_set_fallback(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_set_fallback'));
  OSSL_PROVIDER_do_all := TOSSL_PROVIDER_do_all(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_do_all'));
  OSSL_PROVIDER_gettable_params := TOSSL_PROVIDER_gettable_params(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_gettable_params'));
  OSSL_PROVIDER_get_params := TOSSL_PROVIDER_get_params(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_get_params'));
  OSSL_PROVIDER_query_operation := TOSSL_PROVIDER_query_operation(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_query_operation'));
  OSSL_PROVIDER_unquery_operation := TOSSL_PROVIDER_unquery_operation(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_unquery_operation'));
  OSSL_PROVIDER_get_reason_strings := TOSSL_PROVIDER_get_reason_strings(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_get_reason_strings'));
  OSSL_PROVIDER_get0_dispatch := TOSSL_PROVIDER_get0_dispatch(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_get0_dispatch'));
  OSSL_PROVIDER_self_test := TOSSL_PROVIDER_self_test(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_self_test'));
  OSSL_PROVIDER_get_capabilities := TOSSL_PROVIDER_get_capabilities(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_get_capabilities'));
  OSSL_PROVIDER_get0_name := TOSSL_PROVIDER_get0_name(GetProcAddress(ALibCrypto, 'OSSL_PROVIDER_get0_name'));
  
  // Library context functions
  OSSL_LIB_CTX_new := TOSSL_LIB_CTX_new(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_new'));
  OSSL_LIB_CTX_new_from_dispatch := TOSSL_LIB_CTX_new_from_dispatch(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_new_from_dispatch'));
  OSSL_LIB_CTX_new_child := TOSSL_LIB_CTX_new_child(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_new_child'));
  OSSL_LIB_CTX_free := TOSSL_LIB_CTX_free(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_free'));
  OSSL_LIB_CTX_load_config := TOSSL_LIB_CTX_load_config(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_load_config'));
  OSSL_LIB_CTX_get0_global_default := TOSSL_LIB_CTX_get0_global_default(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_get0_global_default'));
  OSSL_LIB_CTX_set0_default := TOSSL_LIB_CTX_set0_default(GetProcAddress(ALibCrypto, 'OSSL_LIB_CTX_set0_default'));
  
  // OSSL_PARAM construction functions
  OSSL_PARAM_construct_int := TOSSL_PARAM_construct_int(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_int'));
  OSSL_PARAM_construct_uint := TOSSL_PARAM_construct_uint(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_uint'));
  OSSL_PARAM_construct_long := TOSSL_PARAM_construct_long(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_long'));
  OSSL_PARAM_construct_ulong := TOSSL_PARAM_construct_ulong(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_ulong'));
  OSSL_PARAM_construct_int32 := TOSSL_PARAM_construct_int32(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_int32'));
  OSSL_PARAM_construct_uint32 := TOSSL_PARAM_construct_uint32(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_uint32'));
  OSSL_PARAM_construct_int64 := TOSSL_PARAM_construct_int64(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_int64'));
  OSSL_PARAM_construct_uint64 := TOSSL_PARAM_construct_uint64(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_uint64'));
  OSSL_PARAM_construct_size_t := TOSSL_PARAM_construct_size_t(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_size_t'));
  OSSL_PARAM_construct_time_t := TOSSL_PARAM_construct_time_t(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_time_t'));
  OSSL_PARAM_construct_double := TOSSL_PARAM_construct_double(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_double'));
  OSSL_PARAM_construct_utf8_string := TOSSL_PARAM_construct_utf8_string(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_utf8_string'));
  OSSL_PARAM_construct_utf8_ptr := TOSSL_PARAM_construct_utf8_ptr(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_utf8_ptr'));
  OSSL_PARAM_construct_octet_string := TOSSL_PARAM_construct_octet_string(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_octet_string'));
  OSSL_PARAM_construct_octet_ptr := TOSSL_PARAM_construct_octet_ptr(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_octet_ptr'));
  OSSL_PARAM_construct_end := TOSSL_PARAM_construct_end(GetProcAddress(ALibCrypto, 'OSSL_PARAM_construct_end'));
end;

procedure UnloadProviderModule;
begin
  OSSL_PROVIDER_load := nil;
  OSSL_PROVIDER_try_load := nil;
  OSSL_PROVIDER_unload := nil;
  OSSL_PROVIDER_available := nil;
  OSSL_PROVIDER_set_fallback := nil;
  OSSL_PROVIDER_do_all := nil;
  OSSL_PROVIDER_gettable_params := nil;
  OSSL_PROVIDER_get_params := nil;
  OSSL_PROVIDER_query_operation := nil;
  OSSL_PROVIDER_unquery_operation := nil;
  OSSL_PROVIDER_get_reason_strings := nil;
  OSSL_PROVIDER_get0_dispatch := nil;
  OSSL_PROVIDER_self_test := nil;
  OSSL_PROVIDER_get_capabilities := nil;
  OSSL_PROVIDER_get0_name := nil;
  OSSL_LIB_CTX_new := nil;
  OSSL_LIB_CTX_new_from_dispatch := nil;
  OSSL_LIB_CTX_new_child := nil;
  OSSL_LIB_CTX_free := nil;
  OSSL_LIB_CTX_load_config := nil;
  OSSL_LIB_CTX_get0_global_default := nil;
  OSSL_LIB_CTX_set0_default := nil;
  OSSL_PARAM_construct_int := nil;
  OSSL_PARAM_construct_uint := nil;
  OSSL_PARAM_construct_long := nil;
  OSSL_PARAM_construct_ulong := nil;
  OSSL_PARAM_construct_int32 := nil;
  OSSL_PARAM_construct_uint32 := nil;
  OSSL_PARAM_construct_int64 := nil;
  OSSL_PARAM_construct_uint64 := nil;
  OSSL_PARAM_construct_size_t := nil;
  OSSL_PARAM_construct_time_t := nil;
  OSSL_PARAM_construct_double := nil;
  OSSL_PARAM_construct_utf8_string := nil;
  OSSL_PARAM_construct_utf8_ptr := nil;
  OSSL_PARAM_construct_octet_string := nil;
  OSSL_PARAM_construct_octet_ptr := nil;
  OSSL_PARAM_construct_end := nil;
end;

// 辅助函数实现
function LoadProvider(const Name: string; LibCtx: POSSL_LIB_CTX): POSSL_PROVIDER;
var
  NameBytes: TBytes;
begin
  Result := nil;
  if not Assigned(OSSL_PROVIDER_load) then Exit;
  
  NameBytes := TEncoding.UTF8.GetBytes(Name);
  Result := OSSL_PROVIDER_load(LibCtx, PAnsiChar(NameBytes));
end;

function UnloadProvider(Provider: POSSL_PROVIDER): Boolean;
begin
  Result := False;
  if not Assigned(OSSL_PROVIDER_unload) or not Assigned(Provider) then Exit;
  
  Result := OSSL_PROVIDER_unload(Provider) = 1;
end;

function IsProviderAvailable(const Name: string; LibCtx: POSSL_LIB_CTX): Boolean;
var
  NameBytes: TBytes;
begin
  Result := False;
  if not Assigned(OSSL_PROVIDER_available) then Exit;
  
  NameBytes := TEncoding.UTF8.GetBytes(Name);
  Result := OSSL_PROVIDER_available(LibCtx, PAnsiChar(NameBytes)) = 1;
end;

function GetProviderName(Provider: POSSL_PROVIDER): string;
var
  Name: PAnsiChar;
begin
  Result := '';
  if not Assigned(OSSL_PROVIDER_get0_name) or not Assigned(Provider) then Exit;
  
  Name := OSSL_PROVIDER_get0_name(Provider);
  if Assigned(Name) then
    Result := string(Name);
end;

function CreateLibraryContext: POSSL_LIB_CTX;
begin
  Result := nil;
  if not Assigned(OSSL_LIB_CTX_new) then Exit;
  
  Result := OSSL_LIB_CTX_new();
end;

procedure FreeLibraryContext(var LibCtx: POSSL_LIB_CTX);
begin
  if Assigned(LibCtx) and Assigned(OSSL_LIB_CTX_free) then
  begin
    OSSL_LIB_CTX_free(LibCtx);
    LibCtx := nil;
  end;
end;

function LoadConfigForContext(LibCtx: POSSL_LIB_CTX; const ConfigFile: string): Boolean;
var
  FileBytes: TBytes;
begin
  Result := False;
  if not Assigned(OSSL_LIB_CTX_load_config) or not Assigned(LibCtx) then Exit;
  
  FileBytes := TEncoding.UTF8.GetBytes(ConfigFile);
  Result := OSSL_LIB_CTX_load_config(LibCtx, PAnsiChar(FileBytes)) = 1;
end;

function SelfTestProvider(Provider: POSSL_PROVIDER): Boolean;
begin
  Result := False;
  if not Assigned(OSSL_PROVIDER_self_test) or not Assigned(Provider) then Exit;
  
  Result := OSSL_PROVIDER_self_test(Provider) = 1;
end;

end.