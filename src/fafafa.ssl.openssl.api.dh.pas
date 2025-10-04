unit fafafa.ssl.openssl.api.dh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.bn;

const
  { DH flags }
  DH_FLAG_CACHE_MONT_P       = $01;
  DH_FLAG_NO_EXP_CONSTTIME   = $00;
  DH_FLAG_FIPS_METHOD        = $0400;
  DH_FLAG_NON_FIPS_ALLOW     = $0400;
  
  { DH check flags }
  DH_CHECK_P_NOT_PRIME       = $01;
  DH_CHECK_P_NOT_SAFE_PRIME  = $02;
  DH_UNABLE_TO_CHECK_GENERATOR = $04;
  DH_NOT_SUITABLE_GENERATOR  = $08;
  DH_CHECK_Q_NOT_PRIME       = $10;
  DH_CHECK_INVALID_Q_VALUE   = $20;
  DH_CHECK_INVALID_J_VALUE   = $40;
  DH_CHECK_PUBKEY_TOO_SMALL  = $01;
  DH_CHECK_PUBKEY_TOO_LARGE  = $02;
  DH_CHECK_PUBKEY_INVALID    = $04;
  DH_CHECK_P_NOT_STRONG_PRIME = DH_CHECK_P_NOT_SAFE_PRIME;
  
  { DH_check_pub_key flags }
  DH_R_PUBKEY_TOO_SMALL      = DH_CHECK_PUBKEY_TOO_SMALL;
  DH_R_PUBKEY_TOO_LARGE      = DH_CHECK_PUBKEY_TOO_LARGE;
  DH_R_PUBKEY_INVALID        = DH_CHECK_PUBKEY_INVALID;
  
  { DH generator }
  DH_GENERATOR_2             = 2;
  DH_GENERATOR_5             = 5;

type
  { DH_METHOD structure }
  DH_METHOD = record
    name: PAnsiChar;
    generate_key: function(dh: PDH): Integer; cdecl;
    compute_key: function(key: PByte; const pub_key: PBIGNUM; dh: PDH): Integer; cdecl;
    bn_mod_exp: function(const dh: PDH; r: PBIGNUM; const a: PBIGNUM; const p: PBIGNUM; const m: PBIGNUM; ctx: PBN_CTX; m_ctx: PBN_MONT_CTX): Integer; cdecl;
    init: function(dh: PDH): Integer; cdecl;
    finish: function(dh: PDH): Integer; cdecl;
    flags: Integer;
    app_data: PAnsiChar;
    generate_params: function(dh: PDH; prime_len: Integer; generator: Integer; cb: PBN_GENCB): Integer; cdecl;
  end;
  PDH_METHOD = ^DH_METHOD;

  { DH function types }
  TDH_new = function: PDH; cdecl;
  TDH_new_method = function(engine: PENGINE): PDH; cdecl;
  TDH_free = procedure(dh: PDH); cdecl;
  TDH_up_ref = function(dh: PDH): Integer; cdecl;
  TDH_bits = function(const dh: PDH): Integer; cdecl;
  TDH_size = function(const dh: PDH): Integer; cdecl;
  TDH_security_bits = function(const dh: PDH): Integer; cdecl;
  
  { DH parameter/key manipulation }
  TDH_set0_pqg = function(dh: PDH; p: PBIGNUM; q: PBIGNUM; g: PBIGNUM): Integer; cdecl;
  TDH_get0_pqg = procedure(const dh: PDH; const p: PPBIGNUM; const q: PPBIGNUM; const g: PPBIGNUM); cdecl;
  TDH_set0_key = function(dh: PDH; pub_key: PBIGNUM; priv_key: PBIGNUM): Integer; cdecl;
  TDH_get0_key = procedure(const dh: PDH; const pub_key: PPBIGNUM; const priv_key: PPBIGNUM); cdecl;
  TDH_get0_p = function(const dh: PDH): PBIGNUM; cdecl;
  TDH_get0_q = function(const dh: PDH): PBIGNUM; cdecl;
  TDH_get0_g = function(const dh: PDH): PBIGNUM; cdecl;
  TDH_get0_priv_key = function(const dh: PDH): PBIGNUM; cdecl;
  TDH_get0_pub_key = function(const dh: PDH): PBIGNUM; cdecl;
  TDH_clear_flags = procedure(dh: PDH; flags: Integer); cdecl;
  TDH_test_flags = function(const dh: PDH; flags: Integer): Integer; cdecl;
  TDH_set_flags = procedure(dh: PDH; flags: Integer); cdecl;
  TDH_get0_engine = function(d: PDH): PENGINE; cdecl;
  TDH_get_length = function(const dh: PDH): clong; cdecl;
  TDH_set_length = function(dh: PDH; length: clong): Integer; cdecl;
  
  { DH parameter generation }
  TDH_generate_parameters = function(prime_len: Integer; generator: Integer; callback: Pointer; cb_arg: Pointer): PDH; cdecl;
  TDH_generate_parameters_ex = function(dh: PDH; prime_len: Integer; generator: Integer; cb: PBN_GENCB): Integer; cdecl;
  
  { DH key generation and computation }
  TDH_generate_key = function(dh: PDH): Integer; cdecl;
  TDH_compute_key = function(key: PByte; const pub_key: PBIGNUM; dh: PDH): Integer; cdecl;
  TDH_compute_key_padded = function(key: PByte; const pub_key: PBIGNUM; dh: PDH): Integer; cdecl;
  
  { DH checking }
  TDH_check = function(const dh: PDH; codes: PInteger): Integer; cdecl;
  TDH_check_ex = function(const dh: PDH): Integer; cdecl;
  TDH_check_pub_key = function(const dh: PDH; const pub_key: PBIGNUM; codes: PInteger): Integer; cdecl;
  TDH_check_pub_key_ex = function(const dh: PDH; const pub_key: PBIGNUM): Integer; cdecl;
  TDH_check_params = function(const dh: PDH): Integer; cdecl;
  TDH_check_params_ex = function(const dh: PDH): Integer; cdecl;
  
  { DH standard parameters }
  TDH_get_1024_160 = function: PDH; cdecl;
  TDH_get_2048_224 = function: PDH; cdecl;
  TDH_get_2048_256 = function: PDH; cdecl;
  
  { DH KDF functions }
  TDH_KDF_X9_42 = function(&out: PByte; outlen: size_t; const Z: PByte; Zlen: size_t; key_oid: PASN1_OBJECT; const ukm: PByte; ukmlen: size_t; const md: PEVP_MD): Integer; cdecl;
  
  { DH ASN1 functions }
  TDHparams_dup = function(const dh: PDH): PDH; cdecl;
  Ti2d_DHparams = function(const a: PDH; pp: PPByte): Integer; cdecl;
  Td2i_DHparams = function(a: PPDH; const pp: PPByte; length: clong): PDH; cdecl;
  Ti2d_DHxparams = function(const a: PDH; pp: PPByte): Integer; cdecl;
  Td2i_DHxparams = function(a: PPDH; const pp: PPByte; length: clong): PDH; cdecl;
  
  { DH print functions }
  TDHparams_print_fp = function(fp: Pointer; const x: PDH): Integer; cdecl;
  TDHparams_print = function(bp: PBIO; const x: PDH): Integer; cdecl;
  
  { DH method functions }
  TDH_meth_new = function(const name: PAnsiChar; flags: Integer): PDH_METHOD; cdecl;
  TDH_meth_free = procedure(dhm: PDH_METHOD); cdecl;
  TDH_meth_dup = function(const dhm: PDH_METHOD): PDH_METHOD; cdecl;
  TDH_meth_get0_name = function(const dhm: PDH_METHOD): PAnsiChar; cdecl;
  TDH_meth_set1_name = function(dhm: PDH_METHOD; const name: PAnsiChar): Integer; cdecl;
  TDH_meth_get_flags = function(const dhm: PDH_METHOD): Integer; cdecl;
  TDH_meth_set_flags = function(dhm: PDH_METHOD; flags: Integer): Integer; cdecl;
  TDH_meth_get0_app_data = function(const dhm: PDH_METHOD): Pointer; cdecl;
  TDH_meth_set0_app_data = function(dhm: PDH_METHOD; app_data: Pointer): Integer; cdecl;
  TDH_get_default_method = function: PDH_METHOD; cdecl;
  TDH_set_default_method = procedure(const meth: PDH_METHOD); cdecl;
  TDH_OpenSSL = function: PDH_METHOD; cdecl;
  TDH_set_method = function(dh: PDH; const meth: PDH_METHOD): Integer; cdecl;
  TDH_new_by_nid = function(nid: Integer): PDH; cdecl;
  TDH_get_nid = function(const dh: PDH): Integer; cdecl;

var
  { DH functions }
  DH_new: TDH_new;
  DH_new_method: TDH_new_method;
  DH_free: TDH_free;
  DH_up_ref: TDH_up_ref;
  DH_bits: TDH_bits;
  DH_size: TDH_size;
  DH_security_bits: TDH_security_bits;
  
  { DH parameter/key manipulation }
  DH_set0_pqg: TDH_set0_pqg;
  DH_get0_pqg: TDH_get0_pqg;
  DH_set0_key: TDH_set0_key;
  DH_get0_key: TDH_get0_key;
  DH_get0_p: TDH_get0_p;
  DH_get0_q: TDH_get0_q;
  DH_get0_g: TDH_get0_g;
  DH_get0_priv_key: TDH_get0_priv_key;
  DH_get0_pub_key: TDH_get0_pub_key;
  DH_clear_flags: TDH_clear_flags;
  DH_test_flags: TDH_test_flags;
  DH_set_flags: TDH_set_flags;
  DH_get0_engine: TDH_get0_engine;
  DH_get_length: TDH_get_length;
  DH_set_length: TDH_set_length;
  
  { DH parameter generation }
  DH_generate_parameters: TDH_generate_parameters;
  DH_generate_parameters_ex: TDH_generate_parameters_ex;
  
  { DH key generation and computation }
  DH_generate_key: TDH_generate_key;
  DH_compute_key: TDH_compute_key;
  DH_compute_key_padded: TDH_compute_key_padded;
  
  { DH checking }
  DH_check: TDH_check;
  DH_check_ex: TDH_check_ex;
  DH_check_pub_key: TDH_check_pub_key;
  DH_check_pub_key_ex: TDH_check_pub_key_ex;
  DH_check_params: TDH_check_params;
  DH_check_params_ex: TDH_check_params_ex;
  
  { DH standard parameters }
  DH_get_1024_160: TDH_get_1024_160;
  DH_get_2048_224: TDH_get_2048_224;
  DH_get_2048_256: TDH_get_2048_256;
  
  { DH KDF functions }
  DH_KDF_X9_42: TDH_KDF_X9_42;
  
  { DH ASN1 functions }
  DHparams_dup: TDHparams_dup;
  i2d_DHparams: Ti2d_DHparams;
  d2i_DHparams: Td2i_DHparams;
  i2d_DHxparams: Ti2d_DHxparams;
  d2i_DHxparams: Td2i_DHxparams;
  
  { DH print functions }
  DHparams_print_fp: TDHparams_print_fp;
  DHparams_print: TDHparams_print;
  
  { DH method functions }
  DH_meth_new: TDH_meth_new;
  DH_meth_free: TDH_meth_free;
  DH_meth_dup: TDH_meth_dup;
  DH_meth_get0_name: TDH_meth_get0_name;
  DH_meth_set1_name: TDH_meth_set1_name;
  DH_meth_get_flags: TDH_meth_get_flags;
  DH_meth_set_flags: TDH_meth_set_flags;
  DH_meth_get0_app_data: TDH_meth_get0_app_data;
  DH_meth_set0_app_data: TDH_meth_set0_app_data;
  DH_get_default_method: TDH_get_default_method;
  DH_set_default_method: TDH_set_default_method;
  DH_OpenSSL: TDH_OpenSSL;
  DH_set_method: TDH_set_method;
  DH_new_by_nid: TDH_new_by_nid;
  DH_get_nid: TDH_get_nid;

function LoadOpenSSLDH: Boolean;
procedure UnloadOpenSSLDH;
function IsOpenSSLDHLoaded: Boolean;

implementation

uses
  fafafa.ssl.openssl.api.core;

var
  GDHLoaded: Boolean = False;

function LoadOpenSSLDH: Boolean;
var
  LLib: TLibHandle;
begin
  if GDHLoaded then
    Exit(True);
    
  // Use the crypto library handle from core module
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    // Try to load core first
    LoadOpenSSLCore;
    LLib := GetCryptoLibHandle;
  end;
    
  if LLib = NilHandle then
    Exit(False);
    
  // Load DH basic functions
  DH_new := TDH_new(GetProcAddress(LLib, 'DH_new'));
  DH_new_method := TDH_new_method(GetProcAddress(LLib, 'DH_new_method'));
  DH_free := TDH_free(GetProcAddress(LLib, 'DH_free'));
  DH_up_ref := TDH_up_ref(GetProcAddress(LLib, 'DH_up_ref'));
  DH_bits := TDH_bits(GetProcAddress(LLib, 'DH_bits'));
  DH_size := TDH_size(GetProcAddress(LLib, 'DH_size'));
  DH_security_bits := TDH_security_bits(GetProcAddress(LLib, 'DH_security_bits'));
  
  // Load DH parameter/key manipulation functions
  DH_set0_pqg := TDH_set0_pqg(GetProcAddress(LLib, 'DH_set0_pqg'));
  DH_get0_pqg := TDH_get0_pqg(GetProcAddress(LLib, 'DH_get0_pqg'));
  DH_set0_key := TDH_set0_key(GetProcAddress(LLib, 'DH_set0_key'));
  DH_get0_key := TDH_get0_key(GetProcAddress(LLib, 'DH_get0_key'));
  DH_get0_p := TDH_get0_p(GetProcAddress(LLib, 'DH_get0_p'));
  DH_get0_q := TDH_get0_q(GetProcAddress(LLib, 'DH_get0_q'));
  DH_get0_g := TDH_get0_g(GetProcAddress(LLib, 'DH_get0_g'));
  DH_get0_priv_key := TDH_get0_priv_key(GetProcAddress(LLib, 'DH_get0_priv_key'));
  DH_get0_pub_key := TDH_get0_pub_key(GetProcAddress(LLib, 'DH_get0_pub_key'));
  DH_clear_flags := TDH_clear_flags(GetProcAddress(LLib, 'DH_clear_flags'));
  DH_test_flags := TDH_test_flags(GetProcAddress(LLib, 'DH_test_flags'));
  DH_set_flags := TDH_set_flags(GetProcAddress(LLib, 'DH_set_flags'));
  DH_get0_engine := TDH_get0_engine(GetProcAddress(LLib, 'DH_get0_engine'));
  DH_get_length := TDH_get_length(GetProcAddress(LLib, 'DH_get_length'));
  DH_set_length := TDH_set_length(GetProcAddress(LLib, 'DH_set_length'));
  
  // Load DH parameter generation functions
  DH_generate_parameters := TDH_generate_parameters(GetProcAddress(LLib, 'DH_generate_parameters'));
  DH_generate_parameters_ex := TDH_generate_parameters_ex(GetProcAddress(LLib, 'DH_generate_parameters_ex'));
  
  // Load DH key generation and computation functions
  DH_generate_key := TDH_generate_key(GetProcAddress(LLib, 'DH_generate_key'));
  DH_compute_key := TDH_compute_key(GetProcAddress(LLib, 'DH_compute_key'));
  DH_compute_key_padded := TDH_compute_key_padded(GetProcAddress(LLib, 'DH_compute_key_padded'));
  
  // Load DH checking functions
  DH_check := TDH_check(GetProcAddress(LLib, 'DH_check'));
  DH_check_ex := TDH_check_ex(GetProcAddress(LLib, 'DH_check_ex'));
  DH_check_pub_key := TDH_check_pub_key(GetProcAddress(LLib, 'DH_check_pub_key'));
  DH_check_pub_key_ex := TDH_check_pub_key_ex(GetProcAddress(LLib, 'DH_check_pub_key_ex'));
  DH_check_params := TDH_check_params(GetProcAddress(LLib, 'DH_check_params'));
  DH_check_params_ex := TDH_check_params_ex(GetProcAddress(LLib, 'DH_check_params_ex'));
  
  // Load DH standard parameters functions
  DH_get_1024_160 := TDH_get_1024_160(GetProcAddress(LLib, 'DH_get_1024_160'));
  DH_get_2048_224 := TDH_get_2048_224(GetProcAddress(LLib, 'DH_get_2048_224'));
  DH_get_2048_256 := TDH_get_2048_256(GetProcAddress(LLib, 'DH_get_2048_256'));
  
  // Load DH KDF functions
  DH_KDF_X9_42 := TDH_KDF_X9_42(GetProcAddress(LLib, 'DH_KDF_X9_42'));
  
  // Load DH ASN1 functions
  DHparams_dup := TDHparams_dup(GetProcAddress(LLib, 'DHparams_dup'));
  i2d_DHparams := Ti2d_DHparams(GetProcAddress(LLib, 'i2d_DHparams'));
  d2i_DHparams := Td2i_DHparams(GetProcAddress(LLib, 'd2i_DHparams'));
  i2d_DHxparams := Ti2d_DHxparams(GetProcAddress(LLib, 'i2d_DHxparams'));
  d2i_DHxparams := Td2i_DHxparams(GetProcAddress(LLib, 'd2i_DHxparams'));
  
  // Load DH print functions
  DHparams_print_fp := TDHparams_print_fp(GetProcAddress(LLib, 'DHparams_print_fp'));
  DHparams_print := TDHparams_print(GetProcAddress(LLib, 'DHparams_print'));
  
  // Load DH method functions
  DH_meth_new := TDH_meth_new(GetProcAddress(LLib, 'DH_meth_new'));
  DH_meth_free := TDH_meth_free(GetProcAddress(LLib, 'DH_meth_free'));
  DH_meth_dup := TDH_meth_dup(GetProcAddress(LLib, 'DH_meth_dup'));
  DH_meth_get0_name := TDH_meth_get0_name(GetProcAddress(LLib, 'DH_meth_get0_name'));
  DH_meth_set1_name := TDH_meth_set1_name(GetProcAddress(LLib, 'DH_meth_set1_name'));
  DH_meth_get_flags := TDH_meth_get_flags(GetProcAddress(LLib, 'DH_meth_get_flags'));
  DH_meth_set_flags := TDH_meth_set_flags(GetProcAddress(LLib, 'DH_meth_set_flags'));
  DH_meth_get0_app_data := TDH_meth_get0_app_data(GetProcAddress(LLib, 'DH_meth_get0_app_data'));
  DH_meth_set0_app_data := TDH_meth_set0_app_data(GetProcAddress(LLib, 'DH_meth_set0_app_data'));
  DH_get_default_method := TDH_get_default_method(GetProcAddress(LLib, 'DH_get_default_method'));
  DH_set_default_method := TDH_set_default_method(GetProcAddress(LLib, 'DH_set_default_method'));
  DH_OpenSSL := TDH_OpenSSL(GetProcAddress(LLib, 'DH_OpenSSL'));
  DH_set_method := TDH_set_method(GetProcAddress(LLib, 'DH_set_method'));
  DH_new_by_nid := TDH_new_by_nid(GetProcAddress(LLib, 'DH_new_by_nid'));
  DH_get_nid := TDH_get_nid(GetProcAddress(LLib, 'DH_get_nid'));
  
  // Check if critical functions are loaded
  GDHLoaded := Assigned(DH_new) and Assigned(DH_free) and
               Assigned(DH_generate_key) and Assigned(DH_compute_key);
  Result := GDHLoaded;
end;

procedure UnloadOpenSSLDH;
begin
  // Clear DH basic function pointers
  DH_new := nil;
  DH_new_method := nil;
  DH_free := nil;
  DH_up_ref := nil;
  DH_bits := nil;
  DH_size := nil;
  DH_security_bits := nil;
  
  // Clear DH parameter/key manipulation function pointers
  DH_set0_pqg := nil;
  DH_get0_pqg := nil;
  DH_set0_key := nil;
  DH_get0_key := nil;
  DH_get0_p := nil;
  DH_get0_q := nil;
  DH_get0_g := nil;
  DH_get0_priv_key := nil;
  DH_get0_pub_key := nil;
  DH_clear_flags := nil;
  DH_test_flags := nil;
  DH_set_flags := nil;
  DH_get0_engine := nil;
  DH_get_length := nil;
  DH_set_length := nil;
  
  // Clear DH parameter generation function pointers
  DH_generate_parameters := nil;
  DH_generate_parameters_ex := nil;
  
  // Clear DH key generation and computation function pointers
  DH_generate_key := nil;
  DH_compute_key := nil;
  DH_compute_key_padded := nil;
  
  // Clear DH checking function pointers
  DH_check := nil;
  DH_check_ex := nil;
  DH_check_pub_key := nil;
  DH_check_pub_key_ex := nil;
  DH_check_params := nil;
  DH_check_params_ex := nil;
  
  // Clear DH standard parameters function pointers
  DH_get_1024_160 := nil;
  DH_get_2048_224 := nil;
  DH_get_2048_256 := nil;
  
  // Clear DH KDF function pointers
  DH_KDF_X9_42 := nil;
  
  // Clear DH ASN1 function pointers
  DHparams_dup := nil;
  i2d_DHparams := nil;
  d2i_DHparams := nil;
  i2d_DHxparams := nil;
  d2i_DHxparams := nil;
  
  // Clear DH print function pointers
  DHparams_print_fp := nil;
  DHparams_print := nil;
  
  // Clear DH method function pointers
  DH_meth_new := nil;
  DH_meth_free := nil;
  DH_meth_dup := nil;
  DH_meth_get0_name := nil;
  DH_meth_set1_name := nil;
  DH_meth_get_flags := nil;
  DH_meth_set_flags := nil;
  DH_meth_get0_app_data := nil;
  DH_meth_set0_app_data := nil;
  DH_get_default_method := nil;
  DH_set_default_method := nil;
  DH_OpenSSL := nil;
  DH_set_method := nil;
  DH_new_by_nid := nil;
  DH_get_nid := nil;
  
  GDHLoaded := False;
end;

function IsOpenSSLDHLoaded: Boolean;
begin
  Result := GDHLoaded;
end;

end.