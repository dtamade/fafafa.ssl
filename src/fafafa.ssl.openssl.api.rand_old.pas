unit fafafa.ssl.openssl.api.rand_old;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types;

const
  { RAND seed sources }
  RAND_DRBG_TYPE_NONE     = 0;
  RAND_DRBG_TYPE_CTR      = 1;
  RAND_DRBG_TYPE_HMAC     = 2;
  RAND_DRBG_TYPE_HASH     = 3;
  
  { RAND flags }
  RAND_DRBG_FLAG_PUBLIC              = $00;
  RAND_DRBG_FLAG_PRIVATE             = $01;
  RAND_DRBG_FLAG_MASTER              = $02;
  RAND_DRBG_FLAG_CTR_NO_DF           = $04;
  RAND_DRBG_FLAG_HMAC                = $08;
  
  { RAND pool constants }
  RAND_POOL_MIN_ALLOCATION = 256;
  RAND_POOL_MAX_ALLOCATION = 8192;
  RAND_POOL_MIN_LENGTH     = 32;
  RAND_POOL_MAX_LENGTH     = 1024 * 1024;  // 1MB
  
  { RAND DRBG constants }
  RAND_DRBG_STRENGTH       = 256;
  RAND_DRBG_MAX_LENGTH     = 1 shl 20;
  RAND_DRBG_RESEED_INTERVAL = 1 shl 24;
  RAND_DRBG_RESEED_TIME    = 60 * 60;  // 1 hour
  RAND_DRBG_RESEED_REQUESTS = 1 shl 12;

type
  { Forward declarations }
  PRAND_METHOD = ^RAND_METHOD;
  PRAND_DRBG = Pointer;
  PPRAND_DRBG = ^PRAND_DRBG;
  PRAND_POOL = Pointer;
  
  { RAND_METHOD structure }
  RAND_METHOD = record
    seed: function(const buf: Pointer; num: Integer): Integer; cdecl;
    bytes: function(buf: PByte; num: Integer): Integer; cdecl;
    cleanup: procedure; cdecl;
    add: function(const buf: Pointer; num: Integer; randomness: Double): Integer; cdecl;
    pseudorand: function(buf: PByte; num: Integer): Integer; cdecl;
    status: function: Integer; cdecl;
  end;

  { RAND function types }
  TRAND_set_rand_method = function(const meth: PRAND_METHOD): Integer; cdecl;
  TRAND_get_rand_method = function: PRAND_METHOD; cdecl;
  TRAND_SSLeay = function: PRAND_METHOD; cdecl;
  TRAND_OpenSSL = function: PRAND_METHOD; cdecl;
  
  { Random number generation }
  TRAND_bytes = function(buf: PByte; num: Integer): Integer; cdecl;
  TRAND_bytes_ex = function(ctx: Pointer; buf: PByte; num: size_t; strength: Cardinal): Integer; cdecl;
  TRAND_pseudo_bytes = function(buf: PByte; num: Integer): Integer; cdecl;
  TRAND_priv_bytes = function(buf: PByte; num: Integer): Integer; cdecl;
  TRAND_priv_bytes_ex = function(ctx: Pointer; buf: PByte; num: size_t; strength: Cardinal): Integer; cdecl;
  
  { Seeding }
  TRAND_seed = procedure(const buf: Pointer; num: Integer); cdecl;
  TRAND_keep_random_devices_open = procedure(keep: Integer); cdecl;
  TRAND_add = procedure(const buf: Pointer; num: Integer; randomness: Double); cdecl;
  TRAND_load_file = function(const &file: PAnsiChar; max_bytes: clong): Integer; cdecl;
  TRAND_write_file = function(const &file: PAnsiChar): Integer; cdecl;
  TRAND_file_name = function(buf: PAnsiChar; num: size_t): PAnsiChar; cdecl;
  TRAND_status = function: Integer; cdecl;
  TRAND_query_egd_bytes = function(const path: PAnsiChar; buf: PByte; bytes: Integer): Integer; cdecl;
  TRAND_egd = function(const path: PAnsiChar): Integer; cdecl;
  TRAND_egd_bytes = function(const path: PAnsiChar; buf: PByte; bytes: Integer): Integer; cdecl;
  TRAND_poll = function: Integer; cdecl;
  
  { Screen interaction (Windows specific) }
  TRAND_screen = procedure; cdecl;
  TRAND_event = function(h: Cardinal; msg: Cardinal; wp: Cardinal; lp: clong): Integer; cdecl;
  
  { DRBG (Deterministic Random Bit Generator) - OpenSSL 1.1.1+ }
  TRAND_DRBG_new = function(&type: Integer; flags: Cardinal; parent: PRAND_DRBG): PRAND_DRBG; cdecl;
  TRAND_DRBG_new_ex = function(ctx: Pointer; &type: Integer; flags: Cardinal; parent: PRAND_DRBG): PRAND_DRBG; cdecl;
  TRAND_DRBG_secure_new = function(&type: Integer; flags: Cardinal; parent: PRAND_DRBG): PRAND_DRBG; cdecl;
  TRAND_DRBG_secure_new_ex = function(ctx: Pointer; &type: Integer; flags: Cardinal; parent: PRAND_DRBG): PRAND_DRBG; cdecl;
  TRAND_DRBG_set = function(drbg: PRAND_DRBG; &type: Integer; flags: Cardinal): Integer; cdecl;
  TRAND_DRBG_set_defaults = function(&type: Integer; flags: Cardinal): Integer; cdecl;
  TRAND_DRBG_instantiate = function(drbg: PRAND_DRBG; const pers: PByte; perslen: size_t): Integer; cdecl;
  TRAND_DRBG_uninstantiate = function(drbg: PRAND_DRBG): Integer; cdecl;
  TRAND_DRBG_free = procedure(drbg: PRAND_DRBG); cdecl;
  TRAND_DRBG_reseed = function(drbg: PRAND_DRBG; const adin: PByte; adinlen: size_t; prediction_resistance: Integer): Integer; cdecl;
  TRAND_DRBG_generate = function(drbg: PRAND_DRBG; outbuf: PByte; outlen: size_t; prediction_resistance: Integer; const adin: PByte; adinlen: size_t): Integer; cdecl;
  TRAND_DRBG_bytes = function(drbg: PRAND_DRBG; outbuf: PByte; outlen: size_t): Integer; cdecl;
  TRAND_DRBG_get0_master = function: PRAND_DRBG; cdecl;
  TRAND_DRBG_get0_public = function: PRAND_DRBG; cdecl;
  TRAND_DRBG_get0_private = function: PRAND_DRBG; cdecl;
  TRAND_DRBG_set_reseed_interval = function(drbg: PRAND_DRBG; interval: Cardinal): Integer; cdecl;
  TRAND_DRBG_set_reseed_time_interval = function(drbg: PRAND_DRBG; interval: time_t): Integer; cdecl;
  TRAND_DRBG_set_reseed_defaults = function(master_reseed_interval: Cardinal; slave_reseed_interval: Cardinal; master_reseed_time: time_t; slave_reseed_time: time_t): Integer; cdecl;
  TRAND_DRBG_set_callbacks = function(drbg: PRAND_DRBG; get_entropy: Pointer; cleanup_entropy: Pointer; get_nonce: Pointer; cleanup_nonce: Pointer): Integer; cdecl;
  TRAND_DRBG_set_ex_data = function(drbg: PRAND_DRBG; idx: Integer; data: Pointer): Integer; cdecl;
  TRAND_DRBG_get_ex_data = function(const drbg: PRAND_DRBG; idx: Integer): Pointer; cdecl;
  TRAND_DRBG_get_ex_new_index = function(argl: clong; argp: Pointer; new_func: Pointer; dup_func: Pointer; free_func: Pointer): Integer; cdecl;
  
  { RAND pool functions - OpenSSL 1.1.1+ }
  TRAND_POOL_new = function(entropy_requested: size_t; secure: Integer; min_len: size_t; max_len: size_t): PRAND_POOL; cdecl;
  TRAND_POOL_free = procedure(pool: PRAND_POOL); cdecl;
  TRAND_POOL_entropy = function(pool: PRAND_POOL): size_t; cdecl;
  TRAND_POOL_length = function(pool: PRAND_POOL): size_t; cdecl;
  TRAND_POOL_entropy_available = function(pool: PRAND_POOL): size_t; cdecl;
  TRAND_POOL_entropy_needed = function(pool: PRAND_POOL): size_t; cdecl;
  TRAND_POOL_bytes_needed = function(pool: PRAND_POOL; entropy_factor: Cardinal): size_t; cdecl;
  TRAND_POOL_bytes_remaining = function(pool: PRAND_POOL): size_t; cdecl;
  TRAND_POOL_add = function(pool: PRAND_POOL; const buffer: PByte; len: size_t; entropy: size_t): Integer; cdecl;
  TRAND_POOL_add_begin = function(pool: PRAND_POOL; len: size_t): PByte; cdecl;
  TRAND_POOL_add_end = function(pool: PRAND_POOL; len: size_t; entropy: size_t): Integer; cdecl;
  TRAND_POOL_acquire_entropy = function(pool: PRAND_POOL): size_t; cdecl;

var
  { RAND method functions }
  RAND_set_rand_method: TRAND_set_rand_method;
  RAND_get_rand_method: TRAND_get_rand_method;
  RAND_SSLeay: TRAND_SSLeay;
  RAND_OpenSSL: TRAND_OpenSSL;
  
  { Random number generation }
  RAND_bytes: TRAND_bytes;
  RAND_bytes_ex: TRAND_bytes_ex;
  RAND_pseudo_bytes: TRAND_pseudo_bytes;
  RAND_priv_bytes: TRAND_priv_bytes;
  RAND_priv_bytes_ex: TRAND_priv_bytes_ex;
  
  { Seeding }
  RAND_seed: TRAND_seed;
  RAND_keep_random_devices_open: TRAND_keep_random_devices_open;
  RAND_add: TRAND_add;
  RAND_load_file: TRAND_load_file;
  RAND_write_file: TRAND_write_file;
  RAND_file_name: TRAND_file_name;
  RAND_status: TRAND_status;
  RAND_query_egd_bytes: TRAND_query_egd_bytes;
  RAND_egd: TRAND_egd;
  RAND_egd_bytes: TRAND_egd_bytes;
  RAND_poll: TRAND_poll;
  
  { Screen interaction }
  RAND_screen: TRAND_screen;
  RAND_event: TRAND_event;
  
  { DRBG functions }
  RAND_DRBG_new: TRAND_DRBG_new;
  RAND_DRBG_new_ex: TRAND_DRBG_new_ex;
  RAND_DRBG_secure_new: TRAND_DRBG_secure_new;
  RAND_DRBG_secure_new_ex: TRAND_DRBG_secure_new_ex;
  RAND_DRBG_set: TRAND_DRBG_set;
  RAND_DRBG_set_defaults: TRAND_DRBG_set_defaults;
  RAND_DRBG_instantiate: TRAND_DRBG_instantiate;
  RAND_DRBG_uninstantiate: TRAND_DRBG_uninstantiate;
  RAND_DRBG_free: TRAND_DRBG_free;
  RAND_DRBG_reseed: TRAND_DRBG_reseed;
  RAND_DRBG_generate: TRAND_DRBG_generate;
  RAND_DRBG_bytes: TRAND_DRBG_bytes;
  RAND_DRBG_get0_master: TRAND_DRBG_get0_master;
  RAND_DRBG_get0_public: TRAND_DRBG_get0_public;
  RAND_DRBG_get0_private: TRAND_DRBG_get0_private;
  RAND_DRBG_set_reseed_interval: TRAND_DRBG_set_reseed_interval;
  RAND_DRBG_set_reseed_time_interval: TRAND_DRBG_set_reseed_time_interval;
  RAND_DRBG_set_reseed_defaults: TRAND_DRBG_set_reseed_defaults;
  RAND_DRBG_set_callbacks: TRAND_DRBG_set_callbacks;
  RAND_DRBG_set_ex_data: TRAND_DRBG_set_ex_data;
  RAND_DRBG_get_ex_data: TRAND_DRBG_get_ex_data;
  RAND_DRBG_get_ex_new_index: TRAND_DRBG_get_ex_new_index;
  
  { RAND pool functions }
  RAND_POOL_new: TRAND_POOL_new;
  RAND_POOL_free: TRAND_POOL_free;
  RAND_POOL_entropy: TRAND_POOL_entropy;
  RAND_POOL_length: TRAND_POOL_length;
  RAND_POOL_entropy_available: TRAND_POOL_entropy_available;
  RAND_POOL_entropy_needed: TRAND_POOL_entropy_needed;
  RAND_POOL_bytes_needed: TRAND_POOL_bytes_needed;
  RAND_POOL_bytes_remaining: TRAND_POOL_bytes_remaining;
  RAND_POOL_add: TRAND_POOL_add;
  RAND_POOL_add_begin: TRAND_POOL_add_begin;
  RAND_POOL_add_end: TRAND_POOL_add_end;
  RAND_POOL_acquire_entropy: TRAND_POOL_acquire_entropy;

function LoadOpenSSLRAND: Boolean;
procedure UnloadOpenSSLRAND;
function IsOpenSSLRANDLoaded: Boolean;

{ Helper functions }
function RAND_bytes_secure(buf: PByte; num: Integer): Boolean;
function RAND_bytes_secure_ex(buf: PByte; num: size_t; strength: Cardinal = 128): Boolean;
function RAND_generate_hex(len: Integer): string;
function RAND_generate_base64(len: Integer): string;

implementation

var
  GRANDLoaded: Boolean = False;

function LoadOpenSSLRAND: Boolean;
var
  LLib: TLibHandle;
begin
  if GRANDLoaded then
    Exit(True);
    
  // Try to get handle to already loaded library
  LLib := GetLibHandle('libcrypto');
  if LLib = NilHandle then
    LLib := GetLibHandle('libeay32');
    
  if LLib = NilHandle then
    Exit(False);
    
  // Load RAND method functions
  RAND_set_rand_method := GetProcedureAddress(LLib, 'RAND_set_rand_method');
  RAND_get_rand_method := GetProcedureAddress(LLib, 'RAND_get_rand_method');
  RAND_SSLeay := GetProcedureAddress(LLib, 'RAND_SSLeay');
  RAND_OpenSSL := GetProcedureAddress(LLib, 'RAND_OpenSSL');
  
  // Load random number generation functions
  RAND_bytes := GetProcedureAddress(LLib, 'RAND_bytes');
  RAND_bytes_ex := GetProcedureAddress(LLib, 'RAND_bytes_ex');
  RAND_pseudo_bytes := GetProcedureAddress(LLib, 'RAND_pseudo_bytes');
  RAND_priv_bytes := GetProcedureAddress(LLib, 'RAND_priv_bytes');
  RAND_priv_bytes_ex := GetProcedureAddress(LLib, 'RAND_priv_bytes_ex');
  
  // Load seeding functions
  RAND_seed := GetProcedureAddress(LLib, 'RAND_seed');
  RAND_keep_random_devices_open := GetProcedureAddress(LLib, 'RAND_keep_random_devices_open');
  RAND_add := GetProcedureAddress(LLib, 'RAND_add');
  RAND_load_file := GetProcedureAddress(LLib, 'RAND_load_file');
  RAND_write_file := GetProcedureAddress(LLib, 'RAND_write_file');
  RAND_file_name := GetProcedureAddress(LLib, 'RAND_file_name');
  RAND_status := GetProcedureAddress(LLib, 'RAND_status');
  RAND_query_egd_bytes := GetProcedureAddress(LLib, 'RAND_query_egd_bytes');
  RAND_egd := GetProcedureAddress(LLib, 'RAND_egd');
  RAND_egd_bytes := GetProcedureAddress(LLib, 'RAND_egd_bytes');
  RAND_poll := GetProcedureAddress(LLib, 'RAND_poll');
  
  // Load screen interaction functions (Windows specific)
  RAND_screen := GetProcedureAddress(LLib, 'RAND_screen');
  RAND_event := GetProcedureAddress(LLib, 'RAND_event');
  
  // Load DRBG functions (OpenSSL 1.1.1+, may not exist)
  RAND_DRBG_new := GetProcedureAddress(LLib, 'RAND_DRBG_new');
  RAND_DRBG_new_ex := GetProcedureAddress(LLib, 'RAND_DRBG_new_ex');
  RAND_DRBG_secure_new := GetProcedureAddress(LLib, 'RAND_DRBG_secure_new');
  RAND_DRBG_secure_new_ex := GetProcedureAddress(LLib, 'RAND_DRBG_secure_new_ex');
  RAND_DRBG_set := GetProcedureAddress(LLib, 'RAND_DRBG_set');
  RAND_DRBG_set_defaults := GetProcedureAddress(LLib, 'RAND_DRBG_set_defaults');
  RAND_DRBG_instantiate := GetProcedureAddress(LLib, 'RAND_DRBG_instantiate');
  RAND_DRBG_uninstantiate := GetProcedureAddress(LLib, 'RAND_DRBG_uninstantiate');
  RAND_DRBG_free := GetProcedureAddress(LLib, 'RAND_DRBG_free');
  RAND_DRBG_reseed := GetProcedureAddress(LLib, 'RAND_DRBG_reseed');
  RAND_DRBG_generate := GetProcedureAddress(LLib, 'RAND_DRBG_generate');
  RAND_DRBG_bytes := GetProcedureAddress(LLib, 'RAND_DRBG_bytes');
  RAND_DRBG_get0_master := GetProcedureAddress(LLib, 'RAND_DRBG_get0_master');
  RAND_DRBG_get0_public := GetProcedureAddress(LLib, 'RAND_DRBG_get0_public');
  RAND_DRBG_get0_private := GetProcedureAddress(LLib, 'RAND_DRBG_get0_private');
  RAND_DRBG_set_reseed_interval := GetProcedureAddress(LLib, 'RAND_DRBG_set_reseed_interval');
  RAND_DRBG_set_reseed_time_interval := GetProcedureAddress(LLib, 'RAND_DRBG_set_reseed_time_interval');
  RAND_DRBG_set_reseed_defaults := GetProcedureAddress(LLib, 'RAND_DRBG_set_reseed_defaults');
  RAND_DRBG_set_callbacks := GetProcedureAddress(LLib, 'RAND_DRBG_set_callbacks');
  RAND_DRBG_set_ex_data := GetProcedureAddress(LLib, 'RAND_DRBG_set_ex_data');
  RAND_DRBG_get_ex_data := GetProcedureAddress(LLib, 'RAND_DRBG_get_ex_data');
  RAND_DRBG_get_ex_new_index := GetProcedureAddress(LLib, 'RAND_DRBG_get_ex_new_index');
  
  // Load RAND pool functions (OpenSSL 1.1.1+, may not exist)
  RAND_POOL_new := GetProcedureAddress(LLib, 'RAND_POOL_new');
  RAND_POOL_free := GetProcedureAddress(LLib, 'RAND_POOL_free');
  RAND_POOL_entropy := GetProcedureAddress(LLib, 'RAND_POOL_entropy');
  RAND_POOL_length := GetProcedureAddress(LLib, 'RAND_POOL_length');
  RAND_POOL_entropy_available := GetProcedureAddress(LLib, 'RAND_POOL_entropy_available');
  RAND_POOL_entropy_needed := GetProcedureAddress(LLib, 'RAND_POOL_entropy_needed');
  RAND_POOL_bytes_needed := GetProcedureAddress(LLib, 'RAND_POOL_bytes_needed');
  RAND_POOL_bytes_remaining := GetProcedureAddress(LLib, 'RAND_POOL_bytes_remaining');
  RAND_POOL_add := GetProcedureAddress(LLib, 'RAND_POOL_add');
  RAND_POOL_add_begin := GetProcedureAddress(LLib, 'RAND_POOL_add_begin');
  RAND_POOL_add_end := GetProcedureAddress(LLib, 'RAND_POOL_add_end');
  RAND_POOL_acquire_entropy := GetProcedureAddress(LLib, 'RAND_POOL_acquire_entropy');
  
  // The basic RAND functions should be available
  GRANDLoaded := Assigned(RAND_bytes) and Assigned(RAND_seed);
  Result := GRANDLoaded;
end;

procedure UnloadOpenSSLRAND;
begin
  // Clear all function pointers
  RAND_set_rand_method := nil;
  RAND_get_rand_method := nil;
  RAND_SSLeay := nil;
  RAND_OpenSSL := nil;
  
  RAND_bytes := nil;
  RAND_bytes_ex := nil;
  RAND_pseudo_bytes := nil;
  RAND_priv_bytes := nil;
  RAND_priv_bytes_ex := nil;
  
  RAND_seed := nil;
  RAND_keep_random_devices_open := nil;
  RAND_add := nil;
  RAND_load_file := nil;
  RAND_write_file := nil;
  RAND_file_name := nil;
  RAND_status := nil;
  RAND_query_egd_bytes := nil;
  RAND_egd := nil;
  RAND_egd_bytes := nil;
  RAND_poll := nil;
  
  RAND_screen := nil;
  RAND_event := nil;
  
  RAND_DRBG_new := nil;
  RAND_DRBG_new_ex := nil;
  RAND_DRBG_secure_new := nil;
  RAND_DRBG_secure_new_ex := nil;
  RAND_DRBG_set := nil;
  RAND_DRBG_set_defaults := nil;
  RAND_DRBG_instantiate := nil;
  RAND_DRBG_uninstantiate := nil;
  RAND_DRBG_free := nil;
  RAND_DRBG_reseed := nil;
  RAND_DRBG_generate := nil;
  RAND_DRBG_bytes := nil;
  RAND_DRBG_get0_master := nil;
  RAND_DRBG_get0_public := nil;
  RAND_DRBG_get0_private := nil;
  RAND_DRBG_set_reseed_interval := nil;
  RAND_DRBG_set_reseed_time_interval := nil;
  RAND_DRBG_set_reseed_defaults := nil;
  RAND_DRBG_set_callbacks := nil;
  RAND_DRBG_set_ex_data := nil;
  RAND_DRBG_get_ex_data := nil;
  RAND_DRBG_get_ex_new_index := nil;
  
  RAND_POOL_new := nil;
  RAND_POOL_free := nil;
  RAND_POOL_entropy := nil;
  RAND_POOL_length := nil;
  RAND_POOL_entropy_available := nil;
  RAND_POOL_entropy_needed := nil;
  RAND_POOL_bytes_needed := nil;
  RAND_POOL_bytes_remaining := nil;
  RAND_POOL_add := nil;
  RAND_POOL_add_begin := nil;
  RAND_POOL_add_end := nil;
  RAND_POOL_acquire_entropy := nil;
  
  GRANDLoaded := False;
end;

function IsOpenSSLRANDLoaded: Boolean;
begin
  Result := GRANDLoaded;
end;

{ Helper functions }

function RAND_bytes_secure(buf: PByte; num: Integer): Boolean;
begin
  Result := False;
  if not Assigned(RAND_bytes) then
    Exit;
  Result := RAND_bytes(buf, num) = 1;
end;

function RAND_bytes_secure_ex(buf: PByte; num: size_t; strength: Cardinal): Boolean;
begin
  Result := False;
  if Assigned(RAND_bytes_ex) then
    Result := RAND_bytes_ex(nil, buf, num, strength) = 1
  else if Assigned(RAND_bytes) then
    Result := RAND_bytes(buf, num) = 1;
end;

function RAND_generate_hex(len: Integer): string;
const
  HexChars: string = '0123456789abcdef';
var
  LBytes: array of Byte;
  i: Integer;
begin
  Result := '';
  if (len <= 0) or not Assigned(RAND_bytes) then
    Exit;
    
  SetLength(LBytes, (len + 1) div 2);
  if RAND_bytes(@LBytes[0], Length(LBytes)) = 1 then
  begin
    Result := '';
    for i := 0 to Length(LBytes) - 1 do
    begin
      Result := Result + HexChars[(LBytes[i] shr 4) + 1];
      Result := Result + HexChars[(LBytes[i] and $0F) + 1];
    end;
    SetLength(Result, len);
  end;
end;

function RAND_generate_base64(len: Integer): string;
const
  Base64Chars: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  LBytes: array of Byte;
  i, j: Integer;
  LBuf: Cardinal;
begin
  Result := '';
  if (len <= 0) or not Assigned(RAND_bytes) then
    Exit;
    
  SetLength(LBytes, ((len * 3) + 3) div 4);
  if RAND_bytes(@LBytes[0], Length(LBytes)) = 1 then
  begin
    Result := '';
    i := 0;
    while (i < Length(LBytes)) and (Length(Result) < len) do
    begin
      LBuf := LBytes[i];
      Inc(i);
      if i < Length(LBytes) then
      begin
        LBuf := (LBuf shl 8) or LBytes[i];
        Inc(i);
      end;
      if i < Length(LBytes) then
      begin
        LBuf := (LBuf shl 8) or LBytes[i];
        Inc(i);
      end;
      
      for j := 0 to 3 do
      begin
        if Length(Result) < len then
          Result := Result + Base64Chars[((LBuf shr ((3 - j) * 6)) and $3F) + 1];
      end;
    end;
    SetLength(Result, len);
  end;
end;

end.