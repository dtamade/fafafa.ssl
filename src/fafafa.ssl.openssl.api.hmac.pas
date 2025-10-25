unit fafafa.ssl.openssl.api.hmac;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types;

const
  { HMAC constants }
  HMAC_MAX_MD_CBLOCK = 200;  // Maximum block size for HMAC
  
type
  { HMAC_CTX structure - opaque in OpenSSL 1.1.0+ }
  HMAC_CTX = record
    // Internal structure is opaque
  end;
  PHMAC_CTX = ^HMAC_CTX;
  PPHMAC_CTX = ^PHMAC_CTX;

  { HMAC function types }
  THMAC = function(const evp_md: PEVP_MD; const key: Pointer; key_len: Integer;
                   const d: PByte; n: size_t; md: PByte; md_len: PCardinal): PByte; cdecl;
  THMAC_CTX_new = function: PHMAC_CTX; cdecl;
  THMAC_CTX_reset = function(ctx: PHMAC_CTX): Integer; cdecl;
  THMAC_CTX_free = procedure(ctx: PHMAC_CTX); cdecl;
  THMAC_Init = function(ctx: PHMAC_CTX; const key: Pointer; len: Integer; const md: PEVP_MD): Integer; cdecl;
  THMAC_Init_ex = function(ctx: PHMAC_CTX; const key: Pointer; len: Integer; const md: PEVP_MD; impl: PENGINE): Integer; cdecl;
  THMAC_Update = function(ctx: PHMAC_CTX; const data: Pointer; len: size_t): Integer; cdecl;
  THMAC_Final = function(ctx: PHMAC_CTX; md: PByte; len: PCardinal): Integer; cdecl;
  THMAC_CTX_copy = function(dctx: PHMAC_CTX; sctx: PHMAC_CTX): Integer; cdecl;
  THMAC_CTX_set_flags = procedure(ctx: PHMAC_CTX; flags: Cardinal); cdecl;
  THMAC_CTX_get_md = function(const ctx: PHMAC_CTX): PEVP_MD; cdecl;
  THMAC_size = function(const ctx: PHMAC_CTX): size_t; cdecl;
  
  { Legacy functions for older OpenSSL versions }
  THMAC_CTX_init = procedure(ctx: PHMAC_CTX); cdecl;
  THMAC_CTX_cleanup = procedure(ctx: PHMAC_CTX); cdecl;

var
  { HMAC functions }
  HMAC: THMAC;
  HMAC_CTX_new: THMAC_CTX_new;
  HMAC_CTX_reset: THMAC_CTX_reset;
  HMAC_CTX_free: THMAC_CTX_free;
  HMAC_Init: THMAC_Init;
  HMAC_Init_ex: THMAC_Init_ex;
  HMAC_Update: THMAC_Update;
  HMAC_Final: THMAC_Final;
  HMAC_CTX_copy: THMAC_CTX_copy;
  HMAC_CTX_set_flags: THMAC_CTX_set_flags;
  HMAC_CTX_get_md: THMAC_CTX_get_md;
  HMAC_size: THMAC_size;
  
  { Legacy functions }
  HMAC_CTX_init: THMAC_CTX_init;
  HMAC_CTX_cleanup: THMAC_CTX_cleanup;

function LoadOpenSSLHMAC: Boolean;
procedure UnloadOpenSSLHMAC;
function IsOpenSSLHMACLoaded: Boolean;

{ Helper functions }
function HMAC_SHA1(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;
function HMAC_SHA256(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;
function HMAC_SHA512(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;

implementation

uses
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

var
  GHMACLoaded: Boolean = False;

function LoadOpenSSLHMAC: Boolean;
var
  LLib: TLibHandle;
begin
  if GHMACLoaded then
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
    
  // Ensure EVP module is loaded (needed for digest algorithms)
  if not IsEVPLoaded then
    LoadEVP(LLib);
    
  // Load HMAC functions
  HMAC := THMAC(GetProcAddress(LLib, 'HMAC'));
  HMAC_CTX_new := THMAC_CTX_new(GetProcAddress(LLib, 'HMAC_CTX_new'));
  HMAC_CTX_reset := THMAC_CTX_reset(GetProcAddress(LLib, 'HMAC_CTX_reset'));
  HMAC_CTX_free := THMAC_CTX_free(GetProcAddress(LLib, 'HMAC_CTX_free'));
  HMAC_Init := THMAC_Init(GetProcAddress(LLib, 'HMAC_Init'));
  HMAC_Init_ex := THMAC_Init_ex(GetProcAddress(LLib, 'HMAC_Init_ex'));
  HMAC_Update := THMAC_Update(GetProcAddress(LLib, 'HMAC_Update'));
  HMAC_Final := THMAC_Final(GetProcAddress(LLib, 'HMAC_Final'));
  HMAC_CTX_copy := THMAC_CTX_copy(GetProcAddress(LLib, 'HMAC_CTX_copy'));
  HMAC_CTX_set_flags := THMAC_CTX_set_flags(GetProcAddress(LLib, 'HMAC_CTX_set_flags'));
  HMAC_CTX_get_md := THMAC_CTX_get_md(GetProcAddress(LLib, 'HMAC_CTX_get_md'));
  HMAC_size := THMAC_size(GetProcAddress(LLib, 'HMAC_size'));
  
  // Load legacy functions (may not exist in newer versions)
  HMAC_CTX_init := THMAC_CTX_init(GetProcAddress(LLib, 'HMAC_CTX_init'));
  HMAC_CTX_cleanup := THMAC_CTX_cleanup(GetProcAddress(LLib, 'HMAC_CTX_cleanup'));
  
  GHMACLoaded := Assigned(HMAC);
  Result := GHMACLoaded;
end;

procedure UnloadOpenSSLHMAC;
begin
  HMAC := nil;
  HMAC_CTX_new := nil;
  HMAC_CTX_reset := nil;
  HMAC_CTX_free := nil;
  HMAC_Init := nil;
  HMAC_Init_ex := nil;
  HMAC_Update := nil;
  HMAC_Final := nil;
  HMAC_CTX_copy := nil;
  HMAC_CTX_set_flags := nil;
  HMAC_CTX_get_md := nil;
  HMAC_size := nil;
  HMAC_CTX_init := nil;
  HMAC_CTX_cleanup := nil;
  
  GHMACLoaded := False;
end;

function IsOpenSSLHMACLoaded: Boolean;
begin
  Result := GHMACLoaded;
end;

{ Helper functions }

function HMAC_SHA1(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;
begin
  Result := nil;
  if Assigned(HMAC) and Assigned(EVP_sha1) then
    Result := HMAC(EVP_sha1(), key, key_len, data, data_len, digest, nil);
end;

function HMAC_SHA256(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;
begin
  Result := nil;
  if Assigned(HMAC) and Assigned(EVP_sha256) then
    Result := HMAC(EVP_sha256(), key, key_len, data, data_len, digest, nil);
end;

function HMAC_SHA512(const key: Pointer; key_len: Integer; const data: Pointer; data_len: size_t; digest: PByte): PByte;
begin
  Result := nil;
  if Assigned(HMAC) and Assigned(EVP_sha512) then
    Result := HMAC(EVP_sha512(), key, key_len, data, data_len, digest, nil);
end;

end.