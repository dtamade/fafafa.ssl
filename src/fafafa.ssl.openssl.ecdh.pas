unit fafafa.ssl.openssl.ecdh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.bn,
  fafafa.ssl.openssl.ec;

const
  { ECDH flags }
  ECDH_FLAG_COFACTOR_ECDH = $01;

type
  { KDF function type - must be defined before ECDH_METHOD }
  TECDH_KDF = function(const in_: Pointer; inlen: size_t; out_: Pointer; outlen: Psize_t): Pointer; cdecl;
  
  { ECDH compute_key function type }
  TECDH_compute_key_method = function(key: PByte; outlen: size_t; const pub_key: PEC_POINT; 
                                       const ecdh: PEC_KEY; KDF: TECDH_KDF): Integer; cdecl;
  
  { ECDH_METHOD structure }
  ECDH_METHOD = record
    name: PAnsiChar;
    compute_key: TECDH_compute_key_method;
    flags: Integer;
    app_data: PAnsiChar;
  end;
  PECDH_METHOD = ^ECDH_METHOD;

  { ECDH function types }
  TECDH_compute_key = function(out_: PByte; outlen: size_t; const pub_key: PEC_POINT; const eckey: PEC_KEY; KDF: TECDH_KDF): Integer; cdecl;
  TECDH_get_default_method = function: PECDH_METHOD; cdecl;
  TECDH_set_default_method = procedure(const meth: PECDH_METHOD); cdecl;
  TECDH_set_method = function(eckey: PEC_KEY; const meth: PECDH_METHOD): Integer; cdecl;
  TECDH_get_ex_data = function(d: PEC_KEY; idx: Integer): Pointer; cdecl;
  TECDH_set_ex_data = function(d: PEC_KEY; idx: Integer; arg: Pointer): Integer; cdecl;
  TECDH_get_ex_new_index = function(argl: clong; argp: Pointer; new_func: Pointer; dup_func: Pointer; free_func: Pointer): Integer; cdecl;
  
  { ECDH KDF functions }
  TECDH_KDF_X9_63 = function(out_: PByte; outlen: size_t; const Z: PByte; Zlen: size_t; const sinfo: PByte; sinfolen: size_t; const md: PEVP_MD): Integer; cdecl;
  
  { EC_KEY method functions for key agreement }
  TEC_KEY_METHOD_get_compute_key = procedure(const meth: PEC_KEY_METHOD; pck: Pointer); cdecl;
  TEC_KEY_METHOD_set_compute_key = procedure(meth: PEC_KEY_METHOD; ckey: Pointer); cdecl;
  
  { EC_KEY functions related to ECDH }
  TEC_KEY_get_flags = function(const key: PEC_KEY): Integer; cdecl;
  TEC_KEY_set_flags = procedure(key: PEC_KEY; flags: Integer); cdecl;
  TEC_KEY_clear_flags = procedure(key: PEC_KEY; flags: Integer); cdecl;

var
  { ECDH functions }
  ECDH_compute_key: TECDH_compute_key;
  ECDH_get_default_method: TECDH_get_default_method;
  ECDH_set_default_method: TECDH_set_default_method;
  ECDH_set_method: TECDH_set_method;
  ECDH_get_ex_data: TECDH_get_ex_data;
  ECDH_set_ex_data: TECDH_set_ex_data;
  ECDH_get_ex_new_index: TECDH_get_ex_new_index;
  
  { ECDH KDF functions }
  ECDH_KDF_X9_63: TECDH_KDF_X9_63;
  
  { EC_KEY method functions }
  EC_KEY_METHOD_get_compute_key: TEC_KEY_METHOD_get_compute_key;
  EC_KEY_METHOD_set_compute_key: TEC_KEY_METHOD_set_compute_key;
  
  { EC_KEY flag functions }
  EC_KEY_get_flags: TEC_KEY_get_flags;
  EC_KEY_set_flags: TEC_KEY_set_flags;
  EC_KEY_clear_flags: TEC_KEY_clear_flags;

function LoadOpenSSLECDH: Boolean;
procedure UnloadOpenSSLECDH;
function IsOpenSSLECDHLoaded: Boolean;

{ Helper functions for ECDH operations }
function ECDH_ComputeSharedSecret(const APrivateKey: PEC_KEY; const APeerPublicKey: PEC_POINT; 
                                   out ASharedSecret: TBytes): Boolean;
function ECDH_ComputeSharedSecretWithKDF(const APrivateKey: PEC_KEY; const APeerPublicKey: PEC_POINT;
                                          AKDF: TECDH_KDF; out ASharedSecret: TBytes): Boolean;
function ECDH_SetCofactorMode(AKey: PEC_KEY; AUseCofactor: Boolean): Boolean;

implementation

uses
  fafafa.ssl.openssl.core;

var
  GECDHLoaded: Boolean = False;

function LoadOpenSSLECDH: Boolean;
var
  LLib: TLibHandle;
begin
  if GECDHLoaded then
    Exit(True);
    
  // Use the crypto library handle from core module
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    LoadOpenSSLCore;
    LLib := GetCryptoLibHandle;
  end;
    
  if LLib = NilHandle then
    Exit(False);
    
  // Load ECDH functions with proper type casting
  ECDH_compute_key := TECDH_compute_key(GetProcAddress(LLib, 'ECDH_compute_key'));
  ECDH_get_default_method := TECDH_get_default_method(GetProcAddress(LLib, 'ECDH_get_default_method'));
  ECDH_set_default_method := TECDH_set_default_method(GetProcAddress(LLib, 'ECDH_set_default_method'));
  ECDH_set_method := TECDH_set_method(GetProcAddress(LLib, 'ECDH_set_method'));
  ECDH_get_ex_data := TECDH_get_ex_data(GetProcAddress(LLib, 'ECDH_get_ex_data'));
  ECDH_set_ex_data := TECDH_set_ex_data(GetProcAddress(LLib, 'ECDH_set_ex_data'));
  ECDH_get_ex_new_index := TECDH_get_ex_new_index(GetProcAddress(LLib, 'ECDH_get_ex_new_index'));
  
  // Load ECDH KDF functions
  ECDH_KDF_X9_63 := TECDH_KDF_X9_63(GetProcAddress(LLib, 'ECDH_KDF_X9_63'));
  
  // Load EC_KEY method functions
  EC_KEY_METHOD_get_compute_key := TEC_KEY_METHOD_get_compute_key(GetProcAddress(LLib, 'EC_KEY_METHOD_get_compute_key'));
  EC_KEY_METHOD_set_compute_key := TEC_KEY_METHOD_set_compute_key(GetProcAddress(LLib, 'EC_KEY_METHOD_set_compute_key'));
  
  // Load EC_KEY flag functions
  EC_KEY_get_flags := TEC_KEY_get_flags(GetProcAddress(LLib, 'EC_KEY_get_flags'));
  EC_KEY_set_flags := TEC_KEY_set_flags(GetProcAddress(LLib, 'EC_KEY_set_flags'));
  EC_KEY_clear_flags := TEC_KEY_clear_flags(GetProcAddress(LLib, 'EC_KEY_clear_flags'));
  
  // Check if critical functions are loaded
  // Note: In OpenSSL 1.1.0+, ECDH_compute_key might not exist as a separate function
  // The functionality is integrated into EC_KEY operations
  GECDHLoaded := Assigned(ECDH_compute_key) or 
                 (Assigned(EC_KEY_get_flags) and Assigned(EC_KEY_set_flags));
  Result := GECDHLoaded;
end;

procedure UnloadOpenSSLECDH;
begin
  // Clear ECDH function pointers
  ECDH_compute_key := nil;
  ECDH_get_default_method := nil;
  ECDH_set_default_method := nil;
  ECDH_set_method := nil;
  ECDH_get_ex_data := nil;
  ECDH_set_ex_data := nil;
  ECDH_get_ex_new_index := nil;
  
  // Clear ECDH KDF function pointers
  ECDH_KDF_X9_63 := nil;
  
  // Clear EC_KEY method function pointers
  EC_KEY_METHOD_get_compute_key := nil;
  EC_KEY_METHOD_set_compute_key := nil;
  
  // Clear EC_KEY flag function pointers
  EC_KEY_get_flags := nil;
  EC_KEY_set_flags := nil;
  EC_KEY_clear_flags := nil;
  
  GECDHLoaded := False;
end;

function IsOpenSSLECDHLoaded: Boolean;
begin
  Result := GECDHLoaded;
end;

{ Helper functions }

function ECDH_ComputeSharedSecret(const APrivateKey: PEC_KEY; const APeerPublicKey: PEC_POINT; 
                                   out ASharedSecret: TBytes): Boolean;
var
  LFieldSize: Integer;
  LOutLen: Integer;
  LGroup: PEC_GROUP;
begin
  Result := False;
  if not Assigned(ECDH_compute_key) then
    Exit;
    
  if not Assigned(APrivateKey) or not Assigned(APeerPublicKey) then
    Exit;
    
  // Get the EC group from the private key
  LGroup := EC_KEY_get0_group(APrivateKey);
  if not Assigned(LGroup) then
    Exit;
    
  // Calculate the field size
  LFieldSize := EC_GROUP_get_degree(LGroup);
  if LFieldSize <= 0 then
    Exit;
    
  LFieldSize := (LFieldSize + 7) div 8;
  
  // Allocate buffer for shared secret
  SetLength(ASharedSecret, LFieldSize);
  
  // Compute the shared secret
  LOutLen := ECDH_compute_key(@ASharedSecret[0], LFieldSize, APeerPublicKey, APrivateKey, nil);
  if LOutLen > 0 then
  begin
    SetLength(ASharedSecret, LOutLen);
    Result := True;
  end
  else
    SetLength(ASharedSecret, 0);
end;

function ECDH_ComputeSharedSecretWithKDF(const APrivateKey: PEC_KEY; const APeerPublicKey: PEC_POINT;
                                          AKDF: TECDH_KDF; out ASharedSecret: TBytes): Boolean;
var
  LFieldSize: Integer;
  LOutLen: Integer;
  LGroup: PEC_GROUP;
begin
  Result := False;
  if not Assigned(ECDH_compute_key) then
    Exit;
    
  if not Assigned(APrivateKey) or not Assigned(APeerPublicKey) or not Assigned(AKDF) then
    Exit;
    
  // Get the EC group from the private key
  LGroup := EC_KEY_get0_group(APrivateKey);
  if not Assigned(LGroup) then
    Exit;
    
  // Calculate the field size
  LFieldSize := EC_GROUP_get_degree(LGroup);
  if LFieldSize <= 0 then
    Exit;
    
  LFieldSize := (LFieldSize + 7) div 8;
  
  // Allocate buffer for shared secret
  SetLength(ASharedSecret, LFieldSize);
  
  // Compute the shared secret with KDF
  LOutLen := ECDH_compute_key(@ASharedSecret[0], LFieldSize, APeerPublicKey, APrivateKey, AKDF);
  if LOutLen > 0 then
  begin
    SetLength(ASharedSecret, LOutLen);
    Result := True;
  end
  else
    SetLength(ASharedSecret, 0);
end;

function ECDH_SetCofactorMode(AKey: PEC_KEY; AUseCofactor: Boolean): Boolean;
var
  LFlags: Integer;
begin
  Result := False;
  if not Assigned(EC_KEY_get_flags) or not Assigned(EC_KEY_set_flags) or 
     not Assigned(EC_KEY_clear_flags) then
    Exit;
    
  if not Assigned(AKey) then
    Exit;
    
  if AUseCofactor then
  begin
    LFlags := EC_KEY_get_flags(AKey);
    EC_KEY_set_flags(AKey, LFlags or ECDH_FLAG_COFACTOR_ECDH);
  end
  else
  begin
    EC_KEY_clear_flags(AKey, ECDH_FLAG_COFACTOR_ECDH);
  end;
  
  Result := True;
end;

end.