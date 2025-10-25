unit fafafa.ssl.openssl.api.ecdsa;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.ec;

type
  { ECDSA_SIG structure for signature }
  ECDSA_SIG_st = record
    r: PBIGNUM;
    s: PBIGNUM;
  end;
  ECDSA_SIG = ECDSA_SIG_st;
  PECDSA_SIG = ^ECDSA_SIG;
  PPECDSA_SIG = ^PECDSA_SIG;

  { ECDSA_METHOD structure }
  ECDSA_METHOD = record
    name: PAnsiChar;
    ecdsa_do_sign: function(const dgst: PByte; dgst_len: Integer; const inv: PBIGNUM; const rp: PBIGNUM; eckey: PEC_KEY): PECDSA_SIG; cdecl;
    ecdsa_sign_setup: function(eckey: PEC_KEY; ctx: PBN_CTX; kinvp: PPBIGNUM; rp: PPBIGNUM): Integer; cdecl;
    ecdsa_do_verify: function(const dgst: PByte; dgst_len: Integer; const sig: PECDSA_SIG; eckey: PEC_KEY): Integer; cdecl;
    flags: Integer;
    app_data: PAnsiChar;
  end;
  PECDSA_METHOD = ^ECDSA_METHOD;

  { ECDSA function types }
  TECDSA_SIG_new = function: PECDSA_SIG; cdecl;
  TECDSA_SIG_free = procedure(sig: PECDSA_SIG); cdecl;
  Ti2d_ECDSA_SIG = function(const sig: PECDSA_SIG; pp: PPByte): Integer; cdecl;
  Td2i_ECDSA_SIG = function(sig: PPECDSA_SIG; const pp: PPByte; len: clong): PECDSA_SIG; cdecl;
  TECDSA_SIG_get0 = procedure(const sig: PECDSA_SIG; const pr: PPBIGNUM; const ps: PPBIGNUM); cdecl;
  TECDSA_SIG_get0_r = function(const sig: PECDSA_SIG): PBIGNUM; cdecl;
  TECDSA_SIG_get0_s = function(const sig: PECDSA_SIG): PBIGNUM; cdecl;
  TECDSA_SIG_set0 = function(sig: PECDSA_SIG; r: PBIGNUM; s: PBIGNUM): Integer; cdecl;
  
  { ECDSA signing and verification }
  TECDSA_do_sign = function(const dgst: PByte; dgst_len: Integer; eckey: PEC_KEY): PECDSA_SIG; cdecl;
  TECDSA_do_sign_ex = function(const dgst: PByte; dgst_len: Integer; const kinv: PBIGNUM; const rp: PBIGNUM; eckey: PEC_KEY): PECDSA_SIG; cdecl;
  TECDSA_do_verify = function(const dgst: PByte; dgst_len: Integer; const sig: PECDSA_SIG; eckey: PEC_KEY): Integer; cdecl;
  TECDSA_sign = function(type_: Integer; const dgst: PByte; dgstlen: Integer; sig: PByte; siglen: PCardinal; eckey: PEC_KEY): Integer; cdecl;
  TECDSA_sign_ex = function(type_: Integer; const dgst: PByte; dgstlen: Integer; sig: PByte; siglen: PCardinal; const kinv: PBIGNUM; const rp: PBIGNUM; eckey: PEC_KEY): Integer; cdecl;
  TECDSA_sign_setup = function(eckey: PEC_KEY; ctx: PBN_CTX; kinv: PPBIGNUM; rp: PPBIGNUM): Integer; cdecl;
  TECDSA_verify = function(type_: Integer; const dgst: PByte; dgstlen: Integer; const sig: PByte; siglen: Integer; eckey: PEC_KEY): Integer; cdecl;
  TECDSA_size = function(const eckey: PEC_KEY): Integer; cdecl;
  
  { ECDSA method functions }
  TEC_KEY_METHOD_get_sign = procedure(const meth: PEC_KEY_METHOD; 
                                       psign: Pointer;
                                       psign_setup: Pointer;
                                       psign_sig: Pointer); cdecl;
  TEC_KEY_METHOD_set_sign = procedure(meth: PEC_KEY_METHOD;
                                       sign: Pointer;
                                       sign_setup: Pointer;
                                       sign_sig: Pointer); cdecl;
  TEC_KEY_METHOD_get_verify = procedure(const meth: PEC_KEY_METHOD;
                                         pverify: Pointer;
                                         pverify_sig: Pointer); cdecl;
  TEC_KEY_METHOD_set_verify = procedure(meth: PEC_KEY_METHOD;
                                         verify: Pointer;
                                         verify_sig: Pointer); cdecl;
  
  { Old compatibility functions }
  TECDSA_get_default_method = function: PECDSA_METHOD; cdecl;
  TECDSA_set_default_method = procedure(meth: PECDSA_METHOD); cdecl;
  TECDSA_get_ex_data = function(d: PEC_KEY; idx: Integer): Pointer; cdecl;
  TECDSA_set_ex_data = function(d: PEC_KEY; idx: Integer; arg: Pointer): Integer; cdecl;
  TECDSA_get_ex_new_index = function(argl: clong; argp: Pointer; new_func: Pointer; dup_func: Pointer; free_func: Pointer): Integer; cdecl;
  
  { ECDSA method set/get }
  TECDSA_set_method = function(eckey: PEC_KEY; const meth: PECDSA_METHOD): Integer; cdecl;
  TECDSA_get0_method = function(const eckey: PEC_KEY): PECDSA_METHOD; cdecl;

var
  { ECDSA signature functions }
  ECDSA_SIG_new: TECDSA_SIG_new;
  ECDSA_SIG_free: TECDSA_SIG_free;
  i2d_ECDSA_SIG: Ti2d_ECDSA_SIG;
  d2i_ECDSA_SIG: Td2i_ECDSA_SIG;
  ECDSA_SIG_get0: TECDSA_SIG_get0;
  ECDSA_SIG_get0_r: TECDSA_SIG_get0_r;
  ECDSA_SIG_get0_s: TECDSA_SIG_get0_s;
  ECDSA_SIG_set0: TECDSA_SIG_set0;
  
  { ECDSA signing and verification }
  ECDSA_do_sign: TECDSA_do_sign;
  ECDSA_do_sign_ex: TECDSA_do_sign_ex;
  ECDSA_do_verify: TECDSA_do_verify;
  ECDSA_sign: TECDSA_sign;
  ECDSA_sign_ex: TECDSA_sign_ex;
  ECDSA_sign_setup: TECDSA_sign_setup;
  ECDSA_verify: TECDSA_verify;
  ECDSA_size: TECDSA_size;
  
  { ECDSA method functions }
  EC_KEY_METHOD_get_sign: TEC_KEY_METHOD_get_sign;
  EC_KEY_METHOD_set_sign: TEC_KEY_METHOD_set_sign;
  EC_KEY_METHOD_get_verify: TEC_KEY_METHOD_get_verify;
  EC_KEY_METHOD_set_verify: TEC_KEY_METHOD_set_verify;
  
  { Old compatibility functions }
  ECDSA_get_default_method: TECDSA_get_default_method;
  ECDSA_set_default_method: TECDSA_set_default_method;
  ECDSA_get_ex_data: TECDSA_get_ex_data;
  ECDSA_set_ex_data: TECDSA_set_ex_data;
  ECDSA_get_ex_new_index: TECDSA_get_ex_new_index;
  
  { ECDSA method set/get }
  ECDSA_set_method: TECDSA_set_method;
  ECDSA_get0_method: TECDSA_get0_method;

function LoadOpenSSLECDSA: Boolean;
procedure UnloadOpenSSLECDSA;
function IsOpenSSLECDSALoaded: Boolean;

{ Helper functions for ECDSA operations }
function ECDSA_SignData(const AData: PByte; ADataLen: Integer; AKey: PEC_KEY; out ASignature: TBytes): Boolean;
function ECDSA_VerifyData(const AData: PByte; ADataLen: Integer; const ASignature: PByte; ASignatureLen: Integer; AKey: PEC_KEY): Boolean;
function ECDSA_GetSignatureSize(AKey: PEC_KEY): Integer;

implementation

uses
  fafafa.ssl.openssl.api.core;

var
  GECDSALoaded: Boolean = False;

function LoadOpenSSLECDSA: Boolean;
var
  LLib: TLibHandle;
begin
  if GECDSALoaded then
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
    
  // Load ECDSA signature functions with proper type casting
  ECDSA_SIG_new := TECDSA_SIG_new(GetProcAddress(LLib, 'ECDSA_SIG_new'));
  ECDSA_SIG_free := TECDSA_SIG_free(GetProcAddress(LLib, 'ECDSA_SIG_free'));
  i2d_ECDSA_SIG := Ti2d_ECDSA_SIG(GetProcAddress(LLib, 'i2d_ECDSA_SIG'));
  d2i_ECDSA_SIG := Td2i_ECDSA_SIG(GetProcAddress(LLib, 'd2i_ECDSA_SIG'));
  ECDSA_SIG_get0 := TECDSA_SIG_get0(GetProcAddress(LLib, 'ECDSA_SIG_get0'));
  ECDSA_SIG_get0_r := TECDSA_SIG_get0_r(GetProcAddress(LLib, 'ECDSA_SIG_get0_r'));
  ECDSA_SIG_get0_s := TECDSA_SIG_get0_s(GetProcAddress(LLib, 'ECDSA_SIG_get0_s'));
  ECDSA_SIG_set0 := TECDSA_SIG_set0(GetProcAddress(LLib, 'ECDSA_SIG_set0'));
  
  // Load ECDSA signing and verification functions
  ECDSA_do_sign := TECDSA_do_sign(GetProcAddress(LLib, 'ECDSA_do_sign'));
  ECDSA_do_sign_ex := TECDSA_do_sign_ex(GetProcAddress(LLib, 'ECDSA_do_sign_ex'));
  ECDSA_do_verify := TECDSA_do_verify(GetProcAddress(LLib, 'ECDSA_do_verify'));
  ECDSA_sign := TECDSA_sign(GetProcAddress(LLib, 'ECDSA_sign'));
  ECDSA_sign_ex := TECDSA_sign_ex(GetProcAddress(LLib, 'ECDSA_sign_ex'));
  ECDSA_sign_setup := TECDSA_sign_setup(GetProcAddress(LLib, 'ECDSA_sign_setup'));
  ECDSA_verify := TECDSA_verify(GetProcAddress(LLib, 'ECDSA_verify'));
  ECDSA_size := TECDSA_size(GetProcAddress(LLib, 'ECDSA_size'));
  
  // Load ECDSA method functions
  EC_KEY_METHOD_get_sign := TEC_KEY_METHOD_get_sign(GetProcAddress(LLib, 'EC_KEY_METHOD_get_sign'));
  EC_KEY_METHOD_set_sign := TEC_KEY_METHOD_set_sign(GetProcAddress(LLib, 'EC_KEY_METHOD_set_sign'));
  EC_KEY_METHOD_get_verify := TEC_KEY_METHOD_get_verify(GetProcAddress(LLib, 'EC_KEY_METHOD_get_verify'));
  EC_KEY_METHOD_set_verify := TEC_KEY_METHOD_set_verify(GetProcAddress(LLib, 'EC_KEY_METHOD_set_verify'));
  
  // Load old compatibility functions (may not exist in newer versions)
  ECDSA_get_default_method := TECDSA_get_default_method(GetProcAddress(LLib, 'ECDSA_get_default_method'));
  ECDSA_set_default_method := TECDSA_set_default_method(GetProcAddress(LLib, 'ECDSA_set_default_method'));
  ECDSA_get_ex_data := TECDSA_get_ex_data(GetProcAddress(LLib, 'ECDSA_get_ex_data'));
  ECDSA_set_ex_data := TECDSA_set_ex_data(GetProcAddress(LLib, 'ECDSA_set_ex_data'));
  ECDSA_get_ex_new_index := TECDSA_get_ex_new_index(GetProcAddress(LLib, 'ECDSA_get_ex_new_index'));
  ECDSA_set_method := TECDSA_set_method(GetProcAddress(LLib, 'ECDSA_set_method'));
  ECDSA_get0_method := TECDSA_get0_method(GetProcAddress(LLib, 'ECDSA_get0_method'));
  
  // Check if critical functions are loaded
  GECDSALoaded := Assigned(ECDSA_SIG_new) and Assigned(ECDSA_SIG_free) and
                  Assigned(ECDSA_sign) and Assigned(ECDSA_verify);
  Result := GECDSALoaded;
end;

procedure UnloadOpenSSLECDSA;
begin
  // Clear ECDSA signature function pointers
  ECDSA_SIG_new := nil;
  ECDSA_SIG_free := nil;
  i2d_ECDSA_SIG := nil;
  d2i_ECDSA_SIG := nil;
  ECDSA_SIG_get0 := nil;
  ECDSA_SIG_get0_r := nil;
  ECDSA_SIG_get0_s := nil;
  ECDSA_SIG_set0 := nil;
  
  // Clear ECDSA signing and verification function pointers
  ECDSA_do_sign := nil;
  ECDSA_do_sign_ex := nil;
  ECDSA_do_verify := nil;
  ECDSA_sign := nil;
  ECDSA_sign_ex := nil;
  ECDSA_sign_setup := nil;
  ECDSA_verify := nil;
  ECDSA_size := nil;
  
  // Clear ECDSA method function pointers
  EC_KEY_METHOD_get_sign := nil;
  EC_KEY_METHOD_set_sign := nil;
  EC_KEY_METHOD_get_verify := nil;
  EC_KEY_METHOD_set_verify := nil;
  
  // Clear old compatibility function pointers
  ECDSA_get_default_method := nil;
  ECDSA_set_default_method := nil;
  ECDSA_get_ex_data := nil;
  ECDSA_set_ex_data := nil;
  ECDSA_get_ex_new_index := nil;
  ECDSA_set_method := nil;
  ECDSA_get0_method := nil;
  
  GECDSALoaded := False;
end;

function IsOpenSSLECDSALoaded: Boolean;
begin
  Result := GECDSALoaded;
end;

{ Helper functions }

function ECDSA_SignData(const AData: PByte; ADataLen: Integer; AKey: PEC_KEY; out ASignature: TBytes): Boolean;
var
  LSigLen: Cardinal;
  LMaxSigLen: Integer;
begin
  Result := False;
  if not Assigned(ECDSA_sign) or not Assigned(ECDSA_size) then
    Exit;
    
  if not Assigned(AData) or (ADataLen <= 0) or not Assigned(AKey) then
    Exit;
    
  LMaxSigLen := ECDSA_size(AKey);
  if LMaxSigLen <= 0 then
    Exit;
    
  SetLength(ASignature, LMaxSigLen);
  LSigLen := LMaxSigLen;
  
  if ECDSA_sign(0, AData, ADataLen, @ASignature[0], @LSigLen, AKey) = 1 then
  begin
    SetLength(ASignature, LSigLen);
    Result := True;
  end
  else
    SetLength(ASignature, 0);
end;

function ECDSA_VerifyData(const AData: PByte; ADataLen: Integer; const ASignature: PByte; ASignatureLen: Integer; AKey: PEC_KEY): Boolean;
begin
  Result := False;
  if not Assigned(ECDSA_verify) then
    Exit;
    
  if not Assigned(AData) or (ADataLen <= 0) or 
     not Assigned(ASignature) or (ASignatureLen <= 0) or
     not Assigned(AKey) then
    Exit;
    
  Result := ECDSA_verify(0, AData, ADataLen, ASignature, ASignatureLen, AKey) = 1;
end;

function ECDSA_GetSignatureSize(AKey: PEC_KEY): Integer;
begin
  Result := 0;
  if not Assigned(ECDSA_size) or not Assigned(AKey) then
    Exit;
    
  Result := ECDSA_size(AKey);
end;

end.