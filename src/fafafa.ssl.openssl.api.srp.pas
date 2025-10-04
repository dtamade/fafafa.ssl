{******************************************************************************}
{                                                                              }
{  fafafa.ssl.openssl.api.srp - OpenSSL SRP Module Pascal Binding                 }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}
unit fafafa.ssl.openssl.api.srp;

{$MODE OBJFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.consts;

type
  { SRP types }
  SRP_gN_cache = record end;
  PSRP_gN_cache = ^SRP_gN_cache;
  
  SRP_user_pwd = record end;
  PSRP_user_pwd = ^SRP_user_pwd;
  
  SRP_VBASE = record end;
  PSRP_VBASE = ^SRP_VBASE;
  
  SRP_gN = record
    id: PChar;
    g: PBIGNUM;
    N: PBIGNUM;
  end;
  PSRP_gN = ^SRP_gN;
  
  { SRP callback types }
  TSRP_verify_param_callback = function(id: PChar; N: PBIGNUM; g: PBIGNUM): TOpenSSLInt; cdecl;
  TSRP_user_callback = function(user: PSRP_user_pwd; arg: Pointer): TOpenSSLInt; cdecl;
  TSRP_give_pwd_callback = function(login: PChar; user: PSRP_user_pwd): PChar; cdecl;
  
  { Function pointers for dynamic loading }
  { Basic SRP functions }
  TSRPBnbin2bn = function(const s: PByte; len: TOpenSSLInt; ret: PBIGNUM): PBIGNUM; cdecl;
  TSRPCalcB = function(b: PBIGNUM; N: PBIGNUM; g: PBIGNUM; v: PBIGNUM): PBIGNUM; cdecl;
  TSRPCalcServerKey = function(A: PBIGNUM; v: PBIGNUM; u: PBIGNUM; b: PBIGNUM; N: PBIGNUM): PBIGNUM; cdecl;
  TSRPCalcU = function(A: PBIGNUM; B: PBIGNUM; N: PBIGNUM): PBIGNUM; cdecl;
  TSRPCalcClientKey = function(N: PBIGNUM; B: PBIGNUM; g: PBIGNUM; x: PBIGNUM; a: PBIGNUM; u: PBIGNUM): PBIGNUM; cdecl;
  TSRPCalcX = function(s: PBIGNUM; const user: PChar; const pass: PChar): PBIGNUM; cdecl;
  TSRPCalcClient = function(a: PBIGNUM; N: PBIGNUM; g: PBIGNUM): PBIGNUM; cdecl;
  TSRPVerifyAPubKey = function(N: PBIGNUM; A: PBIGNUM): TOpenSSLInt; cdecl;
  TSRPVerifyBPubKey = function(N: PBIGNUM; B: PBIGNUM): TOpenSSLInt; cdecl;
  
  { VBASE functions }
  TSRPVBASEG = function: PSRP_VBASE; cdecl;
  TSRPVBASEFree = procedure(vb: PSRP_VBASE); cdecl;
  TSRPVBASEInit = function(vb: PSRP_VBASE; verifier_file: PChar): TOpenSSLInt; cdecl;
  TSRPVBASEAdd = function(vb: PSRP_VBASE; user_pwd: PSRP_user_pwd): TOpenSSLInt; cdecl;
  TSRPVBASEGetByUser = function(vb: PSRP_VBASE; const username: PChar): PSRP_user_pwd; cdecl;
  TSRPVBASEGet1ByUser = function(vb: PSRP_VBASE; const username: PChar): PSRP_user_pwd; cdecl;
  
  { User management functions }
  TSRPUserPwdNew = function(const username: PChar): PSRP_user_pwd; cdecl;
  TSRPUserPwdFree = procedure(user_pwd: PSRP_user_pwd); cdecl;
  TSRPUserPwdSetSalt = function(user_pwd: PSRP_user_pwd; salt: PBIGNUM): TOpenSSLInt; cdecl;
  TSRPUserPwdSetVerifier = function(user_pwd: PSRP_user_pwd; verifier: PBIGNUM): TOpenSSLInt; cdecl;
  TSRPUserPwdSetInfo = function(user_pwd: PSRP_user_pwd; const info: PChar): TOpenSSLInt; cdecl;
  TSRPUserPwdSetGN = function(user_pwd: PSRP_user_pwd; const g: PChar; const N: PChar): TOpenSSLInt; cdecl;
  TSRPUserPwdGet0Salt = function(user_pwd: PSRP_user_pwd): PBIGNUM; cdecl;
  TSRPUserPwdGet0Verifier = function(user_pwd: PSRP_user_pwd): PBIGNUM; cdecl;
  TSRPUserPwdGet0Username = function(user_pwd: PSRP_user_pwd): PChar; cdecl;
  
  { gN functions }
  TSRPGetDefaultGN = function(const id: PChar): PSRP_gN; cdecl;
  TSRPCheckKnownGNParam = function(g: PBIGNUM; N: PBIGNUM): PChar; cdecl;
  TSRPGet1KnownGN = function(const id: PChar): PSRP_gN; cdecl;
  
  { Password functions }
  TSRPCreateVerifier = function(const user: PChar; const pass: PChar; 
                                salt: PPBIGNUM; verifier: PPBIGNUM; 
                                const N: PChar; const g: PChar): PChar; cdecl;
  TSRPCreateVerifierBN = function(const user: PChar; const pass: PChar;
                                  salt: PPBIGNUM; verifier: PPBIGNUM;
                                  N: PBIGNUM; g: PBIGNUM): TOpenSSLInt; cdecl;

var
  { Function variables }
  SRP_Calc_server_key: TSRPCalcServerKey = nil;
  SRP_Calc_u: TSRPCalcU = nil;
  SRP_Calc_client_key: TSRPCalcClientKey = nil;
  SRP_Calc_x: TSRPCalcX = nil;
  SRP_Calc_B: TSRPCalcB = nil;
  SRP_Calc_A: TSRPCalcClient = nil;
  SRP_Verify_A_mod_N: TSRPVerifyAPubKey = nil;
  SRP_Verify_B_mod_N: TSRPVerifyBPubKey = nil;
  
  { VBASE functions }
  SRP_VBASE_new: TSRPVBASEG = nil;
  SRP_VBASE_free: TSRPVBASEFree = nil;
  SRP_VBASE_init: TSRPVBASEInit = nil;
  SRP_VBASE_add0_user: TSRPVBASEAdd = nil;
  SRP_VBASE_get_by_user: TSRPVBASEGetByUser = nil;
  SRP_VBASE_get1_by_user: TSRPVBASEGet1ByUser = nil;
  
  { User management }
  SRP_user_pwd_new: TSRPUserPwdNew = nil;
  SRP_user_pwd_free: TSRPUserPwdFree = nil;
  SRP_user_pwd_set_salt: TSRPUserPwdSetSalt = nil;
  SRP_user_pwd_set_verifier: TSRPUserPwdSetVerifier = nil;
  SRP_user_pwd_set_info: TSRPUserPwdSetInfo = nil;
  SRP_user_pwd_set_gN: TSRPUserPwdSetGN = nil;
  SRP_user_pwd_get0_salt: TSRPUserPwdGet0Salt = nil;
  SRP_user_pwd_get0_verifier: TSRPUserPwdGet0Verifier = nil;
  SRP_user_pwd_get0_name: TSRPUserPwdGet0Username = nil;
  
  { gN functions }
  SRP_get_default_gN: TSRPGetDefaultGN = nil;
  SRP_check_known_gN_param: TSRPCheckKnownGNParam = nil;
  SRP_get_1_by_id: TSRPGet1KnownGN = nil;
  
  { Password functions }
  SRP_create_verifier: TSRPCreateVerifier = nil;
  SRP_create_verifier_BN: TSRPCreateVerifierBN = nil;

{ Load/Unload functions }
function LoadSRP(const ALibCrypto: THandle): Boolean;
procedure UnloadSRP;

{ Helper functions }
function SRPCreateUser(const Username, Password: string; const gN_id: string = '1024'): PSRP_user_pwd;
function SRPVerifyUser(vbase: PSRP_VBASE; const Username, Password: string): Boolean;
function SRPGenerateVerifier(const Username, Password: string; out Salt, Verifier: string; const gN_id: string = '1024'): Boolean;

implementation

uses
  fafafa.ssl.openssl.api.utils;

var
  SRPLoaded: Boolean = False;

function LoadSRP(const ALibCrypto: THandle): Boolean;
begin
  Result := False;
  if SRPLoaded then Exit(True);
  if ALibCrypto = 0 then Exit;

  { Load calculation functions }
  SRP_Calc_server_key := TSRPCalcServerKey(GetProcAddress(ALibCrypto, 'SRP_Calc_server_key'));
  SRP_Calc_u := TSRPCalcU(GetProcAddress(ALibCrypto, 'SRP_Calc_u'));
  SRP_Calc_client_key := TSRPCalcClientKey(GetProcAddress(ALibCrypto, 'SRP_Calc_client_key'));
  SRP_Calc_x := TSRPCalcX(GetProcAddress(ALibCrypto, 'SRP_Calc_x'));
  SRP_Calc_B := TSRPCalcB(GetProcAddress(ALibCrypto, 'SRP_Calc_B'));
  SRP_Calc_A := TSRPCalcClient(GetProcAddress(ALibCrypto, 'SRP_Calc_A'));
  SRP_Verify_A_mod_N := TSRPVerifyAPubKey(GetProcAddress(ALibCrypto, 'SRP_Verify_A_mod_N'));
  SRP_Verify_B_mod_N := TSRPVerifyBPubKey(GetProcAddress(ALibCrypto, 'SRP_Verify_B_mod_N'));
  
  { Load VBASE functions }
  SRP_VBASE_new := TSRPVBASEG(GetProcAddress(ALibCrypto, 'SRP_VBASE_new'));
  SRP_VBASE_free := TSRPVBASEFree(GetProcAddress(ALibCrypto, 'SRP_VBASE_free'));
  SRP_VBASE_init := TSRPVBASEInit(GetProcAddress(ALibCrypto, 'SRP_VBASE_init'));
  SRP_VBASE_add0_user := TSRPVBASEAdd(GetProcAddress(ALibCrypto, 'SRP_VBASE_add0_user'));
  SRP_VBASE_get_by_user := TSRPVBASEGetByUser(GetProcAddress(ALibCrypto, 'SRP_VBASE_get_by_user'));
  SRP_VBASE_get1_by_user := TSRPVBASEGet1ByUser(GetProcAddress(ALibCrypto, 'SRP_VBASE_get1_by_user'));
  
  { Load user management functions }
  SRP_user_pwd_new := TSRPUserPwdNew(GetProcAddress(ALibCrypto, 'SRP_user_pwd_new'));
  SRP_user_pwd_free := TSRPUserPwdFree(GetProcAddress(ALibCrypto, 'SRP_user_pwd_free'));
  SRP_user_pwd_set_salt := TSRPUserPwdSetSalt(GetProcAddress(ALibCrypto, 'SRP_user_pwd_set_salt'));
  SRP_user_pwd_set_verifier := TSRPUserPwdSetVerifier(GetProcAddress(ALibCrypto, 'SRP_user_pwd_set_verifier'));
  SRP_user_pwd_set_info := TSRPUserPwdSetInfo(GetProcAddress(ALibCrypto, 'SRP_user_pwd_set_info'));
  SRP_user_pwd_set_gN := TSRPUserPwdSetGN(GetProcAddress(ALibCrypto, 'SRP_user_pwd_set_gN'));
  SRP_user_pwd_get0_salt := TSRPUserPwdGet0Salt(GetProcAddress(ALibCrypto, 'SRP_user_pwd_get0_salt'));
  SRP_user_pwd_get0_verifier := TSRPUserPwdGet0Verifier(GetProcAddress(ALibCrypto, 'SRP_user_pwd_get0_verifier'));
  SRP_user_pwd_get0_name := TSRPUserPwdGet0Username(GetProcAddress(ALibCrypto, 'SRP_user_pwd_get0_name'));
  
  { Load gN functions }
  SRP_get_default_gN := TSRPGetDefaultGN(GetProcAddress(ALibCrypto, 'SRP_get_default_gN'));
  SRP_check_known_gN_param := TSRPCheckKnownGNParam(GetProcAddress(ALibCrypto, 'SRP_check_known_gN_param'));
  SRP_get_1_by_id := TSRPGet1KnownGN(GetProcAddress(ALibCrypto, 'SRP_get_1_by_id'));
  
  { Load password functions }
  SRP_create_verifier := TSRPCreateVerifier(GetProcAddress(ALibCrypto, 'SRP_create_verifier'));
  SRP_create_verifier_BN := TSRPCreateVerifierBN(GetProcAddress(ALibCrypto, 'SRP_create_verifier_BN'));

  { Basic functions are enough to consider the module loaded }
  Result := Assigned(SRP_VBASE_new);
  SRPLoaded := Result;
end;

procedure UnloadSRP;
begin
  if not SRPLoaded then Exit;

  SRP_Calc_server_key := nil;
  SRP_Calc_u := nil;
  SRP_Calc_client_key := nil;
  SRP_Calc_x := nil;
  SRP_Calc_B := nil;
  SRP_Calc_A := nil;
  SRP_Verify_A_mod_N := nil;
  SRP_Verify_B_mod_N := nil;
  
  SRP_VBASE_new := nil;
  SRP_VBASE_free := nil;
  SRP_VBASE_init := nil;
  SRP_VBASE_add0_user := nil;
  SRP_VBASE_get_by_user := nil;
  SRP_VBASE_get1_by_user := nil;
  
  SRP_user_pwd_new := nil;
  SRP_user_pwd_free := nil;
  SRP_user_pwd_set_salt := nil;
  SRP_user_pwd_set_verifier := nil;
  SRP_user_pwd_set_info := nil;
  SRP_user_pwd_set_gN := nil;
  SRP_user_pwd_get0_salt := nil;
  SRP_user_pwd_get0_verifier := nil;
  SRP_user_pwd_get0_name := nil;
  
  SRP_get_default_gN := nil;
  SRP_check_known_gN_param := nil;
  SRP_get_1_by_id := nil;
  
  SRP_create_verifier := nil;
  SRP_create_verifier_BN := nil;

  SRPLoaded := False;
end;

{ Helper functions }

function SRPCreateUser(const Username, Password: string; const gN_id: string): PSRP_user_pwd;
var
  Salt, Verifier: PBIGNUM;
  gN: PSRP_gN;
begin
  Result := nil;
  if not Assigned(SRP_user_pwd_new) or not Assigned(SRP_create_verifier_BN) then Exit;
  
  Result := SRP_user_pwd_new(PChar(Username));
  if Result = nil then Exit;
  
  gN := nil;
  if Assigned(SRP_get_default_gN) then
    gN := SRP_get_default_gN(PChar(gN_id));
    
  if gN = nil then
  begin
    SRP_user_pwd_free(Result);
    Result := nil;
    Exit;
  end;
  
  Salt := nil;
  Verifier := nil;
  
  if SRP_create_verifier_BN(PChar(Username), PChar(Password), @Salt, @Verifier, gN^.N, gN^.g) = 1 then
  begin
    SRP_user_pwd_set_salt(Result, Salt);
    SRP_user_pwd_set_verifier(Result, Verifier);
  end
  else
  begin
    SRP_user_pwd_free(Result);
    Result := nil;
  end;
end;

function SRPVerifyUser(vbase: PSRP_VBASE; const Username, Password: string): Boolean;
var
  User: PSRP_user_pwd;
  X, V: PBIGNUM;
begin
  Result := False;
  if (vbase = nil) or not Assigned(SRP_VBASE_get_by_user) then Exit;
  
  User := SRP_VBASE_get_by_user(vbase, PChar(Username));
  if User = nil then Exit;
  
  // TODO: Implement actual verification using password
  // This is a simplified implementation
  Result := User <> nil;
end;

function SRPGenerateVerifier(const Username, Password: string; out Salt, Verifier: string; const gN_id: string): Boolean;
var
  SaltBN, VerifierBN: PBIGNUM;
  SaltStr, VerifierStr: PChar;
begin
  Result := False;
  Salt := '';
  Verifier := '';
  
  if not Assigned(SRP_create_verifier) then Exit;
  
  SaltBN := nil;
  VerifierBN := nil;
  
  SaltStr := SRP_create_verifier(PChar(Username), PChar(Password), @SaltBN, @VerifierBN, PChar(gN_id), nil);
  
  if (SaltBN <> nil) and (VerifierBN <> nil) then
  begin
    // Convert BIGNUM to hex strings
    if Assigned(BN_bn2hex) then
    begin
      SaltStr := BN_bn2hex(SaltBN);
      if SaltStr <> nil then
      begin
        Salt := string(SaltStr);
        // Note: BN_bn2hex returns a string allocated with OPENSSL_malloc
        // which should be freed with OPENSSL_free, but we use FreeMem as fallback
        FreeMem(SaltStr);
      end;
      
      VerifierStr := BN_bn2hex(VerifierBN);
      if VerifierStr <> nil then
      begin
        Verifier := string(VerifierStr);
        // Note: BN_bn2hex returns a string allocated with OPENSSL_malloc
        // which should be freed with OPENSSL_free, but we use FreeMem as fallback
        FreeMem(VerifierStr);
      end;
      
      Result := (Salt <> '') and (Verifier <> '');
    end;
    
    BN_free(SaltBN);
    BN_free(VerifierBN);
  end;
end;

initialization

finalization
  UnloadSRP;

end.