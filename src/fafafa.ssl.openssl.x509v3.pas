unit fafafa.ssl.openssl.x509v3;

{$mode ObjFPC}{$H+}
{$I fafafa.ssl.openssl.inc}

interface

uses
  Classes, SysUtils, fafafa.ssl.openssl.types;

type
  // X509V3 types
  PX509V3_CTX = ^X509V3_CTX;
  X509V3_CTX = record
    flags: Integer;
    issuer_cert: PX509;
    subject_cert: PX509;
    subject_req: PX509_REQ;
    crl: PX509_CRL;
    db_meth: Pointer;
    db: Pointer;
  end;
  
  PX509_EXTENSION_METHOD = ^X509_EXTENSION_METHOD;
  X509_EXTENSION_METHOD = record
    ext_nid: Integer;
    ext_flags: Integer;
    it: PASN1_ITEM;
    ext_new: Pointer;
    ext_free: Pointer;
    d2i: Pointer;
    i2d: Pointer;
    i2s: Pointer;
    s2i: Pointer;
    i2v: Pointer;
    v2i: Pointer;
    i2r: Pointer;
    r2i: Pointer;
    usr_data: Pointer;
  end;
  
  PAUTHORITY_INFO_ACCESS = ^AUTHORITY_INFO_ACCESS;
  AUTHORITY_INFO_ACCESS = record
    method: PASN1_OBJECT;
    location: Pointer; // GENERAL_NAME
  end;
  
  PAUTHORITY_KEYID = ^AUTHORITY_KEYID;
  AUTHORITY_KEYID = record
    keyid: PASN1_OCTET_STRING;
    issuer: Pointer; // GENERAL_NAMES
    serial: PASN1_INTEGER;
  end;
  
  PBASIC_CONSTRAINTS = ^BASIC_CONSTRAINTS;
  BASIC_CONSTRAINTS = record
    ca: Integer;
    pathlen: PASN1_INTEGER;
  end;

const
  // X509V3 extension flags
  X509V3_EXT_DYNAMIC      = $1;
  X509V3_EXT_CTX_DEP      = $2;
  X509V3_EXT_MULTILINE    = $4;
  
  // X509V3 context flags
  X509V3_CTX_TEST         = $1;
  X509V3_CTX_REPLACE      = $2;
  
  // Key usage bits
  KU_DIGITAL_SIGNATURE    = $0080;
  KU_NON_REPUDIATION      = $0040;
  KU_KEY_ENCIPHERMENT     = $0020;
  KU_DATA_ENCIPHERMENT    = $0010;
  KU_KEY_AGREEMENT        = $0008;
  KU_KEY_CERT_SIGN        = $0004;
  KU_CRL_SIGN             = $0002;
  KU_ENCIPHER_ONLY        = $0001;
  KU_DECIPHER_ONLY        = $8000;
  
  // Extended key usage
  XKU_SSL_SERVER          = $1;
  XKU_SSL_CLIENT          = $2;
  XKU_SMIME               = $4;
  XKU_CODE_SIGN           = $8;
  XKU_SGC                 = $10;
  XKU_OCSP_SIGN           = $20;
  XKU_TIMESTAMP           = $40;
  XKU_DVCS                = $80;
  XKU_ANYEKU              = $100;

var
  // X509V3 context functions
  X509V3_set_ctx: procedure(ctx: PX509V3_CTX; issuer: PX509; subject: PX509;
    req: PX509_REQ; crl: PX509_CRL; flags: Integer); cdecl = nil;
  X509V3_set_ctx_test: procedure(ctx: PX509V3_CTX); cdecl = nil;
  X509V3_set_ctx_nodb: procedure(ctx: PX509V3_CTX); cdecl = nil;
  
  // Extension creation functions
  X509V3_EXT_conf: function(conf: Pointer; ctx: PX509V3_CTX; 
    const name: PAnsiChar; const value: PAnsiChar): PX509_EXTENSION; cdecl = nil;
  X509V3_EXT_conf_nid: function(conf: Pointer; ctx: PX509V3_CTX; 
    ext_nid: Integer; const value: PAnsiChar): PX509_EXTENSION; cdecl = nil;
  X509V3_EXT_nconf: function(conf: Pointer; ctx: PX509V3_CTX; 
    const name: PAnsiChar; const value: PAnsiChar): PX509_EXTENSION; cdecl = nil;
  X509V3_EXT_nconf_nid: function(conf: Pointer; ctx: PX509V3_CTX; 
    ext_nid: Integer; const value: PAnsiChar): PX509_EXTENSION; cdecl = nil;
  
  // Extension add functions
  X509V3_EXT_add_conf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; cert: PX509): Integer; cdecl = nil;
  X509V3_EXT_add_nconf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; cert: PX509): Integer; cdecl = nil;
  X509V3_EXT_CRL_add_conf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; crl: PX509_CRL): Integer; cdecl = nil;
  X509V3_EXT_CRL_add_nconf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; crl: PX509_CRL): Integer; cdecl = nil;
  X509V3_EXT_REQ_add_conf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; req: PX509_REQ): Integer; cdecl = nil;
  X509V3_EXT_REQ_add_nconf: function(conf: Pointer; ctx: PX509V3_CTX;
    const section: PAnsiChar; req: PX509_REQ): Integer; cdecl = nil;
  
  // Extension print functions
  X509V3_EXT_print: function(output: PBIO; ext: PX509_EXTENSION; 
    flag: LongWord; indent: Integer): Integer; cdecl = nil;
  X509V3_EXT_print_fp: function(output: Pointer; ext: PX509_EXTENSION; 
    flag: Integer; indent: Integer): Integer; cdecl = nil;
  X509V3_extensions_print: function(output: PBIO; const title: PAnsiChar;
    exts: Pointer; flag: LongWord; indent: Integer): Integer; cdecl = nil;
  
  // Extension get/add functions
  X509V3_EXT_get: function(ext: PX509_EXTENSION): PX509_EXTENSION_METHOD; cdecl = nil;
  X509V3_EXT_get_nid: function(nid: Integer): PX509_EXTENSION_METHOD; cdecl = nil;
  X509V3_EXT_add: function(ext: PX509_EXTENSION_METHOD): Integer; cdecl = nil;
  X509V3_EXT_add_alias: function(nid_to: Integer; nid_from: Integer): Integer; cdecl = nil;
  X509V3_EXT_cleanup: procedure(); cdecl = nil;
  
  // Extension data functions
  X509V3_EXT_d2i: function(ext: PX509_EXTENSION): Pointer; cdecl = nil;
  X509V3_EXT_i2d: function(ext_nid: Integer; crit: Integer; 
    ext_struc: Pointer): PX509_EXTENSION; cdecl = nil;
  X509V3_add1_i2d: function(x: PPX509_EXTENSION; nid: Integer; value: Pointer; 
    crit: Integer; flags: LongWord): Integer; cdecl = nil;
  X509V3_get_d2i: function(const x: Pointer; nid: Integer; 
    var crit: Integer; var idx: Integer): Pointer; cdecl = nil;
    
  // Specific extension functions
  BASIC_CONSTRAINTS_new: function(): PBASIC_CONSTRAINTS; cdecl = nil;
  BASIC_CONSTRAINTS_free: procedure(a: PBASIC_CONSTRAINTS); cdecl = nil;
  d2i_BASIC_CONSTRAINTS: function(a: PPBASIC_CONSTRAINTS; 
    const input: PPByte; len: LongInt): PBASIC_CONSTRAINTS; cdecl = nil;
  i2d_BASIC_CONSTRAINTS: function(a: PBASIC_CONSTRAINTS; 
    output: PPByte): Integer; cdecl = nil;
    
  AUTHORITY_KEYID_new: function(): PAUTHORITY_KEYID; cdecl = nil;
  AUTHORITY_KEYID_free: procedure(a: PAUTHORITY_KEYID); cdecl = nil;
  d2i_AUTHORITY_KEYID: function(a: PPAUTHORITY_KEYID; 
    const input: PPByte; len: LongInt): PAUTHORITY_KEYID; cdecl = nil;
  i2d_AUTHORITY_KEYID: function(a: PAUTHORITY_KEYID; 
    output: PPByte): Integer; cdecl = nil;
    
  AUTHORITY_INFO_ACCESS_new: function(): PAUTHORITY_INFO_ACCESS; cdecl = nil;
  AUTHORITY_INFO_ACCESS_free: procedure(a: PAUTHORITY_INFO_ACCESS); cdecl = nil;
  
  // Name constraint functions
  NAME_CONSTRAINTS_check: function(x: PX509; nc: Pointer): Integer; cdecl = nil;
  NAME_CONSTRAINTS_check_CN: function(x: PX509; nc: Pointer): Integer; cdecl = nil;
  
  // Policy functions
  X509_policy_check: function(ptree: Pointer; pexplicit_policy: PInteger;
    certs: Pointer; policy_oids: Pointer; flags: Cardinal): Integer; cdecl = nil;
  X509_policy_tree_free: procedure(tree: Pointer); cdecl = nil;
  X509_policy_tree_level_count: function(const tree: Pointer): Integer; cdecl = nil;
  X509_policy_tree_get0_level: function(const tree: Pointer; 
    i: Integer): Pointer; cdecl = nil;

procedure LoadX509V3Functions(AHandle: TLibHandle);
procedure UnloadX509V3Functions;

// Helper functions
function X509AddBasicConstraints(Cert: PX509; CA: Boolean; PathLen: Integer = -1): Boolean;
function X509AddKeyUsage(Cert: PX509; Usage: Cardinal): Boolean;
function X509AddExtKeyUsage(Cert: PX509; const Usage: string): Boolean;
function X509AddSubjectAltName(Cert: PX509; const DNS: string): Boolean;

implementation

uses
  fafafa.ssl.openssl.utils, fafafa.ssl.openssl.x509;

procedure LoadX509V3Functions(AHandle: TLibHandle);
begin
  if AHandle = 0 then Exit;
  
  // Context functions
  X509V3_set_ctx := GetProcedureAddress(AHandle, 'X509V3_set_ctx');
  X509V3_set_ctx_test := GetProcedureAddress(AHandle, 'X509V3_set_ctx_test');
  X509V3_set_ctx_nodb := GetProcedureAddress(AHandle, 'X509V3_set_ctx_nodb');
  
  // Extension creation functions
  X509V3_EXT_conf := GetProcedureAddress(AHandle, 'X509V3_EXT_conf');
  X509V3_EXT_conf_nid := GetProcedureAddress(AHandle, 'X509V3_EXT_conf_nid');
  X509V3_EXT_nconf := GetProcedureAddress(AHandle, 'X509V3_EXT_nconf');
  X509V3_EXT_nconf_nid := GetProcedureAddress(AHandle, 'X509V3_EXT_nconf_nid');
  
  // Extension add functions
  X509V3_EXT_add_conf := GetProcedureAddress(AHandle, 'X509V3_EXT_add_conf');
  X509V3_EXT_add_nconf := GetProcedureAddress(AHandle, 'X509V3_EXT_add_nconf');
  X509V3_EXT_CRL_add_conf := GetProcedureAddress(AHandle, 'X509V3_EXT_CRL_add_conf');
  X509V3_EXT_CRL_add_nconf := GetProcedureAddress(AHandle, 'X509V3_EXT_CRL_add_nconf');
  X509V3_EXT_REQ_add_conf := GetProcedureAddress(AHandle, 'X509V3_EXT_REQ_add_conf');
  X509V3_EXT_REQ_add_nconf := GetProcedureAddress(AHandle, 'X509V3_EXT_REQ_add_nconf');
  
  // Extension print functions
  X509V3_EXT_print := GetProcedureAddress(AHandle, 'X509V3_EXT_print');
  X509V3_EXT_print_fp := GetProcedureAddress(AHandle, 'X509V3_EXT_print_fp');
  X509V3_extensions_print := GetProcedureAddress(AHandle, 'X509V3_extensions_print');
  
  // Extension get/add functions
  X509V3_EXT_get := GetProcedureAddress(AHandle, 'X509V3_EXT_get');
  X509V3_EXT_get_nid := GetProcedureAddress(AHandle, 'X509V3_EXT_get_nid');
  X509V3_EXT_add := GetProcedureAddress(AHandle, 'X509V3_EXT_add');
  X509V3_EXT_add_alias := GetProcedureAddress(AHandle, 'X509V3_EXT_add_alias');
  X509V3_EXT_cleanup := GetProcedureAddress(AHandle, 'X509V3_EXT_cleanup');
  
  // Extension data functions
  X509V3_EXT_d2i := GetProcedureAddress(AHandle, 'X509V3_EXT_d2i');
  X509V3_EXT_i2d := GetProcedureAddress(AHandle, 'X509V3_EXT_i2d');
  X509V3_add1_i2d := GetProcedureAddress(AHandle, 'X509V3_add1_i2d');
  X509V3_get_d2i := GetProcedureAddress(AHandle, 'X509V3_get_d2i');
  
  // Basic constraints functions
  BASIC_CONSTRAINTS_new := GetProcedureAddress(AHandle, 'BASIC_CONSTRAINTS_new');
  BASIC_CONSTRAINTS_free := GetProcedureAddress(AHandle, 'BASIC_CONSTRAINTS_free');
  d2i_BASIC_CONSTRAINTS := GetProcedureAddress(AHandle, 'd2i_BASIC_CONSTRAINTS');
  i2d_BASIC_CONSTRAINTS := GetProcedureAddress(AHandle, 'i2d_BASIC_CONSTRAINTS');
  
  // Authority key ID functions
  AUTHORITY_KEYID_new := GetProcedureAddress(AHandle, 'AUTHORITY_KEYID_new');
  AUTHORITY_KEYID_free := GetProcedureAddress(AHandle, 'AUTHORITY_KEYID_free');
  d2i_AUTHORITY_KEYID := GetProcedureAddress(AHandle, 'd2i_AUTHORITY_KEYID');
  i2d_AUTHORITY_KEYID := GetProcedureAddress(AHandle, 'i2d_AUTHORITY_KEYID');
  
  // Authority info access functions
  AUTHORITY_INFO_ACCESS_new := GetProcedureAddress(AHandle, 'AUTHORITY_INFO_ACCESS_new');
  AUTHORITY_INFO_ACCESS_free := GetProcedureAddress(AHandle, 'AUTHORITY_INFO_ACCESS_free');
  
  // Name constraint functions
  NAME_CONSTRAINTS_check := GetProcedureAddress(AHandle, 'NAME_CONSTRAINTS_check');
  NAME_CONSTRAINTS_check_CN := GetProcedureAddress(AHandle, 'NAME_CONSTRAINTS_check_CN');
  
  // Policy functions
  X509_policy_check := GetProcedureAddress(AHandle, 'X509_policy_check');
  X509_policy_tree_free := GetProcedureAddress(AHandle, 'X509_policy_tree_free');
  X509_policy_tree_level_count := GetProcedureAddress(AHandle, 'X509_policy_tree_level_count');
  X509_policy_tree_get0_level := GetProcedureAddress(AHandle, 'X509_policy_tree_get0_level');
end;

procedure UnloadX509V3Functions;
begin
  X509V3_set_ctx := nil;
  X509V3_set_ctx_test := nil;
  X509V3_set_ctx_nodb := nil;
  X509V3_EXT_conf := nil;
  X509V3_EXT_conf_nid := nil;
  X509V3_EXT_nconf := nil;
  X509V3_EXT_nconf_nid := nil;
  X509V3_EXT_add_conf := nil;
  X509V3_EXT_add_nconf := nil;
  X509V3_EXT_CRL_add_conf := nil;
  X509V3_EXT_CRL_add_nconf := nil;
  X509V3_EXT_REQ_add_conf := nil;
  X509V3_EXT_REQ_add_nconf := nil;
  X509V3_EXT_print := nil;
  X509V3_EXT_print_fp := nil;
  X509V3_extensions_print := nil;
  X509V3_EXT_get := nil;
  X509V3_EXT_get_nid := nil;
  X509V3_EXT_add := nil;
  X509V3_EXT_add_alias := nil;
  X509V3_EXT_cleanup := nil;
  X509V3_EXT_d2i := nil;
  X509V3_EXT_i2d := nil;
  X509V3_add1_i2d := nil;
  X509V3_get_d2i := nil;
  BASIC_CONSTRAINTS_new := nil;
  BASIC_CONSTRAINTS_free := nil;
  d2i_BASIC_CONSTRAINTS := nil;
  i2d_BASIC_CONSTRAINTS := nil;
  AUTHORITY_KEYID_new := nil;
  AUTHORITY_KEYID_free := nil;
  d2i_AUTHORITY_KEYID := nil;
  i2d_AUTHORITY_KEYID := nil;
  AUTHORITY_INFO_ACCESS_new := nil;
  AUTHORITY_INFO_ACCESS_free := nil;
  NAME_CONSTRAINTS_check := nil;
  NAME_CONSTRAINTS_check_CN := nil;
  X509_policy_check := nil;
  X509_policy_tree_free := nil;
  X509_policy_tree_level_count := nil;
  X509_policy_tree_get0_level := nil;
end;

function X509AddBasicConstraints(Cert: PX509; CA: Boolean; PathLen: Integer): Boolean;
var
  bc: PBASIC_CONSTRAINTS;
  ext: PX509_EXTENSION;
begin
  Result := False;
  if not Assigned(BASIC_CONSTRAINTS_new) or not Assigned(X509V3_EXT_i2d) then
    Exit;
    
  bc := BASIC_CONSTRAINTS_new();
  if bc = nil then Exit;
  
  try
    if CA then
      bc^.ca := $FF
    else
      bc^.ca := 0;
      
    if PathLen >= 0 then
    begin
      // Set pathlen if specified
      // Note: Would need ASN1_INTEGER_set function
    end;
    
    ext := X509V3_EXT_i2d(NID_basic_constraints, 1, bc);
    if ext <> nil then
    begin
      Result := X509_add_ext(Cert, ext, -1) = 1;
      X509_EXTENSION_free(ext);
    end;
  finally
    BASIC_CONSTRAINTS_free(bc);
  end;
end;

function X509AddKeyUsage(Cert: PX509; Usage: Cardinal): Boolean;
begin
  Result := False;
  // Implementation would require ASN1_BIT_STRING functions
end;

function X509AddExtKeyUsage(Cert: PX509; const Usage: string): Boolean;
begin
  Result := False;
  // Implementation would require extended key usage OID handling
end;

function X509AddSubjectAltName(Cert: PX509; const DNS: string): Boolean;
begin
  Result := False;
  // Implementation would require GENERAL_NAME handling
end;

end.