unit fafafa.ssl.openssl.api.pem;

{$mode Delphi}
{$H+}

interface

uses
  SysUtils, Classes, dynlibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp;

type
  // PEM 密码回调函数类型
  Tpem_password_cb = function(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;

  // PEM 函数类型
  type
    // 基础 PEM 读写函数
    TPEM_read_bio = function(bp: PBIO; name: PPAnsiChar; header: PPAnsiChar; 
      data: PPByte; len: PInteger): Integer; cdecl;
    TPEM_read_bio_ex = function(bp: PBIO; name: PPAnsiChar; header: PPAnsiChar; 
      data: PPByte; len: PInteger; flags: Cardinal): Integer; cdecl;
    TPEM_write_bio = function(bp: PBIO; const name: PAnsiChar; const header: PAnsiChar;
      const data: PByte; len: Integer): Integer; cdecl;
    TPEM_bytes_read_bio = function(pdata: PPByte; plen: PInteger; pnm: PPAnsiChar;
      const name: PAnsiChar; bp: PBIO; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_bytes_read_bio_secmem = function(pdata: PPByte; plen: PInteger; pnm: PPAnsiChar;
      const name: PAnsiChar; bp: PBIO; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;

    // X509 证书 PEM 函数
    TPEM_read_bio_X509 = function(bp: PBIO; x: PPX509; cb: Tpem_password_cb; u: Pointer): PX509; cdecl;
    TPEM_write_bio_X509 = function(bp: PBIO; x: PX509): Integer; cdecl;
    TPEM_read_bio_X509_AUX = function(bp: PBIO; x: PPX509; cb: Tpem_password_cb; u: Pointer): PX509; cdecl;
    TPEM_write_bio_X509_AUX = function(bp: PBIO; x: PX509): Integer; cdecl;
    TPEM_read_bio_X509_REQ = function(bp: PBIO; x: PPX509_REQ; cb: Tpem_password_cb; u: Pointer): PX509_REQ; cdecl;
    TPEM_write_bio_X509_REQ = function(bp: PBIO; x: PX509_REQ): Integer; cdecl;
    TPEM_write_bio_X509_REQ_NEW = function(bp: PBIO; x: PX509_REQ): Integer; cdecl;
    TPEM_read_bio_X509_CRL = function(bp: PBIO; x: PPX509_CRL; cb: Tpem_password_cb; u: Pointer): PX509_CRL; cdecl;
    TPEM_write_bio_X509_CRL = function(bp: PBIO; x: PX509_CRL): Integer; cdecl;

    // 私钥 PEM 函数
    TPEM_read_bio_PrivateKey = function(bp: PBIO; x: PPEVP_PKEY; cb: Tpem_password_cb; u: Pointer): PEVP_PKEY; cdecl;
    TPEM_write_bio_PrivateKey = function(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_write_bio_PrivateKey_traditional = function(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_write_bio_PKCS8PrivateKey = function(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER;
      kstr: PAnsiChar; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_write_bio_PKCS8PrivateKey_nid = function(bp: PBIO; x: PEVP_PKEY; nid: Integer;
      kstr: PAnsiChar; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;

    // 公钥 PEM 函数
    TPEM_read_bio_PUBKEY = function(bp: PBIO; x: PPEVP_PKEY; cb: Tpem_password_cb; u: Pointer): PEVP_PKEY; cdecl;
    TPEM_write_bio_PUBKEY = function(bp: PBIO; x: PEVP_PKEY): Integer; cdecl;

    // RSA 密钥 PEM 函数
    TPEM_read_bio_RSAPrivateKey = function(bp: PBIO; x: PPRSA; cb: Tpem_password_cb; u: Pointer): PRSA; cdecl;
    TPEM_write_bio_RSAPrivateKey = function(bp: PBIO; x: PRSA; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_read_bio_RSAPublicKey = function(bp: PBIO; x: PPRSA; cb: Tpem_password_cb; u: Pointer): PRSA; cdecl;
    TPEM_write_bio_RSAPublicKey = function(bp: PBIO; const x: PRSA): Integer; cdecl;
    TPEM_read_bio_RSA_PUBKEY = function(bp: PBIO; x: PPRSA; cb: Tpem_password_cb; u: Pointer): PRSA; cdecl;
    TPEM_write_bio_RSA_PUBKEY = function(bp: PBIO; x: PRSA): Integer; cdecl;

    // DSA 密钥 PEM 函数
    TPEM_read_bio_DSAPrivateKey = function(bp: PBIO; x: PPDSA; cb: Tpem_password_cb; u: Pointer): PDSA; cdecl;
    TPEM_write_bio_DSAPrivateKey = function(bp: PBIO; x: PDSA; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_read_bio_DSA_PUBKEY = function(bp: PBIO; x: PPDSA; cb: Tpem_password_cb; u: Pointer): PDSA; cdecl;
    TPEM_write_bio_DSA_PUBKEY = function(bp: PBIO; x: PDSA): Integer; cdecl;
    TPEM_read_bio_DSAparams = function(bp: PBIO; x: PPDSA; cb: Tpem_password_cb; u: Pointer): PDSA; cdecl;
    TPEM_write_bio_DSAparams = function(bp: PBIO; const x: PDSA): Integer; cdecl;

    // DH 参数 PEM 函数
    TPEM_read_bio_DHparams = function(bp: PBIO; x: PPDH; cb: Tpem_password_cb; u: Pointer): PDH; cdecl;
    TPEM_write_bio_DHparams = function(bp: PBIO; const x: PDH): Integer; cdecl;
    TPEM_write_bio_DHxparams = function(bp: PBIO; const x: PDH): Integer; cdecl;

    // EC 密钥 PEM 函数
    TPEM_read_bio_ECPrivateKey = function(bp: PBIO; x: PPEC_KEY; cb: Tpem_password_cb; u: Pointer): PEC_KEY; cdecl;
    TPEM_write_bio_ECPrivateKey = function(bp: PBIO; x: PEC_KEY; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;
    TPEM_read_bio_EC_PUBKEY = function(bp: PBIO; x: PPEC_KEY; cb: Tpem_password_cb; u: Pointer): PEC_KEY; cdecl;
    TPEM_write_bio_EC_PUBKEY = function(bp: PBIO; x: PEC_KEY): Integer; cdecl;
    TPEM_read_bio_ECPKParameters = function(bp: PBIO; x: PPEC_GROUP; cb: Tpem_password_cb; u: Pointer): PEC_GROUP; cdecl;
    TPEM_write_bio_ECPKParameters = function(bp: PBIO; const x: PEC_GROUP): Integer; cdecl;

    // PKCS7 PEM 函数
    TPEM_read_bio_PKCS7 = function(bp: PBIO; x: PPPKCS7; cb: Tpem_password_cb; u: Pointer): PPKCS7; cdecl;
    TPEM_write_bio_PKCS7 = function(bp: PBIO; x: PPKCS7): Integer; cdecl;
    TPEM_write_bio_PKCS7_stream = function(out_: PBIO; p7: PPKCS7; in_: PBIO; flags: Integer): Integer; cdecl;

    // PKCS8 函数
    TPEM_read_bio_PKCS8 = function(bp: PBIO; x: PPX509_SIG; cb: Tpem_password_cb; u: Pointer): PX509_SIG; cdecl;
    TPEM_write_bio_PKCS8 = function(bp: PBIO; x: PX509_SIG): Integer; cdecl;
    TPEM_read_bio_PKCS8_PRIV_KEY_INFO = function(bp: PBIO; x: PPPKCS8_PRIV_KEY_INFO; 
      cb: Tpem_password_cb; u: Pointer): PPKCS8_PRIV_KEY_INFO; cdecl;
    TPEM_write_bio_PKCS8_PRIV_KEY_INFO = function(bp: PBIO; x: PPKCS8_PRIV_KEY_INFO): Integer; cdecl;

    // Parameters 函数
    TPEM_read_bio_Parameters = function(bp: PBIO; x: PPEVP_PKEY): PEVP_PKEY; cdecl;
    TPEM_write_bio_Parameters = function(bp: PBIO; x: PEVP_PKEY): Integer; cdecl;

    // CMS 函数
    TPEM_read_bio_CMS = function(bp: PBIO; x: PPCMS_ContentInfo; cb: Tpem_password_cb; u: Pointer): PCMS_ContentInfo; cdecl;
    TPEM_write_bio_CMS = function(bp: PBIO; x: PCMS_ContentInfo): Integer; cdecl;
    TPEM_write_bio_CMS_stream = function(out_: PBIO; cms: PCMS_ContentInfo; in_: PBIO; flags: Integer): Integer; cdecl;

    // SSL Session PEM 函数
    TPEM_read_bio_SSL_SESSION = function(bp: PBIO; x: PPSSL_SESSION; cb: Tpem_password_cb; u: Pointer): PSSL_SESSION; cdecl;
    TPEM_write_bio_SSL_SESSION = function(bp: PBIO; x: PSSL_SESSION): Integer; cdecl;

    // 文件版本的 PEM 函数（使用 FILE*）
    TPEM_read_X509 = function(fp: Pointer; x: PPX509; cb: Tpem_password_cb; u: Pointer): PX509; cdecl;
    TPEM_write_X509 = function(fp: Pointer; x: PX509): Integer; cdecl;
    TPEM_read_PrivateKey = function(fp: Pointer; x: PPEVP_PKEY; cb: Tpem_password_cb; u: Pointer): PEVP_PKEY; cdecl;
    TPEM_write_PrivateKey = function(fp: Pointer; x: PEVP_PKEY; const enc: PEVP_CIPHER;
      kstr: PByte; klen: Integer; cb: Tpem_password_cb; u: Pointer): Integer; cdecl;

    // 实用函数
    TPEM_def_callback = function(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
    TPEM_dek_info = procedure(buf: PAnsiChar; const atype: PAnsiChar; len: Integer; str: PAnsiChar); cdecl;

var
  // 基础 PEM 读写函数
  PEM_read_bio: TPEM_read_bio = nil;
  PEM_read_bio_ex: TPEM_read_bio_ex = nil;
  PEM_write_bio: TPEM_write_bio = nil;
  PEM_bytes_read_bio: TPEM_bytes_read_bio = nil;
  PEM_bytes_read_bio_secmem: TPEM_bytes_read_bio_secmem = nil;

  // X509 证书 PEM 函数
  PEM_read_bio_X509: TPEM_read_bio_X509 = nil;
  PEM_write_bio_X509: TPEM_write_bio_X509 = nil;
  PEM_read_bio_X509_AUX: TPEM_read_bio_X509_AUX = nil;
  PEM_write_bio_X509_AUX: TPEM_write_bio_X509_AUX = nil;
  PEM_read_bio_X509_REQ: TPEM_read_bio_X509_REQ = nil;
  PEM_write_bio_X509_REQ: TPEM_write_bio_X509_REQ = nil;
  PEM_write_bio_X509_REQ_NEW: TPEM_write_bio_X509_REQ_NEW = nil;
  PEM_read_bio_X509_CRL: TPEM_read_bio_X509_CRL = nil;
  PEM_write_bio_X509_CRL: TPEM_write_bio_X509_CRL = nil;

  // 私钥 PEM 函数
  PEM_read_bio_PrivateKey: TPEM_read_bio_PrivateKey = nil;
  PEM_write_bio_PrivateKey: TPEM_write_bio_PrivateKey = nil;
  PEM_write_bio_PrivateKey_traditional: TPEM_write_bio_PrivateKey_traditional = nil;
  PEM_write_bio_PKCS8PrivateKey: TPEM_write_bio_PKCS8PrivateKey = nil;
  PEM_write_bio_PKCS8PrivateKey_nid: TPEM_write_bio_PKCS8PrivateKey_nid = nil;

  // 公钥 PEM 函数
  PEM_read_bio_PUBKEY: TPEM_read_bio_PUBKEY = nil;
  PEM_write_bio_PUBKEY: TPEM_write_bio_PUBKEY = nil;

  // RSA 密钥 PEM 函数
  PEM_read_bio_RSAPrivateKey: TPEM_read_bio_RSAPrivateKey = nil;
  PEM_write_bio_RSAPrivateKey: TPEM_write_bio_RSAPrivateKey = nil;
  PEM_read_bio_RSAPublicKey: TPEM_read_bio_RSAPublicKey = nil;
  PEM_write_bio_RSAPublicKey: TPEM_write_bio_RSAPublicKey = nil;
  PEM_read_bio_RSA_PUBKEY: TPEM_read_bio_RSA_PUBKEY = nil;
  PEM_write_bio_RSA_PUBKEY: TPEM_write_bio_RSA_PUBKEY = nil;

  // DSA 密钥 PEM 函数
  PEM_read_bio_DSAPrivateKey: TPEM_read_bio_DSAPrivateKey = nil;
  PEM_write_bio_DSAPrivateKey: TPEM_write_bio_DSAPrivateKey = nil;
  PEM_read_bio_DSA_PUBKEY: TPEM_read_bio_DSA_PUBKEY = nil;
  PEM_write_bio_DSA_PUBKEY: TPEM_write_bio_DSA_PUBKEY = nil;
  PEM_read_bio_DSAparams: TPEM_read_bio_DSAparams = nil;
  PEM_write_bio_DSAparams: TPEM_write_bio_DSAparams = nil;

  // DH 参数 PEM 函数
  PEM_read_bio_DHparams: TPEM_read_bio_DHparams = nil;
  PEM_write_bio_DHparams: TPEM_write_bio_DHparams = nil;
  PEM_write_bio_DHxparams: TPEM_write_bio_DHxparams = nil;

  // EC 密钥 PEM 函数
  PEM_read_bio_ECPrivateKey: TPEM_read_bio_ECPrivateKey = nil;
  PEM_write_bio_ECPrivateKey: TPEM_write_bio_ECPrivateKey = nil;
  PEM_read_bio_EC_PUBKEY: TPEM_read_bio_EC_PUBKEY = nil;
  PEM_write_bio_EC_PUBKEY: TPEM_write_bio_EC_PUBKEY = nil;
  PEM_read_bio_ECPKParameters: TPEM_read_bio_ECPKParameters = nil;
  PEM_write_bio_ECPKParameters: TPEM_write_bio_ECPKParameters = nil;

  // PKCS7 PEM 函数
  PEM_read_bio_PKCS7: TPEM_read_bio_PKCS7 = nil;
  PEM_write_bio_PKCS7: TPEM_write_bio_PKCS7 = nil;
  PEM_write_bio_PKCS7_stream: TPEM_write_bio_PKCS7_stream = nil;

  // PKCS8 函数
  PEM_read_bio_PKCS8: TPEM_read_bio_PKCS8 = nil;
  PEM_write_bio_PKCS8: TPEM_write_bio_PKCS8 = nil;
  PEM_read_bio_PKCS8_PRIV_KEY_INFO: TPEM_read_bio_PKCS8_PRIV_KEY_INFO = nil;
  PEM_write_bio_PKCS8_PRIV_KEY_INFO: TPEM_write_bio_PKCS8_PRIV_KEY_INFO = nil;

  // Parameters 函数
  PEM_read_bio_Parameters: TPEM_read_bio_Parameters = nil;
  PEM_write_bio_Parameters: TPEM_write_bio_Parameters = nil;

  // CMS 函数
  PEM_read_bio_CMS: TPEM_read_bio_CMS = nil;
  PEM_write_bio_CMS: TPEM_write_bio_CMS = nil;
  PEM_write_bio_CMS_stream: TPEM_write_bio_CMS_stream = nil;

  // SSL Session PEM 函数
  PEM_read_bio_SSL_SESSION: TPEM_read_bio_SSL_SESSION = nil;
  PEM_write_bio_SSL_SESSION: TPEM_write_bio_SSL_SESSION = nil;

  // 文件版本的 PEM 函数
  PEM_read_X509: TPEM_read_X509 = nil;
  PEM_write_X509: TPEM_write_X509 = nil;
  PEM_read_PrivateKey: TPEM_read_PrivateKey = nil;
  PEM_write_PrivateKey: TPEM_write_PrivateKey = nil;

  // 实用函数
  PEM_def_callback: TPEM_def_callback = nil;
  PEM_dek_info: TPEM_dek_info = nil;

// 加载和卸载函数
function LoadOpenSSLPEM(const ACryptoLib: THandle): Boolean;
procedure UnloadOpenSSLPEM;

// 辅助函数
function LoadPrivateKeyFromPEM(const AFileName: string; const APassword: string = ''): PEVP_PKEY;
function LoadPublicKeyFromPEM(const AFileName: string): PEVP_PKEY;
function LoadCertificateFromPEM(const AFileName: string): PX509;
function SavePrivateKeyToPEM(const AFileName: string; AKey: PEVP_PKEY; const APassword: string = ''): Boolean;
function SavePublicKeyToPEM(const AFileName: string; AKey: PEVP_PKEY): Boolean;
function SaveCertificateToPEM(const AFileName: string; ACert: PX509): Boolean;
function LoadPrivateKeyFromMemory(const AData: TBytes; const APassword: string = ''): PEVP_PKEY;
function LoadCertificateFromMemory(const AData: TBytes): PX509;

implementation

var
  FPEMLoaded: Boolean = False;

// 密码回调函数
function PasswordCallback(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
var
  Password: string;
begin
  if userdata <> nil then
  begin
    Password := string(PAnsiChar(userdata));
    if Length(Password) < size then
    begin
      StrPCopy(buf, AnsiString(Password));
      Result := Length(Password);
    end
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function LoadOpenSSLPEM(const ACryptoLib: THandle): Boolean;
begin
  if FPEMLoaded then
    Exit(True);

  if ACryptoLib = 0 then
    Exit(False);

  // 加载基础 PEM 函数
  PEM_read_bio := GetProcAddress(ACryptoLib, 'PEM_read_bio');
  PEM_read_bio_ex := GetProcAddress(ACryptoLib, 'PEM_read_bio_ex');
  PEM_write_bio := GetProcAddress(ACryptoLib, 'PEM_write_bio');
  PEM_bytes_read_bio := GetProcAddress(ACryptoLib, 'PEM_bytes_read_bio');
  PEM_bytes_read_bio_secmem := GetProcAddress(ACryptoLib, 'PEM_bytes_read_bio_secmem');

  // 加载 X509 证书 PEM 函数
  PEM_read_bio_X509 := GetProcAddress(ACryptoLib, 'PEM_read_bio_X509');
  PEM_write_bio_X509 := GetProcAddress(ACryptoLib, 'PEM_write_bio_X509');
  PEM_read_bio_X509_AUX := GetProcAddress(ACryptoLib, 'PEM_read_bio_X509_AUX');
  PEM_write_bio_X509_AUX := GetProcAddress(ACryptoLib, 'PEM_write_bio_X509_AUX');
  PEM_read_bio_X509_REQ := GetProcAddress(ACryptoLib, 'PEM_read_bio_X509_REQ');
  PEM_write_bio_X509_REQ := GetProcAddress(ACryptoLib, 'PEM_write_bio_X509_REQ');
  PEM_write_bio_X509_REQ_NEW := GetProcAddress(ACryptoLib, 'PEM_write_bio_X509_REQ_NEW');
  PEM_read_bio_X509_CRL := GetProcAddress(ACryptoLib, 'PEM_read_bio_X509_CRL');
  PEM_write_bio_X509_CRL := GetProcAddress(ACryptoLib, 'PEM_write_bio_X509_CRL');

  // 加载私钥 PEM 函数
  PEM_read_bio_PrivateKey := GetProcAddress(ACryptoLib, 'PEM_read_bio_PrivateKey');
  PEM_write_bio_PrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_PrivateKey');
  PEM_write_bio_PrivateKey_traditional := GetProcAddress(ACryptoLib, 'PEM_write_bio_PrivateKey_traditional');
  PEM_write_bio_PKCS8PrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS8PrivateKey');
  PEM_write_bio_PKCS8PrivateKey_nid := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS8PrivateKey_nid');

  // 加载公钥 PEM 函数
  PEM_read_bio_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_read_bio_PUBKEY');
  PEM_write_bio_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_write_bio_PUBKEY');

  // 加载 RSA 密钥 PEM 函数
  PEM_read_bio_RSAPrivateKey := GetProcAddress(ACryptoLib, 'PEM_read_bio_RSAPrivateKey');
  PEM_write_bio_RSAPrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_RSAPrivateKey');
  PEM_read_bio_RSAPublicKey := GetProcAddress(ACryptoLib, 'PEM_read_bio_RSAPublicKey');
  PEM_write_bio_RSAPublicKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_RSAPublicKey');
  PEM_read_bio_RSA_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_read_bio_RSA_PUBKEY');
  PEM_write_bio_RSA_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_write_bio_RSA_PUBKEY');

  // 加载 DSA 密钥 PEM 函数
  PEM_read_bio_DSAPrivateKey := GetProcAddress(ACryptoLib, 'PEM_read_bio_DSAPrivateKey');
  PEM_write_bio_DSAPrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_DSAPrivateKey');
  PEM_read_bio_DSA_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_read_bio_DSA_PUBKEY');
  PEM_write_bio_DSA_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_write_bio_DSA_PUBKEY');
  PEM_read_bio_DSAparams := GetProcAddress(ACryptoLib, 'PEM_read_bio_DSAparams');
  PEM_write_bio_DSAparams := GetProcAddress(ACryptoLib, 'PEM_write_bio_DSAparams');

  // 加载 DH 参数 PEM 函数
  PEM_read_bio_DHparams := GetProcAddress(ACryptoLib, 'PEM_read_bio_DHparams');
  PEM_write_bio_DHparams := GetProcAddress(ACryptoLib, 'PEM_write_bio_DHparams');
  PEM_write_bio_DHxparams := GetProcAddress(ACryptoLib, 'PEM_write_bio_DHxparams');

  // 加载 EC 密钥 PEM 函数
  PEM_read_bio_ECPrivateKey := GetProcAddress(ACryptoLib, 'PEM_read_bio_ECPrivateKey');
  PEM_write_bio_ECPrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_bio_ECPrivateKey');
  PEM_read_bio_EC_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_read_bio_EC_PUBKEY');
  PEM_write_bio_EC_PUBKEY := GetProcAddress(ACryptoLib, 'PEM_write_bio_EC_PUBKEY');
  PEM_read_bio_ECPKParameters := GetProcAddress(ACryptoLib, 'PEM_read_bio_ECPKParameters');
  PEM_write_bio_ECPKParameters := GetProcAddress(ACryptoLib, 'PEM_write_bio_ECPKParameters');

  // 加载 PKCS7 PEM 函数
  PEM_read_bio_PKCS7 := GetProcAddress(ACryptoLib, 'PEM_read_bio_PKCS7');
  PEM_write_bio_PKCS7 := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS7');
  PEM_write_bio_PKCS7_stream := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS7_stream');

  // 加载 PKCS8 函数
  PEM_read_bio_PKCS8 := GetProcAddress(ACryptoLib, 'PEM_read_bio_PKCS8');
  PEM_write_bio_PKCS8 := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS8');
  PEM_read_bio_PKCS8_PRIV_KEY_INFO := GetProcAddress(ACryptoLib, 'PEM_read_bio_PKCS8_PRIV_KEY_INFO');
  PEM_write_bio_PKCS8_PRIV_KEY_INFO := GetProcAddress(ACryptoLib, 'PEM_write_bio_PKCS8_PRIV_KEY_INFO');

  // 加载 Parameters 函数
  PEM_read_bio_Parameters := GetProcAddress(ACryptoLib, 'PEM_read_bio_Parameters');
  PEM_write_bio_Parameters := GetProcAddress(ACryptoLib, 'PEM_write_bio_Parameters');

  // 加载 CMS 函数
  PEM_read_bio_CMS := GetProcAddress(ACryptoLib, 'PEM_read_bio_CMS');
  PEM_write_bio_CMS := GetProcAddress(ACryptoLib, 'PEM_write_bio_CMS');
  PEM_write_bio_CMS_stream := GetProcAddress(ACryptoLib, 'PEM_write_bio_CMS_stream');

  // 加载 SSL Session PEM 函数
  PEM_read_bio_SSL_SESSION := GetProcAddress(ACryptoLib, 'PEM_read_bio_SSL_SESSION');
  PEM_write_bio_SSL_SESSION := GetProcAddress(ACryptoLib, 'PEM_write_bio_SSL_SESSION');

  // 加载文件版本的 PEM 函数
  PEM_read_X509 := GetProcAddress(ACryptoLib, 'PEM_read_X509');
  PEM_write_X509 := GetProcAddress(ACryptoLib, 'PEM_write_X509');
  PEM_read_PrivateKey := GetProcAddress(ACryptoLib, 'PEM_read_PrivateKey');
  PEM_write_PrivateKey := GetProcAddress(ACryptoLib, 'PEM_write_PrivateKey');

  // 加载实用函数
  PEM_def_callback := GetProcAddress(ACryptoLib, 'PEM_def_callback');
  PEM_dek_info := GetProcAddress(ACryptoLib, 'PEM_dek_info');

  FPEMLoaded := Assigned(PEM_read_bio_X509) and Assigned(PEM_write_bio_X509);
  Result := FPEMLoaded;
end;

procedure UnloadOpenSSLPEM;
begin
  if not FPEMLoaded then
    Exit;

  // 清理基础 PEM 函数
  PEM_read_bio := nil;
  PEM_read_bio_ex := nil;
  PEM_write_bio := nil;
  PEM_bytes_read_bio := nil;
  PEM_bytes_read_bio_secmem := nil;

  // 清理 X509 证书 PEM 函数
  PEM_read_bio_X509 := nil;
  PEM_write_bio_X509 := nil;
  PEM_read_bio_X509_AUX := nil;
  PEM_write_bio_X509_AUX := nil;
  PEM_read_bio_X509_REQ := nil;
  PEM_write_bio_X509_REQ := nil;
  PEM_write_bio_X509_REQ_NEW := nil;
  PEM_read_bio_X509_CRL := nil;
  PEM_write_bio_X509_CRL := nil;

  // 清理私钥 PEM 函数
  PEM_read_bio_PrivateKey := nil;
  PEM_write_bio_PrivateKey := nil;
  PEM_write_bio_PrivateKey_traditional := nil;
  PEM_write_bio_PKCS8PrivateKey := nil;
  PEM_write_bio_PKCS8PrivateKey_nid := nil;

  // 清理公钥 PEM 函数
  PEM_read_bio_PUBKEY := nil;
  PEM_write_bio_PUBKEY := nil;

  // 清理 RSA 密钥 PEM 函数
  PEM_read_bio_RSAPrivateKey := nil;
  PEM_write_bio_RSAPrivateKey := nil;
  PEM_read_bio_RSAPublicKey := nil;
  PEM_write_bio_RSAPublicKey := nil;
  PEM_read_bio_RSA_PUBKEY := nil;
  PEM_write_bio_RSA_PUBKEY := nil;

  // 清理 DSA 密钥 PEM 函数
  PEM_read_bio_DSAPrivateKey := nil;
  PEM_write_bio_DSAPrivateKey := nil;
  PEM_read_bio_DSA_PUBKEY := nil;
  PEM_write_bio_DSA_PUBKEY := nil;
  PEM_read_bio_DSAparams := nil;
  PEM_write_bio_DSAparams := nil;

  // 清理 DH 参数 PEM 函数
  PEM_read_bio_DHparams := nil;
  PEM_write_bio_DHparams := nil;
  PEM_write_bio_DHxparams := nil;

  // 清理 EC 密钥 PEM 函数
  PEM_read_bio_ECPrivateKey := nil;
  PEM_write_bio_ECPrivateKey := nil;
  PEM_read_bio_EC_PUBKEY := nil;
  PEM_write_bio_EC_PUBKEY := nil;
  PEM_read_bio_ECPKParameters := nil;
  PEM_write_bio_ECPKParameters := nil;

  // 清理 PKCS7 PEM 函数
  PEM_read_bio_PKCS7 := nil;
  PEM_write_bio_PKCS7 := nil;
  PEM_write_bio_PKCS7_stream := nil;

  // 清理 PKCS8 函数
  PEM_read_bio_PKCS8 := nil;
  PEM_write_bio_PKCS8 := nil;
  PEM_read_bio_PKCS8_PRIV_KEY_INFO := nil;
  PEM_write_bio_PKCS8_PRIV_KEY_INFO := nil;

  // 清理 Parameters 函数
  PEM_read_bio_Parameters := nil;
  PEM_write_bio_Parameters := nil;

  // 清理 CMS 函数
  PEM_read_bio_CMS := nil;
  PEM_write_bio_CMS := nil;
  PEM_write_bio_CMS_stream := nil;

  // 清理 SSL Session PEM 函数
  PEM_read_bio_SSL_SESSION := nil;
  PEM_write_bio_SSL_SESSION := nil;

  // 清理文件版本的 PEM 函数
  PEM_read_X509 := nil;
  PEM_write_X509 := nil;
  PEM_read_PrivateKey := nil;
  PEM_write_PrivateKey := nil;

  // 清理实用函数
  PEM_def_callback := nil;
  PEM_dek_info := nil;

  FPEMLoaded := False;
end;

// 辅助函数实现
function LoadPrivateKeyFromPEM(const AFileName: string; const APassword: string): PEVP_PKEY;
var
  Bio: PBIO;
  Pwd: PAnsiChar;
begin
  Result := nil;
  if not FPEMLoaded or not FileExists(AFileName) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'r');
  if Bio = nil then
    Exit;

  try
    if APassword <> '' then
      Pwd := PAnsiChar(AnsiString(APassword))
    else
      Pwd := nil;

    Result := PEM_read_bio_PrivateKey(Bio, nil, @PasswordCallback, Pwd);
  finally
    BIO_free(Bio);
  end;
end;

function LoadPublicKeyFromPEM(const AFileName: string): PEVP_PKEY;
var
  Bio: PBIO;
begin
  Result := nil;
  if not FPEMLoaded or not FileExists(AFileName) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'r');
  if Bio = nil then
    Exit;

  try
    Result := PEM_read_bio_PUBKEY(Bio, nil, nil, nil);
  finally
    BIO_free(Bio);
  end;
end;

function LoadCertificateFromPEM(const AFileName: string): PX509;
var
  Bio: PBIO;
begin
  Result := nil;
  if not FPEMLoaded or not FileExists(AFileName) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'r');
  if Bio = nil then
    Exit;

  try
    Result := PEM_read_bio_X509(Bio, nil, nil, nil);
  finally
    BIO_free(Bio);
  end;
end;

function SavePrivateKeyToPEM(const AFileName: string; AKey: PEVP_PKEY; const APassword: string): Boolean;
var
  Bio: PBIO;
  Enc: PEVP_CIPHER;
  Pwd: PAnsiChar;
begin
  Result := False;
  if not FPEMLoaded or (AKey = nil) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'w');
  if Bio = nil then
    Exit;

  try
    if APassword <> '' then
    begin
      Enc := EVP_aes_256_cbc();
      Pwd := PAnsiChar(AnsiString(APassword));
    end
    else
    begin
      Enc := nil;
      Pwd := nil;
    end;

    Result := PEM_write_bio_PrivateKey(Bio, AKey, Enc, PByte(Pwd), 
      Length(APassword), @PasswordCallback, Pwd) = 1;
  finally
    BIO_free(Bio);
  end;
end;

function SavePublicKeyToPEM(const AFileName: string; AKey: PEVP_PKEY): Boolean;
var
  Bio: PBIO;
begin
  Result := False;
  if not FPEMLoaded or (AKey = nil) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'w');
  if Bio = nil then
    Exit;

  try
    Result := PEM_write_bio_PUBKEY(Bio, AKey) = 1;
  finally
    BIO_free(Bio);
  end;
end;

function SaveCertificateToPEM(const AFileName: string; ACert: PX509): Boolean;
var
  Bio: PBIO;
begin
  Result := False;
  if not FPEMLoaded or (ACert = nil) then
    Exit;

  Bio := BIO_new_file(PAnsiChar(AnsiString(AFileName)), 'w');
  if Bio = nil then
    Exit;

  try
    Result := PEM_write_bio_X509(Bio, ACert) = 1;
  finally
    BIO_free(Bio);
  end;
end;

function LoadPrivateKeyFromMemory(const AData: TBytes; const APassword: string): PEVP_PKEY;
var
  Bio: PBIO;
  Pwd: PAnsiChar;
begin
  Result := nil;
  if not FPEMLoaded or (Length(AData) = 0) then
    Exit;

  Bio := BIO_new_mem_buf(@AData[0], Length(AData));
  if Bio = nil then
    Exit;

  try
    if APassword <> '' then
      Pwd := PAnsiChar(AnsiString(APassword))
    else
      Pwd := nil;

    Result := PEM_read_bio_PrivateKey(Bio, nil, @PasswordCallback, Pwd);
  finally
    BIO_free(Bio);
  end;
end;

function LoadCertificateFromMemory(const AData: TBytes): PX509;
var
  Bio: PBIO;
begin
  Result := nil;
  if not FPEMLoaded or (Length(AData) = 0) then
    Exit;

  Bio := BIO_new_mem_buf(@AData[0], Length(AData));
  if Bio = nil then
    Exit;

  try
    Result := PEM_read_bio_X509(Bio, nil, nil, nil);
  finally
    BIO_free(Bio);
  end;
end;

end.