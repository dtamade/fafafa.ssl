unit fafafa.ssl.openssl.ocsp;

{$mode Delphi}
{$H+}

interface

uses
  SysUtils, Classes, dynlibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.asn1;

type
  // OCSP 类型定义
  OCSP_REQUEST = Pointer;
  POCSP_REQUEST = ^OCSP_REQUEST;
  PPOCSP_REQUEST = ^POCSP_REQUEST;
  
  OCSP_RESPONSE = Pointer;
  POCSP_RESPONSE = ^OCSP_RESPONSE;
  PPOCSP_RESPONSE = ^POCSP_RESPONSE;
  
  OCSP_BASICRESP = Pointer;
  POCSP_BASICRESP = ^OCSP_BASICRESP;
  PPOCSP_BASICRESP = ^POCSP_BASICRESP;
  
  OCSP_CERTID = Pointer;
  POCSP_CERTID = ^OCSP_CERTID;
  PPOCSP_CERTID = ^POCSP_CERTID;
  
  OCSP_ONEREQ = Pointer;
  POCSP_ONEREQ = ^OCSP_ONEREQ;
  
  OCSP_SINGLERESP = Pointer;
  POCSP_SINGLERESP = ^OCSP_SINGLERESP;
  
  OCSP_RESPDATA = Pointer;
  POCSP_RESPDATA = ^OCSP_RESPDATA;
  
  OCSP_RESPBYTES = Pointer;
  POCSP_RESPBYTES = ^OCSP_RESPBYTES;
  
  OCSP_RESPID = Pointer;
  POCSP_RESPID = ^OCSP_RESPID;
  
  OCSP_REVOKEDINFO = Pointer;
  POCSP_REVOKEDINFO = ^OCSP_REVOKEDINFO;
  
  OCSP_CERTSTATUS = Pointer;
  POCSP_CERTSTATUS = ^OCSP_CERTSTATUS;
  
  OCSP_REQINFO = Pointer;
  POCSP_REQINFO = ^OCSP_REQINFO;
  
  OCSP_SIGNATURE = Pointer;
  POCSP_SIGNATURE = ^OCSP_SIGNATURE;
  
  OCSP_CRLID = Pointer;
  POCSP_CRLID = ^OCSP_CRLID;
  
  OCSP_SERVICELOC = Pointer;
  POCSP_SERVICELOC = ^OCSP_SERVICELOC;

  // OCSP 响应状态
  const
    OCSP_RESPONSE_STATUS_SUCCESSFUL            = 0;
    OCSP_RESPONSE_STATUS_MALFORMEDREQUEST      = 1;
    OCSP_RESPONSE_STATUS_INTERNALERROR         = 2;
    OCSP_RESPONSE_STATUS_TRYLATER              = 3;
    OCSP_RESPONSE_STATUS_SIGREQUIRED           = 5;
    OCSP_RESPONSE_STATUS_UNAUTHORIZED          = 6;

    // 证书状态
    V_OCSP_CERTSTATUS_GOOD                     = 0;
    V_OCSP_CERTSTATUS_REVOKED                  = 1;
    V_OCSP_CERTSTATUS_UNKNOWN                  = 2;

    // 吊销原因
    OCSP_REVOKED_STATUS_NOSTATUS               = -1;
    OCSP_REVOKED_STATUS_UNSPECIFIED            = 0;
    OCSP_REVOKED_STATUS_KEYCOMPROMISE          = 1;
    OCSP_REVOKED_STATUS_CACOMPROMISE           = 2;
    OCSP_REVOKED_STATUS_AFFILIATIONCHANGED     = 3;
    OCSP_REVOKED_STATUS_SUPERSEDED             = 4;
    OCSP_REVOKED_STATUS_CESSATIONOFOPERATION   = 5;
    OCSP_REVOKED_STATUS_CERTIFICATEHOLD        = 6;
    OCSP_REVOKED_STATUS_REMOVEFROMCRL          = 8;

    // OCSP 标志
    OCSP_NOCERTS                    = $1;
    OCSP_NOINTERN                   = $2;
    OCSP_NOSIGS                     = $4;
    OCSP_NOCHAIN                    = $8;
    OCSP_NOVERIFY                   = $10;
    OCSP_NOEXPLICIT                 = $20;
    OCSP_NOCASIGN                   = $40;
    OCSP_NODELEGATED                = $80;
    OCSP_NOCHECKS                   = $100;
    OCSP_TRUSTOTHER                 = $200;
    OCSP_RESPID_KEY                 = $400;
    OCSP_NOTIME                     = $800;

    // V_OCSP 标志
    V_OCSP_RESPID_NAME              = 0;
    V_OCSP_RESPID_KEY               = 1;

  // OCSP 函数类型
  type
    // OCSP 请求函数
    TOCSP_REQUEST_new = function(): POCSP_REQUEST; cdecl;
    TOCSP_REQUEST_free = procedure(a: POCSP_REQUEST); cdecl;
    Td2i_OCSP_REQUEST = function(a: PPOCSP_REQUEST; const in_: PPByte; len: Integer): POCSP_REQUEST; cdecl;
    Ti2d_OCSP_REQUEST = function(a: POCSP_REQUEST; out_: PPByte): Integer; cdecl;
    TOCSP_REQUEST_add_ext = function(req: POCSP_REQUEST; ex: PX509_EXTENSION; loc: Integer): Integer; cdecl;
    TOCSP_REQUEST_get_ext = function(x: POCSP_REQUEST; loc: Integer): PX509_EXTENSION; cdecl;
    TOCSP_REQUEST_get_ext_by_NID = function(x: POCSP_REQUEST; nid: Integer; lastpos: Integer): Integer; cdecl;
    TOCSP_REQUEST_get_ext_by_OBJ = function(x: POCSP_REQUEST; obj: ASN1_OBJECT; lastpos: Integer): Integer; cdecl;
    TOCSP_REQUEST_get_ext_by_critical = function(x: POCSP_REQUEST; crit: Integer; lastpos: Integer): Integer; cdecl;
    TOCSP_REQUEST_get_ext_count = function(x: POCSP_REQUEST): Integer; cdecl;
    TOCSP_REQUEST_delete_ext = function(x: POCSP_REQUEST; loc: Integer): PX509_EXTENSION; cdecl;
    TOCSP_REQUEST_print = function(bp: PBIO; a: POCSP_REQUEST; flags: Cardinal): Integer; cdecl;
    TOCSP_REQUEST_sign = function(req: POCSP_REQUEST; signer: PX509; key: PEVP_PKEY; 
      const dgst: PEVP_MD; certs: PSTACK_OF_X509; flags: Cardinal): Integer; cdecl;

    // OCSP 响应函数
    TOCSP_RESPONSE_new = function(): POCSP_RESPONSE; cdecl;
    TOCSP_RESPONSE_free = procedure(a: POCSP_RESPONSE); cdecl;
    Td2i_OCSP_RESPONSE = function(a: PPOCSP_RESPONSE; const in_: PPByte; len: Integer): POCSP_RESPONSE; cdecl;
    Ti2d_OCSP_RESPONSE = function(a: POCSP_RESPONSE; out_: PPByte): Integer; cdecl;
    TOCSP_RESPONSE_create = function(status: Integer; bs: POCSP_BASICRESP): POCSP_RESPONSE; cdecl;
    TOCSP_RESPONSE_status = function(resp: POCSP_RESPONSE): Integer; cdecl;
    TOCSP_RESPONSE_get1_basic = function(resp: POCSP_RESPONSE): POCSP_BASICRESP; cdecl;
    TOCSP_RESPONSE_print = function(bp: PBIO; o: POCSP_RESPONSE; flags: Cardinal): Integer; cdecl;

    // OCSP 基本响应函数
    TOCSP_BASICRESP_new = function(): POCSP_BASICRESP; cdecl;
    TOCSP_BASICRESP_free = procedure(a: POCSP_BASICRESP); cdecl;
    Td2i_OCSP_BASICRESP = function(a: PPOCSP_BASICRESP; const in_: PPByte; len: Integer): POCSP_BASICRESP; cdecl;
    Ti2d_OCSP_BASICRESP = function(a: POCSP_BASICRESP; out_: PPByte): Integer; cdecl;
    TOCSP_BASICRESP_add_ext = function(x: POCSP_BASICRESP; ex: PX509_EXTENSION; loc: Integer): Integer; cdecl;
    TOCSP_BASICRESP_get_ext = function(x: POCSP_BASICRESP; loc: Integer): PX509_EXTENSION; cdecl;
    TOCSP_BASICRESP_get_ext_by_NID = function(x: POCSP_BASICRESP; nid: Integer; lastpos: Integer): Integer; cdecl;
    TOCSP_BASICRESP_get_ext_by_OBJ = function(x: POCSP_BASICRESP; obj: ASN1_OBJECT; lastpos: Integer): Integer; cdecl;
    TOCSP_BASICRESP_get_ext_by_critical = function(x: POCSP_BASICRESP; crit: Integer; lastpos: Integer): Integer; cdecl;
    TOCSP_BASICRESP_get_ext_count = function(x: POCSP_BASICRESP): Integer; cdecl;
    TOCSP_BASICRESP_delete_ext = function(x: POCSP_BASICRESP; loc: Integer): PX509_EXTENSION; cdecl;
    TOCSP_BASICRESP_sign = function(brsp: POCSP_BASICRESP; signer: PX509; key: PEVP_PKEY; 
      const dgst: PEVP_MD; certs: PSTACK_OF_X509; flags: Cardinal): Integer; cdecl;
    TOCSP_BASICRESP_sign_ctx = function(brsp: POCSP_BASICRESP; signer: PX509; 
      ctx: PEVP_MD_CTX; certs: PSTACK_OF_X509; flags: Cardinal): Integer; cdecl;
    TOCSP_BASICRESP_verify = function(bs: POCSP_BASICRESP; certs: PSTACK_OF_X509; 
      st: PX509_STORE; flags: Cardinal): Integer; cdecl;

    // OCSP 证书 ID 函数
    TOCSP_CERTID_new = function(): POCSP_CERTID; cdecl;
    TOCSP_CERTID_free = procedure(a: POCSP_CERTID); cdecl;
    TOCSP_CERTID_dup = function(id: POCSP_CERTID): POCSP_CERTID; cdecl;
    TOCSP_cert_to_id = function(const dgst: PEVP_MD; const subject: PX509; const issuer: PX509): POCSP_CERTID; cdecl;
    TOCSP_cert_id_new = function(const dgst: PEVP_MD; const issuerName: PX509_NAME; 
      const issuerKey: ASN1_BIT_STRING; const serialNumber: ASN1_INTEGER): POCSP_CERTID; cdecl;
    TOCSP_id_issuer_cmp = function(a: POCSP_CERTID; b: POCSP_CERTID): Integer; cdecl;
    TOCSP_id_cmp = function(a: POCSP_CERTID; b: POCSP_CERTID): Integer; cdecl;
    TOCSP_id_get0_info = function(piNameHash: PPASN1_OCTET_STRING; pmd: PPEVP_MD; 
      piKeyHash: PPASN1_OCTET_STRING; pserial: PPASN1_INTEGER; cid: POCSP_CERTID): Integer; cdecl;

    // OCSP 请求操作
    TOCSP_request_add0_id = function(req: POCSP_REQUEST; cid: POCSP_CERTID): POCSP_ONEREQ; cdecl;
    TOCSP_request_add1_nonce = function(req: POCSP_REQUEST; val: PByte; len: Integer): Integer; cdecl;
    TOCSP_check_nonce = function(req: POCSP_REQUEST; bs: POCSP_BASICRESP): Integer; cdecl;
    TOCSP_copy_nonce = function(resp: POCSP_BASICRESP; req: POCSP_REQUEST): Integer; cdecl;
    TOCSP_request_add1_cert = function(req: POCSP_REQUEST; cert: PX509): Integer; cdecl;
    TOCSP_request_onereq_count = function(req: POCSP_REQUEST): Integer; cdecl;
    TOCSP_request_onereq_get0 = function(req: POCSP_REQUEST; i: Integer): POCSP_ONEREQ; cdecl;
    TOCSP_onereq_get0_id = function(one: POCSP_ONEREQ): POCSP_CERTID; cdecl;
    TOCSP_single_get0_status = function(single: POCSP_SINGLERESP; reason: PInteger; 
      revtime: PPASN1_GENERALIZEDTIME; thisupd: PPASN1_GENERALIZEDTIME; 
      nextupd: PPASN1_GENERALIZEDTIME): Integer; cdecl;

    // OCSP 响应操作
    TOCSP_resp_count = function(bs: POCSP_BASICRESP): Integer; cdecl;
    TOCSP_resp_get0 = function(bs: POCSP_BASICRESP; idx: Integer): POCSP_SINGLERESP; cdecl;
    TOCSP_resp_get0_respdata = function(bs: POCSP_BASICRESP): POCSP_RESPDATA; cdecl;
    TOCSP_resp_get0_produced_at = function(const bs: POCSP_BASICRESP): ASN1_GENERALIZEDTIME; cdecl;
    TOCSP_resp_get0_signature = function(const bs: POCSP_BASICRESP): POCSP_SIGNATURE; cdecl;
    TOCSP_resp_get1_id = function(bs: POCSP_BASICRESP; id: PPOCSP_RESPID): Integer; cdecl;
    TOCSP_resp_get0_id = function(const bs: POCSP_BASICRESP; id: PPOCSP_RESPID): Integer; cdecl;
    TOCSP_resp_get0_certs = function(const bs: POCSP_BASICRESP): PSTACK_OF_X509; cdecl;
    TOCSP_resp_find = function(bs: POCSP_BASICRESP; id: POCSP_CERTID; idx: PInteger): Integer; cdecl;
    TOCSP_resp_find_status = function(bs: POCSP_BASICRESP; id: POCSP_CERTID; status: PInteger; 
      reason: PInteger; revtime: PPASN1_GENERALIZEDTIME; thisupd: PPASN1_GENERALIZEDTIME; 
      nextupd: PPASN1_GENERALIZEDTIME): Integer; cdecl;

    // OCSP 基本响应添加
    TOCSP_basic_add1_status = function(rsp: POCSP_BASICRESP; cid: POCSP_CERTID; 
      status: Integer; reason: Integer; revtime: ASN1_TIME; thisupd: ASN1_TIME; 
      nextupd: ASN1_TIME): POCSP_SINGLERESP; cdecl;
    TOCSP_basic_add1_nonce = function(resp: POCSP_BASICRESP; val: PByte; len: Integer): Integer; cdecl;
    TOCSP_basic_add1_cert = function(resp: POCSP_BASICRESP; cert: PX509): Integer; cdecl;

    // OCSP 检查函数
    TOCSP_check_validity = function(thisupd: ASN1_GENERALIZEDTIME; nextupd: ASN1_GENERALIZEDTIME; 
      sec: Integer; maxsec: Integer): Integer; cdecl;

    // OCSP HTTP 函数
    TOCSP_sendreq_new = function(io: PBIO; const path: PAnsiChar; req: POCSP_REQUEST; 
      maxline: Integer): POCSP_REQ_CTX; cdecl;
    TOCSP_sendreq_nbio = function(rctx: PPOCSP_REQ_CTX; resp: PPOCSP_RESPONSE): Integer; cdecl;
    TOCSP_REQ_CTX_free = procedure(rctx: POCSP_REQ_CTX); cdecl;
    TOCSP_REQ_CTX_http = function(rctx: POCSP_REQ_CTX; const op: PAnsiChar; const path: PAnsiChar): Integer; cdecl;
    TOCSP_REQ_CTX_set1_req = function(rctx: POCSP_REQ_CTX; req: POCSP_REQUEST): Integer; cdecl;
    TOCSP_REQ_CTX_add1_header = function(rctx: POCSP_REQ_CTX; const name: PAnsiChar; 
      const value: PAnsiChar): Integer; cdecl;
    TOCSP_REQ_CTX_i2d = function(rctx: POCSP_REQ_CTX; const it: ASN1_ITEM; val: ASN1_VALUE): Integer; cdecl;
    TOCSP_REQ_CTX_nbio_d2i = function(rctx: POCSP_REQ_CTX; val: PPASN1_VALUE; const it: ASN1_ITEM): Integer; cdecl;
    TOCSP_REQ_CTX_get0_mem_bio = function(rctx: POCSP_REQ_CTX): PBIO; cdecl;
    TOCSP_REQ_CTX_nbio = function(rctx: POCSP_REQ_CTX): Integer; cdecl;

    // OCSP 服务定位器
    TOCSP_url_svcloc_new = function(issuer: PX509; const urls: PPSTACK_OF): PX509_EXTENSION; cdecl;
    TOCSP_parse_url = function(const url: PAnsiChar; phost: PPAnsiChar; pport: PPAnsiChar; 
      ppath: PPAnsiChar; pssl: PInteger): Integer; cdecl;

    // OCSP 响应者 ID
    TOCSP_RESPID_set_by_name = function(respid: POCSP_RESPID; cert: PX509): Integer; cdecl;
    TOCSP_RESPID_set_by_key = function(respid: POCSP_RESPID; cert: PX509): Integer; cdecl;
    TOCSP_RESPID_match = function(respid: POCSP_RESPID; cert: PX509): Integer; cdecl;

    // OCSP CRL ID
    TOCSP_crlID_new = function(const url: PAnsiChar; n: PInteger; tim: ASN1_TIME): POCSP_CRLID; cdecl;

    // OCSP 存档截止
    TOCSP_archive_cutoff_new = function(tim: PAnsiChar): PX509_EXTENSION; cdecl;

    // OCSP 接受语言
    TOCSP_accept_responses_new = function(const oids: PPSTACK_OF): PX509_EXTENSION; cdecl;

  // OCSP REQ_CTX
  POCSP_REQ_CTX = Pointer;
  PPOCSP_REQ_CTX = ^POCSP_REQ_CTX;

  // ASN1_VALUE
  ASN1_VALUE = Pointer;
  PPASN1_VALUE = ^ASN1_VALUE;
  
  // PSTACK
  PPSTACK_OF = Pointer;

var
  // OCSP 请求函数
  OCSP_REQUEST_new: TOCSP_REQUEST_new = nil;
  OCSP_REQUEST_free: TOCSP_REQUEST_free = nil;
  d2i_OCSP_REQUEST: Td2i_OCSP_REQUEST = nil;
  i2d_OCSP_REQUEST: Ti2d_OCSP_REQUEST = nil;
  OCSP_REQUEST_add_ext: TOCSP_REQUEST_add_ext = nil;
  OCSP_REQUEST_get_ext: TOCSP_REQUEST_get_ext = nil;
  OCSP_REQUEST_get_ext_by_NID: TOCSP_REQUEST_get_ext_by_NID = nil;
  OCSP_REQUEST_get_ext_by_OBJ: TOCSP_REQUEST_get_ext_by_OBJ = nil;
  OCSP_REQUEST_get_ext_by_critical: TOCSP_REQUEST_get_ext_by_critical = nil;
  OCSP_REQUEST_get_ext_count: TOCSP_REQUEST_get_ext_count = nil;
  OCSP_REQUEST_delete_ext: TOCSP_REQUEST_delete_ext = nil;
  OCSP_REQUEST_print: TOCSP_REQUEST_print = nil;
  OCSP_REQUEST_sign: TOCSP_REQUEST_sign = nil;

  // OCSP 响应函数
  OCSP_RESPONSE_new: TOCSP_RESPONSE_new = nil;
  OCSP_RESPONSE_free: TOCSP_RESPONSE_free = nil;
  d2i_OCSP_RESPONSE: Td2i_OCSP_RESPONSE = nil;
  i2d_OCSP_RESPONSE: Ti2d_OCSP_RESPONSE = nil;
  OCSP_RESPONSE_create: TOCSP_RESPONSE_create = nil;
  OCSP_RESPONSE_status: TOCSP_RESPONSE_status = nil;
  OCSP_RESPONSE_get1_basic: TOCSP_RESPONSE_get1_basic = nil;
  OCSP_RESPONSE_print: TOCSP_RESPONSE_print = nil;

  // OCSP 基本响应函数
  OCSP_BASICRESP_new: TOCSP_BASICRESP_new = nil;
  OCSP_BASICRESP_free: TOCSP_BASICRESP_free = nil;
  d2i_OCSP_BASICRESP: Td2i_OCSP_BASICRESP = nil;
  i2d_OCSP_BASICRESP: Ti2d_OCSP_BASICRESP = nil;
  OCSP_BASICRESP_add_ext: TOCSP_BASICRESP_add_ext = nil;
  OCSP_BASICRESP_get_ext: TOCSP_BASICRESP_get_ext = nil;
  OCSP_BASICRESP_get_ext_by_NID: TOCSP_BASICRESP_get_ext_by_NID = nil;
  OCSP_BASICRESP_get_ext_by_OBJ: TOCSP_BASICRESP_get_ext_by_OBJ = nil;
  OCSP_BASICRESP_get_ext_by_critical: TOCSP_BASICRESP_get_ext_by_critical = nil;
  OCSP_BASICRESP_get_ext_count: TOCSP_BASICRESP_get_ext_count = nil;
  OCSP_BASICRESP_delete_ext: TOCSP_BASICRESP_delete_ext = nil;
  OCSP_BASICRESP_sign: TOCSP_BASICRESP_sign = nil;
  OCSP_BASICRESP_sign_ctx: TOCSP_BASICRESP_sign_ctx = nil;
  OCSP_BASICRESP_verify: TOCSP_BASICRESP_verify = nil;

  // OCSP 证书 ID 函数
  OCSP_CERTID_new: TOCSP_CERTID_new = nil;
  OCSP_CERTID_free: TOCSP_CERTID_free = nil;
  OCSP_CERTID_dup: TOCSP_CERTID_dup = nil;
  OCSP_cert_to_id: TOCSP_cert_to_id = nil;
  OCSP_cert_id_new: TOCSP_cert_id_new = nil;
  OCSP_id_issuer_cmp: TOCSP_id_issuer_cmp = nil;
  OCSP_id_cmp: TOCSP_id_cmp = nil;
  OCSP_id_get0_info: TOCSP_id_get0_info = nil;

  // OCSP 请求操作
  OCSP_request_add0_id: TOCSP_request_add0_id = nil;
  OCSP_request_add1_nonce: TOCSP_request_add1_nonce = nil;
  OCSP_check_nonce: TOCSP_check_nonce = nil;
  OCSP_copy_nonce: TOCSP_copy_nonce = nil;
  OCSP_request_add1_cert: TOCSP_request_add1_cert = nil;
  OCSP_request_onereq_count: TOCSP_request_onereq_count = nil;
  OCSP_request_onereq_get0: TOCSP_request_onereq_get0 = nil;
  OCSP_onereq_get0_id: TOCSP_onereq_get0_id = nil;
  OCSP_single_get0_status: TOCSP_single_get0_status = nil;

  // OCSP 响应操作
  OCSP_resp_count: TOCSP_resp_count = nil;
  OCSP_resp_get0: TOCSP_resp_get0 = nil;
  OCSP_resp_get0_respdata: TOCSP_resp_get0_respdata = nil;
  OCSP_resp_get0_produced_at: TOCSP_resp_get0_produced_at = nil;
  OCSP_resp_get0_signature: TOCSP_resp_get0_signature = nil;
  OCSP_resp_get1_id: TOCSP_resp_get1_id = nil;
  OCSP_resp_get0_id: TOCSP_resp_get0_id = nil;
  OCSP_resp_get0_certs: TOCSP_resp_get0_certs = nil;
  OCSP_resp_find: TOCSP_resp_find = nil;
  OCSP_resp_find_status: TOCSP_resp_find_status = nil;

  // OCSP 基本响应添加
  OCSP_basic_add1_status: TOCSP_basic_add1_status = nil;
  OCSP_basic_add1_nonce: TOCSP_basic_add1_nonce = nil;
  OCSP_basic_add1_cert: TOCSP_basic_add1_cert = nil;

  // OCSP 检查函数
  OCSP_check_validity: TOCSP_check_validity = nil;

  // OCSP HTTP 函数
  OCSP_sendreq_new: TOCSP_sendreq_new = nil;
  OCSP_sendreq_nbio: TOCSP_sendreq_nbio = nil;
  OCSP_REQ_CTX_free: TOCSP_REQ_CTX_free = nil;
  OCSP_REQ_CTX_http: TOCSP_REQ_CTX_http = nil;
  OCSP_REQ_CTX_set1_req: TOCSP_REQ_CTX_set1_req = nil;
  OCSP_REQ_CTX_add1_header: TOCSP_REQ_CTX_add1_header = nil;
  OCSP_REQ_CTX_i2d: TOCSP_REQ_CTX_i2d = nil;
  OCSP_REQ_CTX_nbio_d2i: TOCSP_REQ_CTX_nbio_d2i = nil;
  OCSP_REQ_CTX_get0_mem_bio: TOCSP_REQ_CTX_get0_mem_bio = nil;
  OCSP_REQ_CTX_nbio: TOCSP_REQ_CTX_nbio = nil;

  // OCSP 服务定位器
  OCSP_url_svcloc_new: TOCSP_url_svcloc_new = nil;
  OCSP_parse_url: TOCSP_parse_url = nil;

  // OCSP 响应者 ID
  OCSP_RESPID_set_by_name: TOCSP_RESPID_set_by_name = nil;
  OCSP_RESPID_set_by_key: TOCSP_RESPID_set_by_key = nil;
  OCSP_RESPID_match: TOCSP_RESPID_match = nil;

  // OCSP CRL ID
  OCSP_crlID_new: TOCSP_crlID_new = nil;

  // OCSP 存档截止
  OCSP_archive_cutoff_new: TOCSP_archive_cutoff_new = nil;

  // OCSP 接受语言
  OCSP_accept_responses_new: TOCSP_accept_responses_new = nil;

// 加载和卸载函数
function LoadOpenSSLOCSP(const ACryptoLib: THandle): Boolean;
procedure UnloadOpenSSLOCSP;

// 辅助函数
function CheckCertificateStatus(ACert: PX509; AIssuer: PX509; 
  const AOCSPUrl: string; ATimeout: Integer = 10): Integer;
function CreateOCSPRequest(ACert: PX509; AIssuer: PX509): POCSP_REQUEST;
function SendOCSPRequest(ARequest: POCSP_REQUEST; const AOCSPUrl: string; 
  ATimeout: Integer = 10): POCSP_RESPONSE;
function VerifyOCSPResponse(AResponse: POCSP_RESPONSE; ACert: PX509; 
  AIssuer: PX509; AStore: PX509_STORE): Boolean;

implementation

var
  FOCSPLoaded: Boolean = False;

function LoadOpenSSLOCSP(const ACryptoLib: THandle): Boolean;
begin
  if FOCSPLoaded then
    Exit(True);

  if ACryptoLib = 0 then
    Exit(False);

  // 加载 OCSP 请求函数
  OCSP_REQUEST_new := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_new');
  OCSP_REQUEST_free := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_free');
  d2i_OCSP_REQUEST := GetProcAddress(ACryptoLib, 'd2i_OCSP_REQUEST');
  i2d_OCSP_REQUEST := GetProcAddress(ACryptoLib, 'i2d_OCSP_REQUEST');
  OCSP_REQUEST_add_ext := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_add_ext');
  OCSP_REQUEST_get_ext := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_get_ext');
  OCSP_REQUEST_get_ext_by_NID := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_get_ext_by_NID');
  OCSP_REQUEST_get_ext_by_OBJ := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_get_ext_by_OBJ');
  OCSP_REQUEST_get_ext_by_critical := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_get_ext_by_critical');
  OCSP_REQUEST_get_ext_count := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_get_ext_count');
  OCSP_REQUEST_delete_ext := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_delete_ext');
  OCSP_REQUEST_print := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_print');
  OCSP_REQUEST_sign := GetProcAddress(ACryptoLib, 'OCSP_REQUEST_sign');

  // 加载 OCSP 响应函数
  OCSP_RESPONSE_new := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_new');
  OCSP_RESPONSE_free := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_free');
  d2i_OCSP_RESPONSE := GetProcAddress(ACryptoLib, 'd2i_OCSP_RESPONSE');
  i2d_OCSP_RESPONSE := GetProcAddress(ACryptoLib, 'i2d_OCSP_RESPONSE');
  OCSP_RESPONSE_create := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_create');
  OCSP_RESPONSE_status := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_status');
  OCSP_RESPONSE_get1_basic := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_get1_basic');
  OCSP_RESPONSE_print := GetProcAddress(ACryptoLib, 'OCSP_RESPONSE_print');

  // 加载 OCSP 基本响应函数
  OCSP_BASICRESP_new := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_new');
  OCSP_BASICRESP_free := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_free');
  d2i_OCSP_BASICRESP := GetProcAddress(ACryptoLib, 'd2i_OCSP_BASICRESP');
  i2d_OCSP_BASICRESP := GetProcAddress(ACryptoLib, 'i2d_OCSP_BASICRESP');
  OCSP_BASICRESP_add_ext := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_add_ext');
  OCSP_BASICRESP_get_ext := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_get_ext');
  OCSP_BASICRESP_get_ext_by_NID := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_get_ext_by_NID');
  OCSP_BASICRESP_get_ext_by_OBJ := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_get_ext_by_OBJ');
  OCSP_BASICRESP_get_ext_by_critical := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_get_ext_by_critical');
  OCSP_BASICRESP_get_ext_count := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_get_ext_count');
  OCSP_BASICRESP_delete_ext := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_delete_ext');
  OCSP_BASICRESP_sign := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_sign');
  OCSP_BASICRESP_sign_ctx := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_sign_ctx');
  OCSP_BASICRESP_verify := GetProcAddress(ACryptoLib, 'OCSP_BASICRESP_verify');

  // 加载 OCSP 证书 ID 函数
  OCSP_CERTID_new := GetProcAddress(ACryptoLib, 'OCSP_CERTID_new');
  OCSP_CERTID_free := GetProcAddress(ACryptoLib, 'OCSP_CERTID_free');
  OCSP_CERTID_dup := GetProcAddress(ACryptoLib, 'OCSP_CERTID_dup');
  OCSP_cert_to_id := GetProcAddress(ACryptoLib, 'OCSP_cert_to_id');
  OCSP_cert_id_new := GetProcAddress(ACryptoLib, 'OCSP_cert_id_new');
  OCSP_id_issuer_cmp := GetProcAddress(ACryptoLib, 'OCSP_id_issuer_cmp');
  OCSP_id_cmp := GetProcAddress(ACryptoLib, 'OCSP_id_cmp');
  OCSP_id_get0_info := GetProcAddress(ACryptoLib, 'OCSP_id_get0_info');

  // 加载 OCSP 请求操作
  OCSP_request_add0_id := GetProcAddress(ACryptoLib, 'OCSP_request_add0_id');
  OCSP_request_add1_nonce := GetProcAddress(ACryptoLib, 'OCSP_request_add1_nonce');
  OCSP_check_nonce := GetProcAddress(ACryptoLib, 'OCSP_check_nonce');
  OCSP_copy_nonce := GetProcAddress(ACryptoLib, 'OCSP_copy_nonce');
  OCSP_request_add1_cert := GetProcAddress(ACryptoLib, 'OCSP_request_add1_cert');
  OCSP_request_onereq_count := GetProcAddress(ACryptoLib, 'OCSP_request_onereq_count');
  OCSP_request_onereq_get0 := GetProcAddress(ACryptoLib, 'OCSP_request_onereq_get0');
  OCSP_onereq_get0_id := GetProcAddress(ACryptoLib, 'OCSP_onereq_get0_id');
  OCSP_single_get0_status := GetProcAddress(ACryptoLib, 'OCSP_single_get0_status');

  // 加载 OCSP 响应操作
  OCSP_resp_count := GetProcAddress(ACryptoLib, 'OCSP_resp_count');
  OCSP_resp_get0 := GetProcAddress(ACryptoLib, 'OCSP_resp_get0');
  OCSP_resp_get0_respdata := GetProcAddress(ACryptoLib, 'OCSP_resp_get0_respdata');
  OCSP_resp_get0_produced_at := GetProcAddress(ACryptoLib, 'OCSP_resp_get0_produced_at');
  OCSP_resp_get0_signature := GetProcAddress(ACryptoLib, 'OCSP_resp_get0_signature');
  OCSP_resp_get1_id := GetProcAddress(ACryptoLib, 'OCSP_resp_get1_id');
  OCSP_resp_get0_id := GetProcAddress(ACryptoLib, 'OCSP_resp_get0_id');
  OCSP_resp_get0_certs := GetProcAddress(ACryptoLib, 'OCSP_resp_get0_certs');
  OCSP_resp_find := GetProcAddress(ACryptoLib, 'OCSP_resp_find');
  OCSP_resp_find_status := GetProcAddress(ACryptoLib, 'OCSP_resp_find_status');

  // 加载 OCSP 基本响应添加
  OCSP_basic_add1_status := GetProcAddress(ACryptoLib, 'OCSP_basic_add1_status');
  OCSP_basic_add1_nonce := GetProcAddress(ACryptoLib, 'OCSP_basic_add1_nonce');
  OCSP_basic_add1_cert := GetProcAddress(ACryptoLib, 'OCSP_basic_add1_cert');

  // 加载 OCSP 检查函数
  OCSP_check_validity := GetProcAddress(ACryptoLib, 'OCSP_check_validity');

  // 加载 OCSP HTTP 函数
  OCSP_sendreq_new := GetProcAddress(ACryptoLib, 'OCSP_sendreq_new');
  OCSP_sendreq_nbio := GetProcAddress(ACryptoLib, 'OCSP_sendreq_nbio');
  OCSP_REQ_CTX_free := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_free');
  OCSP_REQ_CTX_http := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_http');
  OCSP_REQ_CTX_set1_req := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_set1_req');
  OCSP_REQ_CTX_add1_header := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_add1_header');
  OCSP_REQ_CTX_i2d := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_i2d');
  OCSP_REQ_CTX_nbio_d2i := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_nbio_d2i');
  OCSP_REQ_CTX_get0_mem_bio := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_get0_mem_bio');
  OCSP_REQ_CTX_nbio := GetProcAddress(ACryptoLib, 'OCSP_REQ_CTX_nbio');

  // 加载 OCSP 服务定位器
  OCSP_url_svcloc_new := GetProcAddress(ACryptoLib, 'OCSP_url_svcloc_new');
  OCSP_parse_url := GetProcAddress(ACryptoLib, 'OCSP_parse_url');

  // 加载 OCSP 响应者 ID
  OCSP_RESPID_set_by_name := GetProcAddress(ACryptoLib, 'OCSP_RESPID_set_by_name');
  OCSP_RESPID_set_by_key := GetProcAddress(ACryptoLib, 'OCSP_RESPID_set_by_key');
  OCSP_RESPID_match := GetProcAddress(ACryptoLib, 'OCSP_RESPID_match');

  // 加载 OCSP CRL ID
  OCSP_crlID_new := GetProcAddress(ACryptoLib, 'OCSP_crlID_new');

  // 加载 OCSP 存档截止
  OCSP_archive_cutoff_new := GetProcAddress(ACryptoLib, 'OCSP_archive_cutoff_new');

  // 加载 OCSP 接受语言
  OCSP_accept_responses_new := GetProcAddress(ACryptoLib, 'OCSP_accept_responses_new');

  FOCSPLoaded := Assigned(OCSP_REQUEST_new) and Assigned(OCSP_RESPONSE_new);
  Result := FOCSPLoaded;
end;

procedure UnloadOpenSSLOCSP;
begin
  if not FOCSPLoaded then
    Exit;

  // 清理 OCSP 请求函数
  OCSP_REQUEST_new := nil;
  OCSP_REQUEST_free := nil;
  d2i_OCSP_REQUEST := nil;
  i2d_OCSP_REQUEST := nil;
  OCSP_REQUEST_add_ext := nil;
  OCSP_REQUEST_get_ext := nil;
  OCSP_REQUEST_get_ext_by_NID := nil;
  OCSP_REQUEST_get_ext_by_OBJ := nil;
  OCSP_REQUEST_get_ext_by_critical := nil;
  OCSP_REQUEST_get_ext_count := nil;
  OCSP_REQUEST_delete_ext := nil;
  OCSP_REQUEST_print := nil;
  OCSP_REQUEST_sign := nil;

  // 清理 OCSP 响应函数
  OCSP_RESPONSE_new := nil;
  OCSP_RESPONSE_free := nil;
  d2i_OCSP_RESPONSE := nil;
  i2d_OCSP_RESPONSE := nil;
  OCSP_RESPONSE_create := nil;
  OCSP_RESPONSE_status := nil;
  OCSP_RESPONSE_get1_basic := nil;
  OCSP_RESPONSE_print := nil;

  // 清理 OCSP 基本响应函数
  OCSP_BASICRESP_new := nil;
  OCSP_BASICRESP_free := nil;
  d2i_OCSP_BASICRESP := nil;
  i2d_OCSP_BASICRESP := nil;
  OCSP_BASICRESP_add_ext := nil;
  OCSP_BASICRESP_get_ext := nil;
  OCSP_BASICRESP_get_ext_by_NID := nil;
  OCSP_BASICRESP_get_ext_by_OBJ := nil;
  OCSP_BASICRESP_get_ext_by_critical := nil;
  OCSP_BASICRESP_get_ext_count := nil;
  OCSP_BASICRESP_delete_ext := nil;
  OCSP_BASICRESP_sign := nil;
  OCSP_BASICRESP_sign_ctx := nil;
  OCSP_BASICRESP_verify := nil;

  // 清理 OCSP 证书 ID 函数
  OCSP_CERTID_new := nil;
  OCSP_CERTID_free := nil;
  OCSP_CERTID_dup := nil;
  OCSP_cert_to_id := nil;
  OCSP_cert_id_new := nil;
  OCSP_id_issuer_cmp := nil;
  OCSP_id_cmp := nil;
  OCSP_id_get0_info := nil;

  // 清理 OCSP 请求操作
  OCSP_request_add0_id := nil;
  OCSP_request_add1_nonce := nil;
  OCSP_check_nonce := nil;
  OCSP_copy_nonce := nil;
  OCSP_request_add1_cert := nil;
  OCSP_request_onereq_count := nil;
  OCSP_request_onereq_get0 := nil;
  OCSP_onereq_get0_id := nil;
  OCSP_single_get0_status := nil;

  // 清理 OCSP 响应操作
  OCSP_resp_count := nil;
  OCSP_resp_get0 := nil;
  OCSP_resp_get0_respdata := nil;
  OCSP_resp_get0_produced_at := nil;
  OCSP_resp_get0_signature := nil;
  OCSP_resp_get1_id := nil;
  OCSP_resp_get0_id := nil;
  OCSP_resp_get0_certs := nil;
  OCSP_resp_find := nil;
  OCSP_resp_find_status := nil;

  // 清理 OCSP 基本响应添加
  OCSP_basic_add1_status := nil;
  OCSP_basic_add1_nonce := nil;
  OCSP_basic_add1_cert := nil;

  // 清理 OCSP 检查函数
  OCSP_check_validity := nil;

  // 清理 OCSP HTTP 函数
  OCSP_sendreq_new := nil;
  OCSP_sendreq_nbio := nil;
  OCSP_REQ_CTX_free := nil;
  OCSP_REQ_CTX_http := nil;
  OCSP_REQ_CTX_set1_req := nil;
  OCSP_REQ_CTX_add1_header := nil;
  OCSP_REQ_CTX_i2d := nil;
  OCSP_REQ_CTX_nbio_d2i := nil;
  OCSP_REQ_CTX_get0_mem_bio := nil;
  OCSP_REQ_CTX_nbio := nil;

  // 清理 OCSP 服务定位器
  OCSP_url_svcloc_new := nil;
  OCSP_parse_url := nil;

  // 清理 OCSP 响应者 ID
  OCSP_RESPID_set_by_name := nil;
  OCSP_RESPID_set_by_key := nil;
  OCSP_RESPID_match := nil;

  // 清理 OCSP CRL ID
  OCSP_crlID_new := nil;

  // 清理 OCSP 存档截止
  OCSP_archive_cutoff_new := nil;

  // 清理 OCSP 接受语言
  OCSP_accept_responses_new := nil;

  FOCSPLoaded := False;
end;

// 辅助函数实现
function CheckCertificateStatus(ACert: PX509; AIssuer: PX509; 
  const AOCSPUrl: string; ATimeout: Integer): Integer;
var
  Req: POCSP_REQUEST;
  Resp: POCSP_RESPONSE;
  BasicResp: POCSP_BASICRESP;
  CertID: POCSP_CERTID;
  Status, Reason: Integer;
  RevTime, ThisUpd, NextUpd: PASN1_GENERALIZEDTIME;
begin
  Result := V_OCSP_CERTSTATUS_UNKNOWN;
  if not FOCSPLoaded or (ACert = nil) or (AIssuer = nil) then
    Exit;

  // 创建 OCSP 请求
  Req := CreateOCSPRequest(ACert, AIssuer);
  if Req = nil then
    Exit;

  try
    // 发送 OCSP 请求
    Resp := SendOCSPRequest(Req, AOCSPUrl, ATimeout);
    if Resp = nil then
      Exit;

    try
      // 检查响应状态
      if OCSP_RESPONSE_status(Resp) <> OCSP_RESPONSE_STATUS_SUCCESSFUL then
        Exit;

      // 获取基本响应
      BasicResp := OCSP_RESPONSE_get1_basic(Resp);
      if BasicResp = nil then
        Exit;

      try
        // 创建证书 ID
        CertID := OCSP_cert_to_id(EVP_sha1(), ACert, AIssuer);
        if CertID = nil then
          Exit;

        try
          // 查找证书状态
          if OCSP_resp_find_status(BasicResp, CertID, @Status, @Reason, 
            @RevTime, @ThisUpd, @NextUpd) = 1 then
          begin
            // 检查有效期
            if OCSP_check_validity(ThisUpd, NextUpd, 300, -1) = 1 then
              Result := Status;
          end;
        finally
          OCSP_CERTID_free(CertID);
        end;
      finally
        OCSP_BASICRESP_free(BasicResp);
      end;
    finally
      OCSP_RESPONSE_free(Resp);
    end;
  finally
    OCSP_REQUEST_free(Req);
  end;
end;

function CreateOCSPRequest(ACert: PX509; AIssuer: PX509): POCSP_REQUEST;
var
  CertID: POCSP_CERTID;
begin
  Result := nil;
  if not FOCSPLoaded or (ACert = nil) or (AIssuer = nil) then
    Exit;

  Result := OCSP_REQUEST_new();
  if Result = nil then
    Exit;

  // 创建证书 ID
  CertID := OCSP_cert_to_id(EVP_sha1(), ACert, AIssuer);
  if CertID = nil then
  begin
    OCSP_REQUEST_free(Result);
    Result := nil;
    Exit;
  end;

  // 添加证书 ID 到请求
  if OCSP_request_add0_id(Result, CertID) = nil then
  begin
    OCSP_CERTID_free(CertID);
    OCSP_REQUEST_free(Result);
    Result := nil;
    Exit;
  end;

  // 添加 nonce
  OCSP_request_add1_nonce(Result, nil, -1);
end;

function SendOCSPRequest(ARequest: POCSP_REQUEST; const AOCSPUrl: string; 
  ATimeout: Integer): POCSP_RESPONSE;
var
  Host, Port, Path: PAnsiChar;
  UseSSL: Integer;
  Bio: PBIO;
  ReqCtx: POCSP_REQ_CTX;
  Resp: POCSP_RESPONSE;
begin
  Result := nil;
  if not FOCSPLoaded or (ARequest = nil) or (AOCSPUrl = '') then
    Exit;

  // 解析 URL
  if OCSP_parse_url(PAnsiChar(AnsiString(AOCSPUrl)), @Host, @Port, @Path, @UseSSL) = 0 then
    Exit;

  // 创建 BIO 连接
  Bio := BIO_new_connect(Host);
  if Bio = nil then
    Exit;

  try
    BIO_set_conn_port(Bio, Port);
    if BIO_do_connect(Bio) <= 0 then
      Exit;

    // 创建 OCSP 请求上下文
    ReqCtx := OCSP_sendreq_new(Bio, Path, ARequest, -1);
    if ReqCtx = nil then
      Exit;

    try
      // 发送请求并接收响应
      Resp := nil;
      while OCSP_sendreq_nbio(@ReqCtx, @Resp) = -1 do
      begin
        // 等待响应
        Sleep(100);
      end;
      Result := Resp;
    finally
      if ReqCtx <> nil then
        OCSP_REQ_CTX_free(ReqCtx);
    end;
  finally
    BIO_free_all(Bio);
  end;
end;

function VerifyOCSPResponse(AResponse: POCSP_RESPONSE; ACert: PX509; 
  AIssuer: PX509; AStore: PX509_STORE): Boolean;
var
  BasicResp: POCSP_BASICRESP;
  Certs: PSTACK_OF_X509;
begin
  Result := False;
  if not FOCSPLoaded or (AResponse = nil) then
    Exit;

  // 检查响应状态
  if OCSP_RESPONSE_status(AResponse) <> OCSP_RESPONSE_STATUS_SUCCESSFUL then
    Exit;

  // 获取基本响应
  BasicResp := OCSP_RESPONSE_get1_basic(AResponse);
  if BasicResp = nil then
    Exit;

  try
    // 创建证书链
    Certs := sk_X509_new_null();
    if Certs = nil then
      Exit;

    try
      sk_X509_push(Certs, AIssuer);
      
      // 验证响应
      Result := OCSP_BASICRESP_verify(BasicResp, Certs, AStore, 0) = 1;
    finally
      sk_X509_free(Certs);
    end;
  finally
    OCSP_BASICRESP_free(BasicResp);
  end;
end;

end.