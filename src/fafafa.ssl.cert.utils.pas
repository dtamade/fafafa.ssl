unit fafafa.ssl.cert.utils;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{**
 * Unit: fafafa.ssl.cert.utils
 * Purpose: 企业级证书工具类
 * 
 * Features:
 * - 自签名证书生成（RSA/ECDSA）
 * - 证书信息提取和验证
 * - 证书链验证
 * - PEM/DER格式转换
 * - 证书指纹计算
 * 
 * Thread Safety: 所有类方法线程安全
 * 
 * Dependencies:
 *   - OpenSSL 1.1.1+ or 3.0+
 *   - fafafa.ssl.exceptions
 *   - fafafa.ssl.crypto.utils (for fingerprint)
 * 
 * @author fafafa.ssl team
 * @version 2.0.0
 * @since 2025-11-26
 *}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.obj,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors;           // Phase 2.1 - Standardized error handling

const
  // 证书默认值
  DEFAULT_CERT_VALID_DAYS = 365;
  DEFAULT_RSA_KEY_BITS = 2048;
  DEFAULT_EC_CURVE = 'prime256v1';
  
  // 证书版本
  X509_VERSION_1 = 0;
  X509_VERSION_2 = 1;
  X509_VERSION_3 = 2;  // X509v3 = 2
  
  // RSA指数
  RSA_EXPONENT_F4 = 65537;
  
  // 序列号
  DEFAULT_SERIAL_NUMBER = 1;

type
  {**
   * 证书格式枚举
   * - cfPEM: PEM格式（Base64编码，文本）
   * - cfDER: DER格式（二进制）
   *}
  TCertFormat = (cfPEM, cfDER);
  
  {**
   * 密钥类型枚举
   * - ktRSA: RSA非对称加密
   * - ktECDSA: 椭圆曲线数字签名算法
   * - ktEd25519: Edwards曲线数字签名（未实现）
   *}
  TKeyType = (ktRSA, ktECDSA, ktEd25519);
  
  { 证书信息 }
  TCertInfo = record
    Subject: string;           // 主题
    Issuer: string;            // 颁发者
    SerialNumber: string;      // 序列号
    NotBefore: TDateTime;      // 起始日期
    NotAfter: TDateTime;       // 结束日期
    PublicKeyType: string;     // 公钥类型
    PublicKeyBits: Integer;    // 公钥位数
    SignatureAlgorithm: string;// 签名算法
    SubjectAltNames: TStringList; // SAN (备用名称)
    KeyUsage: string;          // 密钥用途
    IsCA: Boolean;             // 是否CA证书
    Version: Integer;          // 证书版本
  end;
  
  { 证书生成选项 }
  TCertGenOptions = record
    CommonName: string;        // 通用名称 (CN)
    Organization: string;      // 组织 (O)
    OrganizationalUnit: string;// 组织单位 (OU)
    Country: string;           // 国家 (C)
    State: string;             // 省份 (ST)
    Locality: string;          // 城市 (L)
    ValidDays: Integer;        // 有效天数 (如果NotBefore/NotAfter为0则使用)
    NotBefore: TDateTime;      // 自定义起始时间 (0=使用当前时间)
    NotAfter: TDateTime;       // 自定义结束时间 (0=使用ValidDays计算)
    KeyType: TKeyType;         // 密钥类型
    KeyBits: Integer;          // RSA密钥位数 (2048/4096)
    ECCurve: string;           // EC曲线名称 (prime256v1/secp384r1)
    SubjectAltNames: TStringList; // 备用名称
    IsCA: Boolean;             // 是否CA证书
    SerialNumber: Int64;       // 序列号 (0=自动生成)
    OCSPResponderURL: string;  // 可选：写入 AIA 的 OCSP Responder URL（http/https）
  end;

  {**
   * 企业级证书工具类
   * 
   * 提供完整的证书操作功能，所有方法都是类方法（静态）。
   * 
   * Thread Safety: 所有方法线程安全
   * 
   * @example
   * <code>
   *   var LCert, LKey: string;
   *   var LOptions: TCertGenOptions;
   *   LOptions := TCertificateUtils.DefaultGenOptions;
   *   LOptions.CommonName := 'example.com';
   *   TCertificateUtils.GenerateSelfSigned(LOptions, LCert, LKey);
   * </code>
   *}
  TCertificateUtils = class
  private
    class procedure EnsureInitialized; static;
    
    class function X509NameToString(AName: PX509_NAME): string; static;
    class function ASN1TimeToDateTime(ATime: Pointer): TDateTime; static;
    class function GenerateRSAKey(ABits: Integer): PEVP_PKEY; static;
    class function GenerateECKey(const ACurve: string): PEVP_PKEY; static;
    class function AddNameEntry(AName: PX509_NAME; const AKey, AValue: string): Boolean; static;
    class function AddExtension(ACert: PX509; AIssuer: PX509; ANID: Integer; const AValue: string): Boolean; static;
  public
    { 创建默认生成选项 }
    class function DefaultGenOptions: TCertGenOptions;
    
    { 生成自签名证书
      返回: PEM格式的证书和私钥 (Cert + Key)
      异常: 生成失败时抛出异常 }
    class function GenerateSelfSigned(
      const AOptions: TCertGenOptions;
      out ACertPEM, AKeyPEM: string
    ): Boolean;
    
    { 简化版：生成自签名证书
      参数: CN, 组织, 有效天数
      返回: PEM格式的证书和私钥 }
    class function GenerateSelfSignedSimple(
      const ACommonName, AOrganization: string;
      AValidDays: Integer;
      out ACertPEM, AKeyPEM: string
    ): Boolean;
    
    { 生成由CA签名的证书
      参数: 选项, CA证书PEM, CA私钥PEM
      返回: PEM格式的新证书和私钥 }
    class function GenerateSigned(
      const AOptions: TCertGenOptions;
      const ACA_CertPEM, ACA_KeyPEM: string;
      out ACertPEM, AKeyPEM: string
    ): Boolean;
    
    { 验证证书链
      参数: 证书PEM, CA证书路径
      返回: 验证是否成功 }
    class function VerifyChain(
      const ACertPEM: string;
      const ACAPath: string = ''
    ): Boolean;
    
    { X509_NAME比较 }
    class function CompareX509Names(const AName1, AName2: string; ACaseInsensitive: Boolean = True): Boolean; static;
    
    { 提取证书信息
      参数: 证书PEM
      返回: 证书详细信息 }
    class function GetInfo(const ACertPEM: string): TCertInfo;
    
    { 格式转换: PEM <-> DER
      参数: 输入数据, 源格式, 目标格式
      返回: 转换后的数据 }
    class function ConvertFormat(
      const AInput: TBytes;
      AFromFormat, AToFormat: TCertFormat
    ): TBytes;
    
    { PEM转DER }
    class function PEMToDER(const APEM: string): TBytes;
    
    { DER转PEM }
    class function DERToPEM(const ADER: TBytes): string;
    
    { 从文件加载证书 }
    class function LoadFromFile(const AFileName: string): string;
    
    { 保存证书到文件 }
    class function SaveToFile(const AFileName, ACertPEM: string): Boolean;
    
    { 验证证书有效期 }
    class function IsValid(const ACertPEM: string): Boolean;
    
    { 获取证书指纹 (SHA256) }
    class function GetFingerprint(const ACertPEM: string): string;

    { ==================== Try* 方法（不抛异常版本） ==================== }

    {** 尝试生成自签名证书（不抛异常）
        @param AOptions 证书生成选项
        @param ACertPEM 输出证书PEM
        @param AKeyPEM 输出私钥PEM
        @return 成功返回True *}
    class function TryGenerateSelfSigned(
      const AOptions: TCertGenOptions;
      out ACertPEM, AKeyPEM: string
    ): Boolean;

    {** 尝试生成自签名证书（简化版，不抛异常） *}
    class function TryGenerateSelfSignedSimple(
      const ACommonName, AOrganization: string;
      AValidDays: Integer;
      out ACertPEM, AKeyPEM: string
    ): Boolean;

    {** 尝试生成CA签名证书（不抛异常） *}
    class function TryGenerateSigned(
      const AOptions: TCertGenOptions;
      const ACA_CertPEM, ACA_KeyPEM: string;
      out ACertPEM, AKeyPEM: string
    ): Boolean;

    {** 尝试获取证书信息（不抛异常）
        @param ACertPEM 证书PEM
        @param AInfo 输出证书信息
        @return 成功返回True *}
    class function TryGetInfo(
      const ACertPEM: string;
      out AInfo: TCertInfo
    ): Boolean;

    {** 尝试PEM转DER（不抛异常） *}
    class function TryPEMToDER(
      const APEM: string;
      out ADER: TBytes
    ): Boolean;

    {** 尝试DER转PEM（不抛异常） *}
    class function TryDERToPEM(
      const ADER: TBytes;
      out APEM: string
    ): Boolean;

    {** 尝试从文件加载证书（不抛异常） *}
    class function TryLoadFromFile(
      const AFileName: string;
      out ACertPEM: string
    ): Boolean;

    {** 尝试获取证书指纹（不抛异常） *}
    class function TryGetFingerprint(
      const ACertPEM: string;
      out AFingerprint: string
    ): Boolean;

    {** 尝试验证证书链（不抛异常） *}
    class function TryVerifyChain(
      const ACertPEM: string;
      const ACAPath: string;
      out AIsValid: Boolean
    ): Boolean;
  end;

implementation

uses
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.stack,  // Added stack support
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.certchain;

{ TCertificateUtils }

{**
 * 确保OpenSSL已正确初始化
 * 加载所有证书操作需要的OpenSSL模块
 *
 * @raises ESSLInitError 初始化失败
 *}
class procedure TCertificateUtils.EnsureInitialized;
begin
  if TOpenSSLLoader.IsModuleLoaded(osmInitCert) then
    Exit;

  // 加载OpenSSL核心
  if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
  begin
    try
      LoadOpenSSLCore();
    except
      on E: Exception do
        RaiseInitializationError('OpenSSL core', E.Message);
    end;
  end;

  if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
    RaiseInitializationError('OpenSSL core', 'library not available');

  // 加载证书相关模块
  try
    // X509证书操作
    if not Assigned(X509_new) then
      LoadOpenSSLX509();

    // PEM格式支持
    if not Assigned(PEM_write_bio_X509) then
      LoadOpenSSLPEM(GetCryptoLibHandle);

    // BIO输入/输出
    if not Assigned(BIO_new) then
      LoadOpenSSLBIO();

    // EVP高级接口
    LoadEVP(GetCryptoLibHandle);

    // RSA密钥（按需）
    if not Assigned(RSA_new) then
      LoadOpenSSLRSA();

    // 大数运算
    if not Assigned(BN_new) then
      LoadOpenSSLBN();

    // ASN1编码 (ASN1_INTEGER_set等)
    LoadOpenSSLASN1(GetCryptoLibHandle);

    // EC密钥（按需）
    if not TOpenSSLLoader.IsModuleLoaded(osmEC) then
      LoadECFunctions(GetCryptoLibHandle);

    // OBJ对象识别 (OBJ_txt2nid等)
    LoadOBJModule(GetCryptoLibHandle);

    // X509V3扩展支持
    LoadX509V3Functions(GetCryptoLibHandle);

  except
    on E: Exception do
      RaiseInitializationError('Certificate modules', E.Message);
  end;

  TOpenSSLLoader.SetModuleLoaded(osmInitCert, True);
end;

class function TCertificateUtils.DefaultGenOptions: TCertGenOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.CommonName := 'localhost';
  Result.Organization := 'fafafa.ssl';
  Result.OrganizationalUnit := 'Development';
  Result.Country := 'US';
  Result.State := 'California';
  Result.Locality := 'San Francisco';
  Result.ValidDays := DEFAULT_CERT_VALID_DAYS;
  Result.NotBefore := 0;  // 0 = use current time
  Result.NotAfter := 0;   // 0 = calculate from ValidDays
  Result.KeyType := ktRSA;
  Result.KeyBits := DEFAULT_RSA_KEY_BITS;
  Result.ECCurve := DEFAULT_EC_CURVE;
  Result.SubjectAltNames := nil;
  Result.IsCA := False;
  Result.SerialNumber := 0;
  Result.OCSPResponderURL := '';
end;

{**
 * 生成RSA密钥对
 * 
 * @param ABits 密钥位数（2048/4096）
 * @return EVP_PKEY指针
 * @raises ESSLCertError 生成失败
 *}
class function TCertificateUtils.GenerateRSAKey(ABits: Integer): PEVP_PKEY;
var
  LKey: PRSA;
  LExp: PBIGNUM;
begin
  Result := nil;
  
  EnsureInitialized;
  
  // 验证参数
  if (ABits < 1024) or (ABits > 8192) then
    RaiseInvalidParameter('RSA key size (valid range: 1024-8192)');
  
  LKey := RSA_new();
  if LKey = nil then
    raise ESSLCertError.Create('Failed to create RSA key structure');
  
  LExp := BN_new();
  if LExp = nil then
  begin
    RSA_free(LKey);
    raise ESSLCertError.Create('Failed to create BIGNUM for exponent');
  end;
  
  try
    BN_set_word(LExp, RSA_EXPONENT_F4);
    
    if RSA_generate_key_ex(LKey, ABits, LExp, nil) <> 1 then
    begin
      RSA_free(LKey);
      raise ESSLCertError.CreateFmt(
        'Failed to generate %d-bit RSA key',
        [ABits]
      );
    end;
    
    Result := EVP_PKEY_new();
    if Result = nil then
    begin
      RSA_free(LKey);
      raise ESSLCertError.Create('Failed to create EVP_PKEY');
    end;
    
    if EVP_PKEY_assign(Result, EVP_PKEY_RSA, LKey) <> 1 then
    begin
      EVP_PKEY_free(Result);
      RSA_free(LKey);
      raise ESSLCertError.Create('Failed to assign RSA key to EVP_PKEY');
    end;
  finally
    BN_free(LExp);
  end;
end;

{**
 * 生成椭圆曲线密钥对
 * 
 * @param ACurve 曲线名称（如'prime256v1', 'secp384r1'）
 * @return EVP_PKEY指针
 * @raises ESSLInvalidArgument 无效的曲线名称
 * @raises ESSLCertError 生成失败
 *}
class function TCertificateUtils.GenerateECKey(const ACurve: string): PEVP_PKEY;
var
  LKey: PEC_KEY;
  LNID: Integer;
begin
  Result := nil;
  
  EnsureInitialized;
  
  // 验证参数
  if ACurve = '' then
    RaiseInvalidParameter('EC curve name');
  
  // 获取曲线NID
  LNID := OBJ_txt2nid(PAnsiChar(AnsiString(ACurve)));
  if LNID = NID_undef then
    RaiseInvalidParameter('EC curve name (unknown curve)');
  
  LKey := EC_KEY_new_by_curve_name(LNID);
  if LKey = nil then
    raise ESSLCertError.CreateFmt('Failed to create EC key for curve %s', [ACurve]);
  
  try
    if EC_KEY_generate_key(LKey) <> 1 then
      raise ESSLCertError.CreateFmt('Failed to generate EC key for curve %s', [ACurve]);
    
    Result := EVP_PKEY_new();
    if Result = nil then
      raise ESSLCertError.Create('Failed to create EVP_PKEY');
    
    if EVP_PKEY_assign(Result, EVP_PKEY_EC, LKey) <> 1 then
    begin
      EVP_PKEY_free(Result);
      raise ESSLCertError.Create('Failed to assign EC key to EVP_PKEY');
    end;
  except
    EC_KEY_free(LKey);
    if Result <> nil then
      EVP_PKEY_free(Result);
    raise;  // 重新抛出异常
  end;
end;

class function TCertificateUtils.AddExtension(ACert: PX509; AIssuer: PX509; ANID: Integer; const AValue: string): Boolean;
var
  LExt: PX509_EXTENSION;
  LCtx: X509V3_CTX;
begin
  Result := False;
  X509V3_set_ctx(@LCtx, AIssuer, ACert, nil, nil, 0);
  LExt := X509V3_EXT_conf_nid(nil, @LCtx, ANID, PAnsiChar(AnsiString(AValue)));
  if LExt <> nil then
  begin
    Result := X509_add_ext(ACert, LExt, -1) = 1;
    X509_EXTENSION_free(LExt);
  end;
end;

{**
 * 添加X509名称条目
 * 
 * @param AName X509_NAME指针
 * @param AKey 字段名（如'CN', 'O', 'C'）
 * @param AValue 字段值
 * @return 成功返回True
 * @raises ESSLCertError 添加失败
 *}
class function TCertificateUtils.AddNameEntry(
  AName: PX509_NAME; 
  const AKey, AValue: string
): Boolean;
begin
  if (AName = nil) or (AValue = '') then
    Exit(False);
  
  Result := X509_NAME_add_entry_by_txt(
    AName,
    PAnsiChar(AnsiString(AKey)),
    $1001,  // MBSTRING_ASC
    PByte(PAnsiChar(AnsiString(AValue))),
    -1, -1, 0
  ) = 1;
  
  if not Result then
    raise ESSLCertError.CreateFmt(
      'Failed to add X509 name entry %s=%s',
      [AKey, AValue]
    );
end;

{**
 * 生成自签名证书
 * 
 * 创建X.509v3自签名证书，用于HTTPS服务器、客户端认证等场景。
 * 证书和私钥均以PEM格式输出。
 * 
 * @param AOptions 证书生成选项（使用DefaultGenOptions获取默认值）
 * @param ACertPEM 输出：PEM格式证书
 * @param AKeyPEM 输出：PEM格式私钥
 * 
 * @raises ESSLInvalidArgument 参数无效
 * @raises ESSLCertError 证书生成失败
 * 
 * @example
 * <code>
 *   var LCert, LKey: string;
 *   var LOptions: TCertGenOptions;
 *   LOptions := TCertificateUtils.DefaultGenOptions;
 *   LOptions.CommonName := 'example.com';
 *   LOptions.ValidDays := 730;
 *   TCertificateUtils.GenerateSelfSigned(LOptions, LCert, LKey);
 * </code>
 *}
class function TCertificateUtils.GenerateSelfSigned(
  const AOptions: TCertGenOptions;
  out ACertPEM, AKeyPEM: string
): Boolean;
var
  LKey: PEVP_PKEY;
  LCert: PX509;
  LName: PX509_NAME;
  LSerial: PASN1_INTEGER;
  LNotBefore, LNotAfter: PASN1_TIME;
  LBIO: PBIO;
  LBuffer: array[0..8191] of AnsiChar;
  LLen: Integer;
  LNid: Integer;
begin
  Result := False;
  ACertPEM := '';
  AKeyPEM := '';
  
  EnsureInitialized;
  
  // 验证参数
  if AOptions.CommonName = '' then
    RaiseInvalidParameter('CommonName');
  if AOptions.ValidDays <= 0 then
    RaiseInvalidParameter('ValidDays (must be positive)');
  
  // 生成密钥
  case AOptions.KeyType of
    ktRSA: LKey := GenerateRSAKey(AOptions.KeyBits);
    ktECDSA: LKey := GenerateECKey(AOptions.ECCurve);
    else
      RaiseUnsupported('key type');
  end;
  
  if LKey = nil then
    raise ESSLCertError.Create('Failed to generate private key');
  
  try
    // 创建证书
    LCert := X509_new();
    if LCert = nil then
      raise ESSLCertError.Create('Failed to create X509 certificate');
    
    try
      // 设置版本 (X509v3)
      X509_set_version(LCert, X509_VERSION_3);
      
      // 设置序列号
      LSerial := X509_get_serialNumber(LCert);
      if AOptions.SerialNumber > 0 then
        ASN1_INTEGER_set(LSerial, AOptions.SerialNumber)
      else
        ASN1_INTEGER_set(LSerial, DEFAULT_SERIAL_NUMBER);
      
      // 设置有效期
      LNotBefore := X509_get_notBefore(LCert);
      LNotAfter := X509_get_notAfter(LCert);
      
      // 支持自定义起始时间
      if AOptions.NotBefore > 0 then
        X509_gmtime_adj(LNotBefore, Trunc((AOptions.NotBefore - Now) * 24 * 60 * 60))
      else
        X509_gmtime_adj(LNotBefore, 0);  // 使用当前时间
        
      // 支持自定义结束时间
      if AOptions.NotAfter > 0 then
        X509_gmtime_adj(LNotAfter, Trunc((AOptions.NotAfter - Now) * 24 * 60 * 60))
      else
        X509_gmtime_adj(LNotAfter, Int64(AOptions.ValidDays) * 24 * 60 * 60);
      
      // 设置公钥
      X509_set_pubkey(LCert, LKey);
      
      // 设置主题名称
      LName := X509_get_subject_name(LCert);
      if AOptions.Country <> '' then
        AddNameEntry(LName, 'C', AOptions.Country);
      if AOptions.State <> '' then
        AddNameEntry(LName, 'ST', AOptions.State);
      if AOptions.Locality <> '' then
        AddNameEntry(LName, 'L', AOptions.Locality);
      if AOptions.Organization <> '' then
        AddNameEntry(LName, 'O', AOptions.Organization);
      if AOptions.OrganizationalUnit <> '' then
        AddNameEntry(LName, 'OU', AOptions.OrganizationalUnit);
      AddNameEntry(LName, 'CN', AOptions.CommonName);
      
      // 自签名：颁发者=主题
      X509_set_issuer_name(LCert, LName);
      
      // Add extensions
      // Basic Constraints
      if AOptions.IsCA then
      begin
        if not AddExtension(LCert, LCert, NID_basic_constraints, 'critical,CA:TRUE') then
        ;
      end
      else
        AddExtension(LCert, LCert, NID_basic_constraints, 'CA:FALSE');
        
      // Key Usage
      if AOptions.IsCA then
      begin
        if not AddExtension(LCert, LCert, NID_key_usage, 'critical,keyCertSign,cRLSign') then
        ;
      end
      else
        AddExtension(LCert, LCert, NID_key_usage, 'digitalSignature,keyEncipherment');
        
      // Subject Key Identifier
      AddExtension(LCert, LCert, NID_subject_key_identifier, 'hash');
      
      // Authority Key Identifier
      AddExtension(LCert, LCert, NID_authority_key_identifier, 'keyid:always');

      // Subject Alternative Names
      if (AOptions.SubjectAltNames <> nil) and (AOptions.SubjectAltNames.Count > 0) then
      begin
        // Force comma delimiter without quotes
        AOptions.SubjectAltNames.Delimiter := ',';
        AOptions.SubjectAltNames.QuoteChar := #0;
        if not AddExtension(LCert, LCert, NID_subject_alt_name, AOptions.SubjectAltNames.DelimitedText) then
          ; // Check for failure silently or convert to exception if critical

      end;

      // Authority Information Access (OCSP)
      if (AOptions.OCSPResponderURL <> '') and Assigned(OBJ_txt2nid) then
      begin
        // OID: 1.3.6.1.5.5.7.1.1 (authorityInfoAccess)
        LNid := OBJ_txt2nid(PAnsiChar(AnsiString('1.3.6.1.5.5.7.1.1')));
        if LNid <> NID_undef then
          AddExtension(LCert, LCert, LNid, 'OCSP;URI:' + AOptions.OCSPResponderURL);
      end;

      // 签名
      if X509_sign(LCert, LKey, EVP_sha256()) = 0 then
        raise ESSLCertError.Create('Failed to sign certificate');
      
      // 导出证书为PEM
      LBIO := BIO_new(BIO_s_mem());
      if LBIO = nil then
        raise ESSLCertError.Create('Failed to create BIO for certificate export');
      
      try
        if PEM_write_bio_X509(LBIO, LCert) <> 1 then
          raise ESSLCertError.Create('Failed to write certificate to PEM');
        LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer));
        if LLen > 0 then
          SetString(ACertPEM, PAnsiChar(@LBuffer[0]), LLen)
        else
          raise ESSLCertError.Create('Failed to read certificate PEM data');
      finally
        BIO_free(LBIO);
      end;
      
      // 导出私钥为PEM
      LBIO := BIO_new(BIO_s_mem());
      if LBIO = nil then
        raise ESSLCertError.Create('Failed to create BIO for key export');
      
      try
        if PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil) <> 1 then
          raise ESSLCertError.Create('Failed to write private key to PEM');
        LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer));
        if LLen > 0 then
          SetString(AKeyPEM, PAnsiChar(@LBuffer[0]), LLen)
        else
          raise ESSLCertError.Create('Failed to read private key PEM data');
      finally
        BIO_free(LBIO);
      end;
      
      Result := (ACertPEM <> '') and (AKeyPEM <> '');
      
    finally
      X509_free(LCert);
    end;
  finally
    EVP_PKEY_free(LKey);
  end;
end;

class function TCertificateUtils.GenerateSelfSignedSimple(
  const ACommonName, AOrganization: string;
  AValidDays: Integer;
  out ACertPEM, AKeyPEM: string
): Boolean;
var
  LOptions: TCertGenOptions;
begin
  LOptions := DefaultGenOptions;
  LOptions.CommonName := ACommonName;
  LOptions.Organization := AOrganization;
  LOptions.ValidDays := AValidDays;
  Result := GenerateSelfSigned(LOptions, ACertPEM, AKeyPEM);
end;

class function TCertificateUtils.GenerateSigned(
  const AOptions: TCertGenOptions;
  const ACA_CertPEM, ACA_KeyPEM: string;
  out ACertPEM, AKeyPEM: string
): Boolean;
var
  LKey, LCAKey: PEVP_PKEY;
  LCert, LCACert: PX509;
  LName, LCAName: PX509_NAME;
  LSerial: PASN1_INTEGER;
  LNotBefore, LNotAfter: PASN1_TIME;
  LBIO: PBIO;
  LBuffer: array[0..8191] of AnsiChar;
  LLen: Integer;
  LNid: Integer;
begin
  Result := False;
  ACertPEM := '';
  AKeyPEM := '';
  
  EnsureInitialized;
  
  // 1. 加载CA证书和私钥
  LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(ACA_CertPEM)), Length(ACA_CertPEM));
  if LBIO = nil then 
    raise ESSLCertError.Create('Failed to create BIO for CA cert');
  try
    LCACert := PEM_read_bio_X509(LBIO, nil, nil, nil);
  finally
    BIO_free(LBIO);
  end;
  if LCACert = nil then 
    raise ESSLCertError.Create('Failed to parse CA certificate');
  
  try
    LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(ACA_KeyPEM)), Length(ACA_KeyPEM));
    if LBIO = nil then 
      raise ESSLCertError.Create('Failed to create BIO for CA key');
    try
      LCAKey := PEM_read_bio_PrivateKey(LBIO, nil, nil, nil);
    finally
      BIO_free(LBIO);
    end;
    if LCAKey = nil then 
      raise ESSLCertError.Create('Failed to parse CA private key');
    
    try
      // 2. 生成新密钥对
      case AOptions.KeyType of
        ktRSA: LKey := GenerateRSAKey(AOptions.KeyBits);
        ktECDSA: LKey := GenerateECKey(AOptions.ECCurve);
        else
          RaiseUnsupported('key type');
      end;
      if LKey = nil then 
        raise ESSLCertError.Create('Failed to generate key pair');
      
      try
        // 3. 创建新证书
        LCert := X509_new();
        if LCert = nil then 
          raise ESSLCertError.Create('Failed to create X509 structure');
        
        try
          X509_set_version(LCert, X509_VERSION_3);
          
          LSerial := X509_get_serialNumber(LCert);
          if AOptions.SerialNumber > 0 then
            ASN1_INTEGER_set(LSerial, AOptions.SerialNumber)
          else
            ASN1_INTEGER_set(LSerial, DEFAULT_SERIAL_NUMBER + Random(10000)); // 简单随机
            
          LNotBefore := X509_get_notBefore(LCert);
          LNotAfter := X509_get_notAfter(LCert);
          X509_gmtime_adj(LNotBefore, 0);
          X509_gmtime_adj(LNotAfter, Int64(AOptions.ValidDays) * 24 * 60 * 60);
          
          X509_set_pubkey(LCert, LKey);
          
          // 设置主题
          LName := X509_get_subject_name(LCert);
          if AOptions.Country <> '' then AddNameEntry(LName, 'C', AOptions.Country);
          if AOptions.State <> '' then AddNameEntry(LName, 'ST', AOptions.State);
          if AOptions.Locality <> '' then AddNameEntry(LName, 'L', AOptions.Locality);
          if AOptions.Organization <> '' then AddNameEntry(LName, 'O', AOptions.Organization);
          if AOptions.OrganizationalUnit <> '' then AddNameEntry(LName, 'OU', AOptions.OrganizationalUnit);
          AddNameEntry(LName, 'CN', AOptions.CommonName);
          
          // 设置颁发者（从CA证书获取）
          LCAName := X509_get_subject_name(LCACert);
          X509_set_issuer_name(LCert, LCAName);
          
          // 添加扩展
          // Basic Constraints
          if AOptions.IsCA then
            AddExtension(LCert, LCACert, NID_basic_constraints, 'critical,CA:TRUE')
          else
            AddExtension(LCert, LCACert, NID_basic_constraints, 'CA:FALSE');
            
          // Key Usage
          if AOptions.IsCA then
            AddExtension(LCert, LCACert, NID_key_usage, 'critical,keyCertSign,cRLSign')
          else
            AddExtension(LCert, LCACert, NID_key_usage, 'digitalSignature,keyEncipherment');
            
          // Subject Key Identifier
          AddExtension(LCert, LCACert, NID_subject_key_identifier, 'hash');
          
          // Authority Key Identifier
          AddExtension(LCert, LCACert, NID_authority_key_identifier, 'keyid:always,issuer');
          
          // Subject Alternative Names
          if (AOptions.SubjectAltNames <> nil) and (AOptions.SubjectAltNames.Count > 0) then
            AddExtension(LCert, LCACert, NID_subject_alt_name, AOptions.SubjectAltNames.DelimitedText);

          // Authority Information Access (OCSP)
          if (AOptions.OCSPResponderURL <> '') and Assigned(OBJ_txt2nid) then
          begin
            // OID: 1.3.6.1.5.5.7.1.1 (authorityInfoAccess)
            LNid := OBJ_txt2nid(PAnsiChar(AnsiString('1.3.6.1.5.5.7.1.1')));
            if LNid <> NID_undef then
              AddExtension(LCert, LCACert, LNid, 'OCSP;URI:' + AOptions.OCSPResponderURL);
          end;

          // 4. 使用CA私钥签名
          if X509_sign(LCert, LCAKey, EVP_sha256()) = 0 then
            raise ESSLCertError.Create('Failed to sign certificate with CA key');
            
          // 5. 导出
          LBIO := BIO_new(BIO_s_mem());
          try
            if PEM_write_bio_X509(LBIO, LCert) <> 1 then
              raise ESSLCertError.Create('Failed to write certificate to PEM');
              
            LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer));
            if LLen > 0 then
              SetString(ACertPEM, PAnsiChar(@LBuffer[0]), LLen)
            else
              raise ESSLCertError.Create('Failed to read certificate PEM data');
          finally
            BIO_free(LBIO);
          end;
          
          LBIO := BIO_new(BIO_s_mem());
          try
            if PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil) <> 1 then
              raise ESSLCertError.Create('Failed to write private key to PEM');
              
            LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer));
            if LLen > 0 then
              SetString(AKeyPEM, PAnsiChar(@LBuffer[0]), LLen)
            else
              raise ESSLCertError.Create('Failed to read private key PEM data');
          finally
            BIO_free(LBIO);
          end;
          
          Result := (ACertPEM <> '') and (AKeyPEM <> '');
          
        finally
          X509_free(LCert);
        end;
      finally
        EVP_PKEY_free(LKey);
      end;
    finally
      EVP_PKEY_free(LCAKey);
    end;
  finally
    X509_free(LCACert);
  end;
end;

class function TCertificateUtils.X509NameToString(AName: PX509_NAME): string;
var
  LBIO: PBIO;
  LBuffer: array[0..1023] of AnsiChar;
  LLen: Integer;
begin
  Result := '';
  if AName = nil then Exit;
  
  LBIO := BIO_new(BIO_s_mem());
  if LBIO = nil then Exit;
  
  try
    X509_NAME_print_ex(LBIO, AName, 0, 0);
    LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer));
    if LLen > 0 then
      SetString(Result, PAnsiChar(@LBuffer[0]), LLen);
  finally
    BIO_free(LBIO);
  end;
end;

class function TCertificateUtils.ASN1TimeToDateTime(ATime: Pointer): TDateTime;
begin
  // Delegate to the full implementation in OpenSSL ASN1 API
  Result := fafafa.ssl.openssl.api.asn1.ASN1TimeToDateTime(ASN1_TIME(ATime));
end;

class function TCertificateUtils.GetInfo(const ACertPEM: string): TCertInfo;
var
  LBIO: PBIO;
  LCert: PX509;
  // SAN parsing variables
  LExtNames: POPENSSL_STACK;
  LGenName: Pointer; // PGENERAL_NAME
  LType: Integer;
  LVal: Pointer;
  LCount, I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.SubjectAltNames := TStringList.Create;
  
  if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
    LoadOpenSSLCore();

  if not Assigned(X509_new) then
    LoadOpenSSLX509();
  if not Assigned(BIO_new) then
    LoadOpenSSLBIO();
  if not Assigned(PEM_read_bio_X509) then
    LoadOpenSSLPEM(GetCryptoLibHandle);

  if not TOpenSSLLoader.IsModuleLoaded(osmStack) then
    LoadStackFunctions(); // Ensure stack functions are loaded for SAN extraction
  
  LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(ACertPEM)), Length(ACertPEM));
  if LBIO = nil then Exit;
  
  try
    LCert := PEM_read_bio_X509(LBIO, nil, nil, nil);
    if LCert = nil then Exit;
    
    try
      Result.Subject := X509NameToString(X509_get_subject_name(LCert));
      Result.Issuer := X509NameToString(X509_get_issuer_name(LCert));
      Result.Version := X509_get_version(LCert) + 1;
      Result.NotBefore := ASN1TimeToDateTime(X509_get_notBefore(LCert));
      Result.NotBefore := ASN1TimeToDateTime(X509_get_notBefore(LCert));
      Result.NotAfter := ASN1TimeToDateTime(X509_get_notAfter(LCert));
      
      // Extract IsCA status
      if Assigned(X509_check_ca) then
      begin
        // Force extension caching if purpose checking is available
        if Assigned(X509_check_purpose) then
          X509_check_purpose(LCert, -1, 0);
          
        Result.IsCA := (X509_check_ca(LCert) >= 1);
      end
      else
        Result.IsCA := False;
      
      
      // Extract Subject Alternative Names
      if Assigned(X509_get_ext_d2i) and TOpenSSLLoader.IsModuleLoaded(osmStack) then
      begin
        // Stack loaded
        LExtNames := POPENSSL_STACK(X509_get_ext_d2i(LCert, NID_subject_alt_name, nil, nil));
        if LExtNames <> nil then
        try
          LCount := OPENSSL_sk_num(LExtNames);
          // SAN extension found
          for I := 0 to LCount - 1 do
          begin
            LGenName := OPENSSL_sk_value(LExtNames, I);
            if LGenName <> nil then
            begin
              // GEN_DNS = 2
              LVal := GENERAL_NAME_get0_value(LGenName, @LType);
              if (LType = 2) and (LVal <> nil) then // 2 = GEN_DNS
              begin
                // LVal is ASN1_STRING (implicitly).
                Result.SubjectAltNames.Add('DNS:' + ASN1StringToString(ASN1_STRING(LVal)));
              end;
            end;
          end;
        finally
          GENERAL_NAMES_free(LExtNames);
        end
        else
          ;// WriteLn('Debug: No SAN extension found (X509_get_ext_d2i returned nil)');
      end
      else
        ;// WriteLn('Debug: X509_get_ext_d2i not assigned or stack not loaded');
      
    finally
      X509_free(LCert);
    end;
  finally
    BIO_free(LBIO);
  end;
end;

class function TCertificateUtils.VerifyChain(
  const ACertPEM: string;
  const ACAPath: string
): Boolean;
var
  LCert, LInterCert: ISSLCertificate;
  LStore, LInterStore: ISSLCertificateStore;
  LVerifier: ISSLCertificateChainVerifier;
  LResult: TChainVerifyResult;
  LBIO, LOutBIO: PBIO;
  LX509: PX509;
  LBuffer: array[0..8191] of AnsiChar;
  LLen: Integer;
  LInterPEM: string;
begin
  Result := False;
  LCert := nil;
  LInterStore := nil;
  
  EnsureInitialized;
  
  // 1. 加载叶证书 (第一个证书)
  LCert := TSSLFactory.CreateCertificate;
  if not LCert.LoadFromPEM(ACertPEM) then
    Exit;
    
  // 2. 创建验证器
  LVerifier := TSSLCertificateChainVerifier.Create;
  
  // 3. 尝试加载中间证书（如果有）
  LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(ACertPEM)), Length(ACertPEM));
  if LBIO <> nil then
  try
    // 跳过第一个证书（已作为叶证书加载）
    LX509 := PEM_read_bio_X509(LBIO, nil, nil, nil);
    if LX509 <> nil then
      X509_free(LX509);
      
    // 读取剩余的证书作为中间证书
    while True do
    begin
      LX509 := PEM_read_bio_X509(LBIO, nil, nil, nil);
      if LX509 = nil then Break;
      
      try
        // 将 PX509 转换为 PEM 字符串
        LOutBIO := BIO_new(BIO_s_mem());
        if LOutBIO <> nil then
        try
          if PEM_write_bio_X509(LOutBIO, LX509) = 1 then
          begin
            LLen := BIO_get_mem_data(LOutBIO, PPAnsiChar(@LBuffer));
            if LLen > 0 then
            begin
              SetString(LInterPEM, PAnsiChar(@LBuffer[0]), LLen);
              
              // 创建并加载中间证书
              LInterCert := TSSLFactory.CreateCertificate;
              if LInterCert.LoadFromPEM(LInterPEM) then
              begin
                if LInterStore = nil then
                  LInterStore := TSSLFactory.CreateCertificateStore;
                LInterStore.AddCertificate(LInterCert);
              end;
            end;
          end;
        finally
          BIO_free(LOutBIO);
        end;
      finally
        X509_free(LX509);
      end;
    end;
    
    // 如果找到了中间证书，设置到验证器
    if LInterStore <> nil then
      LVerifier.SetIntermediateStore(LInterStore);
      
  finally
    BIO_free(LBIO);
  end;
  
  // 4. 加载CA证书（如果提供）
  if ACAPath <> '' then
  begin
    try
      LStore := TSSLFactory.CreateCertificateStore;
      if DirectoryExists(ACAPath) then
      begin
        if not LStore.LoadFromPath(ACAPath) then
          ; // 忽略部分加载错误
      end
      else if FileExists(ACAPath) then
      begin
        if not LStore.LoadFromFile(ACAPath) then
          ;
      end;
      
      // 设置信任存储
      if LStore.GetCount > 0 then
        LVerifier.SetTrustedStore(LStore);
    except
      on E: Exception do
      begin
        // P1-2.4: 忽略存储加载错误（非关键）
        {$IFDEF DEBUG}
        WriteLn('[DEBUG] fafafa.ssl.cert.utils: Trust store load failed: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;
  
  // 5. 执行验证
  LVerifier.SetOptions(StrictChainVerifyOptions);
  
  LResult := LVerifier.VerifyCertificate(LCert);
  Result := LResult.IsValid;
end;

class function TCertificateUtils.ConvertFormat(
  const AInput: TBytes;
  AFromFormat, AToFormat: TCertFormat
): TBytes;
begin
  SetLength(Result, 0);
  
  if AFromFormat = AToFormat then
  begin
    Result := Copy(AInput);
    Exit;
  end;
  
  if AFromFormat = cfPEM then
    Result := PEMToDER(TEncoding.ASCII.GetString(AInput))
  else
    Result := TEncoding.ASCII.GetBytes(DERToPEM(AInput));
end;

class function TCertificateUtils.PEMToDER(const APEM: string): TBytes;
var
  LBIO: PBIO;
  LCert: PX509;
  LDERBuf: PByte;
  LDERLen: Integer;
begin
  SetLength(Result, 0);
  
  if APEM = '' then
    Exit;
  
  EnsureInitialized;
  
  // 从PEM字符串加载证书
  LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(APEM)), Length(APEM));
  if LBIO = nil then Exit;
  
  try
    LCert := PEM_read_bio_X509(LBIO, nil, nil, nil);
    if LCert = nil then Exit;
    
    try
      // 转换为DER格式 (两阶段：先获取长度，再转换)
      LDERLen := i2d_X509(LCert, nil);  // 第一阶段：获取需要的buffer大小
      
      if LDERLen > 0 then
      begin
        SetLength(Result, LDERLen);
        LDERBuf := @Result[0];
        // 第二阶段：实际转换 (注意：i2d_X509会修改指针)
        if i2d_X509(LCert, @LDERBuf) <> LDERLen then
          SetLength(Result, 0);  // 转换失败
      end;
    finally
      X509_free(LCert);
    end;
  finally
    BIO_free(LBIO);
  end;
end;

class function TCertificateUtils.DERToPEM(const ADER: TBytes): string;
var
  LBIO: PBIO;
  LCert: PX509;
  LDataPtr: PAnsiChar;
  LLen: Integer;
  LDERPtr: PByte;
begin
  Result := '';
  
  if Length(ADER) = 0 then
    Exit;
  
  EnsureInitialized;
  
  // 从DER字节加载证书
  LDERPtr := @ADER[0];
  LCert := d2i_X509(nil, @LDERPtr, Length(ADER));
  if LCert = nil then Exit;
  
  try
    // 写入PEM格式
    LBIO := BIO_new(BIO_s_mem());
    if LBIO = nil then Exit;
    
    try
      if PEM_write_bio_X509(LBIO, LCert) = 1 then
      begin
        // 使用BIO_get_mem_data获取数据指针和长度
        LLen := BIO_get_mem_data(LBIO, @LDataPtr);
        if (LLen > 0) and (LDataPtr <> nil) then
          SetString(Result, LDataPtr, LLen);
      end;
    finally
      BIO_free(LBIO);
    end;
  finally
    X509_free(LCert);
  end;
end;

class function TCertificateUtils.LoadFromFile(const AFileName: string): string;
var
  LStream: TFileStream;
  LBytes: TBytes;
begin
  Result := '';
  if not FileExists(AFileName) then Exit;
  
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    if LStream.Size > 0 then
      LStream.Read(LBytes[0], LStream.Size);
    Result := TEncoding.UTF8.GetString(LBytes);
  finally
    LStream.Free;
  end;
end;

class function TCertificateUtils.SaveToFile(const AFileName, ACertPEM: string): Boolean;
var
  LStream: TFileStream;
  LBytes: TBytes;
begin
  Result := False;
  try
    LStream := TFileStream.Create(AFileName, fmCreate);
    try
      LBytes := TEncoding.UTF8.GetBytes(ACertPEM);
      if Length(LBytes) > 0 then
        LStream.Write(LBytes[0], Length(LBytes));
      Result := True;
    finally
      LStream.Free;
    end;
  except
    Result := False;
  end;
end;

class function TCertificateUtils.IsValid(const ACertPEM: string): Boolean;
var
  LInfo: TCertInfo;
begin
  LInfo := GetInfo(ACertPEM);
  try
    Result := (Now >= LInfo.NotBefore) and (Now <= LInfo.NotAfter);
  finally
    LInfo.SubjectAltNames.Free;
  end;
end;

{**
 * 计算证书指纹（SHA-256）
 * 
 * 证书指纹是证书的唯一标识，用于验证证书真实性
 * 
 * @param ACertPEM PEM格式证书
 * @return 64字符十六进制指纹字符串（小写）
 * @raises ESSLCertError 计算失败
 *}
class function TCertificateUtils.GetFingerprint(const ACertPEM: string): string;
var
  LBIO: PBIO;
  LCert: PX509;
  LHash: array[0..31] of Byte;  // SHA-256 = 32 bytes
  LLen: Cardinal;
  I: Integer;
begin
  Result := '';
  
  EnsureInitialized;
  
  if ACertPEM = '' then
    RaiseInvalidParameter('Certificate PEM');
  
  LBIO := BIO_new_mem_buf(PAnsiChar(AnsiString(ACertPEM)), Length(ACertPEM));
  if LBIO = nil then
    raise ESSLCertError.Create('Failed to create BIO for certificate');
  
  try
    LCert := PEM_read_bio_X509(LBIO, nil, nil, nil);
    if LCert = nil then
      raise ESSLCertError.Create('Failed to parse certificate PEM');
    
    try
      LLen := 32;
      if X509_digest(LCert, EVP_sha256(), @LHash[0], @LLen) <> 1 then
        raise ESSLCertError.Create('Failed to calculate certificate fingerprint');
      
      // 转换为十六进制字符串
      for I := 0 to 31 do
        Result := Result + LowerCase(IntToHex(LHash[I], 2));
        
    finally
      X509_free(LCert);
    end;
  finally
    BIO_free(LBIO);
  end;
end;

class function TCertificateUtils.CompareX509Names(
  const AName1, AName2: string;
  ACaseInsensitive: Boolean
): Boolean;
var
  LName1, LName2: string;
  LComponents1, LComponents2: TStringList;
  i: Integer;
  
  function NormalizeDN(const ADN: string): string;
  var
    s: string;
  begin
    s := Trim(ADN);
    s := StringReplace(s, ' = ', '=', [rfReplaceAll]);
    s := StringReplace(s, '= ', '=', [rfReplaceAll]);
    s := StringReplace(s, ' =', '=', [rfReplaceAll]);
    s := StringReplace(s, ', ', ',', [rfReplaceAll]);
    
    if ACaseInsensitive then
      Result := LowerCase(s)
    else
      Result := s;
  end;
  
  procedure ParseDN(const ADN: string; AList: TStringList);
  var
    Components: TStringArray;
    j: Integer;
  begin
    AList.Clear;
    AList.Sorted := True;
    AList.Duplicates := dupIgnore;
    
    Components := ADN.Split([',']);
    for j := 0 to Length(Components) - 1 do
      AList.Add(Trim(Components[j]));
  end;
  
begin
  Result := False;
  
  if AName1 = AName2 then
  begin
    Result := True;
    Exit;
  end;
  
  LName1 := NormalizeDN(AName1);
  LName2 := NormalizeDN(AName2);
  
  if LName1 = LName2 then
  begin
    Result := True;
    Exit;
  end;
  
  LComponents1 := TStringList.Create;
  LComponents2 := TStringList.Create;
  try
    ParseDN(LName1, LComponents1);
    ParseDN(LName2, LComponents2);
    
    if LComponents1.Count <> LComponents2.Count then
      Exit;
      
    Result := True;
    for i := 0 to LComponents1.Count - 1 do
    begin
      if LComponents1[i] <> LComponents2[i] then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    LComponents1.Free;
    LComponents2.Free;
  end;
end;

{ ==================== Try* 方法实现 ==================== }

class function TCertificateUtils.TryGenerateSelfSigned(
  const AOptions: TCertGenOptions;
  out ACertPEM, AKeyPEM: string
): Boolean;
begin
  try
    Result := GenerateSelfSigned(AOptions, ACertPEM, AKeyPEM);
  except
    ACertPEM := '';
    AKeyPEM := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryGenerateSelfSignedSimple(
  const ACommonName, AOrganization: string;
  AValidDays: Integer;
  out ACertPEM, AKeyPEM: string
): Boolean;
begin
  try
    Result := GenerateSelfSignedSimple(ACommonName, AOrganization, AValidDays, ACertPEM, AKeyPEM);
  except
    ACertPEM := '';
    AKeyPEM := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryGenerateSigned(
  const AOptions: TCertGenOptions;
  const ACA_CertPEM, ACA_KeyPEM: string;
  out ACertPEM, AKeyPEM: string
): Boolean;
begin
  try
    Result := GenerateSigned(AOptions, ACA_CertPEM, ACA_KeyPEM, ACertPEM, AKeyPEM);
  except
    ACertPEM := '';
    AKeyPEM := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryGetInfo(
  const ACertPEM: string;
  out AInfo: TCertInfo
): Boolean;
begin
  try
    AInfo := GetInfo(ACertPEM);
    Result := True;
  except
    FillChar(AInfo, SizeOf(AInfo), 0);
    AInfo.SubjectAltNames := TStringList.Create;
    Result := False;
  end;
end;

class function TCertificateUtils.TryPEMToDER(
  const APEM: string;
  out ADER: TBytes
): Boolean;
begin
  try
    ADER := PEMToDER(APEM);
    Result := Length(ADER) > 0;
  except
    SetLength(ADER, 0);
    Result := False;
  end;
end;

class function TCertificateUtils.TryDERToPEM(
  const ADER: TBytes;
  out APEM: string
): Boolean;
begin
  try
    APEM := DERToPEM(ADER);
    Result := APEM <> '';
  except
    APEM := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryLoadFromFile(
  const AFileName: string;
  out ACertPEM: string
): Boolean;
begin
  try
    ACertPEM := LoadFromFile(AFileName);
    Result := ACertPEM <> '';
  except
    ACertPEM := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryGetFingerprint(
  const ACertPEM: string;
  out AFingerprint: string
): Boolean;
begin
  try
    AFingerprint := GetFingerprint(ACertPEM);
    Result := AFingerprint <> '';
  except
    AFingerprint := '';
    Result := False;
  end;
end;

class function TCertificateUtils.TryVerifyChain(
  const ACertPEM: string;
  const ACAPath: string;
  out AIsValid: Boolean
): Boolean;
begin
  try
    AIsValid := VerifyChain(ACertPEM, ACAPath);
    Result := True;
  except
    AIsValid := False;
    Result := False;
  end;
end;

end.
