{
  fafafa.ssl.openssl.certificate - OpenSSL 证书实现
  版本: 1.0 (简化版)
  创建: 2025-11-02
}

unit fafafa.ssl.openssl.certificate;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.logging,  // P3-8: 添加日志支持
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.obj,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.consts;

type
  TOpenSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FX509: PX509;
    FOwnsHandle: Boolean;
    FIssuerCert: ISSLCertificate;  // Store issuer certificate for chain building
    // P3-10/15: Extract common fingerprint computation
    function ComputeFingerprint(MD: PEVP_MD): string;
  public
    constructor Create(AX509: PX509; AOwnsHandle: Boolean = True);
    destructor Destroy; override;
    
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
    function LoadFromPEM(const APEM: string): Boolean;
    function LoadFromDER(const ADER: TBytes): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function SaveToStream(AStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    function GetInfo: TSSLCertificateInfo;
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetPublicKey: string;
    function GetPublicKeyAlgorithm: string;
    function GetSignatureAlgorithm: string;
    function GetVersion: Integer;
    function Verify(ACAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(ACAStore: ISSLCertificateStore; 
      AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const AHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    function GetDaysUntilExpiry: Integer;
    function GetSubjectCN: string;
    function GetExtension(const AOID: string): string;
    function GetSubjectAltNames: TSSLStringArray;
    function GetKeyUsage: TSSLStringArray;
    function GetExtendedKeyUsage: TSSLStringArray;
    function GetFingerprint(AHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    procedure SetIssuerCertificate(ACert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

implementation

uses
  fafafa.ssl.utils;  // Phase 3.2 - StringsToArray 统一实现

const
  // X509_NAME print flags
  XN_FLAG_SEP_COMMA_PLUS = 1 shl 16;
  XN_FLAG_DN_REV = 1 shl 20;
  XN_FLAG_FN_SN = 0; // Short name
  XN_FLAG_ONELINE = XN_FLAG_SEP_COMMA_PLUS or XN_FLAG_DN_REV or XN_FLAG_FN_SN;

{ X509NameToString - 将 X509_NAME 转换为字符串

  此辅助函数统一处理 X509_NAME 到字符串的转换逻辑，
  避免 GetSubject 和 GetIssuer 中的代码重复。

  优先使用 X509_NAME_print_ex (RFC 2253 格式)，
  若不可用则回退到 X509_NAME_oneline。
}
function X509NameToString(AName: PX509_NAME): string;
var
  BIO: PBIO;
  Len: Integer;
  Buf: PAnsiChar;
begin
  Result := '';

  if AName = nil then
    Exit;

  // 优先使用 X509_NAME_print_ex (RFC 2253 风格)
  if Assigned(X509_NAME_print_ex) and Assigned(BIO_new) and
     Assigned(BIO_s_mem) and Assigned(BIO_free) then
  begin
    BIO := BIO_new(BIO_s_mem());
    if BIO <> nil then
    try
      if X509_NAME_print_ex(BIO, AName, 0, XN_FLAG_ONELINE) > 0 then
      begin
        Len := BIO_get_mem_data(BIO, PPAnsiChar(@Buf));
        if Len > 0 then
          SetString(Result, Buf, Len);
      end;
    finally
      BIO_free(BIO);
    end;
  end;

  // 后备方案：使用旧的 X509_NAME_oneline
  if (Result = '') and Assigned(X509_NAME_oneline) then
  begin
    Buf := X509_NAME_oneline(AName, nil, 0);
    if Buf <> nil then
    begin
      Result := string(Buf);
      if Assigned(OPENSSL_free) then
        OPENSSL_free(Buf);
    end;
  end;
end;

function IpBytesToString(AData: PByte; ALength: Integer): string;
var
  I: Integer;
  Value: Integer;
begin
  Result := '';
  if (AData = nil) or (ALength <= 0) then
    Exit;

  if ALength = 4 then
  begin
    Result := Format('%d.%d.%d.%d', [AData[0], AData[1], AData[2], AData[3]]);
    Exit;
  end;

  if ALength = 16 then
  begin
    for I := 0 to 7 do
    begin
      Value := (Integer(AData[I * 2]) shl 8) or Integer(AData[I * 2 + 1]);
      if I > 0 then
        Result := Result + ':';
      Result := Result + IntToHex(Value, 1);
    end;
    Exit;
  end;
end;

// StringsToArray 已移至 fafafa.ssl.utils（Phase 3.2）

constructor TOpenSSLCertificate.Create(AX509: PX509; AOwnsHandle: Boolean = True);
begin
  inherited Create;
  FX509 := AX509;
  FOwnsHandle := AOwnsHandle;
end;

destructor TOpenSSLCertificate.Destroy;
begin
  if FOwnsHandle and (FX509 <> nil) and not OpenSSLX509_Finalizing then
  begin
    if Assigned(X509_free) then
    begin
      try
        X509_free(FX509);
      except
        // P3-8: 记录异常而不是静默忽略
        on E: Exception do
          TSecurityLog.Warning('OpenSSL', Format('Exception in TOpenSSLCertificate.Destroy: %s', [E.Message]));
      end;
    end;
  end;
  inherited;
end;

function TOpenSSLCertificate.LoadFromFile(const AFileName: string): Boolean;
var
  F: File;
  BIO: PBIO;
  FileNameA: AnsiString;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  
  FileNameA := AnsiString(AFileName);
  BIO := BIO_new_file(PAnsiChar(FileNameA), 'r');
  if BIO = nil then Exit;
  
  try
    if FOwnsHandle and (FX509 <> nil) then
      X509_free(FX509);
    
    FX509 := PEM_read_bio_X509(BIO, nil, nil, nil);
    FOwnsHandle := True;
    Result := (FX509 <> nil);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.LoadFromStream(AStream: TStream): Boolean;
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
begin
  Result := False;

  // Validate stream
  if AStream = nil then
    Exit;

  Size := AStream.Size - AStream.Position;
  if Size <= 0 then
    Exit;

  SetLength(Data, Size);
  if AStream.Read(Data[0], Size) <> Size then
    Exit;

  BIO := BIO_new_mem_buf(@Data[0], Size);
  if BIO = nil then
    Exit;

  try
    if FOwnsHandle and (FX509 <> nil) then
      X509_free(FX509);

    FX509 := PEM_read_bio_X509(BIO, nil, nil, nil);
    if FX509 = nil then
      FX509 := d2i_X509_bio(BIO, nil);

    FOwnsHandle := True;
    Result := (FX509 <> nil);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
var
  BIO: PBIO;
begin
  Result := False;
  if (AData = nil) or (ASize <= 0) then
    Exit;

  // Free existing certificate if we own it
  if FOwnsHandle and (FX509 <> nil) then
  begin
    X509_free(FX509);
    FX509 := nil;
  end;

  // Try PEM format first
  BIO := BIO_new_mem_buf(AData, ASize);
  if BIO = nil then
    Exit;
  try
    FX509 := PEM_read_bio_X509(BIO, nil, nil, nil);
  finally
    BIO_free(BIO);
  end;

  // If PEM failed, try DER format
  if FX509 = nil then
  begin
    BIO := BIO_new_mem_buf(AData, ASize);
    if BIO = nil then
      Exit;
    try
      FX509 := d2i_X509_bio(BIO, nil);
    finally
      BIO_free(BIO);
    end;
  end;

  FOwnsHandle := True;
  Result := (FX509 <> nil);
end;

function TOpenSSLCertificate.LoadFromPEM(const APEM: string): Boolean;
var
  PEMData: AnsiString;
  BIO: PBIO;
begin
  PEMData := AnsiString(APEM);
  BIO := BIO_new_mem_buf(PAnsiChar(PEMData), Length(PEMData));
  try
    if FOwnsHandle and (FX509 <> nil) then
      X509_free(FX509);
    
    FX509 := PEM_read_bio_X509(BIO, nil, nil, nil);
    FOwnsHandle := True;
    Result := (FX509 <> nil);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.LoadFromDER(const ADER: TBytes): Boolean;
begin
  if Length(ADER) > 0 then
    Result := LoadFromMemory(@ADER[0], Length(ADER))
  else
    Result := False;
end;

function TOpenSSLCertificate.SaveToFile(const AFileName: string): Boolean;
var
  BIO: PBIO;
  FileNameA: AnsiString;
begin
  Result := False;
  if FX509 = nil then Exit;
  
  FileNameA := AnsiString(AFileName);
  BIO := BIO_new_file(PAnsiChar(FileNameA), 'w');
  if BIO = nil then Exit;
  
  try
    Result := (PEM_write_bio_X509(BIO, FX509) = 1);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.SaveToStream(AStream: TStream): Boolean;
var
  Data: TBytes;
begin
  Data := SaveToDER;
  if Length(Data) > 0 then
  begin
    AStream.Write(Data[0], Length(Data));
    Result := True;
  end
  else
    Result := False;
end;

function TOpenSSLCertificate.SaveToPEM: string;
var
  BIO: PBIO;
  Len: Integer;
  Buf: PAnsiChar;
begin
  Result := '';
  if FX509 = nil then Exit;
  
  BIO := BIO_new(BIO_s_mem);
  try
    if PEM_write_bio_X509(BIO, FX509) = 1 then
    begin
      Len := BIO_get_mem_data(BIO, PPAnsiChar(@Buf));
      if Len > 0 then
        SetString(Result, Buf, Len);
    end;
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.SaveToDER: TBytes;
var
  BIO: PBIO;
  Len: Integer;
  Buf: PAnsiChar;
begin
  SetLength(Result, 0);
  if FX509 = nil then Exit;
  
  BIO := BIO_new(BIO_s_mem);
  try
    if i2d_X509_bio(BIO, FX509) > 0 then
    begin
      Len := BIO_get_mem_data(BIO, PPAnsiChar(@Buf));
      if Len > 0 then
      begin
        SetLength(Result, Len);
        Move(Buf^, Result[0], Len);
      end;
    end;
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.GetInfo: TSSLCertificateInfo;
var
  SANs: TSSLStringArray;
  KUList: TSSLStringArray;
  I: Integer;
  Item: string;
  PathLen: LongInt;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.FingerprintSHA1 := GetFingerprintSHA1;
  Result.FingerprintSHA256 := GetFingerprintSHA256;
  Result.PublicKeyAlgorithm := GetPublicKeyAlgorithm;
  Result.SignatureAlgorithm := GetSignatureAlgorithm;
  Result.Version := GetVersion;
  Result.IsCA := IsCA;

  // 路径长度约束（Basic Constraints）
  Result.PathLength := -1;
  Result.PathLenConstraint := -1;
  if Assigned(X509_get_pathlen) and (FX509 <> nil) then
  begin
    PathLen := X509_get_pathlen(FX509);
    // PathLen >= 0 表示显式的 pathLenConstraint；-1 表示无此约束；其他负值视为未知/错误
    if PathLen >= 0 then
    begin
      Result.PathLength := PathLen;
      Result.PathLenConstraint := PathLen;
    end;
  end;

  // KeyUsage 位掩码：从字符串列表映射到 X509v3_KU_* 常量
  Result.KeyUsage := 0;
  KUList := GetKeyUsage;
  for I := 0 to Length(KUList) - 1 do
  begin
    Item := Trim(KUList[I]);
    if Item = '' then
      Continue;

    if SameText(Item, 'digitalSignature') or SameText(Item, 'Digital Signature') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_DIGITAL_SIGNATURE
    else if SameText(Item, 'nonRepudiation') or SameText(Item, 'Non Repudiation') or
            SameText(Item, 'contentCommitment') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_NON_REPUDIATION
    else if SameText(Item, 'keyEncipherment') or SameText(Item, 'Key Encipherment') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_KEY_ENCIPHERMENT
    else if SameText(Item, 'dataEncipherment') or SameText(Item, 'Data Encipherment') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_DATA_ENCIPHERMENT
    else if SameText(Item, 'keyAgreement') or SameText(Item, 'Key Agreement') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_KEY_AGREEMENT
    else if SameText(Item, 'keyCertSign') or SameText(Item, 'Key Cert Sign') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_KEY_CERT_SIGN
    else if SameText(Item, 'cRLSign') or SameText(Item, 'CRL Sign') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_CRL_SIGN
    else if SameText(Item, 'encipherOnly') or SameText(Item, 'Encipher Only') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_ENCIPHER_ONLY
    else if SameText(Item, 'decipherOnly') or SameText(Item, 'Decipher Only') then
      Result.KeyUsage := Result.KeyUsage or X509v3_KU_DECIPHER_ONLY;
  end;

  SANs := GetSubjectAltNames;
  Result.SubjectAltNames := SANs;
end;

function TOpenSSLCertificate.GetSubject: string;
var
  Name: PX509_NAME;
begin
  Result := '';
  if FX509 = nil then Exit;

  // 检查基本API是否加载
  if not Assigned(X509_get_subject_name) then Exit;

  try
    Name := X509_get_subject_name(FX509);
    Result := X509NameToString(Name);
  except
    Result := '';
  end;
end;

function TOpenSSLCertificate.GetIssuer: string;
var
  Name: PX509_NAME;
begin
  Result := '';
  if FX509 = nil then Exit;

  // 检查基本API是否加载
  if not Assigned(X509_get_issuer_name) then Exit;

  try
    Name := X509_get_issuer_name(FX509);
    Result := X509NameToString(Name);
  except
    Result := '';
  end;
end;

function TOpenSSLCertificate.GetSerialNumber: string;
var
  SerialNum: PASN1_INTEGER;
  BN: PBIGNUM;
  HexStr: PAnsiChar;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  // 检查必要的API是否已加载
  if not Assigned(X509_get_serialNumber) or 
    not Assigned(ASN1_INTEGER_to_BN) or 
    not Assigned(BN_bn2hex) then
    Exit;
  
  // 获取序列号
  SerialNum := X509_get_serialNumber(FX509);
  if SerialNum = nil then
    Exit;
  
  // 转换为BIGNUM
  BN := ASN1_INTEGER_to_BN(SerialNum, nil);
  if BN = nil then
    Exit;
  
  try
    // 转换为16进制字符串
    HexStr := BN_bn2hex(BN);
    if HexStr <> nil then
    begin
      Result := string(HexStr);
      // 释放OpenSSL分配的字符串
      if Assigned(OPENSSL_free) then
        OPENSSL_free(HexStr);
    end;
  finally
    // 释放BIGNUM
    if Assigned(BN_free) then
      BN_free(BN);
  end;
end;

function TOpenSSLCertificate.GetNotBefore: TDateTime;
var
  ASN1Time: PASN1_TIME;
  Year, Month, Day, Hour, Min, Sec: Word;
  TimeStr: AnsiString;
begin
  Result := 0;
  
  if FX509 = nil then
    Exit;
  
  ASN1Time := X509_get_notBefore(FX509);
  if ASN1Time = nil then
    Exit;
  
  Result := ASN1TimeToDateTime(ASN1Time);
end;

function TOpenSSLCertificate.GetNotAfter: TDateTime;
var
  ASN1Time: PASN1_TIME;
begin
  Result := 0;
  
  if FX509 = nil then
    Exit;
  
  ASN1Time := X509_get_notAfter(FX509);
  if ASN1Time = nil then
    Exit;
  
  Result := ASN1TimeToDateTime(ASN1Time);
end;

function TOpenSSLCertificate.GetPublicKey: string;
var
  PKey: PEVP_PKEY;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  PKey := X509_get_pubkey(FX509);
  if PKey <> nil then
  begin
    Result := GetPublicKeyAlgorithm; // 简化实现，返回算法名
    EVP_PKEY_free(PKey);
  end;
end;

function TOpenSSLCertificate.GetPublicKeyAlgorithm: string;
var
  PKey: PEVP_PKEY;
  KeyType: Integer;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  PKey := X509_get_pubkey(FX509);
  if PKey = nil then
    Exit;
  
  try
    KeyType := EVP_PKEY_id(PKey);
    case KeyType of
      EVP_PKEY_RSA: Result := 'RSA';
      EVP_PKEY_DSA: Result := 'DSA';
      EVP_PKEY_DH: Result := 'DH';
      EVP_PKEY_EC: Result := 'EC';
    else
      Result := 'Unknown';
    end;
  finally
    EVP_PKEY_free(PKey);
  end;
end;

function TOpenSSLCertificate.GetSignatureAlgorithm: string;
var
  NID: Integer;
  AlgName: PAnsiChar;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  // 检查必要的API是否已加载
  if not Assigned(X509_get_signature_nid) or not Assigned(OBJ_nid2sn) then
  begin
    Result := 'SHA256withRSA'; // 降级到默认值
    Exit;
  end;
  
  // 获取签名算法的NID
  NID := X509_get_signature_nid(FX509);
  if NID <= 0 then
  begin
    Result := 'Unknown';
    Exit;
  end;
  
  // 将NID转换为短名称
  AlgName := OBJ_nid2sn(NID);
  if AlgName <> nil then
    Result := string(AlgName)
  else
    Result := Format('NID:%d', [NID]);
end;

function TOpenSSLCertificate.GetVersion: Integer;
begin
  if FX509 <> nil then
    Result := X509_get_version(FX509) + 1
  else
    Result := 0;
end;

function TOpenSSLCertificate.Verify(ACAStore: ISSLCertificateStore): Boolean;
var
  Store: PX509_STORE;
  Ctx: PX509_STORE_CTX;
begin
  Result := False;
  
  if (FX509 = nil) or (ACAStore = nil) then
    Exit;

  if (not Assigned(X509_STORE_CTX_new)) or
    (not Assigned(X509_STORE_CTX_init)) or
    (not Assigned(X509_verify_cert)) or
    (not Assigned(X509_STORE_CTX_free)) then
  begin
    LoadOpenSSLX509;
    if (not Assigned(X509_STORE_CTX_new)) or
      (not Assigned(X509_STORE_CTX_init)) or
      (not Assigned(X509_verify_cert)) or
      (not Assigned(X509_STORE_CTX_free)) then
      Exit;
  end;
  
  Store := PX509_STORE(ACAStore.GetNativeHandle);
  if Store = nil then
    Exit;

  Ctx := nil;
  try
    try
      Ctx := X509_STORE_CTX_new;
    except
      Exit;
    end;
    if Ctx = nil then
      Exit;

    try
      if X509_STORE_CTX_init(Ctx, Store, FX509, nil) = 1 then
        Result := (X509_verify_cert(Ctx) = 1);
    except
      Result := False;
    end;
  finally
    if Ctx <> nil then
    begin
      try
        X509_STORE_CTX_free(Ctx);
      except
      end;
    end;
  end;
end;

function TOpenSSLCertificate.VerifyEx(ACAStore: ISSLCertificateStore; 
  AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
var
  Store: PX509_STORE;
  Ctx: PX509_STORE_CTX;
  Ret: Integer;
  ErrorCode: Integer;
  ErrorStr: PAnsiChar;
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.Success := False;
  Result := False;
  
  if FX509 = nil then
  begin
    AResult.ErrorMessage := 'Certificate is nil';
    Exit;
  end;
  
  if ACAStore = nil then
  begin
    AResult.ErrorMessage := 'CA store is nil';
    Exit;
  end;
  
  Store := PX509_STORE(ACAStore.GetNativeHandle);
  if Store = nil then
  begin
    AResult.ErrorMessage := 'Invalid CA store handle';
    Exit;
  end;

  if (not Assigned(X509_STORE_CTX_new)) or
    (not Assigned(X509_STORE_CTX_init)) or
    (not Assigned(X509_verify_cert)) or
    (not Assigned(X509_STORE_CTX_free)) or
    (not Assigned(X509_STORE_CTX_get_error)) or
    (not Assigned(X509_verify_cert_error_string)) then
  begin
    LoadOpenSSLX509;
    if (not Assigned(X509_STORE_CTX_new)) or
      (not Assigned(X509_STORE_CTX_init)) or
      (not Assigned(X509_verify_cert)) or
      (not Assigned(X509_STORE_CTX_free)) or
      (not Assigned(X509_STORE_CTX_get_error)) or
      (not Assigned(X509_verify_cert_error_string)) then
    begin
      AResult.ErrorMessage := 'OpenSSL X509 verification API not loaded';
      Exit;
    end;
  end;
  
  // 处理与时间、自签名相关的标志（与旧实现保持一致）
  if (sslCertVerifyIgnoreExpiry in AFlags) and Assigned(X509_STORE_set_flags) then
    X509_STORE_set_flags(Store, X509_V_FLAG_NO_CHECK_TIME);
  
  if (sslCertVerifyAllowSelfSigned in AFlags) and Assigned(X509_STORE_set_flags) then
    X509_STORE_set_flags(Store, X509_V_FLAG_PARTIAL_CHAIN);
  
  Ctx := nil;
  try
    try
      Ctx := X509_STORE_CTX_new;
    except
      AResult.ErrorMessage := 'Failed to create store context';
      Exit;
    end;
    if Ctx = nil then
    begin
      AResult.ErrorMessage := 'Failed to create store context';
      Exit;
    end;

    if X509_STORE_CTX_init(Ctx, Store, FX509, nil) = 1 then
    begin
      // CRL吊销检查已在下方实现（使用X509_V_FLAG_CRL_CHECK标志）
      
      // 如果需要检查吊销状态，则在验证参数上启用 CRL 检查
      if ((sslCertVerifyCheckRevocation in AFlags) or
          (sslCertVerifyCheckCRL in AFlags)) and
        Assigned(X509_STORE_CTX_get0_param) and
        Assigned(X509_VERIFY_PARAM_set_flags) then
      begin
        X509_VERIFY_PARAM_set_flags(
          X509_STORE_CTX_get0_param(Ctx),
          X509_V_FLAG_CRL_CHECK or X509_V_FLAG_CRL_CHECK_ALL
        );
      end;
      
      Ret := X509_verify_cert(Ctx);
      
      if Ret = 1 then
      begin
        AResult.Success := True;
        AResult.ErrorCode := 0;
        AResult.ErrorMessage := 'Certificate verification successful';
        AResult.DetailedInfo := 'OpenSSL verification passed';
        AResult.RevocationStatus := 0;
        Result := True;
      end
      else
      begin
        ErrorCode := X509_STORE_CTX_get_error(Ctx);
        ErrorStr := X509_verify_cert_error_string(ErrorCode);
        
        AResult.Success := False;
        AResult.ErrorCode := ErrorCode;
        if ErrorStr <> nil then
          AResult.ErrorMessage := string(ErrorStr)
        else
          AResult.ErrorMessage := 'Certificate verification failed';
        AResult.DetailedInfo := Format('OpenSSL error: %d - %s',
          [ErrorCode, AResult.ErrorMessage]);
        
        // 映射常见的吊销相关错误到 RevocationStatus
        if ErrorCode = X509_V_ERR_CERT_REVOKED then
          AResult.RevocationStatus := 1
        else if (ErrorCode = X509_V_ERR_UNABLE_TO_GET_CRL) or
                (ErrorCode = X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER) or
                (ErrorCode = X509_V_ERR_CRL_NOT_YET_VALID) or
                (ErrorCode = X509_V_ERR_CRL_HAS_EXPIRED) or
                (ErrorCode = X509_V_ERR_OCSP_VERIFY_NEEDED) or
                (ErrorCode = X509_V_ERR_OCSP_VERIFY_FAILED) or
                (ErrorCode = X509_V_ERR_OCSP_CERT_UNKNOWN) then
          AResult.RevocationStatus := 2
        else
          AResult.RevocationStatus := 0;
      end;
    end
    else
      AResult.ErrorMessage := 'Failed to initialize verification context';
  finally
    if Ctx <> nil then
    begin
      try
        X509_STORE_CTX_free(Ctx);
      except
      end;
    end;
  end;
end;

function TOpenSSLCertificate.VerifyHostname(const AHostname: string): Boolean;
var
  HostnameA: AnsiString;
begin
  Result := False;
  
  if (FX509 = nil) or (AHostname = '') then
    Exit;
  
  HostnameA := AnsiString(AHostname);
  Result := (X509_check_host(FX509, PAnsiChar(HostnameA), Length(HostnameA), 0, nil) = 1);
end;

function TOpenSSLCertificate.IsExpired: Boolean;
var
  CurrentTime: TDateTime;
begin
  Result := False;
  
  if FX509 = nil then
    Exit;
  
  CurrentTime := Now;
  Result := (CurrentTime < GetNotBefore) or (CurrentTime > GetNotAfter);
end;

function TOpenSSLCertificate.IsSelfSigned: Boolean;
var
  SubjectName, IssuerName: PX509_NAME;
begin
  Result := False;
  
  if FX509 = nil then
    Exit;
  
  // Use proper X509_NAME comparison instead of string comparison
  // String comparison is unreliable due to encoding differences and ordering
  if Assigned(X509_get_subject_name) and Assigned(X509_get_issuer_name) and 
    Assigned(X509_NAME_cmp) then
  begin
    SubjectName := X509_get_subject_name(FX509);
    IssuerName := X509_get_issuer_name(FX509);
    
    if (SubjectName <> nil) and (IssuerName <> nil) then
      Result := (X509_NAME_cmp(SubjectName, IssuerName) = 0);
  end
  else
  begin
    // Fallback to string comparison if APIs not available
    Result := (GetSubject = GetIssuer);
  end;
end;

function TOpenSSLCertificate.IsCA: Boolean;
var
  CAValue: Integer;
  Flags: UInt32;
const
  EXFLAG_CA = $10;  // CA 标志位
begin
  Result := False;
  
  if FX509 = nil then
    Exit;
  
  // 优先使用 X509_check_ca（OpenSSL 1.0.0+）
  if Assigned(X509_check_ca) then
  begin
    // Force extension caching, often needed for newly created certificates
    if Assigned(X509_check_purpose) then
      X509_check_purpose(FX509, -1, 0);
      
    CAValue := X509_check_ca(FX509);
    // Return: >= 1 means CA, 0 means not CA, -1 means error
    Result := (CAValue >= 1);
  end
  else if Assigned(X509_get_extension_flags) then
  begin
    // 备用方案：使用扩展标志（需要 OpenSSL 1.1.0+）
    Flags := X509_get_extension_flags(FX509);
    Result := (Flags and EXFLAG_CA) <> 0;
  end;
end;

function TOpenSSLCertificate.GetDaysUntilExpiry: Integer;
var
  ExpiryDate: TDateTime;
begin
  // 返回证书到期天数，已过期返回负数
  if FX509 = nil then
  begin
    Result := -MaxInt;  // 无效证书返回极小值
    Exit;
  end;

  ExpiryDate := GetNotAfter;
  if ExpiryDate = 0 then
  begin
    Result := -MaxInt;  // 无法获取到期日期
    Exit;
  end;

  Result := Trunc(ExpiryDate - Now);
end;

function TOpenSSLCertificate.GetSubjectCN: string;
var
  Subject: string;
  P, PEnd: Integer;
begin
  // 从 Subject DN 中提取 Common Name (CN)
  Result := '';

  if FX509 = nil then
    Exit;

  Subject := GetSubject;
  if Subject = '' then
    Exit;

  // 尝试解析 RFC 2253 格式: "CN=Example, O=Org, ..."
  // 或 OpenSSL oneline 格式: "/CN=Example/O=Org/..."

  // 格式1: "CN=" 开头或 ", CN=" 分隔
  P := Pos('CN=', Subject);
  if P = 0 then
    P := Pos('cn=', Subject);  // 小写兼容

  if P > 0 then
  begin
    // 跳过 "CN="
    Inc(P, 3);

    // 查找分隔符（逗号或斜杠）
    PEnd := P;
    while (PEnd <= Length(Subject)) do
    begin
      if Subject[PEnd] in [',', '/', '+'] then
        Break;
      Inc(PEnd);
    end;

    Result := Trim(Copy(Subject, P, PEnd - P));
    Exit;
  end;

  // 格式2: "/CN=" 格式
  P := Pos('/CN=', Subject);
  if P = 0 then
    P := Pos('/cn=', Subject);

  if P > 0 then
  begin
    Inc(P, 4);
    PEnd := P;
    while (PEnd <= Length(Subject)) and (Subject[PEnd] <> '/') do
      Inc(PEnd);

    Result := Trim(Copy(Subject, P, PEnd - P));
  end;
end;

function TOpenSSLCertificate.GetExtension(const AOID: string): string;
var
  LNID: Integer;
  LIndex: Integer;
  LExt: PX509_EXTENSION;
  LBIO: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  
  if (FX509 = nil) or (AOID = '') then
    Exit;
  
  // 将 OID 字符串转换为 NID
  LNID := OIDToNID(AOID);
  if (LNID = NID_undef) then
    Exit;
  
  // 检查必要的 API 是否已加载
  if (not Assigned(X509_get_ext_by_NID)) or
    (not Assigned(X509_get_ext)) or
    (not Assigned(X509V3_EXT_print)) or
    (not Assigned(BIO_new)) or
    (not Assigned(BIO_s_mem)) or
    (not Assigned(BIO_free)) then
    Exit;
  
  // 定位扩展
  LIndex := X509_get_ext_by_NID(FX509, LNID, -1);
  if LIndex < 0 then
    Exit;
  
  LExt := X509_get_ext(FX509, LIndex);
  if LExt = nil then
    Exit;
  
  // 使用 X509V3_EXT_print 将扩展内容转为可读文本
  LBIO := BIO_new(BIO_s_mem);
  if LBIO = nil then
    Exit;
  try
    if X509V3_EXT_print(LBIO, LExt, 0, 0) = 1 then
    begin
      LLen := BIO_get_mem_data(LBIO, PPAnsiChar(@LBuf));
      if LLen > 0 then
        SetString(Result, LBuf, LLen);
    end;
  finally
    BIO_free(LBIO);
  end;
end;

function TOpenSSLCertificate.GetSubjectAltNames: TSSLStringArray;
var
  ExtStr: string;
  Parts: TStringList;
  I: Integer;
  Item, Name: string;
  Names: PGENERAL_NAMES;
  Gen: PGENERAL_NAME;
  Count: Integer;
  LType: Integer;
  Val: Pointer;
  Data: PByte;
  Len: Integer;
  Crit, Idx: Integer;

  procedure AddToResult(const S: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := S;
  end;

begin
  SetLength(Result, 0);

  if FX509 = nil then
    Exit;

  if Assigned(X509_get_ext_d2i) and
    LoadStackFunctions and
    Assigned(OPENSSL_sk_num) and Assigned(OPENSSL_sk_value) and
    Assigned(GENERAL_NAME_get0_value) and Assigned(GENERAL_NAMES_free) and
    Assigned(ASN1_STRING_length) and
    (Assigned(ASN1_STRING_get0_data) or Assigned(ASN1_STRING_data)) then
  begin
    Crit := 0;
    Idx := -1;
    Names := PGENERAL_NAMES(X509_get_ext_d2i(FX509, NID_subject_alt_name, @Crit, @Idx));
    if Names <> nil then
    begin
      try
        Count := OPENSSL_sk_num(POPENSSL_STACK(Names));
        for I := 0 to Count - 1 do
        begin
          Gen := PGENERAL_NAME(OPENSSL_sk_value(POPENSSL_STACK(Names), I));
          if Gen = nil then
            Continue;

          Val := GENERAL_NAME_get0_value(Gen, @LType);
          if Val = nil then
            Continue;

          if LType = GEN_DNS then
          begin
            ExtStr := ASN1StringToString(ASN1_STRING(Val));
            if ExtStr <> '' then
              AddToResult(ExtStr);
          end
          else if LType = GEN_EMAIL then
          begin
            ExtStr := ASN1StringToString(ASN1_STRING(Val));
            if ExtStr <> '' then
              AddToResult(ExtStr);
          end
          else if LType = GEN_URI then
          begin
            ExtStr := ASN1StringToString(ASN1_STRING(Val));
            if ExtStr <> '' then
              AddToResult(ExtStr);
          end
          else if LType = GEN_IPADD then
          begin
            Len := ASN1_STRING_length(ASN1_STRING(Val));
            if Len <= 0 then
              Continue;
            if Assigned(ASN1_STRING_get0_data) then
              Data := ASN1_STRING_get0_data(ASN1_STRING(Val))
            else
              Data := ASN1_STRING_data(ASN1_STRING(Val));
            if Data = nil then
              Continue;
            Name := IpBytesToString(Data, Len);
            if Name <> '' then
              AddToResult(Name);
          end;
        end;
      finally
        GENERAL_NAMES_free(Names);
      end;
      Exit;
    end;
  end;

  // 简化实现：从扩展字符串中解析
  ExtStr := GetExtension('2.5.29.17'); // subjectAltName OID
  if ExtStr <> '' then
  begin
    // 将换行统一替换为逗号分隔，便于拆分
    ExtStr := StringReplace(ExtStr, LineEnding, ', ', [rfReplaceAll]);
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.StrictDelimiter := False;
      Parts.DelimitedText := ExtStr;
      for I := 0 to Parts.Count - 1 do
      begin
        Item := Trim(Parts[I]);
        if Item = '' then
          Continue;
        // 典型格式: "DNS:example.com"
        if (Length(Item) > 4) and SameText(Copy(Item, 1, 4), 'DNS:') then
        begin
          Name := Trim(Copy(Item, 5, MaxInt));
          if Name <> '' then
            AddToResult(Name);
        end;
        // 邮箱地址，例如 "email:user@example.com" 或 "Email:user@example.com"
        if (Length(Item) > 6) and
          (SameText(Copy(Item, 1, 6), 'email:') or SameText(Copy(Item, 1, 6), 'e-mail:')) then
        begin
          Name := Trim(Copy(Item, 7, MaxInt));
          if Name <> '' then
            AddToResult(Name);
        end;
        // URI 条目，例如 "URI:https://example.test"
        if (Length(Item) > 4) and SameText(Copy(Item, 1, 4), 'URI:') then
        begin
          Name := Trim(Copy(Item, 5, MaxInt));
          if Name <> '' then
            AddToResult(Name);
        end;
        // 额外支持 IP 地址条目，例如 "IP Address:192.168.0.1"
        if (Length(Item) > 11) and SameText(Copy(Item, 1, 11), 'IP Address:') then
        begin
          Name := Trim(Copy(Item, 12, MaxInt));
          if Name <> '' then
            AddToResult(Name);
        end;
      end;
    finally
      Parts.Free;
    end;
  end;
end;

function TOpenSSLCertificate.GetKeyUsage: TSSLStringArray;
var
  ExtStr: string;
  Parts: TStringList;
  I: Integer;
  Item: string;
  KUFlags: UInt32;

  procedure AddToResult(const S: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := S;
  end;

begin
  SetLength(Result, 0);

  if FX509 = nil then
    Exit;

  // 优先使用位标志接口（OpenSSL 1.1+ 提供）
  if Assigned(X509_get_key_usage) then
  begin
    KUFlags := X509_get_key_usage(FX509);
    if (KUFlags and X509v3_KU_DIGITAL_SIGNATURE) <> 0 then
      AddToResult('digitalSignature');
    if (KUFlags and X509v3_KU_NON_REPUDIATION) <> 0 then
      AddToResult('nonRepudiation');
    if (KUFlags and X509v3_KU_KEY_ENCIPHERMENT) <> 0 then
      AddToResult('keyEncipherment');
    if (KUFlags and X509v3_KU_DATA_ENCIPHERMENT) <> 0 then
      AddToResult('dataEncipherment');
    if (KUFlags and X509v3_KU_KEY_AGREEMENT) <> 0 then
      AddToResult('keyAgreement');
    if (KUFlags and X509v3_KU_KEY_CERT_SIGN) <> 0 then
      AddToResult('keyCertSign');
    if (KUFlags and X509v3_KU_CRL_SIGN) <> 0 then
      AddToResult('cRLSign');
    if (KUFlags and X509v3_KU_ENCIPHER_ONLY) <> 0 then
      AddToResult('encipherOnly');
    if (KUFlags and X509v3_KU_DECIPHER_ONLY) <> 0 then
      AddToResult('decipherOnly');
    Exit;
  end;

  // 回退：从扩展文本解析
  ExtStr := GetExtension('2.5.29.15'); // keyUsage OID
  if ExtStr = '' then
    Exit;

  ExtStr := StringReplace(ExtStr, LineEnding, ', ', [rfReplaceAll]);
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := False;
    Parts.DelimitedText := ExtStr;
    for I := 0 to Parts.Count - 1 do
    begin
      Item := Trim(Parts[I]);
      if Item <> '' then
        AddToResult(Item);
    end;
  finally
    Parts.Free;
  end;
end;

function TOpenSSLCertificate.GetExtendedKeyUsage: TSSLStringArray;
var
  ExtStr: string;
  Parts: TStringList;
  I: Integer;
  Item: string;
  EKUFlags: UInt32;

  procedure AddToResult(const S: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := S;
  end;

begin
  SetLength(Result, 0);

  if FX509 = nil then
    Exit;

  // 优先使用位标志接口（XKU_*），然后再回退到文本解析
  if Assigned(X509_get_extended_key_usage) then
  begin
    EKUFlags := X509_get_extended_key_usage(FX509);
    if (EKUFlags and XKU_SSL_SERVER) <> 0 then
      AddToResult('serverAuth');
    if (EKUFlags and XKU_SSL_CLIENT) <> 0 then
      AddToResult('clientAuth');
    if (EKUFlags and XKU_SMIME) <> 0 then
      AddToResult('emailProtection');
    if (EKUFlags and XKU_CODE_SIGN) <> 0 then
      AddToResult('codeSigning');
    if (EKUFlags and XKU_OCSP_SIGN) <> 0 then
      AddToResult('OCSPSigning');
    if (EKUFlags and XKU_TIMESTAMP) <> 0 then
      AddToResult('timeStamping');
    if (EKUFlags and XKU_ANYEKU) <> 0 then
      AddToResult('anyExtendedKeyUsage');
    Exit;
  end;

  // 回退：返回从扩展中读取的原始文本
  ExtStr := GetExtension('2.5.29.37'); // extKeyUsage OID
  if ExtStr = '' then
    Exit;

  ExtStr := StringReplace(ExtStr, LineEnding, ', ', [rfReplaceAll]);
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := False;
    Parts.DelimitedText := ExtStr;
    for I := 0 to Parts.Count - 1 do
    begin
      Item := Trim(Parts[I]);
      if Item <> '' then
        AddToResult(Item);
    end;
  finally
    Parts.Free;
  end;
end;

function TOpenSSLCertificate.GetFingerprint(AHashType: TSSLHash): string;
begin
  case AHashType of
    sslHashSHA1:   Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  else
    Result := '';
  end;
end;

// P3-10/15: Extracted common fingerprint computation with optimized string building
function TOpenSSLCertificate.ComputeFingerprint(MD: PEVP_MD): string;
var
  Digest: array[0..EVP_MAX_MD_SIZE-1] of Byte;
  DigestLen: Cardinal;
  I, Pos: Integer;
  DER, P: PByte;
  DERLen: Integer;
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';
begin
  Result := '';

  if FX509 = nil then
    Exit;

  // Get DER encoding length
  DERLen := i2d_X509(FX509, nil);
  if DERLen <= 0 then
    Exit;

  GetMem(DER, DERLen);
  try
    P := DER;
    i2d_X509(FX509, @P);

    // Compute digest
    DigestLen := 0;
    if EVP_Digest(DER, NativeUInt(DERLen), @Digest[0], DigestLen, MD, nil) = 1 then
    begin
      // P3-10: Pre-allocate string for better performance (XX:XX:XX format)
      SetLength(Result, DigestLen * 3 - 1);
      Pos := 1;
      for I := 0 to DigestLen - 1 do
      begin
        if I > 0 then
        begin
          Result[Pos] := ':';
          Inc(Pos);
        end;
        Result[Pos] := HexChars[Digest[I] shr 4];
        Result[Pos + 1] := HexChars[Digest[I] and $0F];
        Inc(Pos, 2);
      end;
    end;
  finally
    FreeMem(DER);
  end;
end;

function TOpenSSLCertificate.GetFingerprintSHA1: string;
begin
  Result := ComputeFingerprint(EVP_sha1());
end;

function TOpenSSLCertificate.GetFingerprintSHA256: string;
begin
  Result := ComputeFingerprint(EVP_sha256());
end;

procedure TOpenSSLCertificate.SetIssuerCertificate(ACert: ISSLCertificate);
begin
  FIssuerCert := ACert;
end;

function TOpenSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := FIssuerCert;
end;

function TOpenSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FX509;
end;

function TOpenSSLCertificate.Clone: ISSLCertificate;
begin
  Result := nil;
  if FX509 = nil then
    Exit;

  // Increment reference count first
  X509_up_ref(FX509);
  try
    // Create new certificate wrapper - if this fails, we must decrement ref
    Result := TOpenSSLCertificate.Create(FX509, True);
  except
    // Decrement reference count on failure to prevent leak
    if Assigned(X509_free) then
      X509_free(FX509);
    raise;
  end;
end;

end.
