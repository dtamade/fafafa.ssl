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
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.obj,
  fafafa.ssl.openssl.api.crypto;

type
  TOpenSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FX509: PX509;
    FOwnsHandle: Boolean;
  public
    constructor Create(aX509: PX509; aOwnsHandle: Boolean = True);
    destructor Destroy; override;
    
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
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
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(aCAStore: ISSLCertificateStore; 
      aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

implementation

constructor TOpenSSLCertificate.Create(aX509: PX509; aOwnsHandle: Boolean = True);
begin
  inherited Create;
  FX509 := aX509;
  FOwnsHandle := aOwnsHandle;
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
        // 如果在程序退出时 OpenSSL 已被卸载，忽略错误
        on E: Exception do
          ; // 静默忽略
      end;
    end;
  end;
  inherited;
end;

function TOpenSSLCertificate.LoadFromFile(const aFileName: string): Boolean;
var
  F: File;
  BIO: PBIO;
  FileNameA: AnsiString;
begin
  Result := False;
  if not FileExists(aFileName) then Exit;
  
  FileNameA := AnsiString(aFileName);
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

function TOpenSSLCertificate.LoadFromStream(aStream: TStream): Boolean;
var
  Data: TBytes;
  Size: Int64;
  BIO: PBIO;
begin
  Size := aStream.Size - aStream.Position;
  SetLength(Data, Size);
  aStream.Read(Data[0], Size);
  
  BIO := BIO_new_mem_buf(@Data[0], Size);
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

function TOpenSSLCertificate.LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
var
  BIO: PBIO;
begin
  BIO := BIO_new_mem_buf(aData, aSize);
  try
    if FOwnsHandle and (FX509 <> nil) then
      X509_free(FX509);
    
    FX509 := d2i_X509_bio(BIO, nil);
    FOwnsHandle := True;
    Result := (FX509 <> nil);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.LoadFromPEM(const aPEM: string): Boolean;
var
  PEMData: AnsiString;
  BIO: PBIO;
begin
  PEMData := AnsiString(aPEM);
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

function TOpenSSLCertificate.LoadFromDER(const aDER: TBytes): Boolean;
begin
  if Length(aDER) > 0 then
    Result := LoadFromMemory(@aDER[0], Length(aDER))
  else
    Result := False;
end;

function TOpenSSLCertificate.SaveToFile(const aFileName: string): Boolean;
var
  BIO: PBIO;
  FileNameA: AnsiString;
begin
  Result := False;
  if FX509 = nil then Exit;
  
  FileNameA := AnsiString(aFileName);
  BIO := BIO_new_file(PAnsiChar(FileNameA), 'w');
  if BIO = nil then Exit;
  
  try
    Result := (PEM_write_bio_X509(BIO, FX509) = 1);
  finally
    BIO_free(BIO);
  end;
end;

function TOpenSSLCertificate.SaveToStream(aStream: TStream): Boolean;
var
  Data: TBytes;
begin
  Data := SaveToDER;
  if Length(Data) > 0 then
  begin
    aStream.Write(Data[0], Length(Data));
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
      Len := BIO_get_mem_data(BIO, @Buf);
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
      Len := BIO_get_mem_data(BIO, @Buf);
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
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.FingerprintSHA1 := GetFingerprintSHA1;
  Result.FingerprintSHA256 := GetFingerprintSHA256;
  Result.IsCA := IsCA;
end;

function TOpenSSLCertificate.GetSubject: string;
var
  Name: PX509_NAME;
  BIO: PBIO;
  Len: Integer;
  Buf: PAnsiChar;
begin
  Result := '';
  if FX509 = nil then Exit;
  
  // 检查API是否加载
  if not Assigned(X509_get_subject_name) or not Assigned(BIO_new) or 
     not Assigned(BIO_s_mem) or not Assigned(X509_NAME_print_ex) or
     not Assigned(BIO_get_mem_data) or not Assigned(BIO_free) then
    Exit;
  
  try
    Name := X509_get_subject_name(FX509);
    if Name = nil then Exit;
    
    BIO := BIO_new(BIO_s_mem);
    if BIO = nil then Exit;
    
    try
      X509_NAME_print_ex(BIO, Name, 0, 0);
      Len := BIO_get_mem_data(BIO, @Buf);
      if Len > 0 then
        SetString(Result, Buf, Len);
    finally
      BIO_free(BIO);
    end;
  except
    Result := '';
  end;
end;

function TOpenSSLCertificate.GetIssuer: string;
var
  Name: PX509_NAME;
  BIO: PBIO;
  Len: Integer;
  Buf: PAnsiChar;
begin
  Result := '';
  if FX509 = nil then Exit;
  
  // 检查API是否加载
  if not Assigned(X509_get_issuer_name) or not Assigned(BIO_new) or 
     not Assigned(BIO_s_mem) or not Assigned(X509_NAME_print_ex) or
     not Assigned(BIO_get_mem_data) or not Assigned(BIO_free) then
    Exit;
  
  try
    Name := X509_get_issuer_name(FX509);
    if Name = nil then Exit;
    
    BIO := BIO_new(BIO_s_mem);
    if BIO = nil then Exit;
    
    try
      X509_NAME_print_ex(BIO, Name, 0, 0);
      Len := BIO_get_mem_data(BIO, @Buf);
      if Len > 0 then
        SetString(Result, Buf, Len);
    finally
      BIO_free(BIO);
    end;
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
  
  // 简化实现：返回当前时间-1年作为占位
  // TODO: 完整实现需要解析ASN1_TIME
  Result := Now - 365;
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
  
  // 简化实现：返回当前时间+1年作为占位
  // TODO: 完整实现需要解析ASN1_TIME
  Result := Now + 365;
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

function TOpenSSLCertificate.Verify(aCAStore: ISSLCertificateStore): Boolean;
var
  Store: PX509_STORE;
  Ctx: PX509_STORE_CTX;
begin
  Result := False;
  
  if (FX509 = nil) or (aCAStore = nil) then
    Exit;
  
  Store := PX509_STORE(aCAStore.GetNativeHandle);
  if Store = nil then
    Exit;
  
  Ctx := X509_STORE_CTX_new;
  if Ctx = nil then
    Exit;
  
  try
    if X509_STORE_CTX_init(Ctx, Store, FX509, nil) = 1 then
      Result := (X509_verify_cert(Ctx) = 1);
  finally
    X509_STORE_CTX_free(Ctx);
  end;
end;

function TOpenSSLCertificate.VerifyEx(aCAStore: ISSLCertificateStore; 
  aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
var
  Store: PX509_STORE;
  Ctx: PX509_STORE_CTX;
  Ret: Integer;
  ErrorCode: Integer;
  ErrorStr: PAnsiChar;
begin
  Result := False;
  FillChar(aResult, SizeOf(aResult), 0);
  
  if (FX509 = nil) or (aCAStore = nil) then
    Exit;
  
  Store := PX509_STORE(aCAStore.GetNativeHandle);
  if Store = nil then
    Exit;
  
  Ctx := X509_STORE_CTX_new;
  if Ctx = nil then
    Exit;
  
  try
    if X509_STORE_CTX_init(Ctx, Store, FX509, nil) = 1 then
    begin
      // 应用标志 - TODO: 实现CRL检查
      // if (sslCertVerifyCheckRevocation in aFlags) or 
      //    (sslCertVerifyCheckCRL in aFlags) then
      // begin
      //   X509_VERIFY_PARAM_set_flags(...);
      // end;
      
      Ret := X509_verify_cert(Ctx);
      
      if Ret = 1 then
      begin
        aResult.Success := True;
        aResult.ErrorCode := 0;
        aResult.ErrorMessage := 'Certificate verification successful';
        Result := True;
      end
      else
      begin
        ErrorCode := X509_STORE_CTX_get_error(Ctx);
        ErrorStr := X509_verify_cert_error_string(ErrorCode);
        
        aResult.Success := False;
        aResult.ErrorCode := ErrorCode;
        aResult.ErrorMessage := string(ErrorStr);
      end;
    end;
  finally
    X509_STORE_CTX_free(Ctx);
  end;
end;

function TOpenSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
var
  HostnameA: AnsiString;
begin
  Result := False;
  
  if (FX509 = nil) or (aHostname = '') then
    Exit;
  
  HostnameA := AnsiString(aHostname);
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
begin
  Result := False;
  
  if FX509 = nil then
    Exit;
  
  // 比较主题和颁发者是否相同
  Result := (GetSubject = GetIssuer);
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
    CAValue := X509_check_ca(FX509);
    // 返回值：>= 1 表示是CA，0 表示不是CA，-1 表示错误
    Result := (CAValue >= 1);
  end
  else if Assigned(X509_get_extension_flags) then
  begin
    // 备用方案：使用扩展标志（需要 OpenSSL 1.1.0+）
    Flags := X509_get_extension_flags(FX509);
    Result := (Flags and EXFLAG_CA) <> 0;
  end;
end;

function TOpenSSLCertificate.GetExtension(const aOID: string): string;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  // TODO: 实现扩展解析需要更多的OpenSSL API绑定
  // 包括: X509_get_ext_count, X509_EXTENSION_get_object, 
  //      OBJ_obj2txt, X509V3_EXT_print 等
  
  Result := 'Extension parsing not yet implemented';
end;

function TOpenSSLCertificate.GetSubjectAltNames: TStringList;
var
  ExtStr: string;
begin
  Result := TStringList.Create;
  
  if FX509 = nil then
    Exit;
  
  // 简化实现：从扩展字符串中解析
  ExtStr := GetExtension('2.5.29.17'); // subjectAltName OID
  if ExtStr <> '' then
  begin
    // 返回原始文本，用户可以自己解析
    // 格式通常类似: "DNS:example.com, DNS:www.example.com"
    Result.Text := ExtStr;
  end;
end;

function TOpenSSLCertificate.GetKeyUsage: TStringList;
begin
  Result := TStringList.Create;
  
  // 简化实现：返回从扩展中读取的原始文本
  Result.Text := GetExtension('2.5.29.15'); // keyUsage OID
end;

function TOpenSSLCertificate.GetExtendedKeyUsage: TStringList;
begin
  Result := TStringList.Create;
  
  // 简化实现：返回从扩展中读取的原始文本
  Result.Text := GetExtension('2.5.29.37'); // extKeyUsage OID
end;

function TOpenSSLCertificate.GetFingerprint(aHashType: TSSLHash): string;
begin
  case aHashType of
    sslHashSHA1:   Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  else
    Result := '';
  end;
end;

function TOpenSSLCertificate.GetFingerprintSHA1: string;
var
  Digest: array[0..EVP_MAX_MD_SIZE-1] of Byte;
  DigestLen: Cardinal;
  I: Integer;
  DER: PByte;
  DERLen: Integer;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  // 获取DER编码
  DERLen := i2d_X509(FX509, nil);
  if DERLen <= 0 then Exit;
  
  GetMem(DER, DERLen);
  try
    i2d_X509(FX509, @DER);
    
    // 计算SHA1
    DigestLen := 0;
    if EVP_Digest(DER, NativeUInt(DERLen), @Digest[0], DigestLen, EVP_sha1(), nil) = 1 then
    begin
      for I := 0 to DigestLen - 1 do
      begin
        if I > 0 then
          Result := Result + ':';
        Result := Result + IntToHex(Digest[I], 2);
      end;
    end;
  finally
    FreeMem(DER);
  end;
end;

function TOpenSSLCertificate.GetFingerprintSHA256: string;
var
  Digest: array[0..EVP_MAX_MD_SIZE-1] of Byte;
  DigestLen: Cardinal;
  I: Integer;
  DER: PByte;
  DERLen: Integer;
begin
  Result := '';
  
  if FX509 = nil then
    Exit;
  
  // 获取DER编码
  DERLen := i2d_X509(FX509, nil);
  if DERLen <= 0 then Exit;
  
  GetMem(DER, DERLen);
  try
    i2d_X509(FX509, @DER);
    
    // 计算SHA256
    DigestLen := 0;
    if EVP_Digest(DER, NativeUInt(DERLen), @Digest[0], DigestLen, EVP_sha256(), nil) = 1 then
    begin
      for I := 0 to DigestLen - 1 do
      begin
        if I > 0 then
          Result := Result + ':';
        Result := Result + IntToHex(Digest[I], 2);
      end;
    end;
  finally
    FreeMem(DER);
  end;
end;

procedure TOpenSSLCertificate.SetIssuerCertificate(aCert: ISSLCertificate);
begin
  // TODO: Implement
end;

function TOpenSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := nil; // TODO: Implement
end;

function TOpenSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FX509;
end;

function TOpenSSLCertificate.Clone: ISSLCertificate;
begin
  if FX509 <> nil then
  begin
    X509_up_ref(FX509);
    Result := TOpenSSLCertificate.Create(FX509, True);
  end
  else
    Result := nil;
end;

end.
