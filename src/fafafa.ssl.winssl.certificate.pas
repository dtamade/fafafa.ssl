{
  fafafa.ssl.winssl.certificate - WinSSL 证书实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-06
  
  描述:
    实现 ISSLCertificate 接口的 WinSSL 后端。
    封装 Windows 证书上下文 (PCCERT_CONTEXT) 操作。
}

unit fafafa.ssl.winssl.certificate;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.types,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.utils;

type
  { TWinSSLCertificate - Windows 证书类 }
  TWinSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FCertContext: PCCERT_CONTEXT;
    FOwnsContext: Boolean;
    FIssuerCert: ISSLCertificate;  // 颁发者证书引用

    function GetCertInfo: PCERT_INFO;
    function BinaryToHexString(const aData: PByte; aSize: DWORD): string;
    function CalculateFingerprint(aHashType: TSSLHash): string;
    
  public
    constructor Create(aCertContext: PCCERT_CONTEXT; aOwnsContext: Boolean = True);
    destructor Destroy; override;
    
    { ISSLCertificate - 加载和保存 }
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    
    { ISSLCertificate - 证书信息 }
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
    
    { ISSLCertificate - 证书验证 }
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(aCAStore: ISSLCertificateStore; 
      aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    
    { ISSLCertificate - 证书扩展 }
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    
    { ISSLCertificate - 指纹 }
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    { ISSLCertificate - 证书链 }
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    
    { ISSLCertificate - 原生句柄 }
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

{ 工厂函数 }
function CreateWinSSLCertificateFromContext(aCertContext: PCCERT_CONTEXT; aOwnsContext: Boolean = True): ISSLCertificate;

implementation

function StringsToArray(aStrings: TStrings): TSSLStringArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if (aStrings = nil) or (aStrings.Count = 0) then
    Exit;

  SetLength(Result, aStrings.Count);
  for I := 0 to aStrings.Count - 1 do
    Result[I] := Trim(aStrings[I]);
end;

// ============================================================================
// 工厂函数
// ============================================================================

function CreateWinSSLCertificateFromContext(aCertContext: PCCERT_CONTEXT; aOwnsContext: Boolean = True): ISSLCertificate;
begin
  Result := TWinSSLCertificate.Create(aCertContext, aOwnsContext);
end;

// ============================================================================
// TWinSSLCertificate - 构造和析构
// ============================================================================

constructor TWinSSLCertificate.Create(aCertContext: PCCERT_CONTEXT; aOwnsContext: Boolean = True);
begin
  inherited Create;
  FCertContext := aCertContext;
  FOwnsContext := aOwnsContext;
end;

destructor TWinSSLCertificate.Destroy;
begin
  if FOwnsContext and (FCertContext <> nil) then
    CertFreeCertificateContext(FCertContext);
  inherited Destroy;
end;

// ============================================================================
// 内部辅助方法
// ============================================================================

function TWinSSLCertificate.GetCertInfo: PCERT_INFO;
begin
  if FCertContext <> nil then
    Result := FCertContext^.pCertInfo
  else
    Result := nil;
end;

function TWinSSLCertificate.BinaryToHexString(const aData: PByte; aSize: DWORD): string;
var
  i: Integer;
  p: PByte;
begin
  Result := '';
  if (aData = nil) or (aSize = 0) then
    Exit;
  
  p := aData;
  for i := 0 to aSize - 1 do
  begin
    if i > 0 then
      Result := Result + ':';
    Result := Result + IntToHex(p^, 2);
    Inc(p);
  end;
end;

function TWinSSLCertificate.CalculateFingerprint(aHashType: TSSLHash): string;
var
  HashAlg: ALG_ID;
  Hash: array[0..63] of Byte;
  HashSize: DWORD;
begin
  Result := '';
  
  if FCertContext = nil then
    Exit;
  
  // 选择哈希算法
  case aHashType of
    sslHashSHA1: HashAlg := CALG_SHA1;
    sslHashSHA256: HashAlg := CALG_SHA_256;
    sslHashSHA384: HashAlg := CALG_SHA_384;
    sslHashSHA512: HashAlg := CALG_SHA_512;
    sslHashMD5: HashAlg := CALG_MD5;
  else
    Exit;
  end;
  
  HashSize := SizeOf(Hash);
  
  // 计算证书哈希
  if CryptHashCertificate(0, HashAlg, 0, 
                          FCertContext^.pbCertEncoded, 
                          FCertContext^.cbCertEncoded,
                          @Hash[0], @HashSize) then
  begin
    Result := BinaryToHexString(@Hash[0], HashSize);
  end;
end;

// ============================================================================
// ISSLCertificate - 加载和保存
// ============================================================================

function TWinSSLCertificate.LoadFromFile(const aFileName: string): Boolean;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(FileStream);
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TWinSSLCertificate.LoadFromStream(aStream: TStream): Boolean;
var
  Data: TBytes;
  Size: Int64;
begin
  Result := False;
  Size := aStream.Size - aStream.Position;
  if Size <= 0 then
    Exit;
  
  SetLength(Data, Size);
  aStream.Read(Data[0], Size);
  Result := LoadFromDER(Data);
end;

function TWinSSLCertificate.LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
var
  NewContext: PCCERT_CONTEXT;
begin
  Result := False;
  
  if (aData = nil) or (aSize <= 0) then
    Exit;
  
  NewContext := CertCreateCertificateContext(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    aData,
    aSize
  );
  
  if NewContext <> nil then
  begin
    if FOwnsContext and (FCertContext <> nil) then
      CertFreeCertificateContext(FCertContext);
    
    FCertContext := NewContext;
    FOwnsContext := True;
    Result := True;
  end;
end;

function TWinSSLCertificate.LoadFromPEM(const aPEM: string): Boolean;
var
  DERData: TBytes;
  DERSize: DWORD;
  PEMStr: AnsiString;
begin
  Result := False;

  if aPEM = '' then
    Exit;

  // Convert PEM string to AnsiString for CryptStringToBinaryA
  PEMStr := UTF8Encode(aPEM);

  // First call to get the size
  if not CryptStringToBinaryA(
    PAnsiChar(PEMStr),
    Length(PEMStr),
    CRYPT_STRING_BASE64HEADER,
    nil,
    @DERSize,
    nil,
    nil
  ) then
    Exit;

  // Allocate buffer and decode
  SetLength(DERData, DERSize);
  if CryptStringToBinaryA(
    PAnsiChar(PEMStr),
    Length(PEMStr),
    CRYPT_STRING_BASE64HEADER,
    @DERData[0],
    @DERSize,
    nil,
    nil
  ) then
  begin
    SetLength(DERData, DERSize);
    Result := LoadFromDER(DERData);
  end;
end;

function TWinSSLCertificate.LoadFromDER(const aDER: TBytes): Boolean;
begin
  if Length(aDER) > 0 then
    Result := LoadFromMemory(@aDER[0], Length(aDER))
  else
    Result := False;
end;

function TWinSSLCertificate.SaveToFile(const aFileName: string): Boolean;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(aFileName, fmCreate);
    try
      Result := SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TWinSSLCertificate.SaveToStream(aStream: TStream): Boolean;
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

function TWinSSLCertificate.SaveToPEM: string;
var
  DERData: TBytes;
  PEMData: PAnsiChar;
  PEMSize: DWORD;
  PEMStr: AnsiString;
begin
  Result := '';

  if FCertContext = nil then
    Exit;

  DERData := SaveToDER;
  if Length(DERData) = 0 then
    Exit;

  // First call to get the size
  if not CryptBinaryToStringA(
    @DERData[0],
    Length(DERData),
    CRYPT_STRING_BASE64HEADER,
    nil,
    @PEMSize
  ) then
    Exit;

  // Allocate buffer and encode
  GetMem(PEMData, PEMSize);
  try
    if CryptBinaryToStringA(
      @DERData[0],
      Length(DERData),
      CRYPT_STRING_BASE64HEADER,
      PEMData,
      @PEMSize
    ) then
    begin
      SetString(PEMStr, PEMData, PEMSize - 1); // -1 to exclude null terminator
      Result := UTF8Decode(PEMStr);
    end;
  finally
    FreeMem(PEMData);
  end;
end;

function TWinSSLCertificate.SaveToDER: TBytes;
begin
  if (FCertContext <> nil) and (FCertContext^.cbCertEncoded > 0) then
  begin
    SetLength(Result, FCertContext^.cbCertEncoded);
    Move(FCertContext^.pbCertEncoded^, Result[0], FCertContext^.cbCertEncoded);
  end
  else
    SetLength(Result, 0);
end;

// ============================================================================
// ISSLCertificate - 证书信息
// ============================================================================

function TWinSSLCertificate.GetInfo: TSSLCertificateInfo;
var
  SANs: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if FCertContext = nil then
    Exit;
  
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

  SANs := GetSubjectAltNames;
  try
    Result.SubjectAltNames := StringsToArray(SANs);
  finally
    SANs.Free;
  end;
end;

function TWinSSLCertificate.GetSubject: string;
var
  Buffer: array[0..511] of WideChar;
  Size: DWORD;
begin
  Result := '';
  
  if FCertContext = nil then
    Exit;
  
  Size := CertGetNameStringW(
    FCertContext,
    CERT_NAME_SIMPLE_DISPLAY_TYPE,
    0,
    nil,
    @Buffer[0],
    Length(Buffer)
  );
  
  if Size > 1 then
    Result := WideCharToString(@Buffer[0]);
end;

function TWinSSLCertificate.GetIssuer: string;
var
  Buffer: array[0..511] of WideChar;
  Size: DWORD;
begin
  Result := '';
  
  if FCertContext = nil then
    Exit;
  
  Size := CertGetNameStringW(
    FCertContext,
    CERT_NAME_SIMPLE_DISPLAY_TYPE,
    CERT_NAME_ISSUER_FLAG,
    nil,
    @Buffer[0],
    Length(Buffer)
  );
  
  if Size > 1 then
    Result := WideCharToString(@Buffer[0]);
end;

function TWinSSLCertificate.GetSerialNumber: string;
var
  CertInfo: PCERT_INFO;
  i: Integer;
begin
  Result := '';
  
  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;
  
  // 序列号以小端序存储,需要反转
  for i := Integer(CertInfo^.SerialNumber.cbData) - 1 downto 0 do
  begin
    if i < Integer(CertInfo^.SerialNumber.cbData) - 1 then
      Result := Result + ':';
    Result := Result + IntToHex(PByte(CertInfo^.SerialNumber.pbData)[i], 2);
  end;
end;

function TWinSSLCertificate.GetNotBefore: TDateTime;
var
  CertInfo: PCERT_INFO;
  SysTime: TSystemTime;
begin
  Result := 0;
  
  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;
  
  if FileTimeToSystemTime(CertInfo^.NotBefore, SysTime) then
    Result := SystemTimeToDateTime(SysTime);
end;

function TWinSSLCertificate.GetNotAfter: TDateTime;
var
  CertInfo: PCERT_INFO;
  SysTime: TSystemTime;
begin
  Result := 0;
  
  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;
  
  if FileTimeToSystemTime(CertInfo^.NotAfter, SysTime) then
    Result := SystemTimeToDateTime(SysTime);
end;

function TWinSSLCertificate.GetPublicKey: string;
var
  CertInfo: PCERT_INFO;
  EncodedData: PByte;
  EncodedSize: DWORD;
  PEMData: PAnsiChar;
  PEMSize: DWORD;
  PEMStr: AnsiString;
begin
  Result := '';

  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;

  // 第一步：将 SubjectPublicKeyInfo 编码为 DER 格式
  // 先获取需要的缓冲区大小
  if not CryptEncodeObject(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    X509_PUBLIC_KEY_INFO,
    @CertInfo^.SubjectPublicKeyInfo,
    nil,
    @EncodedSize
  ) then
    Exit;

  // 分配缓冲区并编码
  GetMem(EncodedData, EncodedSize);
  try
    if not CryptEncodeObject(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      X509_PUBLIC_KEY_INFO,
      @CertInfo^.SubjectPublicKeyInfo,
      EncodedData,
      @EncodedSize
    ) then
      Exit;

    // 第二步：将 DER 格式转换为 PEM 格式
    // 先获取需要的缓冲区大小
    if not CryptBinaryToStringA(
      EncodedData,
      EncodedSize,
      CRYPT_STRING_BASE64HEADER,
      nil,
      @PEMSize
    ) then
      Exit;

    // 分配缓冲区并转换
    GetMem(PEMData, PEMSize);
    try
      if CryptBinaryToStringA(
        EncodedData,
        EncodedSize,
        CRYPT_STRING_BASE64HEADER,
        PEMData,
        @PEMSize
      ) then
      begin
        SetString(PEMStr, PEMData, PEMSize - 1); // -1 to exclude null terminator
        Result := UTF8Decode(PEMStr);
      end;
    finally
      FreeMem(PEMData);
    end;
  finally
    FreeMem(EncodedData);
  end;
end;

function TWinSSLCertificate.GetPublicKeyAlgorithm: string;
var
  CertInfo: PCERT_INFO;
begin
  Result := '';
  
  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;
  
  Result := string(CertInfo^.SubjectPublicKeyInfo.Algorithm.pszObjId);
end;

function TWinSSLCertificate.GetSignatureAlgorithm: string;
var
  CertInfo: PCERT_INFO;
begin
  Result := '';
  
  CertInfo := GetCertInfo;
  if CertInfo = nil then
    Exit;
  
  Result := string(CertInfo^.SignatureAlgorithm.pszObjId);
end;

function TWinSSLCertificate.GetVersion: Integer;
var
  CertInfo: PCERT_INFO;
begin
  Result := 0;
  
  CertInfo := GetCertInfo;
  if CertInfo <> nil then
    Result := CertInfo^.dwVersion + 1; // Windows 使用 0-based 版本号
end;

// ============================================================================
// ISSLCertificate - 证书验证
// ============================================================================

function TWinSSLCertificate.Verify(aCAStore: ISSLCertificateStore): Boolean;
var
  ChainPara: CERT_CHAIN_PARA;
  ChainContext: PCCERT_CHAIN_CONTEXT;
  PolicyPara: CERT_CHAIN_POLICY_PARA;
  PolicyStatus: CERT_CHAIN_POLICY_STATUS;
  StoreHandle: HCERTSTORE;
begin
  Result := False;

  if FCertContext = nil then
    Exit;

  // 初始化证书链参数
  FillChar(ChainPara, SizeOf(ChainPara), 0);
  ChainPara.cbSize := SizeOf(ChainPara);

  StoreHandle := nil;
  if aCAStore <> nil then
  begin
    // 如果提供了自定义 CA 存储，使用它
    StoreHandle := HCERTSTORE(aCAStore.GetNativeHandle);
  end;

  // 构建证书链
  if not CertGetCertificateChain(
    nil,                    // 使用默认链引擎
    FCertContext,           // 要验证的证书
    nil,                    // 使用当前时间
    StoreHandle,            // 附加证书存储（可选）
    @ChainPara,             // 链参数
    0,                      // 标志
    nil,                    // 保留
    @ChainContext           // 输出链上下文
  ) then
    Exit;

  try
    // 初始化策略参数
    FillChar(PolicyPara, SizeOf(PolicyPara), 0);
    PolicyPara.cbSize := SizeOf(PolicyPara);
    PolicyPara.dwFlags := 0;

    // 初始化策略状态
    FillChar(PolicyStatus, SizeOf(PolicyStatus), 0);
    PolicyStatus.cbSize := SizeOf(PolicyStatus);

    // 验证证书链策略（基本约束、密钥用途等）
    if CertVerifyCertificateChainPolicy(
      CERT_CHAIN_POLICY_BASE,     // 基本验证策略
      ChainContext,               // 证书链
      @PolicyPara,                // 策略参数
      @PolicyStatus               // 策略状态输出
    ) then
    begin
      // 检查验证结果
      Result := (PolicyStatus.dwError = 0);

      // 如果失败，可以通过 PolicyStatus.dwError 获取错误代码
      // 常见错误码：
      // TRUST_E_CERT_SIGNATURE: 签名无效
      // CERT_E_EXPIRED: 证书已过期
      // CERT_E_UNTRUSTEDROOT: 不受信任的根证书
      // CERT_E_WRONG_USAGE: 密钥用途不正确
    end;

  finally
    // 释放证书链上下文
    CertFreeCertificateChain(ChainContext);
  end;
end;

function TWinSSLCertificate.VerifyEx(aCAStore: ISSLCertificateStore; 
  aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
var
  LChainPara: CERT_CHAIN_PARA;
  LChainContext: PCCERT_CHAIN_CONTEXT;
  LPolicyPara: CERT_CHAIN_POLICY_PARA;
  LPolicyStatus: CERT_CHAIN_POLICY_STATUS;
  LStoreHandle: HCERTSTORE;
  LChainFlags: DWORD;
begin
  // 初始化返回值
  FillChar(aResult, SizeOf(aResult), 0);
  aResult.Success := False;
  Result := False;

  if FCertContext = nil then
  begin
    aResult.ErrorCode := ERROR_INVALID_PARAMETER;
    aResult.ErrorMessage := 'Certificate context is nil';
    Exit;
  end;

  // 初始化证书链参数
  FillChar(LChainPara, SizeOf(LChainPara), 0);
  LChainPara.cbSize := SizeOf(LChainPara);

  LStoreHandle := nil;
  if aCAStore <> nil then
    LStoreHandle := HCERTSTORE(aCAStore.GetNativeHandle);

  // 根据标志配置链验证
  LChainFlags := 0;
  
  if (sslCertVerifyCheckRevocation in aFlags) or
     (sslCertVerifyCheckOCSP in aFlags) then
    LChainFlags := LChainFlags or CERT_CHAIN_REVOCATION_CHECK_CHAIN;
    
  if sslCertVerifyCheckCRL in aFlags then
    LChainFlags := LChainFlags or CERT_CHAIN_REVOCATION_CHECK_END_CERT;

  // 构建证书链
  if not CertGetCertificateChain(
    nil,                    // 使用默认链引擎
    FCertContext,           // 要验证的证书
    nil,                    // 使用当前时间
    LStoreHandle,           // 附加证书存储（可选）
    @LChainPara,            // 链参数
    LChainFlags,            // 验证标志
    nil,                    // 保留
    @LChainContext          // 输出链上下文
  ) then
  begin
    aResult.ErrorCode := GetLastError;
    aResult.ErrorMessage := 'Failed to build certificate chain';
    Exit;
  end;

  try
    // 记录链状态
    if LChainContext <> nil then
      aResult.ChainStatus := LChainContext^.TrustStatus.dwErrorStatus;

    // 初始化策略参数
    FillChar(LPolicyPara, SizeOf(LPolicyPara), 0);
    LPolicyPara.cbSize := SizeOf(LPolicyPara);
    LPolicyPara.dwFlags := 0;

    // 初始化策略状态
    FillChar(LPolicyStatus, SizeOf(LPolicyStatus), 0);
    LPolicyStatus.cbSize := SizeOf(LPolicyStatus);

    // 验证证书链策略
    if CertVerifyCertificateChainPolicy(
      CERT_CHAIN_POLICY_BASE,     // 基本验证策略
      LChainContext,               // 证书链
      @LPolicyPara,                // 策略参数
      @LPolicyStatus               // 策略状态输出
    ) then
    begin
      aResult.ErrorCode := LPolicyStatus.dwError;
      aResult.ChainStatus := LPolicyStatus.dwError;
      
      // 检查验证结果
      if LPolicyStatus.dwError = 0 then
      begin
        aResult.Success := True;
        aResult.ErrorMessage := 'Certificate verified successfully';
        Result := True;
      end
      else
      begin
        // 生成友好的错误消息
        case LPolicyStatus.dwError of
          CERT_E_EXPIRED:
            aResult.ErrorMessage := 'Certificate has expired';
          CERT_E_UNTRUSTEDROOT:
            aResult.ErrorMessage := 'Certificate chain to untrusted root';
          CERT_E_WRONG_USAGE:
            aResult.ErrorMessage := 'Certificate has wrong usage';
          CERT_E_REVOKED:
            begin
              aResult.ErrorMessage := 'Certificate has been revoked';
              aResult.RevocationStatus := 1;
            end;
          CERT_E_REVOCATION_FAILURE:
            begin
              aResult.ErrorMessage := 'Revocation check failed';
              aResult.RevocationStatus := 2;
            end;
          TRUST_E_CERT_SIGNATURE:
            aResult.ErrorMessage := 'Certificate signature is invalid';
          CERT_E_CN_NO_MATCH:
            aResult.ErrorMessage := 'Certificate common name does not match';
          CERT_E_INVALID_NAME:
            aResult.ErrorMessage := 'Certificate name is invalid';
        else
          aResult.ErrorMessage := Format('Certificate verification failed (Error: 0x%x)', 
            [LPolicyStatus.dwError]);
        end;
      end;
    end
    else
    begin
      aResult.ErrorCode := GetLastError;
      aResult.ErrorMessage := 'Certificate chain policy verification failed';
    end;

  finally
    // 释放证书链上下文
    CertFreeCertificateChain(LChainContext);
  end;
end;

function TWinSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
var
  SANs: TStringList;
  i: Integer;
  CN, Entry: string;
  HostIsIP, EntryIsIP: Boolean;

  function MatchWildcard(const aPattern, aHostname: string): Boolean;
  var
    PatternParts, HostParts: TStringList;
    j: Integer;
  begin
    Result := False;

    // 精确匹配
    if SameText(aPattern, aHostname) then
    begin
      Result := True;
      Exit;
    end;

    // 通配符匹配 (*.example.com)
    if (Pos('*.', aPattern) = 1) then
    begin
      PatternParts := TStringList.Create;
      HostParts := TStringList.Create;
      try
        PatternParts.Delimiter := '.';
        PatternParts.DelimitedText := aPattern;

        HostParts.Delimiter := '.';
        HostParts.DelimitedText := aHostname;

        // 域名级数必须相同
        if PatternParts.Count = HostParts.Count then
        begin
          Result := True;
          // 从第二级开始比较（跳过通配符）
          for j := 1 to PatternParts.Count - 1 do
          begin
            if not SameText(PatternParts[j], HostParts[j]) then
            begin
              Result := False;
              Break;
            end;
          end;
        end;
      finally
        PatternParts.Free;
        HostParts.Free;
      end;
    end;
  end;

begin
  Result := False;

  if (FCertContext = nil) or (aHostname = '') then
    Exit;

  HostIsIP := TSSLUtils.IsIPAddress(aHostname);

  // 首先检查 Subject Alternative Names (SAN)
  SANs := GetSubjectAltNames;
  try
    for i := 0 to SANs.Count - 1 do
    begin
      Entry := Trim(SANs[i]);
      if Entry = '' then
        Continue;

      EntryIsIP := TSSLUtils.IsIPAddress(Entry);

      if HostIsIP then
      begin
        if EntryIsIP and SameText(Entry, aHostname) then
        begin
          Result := True;
          Exit;
        end;
        Continue;
      end;

      // 只针对域名进行通配符匹配，忽略 IP 及其他条目（email/URI 等）
      if EntryIsIP then
        Continue;
      if not TSSLUtils.IsValidHostname(Entry) then
        Continue;

      if MatchWildcard(Entry, aHostname) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    SANs.Free;
  end;

  // 如果没有 SAN 或未匹配，检查 Common Name (CN)
  CN := GetSubject;

  // 从 Subject 中提取 CN
  // 简化处理：GetSubject 可能已返回简化的名称
  // 如果返回的是完整 DN，需要解析
  if Pos('CN=', CN) > 0 then
  begin
    CN := Copy(CN, Pos('CN=', CN) + 3, Length(CN));
    if Pos(',', CN) > 0 then
      CN := Copy(CN, 1, Pos(',', CN) - 1);
    CN := Trim(CN);
  end;

  // 尝试匹配 CN
  if HostIsIP then
  begin
    Result := SameText(CN, aHostname);
    Exit;
  end;

  if not TSSLUtils.IsValidHostname(CN) then
    Exit;

  Result := MatchWildcard(CN, aHostname);
end;

function TWinSSLCertificate.IsExpired: Boolean;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now();
  Result := (GetNotBefore > CurrentTime) or (GetNotAfter < CurrentTime);
end;

function TWinSSLCertificate.IsSelfSigned: Boolean;
begin
  Result := GetSubject = GetIssuer;
end;

function TWinSSLCertificate.IsCA: Boolean;
var
  ExtInfo: PCERT_EXTENSION;
  BasicConstraints: PCERT_BASIC_CONSTRAINTS2_INFO;
  BasicConstraintsSize: DWORD;
begin
  Result := False;

  if FCertContext = nil then
    Exit;

  // 查找 Basic Constraints 扩展
  ExtInfo := CertFindExtension(
    szOID_BASIC_CONSTRAINTS2,
    GetCertInfo^.cExtension,
    GetCertInfo^.rgExtension
  );

  if ExtInfo = nil then
  begin
    // 尝试旧版本的 Basic Constraints (不常见)
    ExtInfo := CertFindExtension(
      szOID_BASIC_CONSTRAINTS,
      GetCertInfo^.cExtension,
      GetCertInfo^.rgExtension
    );
  end;

  if ExtInfo = nil then
    Exit;

  // 解码 Basic Constraints 扩展
  BasicConstraintsSize := 0;
  if not CryptDecodeObject(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    szOID_BASIC_CONSTRAINTS2,
    ExtInfo^.Value.pbData,
    ExtInfo^.Value.cbData,
    0,
    nil,
    @BasicConstraintsSize
  ) then
    Exit;

  GetMem(BasicConstraints, BasicConstraintsSize);
  try
    if CryptDecodeObject(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      szOID_BASIC_CONSTRAINTS2,
      ExtInfo^.Value.pbData,
      ExtInfo^.Value.cbData,
      0,
      BasicConstraints,
      @BasicConstraintsSize
    ) then
    begin
      Result := BasicConstraints^.fCA;
    end;
  finally
    FreeMem(BasicConstraints);
  end;
end;

// ============================================================================
// ISSLCertificate - 证书扩展
// ============================================================================

function TWinSSLCertificate.GetExtension(const aOID: string): string;
var
  ExtInfo: PCERT_EXTENSION;
  OIDAnsi: AnsiString;
begin
  Result := '';

  if (FCertContext = nil) or (aOID = '') then
    Exit;

  // 转换 OID 为 ANSI 字符串
  OIDAnsi := AnsiString(aOID);

  // 查找扩展
  ExtInfo := CertFindExtension(
    PAnsiChar(OIDAnsi),
    GetCertInfo^.cExtension,
    GetCertInfo^.rgExtension
  );

  if ExtInfo = nil then
    Exit;

  // 将扩展值转换为十六进制字符串
  Result := BinaryToHexString(ExtInfo^.Value.pbData, ExtInfo^.Value.cbData);
end;

function TWinSSLCertificate.GetSubjectAltNames: TStringList;
var
  ExtInfo: PCERT_EXTENSION;
  i: DWORD;
  AltNameInfo: PCERT_ALT_NAME_INFO;
  AltNameInfoSize: DWORD;
  j: DWORD;
  AltName: PCERT_ALT_NAME_ENTRY;
  Addr4, Addr6: PByte;
  SegValue: Word;
  IpStr: string;
  k: Integer;
begin
  Result := TStringList.Create;
  
  if FCertContext = nil then
    Exit;
  
  // 查找 Subject Alternative Name 扩展
  ExtInfo := CertFindExtension(
    szOID_SUBJECT_ALT_NAME2,
    GetCertInfo^.cExtension,
    GetCertInfo^.rgExtension
  );
  
  if ExtInfo = nil then
    Exit;
  
  // 解码扩展
  AltNameInfoSize := 0;
  if not CryptDecodeObject(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    szOID_SUBJECT_ALT_NAME2,
    ExtInfo^.Value.pbData,
    ExtInfo^.Value.cbData,
    0,
    nil,
    @AltNameInfoSize
  ) then
    Exit;

  GetMem(AltNameInfo, AltNameInfoSize);
  try
    if CryptDecodeObject(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      szOID_SUBJECT_ALT_NAME2,
      ExtInfo^.Value.pbData,
      ExtInfo^.Value.cbData,
      0,
      AltNameInfo,
      @AltNameInfoSize
    ) then
    begin
      // 提取每个备用名称
      for j := 0 to AltNameInfo^.cAltEntry - 1 do
      begin
        AltName := @AltNameInfo^.rgAltEntry[j];
        if AltName^.dwAltNameChoice = CERT_ALT_NAME_DNS_NAME then
        begin
          Result.Add(WideCharToString(AltName^.pwszDNSName));
        end
        else if AltName^.dwAltNameChoice = CERT_ALT_NAME_IP_ADDRESS then
        begin
          if AltName^.IPAddress.cbData = 4 then
          begin
            Addr4 := AltName^.IPAddress.pbData;
            if Addr4 <> nil then
              Result.Add(Format('%d.%d.%d.%d', [Addr4[0], Addr4[1], Addr4[2], Addr4[3]]));
          end
          else if AltName^.IPAddress.cbData = 16 then
          begin
            Addr6 := AltName^.IPAddress.pbData;
            if Addr6 <> nil then
            begin
              IpStr := '';
              for k := 0 to 7 do
              begin
                SegValue := (Word(Addr6[k * 2]) shl 8) or Word(Addr6[k * 2 + 1]);
                if k > 0 then
                  IpStr := IpStr + ':';
                IpStr := IpStr + IntToHex(SegValue, 1);
              end;
              if IpStr <> '' then
                Result.Add(IpStr);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(AltNameInfo);
  end;
end;

function TWinSSLCertificate.GetKeyUsage: TStringList;
var
  ExtInfo: PCERT_EXTENSION;
  KeyUsageInfo: PCRYPT_BIT_BLOB;
  KeyUsageSize: DWORD;
  KeyUsageBits: Byte;
begin
  Result := TStringList.Create;

  if FCertContext = nil then
    Exit;

  // 查找 Key Usage 扩展
  ExtInfo := CertFindExtension(
    szOID_KEY_USAGE,
    GetCertInfo^.cExtension,
    GetCertInfo^.rgExtension
  );

  if ExtInfo = nil then
    Exit;

  // 解码 Key Usage 扩展
  KeyUsageSize := 0;
  if not CryptDecodeObject(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    szOID_KEY_USAGE,
    ExtInfo^.Value.pbData,
    ExtInfo^.Value.cbData,
    0,
    nil,
    @KeyUsageSize
  ) then
    Exit;

  GetMem(KeyUsageInfo, KeyUsageSize);
  try
    if CryptDecodeObject(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      szOID_KEY_USAGE,
      ExtInfo^.Value.pbData,
      ExtInfo^.Value.cbData,
      0,
      KeyUsageInfo,
      @KeyUsageSize
    ) then
    begin
      if KeyUsageInfo^.cbData > 0 then
      begin
        KeyUsageBits := PByte(KeyUsageInfo^.pbData)^;

        // 解析密钥用途位
        if (KeyUsageBits and $80) <> 0 then Result.Add('digitalSignature');
        if (KeyUsageBits and $40) <> 0 then Result.Add('nonRepudiation');
        if (KeyUsageBits and $20) <> 0 then Result.Add('keyEncipherment');
        if (KeyUsageBits and $10) <> 0 then Result.Add('dataEncipherment');
        if (KeyUsageBits and $08) <> 0 then Result.Add('keyAgreement');
        if (KeyUsageBits and $04) <> 0 then Result.Add('keyCertSign');
        if (KeyUsageBits and $02) <> 0 then Result.Add('cRLSign');
        if (KeyUsageBits and $01) <> 0 then Result.Add('encipherOnly');

        // 第二个字节（如果存在）
        if KeyUsageInfo^.cbData > 1 then
        begin
          KeyUsageBits := PByte(KeyUsageInfo^.pbData + 1)^;
          if (KeyUsageBits and $80) <> 0 then Result.Add('decipherOnly');
        end;
      end;
    end;
  finally
    FreeMem(KeyUsageInfo);
  end;
end;

function TWinSSLCertificate.GetExtendedKeyUsage: TStringList;
var
  ExtInfo: PCERT_EXTENSION;
  EnhKeyUsage: PCERT_ENHKEY_USAGE;
  EnhKeyUsageSize: DWORD;
  i: DWORD;
  OIDStr: string;
begin
  Result := TStringList.Create;

  if FCertContext = nil then
    Exit;

  // 查找 Extended Key Usage 扩展
  ExtInfo := CertFindExtension(
    szOID_ENHANCED_KEY_USAGE,
    GetCertInfo^.cExtension,
    GetCertInfo^.rgExtension
  );

  if ExtInfo = nil then
    Exit;

  // 解码 Extended Key Usage 扩展
  EnhKeyUsageSize := 0;
  if not CryptDecodeObject(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    szOID_ENHANCED_KEY_USAGE,
    ExtInfo^.Value.pbData,
    ExtInfo^.Value.cbData,
    0,
    nil,
    @EnhKeyUsageSize
  ) then
    Exit;

  GetMem(EnhKeyUsage, EnhKeyUsageSize);
  try
    if CryptDecodeObject(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      szOID_ENHANCED_KEY_USAGE,
      ExtInfo^.Value.pbData,
      ExtInfo^.Value.cbData,
      0,
      EnhKeyUsage,
      @EnhKeyUsageSize
    ) then
    begin
      // 提取每个 OID
      for i := 0 to EnhKeyUsage^.cUsageIdentifier - 1 do
      begin
        OIDStr := string(EnhKeyUsage^.rgpszUsageIdentifier[i]);
        Result.Add(OIDStr);

        // 添加常见 OID 的友好名称
        if OIDStr = szOID_PKIX_KP_SERVER_AUTH then
          Result.Add('TLS Web Server Authentication')
        else if OIDStr = szOID_PKIX_KP_CLIENT_AUTH then
          Result.Add('TLS Web Client Authentication')
        else if OIDStr = szOID_PKIX_KP_CODE_SIGNING then
          Result.Add('Code Signing')
        else if OIDStr = szOID_PKIX_KP_EMAIL_PROTECTION then
          Result.Add('Email Protection');
      end;
    end;
  finally
    FreeMem(EnhKeyUsage);
  end;
end;

// ============================================================================
// ISSLCertificate - 指纹
// ============================================================================

function TWinSSLCertificate.GetFingerprint(aHashType: TSSLHash): string;
begin
  Result := CalculateFingerprint(aHashType);
end;

function TWinSSLCertificate.GetFingerprintSHA1: string;
begin
  Result := CalculateFingerprint(sslHashSHA1);
end;

function TWinSSLCertificate.GetFingerprintSHA256: string;
begin
  Result := CalculateFingerprint(sslHashSHA256);
end;

// ============================================================================
// ISSLCertificate - 证书链
// ============================================================================

procedure TWinSSLCertificate.SetIssuerCertificate(aCert: ISSLCertificate);
begin
  FIssuerCert := aCert;
end;

function TWinSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := FIssuerCert;
end;

// ============================================================================
// ISSLCertificate - 原生句柄
// ============================================================================

function TWinSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FCertContext;
end;

function TWinSSLCertificate.Clone: ISSLCertificate;
var
  DupContext: PCCERT_CONTEXT;
begin
  if FCertContext <> nil then
  begin
    DupContext := CertDuplicateCertificateContext(FCertContext);
    Result := TWinSSLCertificate.Create(DupContext, True);
  end
  else
    Result := nil;
end;

end.
