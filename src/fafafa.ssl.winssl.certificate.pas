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
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.types,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

type
  { TWinSSLCertificate - Windows 证书类 }
  TWinSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FCertContext: PCCERT_CONTEXT;
    FOwnsContext: Boolean;
    
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
begin
  // TODO: 实现 PEM 解析
  Result := False;
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
begin
  // TODO: 实现 PEM 编码
  Result := '';
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
  Result.SubjectAltNames := GetSubjectAltNames;
  Result.IsCA := IsCA;
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
begin
  // TODO: 实现公钥提取
  Result := '';
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
begin
  // TODO: 实现完整验证
  Result := False;
end;

function TWinSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
begin
  // TODO: 实现主机名验证
  Result := False;
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
begin
  // TODO: 检查 Basic Constraints 扩展
  Result := False;
end;

// ============================================================================
// ISSLCertificate - 证书扩展
// ============================================================================

function TWinSSLCertificate.GetExtension(const aOID: string): string;
begin
  // TODO: 实现扩展查询
  Result := '';
end;

function TWinSSLCertificate.GetSubjectAltNames: TStringList;
var
  ExtInfo: PCERT_EXTENSION;
  i: DWORD;
  AltNameInfo: PCERT_ALT_NAME_INFO;
  AltNameInfoSize: DWORD;
  j: DWORD;
  AltName: PCERT_ALT_NAME_ENTRY;
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
        end;
      end;
    end;
  finally
    FreeMem(AltNameInfo);
  end;
end;

function TWinSSLCertificate.GetKeyUsage: TStringList;
begin
  Result := TStringList.Create;
  // TODO: 实现密钥用途解析
end;

function TWinSSLCertificate.GetExtendedKeyUsage: TStringList;
begin
  Result := TStringList.Create;
  // TODO: 实现扩展密钥用途解析
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
  // TODO: 实现颁发者证书设置
end;

function TWinSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  // TODO: 实现颁发者证书获取
  Result := nil;
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
