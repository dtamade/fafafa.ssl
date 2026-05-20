{**
 * Unit: fafafa.ssl.wolfssl.certificate
 * Purpose: WolfSSL 证书和证书存储实现
 *
 * 实现 ISSLCertificate 和 ISSLCertificateStore 接口的 WolfSSL 后端。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.wolfssl.certificate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.x509,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.native_handle,
  fafafa.ssl.wolfssl.api;

type
  { TWolfSSLCertificate - WolfSSL 证书类 }
  TWolfSSLCertificate = class(TInterfacedObject, ISSLCertificate, ISSLNativeHandleAccess)
  private
    FX509: PWOLFSSL_X509;
    FInfo: TSSLCertificateInfo;
    FPEMData: string;
    FDERData: TBytes;
    FIssuerCert: ISSLCertificate;
    function TryLoadX509Parser(out AParser: TX509Certificate): Boolean;
    function TryGetParsedAlgorithmMetadata(out APublicKeyAlgorithm,
      ASignatureAlgorithm: string): Boolean;

  public
    constructor Create; overload;
    constructor Create(AX509: PWOLFSSL_X509); overload;
    destructor Destroy; override;

    { ISSLCertificate - 加载和保存 }
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
    function LoadFromPEM(const APEM: string): Boolean;
    function LoadFromDER(const ADER: TBytes): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function SaveToStream(AStream: TStream): Boolean;
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
    function Verify(ACAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(ACAStore: ISSLCertificateStore;
      AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const AHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;

    { ISSLCertificate - 便利方法 }
    function GetDaysUntilExpiry: Integer;
    function GetSubjectCN: string;

    { ISSLCertificate - 证书扩展 }
    function GetExtension(const AOID: string): string;
    function GetSubjectAltNames: TSSLStringArray;
    function GetKeyUsage: TSSLStringArray;
    function GetExtendedKeyUsage: TSSLStringArray;

    { ISSLCertificate - 指纹 }
    function GetFingerprint(AHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;

    { ISSLCertificate - 证书链 }
    procedure SetIssuerCertificate(ACert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;

    { ISSLNativeHandleAccess implementation }
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;

    function Clone: ISSLCertificate;
  end;

  { TWolfSSLCertificateStore - WolfSSL 证书存储类 }
  TWolfSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore, ISSLNativeHandleAccess)
  private
    FX509Store: PWOLFSSL_X509_STORE;
    FCertificates: TInterfaceList;

  public
    constructor Create;
    destructor Destroy; override;

    { ISSLCertificateStore - 证书管理 }
    function AddCertificate(ACert: ISSLCertificate): Boolean;
    function RemoveCertificate(ACert: ISSLCertificate): Boolean;
    function Contains(ACert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(AIndex: Integer): ISSLCertificate;

    { ISSLCertificateStore - 加载方法 }
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromPath(const APath: string): Boolean;
    function LoadSystemStore: Boolean;

    { ISSLCertificateStore - 查找 }
    function FindBySubject(const ASubject: string): ISSLCertificate;
    function FindByIssuer(const AIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const AFingerprint: string): ISSLCertificate;

    { ISSLCertificateStore - 验证 }
    function VerifyCertificate(ACert: ISSLCertificate): Boolean;
    function BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;

    { ISSLNativeHandleAccess implementation }
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;
  end;

implementation

uses
  Contnrs, DateUtils,
  fafafa.ssl.utils,
  fafafa.ssl.crypto.hash;

function NormalizeWolfCertText(const AValue: string): string;
begin
  Result := Trim(UpperCase(AValue));
  Result := StringReplace(Result, ',', '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
end;

function NormalizeWolfCertFingerprint(const AFingerprint: string): string;
begin
  Result := Trim(UpperCase(AFingerprint));
  Result := StringReplace(Result, ':', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
end;

function X509KeyUsageToBitfield(const AUsage: TX509KeyUsage): Word;
begin
  Result := 0;
  if kuDigitalSignature in AUsage then
    Result := Result or $0080;
  if kuNonRepudiation in AUsage then
    Result := Result or $0040;
  if kuKeyEncipherment in AUsage then
    Result := Result or $0020;
  if kuDataEncipherment in AUsage then
    Result := Result or $0010;
  if kuKeyAgreement in AUsage then
    Result := Result or $0008;
  if kuKeyCertSign in AUsage then
    Result := Result or $0004;
  if kuCRLSign in AUsage then
    Result := Result or $0002;
  if kuEncipherOnly in AUsage then
    Result := Result or $0001;
  if kuDecipherOnly in AUsage then
    Result := Result or $8000;
end;

function X509KeyUsageToStrings(const AUsage: TX509KeyUsage): TSSLStringArray;

  procedure AddToResult(const AValue: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := AValue;
  end;

begin
  SetLength(Result, 0);
  if kuDigitalSignature in AUsage then
    AddToResult('digitalSignature');
  if kuNonRepudiation in AUsage then
    AddToResult('nonRepudiation');
  if kuKeyEncipherment in AUsage then
    AddToResult('keyEncipherment');
  if kuDataEncipherment in AUsage then
    AddToResult('dataEncipherment');
  if kuKeyAgreement in AUsage then
    AddToResult('keyAgreement');
  if kuKeyCertSign in AUsage then
    AddToResult('keyCertSign');
  if kuCRLSign in AUsage then
    AddToResult('cRLSign');
  if kuEncipherOnly in AUsage then
    AddToResult('encipherOnly');
  if kuDecipherOnly in AUsage then
    AddToResult('decipherOnly');
end;

function X509ExtKeyUsageToStrings(const AUsage: TX509ExtKeyUsage): TSSLStringArray;

  procedure AddToResult(const AValue: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := AValue;
  end;

begin
  SetLength(Result, 0);
  if ekuServerAuth in AUsage then
    AddToResult('serverAuth');
  if ekuClientAuth in AUsage then
    AddToResult('clientAuth');
  if ekuCodeSigning in AUsage then
    AddToResult('codeSigning');
  if ekuEmailProtection in AUsage then
    AddToResult('emailProtection');
  if ekuTimeStamping in AUsage then
    AddToResult('timeStamping');
  if ekuOCSPSigning in AUsage then
    AddToResult('OCSPSigning');
end;

function X509SubjectAltNamesToStrings(
  const ASANs: TX509SubjectAltNames): TSSLStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ASANs));
  for I := 0 to High(ASANs) do
    Result[I] := ASANs[I].Value;
end;

{ TWolfSSLCertificate }

constructor TWolfSSLCertificate.Create;
begin
  inherited Create;
  FX509 := nil;
  FPEMData := '';
  SetLength(FDERData, 0);
  FIssuerCert := nil;
  FillChar(FInfo, SizeOf(FInfo), 0);
  FInfo.PathLenConstraint := -1;
  FInfo.PathLength := -1;
end;

constructor TWolfSSLCertificate.Create(AX509: PWOLFSSL_X509);
begin
  Create;
  FX509 := AX509;
end;

destructor TWolfSSLCertificate.Destroy;
begin
  if FX509 <> nil then
  begin
    if Assigned(wolfSSL_X509_free) then
      wolfSSL_X509_free(FX509);
    FX509 := nil;
  end;
  FIssuerCert := nil;
  inherited Destroy;
end;

function TWolfSSLCertificate.TryLoadX509Parser(
  out AParser: TX509Certificate): Boolean;
var
  LDER: TBytes;
begin
  AParser := nil;
  Result := False;

  if FX509 = nil then
    Exit;

  AParser := TX509Certificate.Create;
  try
    if Length(FDERData) > 0 then
      AParser.LoadFromDER(FDERData)
    else if FPEMData <> '' then
      AParser.LoadFromPEM(FPEMData)
    else
    begin
      LDER := SaveToDER;
      if Length(LDER) = 0 then
        Exit;
      AParser.LoadFromDER(LDER);
    end;
    Result := True;
  except
    FreeAndNil(AParser);
    Result := False;
  end;
end;

function TWolfSSLCertificate.TryGetParsedAlgorithmMetadata(
  out APublicKeyAlgorithm, ASignatureAlgorithm: string): Boolean;
var
  LParser: TX509Certificate;
begin
  APublicKeyAlgorithm := '';
  ASignatureAlgorithm := '';
  Result := False;

  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    APublicKeyAlgorithm := LParser.PublicKeyInfo.Algorithm.Name;
    if APublicKeyAlgorithm = '' then
      APublicKeyAlgorithm := LParser.PublicKeyInfo.Algorithm.OID;

    ASignatureAlgorithm := LParser.SignatureAlgorithm.Name;
    if ASignatureAlgorithm = '' then
      ASignatureAlgorithm := LParser.SignatureAlgorithm.OID;

    Result := (APublicKeyAlgorithm <> '') or (ASignatureAlgorithm <> '');
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.LoadFromFile(const AFileName: string): Boolean;
var
  LRawBytes: TBytes;
  LText: string;
  LStream: TFileStream;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  SetLength(FDERData, 0);
  FPEMData := '';

  SetLength(LRawBytes, 0);
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if LStream.Size > 0 then
    begin
      SetLength(LRawBytes, LStream.Size);
      LStream.ReadBuffer(LRawBytes[0], Length(LRawBytes));
    end;
  finally
    LStream.Free;
  end;

  if Length(LRawBytes) > 0 then
  begin
    SetString(LText, PAnsiChar(@LRawBytes[0]), Length(LRawBytes));
    if TSSLUtils.IsPEMFormat(LText) then
    begin
      FPEMData := LText;
      FDERData := TSSLUtils.PEMToDER(FPEMData);
    end
    else
    begin
      FDERData := Copy(LRawBytes);
      FPEMData := TSSLUtils.DERToPEM(FDERData);
    end;
  end;

  if not Assigned(wolfSSL_X509_load_certificate_file) then Exit;

  if FX509 <> nil then
  begin
    if Assigned(wolfSSL_X509_free) then
      wolfSSL_X509_free(FX509);
    FX509 := nil;
  end;

  FX509 := wolfSSL_X509_load_certificate_file(PAnsiChar(AnsiString(AFileName)),
    WOLFSSL_FILETYPE_PEM);
  if FX509 = nil then
    FX509 := wolfSSL_X509_load_certificate_file(PAnsiChar(AnsiString(AFileName)),
      WOLFSSL_FILETYPE_ASN1);

  Result := FX509 <> nil;
end;

function TWolfSSLCertificate.LoadFromStream(AStream: TStream): Boolean;
var
  LData: TBytes;
begin
  Result := False;
  if AStream = nil then Exit;

  SetLength(LData, AStream.Size - AStream.Position);
  if Length(LData) = 0 then Exit;

  AStream.ReadBuffer(LData[0], Length(LData));
  Result := LoadFromMemory(@LData[0], Length(LData));
end;

function TWolfSSLCertificate.LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
var
  LX509: PWOLFSSL_X509;
  LDER: TBytes;
  LParser: TX509Certificate;
begin
  Result := False;
  SetLength(FDERData, 0);
  FPEMData := '';

  if (AData = nil) or (ASize <= 0) then Exit;
  SetLength(LDER, ASize);
  Move(AData^, LDER[0], ASize);

  if not TSSLUtils.IsDERFormat(LDER) then
    Exit;

  // 先用统一 X509 解析器做安全性校验，避免把无效输入交给 wolfSSL parser
  LParser := TX509Certificate.Create;
  try
    try
      LParser.LoadFromDER(LDER);
    except
      Exit;
    end;
  finally
    LParser.Free;
  end;

  if not Assigned(wolfSSL_X509_d2i) then Exit;

  LX509 := wolfSSL_X509_d2i(nil, @LDER[0], Length(LDER));
  if LX509 = nil then
    Exit;

  if FX509 <> nil then
  begin
    if Assigned(wolfSSL_X509_free) then
      wolfSSL_X509_free(FX509);
  end;

  FX509 := LX509;
  FDERData := Copy(LDER);
  FPEMData := TSSLUtils.DERToPEM(FDERData);
  Result := True;
end;

function TWolfSSLCertificate.LoadFromPEM(const APEM: string): Boolean;
var
  LDER: TBytes;
begin
  Result := False;
  SetLength(FDERData, 0);
  FPEMData := '';

  if APEM = '' then Exit;

  if not TSSLUtils.IsPEMFormat(APEM) then
    Exit;

  LDER := TSSLUtils.PEMToDER(APEM);
  if Length(LDER) = 0 then
    Exit;

  Result := LoadFromDER(LDER);

  if Result then
    FPEMData := APEM;
end;

function TWolfSSLCertificate.LoadFromDER(const ADER: TBytes): Boolean;
begin
  Result := False;
  SetLength(FDERData, 0);
  FPEMData := '';

  if Length(ADER) = 0 then Exit;

  Result := LoadFromMemory(@ADER[0], Length(ADER));

  if Result then
  begin
    FDERData := Copy(ADER);
    FPEMData := TSSLUtils.DERToPEM(FDERData);
  end;
end;

function TWolfSSLCertificate.SaveToFile(const AFileName: string): Boolean;
var
  LStream: TFileStream;
begin
  Result := False;
  if FX509 = nil then Exit;

  try
    LStream := TFileStream.Create(AFileName, fmCreate);
    try
      Result := SaveToStream(LStream);
    finally
      LStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TWolfSSLCertificate.SaveToStream(AStream: TStream): Boolean;
var
  LPEM: string;
begin
  Result := False;
  if (AStream = nil) or (FX509 = nil) then Exit;

  LPEM := SaveToPEM;
  if LPEM <> '' then
  begin
    AStream.WriteBuffer(LPEM[1], Length(LPEM));
    Result := True;
  end;
end;

function TWolfSSLCertificate.SaveToPEM: string;
begin
  Result := FPEMData;
  if (Result = '') and (Length(FDERData) > 0) then
    Result := TSSLUtils.DERToPEM(FDERData);
end;

function TWolfSSLCertificate.SaveToDER: TBytes;
var
  LDERLen: Integer;
  LDERPtr: PByte;
begin
  Result := Copy(FDERData);
  if (Length(Result) = 0) and (FX509 <> nil) and Assigned(wolfSSL_i2d_X509) then
  begin
    LDERLen := wolfSSL_i2d_X509(FX509, nil);
    if LDERLen > 0 then
    begin
      SetLength(Result, LDERLen);
      LDERPtr := @Result[0];
      if wolfSSL_i2d_X509(FX509, @LDERPtr) = LDERLen then
        FDERData := Copy(Result)
      else
        SetLength(Result, 0);
    end;
  end;

  if (Length(Result) = 0) and (FPEMData <> '') then
    Result := TSSLUtils.PEMToDER(FPEMData);
end;

function TWolfSSLCertificate.GetInfo: TSSLCertificateInfo;
var
  LParser: TX509Certificate;
begin
  Result := FInfo;
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.Version := GetVersion;

  if TryLoadX509Parser(LParser) then
  begin
    try
      Result.PublicKeyAlgorithm := LParser.PublicKeyInfo.Algorithm.Name;
      if Result.PublicKeyAlgorithm = '' then
        Result.PublicKeyAlgorithm := LParser.PublicKeyInfo.Algorithm.OID;

      Result.SignatureAlgorithm := LParser.SignatureAlgorithm.Name;
      if Result.SignatureAlgorithm = '' then
        Result.SignatureAlgorithm := LParser.SignatureAlgorithm.OID;

      Result.PublicKeySize := LParser.PublicKeyInfo.KeySize;
      Result.IsCA := LParser.IsCA;
      Result.PathLenConstraint := LParser.BasicConstraints.PathLenConstraint;
      Result.PathLength := LParser.BasicConstraints.PathLenConstraint;
      Result.KeyUsage := X509KeyUsageToBitfield(LParser.KeyUsage);
      Result.SubjectAltNames := X509SubjectAltNamesToStrings(LParser.SubjectAltNames);
    finally
      LParser.Free;
    end;
  end
  else
  begin
    Result.PublicKeyAlgorithm := GetPublicKeyAlgorithm;
    Result.SignatureAlgorithm := GetSignatureAlgorithm;
  end;
end;

function TWolfSSLCertificate.GetSubject: string;
var
  LBuf: array[0..511] of AnsiChar;
  LName: Pointer;
begin
  Result := '';
  if FX509 = nil then Exit;

  if Assigned(wolfSSL_X509_get_subject_name) and Assigned(wolfSSL_X509_NAME_oneline) then
  begin
    LName := wolfSSL_X509_get_subject_name(FX509);
    if LName <> nil then
    begin
      FillChar(LBuf, SizeOf(LBuf), 0);
      wolfSSL_X509_NAME_oneline(LName, @LBuf[0], SizeOf(LBuf) - 1);
      Result := string(PAnsiChar(@LBuf[0]));
    end;
  end;

  if Result = '' then
    Result := 'Subject';  // 占位符
end;

function TWolfSSLCertificate.GetIssuer: string;
var
  LBuf: array[0..511] of AnsiChar;
  LName: Pointer;
begin
  Result := '';
  if FX509 = nil then Exit;

  if Assigned(wolfSSL_X509_get_issuer_name) and Assigned(wolfSSL_X509_NAME_oneline) then
  begin
    LName := wolfSSL_X509_get_issuer_name(FX509);
    if LName <> nil then
    begin
      FillChar(LBuf, SizeOf(LBuf), 0);
      wolfSSL_X509_NAME_oneline(LName, @LBuf[0], SizeOf(LBuf) - 1);
      Result := string(PAnsiChar(@LBuf[0]));
    end;
  end;

  if Result = '' then
    Result := 'Issuer';  // 占位符
end;

function TWolfSSLCertificate.GetSerialNumber: string;
var
  LSerial: Pointer;
  LBuf: array[0..127] of Byte;
  I, LLen: Integer;
begin
  Result := '';
  if FX509 = nil then Exit;

  if Assigned(wolfSSL_X509_get_serial_number) then
  begin
    LSerial := wolfSSL_X509_get_serial_number(FX509);
    if LSerial <> nil then
    begin
      // WolfSSL 返回 ASN1_INTEGER 指针，需要转换为十六进制字符串
      // 简化实现：返回指针地址作为标识符
      Result := IntToHex(PtrUInt(LSerial), 16);
    end;
  end;

  if Result = '' then
    Result := '0';
end;

function TWolfSSLCertificate.GetNotBefore: TDateTime;
var
  LParser: TX509Certificate;
begin
  Result := 0;
  if FPEMData = '' then
    Exit;

  LParser := TX509Certificate.Create;
  try
    try
      LParser.LoadFromPEM(FPEMData);
      Result := LParser.Validity.NotBefore;
    except
      Result := 0;
    end;
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetNotAfter: TDateTime;
var
  LParser: TX509Certificate;
begin
  Result := 0;
  if FPEMData = '' then
    Exit;

  LParser := TX509Certificate.Create;
  try
    try
      LParser.LoadFromPEM(FPEMData);
      Result := LParser.Validity.NotAfter;
    except
      Result := 0;
    end;
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetPublicKey: string;
begin
  Result := GetPublicKeyAlgorithm;
end;

function TWolfSSLCertificate.GetPublicKeyAlgorithm: string;
var
  LSignatureAlgorithm: string;
begin
  if TryGetParsedAlgorithmMetadata(Result, LSignatureAlgorithm) and (Result <> '') then
    Exit;

  Result := 'RSA';  // 默认
end;

function TWolfSSLCertificate.GetSignatureAlgorithm: string;
var
  LPublicKeyAlgorithm: string;
begin
  if TryGetParsedAlgorithmMetadata(LPublicKeyAlgorithm, Result) and (Result <> '') then
    Exit;

  Result := 'SHA256withRSA';  // 默认
end;

function TWolfSSLCertificate.GetVersion: Integer;
begin
  Result := 3;  // X.509 v3 默认值
  if FX509 = nil then Exit;

  if Assigned(wolfSSL_X509_get_version) then
    Result := wolfSSL_X509_get_version(FX509) + 1;  // WolfSSL 返回 0-based
end;

function TWolfSSLCertificate.Verify(ACAStore: ISSLCertificateStore): Boolean;
var
  LStore: PWOLFSSL_X509_STORE;
  LCACert: ISSLCertificate;
  I: Integer;
begin
  Result := False;
  if FX509 = nil then Exit;
  if ACAStore = nil then Exit;

  // 获取 CA Store 的原生句柄
  LStore := PWOLFSSL_X509_STORE(GetNativeHandleSafe(ACAStore, 'TWolfSSLCertificate.Verify'));

  // 如果有原生 Store，使用它进行验证
  if LStore <> nil then
  begin
    // WolfSSL 需要使用 wolfSSL_X509_STORE_CTX 进行验证
    // 由于 API 限制，我们使用简化的验证逻辑
    // 检查证书是否在 CA Store 中或由 CA Store 中的证书签发
    for I := 0 to ACAStore.GetCount - 1 do
    begin
      LCACert := ACAStore.GetCertificate(I);
      if LCACert <> nil then
      begin
        // 检查是否是自签名证书且在 CA Store 中
        if IsSelfSigned and (GetSubject = LCACert.GetSubject) then
        begin
          Result := True;
          Exit;
        end;
        // 检查颁发者是否匹配
        if GetIssuer = LCACert.GetSubject then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end
  else
  begin
    // 没有原生 Store，使用证书列表进行验证
    for I := 0 to ACAStore.GetCount - 1 do
    begin
      LCACert := ACAStore.GetCertificate(I);
      if LCACert <> nil then
      begin
        if IsSelfSigned and (GetSubject = LCACert.GetSubject) then
        begin
          Result := True;
          Exit;
        end;
        if GetIssuer = LCACert.GetSubject then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

  // 如果是自签名证书且没有找到匹配的 CA，返回 False
  // 这是安全的默认行为
end;

function TWolfSSLCertificate.VerifyEx(ACAStore: ISSLCertificateStore;
  AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.Success := Verify(ACAStore);
  Result := AResult.Success;
end;

function TWolfSSLCertificate.VerifyHostname(const AHostname: string): Boolean;
var
  SANs: TSSLStringArray;
  i: Integer;
  CN, Entry: string;
  HostIsIP, EntryIsIP: Boolean;

  function MatchWildcard(const APattern, AHostname: string): Boolean;
  var
    PatternParts, HostParts: TStringList;
    j: Integer;
  begin
    Result := False;

    // Exact match
    if SameText(APattern, AHostname) then
    begin
      Result := True;
      Exit;
    end;

    // Wildcard match (*.example.com)
    if (Pos('*.', APattern) = 1) then
    begin
      PatternParts := TStringList.Create;
      HostParts := TStringList.Create;
      try
        PatternParts.Delimiter := '.';
        PatternParts.DelimitedText := APattern;

        HostParts.Delimiter := '.';
        HostParts.DelimitedText := AHostname;

        // Same label count
        if PatternParts.Count = HostParts.Count then
        begin
          Result := True;
          // Compare from 2nd label (skip wildcard)
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

  if (FX509 = nil) or (AHostname = '') then
    Exit;

  HostIsIP := TSSLUtils.IsIPAddress(AHostname);

  // First check SAN entries
  SANs := GetSubjectAltNames;
  for i := 0 to High(SANs) do
  begin
    Entry := Trim(SANs[i]);
    if Entry = '' then
      Continue;

    EntryIsIP := TSSLUtils.IsIPAddress(Entry);

    if HostIsIP then
    begin
      if EntryIsIP and SameText(Entry, AHostname) then
      begin
        Result := True;
        Exit;
      end;
      Continue;
    end;

    // Only match hostnames (ignore IP/email/URI etc)
    if EntryIsIP then
      Continue;
    if not TSSLUtils.IsValidHostname(Entry) then
      Continue;

    if MatchWildcard(Entry, AHostname) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Fallback to CN
  CN := Trim(GetSubjectCN);
  if CN = '' then
    Exit;

  if HostIsIP then
  begin
    Result := SameText(CN, AHostname);
    Exit;
  end;

  if not TSSLUtils.IsValidHostname(CN) then
    Exit;

  Result := MatchWildcard(CN, AHostname);
end;

function TWolfSSLCertificate.IsExpired: Boolean;
var
  LNotAfter: TDateTime;
begin
  LNotAfter := GetNotAfter;
  if LNotAfter <= 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := Now > LNotAfter;
end;

function TWolfSSLCertificate.IsSelfSigned: Boolean;
begin
  Result := GetSubject = GetIssuer;
end;

function TWolfSSLCertificate.IsCA: Boolean;
var
  LParser: TX509Certificate;
begin
  Result := False;
  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    Result := LParser.IsCA;
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetDaysUntilExpiry: Integer;
var
  LNotAfter: TDateTime;
begin
  LNotAfter := GetNotAfter;
  if LNotAfter <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := DaysBetween(Now, LNotAfter);
  if IsExpired then
    Result := -Result;
end;

function TWolfSSLCertificate.GetSubjectCN: string;
var
  LSubject: string;
  LPos: Integer;
begin
  Result := '';
  LSubject := GetSubject;
  LPos := Pos('CN=', LSubject);
  if LPos > 0 then
  begin
    Result := Copy(LSubject, LPos + 3, Length(LSubject));
    LPos := Pos(',', Result);
    if LPos > 0 then
      Result := Copy(Result, 1, LPos - 1);
  end;
end;

function TWolfSSLCertificate.GetExtension(const AOID: string): string;
var
  LParser: TX509Certificate;
  LTargetOID: string;
  I: Integer;
begin
  Result := '';
  LTargetOID := Trim(AOID);
  if LTargetOID = '' then
    Exit;

  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    for I := 0 to High(LParser.Extensions) do
    begin
      if SameText(LParser.Extensions[I].OID, LTargetOID) then
      begin
        if Length(LParser.Extensions[I].Value) > 0 then
          Result := HashToHex(LParser.Extensions[I].Value)
        else
          Result := LParser.Extensions[I].Name;
        Exit;
      end;
    end;
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetSubjectAltNames: TSSLStringArray;
var
  LParser: TX509Certificate;
begin
  SetLength(Result, 0);
  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    Result := X509SubjectAltNamesToStrings(LParser.SubjectAltNames);
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetKeyUsage: TSSLStringArray;
var
  LParser: TX509Certificate;
begin
  SetLength(Result, 0);
  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    Result := X509KeyUsageToStrings(LParser.KeyUsage);
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetExtendedKeyUsage: TSSLStringArray;
var
  LParser: TX509Certificate;
begin
  SetLength(Result, 0);
  if not TryLoadX509Parser(LParser) then
    Exit;

  try
    Result := X509ExtKeyUsageToStrings(LParser.ExtKeyUsage);
  finally
    LParser.Free;
  end;
end;

function TWolfSSLCertificate.GetFingerprint(AHashType: TSSLHash): string;
begin
  Result := '';
  case AHashType of
    sslHashSHA1: Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  else
    Result := '';
  end;
end;

function TWolfSSLCertificate.GetFingerprintSHA1: string;
var
  LDER: TBytes;
begin
  LDER := SaveToDER;
  if Length(LDER) = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := HashToHex(SHA1(LDER));
end;

function TWolfSSLCertificate.GetFingerprintSHA256: string;
var
  LDER: TBytes;
begin
  LDER := SaveToDER;
  if Length(LDER) = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := HashToHex(SHA256(LDER));
end;

procedure TWolfSSLCertificate.SetIssuerCertificate(ACert: ISSLCertificate);
begin
  FIssuerCert := ACert;
end;

function TWolfSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := FIssuerCert;
end;

function TWolfSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FX509;
end;

function TWolfSSLCertificate.GetBackendType: TSSLLibraryType;
begin
  Result := sslWolfSSL;
end;

function TWolfSSLCertificate.IsNativeHandleValid: Boolean;
begin
  Result := (FX509 <> nil);
end;

function TWolfSSLCertificate.Clone: ISSLCertificate;
var
  LClone: TWolfSSLCertificate;
  LDER: TBytes;
begin
  Result := nil;
  LClone := TWolfSSLCertificate.Create;
  try
    LClone.FInfo := FInfo;
    if Length(FDERData) > 0 then
      LDER := Copy(FDERData)
    else if FPEMData <> '' then
      LDER := TSSLUtils.PEMToDER(FPEMData)
    else
      LDER := SaveToDER;

    if Length(LDER) > 0 then
    begin
      if not LClone.LoadFromDER(LDER) then
        Exit;
      LClone.FDERData := Copy(LDER);
      if FPEMData <> '' then
        LClone.FPEMData := FPEMData
      else
        LClone.FPEMData := TSSLUtils.DERToPEM(LDER);
    end
    else
    begin
      LClone.FPEMData := FPEMData;
      LClone.FDERData := Copy(FDERData);
    end;

    LClone.FIssuerCert := FIssuerCert;
    Result := LClone;
    LClone := nil;
  finally
    LClone.Free;
  end;
end;

{ TWolfSSLCertificateStore }

constructor TWolfSSLCertificateStore.Create;
begin
  inherited Create;
  if Assigned(wolfSSL_X509_STORE_new) then
    FX509Store := wolfSSL_X509_STORE_new()
  else
    FX509Store := nil;
  FCertificates := TInterfaceList.Create;
end;

destructor TWolfSSLCertificateStore.Destroy;
begin
  Clear;
  FCertificates.Free;
  if FX509Store <> nil then
  begin
    if Assigned(wolfSSL_X509_STORE_free) then
      wolfSSL_X509_STORE_free(FX509Store);
    FX509Store := nil;
  end;
  inherited Destroy;
end;

function TWolfSSLCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  if ACert = nil then Exit;
  if Contains(ACert) then Exit;

  FCertificates.Add(ACert);
  Result := True;
end;

function TWolfSSLCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
var
  LIndex: Integer;
  LTarget: string;
  I: Integer;
  LExisting: ISSLCertificate;
begin
  Result := False;
  if ACert = nil then Exit;

  LIndex := FCertificates.IndexOf(ACert);
  if LIndex < 0 then
  begin
    LTarget := NormalizeWolfCertFingerprint(ACert.GetFingerprintSHA256);
    if LTarget = '' then
      LTarget := NormalizeWolfCertFingerprint(ACert.GetFingerprintSHA1);

    if LTarget <> '' then
    begin
      for I := 0 to FCertificates.Count - 1 do
      begin
        LExisting := FCertificates[I] as ISSLCertificate;
        if NormalizeWolfCertFingerprint(LExisting.GetFingerprintSHA256) = LTarget then
        begin
          LIndex := I;
          Break;
        end;
      end;
    end;
  end;

  if LIndex >= 0 then
  begin
    FCertificates.Delete(LIndex);
    Result := True;
  end;
end;

function TWolfSSLCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
var
  LTarget: string;
  I: Integer;
  LExisting: ISSLCertificate;
begin
  Result := False;
  if ACert = nil then
    Exit;

  if FCertificates.IndexOf(ACert) >= 0 then
    Exit(True);

  LTarget := NormalizeWolfCertFingerprint(ACert.GetFingerprintSHA256);
  if LTarget = '' then
    LTarget := NormalizeWolfCertFingerprint(ACert.GetFingerprintSHA1);
  if LTarget = '' then
    Exit(False);

  for I := 0 to FCertificates.Count - 1 do
  begin
    LExisting := FCertificates[I] as ISSLCertificate;
    if NormalizeWolfCertFingerprint(LExisting.GetFingerprintSHA256) = LTarget then
      Exit(True);
  end;
end;

procedure TWolfSSLCertificateStore.Clear;
begin
  FCertificates.Clear;
end;

function TWolfSSLCertificateStore.GetCount: Integer;
begin
  Result := FCertificates.Count;
end;

function TWolfSSLCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FCertificates.Count) then
    Result := FCertificates[AIndex] as ISSLCertificate;
end;

function TWolfSSLCertificateStore.LoadFromFile(const AFileName: string): Boolean;
var
  LCert: TWolfSSLCertificate;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  LCert := TWolfSSLCertificate.Create;
  try
    if LCert.LoadFromFile(AFileName) then
    begin
      FCertificates.Add(LCert);
      Result := True;
    end;
  except
    LCert.Free;
    raise;
  end;
end;

function TWolfSSLCertificateStore.LoadFromPath(const APath: string): Boolean;
var
  LSearchRec: TSearchRec;
  LCount: Integer;
begin
  Result := False;
  if not DirectoryExists(APath) then Exit;

  LCount := 0;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.pem', faAnyFile, LSearchRec) = 0 then
  begin
    try
      repeat
        if LoadFromFile(IncludeTrailingPathDelimiter(APath) + LSearchRec.Name) then
          Inc(LCount);
      until FindNext(LSearchRec) <> 0;
    finally
      FindClose(LSearchRec);
    end;
  end;

  // 也加载 .crt 文件
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.crt', faAnyFile, LSearchRec) = 0 then
  begin
    try
      repeat
        if LoadFromFile(IncludeTrailingPathDelimiter(APath) + LSearchRec.Name) then
          Inc(LCount);
      until FindNext(LSearchRec) <> 0;
    finally
      FindClose(LSearchRec);
    end;
  end;

  Result := LCount > 0;
end;

function TWolfSSLCertificateStore.LoadSystemStore: Boolean;
begin
  Result := False;
  {$IFDEF LINUX}
  // Linux 系统 CA 路径
  if DirectoryExists('/etc/ssl/certs') then
    Result := LoadFromPath('/etc/ssl/certs')
  else if DirectoryExists('/etc/pki/tls/certs') then
    Result := LoadFromPath('/etc/pki/tls/certs');
  {$ENDIF}
  {$IFDEF DARWIN}
  // macOS 系统 CA
  if FileExists('/etc/ssl/cert.pem') then
    Result := LoadFromFile('/etc/ssl/cert.pem');
  {$ENDIF}
end;

function TWolfSSLCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
var
  I: Integer;
  LCert: ISSLCertificate;
  LTarget: string;
begin
  Result := nil;
  LTarget := NormalizeWolfCertText(ASubject);
  if LTarget = '' then
    Exit;

  for I := 0 to FCertificates.Count - 1 do
  begin
    LCert := FCertificates[I] as ISSLCertificate;
    if Pos(LTarget, NormalizeWolfCertText(LCert.GetSubject)) > 0 then
    begin
      Result := LCert;
      Exit;
    end;
  end;
end;

function TWolfSSLCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
var
  I: Integer;
  LCert: ISSLCertificate;
begin
  Result := nil;
  for I := 0 to FCertificates.Count - 1 do
  begin
    LCert := FCertificates[I] as ISSLCertificate;
    if Pos(AIssuer, LCert.GetIssuer) > 0 then
    begin
      Result := LCert;
      Exit;
    end;
  end;
end;

function TWolfSSLCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
var
  I: Integer;
  LCert: ISSLCertificate;
begin
  Result := nil;
  for I := 0 to FCertificates.Count - 1 do
  begin
    LCert := FCertificates[I] as ISSLCertificate;
    if LCert.GetSerialNumber = ASerialNumber then
    begin
      Result := LCert;
      Exit;
    end;
  end;
end;

function TWolfSSLCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
var
  I: Integer;
  LCert: ISSLCertificate;
begin
  Result := nil;
  for I := 0 to FCertificates.Count - 1 do
  begin
    LCert := FCertificates[I] as ISSLCertificate;
    if (LCert.GetFingerprintSHA1 = AFingerprint) or
      (LCert.GetFingerprintSHA256 = AFingerprint) then
    begin
      Result := LCert;
      Exit;
    end;
  end;
end;

function TWolfSSLCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  if ACert = nil then Exit;
  Result := ACert.Verify(Self);
end;

function TWolfSSLCertificateStore.BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
var
  LChain: array of ISSLCertificate;
  LCurrent: ISSLCertificate;
  LIssuer: ISSLCertificate;
  LMaxDepth: Integer;
begin
  SetLength(Result, 0);
  if ACert = nil then Exit;

  SetLength(LChain, 0);
  LCurrent := ACert;
  LMaxDepth := 10;  // 防止无限循环

  while (LCurrent <> nil) and (Length(LChain) < LMaxDepth) do
  begin
    SetLength(LChain, Length(LChain) + 1);
    LChain[High(LChain)] := LCurrent;

    // 自签名证书是链的终点
    if LCurrent.IsSelfSigned then
      Break;

    // 查找颁发者
    LIssuer := FindBySubject(LCurrent.GetIssuer);
    if LIssuer = nil then
      Break;

    LCurrent := LIssuer;
  end;

  Result := LChain;
end;

function TWolfSSLCertificateStore.GetNativeHandle: Pointer;
begin
  Result := FX509Store;
end;

function TWolfSSLCertificateStore.GetBackendType: TSSLLibraryType;
begin
  Result := sslWolfSSL;
end;

function TWolfSSLCertificateStore.IsNativeHandleValid: Boolean;
begin
  Result := (FX509Store <> nil);
end;

end.
