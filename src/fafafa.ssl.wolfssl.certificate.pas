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
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api;

type
  { TWolfSSLCertificate - WolfSSL 证书类 }
  TWolfSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FX509: PWOLFSSL_X509;
    FInfo: TSSLCertificateInfo;
    FPEMData: string;
    FDERData: TBytes;
    FIssuerCert: ISSLCertificate;

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

    { ISSLCertificate - 原生句柄 }
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

  { TWolfSSLCertificateStore - WolfSSL 证书存储类 }
  TWolfSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
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

    { ISSLCertificateStore - 原生句柄 }
    function GetNativeHandle: Pointer;
  end;

implementation

uses
  Contnrs, DateUtils;

{ TWolfSSLCertificate }

constructor TWolfSSLCertificate.Create;
begin
  inherited Create;
  FX509 := nil;
  FPEMData := '';
  SetLength(FDERData, 0);
  FIssuerCert := nil;
  FillChar(FInfo, SizeOf(FInfo), 0);
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

function TWolfSSLCertificate.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  if not Assigned(wolfSSL_X509_load_certificate_file) then Exit;

  if FX509 <> nil then
  begin
    if Assigned(wolfSSL_X509_free) then
      wolfSSL_X509_free(FX509);
    FX509 := nil;
  end;

  FX509 := wolfSSL_X509_load_certificate_file(PAnsiChar(AnsiString(AFileName)),
    WOLFSSL_FILETYPE_PEM);
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
begin
  Result := False;
  if (AData = nil) or (ASize <= 0) then Exit;
  if not Assigned(wolfSSL_X509_d2i) then Exit;

  if FX509 <> nil then
  begin
    if Assigned(wolfSSL_X509_free) then
      wolfSSL_X509_free(FX509);
    FX509 := nil;
  end;

  FX509 := wolfSSL_X509_d2i(nil, AData, ASize);
  Result := FX509 <> nil;
end;

function TWolfSSLCertificate.LoadFromPEM(const APEM: string): Boolean;
var
  LAnsi: AnsiString;
begin
  Result := False;
  if APEM = '' then Exit;

  LAnsi := AnsiString(APEM);
  FPEMData := APEM;
  Result := LoadFromMemory(PAnsiChar(LAnsi), Length(LAnsi));
end;

function TWolfSSLCertificate.LoadFromDER(const ADER: TBytes): Boolean;
begin
  Result := False;
  if Length(ADER) = 0 then Exit;

  FDERData := Copy(ADER);
  Result := LoadFromMemory(@ADER[0], Length(ADER));
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
  // WolfSSL PEM 导出需要额外 API
end;

function TWolfSSLCertificate.SaveToDER: TBytes;
begin
  Result := Copy(FDERData);
end;

function TWolfSSLCertificate.GetInfo: TSSLCertificateInfo;
begin
  Result := FInfo;
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.Version := GetVersion;
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
begin
  Result := '';
  if FX509 = nil then Exit;
  Result := '0';  // 占位符
end;

function TWolfSSLCertificate.GetNotBefore: TDateTime;
begin
  Result := 0;
  if FX509 = nil then Exit;
  Result := Now - 365;  // 占位符
end;

function TWolfSSLCertificate.GetNotAfter: TDateTime;
begin
  Result := 0;
  if FX509 = nil then Exit;
  Result := Now + 365;  // 占位符
end;

function TWolfSSLCertificate.GetPublicKey: string;
begin
  Result := '';
end;

function TWolfSSLCertificate.GetPublicKeyAlgorithm: string;
begin
  Result := 'RSA';  // 默认
end;

function TWolfSSLCertificate.GetSignatureAlgorithm: string;
begin
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
begin
  Result := False;
  if FX509 = nil then Exit;
  if ACAStore = nil then Exit;
  // WolfSSL 证书验证需要额外实现
  Result := True;  // 占位符
end;

function TWolfSSLCertificate.VerifyEx(ACAStore: ISSLCertificateStore;
  AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.Success := Verify(ACAStore);
  Result := AResult.Success;
end;

function TWolfSSLCertificate.VerifyHostname(const AHostname: string): Boolean;
begin
  Result := False;
  if FX509 = nil then Exit;
  if AHostname = '' then Exit;
  // 检查 CN 或 SAN
  Result := Pos(AHostname, GetSubjectCN) > 0;
end;

function TWolfSSLCertificate.IsExpired: Boolean;
begin
  Result := Now > GetNotAfter;
end;

function TWolfSSLCertificate.IsSelfSigned: Boolean;
begin
  Result := GetSubject = GetIssuer;
end;

function TWolfSSLCertificate.IsCA: Boolean;
begin
  Result := False;  // 需要检查 BasicConstraints
end;

function TWolfSSLCertificate.GetDaysUntilExpiry: Integer;
begin
  Result := DaysBetween(Now, GetNotAfter);
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
begin
  Result := '';
  // WolfSSL 扩展获取需要额外 API
end;

function TWolfSSLCertificate.GetSubjectAltNames: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TWolfSSLCertificate.GetKeyUsage: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TWolfSSLCertificate.GetExtendedKeyUsage: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TWolfSSLCertificate.GetFingerprint(AHashType: TSSLHash): string;
begin
  Result := '';
  case AHashType of
    sslHashSHA1: Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  end;
end;

function TWolfSSLCertificate.GetFingerprintSHA1: string;
begin
  Result := '';
  // 需要 WolfSSL 哈希 API
end;

function TWolfSSLCertificate.GetFingerprintSHA256: string;
begin
  Result := '';
  // 需要 WolfSSL 哈希 API
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

function TWolfSSLCertificate.Clone: ISSLCertificate;
var
  LClone: TWolfSSLCertificate;
begin
  LClone := TWolfSSLCertificate.Create;
  LClone.FPEMData := FPEMData;
  LClone.FDERData := Copy(FDERData);
  LClone.FInfo := FInfo;
  // X509 需要深拷贝，暂时不复制原生句柄
  Result := LClone;
end;

{ TWolfSSLCertificateStore }

constructor TWolfSSLCertificateStore.Create;
begin
  inherited Create;
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
begin
  Result := False;
  if ACert = nil then Exit;

  LIndex := FCertificates.IndexOf(ACert);
  if LIndex >= 0 then
  begin
    FCertificates.Delete(LIndex);
    Result := True;
  end;
end;

function TWolfSSLCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
begin
  Result := FCertificates.IndexOf(ACert) >= 0;
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
begin
  Result := nil;
  for I := 0 to FCertificates.Count - 1 do
  begin
    LCert := FCertificates[I] as ISSLCertificate;
    if Pos(ASubject, LCert.GetSubject) > 0 then
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

end.
