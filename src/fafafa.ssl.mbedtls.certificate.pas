{**
 * Unit: fafafa.ssl.mbedtls.certificate
 * Purpose: MbedTLS 证书和证书存储实现
 *
 * 实现 ISSLCertificate 和 ISSLCertificateStore 接口的 MbedTLS 后端。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.mbedtls.certificate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api;

type
  { TMbedTLSCertificate - MbedTLS 证书类 }
  TMbedTLSCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FX509Crt: Pmbedtls_x509_crt;
    FInfo: TSSLCertificateInfo;
    FPEMData: string;
    FDERData: TBytes;
    FIssuerCert: ISSLCertificate;
    FOwnsHandle: Boolean;

    procedure AllocateCertificate;
    procedure FreeCertificate;

  public
    constructor Create; overload;
    constructor Create(ACrt: Pmbedtls_x509_crt; AOwnsHandle: Boolean = False); overload;
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

  { TMbedTLSCertificateStore - MbedTLS 证书存储类 }
  TMbedTLSCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  private
    FCACerts: Pmbedtls_x509_crt;
    FCertificates: TInterfaceList;

    procedure AllocateStore;
    procedure FreeStore;

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

const
  MBEDTLS_X509_CRT_SIZE = 1024;  // 估算大小

{ TMbedTLSCertificate }

constructor TMbedTLSCertificate.Create;
begin
  inherited Create;
  FX509Crt := nil;
  FPEMData := '';
  SetLength(FDERData, 0);
  FIssuerCert := nil;
  FOwnsHandle := True;
  FillChar(FInfo, SizeOf(FInfo), 0);
end;

constructor TMbedTLSCertificate.Create(ACrt: Pmbedtls_x509_crt; AOwnsHandle: Boolean);
begin
  Create;
  FX509Crt := ACrt;
  FOwnsHandle := AOwnsHandle;
end;

destructor TMbedTLSCertificate.Destroy;
begin
  if FOwnsHandle then
    FreeCertificate;
  FIssuerCert := nil;
  inherited Destroy;
end;

procedure TMbedTLSCertificate.AllocateCertificate;
begin
  if FX509Crt <> nil then
    FreeCertificate;

  GetMem(FX509Crt, MBEDTLS_X509_CRT_SIZE);
  FillChar(FX509Crt^, MBEDTLS_X509_CRT_SIZE, 0);

  if Assigned(mbedtls_x509_crt_init) then
    mbedtls_x509_crt_init(FX509Crt);

  FOwnsHandle := True;
end;

procedure TMbedTLSCertificate.FreeCertificate;
begin
  if FX509Crt <> nil then
  begin
    if Assigned(mbedtls_x509_crt_free) then
      mbedtls_x509_crt_free(FX509Crt);
    FreeMem(FX509Crt);
    FX509Crt := nil;
  end;
end;

function TMbedTLSCertificate.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  if not Assigned(mbedtls_x509_crt_parse_file) then Exit;

  AllocateCertificate;

  if mbedtls_x509_crt_parse_file(FX509Crt, PAnsiChar(AnsiString(AFileName))) = 0 then
    Result := True
  else
    FreeCertificate;
end;

function TMbedTLSCertificate.LoadFromStream(AStream: TStream): Boolean;
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

function TMbedTLSCertificate.LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
begin
  Result := False;
  if (AData = nil) or (ASize <= 0) then Exit;
  if not Assigned(mbedtls_x509_crt_parse) then Exit;

  AllocateCertificate;

  // MbedTLS 需要 null 终止的 PEM 或 DER 数据
  if mbedtls_x509_crt_parse(FX509Crt, AData, ASize) = 0 then
    Result := True
  else
    FreeCertificate;
end;

function TMbedTLSCertificate.LoadFromPEM(const APEM: string): Boolean;
var
  LAnsi: AnsiString;
begin
  Result := False;
  if APEM = '' then Exit;

  LAnsi := AnsiString(APEM);
  FPEMData := APEM;
  // MbedTLS PEM 解析需要 null 终止
  Result := LoadFromMemory(PAnsiChar(LAnsi), Length(LAnsi) + 1);
end;

function TMbedTLSCertificate.LoadFromDER(const ADER: TBytes): Boolean;
begin
  Result := False;
  if Length(ADER) = 0 then Exit;

  FDERData := Copy(ADER);
  Result := LoadFromMemory(@ADER[0], Length(ADER));
end;

function TMbedTLSCertificate.SaveToFile(const AFileName: string): Boolean;
var
  LStream: TFileStream;
begin
  Result := False;
  if FX509Crt = nil then Exit;

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

function TMbedTLSCertificate.SaveToStream(AStream: TStream): Boolean;
var
  LPEM: string;
begin
  Result := False;
  if (AStream = nil) or (FX509Crt = nil) then Exit;

  LPEM := SaveToPEM;
  if LPEM <> '' then
  begin
    AStream.WriteBuffer(LPEM[1], Length(LPEM));
    Result := True;
  end;
end;

function TMbedTLSCertificate.SaveToPEM: string;
begin
  Result := FPEMData;
end;

function TMbedTLSCertificate.SaveToDER: TBytes;
begin
  Result := Copy(FDERData);
end;

function TMbedTLSCertificate.GetInfo: TSSLCertificateInfo;
begin
  Result := FInfo;
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.Version := GetVersion;
end;

function TMbedTLSCertificate.GetSubject: string;
begin
  Result := 'Subject';  // 占位符 - MbedTLS 需要额外 API
end;

function TMbedTLSCertificate.GetIssuer: string;
begin
  Result := 'Issuer';  // 占位符
end;

function TMbedTLSCertificate.GetSerialNumber: string;
begin
  Result := '0';  // 占位符
end;

function TMbedTLSCertificate.GetNotBefore: TDateTime;
begin
  Result := Now - 365;  // 占位符
end;

function TMbedTLSCertificate.GetNotAfter: TDateTime;
begin
  Result := Now + 365;  // 占位符
end;

function TMbedTLSCertificate.GetPublicKey: string;
begin
  Result := '';
end;

function TMbedTLSCertificate.GetPublicKeyAlgorithm: string;
begin
  Result := 'RSA';  // 默认
end;

function TMbedTLSCertificate.GetSignatureAlgorithm: string;
begin
  Result := 'SHA256withRSA';  // 默认
end;

function TMbedTLSCertificate.GetVersion: Integer;
begin
  Result := 3;  // X.509 v3 默认值
end;

function TMbedTLSCertificate.Verify(ACAStore: ISSLCertificateStore): Boolean;
begin
  Result := False;
  if FX509Crt = nil then Exit;
  if ACAStore = nil then Exit;
  Result := True;  // 占位符
end;

function TMbedTLSCertificate.VerifyEx(ACAStore: ISSLCertificateStore;
  AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
begin
  FillChar(AResult, SizeOf(AResult), 0);
  AResult.Success := Verify(ACAStore);
  Result := AResult.Success;
end;

function TMbedTLSCertificate.VerifyHostname(const AHostname: string): Boolean;
begin
  Result := False;
  if FX509Crt = nil then Exit;
  if AHostname = '' then Exit;
  Result := Pos(AHostname, GetSubjectCN) > 0;
end;

function TMbedTLSCertificate.IsExpired: Boolean;
begin
  Result := Now > GetNotAfter;
end;

function TMbedTLSCertificate.IsSelfSigned: Boolean;
begin
  Result := GetSubject = GetIssuer;
end;

function TMbedTLSCertificate.IsCA: Boolean;
begin
  Result := False;  // 需要检查 BasicConstraints
end;

function TMbedTLSCertificate.GetDaysUntilExpiry: Integer;
begin
  Result := DaysBetween(Now, GetNotAfter);
  if IsExpired then
    Result := -Result;
end;

function TMbedTLSCertificate.GetSubjectCN: string;
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

function TMbedTLSCertificate.GetExtension(const AOID: string): string;
begin
  Result := '';
end;

function TMbedTLSCertificate.GetSubjectAltNames: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TMbedTLSCertificate.GetKeyUsage: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TMbedTLSCertificate.GetExtendedKeyUsage: TSSLStringArray;
begin
  SetLength(Result, 0);
end;

function TMbedTLSCertificate.GetFingerprint(AHashType: TSSLHash): string;
begin
  Result := '';
  case AHashType of
    sslHashSHA1: Result := GetFingerprintSHA1;
    sslHashSHA256: Result := GetFingerprintSHA256;
  end;
end;

function TMbedTLSCertificate.GetFingerprintSHA1: string;
begin
  Result := '';
end;

function TMbedTLSCertificate.GetFingerprintSHA256: string;
begin
  Result := '';
end;

procedure TMbedTLSCertificate.SetIssuerCertificate(ACert: ISSLCertificate);
begin
  FIssuerCert := ACert;
end;

function TMbedTLSCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := FIssuerCert;
end;

function TMbedTLSCertificate.GetNativeHandle: Pointer;
begin
  Result := FX509Crt;
end;

function TMbedTLSCertificate.Clone: ISSLCertificate;
var
  LClone: TMbedTLSCertificate;
begin
  LClone := TMbedTLSCertificate.Create;
  LClone.FPEMData := FPEMData;
  LClone.FDERData := Copy(FDERData);
  LClone.FInfo := FInfo;
  Result := LClone;
end;

{ TMbedTLSCertificateStore }

constructor TMbedTLSCertificateStore.Create;
begin
  inherited Create;
  FCACerts := nil;
  FCertificates := TInterfaceList.Create;
end;

destructor TMbedTLSCertificateStore.Destroy;
begin
  Clear;
  FCertificates.Free;
  FreeStore;
  inherited Destroy;
end;

procedure TMbedTLSCertificateStore.AllocateStore;
begin
  if FCACerts <> nil then
    FreeStore;

  GetMem(FCACerts, MBEDTLS_X509_CRT_SIZE);
  FillChar(FCACerts^, MBEDTLS_X509_CRT_SIZE, 0);

  if Assigned(mbedtls_x509_crt_init) then
    mbedtls_x509_crt_init(FCACerts);
end;

procedure TMbedTLSCertificateStore.FreeStore;
begin
  if FCACerts <> nil then
  begin
    if Assigned(mbedtls_x509_crt_free) then
      mbedtls_x509_crt_free(FCACerts);
    FreeMem(FCACerts);
    FCACerts := nil;
  end;
end;

function TMbedTLSCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  if ACert = nil then Exit;
  if Contains(ACert) then Exit;

  FCertificates.Add(ACert);
  Result := True;
end;

function TMbedTLSCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
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

function TMbedTLSCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
begin
  Result := FCertificates.IndexOf(ACert) >= 0;
end;

procedure TMbedTLSCertificateStore.Clear;
begin
  FCertificates.Clear;
end;

function TMbedTLSCertificateStore.GetCount: Integer;
begin
  Result := FCertificates.Count;
end;

function TMbedTLSCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FCertificates.Count) then
    Result := FCertificates[AIndex] as ISSLCertificate;
end;

function TMbedTLSCertificateStore.LoadFromFile(const AFileName: string): Boolean;
var
  LCert: TMbedTLSCertificate;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  LCert := TMbedTLSCertificate.Create;
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

function TMbedTLSCertificateStore.LoadFromPath(const APath: string): Boolean;
var
  LCount: Integer;
begin
  Result := False;
  if not DirectoryExists(APath) then Exit;

  LCount := 0;

  // 使用 MbedTLS 的路径加载功能
  if FCACerts = nil then
    AllocateStore;

  if Assigned(mbedtls_x509_crt_parse_path) then
  begin
    if mbedtls_x509_crt_parse_path(FCACerts, PAnsiChar(AnsiString(APath))) >= 0 then
      Inc(LCount);
  end;

  Result := LCount > 0;
end;

function TMbedTLSCertificateStore.LoadSystemStore: Boolean;
begin
  Result := False;
  {$IFDEF LINUX}
  if DirectoryExists('/etc/ssl/certs') then
    Result := LoadFromPath('/etc/ssl/certs')
  else if DirectoryExists('/etc/pki/tls/certs') then
    Result := LoadFromPath('/etc/pki/tls/certs');
  {$ENDIF}
  {$IFDEF DARWIN}
  if FileExists('/etc/ssl/cert.pem') then
    Result := LoadFromFile('/etc/ssl/cert.pem');
  {$ENDIF}
end;

function TMbedTLSCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
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

function TMbedTLSCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
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

function TMbedTLSCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
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

function TMbedTLSCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
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

function TMbedTLSCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  if ACert = nil then Exit;
  Result := ACert.Verify(Self);
end;

function TMbedTLSCertificateStore.BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
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
  LMaxDepth := 10;

  while (LCurrent <> nil) and (Length(LChain) < LMaxDepth) do
  begin
    SetLength(LChain, Length(LChain) + 1);
    LChain[High(LChain)] := LCurrent;

    if LCurrent.IsSelfSigned then
      Break;

    LIssuer := FindBySubject(LCurrent.GetIssuer);
    if LIssuer = nil then
      Break;

    LCurrent := LIssuer;
  end;

  Result := LChain;
end;

function TMbedTLSCertificateStore.GetNativeHandle: Pointer;
begin
  Result := FCACerts;
end;

end.
