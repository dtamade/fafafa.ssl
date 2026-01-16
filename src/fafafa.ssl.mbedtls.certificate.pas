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
  fafafa.ssl.base64,
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
  Contnrs, DateUtils,
  fafafa.ssl.utils;

const
  MBEDTLS_X509_CRT_SIZE = 1024;  // 估算大小

{ Helper function to extract field from MbedTLS info output }
function ExtractField(const AInfo, AFieldName: string): string;
var
  LPos, LEndPos: Integer;
  LSearchStr: string;
begin
  Result := '';
  LSearchStr := AFieldName + ' name';
  LPos := Pos(LSearchStr, AInfo);
  if LPos = 0 then
  begin
    LSearchStr := AFieldName + ':';
    LPos := Pos(LSearchStr, AInfo);
  end;

  if LPos > 0 then
  begin
    LPos := LPos + Length(LSearchStr);
    // Skip whitespace
    while (LPos <= Length(AInfo)) and (AInfo[LPos] in [' ', #9, ':']) do
      Inc(LPos);
    // Find end of line
    LEndPos := LPos;
    while (LEndPos <= Length(AInfo)) and not (AInfo[LEndPos] in [#10, #13]) do
      Inc(LEndPos);
    Result := Trim(Copy(AInfo, LPos, LEndPos - LPos));
  end;
end;

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
var
  LDER: TBytes;
  LBase64: string;
  LLine: string;
  I, LPos, LLineLen: Integer;
begin
  // Return cached PEM if available
  if FPEMData <> '' then
  begin
    Result := FPEMData;
    Exit;
  end;

  // Convert DER to PEM
  LDER := SaveToDER;
  if Length(LDER) = 0 then
  begin
    Result := '';
    Exit;
  end;

  // Encode to Base64
  LBase64 := TBase64Utils.Encode(LDER);
  if LBase64 = '' then
  begin
    Result := '';
    Exit;
  end;

  // Build PEM format with 64-char line wrapping
  Result := '-----BEGIN CERTIFICATE-----' + LineEnding;
  LPos := 1;
  LLineLen := 64;
  while LPos <= Length(LBase64) do
  begin
    if LPos + LLineLen - 1 <= Length(LBase64) then
      LLine := Copy(LBase64, LPos, LLineLen)
    else
      LLine := Copy(LBase64, LPos, Length(LBase64) - LPos + 1);
    Result := Result + LLine + LineEnding;
    Inc(LPos, LLineLen);
  end;
  Result := Result + '-----END CERTIFICATE-----' + LineEnding;

  // Cache the result
  FPEMData := Result;
end;

function TMbedTLSCertificate.SaveToDER: TBytes;
begin
  // Return cached DER if available
  if Length(FDERData) > 0 then
  begin
    Result := Copy(FDERData);
    Exit;
  end;

  // Extract DER from native handle
  SetLength(Result, 0);
  if FX509Crt = nil then Exit;

  // Access raw DER data from MbedTLS certificate structure
  if (FX509Crt^.raw.p <> nil) and (FX509Crt^.raw.len > 0) then
  begin
    SetLength(Result, FX509Crt^.raw.len);
    Move(FX509Crt^.raw.p^, Result[0], FX509Crt^.raw.len);
    // Cache the result
    FDERData := Copy(Result);
  end;
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
var
  LBuf: array[0..2047] of AnsiChar;
  LLen: Integer;
begin
  Result := '';
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen > 0 then
  begin
    // Parse subject from info output
    Result := ExtractField(string(LBuf), 'subject');
    if Result = '' then
      Result := 'Subject';  // Fallback
  end
  else
    Result := 'Subject';
end;

function TMbedTLSCertificate.GetIssuer: string;
var
  LBuf: array[0..2047] of AnsiChar;
  LLen: Integer;
begin
  Result := '';
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen > 0 then
  begin
    // Parse issuer from info output
    Result := ExtractField(string(LBuf), 'issuer');
    if Result = '' then
      Result := 'Issuer';  // Fallback
  end
  else
    Result := 'Issuer';
end;

function TMbedTLSCertificate.GetSerialNumber: string;
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo, LLine: string;
  LPos, LEndPos: Integer;
begin
  Result := '';
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  // 查找 "serial number" 行
  LPos := Pos('serial number', LowerCase(LInfo));
  if LPos > 0 then
  begin
    // 找到冒号后的内容
    LPos := Pos(':', Copy(LInfo, LPos, Length(LInfo)));
    if LPos > 0 then
    begin
      LPos := Pos('serial number', LowerCase(LInfo)) + LPos;
      // 跳过空白
      while (LPos <= Length(LInfo)) and (LInfo[LPos] in [' ', #9, ':']) do
        Inc(LPos);
      // 找到行尾
      LEndPos := LPos;
      while (LEndPos <= Length(LInfo)) and not (LInfo[LEndPos] in [#10, #13]) do
        Inc(LEndPos);
      Result := Trim(Copy(LInfo, LPos, LEndPos - LPos));
    end;
  end;

  if Result = '' then
    Result := '0';
end;

function ParseMbedTLSDate(const ADateStr: string): TDateTime;
var
  LYear, LMonth, LDay, LHour, LMin, LSec: Word;
  LParts: TStringArray;
  LDatePart, LTimePart: string;
  LPos: Integer;
begin
  Result := 0;
  // MbedTLS 日期格式: "2024-01-15 12:30:45" 或 "Jan 15 12:30:45 2024 GMT"
  if ADateStr = '' then Exit;

  // 尝试解析 YYYY-MM-DD HH:MM:SS 格式
  LPos := Pos(' ', ADateStr);
  if LPos > 0 then
  begin
    LDatePart := Copy(ADateStr, 1, LPos - 1);
    LTimePart := Copy(ADateStr, LPos + 1, Length(ADateStr));

    // 解析日期部分
    if (Length(LDatePart) >= 10) and (LDatePart[5] = '-') and (LDatePart[8] = '-') then
    begin
      try
        LYear := StrToIntDef(Copy(LDatePart, 1, 4), 0);
        LMonth := StrToIntDef(Copy(LDatePart, 6, 2), 0);
        LDay := StrToIntDef(Copy(LDatePart, 9, 2), 0);

        // 解析时间部分
        LHour := 0; LMin := 0; LSec := 0;
        if (Length(LTimePart) >= 8) and (LTimePart[3] = ':') then
        begin
          LHour := StrToIntDef(Copy(LTimePart, 1, 2), 0);
          LMin := StrToIntDef(Copy(LTimePart, 4, 2), 0);
          LSec := StrToIntDef(Copy(LTimePart, 7, 2), 0);
        end;

        if (LYear > 0) and (LMonth in [1..12]) and (LDay in [1..31]) then
          Result := EncodeDate(LYear, LMonth, LDay) + EncodeTime(LHour, LMin, LSec, 0);
      except
        Result := 0;
      end;
    end;
  end;
end;

function TMbedTLSCertificate.GetNotBefore: TDateTime;
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo, LDateStr: string;
  LPos, LEndPos: Integer;
begin
  Result := Now - 365;  // 默认值
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  // 查找 "issued on" 或 "not before"
  LPos := Pos('issued  on', LowerCase(LInfo));
  if LPos = 0 then
    LPos := Pos('not before', LowerCase(LInfo));

  if LPos > 0 then
  begin
    // 找到冒号后的内容
    LPos := LPos + 10;  // 跳过 "issued  on" 或 "not before"
    while (LPos <= Length(LInfo)) and (LInfo[LPos] in [' ', #9, ':']) do
      Inc(LPos);
    // 找到行尾
    LEndPos := LPos;
    while (LEndPos <= Length(LInfo)) and not (LInfo[LEndPos] in [#10, #13]) do
      Inc(LEndPos);
    LDateStr := Trim(Copy(LInfo, LPos, LEndPos - LPos));

    if LDateStr <> '' then
    begin
      Result := ParseMbedTLSDate(LDateStr);
      if Result = 0 then
        Result := Now - 365;
    end;
  end;
end;

function TMbedTLSCertificate.GetNotAfter: TDateTime;
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo, LDateStr: string;
  LPos, LEndPos: Integer;
begin
  Result := Now + 365;  // 默认值
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  // 查找 "expires on" 或 "not after"
  LPos := Pos('expires on', LowerCase(LInfo));
  if LPos = 0 then
    LPos := Pos('not after', LowerCase(LInfo));

  if LPos > 0 then
  begin
    // 找到冒号后的内容
    LPos := LPos + 10;  // 跳过 "expires on" 或 "not after"
    while (LPos <= Length(LInfo)) and (LInfo[LPos] in [' ', #9, ':']) do
      Inc(LPos);
    // 找到行尾
    LEndPos := LPos;
    while (LEndPos <= Length(LInfo)) and not (LInfo[LEndPos] in [#10, #13]) do
      Inc(LEndPos);
    LDateStr := Trim(Copy(LInfo, LPos, LEndPos - LPos));

    if LDateStr <> '' then
    begin
      Result := ParseMbedTLSDate(LDateStr);
      if Result = 0 then
        Result := Now + 365;
    end;
  end;
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
var
  LFlags: Cardinal;
  LCACerts: Pmbedtls_x509_crt;
begin
  Result := False;
  if FX509Crt = nil then Exit;
  if ACAStore = nil then Exit;
  if not Assigned(mbedtls_x509_crt_verify) then Exit;

  LCACerts := Pmbedtls_x509_crt(ACAStore.GetNativeHandle);
  if LCACerts = nil then Exit;

  LFlags := 0;
  Result := mbedtls_x509_crt_verify(FX509Crt, LCACerts, nil, nil, @LFlags, nil, nil) = 0;
end;

function TMbedTLSCertificate.VerifyEx(ACAStore: ISSLCertificateStore;
  AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
var
  LFlags: Cardinal;
  LCACerts: Pmbedtls_x509_crt;
  LBuf: array[0..1023] of AnsiChar;
begin
  FillChar(AResult, SizeOf(AResult), 0);
  Result := False;

  if FX509Crt = nil then
  begin
    AResult.ErrorMessage := 'Certificate not loaded';
    Exit;
  end;

  if ACAStore = nil then
  begin
    AResult.ErrorMessage := 'CA store is nil';
    Exit;
  end;

  if not Assigned(mbedtls_x509_crt_verify) then
  begin
    AResult.ErrorMessage := 'mbedtls_x509_crt_verify not available';
    Exit;
  end;

  LCACerts := Pmbedtls_x509_crt(ACAStore.GetNativeHandle);
  LFlags := 0;

  if mbedtls_x509_crt_verify(FX509Crt, LCACerts, nil, nil, @LFlags, nil, nil) = 0 then
  begin
    AResult.Success := True;
    Result := True;
  end
  else
  begin
    AResult.Success := False;
    AResult.ErrorCode := Integer(LFlags);
    // Get verification error info
    if Assigned(mbedtls_x509_crt_verify_info) then
    begin
      FillChar(LBuf, SizeOf(LBuf), 0);
      mbedtls_x509_crt_verify_info(@LBuf[0], SizeOf(LBuf), '', LFlags);
      AResult.ErrorMessage := string(LBuf);
    end;
  end;
end;

function TMbedTLSCertificate.VerifyHostname(const AHostname: string): Boolean;
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

  if (FX509Crt = nil) or (AHostname = '') then
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
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo: string;
  LLines: TStringList;
  I: Integer;
  LInSAN: Boolean;
  LLine, LLabel, LValue: string;
  LColonPos: Integer;
  LSANs: array of string;

  procedure AddSAN(const AValue: string);
  begin
    if AValue = '' then
      Exit;
    SetLength(LSANs, Length(LSANs) + 1);
    LSANs[High(LSANs)] := AValue;
  end;

begin
  SetLength(Result, 0);
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  SetLength(LSANs, 0);

  LLines := TStringList.Create;
  try
    LLines.Text := LInfo;
    LInSAN := False;

    for I := 0 to LLines.Count - 1 do
    begin
      LLine := LLines[I];

      if not LInSAN then
      begin
        if Pos('subject alt name', LowerCase(LLine)) > 0 then
          LInSAN := True;
        Continue;
      end;

      // Stop when leaving SAN section
      if Trim(LLine) = '' then
        Break;
      if (Length(LLine) > 0) and not (LLine[1] in [' ', #9]) then
        Break;

      LLine := Trim(LLine);
      LColonPos := Pos(':', LLine);
      if LColonPos <= 0 then
        Continue;

      LLabel := Trim(Copy(LLine, 1, LColonPos - 1));
      LValue := Trim(Copy(LLine, LColonPos + 1, MaxInt));

      if SameText(LLabel, 'dNSName') or SameText(LLabel, 'DNSName') or SameText(LLabel, 'DNS') then
        AddSAN(LValue)
      else if SameText(LLabel, 'iPAddress') or SameText(LLabel, 'IPAddress') or SameText(LLabel, 'IP Address') then
        AddSAN(LValue);
    end;
  finally
    LLines.Free;
  end;

  Result := LSANs;
end;

function TMbedTLSCertificate.GetKeyUsage: TSSLStringArray;
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo, LUsage: string;
  LPos, LEndPos: Integer;
  LUsages: array of string;
begin
  SetLength(Result, 0);
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  SetLength(LUsages, 0);

  // 查找 "key usage" 部分
  LPos := Pos('key usage', LowerCase(LInfo));
  if LPos > 0 then
  begin
    LPos := LPos + 9;
    while (LPos <= Length(LInfo)) and (LInfo[LPos] in [' ', #9, ':']) do
      Inc(LPos);

    // 读取到行尾
    LEndPos := LPos;
    while (LEndPos <= Length(LInfo)) and not (LInfo[LEndPos] in [#10, #13]) do
      Inc(LEndPos);

    LUsage := Trim(Copy(LInfo, LPos, LEndPos - LPos));

    // 按逗号分割
    while LUsage <> '' do
    begin
      LPos := Pos(',', LUsage);
      if LPos > 0 then
      begin
        SetLength(LUsages, Length(LUsages) + 1);
        LUsages[High(LUsages)] := Trim(Copy(LUsage, 1, LPos - 1));
        LUsage := Trim(Copy(LUsage, LPos + 1, Length(LUsage)));
      end
      else
      begin
        SetLength(LUsages, Length(LUsages) + 1);
        LUsages[High(LUsages)] := Trim(LUsage);
        Break;
      end;
    end;
  end;

  Result := LUsages;
end;

function TMbedTLSCertificate.GetExtendedKeyUsage: TSSLStringArray;
var
  LBuf: array[0..4095] of AnsiChar;
  LLen: Integer;
  LInfo, LUsage: string;
  LPos, LEndPos: Integer;
  LUsages: array of string;
begin
  SetLength(Result, 0);
  if FX509Crt = nil then Exit;
  if not Assigned(mbedtls_x509_crt_info) then Exit;

  FillChar(LBuf, SizeOf(LBuf), 0);
  LLen := mbedtls_x509_crt_info(@LBuf[0], SizeOf(LBuf), '', FX509Crt);
  if LLen <= 0 then Exit;

  LInfo := string(LBuf);
  SetLength(LUsages, 0);

  // 查找 "ext key usage" 或 "extended key usage" 部分
  LPos := Pos('ext key usage', LowerCase(LInfo));
  if LPos = 0 then
    LPos := Pos('extended key usage', LowerCase(LInfo));

  if LPos > 0 then
  begin
    // 跳过标签
    while (LPos <= Length(LInfo)) and (LInfo[LPos] <> ':') do
      Inc(LPos);
    Inc(LPos);  // 跳过冒号
    while (LPos <= Length(LInfo)) and (LInfo[LPos] in [' ', #9]) do
      Inc(LPos);

    // 读取到行尾
    LEndPos := LPos;
    while (LEndPos <= Length(LInfo)) and not (LInfo[LEndPos] in [#10, #13]) do
      Inc(LEndPos);

    LUsage := Trim(Copy(LInfo, LPos, LEndPos - LPos));

    // 按逗号分割
    while LUsage <> '' do
    begin
      LPos := Pos(',', LUsage);
      if LPos > 0 then
      begin
        SetLength(LUsages, Length(LUsages) + 1);
        LUsages[High(LUsages)] := Trim(Copy(LUsage, 1, LPos - 1));
        LUsage := Trim(Copy(LUsage, LPos + 1, Length(LUsage)));
      end
      else
      begin
        SetLength(LUsages, Length(LUsages) + 1);
        LUsages[High(LUsages)] := Trim(LUsage);
        Break;
      end;
    end;
  end;

  Result := LUsages;
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
var
  LMdInfo: Pointer;
  LHash: array[0..19] of Byte;  // SHA1 = 20 bytes
  I: Integer;
begin
  Result := '';
  if FX509Crt = nil then Exit;
  if Length(FDERData) = 0 then Exit;
  if not Assigned(mbedtls_md_info_from_type) then Exit;
  if not Assigned(mbedtls_md) then Exit;

  LMdInfo := mbedtls_md_info_from_type(MBEDTLS_MD_SHA1);
  if LMdInfo = nil then Exit;

  FillChar(LHash, SizeOf(LHash), 0);
  if mbedtls_md(LMdInfo, @FDERData[0], Length(FDERData), @LHash[0]) = 0 then
  begin
    for I := 0 to 19 do
      Result := Result + IntToHex(LHash[I], 2);
  end;
end;

function TMbedTLSCertificate.GetFingerprintSHA256: string;
var
  LMdInfo: Pointer;
  LHash: array[0..31] of Byte;  // SHA256 = 32 bytes
  I: Integer;
begin
  Result := '';
  if FX509Crt = nil then Exit;
  if Length(FDERData) = 0 then Exit;
  if not Assigned(mbedtls_md_info_from_type) then Exit;
  if not Assigned(mbedtls_md) then Exit;

  LMdInfo := mbedtls_md_info_from_type(MBEDTLS_MD_SHA256);
  if LMdInfo = nil then Exit;

  FillChar(LHash, SizeOf(LHash), 0);
  if mbedtls_md(LMdInfo, @FDERData[0], Length(FDERData), @LHash[0]) = 0 then
  begin
    for I := 0 to 31 do
      Result := Result + IntToHex(LHash[I], 2);
  end;
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
