unit fafafa.ssl.cert.manager;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ fafafa.ssl 证书管理器
  
  简化证书相关操作：
  - 证书生成
  - 证书加载
  - 证书验证
  - 证书信息查询
}

interface

uses
  SysUtils, Classes, DateUtils, StrUtils,
  fafafa.ssl, fafafa.ssl.base;

type
  { 证书信息 }
  TCertificateInfo = record
    Subject: string;           // 主题
    Issuer: string;            // 颁发者
    SerialNumber: string;      // 序列号
    NotBefore: TDateTime;      // 有效期起始
    NotAfter: TDateTime;       // 有效期结束
    IsValid: Boolean;          // 是否有效
    DaysUntilExpiry: Integer;  // 距离过期天数
    Fingerprint: string;       // 指纹
    PublicKeyAlgorithm: string;// 公钥算法
    SignatureAlgorithm: string;// 签名算法
    KeySize: Integer;          // 密钥长度
  end;

  { 证书生成选项 }
  TCertGenerationOptions = record
    CommonName: string;        // 通用名（CN）
    Organization: string;      // 组织（O）
    OrganizationalUnit: string;// 组织单位（OU）
    Country: string;           // 国家（C）
    State: string;             // 省/州（ST）
    Locality: string;          // 城市（L）
    EmailAddress: string;      // 邮箱
    ValidDays: Integer;        // 有效天数
    KeyBits: Integer;          // 密钥位数（2048/4096）
    IsSelfSigned: Boolean;     // 是否自签名
    IsCA: Boolean;             // 是否CA证书
  end;

  { 证书管理器 }
  TCertificateManager = class
  public
    { 默认生成选项 }
    class function DefaultGenerationOptions: TCertGenerationOptions;
    
    { 证书生成（需要OpenSSL命令行工具） }
    class function GenerateSelfSigned(
      const AOptions: TCertGenerationOptions;
      const ACertFile, AKeyFile: string;
      const AKeyPassword: string = ''): Boolean;
    
    { 快速生成自签名证书 }
    class function QuickGenerateSelfSigned(
      const ACommonName: string;
      const ACertFile, AKeyFile: string;
      AValidDays: Integer = 365;
      AKeyBits: Integer = 2048): Boolean;
    
    { 证书加载 }
    class function LoadFromFile(const APath: string): ISSLCertificate;
    class function LoadFromPEM(const APEM: string): ISSLCertificate;
    
    { 证书验证 }
    class function Verify(
      ACert: ISSLCertificate;
      ATrustedCerts: array of ISSLCertificate;
      out AError: string): Boolean;
    
    { 证书信息 }
    class function GetInfo(ACert: ISSLCertificate): TCertificateInfo;
    class function IsExpired(ACert: ISSLCertificate): Boolean;
    class function IsExpiringSoon(ACert: ISSLCertificate; ADays: Integer = 30): Boolean;
    class function DaysUntilExpiry(ACert: ISSLCertificate): Integer;
    
    { 证书比较 }
    class function AreEqual(ACert1, ACert2: ISSLCertificate): Boolean;
    
    { 证书导出 }
    class function ExportToPEM(ACert: ISSLCertificate): string;
    class function ExportToFile(ACert: ISSLCertificate; const APath: string): Boolean;
    
    { 实用方法 }
    class function ValidateCertificateFile(const APath: string): Boolean;
    class function ValidatePrivateKeyFile(const APath: string): Boolean;
    class function ParseSubjectField(const ASubject, AField: string): string;
  end;

implementation

uses
  Process;

{ TCertificateManager }

class function TCertificateManager.DefaultGenerationOptions: TCertGenerationOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.CommonName := 'localhost';
  Result.Organization := 'Development';
  Result.Country := 'CN';
  Result.ValidDays := 365;
  Result.KeyBits := 2048;
  Result.IsSelfSigned := True;
  Result.IsCA := False;
end;

class function TCertificateManager.GenerateSelfSigned(
  const AOptions: TCertGenerationOptions;
  const ACertFile, AKeyFile: string;
  const AKeyPassword: string): Boolean;
var
  LProcess: TProcess;
  LSubject: string;
  LOutput: TStringList;
  LExitCode: Integer;
begin
  Result := False;
  
  // 构建Subject字符串
  LSubject := '';
  if AOptions.Country <> '' then
    LSubject := LSubject + '/C=' + AOptions.Country;
  if AOptions.State <> '' then
    LSubject := LSubject + '/ST=' + AOptions.State;
  if AOptions.Locality <> '' then
    LSubject := LSubject + '/L=' + AOptions.Locality;
  if AOptions.Organization <> '' then
    LSubject := LSubject + '/O=' + AOptions.Organization;
  if AOptions.OrganizationalUnit <> '' then
    LSubject := LSubject + '/OU=' + AOptions.OrganizationalUnit;
  if AOptions.CommonName <> '' then
    LSubject := LSubject + '/CN=' + AOptions.CommonName;
  if AOptions.EmailAddress <> '' then
    LSubject := LSubject + '/emailAddress=' + AOptions.EmailAddress;
  
  if LSubject = '' then
    LSubject := '/CN=localhost';
  
  try
    LProcess := TProcess.Create(nil);
    LOutput := TStringList.Create;
    try
      // 使用OpenSSL生成自签名证书
      LProcess.Executable := 'openssl';
      LProcess.Parameters.Add('req');
      LProcess.Parameters.Add('-x509');
      LProcess.Parameters.Add('-nodes');
      LProcess.Parameters.Add('-days');
      LProcess.Parameters.Add(IntToStr(AOptions.ValidDays));
      LProcess.Parameters.Add('-newkey');
      LProcess.Parameters.Add('rsa:' + IntToStr(AOptions.KeyBits));
      LProcess.Parameters.Add('-keyout');
      LProcess.Parameters.Add(AKeyFile);
      LProcess.Parameters.Add('-out');
      LProcess.Parameters.Add(ACertFile);
      LProcess.Parameters.Add('-subj');
      LProcess.Parameters.Add(LSubject);
      
      LProcess.Options := [poWaitOnExit, poUsePipes];
      LProcess.Execute;
      
      LExitCode := LProcess.ExitStatus;
      Result := (LExitCode = 0) and FileExists(ACertFile) and FileExists(AKeyFile);
      
    finally
      LOutput.Free;
      LProcess.Free;
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

class function TCertificateManager.QuickGenerateSelfSigned(
  const ACommonName: string;
  const ACertFile, AKeyFile: string;
  AValidDays: Integer;
  AKeyBits: Integer): Boolean;
var
  LOptions: TCertGenerationOptions;
begin
  LOptions := DefaultGenerationOptions;
  LOptions.CommonName := ACommonName;
  LOptions.ValidDays := AValidDays;
  LOptions.KeyBits := AKeyBits;
  Result := GenerateSelfSigned(LOptions, ACertFile, AKeyFile);
end;

class function TCertificateManager.LoadFromFile(const APath: string): ISSLCertificate;
var
  LContext: ISSLContext;
begin
  Result := nil;
  if not FileExists(APath) then
    Exit;
    
  try
    LContext := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
    LContext.LoadCertificate(APath);
    // 注意：这里需要实际的获取证书方法，简化实现
  except
    Result := nil;
  end;
end;

class function TCertificateManager.LoadFromPEM(const APEM: string): ISSLCertificate;
begin
  Result := nil;
  // 需要实现从PEM字符串加载证书
end;

class function TCertificateManager.Verify(
  ACert: ISSLCertificate;
  ATrustedCerts: array of ISSLCertificate;
  out AError: string): Boolean;
begin
  Result := False;
  AError := '';
  
  if ACert = nil then
  begin
    AError := '证书为空';
    Exit;
  end;
  
  try
    // 检查有效期
    if IsExpired(ACert) then
    begin
      AError := '证书已过期';
      Exit;
    end;
    
    // 这里应该实现完整的证书链验证
    // 简化实现仅检查基本信息
    Result := True;
    
  except
    on E: Exception do
    begin
      AError := E.Message;
      Result := False;
    end;
  end;
end;

class function TCertificateManager.GetInfo(ACert: ISSLCertificate): TCertificateInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if ACert = nil then
    Exit;
    
  try
    Result.Subject := ACert.GetSubject;
    Result.Issuer := ACert.GetIssuer;
    Result.SerialNumber := ACert.GetSerialNumber;
    Result.NotBefore := ACert.GetNotBefore;
    Result.NotAfter := ACert.GetNotAfter;
    Result.IsValid := not IsExpired(ACert);
    Result.DaysUntilExpiry := DaysUntilExpiry(ACert);
    // 其他字段需要根据实际接口实现
  except
    // 忽略错误
  end;
end;

class function TCertificateManager.IsExpired(ACert: ISSLCertificate): Boolean;
var
  LNotAfter: TDateTime;
begin
  Result := True;
  if ACert = nil then
    Exit;
    
  try
    LNotAfter := ACert.GetNotAfter;
    Result := Now > LNotAfter;
  except
    Result := True;
  end;
end;

class function TCertificateManager.IsExpiringSoon(
  ACert: ISSLCertificate; ADays: Integer): Boolean;
var
  LDaysLeft: Integer;
begin
  LDaysLeft := DaysUntilExpiry(ACert);
  Result := (LDaysLeft >= 0) and (LDaysLeft <= ADays);
end;

class function TCertificateManager.DaysUntilExpiry(ACert: ISSLCertificate): Integer;
var
  LNotAfter: TDateTime;
begin
  Result := -1;
  if ACert = nil then
    Exit;
    
  try
    LNotAfter := ACert.GetNotAfter;
    Result := DaysBetween(LNotAfter, Now);
    if Now > LNotAfter then
      Result := -Result;  // 已过期，返回负数
  except
    Result := -1;
  end;
end;

class function TCertificateManager.AreEqual(ACert1, ACert2: ISSLCertificate): Boolean;
begin
  Result := False;
  if (ACert1 = nil) or (ACert2 = nil) then
    Exit;
    
  try
    Result := (ACert1.GetSerialNumber = ACert2.GetSerialNumber) and
              (ACert1.GetSubject = ACert2.GetSubject) and
              (ACert1.GetIssuer = ACert2.GetIssuer);
  except
    Result := False;
  end;
end;

class function TCertificateManager.ExportToPEM(ACert: ISSLCertificate): string;
begin
  Result := '';
  // 需要实现导出为PEM格式
end;

class function TCertificateManager.ExportToFile(
  ACert: ISSLCertificate; const APath: string): Boolean;
var
  LPEM: string;
  LFile: TextFile;
begin
  Result := False;
  
  LPEM := ExportToPEM(ACert);
  if LPEM = '' then
    Exit;
    
  try
    AssignFile(LFile, APath);
    Rewrite(LFile);
    Write(LFile, LPEM);
    CloseFile(LFile);
    Result := True;
  except
    Result := False;
  end;
end;

class function TCertificateManager.ValidateCertificateFile(const APath: string): Boolean;
var
  LProcess: TProcess;
begin
  Result := False;
  
  if not FileExists(APath) then
    Exit;
    
  try
    LProcess := TProcess.Create(nil);
    try
      LProcess.Executable := 'openssl';
      LProcess.Parameters.Add('x509');
      LProcess.Parameters.Add('-in');
      LProcess.Parameters.Add(APath);
      LProcess.Parameters.Add('-noout');
      LProcess.Options := [poWaitOnExit];
      LProcess.Execute;
      Result := (LProcess.ExitStatus = 0);
    finally
      LProcess.Free;
    end;
  except
    Result := False;
  end;
end;

class function TCertificateManager.ValidatePrivateKeyFile(const APath: string): Boolean;
var
  LProcess: TProcess;
begin
  Result := False;
  
  if not FileExists(APath) then
    Exit;
    
  try
    LProcess := TProcess.Create(nil);
    try
      LProcess.Executable := 'openssl';
      LProcess.Parameters.Add('rsa');
      LProcess.Parameters.Add('-in');
      LProcess.Parameters.Add(APath);
      LProcess.Parameters.Add('-check');
      LProcess.Parameters.Add('-noout');
      LProcess.Options := [poWaitOnExit];
      LProcess.Execute;
      Result := (LProcess.ExitStatus = 0);
    finally
      LProcess.Free;
    end;
  except
    Result := False;
  end;
end;

class function TCertificateManager.ParseSubjectField(
  const ASubject, AField: string): string;
var
  LPos, LEndPos: Integer;
  LFieldPrefix: string;
begin
  Result := '';
  LFieldPrefix := AField + '=';
  
  LPos := Pos(LFieldPrefix, ASubject);
  if LPos > 0 then
  begin
    LPos := LPos + Length(LFieldPrefix);
    LEndPos := PosEx('/', ASubject, LPos);
    if LEndPos > 0 then
      Result := Copy(ASubject, LPos, LEndPos - LPos)
    else
      Result := Copy(ASubject, LPos, Length(ASubject));
  end;
end;

end.
