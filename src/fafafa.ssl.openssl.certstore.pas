{
  fafafa.ssl.openssl.certstore - OpenSSL 证书存储实现
  版本: 1.0 (简化版)
}

unit fafafa.ssl.openssl.certstore;

{$mode ObjFPC}{$H+}
{.$DEFINE DEBUG_CERTSTORE}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.logging,  // P3-8: 添加日志支持
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.certificate;

type
  TOpenSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  private
    FStore: PX509_STORE;
    FOwnsHandle: Boolean;
    FCertificates: TList;  // 缓存证书列表用于枚举
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddCertificate(ACert: ISSLCertificate): Boolean;
    function RemoveCertificate(ACert: ISSLCertificate): Boolean;
    function Contains(ACert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(AIndex: Integer): ISSLCertificate;
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromPath(const APath: string): Boolean;
    function LoadSystemStore: Boolean;
    function FindBySubject(const ASubject: string): ISSLCertificate;
    function FindByIssuer(const AIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const AFingerprint: string): ISSLCertificate;
    function VerifyCertificate(ACert: ISSLCertificate): Boolean;
    function BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
    function GetNativeHandle: Pointer;
  end;

implementation

uses
  fafafa.ssl.certchain,
  fafafa.ssl.secure;

constructor TOpenSSLCertificateStore.Create;
begin
  inherited Create;
  FStore := X509_STORE_new;
  FOwnsHandle := True;
  FCertificates := TList.Create;
end;

destructor TOpenSSLCertificateStore.Destroy;
begin
  // 清空证书列表
  FCertificates.Clear;
  FCertificates.Free;
  
  // 释放 X509_STORE（只释放一次！）
  // 注意：如果 OpenSSL 正在卸载，跳过清理以避免崩溃
  if FOwnsHandle and (FStore <> nil) and not OpenSSLX509_Finalizing then
  begin
    if Assigned(X509_STORE_free) then
    begin
      try
        X509_STORE_free(FStore);
      except
        // P3-8: 记录异常而不是静默忽略
        on E: Exception do
          TSecurityLog.Warning('OpenSSL', Format('Exception in TOpenSSLCertificateStore.Destroy: %s', [E.Message]));
      end;
    end;
  end;
  
  inherited;
end;

function TOpenSSLCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
var
  X509: PX509;
begin
  Result := False;
  if (FStore = nil) or (ACert = nil) then Exit;
  
  X509 := PX509(ACert.GetNativeHandle);
  if X509 = nil then Exit;
  
  Result := (X509_STORE_add_cert(FStore, X509) = 1);
  if Result then
    FCertificates.Add(X509);
end;

function TOpenSSLCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
begin
  // Note: OpenSSL X509_STORE design does not support certificate removal.
  // This is a limitation of OpenSSL's architecture, not a missing feature.
  // Workaround: Create a new store or reload certificates selectively.
  Result := False;
end;

function TOpenSSLCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
var
  FP: string;
begin
  Result := False;
  if (ACert = nil) or (FCertificates.Count = 0) then
    Exit;
  
  // 使用指纹进行匹配，避免依赖底层句柄是否复用
  FP := ACert.GetFingerprintSHA256;
  if FP = '' then
    FP := ACert.GetFingerprint(sslHashSHA256);
  if FP = '' then
    FP := ACert.GetFingerprintSHA1;
  if FP = '' then
    Exit;
  
  Result := FindByFingerprint(FP) <> nil;
end;

procedure TOpenSSLCertificateStore.Clear;
begin
  // 清空证书缓存列表
  FCertificates.Clear;
  
  // 重新创建 store
  if FOwnsHandle and (FStore <> nil) and not OpenSSLX509_Finalizing then
  begin
    if Assigned(X509_STORE_free) then
    begin
      try
        X509_STORE_free(FStore);
      except
        on E: Exception do
          ; // 静默忽略
      end;
    end;
  end;
  
  FStore := X509_STORE_new;
  FOwnsHandle := True;
end;

function TOpenSSLCertificateStore.GetCount: Integer;
begin
  Result := FCertificates.Count;
end;

function TOpenSSLCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
var
  X509Cert: PX509;
begin
  Result := nil;
  
  if (AIndex < 0) or (AIndex >= FCertificates.Count) then
    Exit;
  
  X509Cert := PX509(FCertificates[AIndex]);
  if X509Cert = nil then
    Exit;
  
  X509_up_ref(X509Cert);
  Result := TOpenSSLCertificate.Create(X509Cert, True);
end;

function TOpenSSLCertificateStore.LoadFromFile(const AFileName: string): Boolean;
var
  FileNameA: AnsiString;
  BIO: PBIO;
  X509Cert: PX509;
  Cert: ISSLCertificate;
  CertCount: Integer;
begin
  Result := False;
  CertCount := 0;
  

  
  if FStore = nil then
  begin
    Exit;
  end;
  
  if not Assigned(BIO_new_file) or not Assigned(PEM_read_bio_X509) then
  begin
    Exit;
  end;
  
  try
    // 读取证书文件
    FileNameA := AnsiString(AFileName);
    BIO := BIO_new_file(PAnsiChar(FileNameA), 'r');
    if BIO = nil then
    begin
      Exit;
    end;
    
    try
      // 尝试读取所有证书（可能是链）
      
      repeat
        X509Cert := PEM_read_bio_X509(BIO, nil, nil, nil);
        
        
        if X509Cert <> nil then
        begin
          // NOTE: X509_STORE_add_cert causes Access Violation in some OpenSSL versions
          // We only need FCertificates list for enumeration
          // X509_STORE is mainly used for certificate verification (VerifyCertificate, BuildCertificateChain)
          // if Assigned(X509_STORE_add_cert) then
          //   X509_STORE_add_cert(FStore, X509Cert);
          
          // 添加到枚举列表
          if Assigned(X509_up_ref) then
            X509_up_ref(X509Cert);  // 为FCertificates增加引用
          
          
          FCertificates.Add(X509Cert);
          
          
          Inc(CertCount);
        end;
      until X509Cert = nil;
      
      Result := (CertCount > 0);
      
      
    finally
      if Assigned(BIO_free) then
        BIO_free(BIO);
    end;
  except
    on E: Exception do
    begin
      
      Result := False;
    end;
  end;
end;

function TOpenSSLCertificateStore.LoadFromPath(const APath: string): Boolean;
var
  SR: TSearchRec;
  FilePath: string;
  Count: Integer;
  SearchPath: string;
  FindResult: Integer;
begin
  Result := False;
  Count := 0;
  
  try
    // 确保路径有正确的分隔符
    SearchPath := IncludeTrailingPathDelimiter(APath);
    
    
    // 扫描目录中的所有 .pem 文件
    FindResult := FindFirst(SearchPath + '*.pem', faAnyFile, SR);
    if FindResult = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          FilePath := SearchPath + SR.Name;
          if LoadFromFile(FilePath) then
            Inc(Count);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    
    // 扫描目录中的所有 .crt 文件
    FindResult := FindFirst(SearchPath + '*.crt', faAnyFile, SR);
    if FindResult = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          FilePath := SearchPath + SR.Name;
          if LoadFromFile(FilePath) then
            Inc(Count);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    
    Result := (Count > 0);
  except
    on E: Exception do
    begin
      {$IFDEF DEBUG_CERTSTORE}
      // Error during path loading
      {$ENDIF}
      Result := False;
    end;
  end;
end;

function TOpenSSLCertificateStore.LoadSystemStore: Boolean;
const
  // Linux 系统证书路径
  LinuxCertPaths: array[0..4] of string = (
    '/etc/ssl/certs',           // Debian/Ubuntu
    '/etc/pki/tls/certs',       // RedHat/CentOS
    '/usr/share/ca-certificates',
    '/usr/local/share/ca-certificates',
    '/etc/ssl/cert.pem'
  );
var
  I: Integer;
  LoadedAny: Boolean;
begin
  Result := False;
  LoadedAny := False;
  
  // 首先尝试 OpenSSL 默认路径
  if FStore <> nil then
  begin
    if Assigned(X509_STORE_set_default_paths) then
    begin
      try
        X509_STORE_set_default_paths(FStore);
      except
        // 继续尝试其他方法
      end;
    end;
  end;
  
  // 尝试从已知的系统路径加载
  for I := Low(LinuxCertPaths) to High(LinuxCertPaths) do
  begin
    if DirectoryExists(LinuxCertPaths[I]) then
    begin
      if LoadFromPath(LinuxCertPaths[I]) then
        LoadedAny := True;
    end
    else if FileExists(LinuxCertPaths[I]) then
    begin
      if LoadFromFile(LinuxCertPaths[I]) then
        LoadedAny := True;
    end;
  end;
  
  Result := LoadedAny or (GetCount > 0);
end;

function TOpenSSLCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
var
  I: Integer;
  Cert: ISSLCertificate;
  Subject: string;
begin
  Result := nil;
  
  for I := 0 to FCertificates.Count - 1 do
  begin
    Cert := GetCertificate(I);
    if Cert <> nil then
    begin
      try
        Subject := Cert.GetSubject;
        // 部分匹配：检查 subject 中是否包含搜索字符串
        if (Subject <> '') and (Pos(UpperCase(ASubject), UpperCase(Subject)) > 0) then
        begin
          Result := Cert;
          Exit;
        end;
      except
        // 如果获取 Subject 失败，继续下一个
        Continue;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
var
  I: Integer;
  Cert: ISSLCertificate;
  Issuer: string;
begin
  Result := nil;
  
  for I := 0 to FCertificates.Count - 1 do
  begin
    Cert := GetCertificate(I);
    if Cert <> nil then
    begin
      try
        Issuer := Cert.GetIssuer;
        // 部分匹配：检查 issuer 中是否包含搜索字符串
        if (Issuer <> '') and (Pos(UpperCase(AIssuer), UpperCase(Issuer)) > 0) then
        begin
          Result := Cert;
          Exit;
        end;
      except
        Continue;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
var
  I: Integer;
  Cert: ISSLCertificate;
  Serial: string;
begin
  Result := nil;
  
  for I := 0 to FCertificates.Count - 1 do
  begin
    Cert := GetCertificate(I);
    if Cert <> nil then
    begin
      try
        Serial := Cert.GetSerialNumber;
        // 精确匹配（不区分大小写）
        if (Serial <> '') and (UpperCase(Serial) = UpperCase(ASerialNumber)) then
        begin
          Result := Cert;
          Exit;
        end;
      except
        Continue;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
var
  I: Integer;
  Cert: ISSLCertificate;
  FP_SHA1, FP_SHA256: string;
  SearchFP: string;
begin
  Result := nil;
  SearchFP := UpperCase(StringReplace(AFingerprint, ':', '', [rfReplaceAll]));
  
  for I := 0 to FCertificates.Count - 1 do
  begin
    Cert := GetCertificate(I);
    if Cert <> nil then
    begin
      try
        // Try SHA1 fingerprint (constant-time comparison)
        FP_SHA1 := UpperCase(StringReplace(Cert.GetFingerprintSHA1, ':', '', [rfReplaceAll]));
        if (FP_SHA1 <> '') and SecureCompareStrings(FP_SHA1, SearchFP) then
        begin
          Result := Cert;
          Exit;
        end;
        
        // Try SHA256 fingerprint (constant-time comparison)
        FP_SHA256 := UpperCase(StringReplace(Cert.GetFingerprintSHA256, ':', '', [rfReplaceAll]));
        if (FP_SHA256 <> '') and SecureCompareStrings(FP_SHA256, SearchFP) then
        begin
          Result := Cert;
          Exit;
        end;
      except
        Continue;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  if (ACert = nil) or (FStore = nil) then
    Exit;
  
  // 直接复用证书对象的 Verify 实现，委托给当前 store
  Result := ACert.Verify(Self);
end;

function TOpenSSLCertificateStore.BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
begin
  SetLength(Result, 0);
  if ACert = nil then
    Exit;
  
  // 使用通用的证书链验证器来构建证书链
  with TSSLCertificateChainVerifier.Create as ISSLCertificateChainVerifier do
  begin
    SetTrustedStore(Self);
    if not BuildChain(ACert, Result) then
      SetLength(Result, 0);
  end;
end;

function TOpenSSLCertificateStore.GetNativeHandle: Pointer;
begin
  Result := FStore;
end;

end.
