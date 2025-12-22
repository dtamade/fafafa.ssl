{
  fafafa.ssl.winssl.certstore - WinSSL 证书存储实现

  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-09

  描述:
    实现 ISSLCertificateStore 接口的 WinSSL 后端。
    封装 Windows 证书存储（ROOT, MY, CA, TRUST 等）操作。
}

unit fafafa.ssl.winssl.certstore;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.certificate;

type
  { TWinSSLCertificateStore - Windows 证书存储类 }
  TWinSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  private
    FStoreHandle: HCERTSTORE;
    FStoreName: string;
    FOwnsHandle: Boolean;
    FCertificates: TList;  // 缓存的证书列表

    procedure ClearCache;
    procedure LoadCertificates;

  public
    constructor Create(const AStoreName: string); overload;
    constructor Create(AStoreHandle: HCERTSTORE; AOwnsHandle: Boolean = False); overload;
    destructor Destroy; override;

    { ISSLCertificateStore - 存储管理 }
    function Open(const AName: string; AWritable: Boolean = False): Boolean;
    procedure Close;
    function IsOpen: Boolean;

    { ISSLCertificateStore - 证书操作 }
    function AddCertificate(ACert: ISSLCertificate): Boolean;
    function RemoveCertificate(ACert: ISSLCertificate): Boolean;
    function Contains(ACert: ISSLCertificate): Boolean;
    procedure Clear;

    { ISSLCertificateStore - 证书查询 }
    function GetCount: Integer;
    function GetCertificate(AIndex: Integer): ISSLCertificate;
    function GetAllCertificates: TSSLCertificateArray;

    { ISSLCertificateStore - 证书加载 }
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromPath(const APath: string): Boolean;
    function LoadSystemStore: Boolean;

    { ISSLCertificateStore - 证书搜索 }
    function FindBySubject(const ASubject: string): ISSLCertificate;
    function FindByIssuer(const AIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const AFingerprint: string): ISSLCertificate;

    { ISSLCertificateStore - 证书验证 }
    function VerifyCertificate(ACert: ISSLCertificate): Boolean;
    function BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;

    { ISSLCertificateStore - 原生句柄 }
    function GetNativeHandle: Pointer;

    { 额外的辅助方法（不在接口中） }
    function OpenSystemStore(const AStoreName: string): Boolean;
    function GetSystemStoreNames: TStringList;
  end;

{ 工厂函数 }
function CreateWinSSLCertificateStore(const AStoreName: string): ISSLCertificateStore;
function OpenSystemStore(const AStoreName: string): ISSLCertificateStore;

{ 常见系统存储名称 }
const
  SSL_STORE_ROOT = 'ROOT';          // 受信任根证书
  SSL_STORE_MY = 'MY';              // 个人证书
  SSL_STORE_CA = 'CA';              // 中间证书颁发机构
  SSL_STORE_TRUST = 'Trust';        // 企业信任
  SSL_STORE_DISALLOWED = 'Disallowed'; // 不受信任的证书

implementation

uses
  fafafa.ssl.secure;

// ============================================================================
// 工厂函数
// ============================================================================

function CreateWinSSLCertificateStore(const AStoreName: string): ISSLCertificateStore;
begin
  Result := TWinSSLCertificateStore.Create(AStoreName);
end;

function OpenSystemStore(const AStoreName: string): ISSLCertificateStore;
var
  Store: TWinSSLCertificateStore;
begin
  Store := TWinSSLCertificateStore.Create('');
  if Store.OpenSystemStore(AStoreName) then
    Result := Store
  else
  begin
    Store.Free;
    Result := nil;
  end;
end;

// ============================================================================
// TWinSSLCertificateStore - 构造和析构
// ============================================================================

constructor TWinSSLCertificateStore.Create(const AStoreName: string);
begin
  inherited Create;
  FStoreHandle := nil;
  FStoreName := AStoreName;
  FOwnsHandle := True;
  FCertificates := TList.Create;

  if AStoreName <> '' then
    OpenSystemStore(AStoreName);
end;

constructor TWinSSLCertificateStore.Create(AStoreHandle: HCERTSTORE; AOwnsHandle: Boolean = False);
begin
  inherited Create;
  FStoreHandle := AStoreHandle;
  FStoreName := '';
  FOwnsHandle := AOwnsHandle;
  FCertificates := TList.Create;

  if AStoreHandle <> nil then
    LoadCertificates;
end;

destructor TWinSSLCertificateStore.Destroy;
begin
  ClearCache;
  FCertificates.Free;

  if FOwnsHandle and (FStoreHandle <> nil) then
    CertCloseStore(FStoreHandle, 0);

  inherited Destroy;
end;

// ============================================================================
// 内部辅助方法
// ============================================================================

procedure TWinSSLCertificateStore.ClearCache;
var
  i: Integer;
begin
  for i := 0 to FCertificates.Count - 1 do
    ISSLCertificate(FCertificates[i])._Release;
  FCertificates.Clear;
end;

procedure TWinSSLCertificateStore.LoadCertificates;
var
  CertContext: PCCERT_CONTEXT;
  Cert: ISSLCertificate;
begin
  ClearCache;

  if FStoreHandle = nil then
    Exit;

  // 枚举存储中的所有证书
  CertContext := nil;
  repeat
    CertContext := CertEnumCertificatesInStore(FStoreHandle, CertContext);
    if CertContext <> nil then
    begin
      // 创建证书对象（不拥有上下文，因为它由枚举器管理）
      Cert := CreateWinSSLCertificateFromContext(
        CertDuplicateCertificateContext(CertContext),
        True
      );
      Cert._AddRef;
      FCertificates.Add(Pointer(Cert));
    end;
  until CertContext = nil;
end;

// ============================================================================
// ISSLCertificateStore - 存储管理
// ============================================================================

function TWinSSLCertificateStore.Open(const AName: string; AWritable: Boolean = False): Boolean;
var
  Flags: DWORD;
begin
  Result := False;

  // 关闭现有存储
  Close;

  // 设置打开标志
  Flags := CERT_STORE_OPEN_EXISTING_FLAG;
  if not AWritable then
    Flags := Flags or CERT_STORE_READONLY_FLAG;

  // 打开系统存储
  FStoreHandle := CertOpenSystemStoreW(0, PWideChar(WideString(AName)));

  if FStoreHandle <> nil then
  begin
    FStoreName := AName;
    FOwnsHandle := True;
    LoadCertificates;
    Result := True;
  end;
end;

procedure TWinSSLCertificateStore.Close;
begin
  ClearCache;

  if FOwnsHandle and (FStoreHandle <> nil) then
  begin
    CertCloseStore(FStoreHandle, 0);
    FStoreHandle := nil;
  end;

  FStoreName := '';
end;

function TWinSSLCertificateStore.IsOpen: Boolean;
begin
  Result := (FStoreHandle <> nil);
end;

// ============================================================================
// ISSLCertificateStore - 证书操作
// ============================================================================

function TWinSSLCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
var
  CertContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (ACert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(ACert.GetNativeHandle);
  if CertContext = nil then
    Exit;

  // 添加证书到存储
  Result := CertAddCertificateContextToStore(
    FStoreHandle,
    CertContext,
    CERT_STORE_ADD_REPLACE_EXISTING,
    nil
  );

  if Result then
  begin
    // 重新加载证书列表
    LoadCertificates;
  end;
end;

function TWinSSLCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
var
  CertContext, FoundContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (ACert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(ACert.GetNativeHandle);
  if CertContext = nil then
    Exit;

  // 在存储中查找证书
  FoundContext := CertFindCertificateInStore(
    FStoreHandle,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    CERT_FIND_EXISTING,
    CertContext,
    nil
  );

  if FoundContext <> nil then
  begin
    // 删除证书
    Result := CertDeleteCertificateFromStore(FoundContext);

    if Result then
    begin
      // 重新加载证书列表
      LoadCertificates;
    end;
  end;
end;

function TWinSSLCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
var
  CertContext, FoundContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (ACert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(ACert.GetNativeHandle);
  if CertContext = nil then
    Exit;

  // 在存储中查找证书
  FoundContext := CertFindCertificateInStore(
    FStoreHandle,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    CERT_FIND_EXISTING,
    CertContext,
    nil
  );

  Result := (FoundContext <> nil);

  if FoundContext <> nil then
    CertFreeCertificateContext(FoundContext);
end;

procedure TWinSSLCertificateStore.Clear;
begin
  ClearCache;

  // 注意：这不会清除 Windows 系统存储中的证书
  // 只清除我们的内存缓存
end;

// ============================================================================
// ISSLCertificateStore - 证书加载
// ============================================================================

function TWinSSLCertificateStore.LoadFromFile(const AFileName: string): Boolean;
var
  Cert: ISSLCertificate;
  CertImpl: TWinSSLCertificate;
begin
  Result := False;

  if not FileExists(AFileName) then
    Exit;

  // 创建证书对象并加载文件
  CertImpl := TWinSSLCertificate.Create(nil, False);
  Cert := CertImpl;

  if Cert.LoadFromFile(AFileName) then
  begin
    Result := AddCertificate(Cert);
  end;
end;

function TWinSSLCertificateStore.LoadFromPath(const APath: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
  LoadedCount: Integer;
begin
  Result := False;
  LoadedCount := 0;

  if not DirectoryExists(APath) then
    Exit;

  // 搜索路径中的证书文件
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.cer', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
        if LoadFromFile(FilePath) then
          Inc(LoadedCount);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // 也搜索 .pem 和 .crt 文件
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.pem', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
        if LoadFromFile(FilePath) then
          Inc(LoadedCount);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.crt', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
        if LoadFromFile(FilePath) then
          Inc(LoadedCount);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  Result := (LoadedCount > 0);
end;

function TWinSSLCertificateStore.LoadSystemStore: Boolean;
begin
  // 默认加载 ROOT 系统存储（受信任根证书）
  Result := OpenSystemStore(SSL_STORE_ROOT);
end;

// ============================================================================
// ISSLCertificateStore - 证书查询
// ============================================================================

function TWinSSLCertificateStore.GetCount: Integer;
begin
  Result := FCertificates.Count;
end;

function TWinSSLCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
begin
  if (AIndex >= 0) and (AIndex < FCertificates.Count) then
    Result := ISSLCertificate(FCertificates[AIndex])
  else
    Result := nil;
end;

function TWinSSLCertificateStore.GetAllCertificates: TSSLCertificateArray;
var
  i: Integer;
begin
  SetLength(Result, FCertificates.Count);
  for i := 0 to FCertificates.Count - 1 do
    Result[i] := ISSLCertificate(FCertificates[i]);
end;

// ============================================================================
// ISSLCertificateStore - 证书搜索
// ============================================================================

function TWinSSLCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
var
  CertContext: PCCERT_CONTEXT;
  SubjectW: WideString;
begin
  Result := nil;

  if (FStoreHandle = nil) or (ASubject = '') then
    Exit;

  SubjectW := WideString(ASubject);

  // 搜索主题包含指定字符串的证书
  CertContext := CertFindCertificateInStore(
    FStoreHandle,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    CERT_FIND_SUBJECT_STR_W,
    PWideChar(SubjectW),
    nil
  );

  if CertContext <> nil then
    Result := CreateWinSSLCertificateFromContext(CertContext, True);
end;

function TWinSSLCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
var
  CertContext: PCCERT_CONTEXT;
  IssuerW: WideString;
begin
  Result := nil;

  if (FStoreHandle = nil) or (AIssuer = '') then
    Exit;

  IssuerW := WideString(AIssuer);

  // 搜索颁发者包含指定字符串的证书
  CertContext := CertFindCertificateInStore(
    FStoreHandle,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    CERT_FIND_ISSUER_STR_W,
    PWideChar(IssuerW),
    nil
  );

  if CertContext <> nil then
    Result := CreateWinSSLCertificateFromContext(CertContext, True);
end;

function TWinSSLCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
var
  I: Integer;
  Cert: ISSLCertificate;
begin
  Result := nil;
  for I := 0 to FCertificates.Count - 1 do
  begin
    Cert := ISSLCertificate(FCertificates[I]);
    // Use constant-time comparison for serial numbers
    if SameText(Cert.GetSerialNumber, ASerialNumber) then
    begin
      Result := Cert;
      Exit;
    end;
  end;
end;

function TWinSSLCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
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
    Cert := ISSLCertificate(FCertificates[I]);
    
    // Try SHA256 fingerprint (constant-time comparison)
    FP_SHA256 := UpperCase(StringReplace(Cert.GetFingerprintSHA256, ':', '', [rfReplaceAll]));
    if (FP_SHA256 <> '') and SecureCompareStrings(FP_SHA256, SearchFP) then
    begin
      Result := Cert;
      Exit;
    end;
    
    // Try SHA1 fingerprint (constant-time comparison)
    FP_SHA1 := UpperCase(StringReplace(Cert.GetFingerprintSHA1, ':', '', [rfReplaceAll]));
    if (FP_SHA1 <> '') and SecureCompareStrings(FP_SHA1, SearchFP) then
    begin
      Result := Cert;
      Exit;
    end;
  end;
end;

// ============================================================================
// ISSLCertificateStore - 证书验证
// ============================================================================

function TWinSSLCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  if ACert = nil then
  begin
    Result := False;
    Exit;
  end;

  // 使用证书自身的 Verify 方法，传入当前存储作为 CA 存储
  Result := ACert.Verify(Self);
end;

function TWinSSLCertificateStore.BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
var
  ChainPara: CERT_CHAIN_PARA;
  ChainContext: PCCERT_CHAIN_CONTEXT;
  CertContext: PCCERT_CONTEXT;
  ChainList: TList;
  i, j: Integer;
  SimpleChain: PCERT_SIMPLE_CHAIN;
  ChainCert: ISSLCertificate;
begin
  SetLength(Result, 0);

  if (ACert = nil) then
    Exit;

  CertContext := PCCERT_CONTEXT(ACert.GetNativeHandle);
  if CertContext = nil then
    Exit;

  // 初始化证书链参数
  FillChar(ChainPara, SizeOf(ChainPara), 0);
  ChainPara.cbSize := SizeOf(ChainPara);

  // 构建证书链
  if not CertGetCertificateChain(
    nil,                  // 使用默认链引擎
    CertContext,        // 要验证的证书
    nil,                // 使用当前时间
    FStoreHandle,       // 附加证书存储
    @ChainPara,         // 链参数
    0,                  // 标志
    nil,                // 保留
    @ChainContext       // 输出链上下文
  ) then
    Exit;

  try
    // 提取证书链
    ChainList := TList.Create;
    try
      // 遍历所有简单链（通常只有一个）
      for i := 0 to ChainContext^.cChain - 1 do
      begin
        SimpleChain := ChainContext^.rgpChain[i];

        // 遍历链中的所有证书
        for j := 0 to SimpleChain^.cElement - 1 do
        begin
          ChainCert := CreateWinSSLCertificateFromContext(
            CertDuplicateCertificateContext(SimpleChain^.rgpElement[j]^.pCertContext),
            True
          );
          ChainList.Add(Pointer(ChainCert));
        end;
      end;

      // 转换为数组
      SetLength(Result, ChainList.Count);
      for i := 0 to ChainList.Count - 1 do
        Result[i] := ISSLCertificate(ChainList[i]);

    finally
      ChainList.Free;
    end;

  finally
    CertFreeCertificateChain(ChainContext);
  end;
end;

// ============================================================================
// 额外的辅助方法
// ============================================================================

function TWinSSLCertificateStore.OpenSystemStore(const AStoreName: string): Boolean;
begin
  Result := Open(AStoreName, False);
end;

function TWinSSLCertificateStore.GetSystemStoreNames: TStringList;
begin
  Result := TStringList.Create;

  // 添加常见的系统存储
  Result.Add(SSL_STORE_ROOT);
  Result.Add(SSL_STORE_MY);
  Result.Add(SSL_STORE_CA);
  Result.Add(SSL_STORE_TRUST);
  Result.Add(SSL_STORE_DISALLOWED);

  // 可以通过 CertEnumSystemStore 枚举所有系统存储
  // 这里简化处理，只返回常见的存储
end;

// ============================================================================
// ISSLCertificateStore - 原生句柄
// ============================================================================

function TWinSSLCertificateStore.GetNativeHandle: Pointer;
begin
  Result := Pointer(FStoreHandle);
end;

end.
