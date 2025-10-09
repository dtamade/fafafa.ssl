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
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.types,
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
    constructor Create(const aStoreName: string); overload;
    constructor Create(aStoreHandle: HCERTSTORE; aOwnsHandle: Boolean = False); overload;
    destructor Destroy; override;

    { ISSLCertificateStore - 存储管理 }
    function Open(const aName: string; aWritable: Boolean = False): Boolean;
    procedure Close;
    function IsOpen: Boolean;

    { ISSLCertificateStore - 证书操作 }
    function AddCertificate(aCert: ISSLCertificate): Boolean;
    function RemoveCertificate(aCert: ISSLCertificate): Boolean;
    function Contains(aCert: ISSLCertificate): Boolean;
    procedure Clear;

    { ISSLCertificateStore - 证书查询 }
    function GetCount: Integer;
    function GetCertificate(aIndex: Integer): ISSLCertificate;
    function GetAllCertificates: TSSLCertificateArray;

    { ISSLCertificateStore - 证书加载 }
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromPath(const aPath: string): Boolean;
    function LoadSystemStore: Boolean;

    { ISSLCertificateStore - 证书搜索 }
    function FindBySubject(const aSubject: string): ISSLCertificate;
    function FindByIssuer(const aIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const aFingerprint: string): ISSLCertificate;

    { ISSLCertificateStore - 证书验证 }
    function VerifyCertificate(aCert: ISSLCertificate): Boolean;
    function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;

    { ISSLCertificateStore - 原生句柄 }
    function GetNativeHandle: Pointer;

    { 额外的辅助方法（不在接口中） }
    function OpenSystemStore(const aStoreName: string): Boolean;
    function GetSystemStoreNames: TStringList;
  end;

{ 工厂函数 }
function CreateWinSSLCertificateStore(const aStoreName: string): ISSLCertificateStore;
function OpenSystemStore(const aStoreName: string): ISSLCertificateStore;

{ 常见系统存储名称 }
const
  SSL_STORE_ROOT = 'ROOT';          // 受信任根证书
  SSL_STORE_MY = 'MY';              // 个人证书
  SSL_STORE_CA = 'CA';              // 中间证书颁发机构
  SSL_STORE_TRUST = 'Trust';        // 企业信任
  SSL_STORE_DISALLOWED = 'Disallowed'; // 不受信任的证书

implementation

// ============================================================================
// 工厂函数
// ============================================================================

function CreateWinSSLCertificateStore(const aStoreName: string): ISSLCertificateStore;
begin
  Result := TWinSSLCertificateStore.Create(aStoreName);
end;

function OpenSystemStore(const aStoreName: string): ISSLCertificateStore;
var
  Store: TWinSSLCertificateStore;
begin
  Store := TWinSSLCertificateStore.Create('');
  if Store.OpenSystemStore(aStoreName) then
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

constructor TWinSSLCertificateStore.Create(const aStoreName: string);
begin
  inherited Create;
  FStoreHandle := nil;
  FStoreName := aStoreName;
  FOwnsHandle := True;
  FCertificates := TList.Create;

  if aStoreName <> '' then
    OpenSystemStore(aStoreName);
end;

constructor TWinSSLCertificateStore.Create(aStoreHandle: HCERTSTORE; aOwnsHandle: Boolean = False);
begin
  inherited Create;
  FStoreHandle := aStoreHandle;
  FStoreName := '';
  FOwnsHandle := aOwnsHandle;
  FCertificates := TList.Create;

  if aStoreHandle <> nil then
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

function TWinSSLCertificateStore.Open(const aName: string; aWritable: Boolean = False): Boolean;
var
  Flags: DWORD;
begin
  Result := False;

  // 关闭现有存储
  Close;

  // 设置打开标志
  Flags := CERT_STORE_OPEN_EXISTING_FLAG;
  if not aWritable then
    Flags := Flags or CERT_STORE_READONLY_FLAG;

  // 打开系统存储
  FStoreHandle := CertOpenSystemStoreW(0, PWideChar(WideString(aName)));

  if FStoreHandle <> nil then
  begin
    FStoreName := aName;
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

function TWinSSLCertificateStore.AddCertificate(aCert: ISSLCertificate): Boolean;
var
  CertContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (aCert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(aCert.GetNativeHandle);
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

function TWinSSLCertificateStore.RemoveCertificate(aCert: ISSLCertificate): Boolean;
var
  CertContext, FoundContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (aCert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(aCert.GetNativeHandle);
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

function TWinSSLCertificateStore.Contains(aCert: ISSLCertificate): Boolean;
var
  CertContext, FoundContext: PCCERT_CONTEXT;
begin
  Result := False;

  if (FStoreHandle = nil) or (aCert = nil) then
    Exit;

  // 获取证书的原生上下文
  CertContext := PCCERT_CONTEXT(aCert.GetNativeHandle);
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

function TWinSSLCertificateStore.LoadFromFile(const aFileName: string): Boolean;
var
  Cert: ISSLCertificate;
  CertImpl: TWinSSLCertificate;
begin
  Result := False;

  if not FileExists(aFileName) then
    Exit;

  // 创建证书对象并加载文件
  CertImpl := TWinSSLCertificate.Create(nil, False);
  Cert := CertImpl;

  if Cert.LoadFromFile(aFileName) then
  begin
    Result := AddCertificate(Cert);
  end;
end;

function TWinSSLCertificateStore.LoadFromPath(const aPath: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
  LoadedCount: Integer;
begin
  Result := False;
  LoadedCount := 0;

  if not DirectoryExists(aPath) then
    Exit;

  // 搜索路径中的证书文件
  if FindFirst(IncludeTrailingPathDelimiter(aPath) + '*.cer', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(aPath) + SearchRec.Name;
        if LoadFromFile(FilePath) then
          Inc(LoadedCount);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // 也搜索 .pem 和 .crt 文件
  if FindFirst(IncludeTrailingPathDelimiter(aPath) + '*.pem', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(aPath) + SearchRec.Name;
        if LoadFromFile(FilePath) then
          Inc(LoadedCount);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  if FindFirst(IncludeTrailingPathDelimiter(aPath) + '*.crt', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FilePath := IncludeTrailingPathDelimiter(aPath) + SearchRec.Name;
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

function TWinSSLCertificateStore.GetCertificate(aIndex: Integer): ISSLCertificate;
begin
  if (aIndex >= 0) and (aIndex < FCertificates.Count) then
    Result := ISSLCertificate(FCertificates[aIndex])
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

function TWinSSLCertificateStore.FindBySubject(const aSubject: string): ISSLCertificate;
var
  CertContext: PCCERT_CONTEXT;
  SubjectW: WideString;
begin
  Result := nil;

  if (FStoreHandle = nil) or (aSubject = '') then
    Exit;

  SubjectW := WideString(aSubject);

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

function TWinSSLCertificateStore.FindByIssuer(const aIssuer: string): ISSLCertificate;
var
  CertContext: PCCERT_CONTEXT;
  IssuerW: WideString;
begin
  Result := nil;

  if (FStoreHandle = nil) or (aIssuer = '') then
    Exit;

  IssuerW := WideString(aIssuer);

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

function TWinSSLCertificateStore.FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
var
  i: Integer;
  Cert: ISSLCertificate;
begin
  Result := nil;

  // 在缓存中搜索
  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := ISSLCertificate(FCertificates[i]);
    if SameText(Cert.GetSerialNumber, aSerialNumber) then
    begin
      Result := Cert;
      Exit;
    end;
  end;
end;

function TWinSSLCertificateStore.FindByFingerprint(const aFingerprint: string): ISSLCertificate;
var
  i: Integer;
  Cert: ISSLCertificate;
begin
  Result := nil;

  // 在缓存中搜索
  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := ISSLCertificate(FCertificates[i]);
    if SameText(Cert.GetFingerprintSHA256, aFingerprint) or
       SameText(Cert.GetFingerprintSHA1, aFingerprint) then
    begin
      Result := Cert;
      Exit;
    end;
  end;
end;

// ============================================================================
// ISSLCertificateStore - 证书验证
// ============================================================================

function TWinSSLCertificateStore.VerifyCertificate(aCert: ISSLCertificate): Boolean;
begin
  if aCert = nil then
  begin
    Result := False;
    Exit;
  end;

  // 使用证书自身的 Verify 方法，传入当前存储作为 CA 存储
  Result := aCert.Verify(Self);
end;

function TWinSSLCertificateStore.BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
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

  if (aCert = nil) then
    Exit;

  CertContext := PCCERT_CONTEXT(aCert.GetNativeHandle);
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

function TWinSSLCertificateStore.OpenSystemStore(const aStoreName: string): Boolean;
begin
  Result := Open(aStoreName, False);
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
