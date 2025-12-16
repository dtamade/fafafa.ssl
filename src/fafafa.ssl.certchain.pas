{******************************************************************************}
{                                                                              }
{  fafafa.ssl - A unified SSL/TLS library for FreePascal                      }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{  证书链验证模块 - 提供完整的证书链验证功能                                 }
{                                                                              }
{******************************************************************************}
unit fafafa.ssl.certchain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fafafa.ssl.base;

type
  { 证书链验证选项 }
  TChainVerifyOption = (
    cvoCheckTime,           // 检查证书有效期
    cvoCheckSignature,      // 验证签名
    cvoCheckKeyUsage,       // 检查密钥用途
    cvoCheckExtKeyUsage,    // 检查扩展密钥用途
    cvoCheckCAConstraints,  // 检查CA约束
    cvoCheckRevocation,     // 检查吊销状态
    cvoCheckHostname,       // 验证主机名
    cvoAllowSelfSigned,     // 允许自签名证书
    cvoAllowPartialChain,   // 允许部分链
    cvoRequireEVCert        // 要求EV证书
  );
  TChainVerifyOptions = set of TChainVerifyOption;
  
  { 证书链验证结果 }
  TChainVerifyResult = record
    IsValid: Boolean;
    ErrorCode: Integer;
    ErrorMessage: string;
    ChainLength: Integer;
    TrustedRoot: Boolean;
    SelfSigned: Boolean;
    HostnameMatch: Boolean;
    Warnings: TStringList;
  end;
  
  { 证书链验证器接口 }
  ISSLCertificateChainVerifier = interface
    ['{A8B3C4D5-E6F7-4829-9ABC-DEF012345678}']
    
    // 设置选项
    procedure SetOptions(aOptions: TChainVerifyOptions);
    function GetOptions: TChainVerifyOptions;
    
    // 设置信任的根证书存储
    procedure SetTrustedStore(aStore: ISSLCertificateStore);
    function GetTrustedStore: ISSLCertificateStore;
    
    // 设置中间证书存储
    procedure SetIntermediateStore(aStore: ISSLCertificateStore);
    function GetIntermediateStore: ISSLCertificateStore;
    
    // 设置CRL存储
    procedure SetCRLStore(aCRLs: TStringList);
    function GetCRLStore: TStringList;
    
    // 验证单个证书
    function VerifyCertificate(aCert: ISSLCertificate; 
                              const aHostname: string = ''): TChainVerifyResult;
    
    // 验证证书链
    function VerifyChain(const aChain: TSSLCertificateArray;
                        const aHostname: string = ''): TChainVerifyResult;
    
    // 构建证书链（从叶证书开始）
    function BuildChain(aLeafCert: ISSLCertificate;
                      out aChain: TSSLCertificateArray): Boolean;
    
    // 检查特定的验证项
    function CheckCertificateTime(aCert: ISSLCertificate): Boolean;
    function CheckCertificateSignature(aCert: ISSLCertificate; 
                                      aIssuer: ISSLCertificate): Boolean;
    function CheckCertificateKeyUsage(aCert: ISSLCertificate;
                                    aIsCA: Boolean): Boolean;
    function CheckCertificateRevocation(aCert: ISSLCertificate): Boolean;
    function CheckHostname(aCert: ISSLCertificate; 
                          const aHostname: string): Boolean;
  end;

  { 证书链验证器基类 }
  TSSLCertificateChainVerifier = class(TInterfacedObject, ISSLCertificateChainVerifier)
  private
    FOptions: TChainVerifyOptions;
    FTrustedStore: ISSLCertificateStore;
    FIntermediateStore: ISSLCertificateStore;
    FCRLStore: TStringList;
    
    function FindIssuer(aCert: ISSLCertificate): ISSLCertificate;
    function IsRootCertificate(aCert: ISSLCertificate): Boolean;
    function IsSelfSigned(aCert: ISSLCertificate): Boolean;
    function ValidatePathLength(const aChain: TSSLCertificateArray): Boolean;
    function MatchHostname(const aCertName, aHostname: string): Boolean;
    function ParseSubjectAltNames(aCert: ISSLCertificate): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    
    // ISSLCertificateChainVerifier implementation
    procedure SetOptions(aOptions: TChainVerifyOptions);
    function GetOptions: TChainVerifyOptions;
    
    procedure SetTrustedStore(aStore: ISSLCertificateStore);
    function GetTrustedStore: ISSLCertificateStore;
    
    procedure SetIntermediateStore(aStore: ISSLCertificateStore);
    function GetIntermediateStore: ISSLCertificateStore;
    
    procedure SetCRLStore(aCRLs: TStringList);
    function GetCRLStore: TStringList;
    
    function VerifyCertificate(aCert: ISSLCertificate;
                              const aHostname: string = ''): TChainVerifyResult;
    
    function VerifyChain(const aChain: TSSLCertificateArray;
                        const aHostname: string = ''): TChainVerifyResult;
    
    function BuildChain(aLeafCert: ISSLCertificate;
                      out aChain: TSSLCertificateArray): Boolean;
    
    function CheckCertificateTime(aCert: ISSLCertificate): Boolean;
    function CheckCertificateSignature(aCert: ISSLCertificate;
                                      aIssuer: ISSLCertificate): Boolean;
    function CheckCertificateKeyUsage(aCert: ISSLCertificate;
                                    aIsCA: Boolean): Boolean;
    function CheckCertificateRevocation(aCert: ISSLCertificate): Boolean;
    function CheckHostname(aCert: ISSLCertificate;
                          const aHostname: string): Boolean;
  end;

  { 默认的证书链验证选项 }
const
  DefaultChainVerifyOptions: TChainVerifyOptions = [
    cvoCheckTime,
    cvoCheckSignature,
    cvoCheckKeyUsage,
    cvoCheckCAConstraints,
    cvoCheckHostname
  ];
  
  StrictChainVerifyOptions: TChainVerifyOptions = [
    cvoCheckTime,
    cvoCheckSignature,
    cvoCheckKeyUsage,
    cvoCheckExtKeyUsage,
    cvoCheckCAConstraints,
    cvoCheckRevocation,
    cvoCheckHostname
  ];

implementation

{ TSSLCertificateChainVerifier }

constructor TSSLCertificateChainVerifier.Create;
begin
  inherited Create;
  FOptions := DefaultChainVerifyOptions;
  FCRLStore := TStringList.Create;
end;

destructor TSSLCertificateChainVerifier.Destroy;
begin
  FCRLStore.Free;
  inherited;
end;

procedure TSSLCertificateChainVerifier.SetOptions(aOptions: TChainVerifyOptions);
begin
  FOptions := aOptions;
end;

function TSSLCertificateChainVerifier.GetOptions: TChainVerifyOptions;
begin
  Result := FOptions;
end;

procedure TSSLCertificateChainVerifier.SetTrustedStore(aStore: ISSLCertificateStore);
begin
  FTrustedStore := aStore;
end;

function TSSLCertificateChainVerifier.GetTrustedStore: ISSLCertificateStore;
begin
  Result := FTrustedStore;
end;

procedure TSSLCertificateChainVerifier.SetIntermediateStore(aStore: ISSLCertificateStore);
begin
  FIntermediateStore := aStore;
end;

function TSSLCertificateChainVerifier.GetIntermediateStore: ISSLCertificateStore;
begin
  Result := FIntermediateStore;
end;

procedure TSSLCertificateChainVerifier.SetCRLStore(aCRLs: TStringList);
begin
  FCRLStore.Assign(aCRLs);
end;

function TSSLCertificateChainVerifier.GetCRLStore: TStringList;
begin
  Result := FCRLStore;
end;

function TSSLCertificateChainVerifier.FindIssuer(aCert: ISSLCertificate): ISSLCertificate;
var
  IssuerName: string;
begin
  Result := nil;
  
  if aCert = nil then
    Exit;
    
  IssuerName := aCert.GetIssuer;
  
  // 先在信任的根证书中查找
  if Assigned(FTrustedStore) then
  begin
    Result := FTrustedStore.FindByIssuer(IssuerName);
    if Result <> nil then
      Exit;
  end;
  
  // 再在中间证书中查找
  if Assigned(FIntermediateStore) then
  begin
    Result := FIntermediateStore.FindBySubject(IssuerName);
  end;
end;

function TSSLCertificateChainVerifier.IsRootCertificate(aCert: ISSLCertificate): Boolean;
begin
  Result := False;
  
  if (aCert = nil) or (FTrustedStore = nil) then
    Exit;
    
  // 检查是否在信任的根证书存储中
  Result := FTrustedStore.Contains(aCert);
end;

function TSSLCertificateChainVerifier.IsSelfSigned(aCert: ISSLCertificate): Boolean;
begin
  Result := False;
  
  if aCert = nil then
    Exit;
    
  // 自签名证书的 Subject 和 Issuer 相同
  Result := aCert.GetSubject = aCert.GetIssuer;
end;

function TSSLCertificateChainVerifier.ValidatePathLength(const aChain: TSSLCertificateArray): Boolean;
var
  i: Integer;
  MaxPathLength: Integer;
  CertInfo: TSSLCertificateInfo;
begin
  Result := True;
  MaxPathLength := -1; // 无限制
  
  // 从根证书开始检查路径长度约束
  for i := High(aChain) downto 0 do
  begin
    CertInfo := aChain[i].GetInfo;
    
    // 检查是否是CA证书
    if CertInfo.IsCA then
    begin
      // 如果有路径长度限制
      if CertInfo.PathLenConstraint >= 0 then
      begin
        if MaxPathLength < 0 then
          MaxPathLength := CertInfo.PathLenConstraint
        else
          MaxPathLength := Min(MaxPathLength, CertInfo.PathLenConstraint);
      end;
      
      // 检查当前深度是否超过限制
      if (MaxPathLength >= 0) and (i < High(aChain) - MaxPathLength) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TSSLCertificateChainVerifier.MatchHostname(const aCertName, aHostname: string): Boolean;
var
  CertParts, HostParts: TStringList;
  i: Integer;
begin
  Result := False;
  
  // 精确匹配
  if SameText(aCertName, aHostname) then
  begin
    Result := True;
    Exit;
  end;
  
  // 通配符匹配
  if (Pos('*.', aCertName) = 1) then
  begin
    CertParts := TStringList.Create;
    HostParts := TStringList.Create;
    try
      CertParts.Delimiter := '.';
      CertParts.DelimitedText := aCertName;
      
      HostParts.Delimiter := '.';
      HostParts.DelimitedText := aHostname;
      
      // 域名级数必须相同
      if CertParts.Count = HostParts.Count then
      begin
        Result := True;
        // 从第二级开始比较（跳过通配符）
        for i := 1 to CertParts.Count - 1 do
        begin
          if not SameText(CertParts[i], HostParts[i]) then
          begin
            Result := False;
            Break;
          end;
        end;
      end;
    finally
      CertParts.Free;
      HostParts.Free;
    end;
  end;
end;

function TSSLCertificateChainVerifier.ParseSubjectAltNames(aCert: ISSLCertificate): TStringList;
var
  RawSANs: TStringList;
  Line, Item: string;
  i, SepPos: Integer;
begin
  Result := TStringList.Create;
  
  if aCert = nil then
    Exit;
  
  RawSANs := aCert.GetSubjectAltNames;
  if RawSANs = nil then
    Exit;
  try
    for i := 0 to RawSANs.Count - 1 do
    begin
      Line := Trim(RawSANs[i]);
      if Line = '' then
        Continue;
      
      // 一行中可能包含多个以逗号分隔的条目
      repeat
        SepPos := Pos(',', Line);
        if SepPos > 0 then
        begin
          Item := Trim(Copy(Line, 1, SepPos - 1));
          Line := Trim(Copy(Line, SepPos + 1, MaxInt));
        end
        else
        begin
          Item := Line;
          Line := '';
        end;
        
        if Item <> '' then
        begin
          // 去掉类似 "DNS:" 这类前缀，只保留主机名部分
          SepPos := Pos(':', Item);
          if SepPos > 0 then
            Item := Trim(Copy(Item, SepPos + 1, MaxInt));
          
          if Item <> '' then
            Result.Add(Item);
        end;
      until Line = '';
    end;
  finally
    RawSANs.Free;
  end;
end;

function TSSLCertificateChainVerifier.CheckCertificateTime(aCert: ISSLCertificate): Boolean;
var
  Info: TSSLCertificateInfo;
  CurrentTime: TDateTime;
begin
  Result := False;
  
  if aCert = nil then
    Exit;
    
  Info := aCert.GetInfo;
  CurrentTime := Now;
  
  // 检查证书是否在有效期内
  Result := (CurrentTime >= Info.NotBefore) and (CurrentTime <= Info.NotAfter);
end;

function TSSLCertificateChainVerifier.CheckCertificateSignature(aCert: ISSLCertificate;
  aIssuer: ISSLCertificate): Boolean;
begin
  Result := True;
  
  if aCert = nil then
    Exit;
  
  // 如果配置了信任存储，则委托给底层证书实现进行完整验证
  // 这通常会检查签名有效性以及证书链是否可信
  if Assigned(FTrustedStore) then
    Result := aCert.Verify(FTrustedStore);
  
  // 注意：当前使用底层库的标准验证，已包含签名检查。
  // 可选增强：使用 aIssuer 的公钥对签名做更细粒度的逐跳验证。
  // 这适用于需要自定义验证逻辑的高级场景。
end;

function TSSLCertificateChainVerifier.CheckCertificateKeyUsage(aCert: ISSLCertificate;
  aIsCA: Boolean): Boolean;
var
  Info: TSSLCertificateInfo;
begin
  Result := True;
  
  if aCert = nil then
  begin
    Result := False;
    Exit;
  end;
  
  Info := aCert.GetInfo;
  
  if aIsCA then
  begin
    // CA证书必须有 keyCertSign 权限
    Result := Info.IsCA and (Info.KeyUsage and $04 <> 0); // keyCertSign
  end
  else
  begin
    // 终端证书通常需要 digitalSignature 和 keyEncipherment
    Result := (Info.KeyUsage and $80 <> 0) or  // digitalSignature
              (Info.KeyUsage and $20 <> 0);     // keyEncipherment
  end;
end;

function TSSLCertificateChainVerifier.CheckCertificateRevocation(aCert: ISSLCertificate): Boolean;
var
  VerifyResult: TSSLCertVerifyResult;
  Flags: TSSLCertVerifyFlags;
begin
  Result := True;
  
  if aCert = nil then
    Exit;
  
  // 如果没有信任存储，则无法进行有意义的吊销检查
  // 为保持向后兼容，这种情况下不认为证书已被吊销
  if not Assigned(FTrustedStore) then
    Exit;
  
  // 构造验证标志：始终请求检查吊销状态/CRL
  Flags := [];
  Include(Flags, sslCertVerifyCheckRevocation);
  Include(Flags, sslCertVerifyCheckCRL);
  Include(Flags, sslCertVerifyCheckOCSP);
  
  // 如果链验证选项中未启用时间检查，则在底层验证中忽略过期错误
  if not (cvoCheckTime in FOptions) then
    Include(Flags, sslCertVerifyIgnoreExpiry);
  
  // 如果允许自签名证书，则在底层验证中放宽自签名限制
  if cvoAllowSelfSigned in FOptions then
    Include(Flags, sslCertVerifyAllowSelfSigned);
  
  // 调用底层证书实现执行实际的撤销检查
  if not aCert.VerifyEx(FTrustedStore, Flags, VerifyResult) then
  begin
    // 仅当底层明确标记为“已吊销”时才认为吊销检查失败
    if VerifyResult.RevocationStatus = 1 then
      Result := False
    else
      Result := True; // 其他错误交由时间/签名等检查处理
  end;
end;

function TSSLCertificateChainVerifier.CheckHostname(aCert: ISSLCertificate;
  const aHostname: string): Boolean;
var
  CN: string;
  SANs: TStringList;
  i: Integer;
  CertInfo: TSSLCertificateInfo;
begin
  Result := False;
  
  if (aCert = nil) or (aHostname = '') then
    Exit;
  
  // 首先检查 Subject Alternative Names
  SANs := ParseSubjectAltNames(aCert);
  try
    for i := 0 to SANs.Count - 1 do
    begin
      if MatchHostname(SANs[i], aHostname) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    SANs.Free;
  end;
  
  // 如果没有SAN或没有匹配，检查CN
  CertInfo := aCert.GetInfo;
  
  // 从Subject中提取CN
  // 简化处理：假设Subject格式为 "CN=xxx, ..."
  if Pos('CN=', CertInfo.Subject) > 0 then
  begin
    CN := CertInfo.Subject;
    CN := Copy(CN, Pos('CN=', CN) + 3, Length(CN));
    if Pos(',', CN) > 0 then
      CN := Copy(CN, 1, Pos(',', CN) - 1);
    
    Result := MatchHostname(Trim(CN), aHostname);
  end;
end;

function TSSLCertificateChainVerifier.BuildChain(aLeafCert: ISSLCertificate;
  out aChain: TSSLCertificateArray): Boolean;
var
  CurrentCert, IssuerCert: ISSLCertificate;
  ChainList: TList;
  MaxDepth: Integer;
begin
  Result := False;
  SetLength(aChain, 0);
  
  if aLeafCert = nil then
    Exit;
    
  ChainList := TList.Create;
  try
    CurrentCert := aLeafCert;
    ChainList.Add(Pointer(CurrentCert));
    MaxDepth := 10; // 防止无限循环
    
    // 构建证书链
    while (not IsSelfSigned(CurrentCert)) and 
          (not IsRootCertificate(CurrentCert)) and
          (ChainList.Count < MaxDepth) do
    begin
      IssuerCert := FindIssuer(CurrentCert);
      if IssuerCert = nil then
      begin
        // 无法找到颁发者，链断裂
        if cvoAllowPartialChain in FOptions then
          Result := True  // 允许部分链
        else
          Exit;
        Break;
      end;
      
      ChainList.Add(Pointer(IssuerCert));
      CurrentCert := IssuerCert;
    end;
    
    // 转换为数组
    SetLength(aChain, ChainList.Count);
    for MaxDepth := 0 to ChainList.Count - 1 do
      aChain[MaxDepth] := ISSLCertificate(ChainList[MaxDepth]);
      
    Result := True;
  finally
    ChainList.Free;
  end;
end;

function TSSLCertificateChainVerifier.VerifyCertificate(aCert: ISSLCertificate;
  const aHostname: string = ''): TChainVerifyResult;
var
  Chain: TSSLCertificateArray;
begin
  // 构建证书链
  if BuildChain(aCert, Chain) then
    Result := VerifyChain(Chain, aHostname)
  else
  begin
    Result.IsValid := False;
    Result.ErrorCode := -1;
    Result.ErrorMessage := 'Failed to build certificate chain';
    Result.ChainLength := 0;
    Result.TrustedRoot := False;
    Result.SelfSigned := IsSelfSigned(aCert);
    Result.HostnameMatch := False;
    Result.Warnings := nil;
  end;
end;

function TSSLCertificateChainVerifier.VerifyChain(const aChain: TSSLCertificateArray;
  const aHostname: string = ''): TChainVerifyResult;
var
  i: Integer;
  CurrentCert, IssuerCert: ISSLCertificate;
begin
  // 初始化结果
  Result.IsValid := True;
  Result.ErrorCode := 0;
  Result.ErrorMessage := '';
  Result.ChainLength := Length(aChain);
  Result.TrustedRoot := False;
  Result.SelfSigned := False;
  Result.HostnameMatch := True;
  Result.Warnings := TStringList.Create;
  
  // 如果启用了吊销检查但未配置 CRL 存储，给出提示性警告
  if (cvoCheckRevocation in FOptions) and
     ((FCRLStore = nil) or (FCRLStore.Count = 0)) then
    Result.Warnings.Add('已启用吊销检查选项但未配置 CRL 存储，未对证书撤销状态进行验证。');
  
  if Length(aChain) = 0 then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Empty certificate chain';
    Exit;
  end;
  
  // 检查主机名（只检查叶证书）
  if (cvoCheckHostname in FOptions) and (aHostname <> '') then
  begin
    Result.HostnameMatch := CheckHostname(aChain[0], aHostname);
    if not Result.HostnameMatch then
    begin
      Result.IsValid := False;
      Result.ErrorMessage := 'Hostname verification failed';
      Exit;
    end;
  end;
  
  // 验证证书链
  for i := 0 to High(aChain) do
  begin
    CurrentCert := aChain[i];
    
    // 检查时间有效性
    if cvoCheckTime in FOptions then
    begin
      if not CheckCertificateTime(CurrentCert) then
      begin
        Result.IsValid := False;
        Result.ErrorMessage := Format('Certificate %d expired or not yet valid', [i]);
        Exit;
      end;
    end;
    
    // 检查密钥用途
    if cvoCheckKeyUsage in FOptions then
    begin
      if not CheckCertificateKeyUsage(CurrentCert, i > 0) then
      begin
        Result.IsValid := False;
        Result.ErrorMessage := Format('Invalid key usage for certificate %d', [i]);
        Exit;
      end;
    end;
    
    // 检查吊销状态
    if cvoCheckRevocation in FOptions then
    begin
      if not CheckCertificateRevocation(CurrentCert) then
      begin
        Result.IsValid := False;
        Result.ErrorMessage := Format('Certificate %d is revoked', [i]);
        Exit;
      end;
    end;
    
    // 验证签名（除了根证书）
    if (cvoCheckSignature in FOptions) and (i < High(aChain)) then
    begin
      IssuerCert := aChain[i + 1];
      if not CheckCertificateSignature(CurrentCert, IssuerCert) then
      begin
        Result.IsValid := False;
        Result.ErrorMessage := Format('Invalid signature for certificate %d', [i]);
        Exit;
      end;
    end;
  end;
  
  // 检查根证书
  if High(aChain) >= 0 then
  begin
    Result.SelfSigned := IsSelfSigned(aChain[High(aChain)]);
    Result.TrustedRoot := IsRootCertificate(aChain[High(aChain)]);
    
    if not Result.TrustedRoot and not (cvoAllowSelfSigned in FOptions) then
    begin
      Result.IsValid := False;
      Result.ErrorMessage := 'Untrusted root certificate';
    end;
  end;
  
  // 验证路径长度约束
  if (cvoCheckCAConstraints in FOptions) and not ValidatePathLength(aChain) then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Path length constraint violated';
  end;
end;

end.
