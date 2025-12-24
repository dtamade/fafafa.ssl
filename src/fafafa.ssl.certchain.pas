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
  
  {**
   * ISSLCertificateChainVerifier - 证书链验证器接口
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
  ISSLCertificateChainVerifier = interface
    ['{A8B3C4D5-E6F7-4829-9ABC-DEF012345678}']
    
    // 设置选项
    procedure SetOptions(AOptions: TChainVerifyOptions);
    function GetOptions: TChainVerifyOptions;
    
    // 设置信任的根证书存储
    procedure SetTrustedStore(AStore: ISSLCertificateStore);
    function GetTrustedStore: ISSLCertificateStore;
    
    // 设置中间证书存储
    procedure SetIntermediateStore(AStore: ISSLCertificateStore);
    function GetIntermediateStore: ISSLCertificateStore;
    
    // 设置CRL存储
    procedure SetCRLStore(ACRLs: TStringList);
    function GetCRLStore: TStringList;
    
    // 验证单个证书
    function VerifyCertificate(ACert: ISSLCertificate; 
                              const AHostname: string = ''): TChainVerifyResult;
    
    // 验证证书链
    function VerifyChain(const AChain: TSSLCertificateArray;
                        const AHostname: string = ''): TChainVerifyResult;
    
    // 构建证书链（从叶证书开始）
    function BuildChain(ALeafCert: ISSLCertificate;
                      out AChain: TSSLCertificateArray): Boolean;
    
    // 检查特定的验证项
    function CheckCertificateTime(ACert: ISSLCertificate): Boolean;
    function CheckCertificateSignature(ACert: ISSLCertificate; 
                                      AIssuer: ISSLCertificate): Boolean;
    function CheckCertificateKeyUsage(ACert: ISSLCertificate;
                                    AIsCA: Boolean): Boolean;
    function CheckCertificateRevocation(ACert: ISSLCertificate): Boolean;
    function CheckHostname(ACert: ISSLCertificate; 
                          const AHostname: string): Boolean;
  end;

  { 证书链验证器基类 }
  TSSLCertificateChainVerifier = class(TInterfacedObject, ISSLCertificateChainVerifier)
  private
    FOptions: TChainVerifyOptions;
    FTrustedStore: ISSLCertificateStore;
    FIntermediateStore: ISSLCertificateStore;
    FCRLStore: TStringList;
    
    function FindIssuer(ACert: ISSLCertificate): ISSLCertificate;
    function IsRootCertificate(ACert: ISSLCertificate): Boolean;
    function IsSelfSigned(ACert: ISSLCertificate): Boolean;
    function ValidatePathLength(const AChain: TSSLCertificateArray): Boolean;
    function MatchHostname(const ACertName, AHostname: string): Boolean;
    function ParseSubjectAltNames(ACert: ISSLCertificate): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    
    // ISSLCertificateChainVerifier implementation
    procedure SetOptions(AOptions: TChainVerifyOptions);
    function GetOptions: TChainVerifyOptions;
    
    procedure SetTrustedStore(AStore: ISSLCertificateStore);
    function GetTrustedStore: ISSLCertificateStore;
    
    procedure SetIntermediateStore(AStore: ISSLCertificateStore);
    function GetIntermediateStore: ISSLCertificateStore;
    
    procedure SetCRLStore(ACRLs: TStringList);
    function GetCRLStore: TStringList;
    
    function VerifyCertificate(ACert: ISSLCertificate;
                              const AHostname: string = ''): TChainVerifyResult;
    
    function VerifyChain(const AChain: TSSLCertificateArray;
                        const AHostname: string = ''): TChainVerifyResult;
    
    function BuildChain(ALeafCert: ISSLCertificate;
                      out AChain: TSSLCertificateArray): Boolean;
    
    function CheckCertificateTime(ACert: ISSLCertificate): Boolean;
    function CheckCertificateSignature(ACert: ISSLCertificate;
                                      AIssuer: ISSLCertificate): Boolean;
    function CheckCertificateKeyUsage(ACert: ISSLCertificate;
                                    AIsCA: Boolean): Boolean;
    function CheckCertificateRevocation(ACert: ISSLCertificate): Boolean;
    function CheckHostname(ACert: ISSLCertificate;
                          const AHostname: string): Boolean;
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

procedure TSSLCertificateChainVerifier.SetOptions(AOptions: TChainVerifyOptions);
begin
  FOptions := AOptions;
end;

function TSSLCertificateChainVerifier.GetOptions: TChainVerifyOptions;
begin
  Result := FOptions;
end;

procedure TSSLCertificateChainVerifier.SetTrustedStore(AStore: ISSLCertificateStore);
begin
  FTrustedStore := AStore;
end;

function TSSLCertificateChainVerifier.GetTrustedStore: ISSLCertificateStore;
begin
  Result := FTrustedStore;
end;

procedure TSSLCertificateChainVerifier.SetIntermediateStore(AStore: ISSLCertificateStore);
begin
  FIntermediateStore := AStore;
end;

function TSSLCertificateChainVerifier.GetIntermediateStore: ISSLCertificateStore;
begin
  Result := FIntermediateStore;
end;

procedure TSSLCertificateChainVerifier.SetCRLStore(ACRLs: TStringList);
begin
  FCRLStore.Assign(ACRLs);
end;

function TSSLCertificateChainVerifier.GetCRLStore: TStringList;
begin
  Result := FCRLStore;
end;

function TSSLCertificateChainVerifier.FindIssuer(ACert: ISSLCertificate): ISSLCertificate;
var
  IssuerName: string;
begin
  Result := nil;
  
  if ACert = nil then
    Exit;
    
  IssuerName := ACert.GetIssuer;
  
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

function TSSLCertificateChainVerifier.IsRootCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  
  if (ACert = nil) or (FTrustedStore = nil) then
    Exit;
    
  // 检查是否在信任的根证书存储中
  Result := FTrustedStore.Contains(ACert);
end;

function TSSLCertificateChainVerifier.IsSelfSigned(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
  
  if ACert = nil then
    Exit;
    
  // 自签名证书的 Subject 和 Issuer 相同
  Result := ACert.GetSubject = ACert.GetIssuer;
end;

function TSSLCertificateChainVerifier.ValidatePathLength(const AChain: TSSLCertificateArray): Boolean;
var
  i: Integer;
  MaxPathLength: Integer;
  CertInfo: TSSLCertificateInfo;
begin
  Result := True;
  MaxPathLength := -1; // 无限制
  
  // 从根证书开始检查路径长度约束
  for i := High(AChain) downto 0 do
  begin
    CertInfo := AChain[i].GetInfo;
    
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
      if (MaxPathLength >= 0) and (i < High(AChain) - MaxPathLength) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TSSLCertificateChainVerifier.MatchHostname(const ACertName, AHostname: string): Boolean;
var
  CertParts, HostParts: TStringList;
  i: Integer;
begin
  Result := False;
  
  // 精确匹配
  if SameText(ACertName, AHostname) then
  begin
    Result := True;
    Exit;
  end;
  
  // 通配符匹配
  if (Pos('*.', ACertName) = 1) then
  begin
    CertParts := TStringList.Create;
    HostParts := TStringList.Create;
    try
      CertParts.Delimiter := '.';
      CertParts.DelimitedText := ACertName;
      
      HostParts.Delimiter := '.';
      HostParts.DelimitedText := AHostname;
      
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

function TSSLCertificateChainVerifier.ParseSubjectAltNames(ACert: ISSLCertificate): TStringList;
var
  RawSANs: TSSLStringArray;
  Line, Item: string;
  i, SepPos: Integer;
begin
  Result := TStringList.Create;

  if ACert = nil then
    Exit;

  RawSANs := ACert.GetSubjectAltNames;
  if Length(RawSANs) = 0 then
    Exit;

  for i := 0 to Length(RawSANs) - 1 do
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
end;

function TSSLCertificateChainVerifier.CheckCertificateTime(ACert: ISSLCertificate): Boolean;
var
  Info: TSSLCertificateInfo;
  CurrentTime: TDateTime;
begin
  Result := False;
  
  if ACert = nil then
    Exit;
    
  Info := ACert.GetInfo;
  CurrentTime := Now;
  
  // 检查证书是否在有效期内
  Result := (CurrentTime >= Info.NotBefore) and (CurrentTime <= Info.NotAfter);
end;

function TSSLCertificateChainVerifier.CheckCertificateSignature(ACert: ISSLCertificate;
  AIssuer: ISSLCertificate): Boolean;
begin
  Result := True;
  
  if ACert = nil then
    Exit;
  
  // 如果配置了信任存储，则委托给底层证书实现进行完整验证
  // 这通常会检查签名有效性以及证书链是否可信
  if Assigned(FTrustedStore) then
    Result := ACert.Verify(FTrustedStore);
  
  // 注意：当前使用底层库的标准验证，已包含签名检查。
  // 可选增强：使用 AIssuer 的公钥对签名做更细粒度的逐跳验证。
  // 这适用于需要自定义验证逻辑的高级场景。
end;

function TSSLCertificateChainVerifier.CheckCertificateKeyUsage(ACert: ISSLCertificate;
  AIsCA: Boolean): Boolean;
var
  Info: TSSLCertificateInfo;
begin
  Result := True;
  
  if ACert = nil then
  begin
    Result := False;
    Exit;
  end;
  
  Info := ACert.GetInfo;
  
  if AIsCA then
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

function TSSLCertificateChainVerifier.CheckCertificateRevocation(ACert: ISSLCertificate): Boolean;
var
  VerifyResult: TSSLCertVerifyResult;
  Flags: TSSLCertVerifyFlags;
begin
  Result := True;
  
  if ACert = nil then
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
  if not ACert.VerifyEx(FTrustedStore, Flags, VerifyResult) then
  begin
    // 仅当底层明确标记为“已吊销”时才认为吊销检查失败
    if VerifyResult.RevocationStatus = 1 then
      Result := False
    else
      Result := True; // 其他错误交由时间/签名等检查处理
  end;
end;

function TSSLCertificateChainVerifier.CheckHostname(ACert: ISSLCertificate;
  const AHostname: string): Boolean;
var
  CN: string;
  SANs: TStringList;
  i: Integer;
  CertInfo: TSSLCertificateInfo;
begin
  Result := False;
  
  if (ACert = nil) or (AHostname = '') then
    Exit;
  
  // 首先检查 Subject Alternative Names
  SANs := ParseSubjectAltNames(ACert);
  try
    for i := 0 to SANs.Count - 1 do
    begin
      if MatchHostname(SANs[i], AHostname) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    SANs.Free;
  end;
  
  // 如果没有SAN或没有匹配，检查CN
  CertInfo := ACert.GetInfo;
  
  // 从Subject中提取CN
  // 简化处理：假设Subject格式为 "CN=xxx, ..."
  if Pos('CN=', CertInfo.Subject) > 0 then
  begin
    CN := CertInfo.Subject;
    CN := Copy(CN, Pos('CN=', CN) + 3, Length(CN));
    if Pos(',', CN) > 0 then
      CN := Copy(CN, 1, Pos(',', CN) - 1);
    
    Result := MatchHostname(Trim(CN), AHostname);
  end;
end;

function TSSLCertificateChainVerifier.BuildChain(ALeafCert: ISSLCertificate;
  out AChain: TSSLCertificateArray): Boolean;
var
  CurrentCert, IssuerCert: ISSLCertificate;
  ChainList: TList;
  MaxDepth: Integer;
begin
  Result := False;
  SetLength(AChain, 0);
  
  if ALeafCert = nil then
    Exit;
    
  ChainList := TList.Create;
  try
    CurrentCert := ALeafCert;
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
    SetLength(AChain, ChainList.Count);
    for MaxDepth := 0 to ChainList.Count - 1 do
      AChain[MaxDepth] := ISSLCertificate(ChainList[MaxDepth]);
      
    Result := True;
  finally
    ChainList.Free;
  end;
end;

function TSSLCertificateChainVerifier.VerifyCertificate(ACert: ISSLCertificate;
  const AHostname: string = ''): TChainVerifyResult;
var
  Chain: TSSLCertificateArray;
begin
  // 构建证书链
  if BuildChain(ACert, Chain) then
    Result := VerifyChain(Chain, AHostname)
  else
  begin
    Result.IsValid := False;
    Result.ErrorCode := -1;
    Result.ErrorMessage := 'Failed to build certificate chain';
    Result.ChainLength := 0;
    Result.TrustedRoot := False;
    Result.SelfSigned := IsSelfSigned(ACert);
    Result.HostnameMatch := False;
    Result.Warnings := nil;
  end;
end;

function TSSLCertificateChainVerifier.VerifyChain(const AChain: TSSLCertificateArray;
  const AHostname: string = ''): TChainVerifyResult;
var
  i: Integer;
  CurrentCert, IssuerCert: ISSLCertificate;
begin
  // 初始化结果
  Result.IsValid := True;
  Result.ErrorCode := 0;
  Result.ErrorMessage := '';
  Result.ChainLength := Length(AChain);
  Result.TrustedRoot := False;
  Result.SelfSigned := False;
  Result.HostnameMatch := True;
  Result.Warnings := TStringList.Create;
  
  // 如果启用了吊销检查但未配置 CRL 存储，给出提示性警告
  if (cvoCheckRevocation in FOptions) and
    ((FCRLStore = nil) or (FCRLStore.Count = 0)) then
    Result.Warnings.Add('已启用吊销检查选项但未配置 CRL 存储，未对证书撤销状态进行验证。');
  
  if Length(AChain) = 0 then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Empty certificate chain';
    Exit;
  end;
  
  // 检查主机名（只检查叶证书）
  if (cvoCheckHostname in FOptions) and (AHostname <> '') then
  begin
    Result.HostnameMatch := CheckHostname(AChain[0], AHostname);
    if not Result.HostnameMatch then
    begin
      Result.IsValid := False;
      Result.ErrorMessage := 'Hostname verification failed';
      Exit;
    end;
  end;
  
  // 验证证书链
  for i := 0 to High(AChain) do
  begin
    CurrentCert := AChain[i];
    
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
    if (cvoCheckSignature in FOptions) and (i < High(AChain)) then
    begin
      IssuerCert := AChain[i + 1];
      if not CheckCertificateSignature(CurrentCert, IssuerCert) then
      begin
        Result.IsValid := False;
        Result.ErrorMessage := Format('Invalid signature for certificate %d', [i]);
        Exit;
      end;
    end;
  end;
  
  // 检查根证书
  if High(AChain) >= 0 then
  begin
    Result.SelfSigned := IsSelfSigned(AChain[High(AChain)]);
    Result.TrustedRoot := IsRootCertificate(AChain[High(AChain)]);
    
    if not Result.TrustedRoot and not (cvoAllowSelfSigned in FOptions) then
    begin
      Result.IsValid := False;
      Result.ErrorMessage := 'Untrusted root certificate';
    end;
  end;
  
  // 验证路径长度约束
  if (cvoCheckCAConstraints in FOptions) and not ValidatePathLength(AChain) then
  begin
    Result.IsValid := False;
    Result.ErrorMessage := 'Path length constraint violated';
  end;
end;

end.
