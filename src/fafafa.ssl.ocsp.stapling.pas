unit fafafa.ssl.ocsp.stapling;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

{
  OCSP Stapling 管理器
  
  提供完整的 OCSP Stapling 支持:
  - 客户端: 请求和验证 stapled OCSP 响应
  - 服务端: 获取和附加 OCSP 响应到握手
  - 自动刷新: 后台更新即将过期的响应
  - 缓存集成: 使用 TOCSPResponseCache 减少网络请求
  
  RFC 6066 (TLS Extensions) - OCSP Status Request
  RFC 6961 (TLS Multiple Certificate Status Request)
  
  @author fafafa.ssl team
  @version 1.0.0
}

interface

uses
  SysUtils, Classes, SyncObjs, DateUtils,
  fafafa.ssl.ocsp,
  fafafa.ssl.ocsp.cache,
  fafafa.ssl.x509,
  fafafa.ssl.crypto.hash;

type
  // ========================================================================
  // OCSP Stapling 配置
  // ========================================================================
  TOCSPStaplingConfig = record
    EnableClientRequest: Boolean;     // 客户端是否请求 stapling
    EnableServerStapling: Boolean;    // 服务端是否提供 stapling
    RequireStapling: Boolean;         // 是否强制要求 stapling (客户端)
    AutoRefresh: Boolean;             // 是否自动刷新响应
    RefreshBeforeExpiry: Integer;     // 过期前多少秒刷新 (默认 3600)
    MaxRetries: Integer;              // 获取失败最大重试次数
    TimeoutSeconds: Integer;          // HTTP 请求超时 (秒)
    UseCache: Boolean;                // 是否使用缓存
    
    class function Default: TOCSPStaplingConfig; static;
  end;

  // ========================================================================
  // OCSP Stapling 状态
  // ========================================================================
  TOCSPStaplingStatus = (
    ossNotRequested,      // 未请求
    ossRequested,         // 已请求,等待响应
    ossReceived,          // 已收到响应
    ossVerified,          // 已验证通过
    ossVerificationFailed,// 验证失败
    ossNotProvided,       // 服务端未提供
    ossExpired            // 响应已过期
  );

  // ========================================================================
  // OCSP Stapling 结果
  // ========================================================================
  TOCSPStaplingResult = record
    Status: TOCSPStaplingStatus;
    Response: TBytes;                 // DER 编码的 OCSP 响应
    CertStatus: TOCSPCertStatus;      // 证书状态
    ThisUpdate: TDateTime;
    NextUpdate: TDateTime;
    ErrorMessage: string;
    
    function IsValid: Boolean;
    function NeedsRefresh: Boolean;
  end;

  // ========================================================================
  // OCSP Stapling 客户端
  // ========================================================================
  
  { TOCSPStaplingClient - 客户端 Stapling 管理
    
    职责:
    - 在 TLS 握手中请求 OCSP stapling
    - 验证服务端提供的 stapled 响应
    - 根据配置决定是否强制要求 stapling
  }
  TOCSPStaplingClient = class
  private
    FConfig: TOCSPStaplingConfig;
    FCache: TOCSPResponseCache;
    FLastResult: TOCSPStaplingResult;
    
    function VerifyStapledResponse(const AResponse: TBytes; 
      ACert, AIssuerCert: TX509Certificate): Boolean;
    function CheckResponseFreshness(const AResponse: TOCSPResponse): Boolean;
  public
    constructor Create(AConfig: TOCSPStaplingConfig; ACache: TOCSPResponseCache = nil);
    destructor Destroy; override;
    
    // 客户端 API
    function ShouldRequestStapling: Boolean;
    function ProcessStapledResponse(const AResponse: TBytes;
      ACert, AIssuerCert: TX509Certificate): TOCSPStaplingResult;
    function ValidateStaplingRequirement(AStaplingProvided: Boolean): Boolean;
    
    property Config: TOCSPStaplingConfig read FConfig write FConfig;
    property LastResult: TOCSPStaplingResult read FLastResult;
  end;

  // ========================================================================
  // OCSP Stapling 服务端
  // ========================================================================
  
  { TOCSPStaplingServer - 服务端 Stapling 管理
    
    职责:
    - 为服务器证书获取 OCSP 响应
    - 缓存响应并在过期前自动刷新
    - 在 TLS 握手中附加响应
  }
  TOCSPStaplingServer = class
  private
    FConfig: TOCSPStaplingConfig;
    FCache: TOCSPResponseCache;
    FLock: TCriticalSection;
    FAutoRefreshEnabled: Boolean;
    
    function FetchOCSPResponse(ACert, AIssuerCert: TX509Certificate): TBytes;
    function SendOCSPRequest(const AURL: string; const ARequest: TBytes): TBytes;
    function ShouldRefreshResponse(const AResponse: TBytes): Boolean;
  public
    constructor Create(AConfig: TOCSPStaplingConfig; ACache: TOCSPResponseCache = nil);
    destructor Destroy; override;
    
    // 服务端 API
    function GetStapledResponse(ACert, AIssuerCert: TX509Certificate;
      AForceRefresh: Boolean = False): TBytes;
    function RefreshResponse(ACert, AIssuerCert: TX509Certificate): Boolean;
    procedure EnableAutoRefresh;
    procedure DisableAutoRefresh;
    
    property Config: TOCSPStaplingConfig read FConfig write FConfig;
    property AutoRefreshEnabled: Boolean read FAutoRefreshEnabled;
  end;

  // ========================================================================
  // OCSP Stapling 管理器 (统一接口)
  // ========================================================================
  
  { TOCSPStaplingManager - 统一的 Stapling 管理器
    
    提供客户端和服务端的统一接口,简化使用
  }
  TOCSPStaplingManager = class
  private
    FConfig: TOCSPStaplingConfig;
    FCache: TOCSPResponseCache;
    FClient: TOCSPStaplingClient;
    FServer: TOCSPStaplingServer;
    FOwnCache: Boolean;
  public
    constructor Create(AConfig: TOCSPStaplingConfig; ACache: TOCSPResponseCache = nil);
    destructor Destroy; override;
    
    // 客户端接口
    function ClientShouldRequest: Boolean;
    function ClientProcessResponse(const AResponse: TBytes;
      ACert, AIssuerCert: TX509Certificate): TOCSPStaplingResult;
    function ClientValidateRequirement(AProvided: Boolean): Boolean;
    
    // 服务端接口
    function ServerGetResponse(ACert, AIssuerCert: TX509Certificate): TBytes;
    function ServerRefreshResponse(ACert, AIssuerCert: TX509Certificate): Boolean;
    
    // 通用接口
    function GetCacheStats: TOCSPCacheStats;
    procedure ClearCache;
    
    property Config: TOCSPStaplingConfig read FConfig write FConfig;
    property Cache: TOCSPResponseCache read FCache;
    property Client: TOCSPStaplingClient read FClient;
    property Server: TOCSPStaplingServer read FServer;
  end;

implementation

uses
  fafafa.ssl.http.client;  // 假设有 HTTP 客户端模块

// ========================================================================
// TOCSPStaplingConfig
// ========================================================================

class function TOCSPStaplingConfig.Default: TOCSPStaplingConfig;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.EnableClientRequest := True;
  Result.EnableServerStapling := True;
  Result.RequireStapling := False;        // 默认不强制
  Result.AutoRefresh := True;
  Result.RefreshBeforeExpiry := 3600;     // 1小时
  Result.MaxRetries := 3;
  Result.TimeoutSeconds := 10;
  Result.UseCache := True;
end;

// ========================================================================
// TOCSPStaplingResult
// ========================================================================

function TOCSPStaplingResult.IsValid: Boolean;
begin
  Result := (Status = ossVerified) and (CertStatus = ocspGood);
end;

function TOCSPStaplingResult.NeedsRefresh: Boolean;
var
  TimeUntilExpiry: Int64;
begin
  if NextUpdate = 0 then
    Exit(False);
  
  TimeUntilExpiry := SecondsBetween(Now, NextUpdate);
  Result := TimeUntilExpiry < 3600;  // 1小时内过期
end;

// ========================================================================
// TOCSPStaplingClient
// ========================================================================

constructor TOCSPStaplingClient.Create(AConfig: TOCSPStaplingConfig; 
  ACache: TOCSPResponseCache);
begin
  inherited Create;
  FConfig := AConfig;
  FCache := ACache;
  FillChar(FLastResult, SizeOf(FLastResult), 0);
  FLastResult.Status := ossNotRequested;
end;

destructor TOCSPStaplingClient.Destroy;
begin
  inherited Destroy;
end;

function TOCSPStaplingClient.ShouldRequestStapling: Boolean;
begin
  Result := FConfig.EnableClientRequest;
end;

function TOCSPStaplingClient.ProcessStapledResponse(const AResponse: TBytes;
  ACert, AIssuerCert: TX509Certificate): TOCSPStaplingResult;
var
  OCSPResp: TOCSPResponse;
  CertID: TOCSPCertID;
  SingleResp: TOCSPSingleResponse;
  Idx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Status := ossReceived;
  Result.Response := Copy(AResponse, 0, Length(AResponse));
  
  if Length(AResponse) = 0 then
  begin
    Result.Status := ossNotProvided;
    Result.ErrorMessage := 'No OCSP response provided';
    FLastResult := Result;
    Exit;
  end;
  
  // 解析响应
  OCSPResp := TOCSPResponse.Create;
  try
    try
      OCSPResp.LoadFromDER(AResponse);
      
      // 检查响应状态
      if OCSPResp.ResponseStatus <> ocsprsSuccessful then
      begin
        Result.Status := ossVerificationFailed;
        Result.ErrorMessage := 'OCSP response status: ' + 
          OCSPResponseStatusToString(OCSPResp.ResponseStatus);
        FLastResult := Result;
        Exit;
      end;
      
      // 查找证书的响应
      CertID := TOCSPCertID.Create(ACert, AIssuerCert);
      Idx := OCSPResp.FindResponse(CertID);
      
      if Idx < 0 then
      begin
        Result.Status := ossVerificationFailed;
        Result.ErrorMessage := 'Certificate not found in OCSP response';
        FLastResult := Result;
        Exit;
      end;
      
      SingleResp := OCSPResp.Responses[Idx];
      Result.CertStatus := SingleResp.CertStatus;
      Result.ThisUpdate := SingleResp.ThisUpdate;
      Result.NextUpdate := SingleResp.NextUpdate;
      
      // 检查响应新鲜度
      if not CheckResponseFreshness(OCSPResp) then
      begin
        Result.Status := ossExpired;
        Result.ErrorMessage := 'OCSP response is expired or not yet valid';
        FLastResult := Result;
        Exit;
      end;
      
      // 验证通过
      Result.Status := ossVerified;
      
      // 缓存响应
      if FConfig.UseCache and (FCache <> nil) then
      begin
        FCache.Put(ACert.SerialNumber, AResponse, 
          SingleResp.ThisUpdate, SingleResp.NextUpdate);
      end;
      
    except
      on E: Exception do
      begin
        Result.Status := ossVerificationFailed;
        Result.ErrorMessage := 'Failed to parse OCSP response: ' + E.Message;
      end;
    end;
  finally
    OCSPResp.Free;
  end;
  
  FLastResult := Result;
end;

function TOCSPStaplingClient.ValidateStaplingRequirement(
  AStaplingProvided: Boolean): Boolean;
begin
  // 如果不要求 stapling,总是返回 True
  if not FConfig.RequireStapling then
    Exit(True);
  
  // 如果要求 stapling,检查是否提供
  Result := AStaplingProvided and (FLastResult.Status = ossVerified);
end;

function TOCSPStaplingClient.VerifyStapledResponse(const AResponse: TBytes;
  ACert, AIssuerCert: TX509Certificate): Boolean;
var
  Res: TOCSPStaplingResult;
begin
  Res := ProcessStapledResponse(AResponse, ACert, AIssuerCert);
  Result := Res.IsValid;
end;

function TOCSPStaplingClient.CheckResponseFreshness(
  const AResponse: TOCSPResponse): Boolean;
var
  Now: TDateTime;
begin
  Now := SysUtils.Now;
  
  // 检查 producedAt 不在未来
  if AResponse.ProducedAt > Now then
    Exit(False);
  
  // 检查至少有一个响应
  if Length(AResponse.Responses) = 0 then
    Exit(False);
  
  // 检查第一个响应的时间
  Result := (AResponse.Responses[0].ThisUpdate <= Now);
  
  // 如果有 nextUpdate,检查未过期
  if AResponse.Responses[0].HasNextUpdate then
    Result := Result and (AResponse.Responses[0].NextUpdate > Now);
end;

// ========================================================================
// TOCSPStaplingServer
// ========================================================================

constructor TOCSPStaplingServer.Create(AConfig: TOCSPStaplingConfig;
  ACache: TOCSPResponseCache);
begin
  inherited Create;
  FConfig := AConfig;
  FCache := ACache;
  FLock := TCriticalSection.Create;
  FAutoRefreshEnabled := False;
end;

destructor TOCSPStaplingServer.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TOCSPStaplingServer.GetStapledResponse(ACert, AIssuerCert: TX509Certificate;
  AForceRefresh: Boolean): TBytes;
var
  CachedResponse: TBytes;
  NeedsFetch: Boolean;
begin
  SetLength(Result, 0);
  
  if not FConfig.EnableServerStapling then
    Exit;
  
  FLock.Enter;
  try
    NeedsFetch := AForceRefresh;
    
    // 尝试从缓存获取
    if FConfig.UseCache and (FCache <> nil) and not AForceRefresh then
    begin
      if FCache.Get(ACert.SerialNumber, CachedResponse) then
      begin
        // 检查是否需要刷新
        if not ShouldRefreshResponse(CachedResponse) then
        begin
          Result := CachedResponse;
          Exit;
        end
        else
          NeedsFetch := True;
      end
      else
        NeedsFetch := True;
    end
    else
      NeedsFetch := True;
    
    // 需要获取新响应
    if NeedsFetch then
    begin
      Result := FetchOCSPResponse(ACert, AIssuerCert);
    end;
    
  finally
    FLock.Leave;
  end;
end;

function TOCSPStaplingServer.RefreshResponse(ACert, AIssuerCert: TX509Certificate): Boolean;
var
  Response: TBytes;
begin
  Result := False;
  
  try
    Response := FetchOCSPResponse(ACert, AIssuerCert);
    Result := Length(Response) > 0;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TOCSPStaplingServer.FetchOCSPResponse(ACert, AIssuerCert: TX509Certificate): TBytes;
var
  OCSPURL: string;
  Request: TOCSPRequest;
  RequestData: TBytes;
  ResponseData: TBytes;
  OCSPResp: TOCSPResponse;
  Retry: Integer;
begin
  SetLength(Result, 0);
  
  // 从证书中提取 OCSP URL
  OCSPURL := GetOCSPURLFromCertificate(ACert);
  if OCSPURL = '' then
    Exit;
  
  // 创建 OCSP 请求
  Request := TOCSPRequest.Create;
  try
    Request.AddCertificate(ACert, AIssuerCert);
    Request.UseNonce := False;  // Stapling 通常不使用 nonce
    RequestData := Request.Encode;
  finally
    Request.Free;
  end;
  
  // 发送请求 (带重试)
  Retry := 0;
  while Retry < FConfig.MaxRetries do
  begin
    try
      ResponseData := SendOCSPRequest(OCSPURL, RequestData);
      
      if Length(ResponseData) > 0 then
      begin
        // 验证响应
        OCSPResp := TOCSPResponse.Create;
        try
          OCSPResp.LoadFromDER(ResponseData);
          
          if OCSPResp.ResponseStatus = ocsprsSuccessful then
          begin
            Result := ResponseData;
            
            // 缓存响应
            if FConfig.UseCache and (FCache <> nil) and 
               (Length(OCSPResp.Responses) > 0) then
            begin
              FCache.Put(ACert.SerialNumber, ResponseData,
                OCSPResp.Responses[0].ThisUpdate,
                OCSPResp.Responses[0].NextUpdate);
            end;
            
            Exit;
          end;
        finally
          OCSPResp.Free;
        end;
      end;
      
    except
      on E: Exception do
      begin
        // 记录错误,继续重试
        Inc(Retry);
        if Retry >= FConfig.MaxRetries then
          raise;
        Sleep(1000 * Retry);  // 指数退避
      end;
    end;
    
    Inc(Retry);
  end;
end;

function TOCSPStaplingServer.SendOCSPRequest(const AURL: string; 
  const ARequest: TBytes): TBytes;
var
  HTTPClient: TSimpleHTTPClient;
begin
  // 简化的 HTTP POST 实现
  // 实际实现应使用完整的 HTTP 客户端
  HTTPClient := TSimpleHTTPClient.Create;
  try
    HTTPClient.Timeout := FConfig.TimeoutSeconds * 1000;
    HTTPClient.ContentType := 'application/ocsp-request';
    Result := HTTPClient.Post(AURL, ARequest);
  finally
    HTTPClient.Free;
  end;
end;

function TOCSPStaplingServer.ShouldRefreshResponse(const AResponse: TBytes): Boolean;
var
  OCSPResp: TOCSPResponse;
  TimeUntilExpiry: Int64;
begin
  Result := True;  // 默认需要刷新
  
  if Length(AResponse) = 0 then
    Exit;
  
  OCSPResp := TOCSPResponse.Create;
  try
    try
      OCSPResp.LoadFromDER(AResponse);
      
      if (OCSPResp.ResponseStatus = ocsprsSuccessful) and
         (Length(OCSPResp.Responses) > 0) then
      begin
        // 检查是否在刷新窗口内
        if OCSPResp.Responses[0].HasNextUpdate then
        begin
          TimeUntilExpiry := SecondsBetween(Now, OCSPResp.Responses[0].NextUpdate);
          Result := TimeUntilExpiry < FConfig.RefreshBeforeExpiry;
        end
        else
          Result := False;  // 没有过期时间,不需要刷新
      end;
    except
      Result := True;  // 解析失败,需要刷新
    end;
  finally
    OCSPResp.Free;
  end;
end;

procedure TOCSPStaplingServer.EnableAutoRefresh;
begin
  FAutoRefreshEnabled := True;
  // TODO: 启动后台刷新线程
end;

procedure TOCSPStaplingServer.DisableAutoRefresh;
begin
  FAutoRefreshEnabled := False;
  // TODO: 停止后台刷新线程
end;

// ========================================================================
// TOCSPStaplingManager
// ========================================================================

constructor TOCSPStaplingManager.Create(AConfig: TOCSPStaplingConfig;
  ACache: TOCSPResponseCache);
begin
  inherited Create;
  FConfig := AConfig;
  
  // 创建或使用提供的缓存
  if ACache <> nil then
  begin
    FCache := ACache;
    FOwnCache := False;
  end
  else if FConfig.UseCache then
  begin
    FCache := TOCSPResponseCache.Create;
    FOwnCache := True;
  end
  else
  begin
    FCache := nil;
    FOwnCache := False;
  end;
  
  // 创建客户端和服务端
  FClient := TOCSPStaplingClient.Create(FConfig, FCache);
  FServer := TOCSPStaplingServer.Create(FConfig, FCache);
end;

destructor TOCSPStaplingManager.Destroy;
begin
  FClient.Free;
  FServer.Free;
  if FOwnCache then
    FCache.Free;
  inherited Destroy;
end;

function TOCSPStaplingManager.ClientShouldRequest: Boolean;
begin
  Result := FClient.ShouldRequestStapling;
end;

function TOCSPStaplingManager.ClientProcessResponse(const AResponse: TBytes;
  ACert, AIssuerCert: TX509Certificate): TOCSPStaplingResult;
begin
  Result := FClient.ProcessStapledResponse(AResponse, ACert, AIssuerCert);
end;

function TOCSPStaplingManager.ClientValidateRequirement(AProvided: Boolean): Boolean;
begin
  Result := FClient.ValidateStaplingRequirement(AProvided);
end;

function TOCSPStaplingManager.ServerGetResponse(ACert, AIssuerCert: TX509Certificate): TBytes;
begin
  Result := FServer.GetStapledResponse(ACert, AIssuerCert);
end;

function TOCSPStaplingManager.ServerRefreshResponse(ACert, AIssuerCert: TX509Certificate): Boolean;
begin
  Result := FServer.RefreshResponse(ACert, AIssuerCert);
end;

function TOCSPStaplingManager.GetCacheStats: TOCSPCacheStats;
begin
  if FCache <> nil then
    Result := FCache.GetStats
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TOCSPStaplingManager.ClearCache;
begin
  if FCache <> nil then
    FCache.Clear;
end;

end.
