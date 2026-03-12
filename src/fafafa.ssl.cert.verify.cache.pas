unit fafafa.ssl.cert.verify.cache;

{$mode ObjFPC}{$H+}

{
  证书验证结果缓存

  特性：
  - 基于证书指纹（SHA-256）的缓存
  - 线程安全（使用临界区）
  - LRU 驱逐策略
  - 可配置缓存大小和 TTL
  - 原子操作，零拷贝

  性能目标：
  - 缓存命中：<0.1 ms
  - 缓存未命中：正常验证（10-50 ms）
  - 10x 握手性能提升（重复证书场景）

  设计原则：
  - 简洁 API（Get/Set）
  - 线程安全
  - 内存高效
}

interface

uses
  SysUtils, Classes, SyncObjs, DateUtils,
  fafafa.ssl.openssl.base;

type
  { 验证结果 }
  TCertVerifyResult = record
    Valid: Boolean;
    ErrorCode: Integer;
    ErrorMessage: string;
    VerifiedAt: TDateTime;
  end;

  { 缓存条目 }
  TCacheEntry = record
    Fingerprint: array[0..31] of Byte;  // SHA-256 (32 bytes)
    Result: TCertVerifyResult;
    LastAccess: TDateTime;
    HitCount: Integer;
  end;

  { 证书验证缓存（线程安全，LRU）}
  TCertVerifyCache = class
  private
    FLock: TCriticalSection;
    FEntries: array of TCacheEntry;
    FCapacity: Integer;
    FCount: Integer;
    FTTL: Integer;  // 秒

    FHits: Int64;
    FMisses: Int64;

    function ComputeFingerprint(ACert: PX509): TBytes;
    function FindEntry(const AFingerprint: TBytes): Integer;
    procedure EvictOldest;
    function IsExpired(const AEntry: TCacheEntry): Boolean;

  public
    constructor Create(ACapacity: Integer = 1000; ATTL: Integer = 3600);
    destructor Destroy; override;

    { 缓存操作 }
    function TryGet(ACert: PX509; out AResult: TCertVerifyResult): Boolean;
    procedure Put(ACert: PX509; const AResult: TCertVerifyResult);
    procedure Clear;

    { 统计 }
    function GetHitRate: Double;
    function GetSize: Integer;
    procedure GetStats(out AHits, AMisses, ASize: Int64);

    { 属性 }
    property Capacity: Integer read FCapacity;
    property TTL: Integer read FTTL write FTTL;
  end;

  { 全局缓存实例 }
function GetGlobalCertVerifyCache: TCertVerifyCache;

implementation

uses
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.loader;

var
  GlobalCache: TCertVerifyCache = nil;
  GlobalCacheLock: TCriticalSection = nil;

function GetGlobalCertVerifyCache: TCertVerifyCache;
begin
  GlobalCacheLock.Enter;
  try
    if GlobalCache = nil then
      GlobalCache := TCertVerifyCache.Create;
    Result := GlobalCache;
  finally
    GlobalCacheLock.Leave;
  end;
end;

{ TCertVerifyCache }

constructor TCertVerifyCache.Create(ACapacity: Integer; ATTL: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  if ACapacity > 0 then
    FCapacity := ACapacity
  else
    FCapacity := 0;
  FTTL := ATTL;
  FCount := 0;
  FHits := 0;
  FMisses := 0;
  SetLength(FEntries, FCapacity);
end;

destructor TCertVerifyCache.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TCertVerifyCache.ComputeFingerprint(ACert: PX509): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LDigest: array[0..31] of Byte;
  LLen: Cardinal;
  LDerData: TBytes;
  LDerLen: Integer;
  LEncodedLen: Integer;
  LDerPtr: PByte;
  LDerWritePtr: PByte;
  LMD: PEVP_MD;
begin
  Result := nil;
  if ACert = nil then
    Exit;
  if not Assigned(i2d_X509) then
    Exit;

  // 1) 获取完整 DER 编码（避免固定缓冲区截断）
  LDerLen := i2d_X509(ACert, nil);
  if LDerLen <= 0 then
    Exit;

  SetLength(LDerData, LDerLen);
  LDerPtr := @LDerData[0];
  LDerWritePtr := LDerPtr;
  LEncodedLen := i2d_X509(ACert, @LDerWritePtr);
  if LEncodedLen <> LDerLen then
    Exit;

  // 2) 计算 SHA-256
  if (not Assigned(EVP_MD_CTX_new)) or (not Assigned(EVP_MD_CTX_free)) or
    (not Assigned(EVP_DigestInit_ex)) or (not Assigned(EVP_DigestUpdate)) or
    (not Assigned(EVP_DigestFinal_ex)) or (not Assigned(EVP_sha256)) then
    Exit;

  LMD := EVP_sha256();
  if LMD = nil then
    Exit;

  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    Exit;

  try
    if EVP_DigestInit_ex(LCtx, LMD, nil) <> 1 then
      Exit;
    if EVP_DigestUpdate(LCtx, LDerPtr, Cardinal(LDerLen)) <> 1 then
      Exit;

    LLen := 32;
    if EVP_DigestFinal_ex(LCtx, @LDigest[0], LLen) <> 1 then
      Exit;

    SetLength(Result, 32);
    Move(LDigest[0], Result[0], 32);
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

function TCertVerifyCache.FindEntry(const AFingerprint: TBytes): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Length(AFingerprint) <> 32 then
    Exit;

  for i := 0 to FCount - 1 do
  begin
    if CompareMem(@FEntries[i].Fingerprint[0], @AFingerprint[0], 32) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCertVerifyCache.IsExpired(const AEntry: TCacheEntry): Boolean;
begin
  if FTTL <= 0 then
    Exit(True);
  Result := SecondsBetween(Now, AEntry.Result.VerifiedAt) >= FTTL;
end;

procedure TCertVerifyCache.EvictOldest;
var
  OldestIdx: Integer;
  OldestTime: TDateTime;
  i: Integer;
begin
  if FCount = 0 then
    Exit;

  OldestIdx := 0;
  OldestTime := FEntries[0].LastAccess;

  for i := 1 to FCount - 1 do
  begin
    if FEntries[i].LastAccess < OldestTime then
    begin
      OldestIdx := i;
      OldestTime := FEntries[i].LastAccess;
    end;
  end;

  // 移除最旧的条目
  if OldestIdx < FCount - 1 then
    Move(FEntries[OldestIdx + 1], FEntries[OldestIdx],
      (FCount - OldestIdx - 1) * SizeOf(TCacheEntry));

  Dec(FCount);
end;

function TCertVerifyCache.TryGet(ACert: PX509; out AResult: TCertVerifyResult): Boolean;
var
  LFingerprint: TBytes;
  LIdx: Integer;
begin
  Result := False;
  LFingerprint := ComputeFingerprint(ACert);

  FLock.Enter;
  try
    if Length(LFingerprint) <> 32 then
    begin
      Inc(FMisses);
      Exit;
    end;

    LIdx := FindEntry(LFingerprint);
    if LIdx < 0 then
    begin
      Inc(FMisses);
      Exit;
    end;

    // 检查是否过期
    if IsExpired(FEntries[LIdx]) then
    begin
      // 删除过期条目
      if LIdx < FCount - 1 then
        Move(FEntries[LIdx + 1], FEntries[LIdx],
          (FCount - LIdx - 1) * SizeOf(TCacheEntry));
      Dec(FCount);
      Inc(FMisses);
      Exit;
    end;

    // 缓存命中
    AResult := FEntries[LIdx].Result;
    FEntries[LIdx].LastAccess := Now;
    Inc(FEntries[LIdx].HitCount);
    Inc(FHits);
    Result := True;

  finally
    FLock.Leave;
  end;
end;

procedure TCertVerifyCache.Put(ACert: PX509; const AResult: TCertVerifyResult);
var
  LFingerprint: TBytes;
  LIdx: Integer;
begin
  if FCapacity <= 0 then
    Exit;

  LFingerprint := ComputeFingerprint(ACert);
  if Length(LFingerprint) <> 32 then
    Exit;

  FLock.Enter;
  try
    // 检查是否已存在
    LIdx := FindEntry(LFingerprint);
    if LIdx >= 0 then
    begin
      // 更新现有条目
      FEntries[LIdx].Result := AResult;
      FEntries[LIdx].LastAccess := Now;
      Exit;
    end;

    // 添加新条目
    if FCount >= FCapacity then
      EvictOldest;

    LIdx := FCount;
    Inc(FCount);

    Move(LFingerprint[0], FEntries[LIdx].Fingerprint[0], 32);
    FEntries[LIdx].Result := AResult;
    FEntries[LIdx].LastAccess := Now;
    FEntries[LIdx].HitCount := 0;

  finally
    FLock.Leave;
  end;
end;

procedure TCertVerifyCache.Clear;
begin
  FLock.Enter;
  try
    FCount := 0;
    FHits := 0;
    FMisses := 0;
  finally
    FLock.Leave;
  end;
end;

function TCertVerifyCache.GetHitRate: Double;
var
  LTotal: Int64;
begin
  FLock.Enter;
  try
    LTotal := FHits + FMisses;
    if LTotal = 0 then
      Result := 0.0
    else
      Result := (FHits * 100.0) / LTotal;
  finally
    FLock.Leave;
  end;
end;

function TCertVerifyCache.GetSize: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

procedure TCertVerifyCache.GetStats(out AHits, AMisses, ASize: Int64);
begin
  FLock.Enter;
  try
    AHits := FHits;
    AMisses := FMisses;
    ASize := FCount;
  finally
    FLock.Leave;
  end;
end;

initialization
  GlobalCacheLock := TCriticalSection.Create;

finalization
  if GlobalCache <> nil then
  begin
    GlobalCache.Free;
    GlobalCache := nil;
  end;
  if GlobalCacheLock <> nil then
  begin
    GlobalCacheLock.Free;
    GlobalCacheLock := nil;
  end;

end.
