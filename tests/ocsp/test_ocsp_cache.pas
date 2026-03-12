program test_ocsp_cache;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, DateUtils,
  fafafa.ssl.ocsp.cache;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure StartTest(const TestName: string);
begin
  Inc(TotalTests);
  Write('[', TotalTests, '] ', TestName, '... ');
end;

procedure PassTest;
begin
  Inc(PassedTests);
  WriteLn('PASS');
end;

procedure FailTest(const Reason: string);
begin
  Inc(FailedTests);
  WriteLn('FAIL: ', Reason);
end;

// ========================================================================
// 辅助函数
// ========================================================================

function CreateTestSerialNumber(ID: Integer): TBytes;
var
  S: string;
  I: Integer;
begin
  // 创建一个简单的测试序列号
  S := IntToStr(ID);
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I - 1] := Ord(S[I]);
end;

function CreateTestResponse(Size: Integer): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
    Result[I] := Byte((I mod 256));
end;

// ========================================================================
// 测试: 缓存创建
// ========================================================================

procedure TestCacheCreation;
var
  Cache: TOCSPResponseCache;
begin
  StartTest('Create OCSP response cache');
  try
    Cache := TOCSPResponseCache.Create;
    try
      if Cache = nil then
        FailTest('Cache is nil')
      else if Cache.GetCount <> 0 then
        FailTest('New cache should be empty')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCacheCreationWithParams;
var
  Cache: TOCSPResponseCache;
begin
  StartTest('Create cache with custom parameters');
  try
    Cache := TOCSPResponseCache.Create(500, 7200);  // 500 条目，2小时 TTL
    try
      if Cache.MaxEntries <> 500 then
        FailTest('MaxEntries should be 500')
      else if Cache.DefaultTTL <> 7200 then
        FailTest('DefaultTTL should be 7200')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: Put 和 Get 操作
// ========================================================================

procedure TestPutAndGet;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Put and Get cache entry');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(100);

      // 放入缓存
      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

      // 取出缓存
      if not Cache.Get(SerialNumber, Retrieved) then
        FailTest('Failed to get cached entry')
      else if Length(Retrieved) <> Length(Response) then
        FailTest('Retrieved response length mismatch')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetNonexistent;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response: TBytes;
begin
  StartTest('Get nonexistent entry returns false');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(999);

      if Cache.Get(SerialNumber, Response) then
        FailTest('Should return false for nonexistent entry')
      else if Length(Response) <> 0 then
        FailTest('Response should be empty')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestPutEmptyResponse;
var
  Cache: TOCSPResponseCache;
  SerialNumber, EmptyResponse, Retrieved: TBytes;
begin
  StartTest('Put empty response is ignored');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      SetLength(EmptyResponse, 0);

      // 尝试放入空响应
      Cache.Put(SerialNumber, EmptyResponse, Now, IncHour(Now, 1));

      // 应该无法取出
      if Cache.Get(SerialNumber, Retrieved) then
        FailTest('Empty response should not be cached')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestEmptySerialNumberRejected;
var
  Cache: TOCSPResponseCache;
  EmptySerial, Response, Retrieved: TBytes;
begin
  StartTest('Empty serial number should not be cached');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SetLength(EmptySerial, 0);
      Response := CreateTestResponse(32);

      Cache.Put(EmptySerial, Response, Now, IncHour(Now, 1));

      if Cache.GetCount <> 0 then
        FailTest('Empty serial should not create cache entry')
      else if Cache.Contains(EmptySerial) then
        FailTest('Contains should return false for empty serial')
      else if Cache.Get(EmptySerial, Retrieved) then
        FailTest('Get should return false for empty serial')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestContains;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response: TBytes;
begin
  StartTest('Contains check');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 初始应该不包含
      if Cache.Contains(SerialNumber) then
        FailTest('Should not contain entry initially')
      else
      begin
        // 放入后应该包含
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

        if not Cache.Contains(SerialNumber) then
          FailTest('Should contain entry after Put')
        else
          PassTest;
      end;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestSmallMaxEntriesStillStoresEntry;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Small MaxEntries should still store at least one entry');
  try
    Cache := TOCSPResponseCache.Create(1, 3600);
    try
      SerialNumber := CreateTestSerialNumber(1234);
      Response := CreateTestResponse(40);

      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

      if not Cache.Get(SerialNumber, Retrieved) then
        FailTest('Entry should be retrievable when MaxEntries=1')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 过期处理
// ========================================================================

procedure TestExpiredEntryReturnsMiss;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Expired entry returns cache miss');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 放入一个已经过期的条目
      Cache.Put(SerialNumber, Response, IncHour(Now, -2), IncHour(Now, -1));

      // 应该返回 miss
      if Cache.Get(SerialNumber, Retrieved) then
        FailTest('Expired entry should return miss')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestAutoExpireOnGet;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Auto expire on Get removes entry');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 放入一个即将过期的条目
      Cache.Put(SerialNumber, Response, IncSecond(Now, -10), IncSecond(Now, -1));

      // Get 应该返回 miss 并删除条目
      if Cache.Get(SerialNumber, Retrieved) then
        FailTest('Expired entry should be removed')
      else if Cache.Contains(SerialNumber) then
        FailTest('Entry should be removed after expired Get')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestContainsExpiredEntry;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response: TBytes;
begin
  StartTest('Contains should reject expired entry and cleanup');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(77);
      Response := CreateTestResponse(32);

      Cache.Put(SerialNumber, Response, IncSecond(Now, -10), IncSecond(Now, -1));

      if Cache.Contains(SerialNumber) then
        FailTest('Contains should return false for expired entry')
      else if Cache.GetCount <> 0 then
        FailTest('Expired entry should be removed by Contains check')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 更新和删除
// ========================================================================

procedure TestUpdateEntry;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response1, Response2, Retrieved: TBytes;
begin
  StartTest('Update existing entry');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response1 := CreateTestResponse(50);
      Response2 := CreateTestResponse(100);

      // 放入第一个响应
      Cache.Put(SerialNumber, Response1, Now, IncHour(Now, 1));

      // 更新为第二个响应
      Cache.Put(SerialNumber, Response2, Now, IncHour(Now, 2));

      // 应该得到第二个响应
      if not Cache.Get(SerialNumber, Retrieved) then
        FailTest('Failed to get updated entry')
      else if Length(Retrieved) <> 100 then
        FailTest('Should get updated response')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestRemoveEntry;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Remove entry');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 放入
      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

      // 删除
      Cache.Remove(SerialNumber);

      // 应该无法取出
      if Cache.Get(SerialNumber, Retrieved) then
        FailTest('Should not get removed entry')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestClear;
var
  Cache: TOCSPResponseCache;
  I: Integer;
  SerialNumber, Response: TBytes;
begin
  StartTest('Clear all entries');
  try
    Cache := TOCSPResponseCache.Create;
    try
      // 放入多个条目
      for I := 1 to 10 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50);
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;

      // 清空
      Cache.Clear;

      // 检查计数
      if Cache.GetCount <> 0 then
        FailTest('Cache should be empty after Clear')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: LRU 驱逐策略
// ========================================================================

procedure TestLRUEviction;
var
  Cache: TOCSPResponseCache;
  I: Integer;
  SerialNumber, Response, Retrieved: TBytes;
  MaxEntries: Integer;
begin
  StartTest('LRU eviction when exceeding max entries');
  try
    MaxEntries := 100;
    Cache := TOCSPResponseCache.Create(MaxEntries, 3600);
    try
      // 放入超过最大数量的条目
      for I := 1 to MaxEntries + 50 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50);
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;

      // 检查缓存大小不超过限制（考虑分片，允许一定误差）
      if Cache.GetCount > MaxEntries + 16 then  // 16 是分片数量
        FailTest('Cache should not exceed max entries significantly, got: ' + IntToStr(Cache.GetCount))
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 统计信息
// ========================================================================

procedure TestCacheStats;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
  Stats: TOCSPCacheStats;
begin
  StartTest('Cache statistics');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 放入一个条目
      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

      // 命中
      Cache.Get(SerialNumber, Retrieved);
      Cache.Get(SerialNumber, Retrieved);

      // 未命中
      Cache.Get(CreateTestSerialNumber(999), Retrieved);

      // 检查统计
      Stats := Cache.GetStats;

      if Stats.TotalRequests <> 3 then
        FailTest('TotalRequests should be 3, got: ' + IntToStr(Stats.TotalRequests))
      else if Stats.CacheHits <> 2 then
        FailTest('CacheHits should be 2, got: ' + IntToStr(Stats.CacheHits))
      else if Stats.CacheMisses <> 1 then
        FailTest('CacheMisses should be 1, got: ' + IntToStr(Stats.CacheMisses))
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestHitRate;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
  Stats: TOCSPCacheStats;
  ExpectedRate: Double;
begin
  StartTest('Hit rate calculation');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));

      // 3 次命中，1 次未命中
      Cache.Get(SerialNumber, Retrieved);
      Cache.Get(SerialNumber, Retrieved);
      Cache.Get(SerialNumber, Retrieved);
      Cache.Get(CreateTestSerialNumber(999), Retrieved);

      Stats := Cache.GetStats;
      ExpectedRate := 75.0;  // 3/4 * 100

      if Abs(Stats.HitRate - ExpectedRate) > 0.1 then
        FailTest('HitRate should be ~75%, got: ' + FloatToStr(Stats.HitRate))
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestResetStats;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
  Stats: TOCSPCacheStats;
begin
  StartTest('Reset statistics');
  try
    Cache := TOCSPResponseCache.Create;
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      Cache.Get(SerialNumber, Retrieved);

      // 重置统计
      Cache.ResetStats;

      Stats := Cache.GetStats;

      if Stats.TotalRequests <> 0 then
        FailTest('TotalRequests should be 0 after reset')
      else if Stats.CacheHits <> 0 then
        FailTest('CacheHits should be 0 after reset')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 并发操作
// ========================================================================

type
  TConcurrentPutThread = class(TThread)
  private
    FCache: TOCSPResponseCache;
    FPutThreadID: Integer;
    FPutIterations: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TOCSPResponseCache; AThreadID: Integer; AIterations: Integer);
  end;

constructor TConcurrentPutThread.Create(ACache: TOCSPResponseCache; AThreadID: Integer; AIterations: Integer);
begin
  inherited Create(True);
  FCache := ACache;
  FPutThreadID := AThreadID;
  FPutIterations := AIterations;
  FreeOnTerminate := False;
end;

procedure TConcurrentPutThread.Execute;
var
  I: Integer;
  SerialNumber, Response: TBytes;
begin
  for I := 1 to FPutIterations do
  begin
    SerialNumber := CreateTestSerialNumber(FPutThreadID * 10000 + I);
    Response := CreateTestResponse(50 + (I mod 50));
    FCache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
  end;
end;

procedure TestConcurrentPut;
const
  ThreadCount = 4;
  IterationsPerThread = 100;
var
  Cache: TOCSPResponseCache;
  Threads: array[0..ThreadCount-1] of TConcurrentPutThread;
  I: Integer;
begin
  StartTest('Concurrent Put operations');
  try
    Cache := TOCSPResponseCache.Create(10000, 3600);
    try
      // 创建并启动线程
      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I] := TConcurrentPutThread.Create(Cache, I, IterationsPerThread);
        Threads[I].Start;
      end;

      // 等待所有线程完成
      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I].WaitFor;
        Threads[I].Free;
      end;

      // 验证缓存中有条目
      if Cache.GetCount = 0 then
        FailTest('Cache should have entries after concurrent Put')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

type
  TConcurrentGetThread = class(TThread)
  private
    FCache: TOCSPResponseCache;
    FGetThreadID: Integer;
    FGetIterations: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TOCSPResponseCache; AThreadID: Integer; AIterations: Integer);
  end;

constructor TConcurrentGetThread.Create(ACache: TOCSPResponseCache; AThreadID: Integer; AIterations: Integer);
begin
  inherited Create(True);
  FCache := ACache;
  FGetThreadID := AThreadID;
  FGetIterations := AIterations;
  FreeOnTerminate := False;
end;

procedure TConcurrentGetThread.Execute;
var
  I: Integer;
  SerialNumber, Response: TBytes;
begin
  for I := 1 to FGetIterations do
  begin
    SerialNumber := CreateTestSerialNumber((I mod 100) + 1);  // 查询已存在的键
    FCache.Get(SerialNumber, Response);
  end;
end;

procedure TestConcurrentGet;
const
  ThreadCount = 4;
  IterationsPerThread = 100;
var
  Cache: TOCSPResponseCache;
  Threads: array[0..ThreadCount-1] of TConcurrentGetThread;
  I: Integer;
  SerialNumber, Response: TBytes;
begin
  StartTest('Concurrent Get operations');
  try
    Cache := TOCSPResponseCache.Create(10000, 3600);
    try
      // 预填充缓存
      for I := 1 to 100 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50);
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;

      // 创建并启动线程
      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I] := TConcurrentGetThread.Create(Cache, I, IterationsPerThread);
        Threads[I].Start;
      end;

      // 等待所有线程完成
      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I].WaitFor;
        Threads[I].Free;
      end;

      // 验证没有崩溃即为成功
      PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

type
  TGetPutStressThread = class(TThread)
  private
    FCache: TOCSPResponseCache;
    FIterations: Integer;
    FCompleted: Boolean;
    FProgress: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TOCSPResponseCache; AIterations: Integer);
    property Completed: Boolean read FCompleted;
    property Progress: Integer read FProgress;
  end;

  TStatsStressThread = class(TThread)
  private
    FCache: TOCSPResponseCache;
    FIterations: Integer;
    FCompleted: Boolean;
    FProgress: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TOCSPResponseCache; AIterations: Integer);
    property Completed: Boolean read FCompleted;
    property Progress: Integer read FProgress;
  end;

constructor TGetPutStressThread.Create(ACache: TOCSPResponseCache; AIterations: Integer);
begin
  inherited Create(True);
  FCache := ACache;
  FIterations := AIterations;
  FCompleted := False;
  FProgress := 0;
  FreeOnTerminate := False;
end;

procedure TGetPutStressThread.Execute;
var
  I: Integer;
  SerialNumber, Response, Retrieved: TBytes;
begin
  for I := 1 to FIterations do
  begin
    if Terminated then
      Break;

    SetLength(SerialNumber, 4);
    SerialNumber[0] := 1;
    SerialNumber[1] := Byte(I and $FF);
    SerialNumber[2] := Byte((I shr 8) and $FF);
    SerialNumber[3] := Byte((I shr 16) and $FF);

    Response := CreateTestResponse(64);
    FCache.Put(SerialNumber, Response, Now, IncSecond(Now, 30));
    FCache.Get(SerialNumber, Retrieved);

    FProgress := I;
  end;
  FCompleted := True;
end;

constructor TStatsStressThread.Create(ACache: TOCSPResponseCache; AIterations: Integer);
begin
  inherited Create(True);
  FCache := ACache;
  FIterations := AIterations;
  FCompleted := False;
  FProgress := 0;
  FreeOnTerminate := False;
end;

procedure TStatsStressThread.Execute;
var
  I: Integer;
  Stats: TOCSPCacheStats;
begin
  for I := 1 to FIterations do
  begin
    if Terminated then
      Break;

    Stats := FCache.GetStats;
    if Stats.TotalEntries < 0 then
      raise Exception.Create('Invalid TotalEntries');

    if (I mod 1000) = 0 then
      FCache.ResetStats;

    FProgress := I;
  end;
  FCompleted := True;
end;

procedure TestConcurrentStatsAndGetNoDeadlock;
const
  StressIterations = 2000000;
  MaxObserveMs = 12000;
  StallMs = 1200;
  ProgressTarget = 25000;
var
  Cache: TOCSPResponseCache;
  GetPutThread: TGetPutStressThread;
  StatsThread: TStatsStressThread;
  StartTick, LastProgressTick: QWord;
  LastGetProgress, LastStatsProgress: Integer;
  Success: Boolean;
  CanCleanup: Boolean;
begin
  StartTest('Concurrent Get/Put and GetStats/ResetStats should not deadlock');
  Cache := nil;
  GetPutThread := nil;
  StatsThread := nil;
  CanCleanup := False;
  Success := False;
  LastGetProgress := 0;
  LastStatsProgress := 0;

  try
    Cache := TOCSPResponseCache.Create(20000, 3600);
    GetPutThread := TGetPutStressThread.Create(Cache, StressIterations);
    StatsThread := TStatsStressThread.Create(Cache, StressIterations);

    GetPutThread.Start;
    StatsThread.Start;

    StartTick := GetTickCount64;
    LastProgressTick := StartTick;

    while (GetTickCount64 - StartTick) < MaxObserveMs do
    begin
      if (GetPutThread.Progress <> LastGetProgress) or
         (StatsThread.Progress <> LastStatsProgress) then
      begin
        LastGetProgress := GetPutThread.Progress;
        LastStatsProgress := StatsThread.Progress;
        LastProgressTick := GetTickCount64;
      end;

      if (GetTickCount64 - LastProgressTick) > StallMs then
      begin
        FailTest('Potential deadlock detected: concurrent progress stalled');
        Exit;
      end;

      if (LastGetProgress >= ProgressTarget) and
         (LastStatsProgress >= ProgressTarget) then
      begin
        Success := True;
        Break;
      end;

      Sleep(10);
    end;

    if not Success then
    begin
      FailTest('Stress observation timeout before reaching safe progress target');
      Exit;
    end;

    GetPutThread.Terminate;
    StatsThread.Terminate;
    GetPutThread.WaitFor;
    StatsThread.WaitFor;
    CanCleanup := True;
    PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;

  if CanCleanup then
  begin
    GetPutThread.Free;
    StatsThread.Free;
    Cache.Free;
  end;
end;

// ========================================================================
// 测试: 持久化
// ========================================================================

procedure TestSaveToFile;
var
  Cache: TOCSPResponseCache;
  I: Integer;
  SerialNumber, Response: TBytes;
  FileName: string;
begin
  StartTest('Save cache to file');
  try
    FileName := 'test_ocsp_cache.dat';
    Cache := TOCSPResponseCache.Create;
    try
      // 放入一些条目
      for I := 1 to 10 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50 + I * 10);
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;

      // 保存到文件
      if not Cache.SaveToFile(FileName) then
        FailTest('Failed to save cache to file')
      else if not FileExists(FileName) then
        FailTest('Cache file was not created')
      else
      begin
        PassTest;
        DeleteFile(FileName);
      end;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadFromFile;
var
  Cache1, Cache2: TOCSPResponseCache;
  I: Integer;
  SerialNumber, Response, Retrieved: TBytes;
  FileName: string;
begin
  StartTest('Load cache from file');
  try
    FileName := 'test_ocsp_cache_load.dat';

    // 创建并保存缓存
    Cache1 := TOCSPResponseCache.Create;
    try
      for I := 1 to 5 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50);
        Cache1.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;
      Cache1.SaveToFile(FileName);
    finally
      Cache1.Free;
    end;

    // 加载缓存
    Cache2 := TOCSPResponseCache.Create;
    try
      if not Cache2.LoadFromFile(FileName) then
        FailTest('Failed to load cache from file')
      else if Cache2.GetCount = 0 then
        FailTest('Loaded cache should have entries')
      else
      begin
        // 验证可以读取条目
        SerialNumber := CreateTestSerialNumber(1);
        if not Cache2.Get(SerialNumber, Retrieved) then
          FailTest('Failed to get entry from loaded cache')
        else
          PassTest;
      end;
    finally
      Cache2.Free;
      DeleteFile(FileName);
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadFromNonexistentFile;
var
  Cache: TOCSPResponseCache;
begin
  StartTest('Load from nonexistent file returns false');
  try
    Cache := TOCSPResponseCache.Create;
    try
      if Cache.LoadFromFile('nonexistent_cache_file.dat') then
        FailTest('Should return false for nonexistent file')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 边界条件
// ========================================================================

procedure TestDefaultTTLUsed;
var
  Cache: TOCSPResponseCache;
  SerialNumber, Response, Retrieved: TBytes;
begin
  StartTest('Default TTL used when NextUpdate is 0');
  try
    Cache := TOCSPResponseCache.Create(1000, 3600);  // 1 小时 TTL
    try
      SerialNumber := CreateTestSerialNumber(1);
      Response := CreateTestResponse(50);

      // 放入时 NextUpdate = 0
      Cache.Put(SerialNumber, Response, Now, 0);

      // 应该可以取出
      if not Cache.Get(SerialNumber, Retrieved) then
        FailTest('Entry should be available')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestMultipleEntriesMultipleShards;
var
  Cache: TOCSPResponseCache;
  I: Integer;
  SerialNumber, Response, Retrieved: TBytes;
  AllFound: Boolean;
begin
  StartTest('Multiple entries across multiple shards');
  try
    Cache := TOCSPResponseCache.Create;
    try
      // 放入足够多的条目以分布到多个分片
      for I := 1 to 100 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        Response := CreateTestResponse(50);
        Cache.Put(SerialNumber, Response, Now, IncHour(Now, 1));
      end;

      // 验证所有条目都可以取出
      AllFound := True;
      for I := 1 to 100 do
      begin
        SerialNumber := CreateTestSerialNumber(I);
        if not Cache.Get(SerialNumber, Retrieved) then
        begin
          AllFound := False;
          Break;
        end;
      end;

      if not AllFound then
        FailTest('Not all entries could be retrieved')
      else
        PassTest;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: TOCSPCacheEntry 记录方法
// ========================================================================

procedure TestCacheEntryIsExpired;
var
  Entry: TOCSPCacheEntry;
begin
  StartTest('TOCSPCacheEntry.IsExpired');
  try
    FillChar(Entry, SizeOf(Entry), 0);

    // Unknown/zero nextUpdate should fail closed
    Entry.NextUpdate := 0;
    if not Entry.IsExpired then
      FailTest('Entry with NextUpdate=0 should be treated as expired')
    else
    begin
    // 测试过期条目
    Entry.NextUpdate := IncHour(Now, -1);  // 1 小时前过期
    if not Entry.IsExpired then
      FailTest('Entry should be expired')
    else
    begin
      // 测试未过期条目
      Entry.NextUpdate := IncHour(Now, 1);  // 1 小时后过期
      if Entry.IsExpired then
        FailTest('Entry should not be expired')
      else
        PassTest;
    end;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCacheEntryIsValid;
var
  Entry: TOCSPCacheEntry;
begin
  StartTest('TOCSPCacheEntry.IsValid');
  try
    FillChar(Entry, SizeOf(Entry), 0);

    // 空响应应该无效
    SetLength(Entry.ResponseData, 0);
    Entry.NextUpdate := IncHour(Now, 1);
    if Entry.IsValid then
      FailTest('Entry with empty response should be invalid')
    else
    begin
      // 有数据且未过期应该有效
      SetLength(Entry.ResponseData, 10);
      if not Entry.IsValid then
        FailTest('Entry with data and not expired should be valid')
      else
        PassTest;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: TOCSPCacheStats 记录方法
// ========================================================================

procedure TestCacheStatsHitRate;
var
  Stats: TOCSPCacheStats;
begin
  StartTest('TOCSPCacheStats.HitRate');
  try
    FillChar(Stats, SizeOf(Stats), 0);

    // 无请求时应该返回 0
    if Stats.HitRate <> 0 then
      FailTest('HitRate should be 0 when TotalRequests is 0')
    else
    begin
      // 设置一些统计数据
      Stats.TotalRequests := 100;
      Stats.CacheHits := 75;

      if Abs(Stats.HitRate - 75.0) > 0.01 then
        FailTest('HitRate should be 75%')
      else
        PassTest;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCacheStatsMissRate;
var
  Stats: TOCSPCacheStats;
begin
  StartTest('TOCSPCacheStats.MissRate');
  try
    FillChar(Stats, SizeOf(Stats), 0);
    Stats.TotalRequests := 100;
    Stats.CacheMisses := 25;

    if Abs(Stats.MissRate - 25.0) > 0.01 then
      FailTest('MissRate should be 25%')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 主程序
// ========================================================================

procedure PrintSummary;
var
  PassRate: Double;
begin
  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn('Total Tests: ', TotalTests);
  if TotalTests > 0 then
    PassRate := (PassedTests / TotalTests) * 100.0
  else
    PassRate := 0;
  WriteLn('Passed: ', PassedTests, ' (', PassRate:0:1, '%)');
  WriteLn('Failed: ', FailedTests, ' (', (100 - PassRate):0:1, '%)');

  if FailedTests = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed!');
end;

begin
  WriteLn('=== OCSP Response Cache Module Tests ===');
  WriteLn;

  // 缓存创建测试
  TestCacheCreation;
  TestCacheCreationWithParams;

  // Put 和 Get 测试
  TestPutAndGet;
  TestGetNonexistent;
  TestPutEmptyResponse;
  TestEmptySerialNumberRejected;
  TestContains;
  TestSmallMaxEntriesStillStoresEntry;

  // 过期处理测试
  TestExpiredEntryReturnsMiss;
  TestAutoExpireOnGet;
  TestContainsExpiredEntry;

  // 更新和删除测试
  TestUpdateEntry;
  TestRemoveEntry;
  TestClear;

  // LRU 驱逐测试
  TestLRUEviction;

  // 统计信息测试
  TestCacheStats;
  TestHitRate;
  TestResetStats;

  // 并发测试
  TestConcurrentPut;
  TestConcurrentGet;
  TestConcurrentStatsAndGetNoDeadlock;

  // 持久化测试
  TestSaveToFile;
  TestLoadFromFile;
  TestLoadFromNonexistentFile;

  // 边界条件测试
  TestDefaultTTLUsed;
  TestMultipleEntriesMultipleShards;

  // 记录方法测试
  TestCacheEntryIsExpired;
  TestCacheEntryIsValid;
  TestCacheStatsHitRate;
  TestCacheStatsMissRate;

  PrintSummary;

  // 返回退出码
  if FailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end.
