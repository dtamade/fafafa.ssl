program benchmark_ocsp_cache;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Classes,
  fafafa.ssl.ocsp.cache,
  fafafa.ssl.ocsp.stapling;

type
  TBenchmarkResult = record
    TestName: string;
    Operations: Integer;
    TotalTimeMs: Int64;
    OpsPerSecond: Double;
    AvgLatencyUs: Double;
    MinLatencyUs: Int64;
    MaxLatencyUs: Int64;
    MemoryUsedKB: Int64;
  end;

var
  Cache: TOCSPResponseCache;
  Results: array of TBenchmarkResult;

procedure AddResult(const AResult: TBenchmarkResult);
begin
  SetLength(Results, Length(Results) + 1);
  Results[High(Results)] := AResult;
end;

function GetMemoryUsed: Int64;
var
  Status: THeapStatus;
begin
  Status := GetHeapStatus;
  Result := Status.TotalAllocated div 1024;  // KB
end;

procedure BenchmarkCachePut;
var
  StartTime, EndTime: TDateTime;
  StartMem, EndMem: Int64;
  I: Integer;
  TestData: TBytes;
  SerialNumber: TBytes;
  Result: TBenchmarkResult;
  MinTime, MaxTime, TotalTime: Int64;
  OpStart, OpEnd: TDateTime;
  OpTime: Int64;
begin
  WriteLn('=== Benchmark: Cache Put Operations ===');
  
  SetLength(TestData, 1024);  // 1KB response
  FillChar(TestData[0], 1024, $AA);
  
  MinTime := High(Int64);
  MaxTime := 0;
  TotalTime := 0;
  
  StartMem := GetMemoryUsed;
  StartTime := Now;
  
  for I := 1 to 10000 do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte(I mod 256));
    
    OpStart := Now;
    Cache.Put(SerialNumber, TestData, Now, Now + 1.0);
    OpEnd := Now;
    
    OpTime := MilliSecondsBetween(OpEnd, OpStart) * 1000;  // 转换为微秒
    if OpTime < MinTime then MinTime := OpTime;
    if OpTime > MaxTime then MaxTime := OpTime;
    TotalTime := TotalTime + OpTime;
  end;
  
  EndTime := Now;
  EndMem := GetMemoryUsed;
  
  Result.TestName := 'Cache Put (10K ops)';
  Result.Operations := 10000;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.OpsPerSecond := 10000.0 / (Result.TotalTimeMs / 1000.0);
  Result.AvgLatencyUs := TotalTime / 10000.0;
  Result.MinLatencyUs := MinTime;
  Result.MaxLatencyUs := MaxTime;
  Result.MemoryUsedKB := EndMem - StartMem;
  
  AddResult(Result);
  
  WriteLn('  Operations: ', Result.Operations);
  WriteLn('  Total time: ', Result.TotalTimeMs, ' ms');
  WriteLn('  Ops/sec: ', Result.OpsPerSecond:0:2);
  WriteLn('  Avg latency: ', Result.AvgLatencyUs:0:2, ' μs');
  WriteLn('  Min latency: ', Result.MinLatencyUs, ' μs');
  WriteLn('  Max latency: ', Result.MaxLatencyUs, ' μs');
  WriteLn('  Memory used: ', Result.MemoryUsedKB, ' KB');
  WriteLn;
end;

procedure BenchmarkCacheGet;
var
  StartTime, EndTime: TDateTime;
  I: Integer;
  TestData, Retrieved: TBytes;
  SerialNumber: TBytes;
  Result: TBenchmarkResult;
  MinTime, MaxTime, TotalTime: Int64;
  OpStart, OpEnd: TDateTime;
  OpTime: Int64;
  HitCount: Integer;
begin
  WriteLn('=== Benchmark: Cache Get Operations ===');
  
  // 预填充缓存
  SetLength(TestData, 1024);
  FillChar(TestData[0], 1024, $AA);
  
  for I := 1 to 1000 do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte(I mod 256));
    Cache.Put(SerialNumber, TestData, Now, Now + 1.0);
  end;
  
  MinTime := High(Int64);
  MaxTime := 0;
  TotalTime := 0;
  HitCount := 0;
  
  StartTime := Now;
  
  for I := 1 to 100000 do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte(I mod 256));
    
    OpStart := Now;
    if Cache.Get(SerialNumber, Retrieved) then
      Inc(HitCount);
    OpEnd := Now;
    
    OpTime := MilliSecondsBetween(OpEnd, OpStart) * 1000;
    if OpTime < MinTime then MinTime := OpTime;
    if OpTime > MaxTime then MaxTime := OpTime;
    TotalTime := TotalTime + OpTime;
  end;
  
  EndTime := Now;
  
  Result.TestName := 'Cache Get (100K ops)';
  Result.Operations := 100000;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.OpsPerSecond := 100000.0 / (Result.TotalTimeMs / 1000.0);
  Result.AvgLatencyUs := TotalTime / 100000.0;
  Result.MinLatencyUs := MinTime;
  Result.MaxLatencyUs := MaxTime;
  Result.MemoryUsedKB := 0;
  
  AddResult(Result);
  
  WriteLn('  Operations: ', Result.Operations);
  WriteLn('  Total time: ', Result.TotalTimeMs, ' ms');
  WriteLn('  Ops/sec: ', Result.OpsPerSecond:0:2);
  WriteLn('  Avg latency: ', Result.AvgLatencyUs:0:2, ' μs');
  WriteLn('  Min latency: ', Result.MinLatencyUs, ' μs');
  WriteLn('  Max latency: ', Result.MaxLatencyUs, ' μs');
  WriteLn('  Hit rate: ', (HitCount / 100000.0 * 100.0):0:2, '%');
  WriteLn;
end;

procedure BenchmarkCacheMixed;
var
  StartTime, EndTime: TDateTime;
  I: Integer;
  TestData, Retrieved: TBytes;
  SerialNumber: TBytes;
  Result: TBenchmarkResult;
  Stats: TOCSPCacheStats;
begin
  WriteLn('=== Benchmark: Mixed Operations (80% Get, 20% Put) ===');
  
  SetLength(TestData, 1024);
  FillChar(TestData[0], 1024, $AA);
  
  StartTime := Now;
  
  for I := 1 to 50000 do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte(I mod 256));
    
    if (I mod 5) = 0 then
      // 20% Put
      Cache.Put(SerialNumber, TestData, Now, Now + 1.0)
    else
      // 80% Get
      Cache.Get(SerialNumber, Retrieved);
  end;
  
  EndTime := Now;
  Stats := Cache.GetStats;
  
  Result.TestName := 'Mixed Ops (50K ops)';
  Result.Operations := 50000;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.OpsPerSecond := 50000.0 / (Result.TotalTimeMs / 1000.0);
  Result.AvgLatencyUs := (Result.TotalTimeMs * 1000.0) / 50000.0;
  Result.MinLatencyUs := 0;
  Result.MaxLatencyUs := 0;
  Result.MemoryUsedKB := 0;
  
  AddResult(Result);
  
  WriteLn('  Operations: ', Result.Operations);
  WriteLn('  Total time: ', Result.TotalTimeMs, ' ms');
  WriteLn('  Ops/sec: ', Result.OpsPerSecond:0:2);
  WriteLn('  Avg latency: ', Result.AvgLatencyUs:0:2, ' μs');
  WriteLn('  Cache entries: ', Stats.TotalEntries);
  WriteLn('  Hit rate: ', Stats.HitRate:0:2, '%');
  WriteLn;
end;

procedure BenchmarkMemoryUsage;
var
  StartMem, EndMem: Int64;
  I: Integer;
  TestData: TBytes;
  SerialNumber: TBytes;
begin
  WriteLn('=== Benchmark: Memory Usage ===');
  
  StartMem := GetMemoryUsed;
  WriteLn('  Initial memory: ', StartMem, ' KB');
  
  // 填充 1000 个 1KB 响应
  SetLength(TestData, 1024);
  FillChar(TestData[0], 1024, $AA);
  
  for I := 1 to 1000 do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte(I mod 256));
    Cache.Put(SerialNumber, TestData, Now, Now + 1.0);
  end;
  
  EndMem := GetMemoryUsed;
  
  WriteLn('  After 1000 entries: ', EndMem, ' KB');
  WriteLn('  Memory per entry: ', (EndMem - StartMem) / 1000.0:0:2, ' KB');
  WriteLn('  Total overhead: ', EndMem - StartMem, ' KB');
  WriteLn;
end;

procedure PrintSummary;
var
  I: Integer;
begin
  WriteLn('=======================================');
  WriteLn('  Performance Summary');
  WriteLn('=======================================');
  WriteLn;
  
  for I := 0 to High(Results) do
  begin
    WriteLn(Results[I].TestName);
    WriteLn('  Throughput: ', Results[I].OpsPerSecond:0:2, ' ops/sec');
    WriteLn('  Avg Latency: ', Results[I].AvgLatencyUs:0:2, ' μs');
    if Results[I].MemoryUsedKB > 0 then
      WriteLn('  Memory: ', Results[I].MemoryUsedKB, ' KB');
    WriteLn;
  end;
  
  WriteLn('Performance Grade:');
  if Results[0].OpsPerSecond > 50000 then
    WriteLn('  ✓ Excellent (>50K ops/sec)')
  else if Results[0].OpsPerSecond > 20000 then
    WriteLn('  ✓ Good (>20K ops/sec)')
  else if Results[0].OpsPerSecond > 10000 then
    WriteLn('  ⚠ Acceptable (>10K ops/sec)')
  else
    WriteLn('  ✗ Needs optimization (<10K ops/sec)');
end;

begin
  WriteLn('=======================================');
  WriteLn('  OCSP Cache Performance Benchmark');
  WriteLn('=======================================');
  WriteLn;
  
  try
    Cache := TOCSPResponseCache.Create(10000);
    try
      BenchmarkCachePut;
      Cache.Clear;
      
      BenchmarkCacheGet;
      Cache.Clear;
      
      BenchmarkCacheMixed;
      Cache.Clear;
      
      BenchmarkMemoryUsage;
      
      PrintSummary;
      
      ExitCode := 0;
    finally
      Cache.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
