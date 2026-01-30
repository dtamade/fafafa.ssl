program benchmark_concurrent;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, DateUtils, Classes,
  fafafa.ssl.ocsp.cache,
  fafafa.ssl.ocsp.stapling;

type
  TWorkerThread = class(TThread)
  private
    FCache: TOCSPResponseCache;
    FOperations: Integer;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FTestData: TBytes;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TOCSPResponseCache; AOperations: Integer);
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
  end;

constructor TWorkerThread.Create(ACache: TOCSPResponseCache; AOperations: Integer);
begin
  inherited Create(True);
  FCache := ACache;
  FOperations := AOperations;
  FreeOnTerminate := False;
  
  SetLength(FTestData, 1024);
  FillChar(FTestData[0], 1024, $AA);
end;

procedure TWorkerThread.Execute;
var
  I: Integer;
  SerialNumber: TBytes;
  Retrieved: TBytes;
begin
  FStartTime := Now;
  
  for I := 1 to FOperations do
  begin
    SetLength(SerialNumber, 16);
    FillChar(SerialNumber[0], 16, Byte((ThreadID + I) mod 256));
    
    // 80% Get, 20% Put
    if (I mod 5) = 0 then
      FCache.Put(SerialNumber, FTestData, Now, Now + 1.0)
    else
      FCache.Get(SerialNumber, Retrieved);
  end;
  
  FEndTime := Now;
end;

procedure BenchmarkConcurrency;
var
  Cache: TOCSPResponseCache;
  Threads: array[1..4] of TWorkerThread;
  I: Integer;
  TotalOps: Integer;
  TotalTime: Int64;
  Stats: TOCSPCacheStats;
begin
  WriteLn('=======================================');
  WriteLn('  Concurrent Performance Test');
  WriteLn('=======================================');
  WriteLn;
  
  Cache := TOCSPResponseCache.Create(10000);
  try
    TotalOps := 10000 * 4;  // 4 threads * 10K ops each
    
    WriteLn('Starting 4 concurrent threads...');
    WriteLn('Each thread: 10,000 operations (80% Get, 20% Put)');
    WriteLn;
    
    // 创建并启动线程
    for I := 1 to 4 do
    begin
      Threads[I] := TWorkerThread.Create(Cache, 10000);
      Threads[I].Start;
    end;
    
    // 等待所有线程完成
    for I := 1 to 4 do
      Threads[I].WaitFor;
    
    // 计算总时间 (取最长的线程时间)
    TotalTime := 0;
    for I := 1 to 4 do
    begin
      if MilliSecondsBetween(Threads[I].EndTime, Threads[I].StartTime) > TotalTime then
        TotalTime := MilliSecondsBetween(Threads[I].EndTime, Threads[I].StartTime);
    end;
    
    WriteLn('=== Results ===');
    WriteLn('  Total operations: ', TotalOps);
    WriteLn('  Total time: ', TotalTime, ' ms');
    WriteLn('  Throughput: ', (TotalOps / (TotalTime / 1000.0)):0:2, ' ops/sec');
    WriteLn('  Avg latency: ', (TotalTime * 1000.0 / TotalOps):0:2, ' μs');
    WriteLn;
    
    Stats := Cache.GetStats;
    WriteLn('=== Cache Stats ===');
    WriteLn('  Total entries: ', Stats.TotalEntries);
    WriteLn('  Hit rate: ', Stats.HitRate:0:2, '%');
    WriteLn('  Total requests: ', Stats.TotalRequests);
    WriteLn;
    
    // 清理线程
    for I := 1 to 4 do
      Threads[I].Free;
      
  finally
    Cache.Free;
  end;
end;

procedure BenchmarkMemoryPressure;
var
  Cache: TOCSPResponseCache;
  I: Integer;
  TestData: TBytes;
  SerialNumber: TBytes;
  StartMem, EndMem: Int64;
  Stats: TOCSPCacheStats;
begin
  WriteLn('=======================================');
  WriteLn('  Memory Pressure Test');
  WriteLn('=======================================');
  WriteLn;
  
  Cache := TOCSPResponseCache.Create(10000);
  try
    StartMem := GetHeapStatus.TotalAllocated div 1024;
    
    // 填充 10,000 个 1KB 响应
    SetLength(TestData, 1024);
    FillChar(TestData[0], 1024, $AA);
    
    WriteLn('Filling cache with 10,000 entries...');
    for I := 1 to 10000 do
    begin
      SetLength(SerialNumber, 16);
      FillChar(SerialNumber[0], 16, Byte(I mod 256));
      Cache.Put(SerialNumber, TestData, Now, Now + 1.0);
      
      if (I mod 1000) = 0 then
        Write('.');
    end;
    WriteLn;
    WriteLn;
    
    EndMem := GetHeapStatus.TotalAllocated div 1024;
    Stats := Cache.GetStats;
    
    WriteLn('=== Results ===');
    WriteLn('  Cache entries: ', Stats.TotalEntries);
    WriteLn('  Memory used: ', EndMem - StartMem, ' KB');
    WriteLn('  Memory per entry: ', ((EndMem - StartMem) / Stats.TotalEntries):0:2, ' KB');
    WriteLn('  Total overhead: ', (EndMem - StartMem) - Stats.TotalEntries, ' KB');
    WriteLn;
    
  finally
    Cache.Free;
  end;
end;

begin
  WriteLn('=======================================');
  WriteLn('  OCSP Cache Concurrent Benchmark');
  WriteLn('=======================================');
  WriteLn;
  
  try
    BenchmarkConcurrency;
    WriteLn;
    BenchmarkMemoryPressure;
    
    ExitCode := 0;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
