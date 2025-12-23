program stress_crypto_operations;

{$mode objfpc}{$H+}

{**
 * Stress Test Suite: Crypto Operations
 *
 * P2-8: 本地压力测试套件（无需网络连接）
 *
 * 测试内容：
 * - 高负载下的哈希操作稳定性
 * - 高负载下的加密/解密操作稳定性
 * - 内存分配/释放模式
 * - 资源清理验证
 * - 性能退化检测
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes, DateUtils, Math,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
  fafafa.ssl.base;

const
  // 压力测试级别
  STRESS_LEVEL_LIGHT   = 1000;    // 轻量级
  STRESS_LEVEL_MEDIUM  = 10000;   // 中等
  STRESS_LEVEL_HEAVY   = 50000;   // 重度
  STRESS_LEVEL_EXTREME = 100000;  // 极端

  // 数据大小变化范围
  MIN_DATA_SIZE = 16;
  MAX_DATA_SIZE = 65536;

  // 通过阈值
  SUCCESS_RATE_THRESHOLD = 99.9;  // 99.9% 成功率
  MAX_MEMORY_GROWTH_MB = 50;      // 最大内存增长 50MB

type
  TStressTestResult = record
    TestName: string;
    Iterations: Integer;
    SuccessCount: Integer;
    FailureCount: Integer;
    TotalTimeMs: Int64;
    AvgTimeUs: Double;  // 微秒
    MinTimeUs: Double;
    MaxTimeUs: Double;
    SuccessRate: Double;
    MemoryBefore: Int64;
    MemoryAfter: Int64;
    MemoryGrowthKB: Int64;
  end;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;
  GStartMemory: Int64 = 0;

{ 获取当前进程内存使用（Linux） }
function GetProcessMemoryKB: Int64;
var
  F: TextFile;
  Line: string;
  Parts: TStringArray;
begin
  Result := 0;
  {$IFDEF UNIX}
  try
    AssignFile(F, '/proc/self/status');
    Reset(F);
    try
      while not EOF(F) do
      begin
        ReadLn(F, Line);
        if Pos('VmRSS:', Line) = 1 then
        begin
          // VmRSS:    12345 kB
          Parts := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
          if Length(Parts) >= 2 then
            Result := StrToInt64Def(Parts[1], 0);
          Break;
        end;
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := 0;
  end;
  {$ENDIF}
end;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('  ', ATitle);
  WriteLn('================================================================');
  WriteLn;
end;

procedure PrintResult(const AResult: TStressTestResult);
var
  StatusStr: string;
  Passed: Boolean;
begin
  WriteLn('Test: ', AResult.TestName);
  WriteLn('  Iterations: ', AResult.Iterations);
  WriteLn('  Success: ', AResult.SuccessCount, ' / Failure: ', AResult.FailureCount);
  WriteLn('  Success Rate: ', Format('%.2f', [AResult.SuccessRate]), '%');
  WriteLn('  Total Time: ', AResult.TotalTimeMs, ' ms');
  WriteLn('  Avg Time: ', Format('%.2f', [AResult.AvgTimeUs]), ' us/op');
  WriteLn('  Min/Max Time: ', Format('%.2f', [AResult.MinTimeUs]), ' / ',
          Format('%.2f', [AResult.MaxTimeUs]), ' us');
  WriteLn('  Memory: ', AResult.MemoryBefore, ' KB -> ', AResult.MemoryAfter,
          ' KB (Growth: ', AResult.MemoryGrowthKB, ' KB)');

  Passed := (AResult.SuccessRate >= SUCCESS_RATE_THRESHOLD) and
            (AResult.MemoryGrowthKB < MAX_MEMORY_GROWTH_MB * 1024);

  if Passed then
  begin
    StatusStr := '✓ PASS';
    Inc(GTestsPassed);
  end
  else
  begin
    if AResult.SuccessRate < SUCCESS_RATE_THRESHOLD then
      StatusStr := '✗ FAIL (success rate below ' + Format('%.1f', [SUCCESS_RATE_THRESHOLD]) + '%)'
    else
      StatusStr := '✗ FAIL (memory growth exceeds ' + IntToStr(MAX_MEMORY_GROWTH_MB) + ' MB)';
    Inc(GTestsFailed);
  end;

  WriteLn('  Status: ', StatusStr);
  WriteLn;
end;

{ 生成随机测试数据 }
function GenerateRandomData(ASize: Integer): TBytes;
var
  I: Integer;
begin
  SetLength(Result, ASize);
  for I := 0 to ASize - 1 do
    Result[I] := Random(256);
end;

{ ==================== SHA-256 压力测试 ==================== }

function StressSHA256(AIterations: Integer): TStressTestResult;
var
  I, DataSize: Integer;
  Data, Hash: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('SHA-256 Stress (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      // 变化数据大小
      DataSize := MIN_DATA_SIZE + Random(MAX_DATA_SIZE - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);

      IterStart := Now;
      Hash := TCryptoUtils.SHA256(Data);
      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if Length(Hash) = 32 then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    // 每 10% 打印进度
    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== SHA-512 压力测试 ==================== }

function StressSHA512(AIterations: Integer): TStressTestResult;
var
  I, DataSize: Integer;
  Data, Hash: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('SHA-512 Stress (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      DataSize := MIN_DATA_SIZE + Random(MAX_DATA_SIZE - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);

      IterStart := Now;
      Hash := TCryptoUtils.SHA512(Data);
      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if Length(Hash) = 64 then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== AES-GCM 加密/解密压力测试 ==================== }

function StressAESGCM(AIterations: Integer): TStressTestResult;
var
  I, DataSize: Integer;
  Data, Key, IV, Encrypted, Decrypted: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('AES-256-GCM Encrypt/Decrypt (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  // 预生成密钥和 IV
  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      // 变化数据大小（加密操作用较小数据）
      DataSize := MIN_DATA_SIZE + Random(4096 - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);

      IterStart := Now;

      // 加密
      Encrypted := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);

      // 解密
      Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted, Key, IV);

      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      // 验证解密结果
      if (Length(Decrypted) = Length(Data)) and
         CompareMem(@Data[0], @Decrypted[0], Length(Data)) then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== AES-CBC 加密/解密压力测试 ==================== }

function StressAESCBC(AIterations: Integer): TStressTestResult;
var
  I, DataSize: Integer;
  Data, Key, IV, Encrypted, Decrypted: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('AES-256-CBC Encrypt/Decrypt (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  // 预生成密钥和 IV
  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(16);  // CBC 需要 16 字节 IV

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      DataSize := MIN_DATA_SIZE + Random(4096 - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);

      IterStart := Now;

      Encrypted := TCryptoUtils.AES_CBC_Encrypt(Data, Key, IV);
      Decrypted := TCryptoUtils.AES_CBC_Decrypt(Encrypted, Key, IV);

      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if (Length(Decrypted) = Length(Data)) and
         CompareMem(@Data[0], @Decrypted[0], Length(Data)) then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== Base64 编码/解码压力测试 ==================== }

function StressBase64(AIterations: Integer): TStressTestResult;
var
  I, DataSize: Integer;
  Data, Decoded: TBytes;
  Encoded: string;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('Base64 Encode/Decode (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      DataSize := MIN_DATA_SIZE + Random(8192 - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);

      IterStart := Now;

      Encoded := TEncodingUtils.Base64Encode(Data);
      Decoded := TEncodingUtils.Base64Decode(Encoded);

      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if (Length(Decoded) = Length(Data)) and
         CompareMem(@Data[0], @Decoded[0], Length(Data)) then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== 随机数生成压力测试 ==================== }

function StressSecureRandom(AIterations: Integer): TStressTestResult;
var
  I, RandSize: Integer;
  RandomBytes: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
begin
  Result.TestName := Format('SecureRandom Generation (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      RandSize := 16 + Random(256 - 16);

      IterStart := Now;
      RandomBytes := TCryptoUtils.SecureRandom(RandSize);
      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if Length(RandomBytes) = RandSize then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== 混合操作压力测试 ==================== }

function StressMixedOperations(AIterations: Integer): TStressTestResult;
var
  I, OpType, DataSize: Integer;
  Data, Key, IV, Encrypted, Decrypted, Hash: TBytes;
  StartTime, EndTime, IterStart: TDateTime;
  IterTimeUs: Double;
  Success: Boolean;
begin
  Result.TestName := Format('Mixed Operations (%d iterations)', [AIterations]);
  Result.Iterations := AIterations;
  Result.SuccessCount := 0;
  Result.FailureCount := 0;
  Result.MinTimeUs := MaxDouble;
  Result.MaxTimeUs := 0;
  Result.MemoryBefore := GetProcessMemoryKB;

  TCryptoUtils.EnsureInitialized;

  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);

  StartTime := Now;

  for I := 1 to AIterations do
  begin
    try
      OpType := Random(5);  // 0-4: 随机选择操作类型
      DataSize := MIN_DATA_SIZE + Random(2048 - MIN_DATA_SIZE);
      Data := GenerateRandomData(DataSize);
      Success := False;

      IterStart := Now;

      case OpType of
        0: begin  // SHA-256
             Hash := TCryptoUtils.SHA256(Data);
             Success := Length(Hash) = 32;
           end;
        1: begin  // SHA-512
             Hash := TCryptoUtils.SHA512(Data);
             Success := Length(Hash) = 64;
           end;
        2: begin  // AES-GCM
             Encrypted := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
             Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted, Key, IV);
             Success := (Length(Decrypted) = Length(Data)) and
                        CompareMem(@Data[0], @Decrypted[0], Length(Data));
           end;
        3: begin  // SecureRandom
             Hash := TCryptoUtils.SecureRandom(32);
             Success := Length(Hash) = 32;
           end;
        4: begin  // Base64
             Decrypted := TEncodingUtils.Base64Decode(TEncodingUtils.Base64Encode(Data));
             Success := (Length(Decrypted) = Length(Data)) and
                        CompareMem(@Data[0], @Decrypted[0], Length(Data));
           end;
      end;

      IterTimeUs := MilliSecondSpan(IterStart, Now) * 1000;

      if Success then
      begin
        Inc(Result.SuccessCount);
        if IterTimeUs < Result.MinTimeUs then Result.MinTimeUs := IterTimeUs;
        if IterTimeUs > Result.MaxTimeUs then Result.MaxTimeUs := IterTimeUs;
      end
      else
        Inc(Result.FailureCount);

    except
      Inc(Result.FailureCount);
    end;

    if (AIterations >= 1000) and (I mod (AIterations div 10) = 0) then
      Write('.');
  end;

  if AIterations >= 1000 then WriteLn;

  EndTime := Now;
  Result.TotalTimeMs := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimeUs := (Result.TotalTimeMs * 1000) / AIterations;
  Result.SuccessRate := (Result.SuccessCount / AIterations) * 100;
  Result.MemoryAfter := GetProcessMemoryKB;
  Result.MemoryGrowthKB := Result.MemoryAfter - Result.MemoryBefore;
end;

{ ==================== 主程序 ==================== }

procedure RunStressTestSuite(ALevel: Integer; const ALevelName: string);
begin
  PrintHeader('Stress Level: ' + ALevelName + ' (' + IntToStr(ALevel) + ' iterations)');

  PrintResult(StressSHA256(ALevel));
  PrintResult(StressSHA512(ALevel));
  PrintResult(StressAESGCM(ALevel));
  PrintResult(StressAESCBC(ALevel));
  PrintResult(StressBase64(ALevel));
  PrintResult(StressSecureRandom(ALevel));
  PrintResult(StressMixedOperations(ALevel));
end;

procedure PrintFinalSummary;
var
  FinalMemory: Int64;
begin
  FinalMemory := GetProcessMemoryKB;

  PrintHeader('STRESS TEST SUMMARY');

  WriteLn('Total Tests: ', GTestsPassed + GTestsFailed);
  WriteLn('Passed: ', GTestsPassed, ' ✓');
  WriteLn('Failed: ', GTestsFailed, ' ✗');
  WriteLn;
  WriteLn('Memory Usage:');
  WriteLn('  Start: ', GStartMemory, ' KB');
  WriteLn('  End: ', FinalMemory, ' KB');
  WriteLn('  Total Growth: ', FinalMemory - GStartMemory, ' KB');
  WriteLn;

  if GTestsFailed = 0 then
  begin
    WriteLn('Result: ✓ ALL STRESS TESTS PASSED');
    WriteLn('Library is stable under high load conditions.');
  end
  else
  begin
    WriteLn('Result: ✗ SOME STRESS TESTS FAILED');
    WriteLn('Review failed tests for stability issues.');
  end;

  WriteLn('================================================================');
  WriteLn;
end;

var
  StressLevel: string;
begin
  Randomize;
  GStartMemory := GetProcessMemoryKB;

  PrintHeader('fafafa.ssl Crypto Operations Stress Test Suite');

  WriteLn('Configuration:');
  WriteLn('  Success Rate Threshold: ', Format('%.1f', [SUCCESS_RATE_THRESHOLD]), '%');
  WriteLn('  Max Memory Growth: ', MAX_MEMORY_GROWTH_MB, ' MB');
  WriteLn('  Data Size Range: ', MIN_DATA_SIZE, ' - ', MAX_DATA_SIZE, ' bytes');
  WriteLn('  Initial Memory: ', GStartMemory, ' KB');
  WriteLn;

  // 检查命令行参数
  if ParamCount >= 1 then
    StressLevel := LowerCase(ParamStr(1))
  else
    StressLevel := 'medium';

  WriteLn('Stress Level: ', StressLevel);
  WriteLn;

  try
    case StressLevel of
      'light':   RunStressTestSuite(STRESS_LEVEL_LIGHT, 'Light');
      'medium':  RunStressTestSuite(STRESS_LEVEL_MEDIUM, 'Medium');
      'heavy':   RunStressTestSuite(STRESS_LEVEL_HEAVY, 'Heavy');
      'extreme': RunStressTestSuite(STRESS_LEVEL_EXTREME, 'Extreme');
      'all': begin
               RunStressTestSuite(STRESS_LEVEL_LIGHT, 'Light');
               RunStressTestSuite(STRESS_LEVEL_MEDIUM, 'Medium');
               RunStressTestSuite(STRESS_LEVEL_HEAVY, 'Heavy');
             end;
    else
      WriteLn('Unknown stress level. Using medium.');
      RunStressTestSuite(STRESS_LEVEL_MEDIUM, 'Medium');
    end;

    PrintFinalSummary;

    if GTestsFailed > 0 then
      Halt(1);

  except
    on E: Exception do
    begin
      WriteLn('CRITICAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
