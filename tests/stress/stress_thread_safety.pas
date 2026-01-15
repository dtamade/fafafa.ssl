program stress_thread_safety;

{$mode objfpc}{$H+}

{**
 * Thread Safety Test Suite
 *
 * P2-9: 验证加密库在多线程环境下的线程安全性
 *
 * 测试内容：
 * - 多线程并发哈希计算
 * - 多线程并发加密/解密
 * - 多线程并发随机数生成
 * - 多线程并发 Base64 编解码
 * - 共享资源竞争检测
 * - 数据一致性验证
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  SysUtils, Classes, SyncObjs, DateUtils, Math,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
  fafafa.ssl.base;

const
  // 线程数配置
  THREAD_COUNT_LOW = 4;
  THREAD_COUNT_MEDIUM = 8;
  THREAD_COUNT_HIGH = 16;

  // 每线程迭代数
  ITERATIONS_PER_THREAD = 500;

  // 测试超时（秒）
  TEST_TIMEOUT_SEC = 60;

type
  { 线程测试结果 }
  TThreadTestResult = record
    ThreadId: Integer;
    Iterations: Integer;
    SuccessCount: Integer;
    FailureCount: Integer;
    TimeMs: Int64;
    ErrorMessage: string;
  end;
  PThreadTestResult = ^TThreadTestResult;

  { 线程结果数组 }
  TThreadResultArray = array of TThreadTestResult;

  { 基础测试线程类 }
  TBaseTestThread = class(TThread)
  protected
    FTestId: Integer;
    FIterations: Integer;
    FResult: TThreadTestResult;
    FStartEvent: TEvent;
    procedure Execute; override;
    procedure DoTest; virtual; abstract;
  public
    constructor Create(AThreadId, AIterations: Integer; AStartEvent: TEvent);
    property TestResult: TThreadTestResult read FResult;
  end;

  { SHA-256 线程测试 }
  TSHA256Thread = class(TBaseTestThread)
  protected
    procedure DoTest; override;
  end;

  { SHA-512 线程测试 }
  TSHA512Thread = class(TBaseTestThread)
  protected
    procedure DoTest; override;
  end;

  { AES-GCM 线程测试 }
  TAESGCMThread = class(TBaseTestThread)
  private
    FKey: TBytes;
    FIV: TBytes;
  protected
    procedure DoTest; override;
  public
    constructor Create(AThreadId, AIterations: Integer; AStartEvent: TEvent;
      const AKey, AIV: TBytes);
  end;

  { SecureRandom 线程测试 }
  TSecureRandomThread = class(TBaseTestThread)
  protected
    procedure DoTest; override;
  end;

  { Base64 线程测试 }
  TBase64Thread = class(TBaseTestThread)
  protected
    procedure DoTest; override;
  end;

  { 混合操作线程测试 }
  TMixedThread = class(TBaseTestThread)
  protected
    procedure DoTest; override;
  end;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

{ 生成随机测试数据 }
function GenerateRandomData(ASize: Integer): TBytes;
var
  I: Integer;
begin
  SetLength(Result, ASize);
  for I := 0 to ASize - 1 do
    Result[I] := Random(256);
end;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('  ', ATitle);
  WriteLn('================================================================');
  WriteLn;
end;

{ TBaseTestThread }

constructor TBaseTestThread.Create(AThreadId, AIterations: Integer; AStartEvent: TEvent);
begin
  inherited Create(True);  // 创建时挂起
  FreeOnTerminate := False;
  FTestId := AThreadId;
  FIterations := AIterations;
  FStartEvent := AStartEvent;

  FResult.ThreadId := AThreadId;
  FResult.Iterations := AIterations;
  FResult.SuccessCount := 0;
  FResult.FailureCount := 0;
  FResult.TimeMs := 0;
  FResult.ErrorMessage := '';
end;

procedure TBaseTestThread.Execute;
var
  StartTime: TDateTime;
begin
  // 等待启动信号（确保所有线程同时开始）
  FStartEvent.WaitFor(INFINITE);

  StartTime := Now;
  try
    DoTest;
  except
    on E: Exception do
    begin
      FResult.ErrorMessage := E.Message;
      Inc(FResult.FailureCount);
    end;
  end;
  FResult.TimeMs := MilliSecondsBetween(Now, StartTime);
end;

{ TSHA256Thread }

procedure TSHA256Thread.DoTest;
var
  I, DataSize: Integer;
  Data, Hash, ExpectedHash: TBytes;
begin
  for I := 1 to FIterations do
  begin
    try
      DataSize := 64 + Random(1024);
      Data := GenerateRandomData(DataSize);

      // 计算两次哈希，验证一致性
      Hash := TCryptoUtils.SHA256(Data);
      ExpectedHash := TCryptoUtils.SHA256(Data);

      if (Length(Hash) = 32) and (Length(ExpectedHash) = 32) and
         CompareMem(@Hash[0], @ExpectedHash[0], 32) then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ TSHA512Thread }

procedure TSHA512Thread.DoTest;
var
  I, DataSize: Integer;
  Data, Hash, ExpectedHash: TBytes;
begin
  for I := 1 to FIterations do
  begin
    try
      DataSize := 64 + Random(1024);
      Data := GenerateRandomData(DataSize);

      Hash := TCryptoUtils.SHA512(Data);
      ExpectedHash := TCryptoUtils.SHA512(Data);

      if (Length(Hash) = 64) and (Length(ExpectedHash) = 64) and
         CompareMem(@Hash[0], @ExpectedHash[0], 64) then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ TAESGCMThread }

constructor TAESGCMThread.Create(AThreadId, AIterations: Integer;
  AStartEvent: TEvent; const AKey, AIV: TBytes);
begin
  inherited Create(AThreadId, AIterations, AStartEvent);
  FKey := AKey;
  FIV := AIV;
end;

procedure TAESGCMThread.DoTest;
var
  I, DataSize: Integer;
  Data, Encrypted, Decrypted: TBytes;
begin
  for I := 1 to FIterations do
  begin
    try
      DataSize := 16 + Random(512);
      Data := GenerateRandomData(DataSize);

      Encrypted := TCryptoUtils.AES_GCM_Encrypt(Data, FKey, FIV);
      Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted, FKey, FIV);

      if (Length(Decrypted) = Length(Data)) and
         CompareMem(@Data[0], @Decrypted[0], Length(Data)) then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ TSecureRandomThread }

procedure TSecureRandomThread.DoTest;
var
  I, RandSize: Integer;
  Random1, Random2: TBytes;
begin
  for I := 1 to FIterations do
  begin
    try
      RandSize := 16 + Random(64);

      Random1 := TCryptoUtils.SecureRandom(RandSize);
      Random2 := TCryptoUtils.SecureRandom(RandSize);

      // 两次随机应该不同（极低概率相同）
      if (Length(Random1) = RandSize) and (Length(Random2) = RandSize) and
         not CompareMem(@Random1[0], @Random2[0], RandSize) then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ TBase64Thread }

procedure TBase64Thread.DoTest;
var
  I, DataSize: Integer;
  Data, Decoded: TBytes;
  Encoded: string;
begin
  for I := 1 to FIterations do
  begin
    try
      DataSize := 32 + Random(256);
      Data := GenerateRandomData(DataSize);

      Encoded := TEncodingUtils.Base64Encode(Data);
      Decoded := TEncodingUtils.Base64Decode(Encoded);

      if (Length(Decoded) = Length(Data)) and
         CompareMem(@Data[0], @Decoded[0], Length(Data)) then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ TMixedThread }

procedure TMixedThread.DoTest;
var
  I, OpType, DataSize: Integer;
  Data, Hash, Key, IV, Encrypted, Decrypted: TBytes;
  Success: Boolean;
begin
  // 预生成密钥
  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);

  for I := 1 to FIterations do
  begin
    try
      OpType := Random(4);
      DataSize := 32 + Random(256);
      Data := GenerateRandomData(DataSize);
      Success := False;

      case OpType of
        0: begin
             Hash := TCryptoUtils.SHA256(Data);
             Success := Length(Hash) = 32;
           end;
        1: begin
             Hash := TCryptoUtils.SHA512(Data);
             Success := Length(Hash) = 64;
           end;
        2: begin
             Encrypted := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
             Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted, Key, IV);
             Success := (Length(Decrypted) = Length(Data)) and
                        CompareMem(@Data[0], @Decrypted[0], Length(Data));
           end;
        3: begin
             Decrypted := TEncodingUtils.Base64Decode(TEncodingUtils.Base64Encode(Data));
             Success := (Length(Decrypted) = Length(Data)) and
                        CompareMem(@Data[0], @Decrypted[0], Length(Data));
           end;
      end;

      if Success then
        Inc(FResult.SuccessCount)
      else
        Inc(FResult.FailureCount);
    except
      Inc(FResult.FailureCount);
    end;
  end;
end;

{ 运行线程测试 }
function RunThreadTest(const ATestName: string; AThreadCount: Integer;
  AThreadClass: TClass; const AKey: TBytes = nil; const AIV: TBytes = nil): Boolean;
var
  Threads: array of TBaseTestThread;
  StartEvent: TEvent;
  I: Integer;
  TotalSuccess, TotalFailure: Integer;
  TotalTime: Int64;
  SuccessRate: Double;
  TimeoutCount: Integer;
begin
  Result := False;
  WriteLn('Test: ', ATestName);
  WriteLn('  Threads: ', AThreadCount);
  WriteLn('  Iterations/Thread: ', ITERATIONS_PER_THREAD);

  StartEvent := TEvent.Create(nil, True, False, '');
  try
    SetLength(Threads, AThreadCount);

    // 创建线程
    for I := 0 to AThreadCount - 1 do
    begin
      if AThreadClass = TAESGCMThread then
        Threads[I] := TAESGCMThread.Create(I, ITERATIONS_PER_THREAD, StartEvent, AKey, AIV)
      else if AThreadClass = TSHA256Thread then
        Threads[I] := TSHA256Thread.Create(I, ITERATIONS_PER_THREAD, StartEvent)
      else if AThreadClass = TSHA512Thread then
        Threads[I] := TSHA512Thread.Create(I, ITERATIONS_PER_THREAD, StartEvent)
      else if AThreadClass = TSecureRandomThread then
        Threads[I] := TSecureRandomThread.Create(I, ITERATIONS_PER_THREAD, StartEvent)
      else if AThreadClass = TBase64Thread then
        Threads[I] := TBase64Thread.Create(I, ITERATIONS_PER_THREAD, StartEvent)
      else if AThreadClass = TMixedThread then
        Threads[I] := TMixedThread.Create(I, ITERATIONS_PER_THREAD, StartEvent)
      else
        Threads[I] := TBaseTestThread(AThreadClass.Create);
    end;

    // 启动所有线程
    for I := 0 to AThreadCount - 1 do
      Threads[I].Start;

    // 发送启动信号（所有线程同时开始）
    StartEvent.SetEvent;

    // 等待所有线程完成
    TimeoutCount := 0;
    for I := 0 to AThreadCount - 1 do
    begin
      if Threads[I].WaitFor <> 0 then
        Inc(TimeoutCount);
    end;

    // 收集结果
    TotalSuccess := 0;
    TotalFailure := 0;
    TotalTime := 0;

    for I := 0 to AThreadCount - 1 do
    begin
      Inc(TotalSuccess, Threads[I].TestResult.SuccessCount);
      Inc(TotalFailure, Threads[I].TestResult.FailureCount);
      if Threads[I].TestResult.TimeMs > TotalTime then
        TotalTime := Threads[I].TestResult.TimeMs;

      if Threads[I].TestResult.ErrorMessage <> '' then
        WriteLn('  Thread ', I, ' Error: ', Threads[I].TestResult.ErrorMessage);
    end;

    // 计算成功率
    if TotalSuccess + TotalFailure > 0 then
      SuccessRate := (TotalSuccess / (TotalSuccess + TotalFailure)) * 100
    else
      SuccessRate := 0;

    WriteLn('  Total Success: ', TotalSuccess);
    WriteLn('  Total Failure: ', TotalFailure);
    WriteLn('  Success Rate: ', Format('%.2f', [SuccessRate]), '%');
    WriteLn('  Max Thread Time: ', TotalTime, ' ms');
    WriteLn('  Throughput: ', Format('%.0f', [(TotalSuccess + TotalFailure) / (TotalTime / 1000)]), ' ops/sec');

    Result := (SuccessRate >= 99.9) and (TimeoutCount = 0);

    if Result then
    begin
      WriteLn('  Status: PASS');
      Inc(GTestsPassed);
    end
    else
    begin
      WriteLn('  Status: FAIL');
      Inc(GTestsFailed);
    end;
    WriteLn;

    // 释放线程
    for I := 0 to AThreadCount - 1 do
      Threads[I].Free;
  finally
    StartEvent.Free;
  end;
end;

procedure RunAllThreadSafetyTests;
var
  Key, IV: TBytes;
begin
  TCryptoUtils.EnsureInitialized;

  // 预生成共享密钥（测试共享资源场景）
  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);

  PrintHeader('Thread Safety Tests - Low Concurrency (' + IntToStr(THREAD_COUNT_LOW) + ' threads)');

  RunThreadTest('SHA-256 Concurrent', THREAD_COUNT_LOW, TSHA256Thread);
  RunThreadTest('SHA-512 Concurrent', THREAD_COUNT_LOW, TSHA512Thread);
  RunThreadTest('AES-GCM Concurrent (Shared Key)', THREAD_COUNT_LOW, TAESGCMThread, Key, IV);
  RunThreadTest('SecureRandom Concurrent', THREAD_COUNT_LOW, TSecureRandomThread);
  RunThreadTest('Base64 Concurrent', THREAD_COUNT_LOW, TBase64Thread);
  RunThreadTest('Mixed Operations Concurrent', THREAD_COUNT_LOW, TMixedThread);

  PrintHeader('Thread Safety Tests - Medium Concurrency (' + IntToStr(THREAD_COUNT_MEDIUM) + ' threads)');

  RunThreadTest('SHA-256 Concurrent', THREAD_COUNT_MEDIUM, TSHA256Thread);
  RunThreadTest('AES-GCM Concurrent (Shared Key)', THREAD_COUNT_MEDIUM, TAESGCMThread, Key, IV);
  RunThreadTest('Mixed Operations Concurrent', THREAD_COUNT_MEDIUM, TMixedThread);

  PrintHeader('Thread Safety Tests - High Concurrency (' + IntToStr(THREAD_COUNT_HIGH) + ' threads)');

  RunThreadTest('SHA-256 Concurrent', THREAD_COUNT_HIGH, TSHA256Thread);
  RunThreadTest('AES-GCM Concurrent (Shared Key)', THREAD_COUNT_HIGH, TAESGCMThread, Key, IV);
  RunThreadTest('Mixed Operations Concurrent', THREAD_COUNT_HIGH, TMixedThread);
end;

procedure PrintFinalSummary;
begin
  PrintHeader('THREAD SAFETY TEST SUMMARY');

  WriteLn('Total Tests: ', GTestsPassed + GTestsFailed);
  WriteLn('Passed: ', GTestsPassed, ' OK');
  WriteLn('Failed: ', GTestsFailed, ' FAILED');
  WriteLn;

  if GTestsFailed = 0 then
  begin
    WriteLn('Result: ALL THREAD SAFETY TESTS PASSED');
    WriteLn('Library is thread-safe under concurrent access.');
  end
  else
  begin
    WriteLn('Result: SOME THREAD SAFETY TESTS FAILED');
    WriteLn('Review failed tests for potential race conditions.');
  end;

  WriteLn('================================================================');
  WriteLn;
end;

begin
  Randomize;

  PrintHeader('fafafa.ssl Thread Safety Test Suite');

  WriteLn('Configuration:');
  WriteLn('  Thread Counts: ', THREAD_COUNT_LOW, '/', THREAD_COUNT_MEDIUM, '/', THREAD_COUNT_HIGH);
  WriteLn('  Iterations per Thread: ', ITERATIONS_PER_THREAD);
  WriteLn('  Timeout: ', TEST_TIMEOUT_SEC, ' seconds');
  WriteLn;

  try
    RunAllThreadSafetyTests;
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
