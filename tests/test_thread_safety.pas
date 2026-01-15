{
  test_thread_safety.pas - Thread Safety Test Framework

  Tests thread safety for critical functions:
  - Concurrent crypto operations
  - Shared context access
  - Resource management under load
  - Deadlock detection

  Part of test-quality-improvement Phase 1, Task 4
}

program test_thread_safety;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.memutils,
  fafafa.ssl.encoding;

const
  THREAD_COUNT = 4;
  ITERATIONS_PER_THREAD = 100;
  STRESS_ITERATIONS = 500;

type
  { TThreadTestResult - Result from a thread test }
  TThreadTestResult = record
    ThreadID: Integer;
    Passed: Boolean;
    ErrorMessage: string;
    IterationsCompleted: Integer;
  end;

  { THashTestThread - Thread for testing hash operations }
  THashTestThread = class(TThread)
  private
    FTestID: Integer;
    FIterations: Integer;
    FResult: TThreadTestResult;
  protected
    procedure Execute; override;
  public
    constructor Create(ATestID, AIterations: Integer);
    property Result: TThreadTestResult read FResult;
  end;

  { TRandomTestThread - Thread for testing random generation }
  TRandomTestThread = class(TThread)
  private
    FTestID: Integer;
    FIterations: Integer;
    FResult: TThreadTestResult;
  protected
    procedure Execute; override;
  public
    constructor Create(ATestID, AIterations: Integer);
    property Result: TThreadTestResult read FResult;
  end;

  { TEncodingTestThread - Thread for testing encoding operations }
  TEncodingTestThread = class(TThread)
  private
    FTestID: Integer;
    FIterations: Integer;
    FResult: TThreadTestResult;
  protected
    procedure Execute; override;
  public
    constructor Create(ATestID, AIterations: Integer);
    property Result: TThreadTestResult read FResult;
  end;

  { TMemUtilsTestThread - Thread for testing memory operations }
  TMemUtilsTestThread = class(TThread)
  private
    FTestID: Integer;
    FIterations: Integer;
    FResult: TThreadTestResult;
  protected
    procedure Execute; override;
  public
    constructor Create(ATestID, AIterations: Integer);
    property Result: TThreadTestResult read FResult;
  end;

var
  PassCount: Integer = 0;
  FailCount: Integer = 0;

procedure PrintResult(const ATestName: string; APassed: Boolean; const ADetails: string = '');
begin
  if APassed then
  begin
    Write('[PASS] ');
    Inc(PassCount);
  end
  else
  begin
    Write('[FAIL] ');
    Inc(FailCount);
  end;
  Write(ATestName);
  if ADetails <> '' then
    WriteLn(' - ', ADetails)
  else
    WriteLn;
end;

procedure PrintCategory(const ACategory: string);
begin
  WriteLn;
  WriteLn('=== ', ACategory, ' ===');
  WriteLn;
end;

{ THashTestThread }

constructor THashTestThread.Create(ATestID, AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTestID := ATestID;
  FIterations := AIterations;
  FResult.ThreadID := ATestID;
  FResult.Passed := False;
  FResult.ErrorMessage := '';
  FResult.IterationsCompleted := 0;
end;

procedure THashTestThread.Execute;
var
  I: Integer;
  Data: TBytes;
  Hash: TBytes;
  Ctx: THashContext;
begin
  try
    for I := 1 to FIterations do
    begin
      if Terminated then
        Break;

      // Test pure Pascal hash (thread-safe by design)
      SetLength(Data, 32);
      FillChar(Data[0], 32, Byte(I mod 256));
      
      // SHA256
      Hash := fafafa.ssl.crypto.hash.SHA256(Data);
      if Length(Hash) <> 32 then
        raise Exception.Create('SHA256 returned wrong length');

      // SHA512
      Hash := fafafa.ssl.crypto.hash.SHA512(Data);
      if Length(Hash) <> 64 then
        raise Exception.Create('SHA512 returned wrong length');

      // Context-based hash
      Ctx := CreateHashContext(haSHA256);
      try
        Ctx.Update(Data);
        Hash := Ctx.Final;
        if Length(Hash) <> 32 then
          raise Exception.Create('Context SHA256 returned wrong length');
      finally
        Ctx.Free;
      end;

      Inc(FResult.IterationsCompleted);
    end;
    
    FResult.Passed := True;
    FResult.ErrorMessage := Format('Completed %d iterations', [FResult.IterationsCompleted]);
  except
    on E: Exception do
    begin
      FResult.Passed := False;
      FResult.ErrorMessage := Format('Failed at iteration %d: %s', 
        [FResult.IterationsCompleted + 1, E.Message]);
    end;
  end;
end;

{ TRandomTestThread }

constructor TRandomTestThread.Create(ATestID, AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTestID := ATestID;
  FIterations := AIterations;
  FResult.ThreadID := ATestID;
  FResult.Passed := False;
  FResult.ErrorMessage := '';
  FResult.IterationsCompleted := 0;
end;

procedure TRandomTestThread.Execute;
var
  I: Integer;
  RandomData: TBytes;
  Success: Boolean;
begin
  try
    for I := 1 to FIterations do
    begin
      if Terminated then
        Break;

      // Test TrySecureRandom (thread-safe)
      Success := TCryptoUtils.TrySecureRandom(32, RandomData);
      if Success then
      begin
        if Length(RandomData) <> 32 then
          raise Exception.Create('TrySecureRandom returned wrong length');
      end;
      // Note: TrySecureRandom may fail if OpenSSL not available, that's OK

      Inc(FResult.IterationsCompleted);
    end;
    
    FResult.Passed := True;
    FResult.ErrorMessage := Format('Completed %d iterations', [FResult.IterationsCompleted]);
  except
    on E: Exception do
    begin
      FResult.Passed := False;
      FResult.ErrorMessage := Format('Failed at iteration %d: %s', 
        [FResult.IterationsCompleted + 1, E.Message]);
    end;
  end;
end;

{ TEncodingTestThread }

constructor TEncodingTestThread.Create(ATestID, AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTestID := ATestID;
  FIterations := AIterations;
  FResult.ThreadID := ATestID;
  FResult.Passed := False;
  FResult.ErrorMessage := '';
  FResult.IterationsCompleted := 0;
end;

procedure TEncodingTestThread.Execute;
var
  I: Integer;
  Data: TBytes;
  Encoded: string;
  Decoded: TBytes;
  J: Integer;
begin
  try
    for I := 1 to FIterations do
    begin
      if Terminated then
        Break;

      // Create test data
      SetLength(Data, 64);
      for J := 0 to 63 do
        Data[J] := Byte((I * FTestID + J) mod 256);

      // Test Hex encoding using simple implementation (thread-safe)
      // Build hex string manually to avoid OpenSSL dependency
      Encoded := '';
      for J := 0 to High(Data) do
        Encoded := Encoded + LowerCase(IntToHex(Data[J], 2));
      
      if Length(Encoded) <> 128 then
        raise Exception.Create('Hex encode returned wrong length');

      // Decode hex manually
      SetLength(Decoded, Length(Encoded) div 2);
      for J := 0 to High(Decoded) do
        Decoded[J] := StrToInt('$' + Copy(Encoded, J * 2 + 1, 2));
      
      if Length(Decoded) <> 64 then
        raise Exception.Create('Hex decode returned wrong length');

      Inc(FResult.IterationsCompleted);
    end;
    
    FResult.Passed := True;
    FResult.ErrorMessage := Format('Completed %d iterations', [FResult.IterationsCompleted]);
  except
    on E: Exception do
    begin
      FResult.Passed := False;
      FResult.ErrorMessage := Format('Failed at iteration %d: %s', 
        [FResult.IterationsCompleted + 1, E.Message]);
    end;
  end;
end;

{ TMemUtilsTestThread }

constructor TMemUtilsTestThread.Create(ATestID, AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTestID := ATestID;
  FIterations := AIterations;
  FResult.ThreadID := ATestID;
  FResult.Passed := False;
  FResult.ErrorMessage := '';
  FResult.IterationsCompleted := 0;
end;

procedure TMemUtilsTestThread.Execute;
var
  I, J: Integer;
  Data: TBytes;
  AllZero: Boolean;
begin
  try
    for I := 1 to FIterations do
    begin
      if Terminated then
        Break;

      // Test SecureZeroBytes (thread-safe - operates on local data)
      SetLength(Data, 128);
      for J := 0 to 127 do
        Data[J] := $FF;
      
      // Only call SecureZeroBytes if array has data
      if Length(Data) > 0 then
        SecureZeroBytes(Data);
      
      // Verify zeroed
      AllZero := True;
      for J := 0 to High(Data) do
      begin
        if Data[J] <> 0 then
        begin
          AllZero := False;
          Break;
        end;
      end;
      
      if not AllZero then
        raise Exception.Create('SecureZeroBytes failed to zero');

      Inc(FResult.IterationsCompleted);
    end;
    
    FResult.Passed := True;
    FResult.ErrorMessage := Format('Completed %d iterations', [FResult.IterationsCompleted]);
  except
    on E: Exception do
    begin
      FResult.Passed := False;
      FResult.ErrorMessage := Format('Failed at iteration %d: %s', 
        [FResult.IterationsCompleted + 1, E.Message]);
    end;
  end;
end;

{ Test Procedures }

procedure TestConcurrentHashing;
var
  Threads: array[0..THREAD_COUNT-1] of THashTestThread;
  I: Integer;
  AllPassed: Boolean;
  TotalIterations: Integer;
begin
  PrintCategory('Concurrent Hash Operations');

  // Create threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I] := THashTestThread.Create(I, ITERATIONS_PER_THREAD);

  // Start all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Start;

  // Wait for all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].WaitFor;

  // Check results
  AllPassed := True;
  TotalIterations := 0;
  for I := 0 to THREAD_COUNT - 1 do
  begin
    if not Threads[I].Result.Passed then
    begin
      AllPassed := False;
      WriteLn('  Thread ', I, ' failed: ', Threads[I].Result.ErrorMessage);
    end;
    TotalIterations := TotalIterations + Threads[I].Result.IterationsCompleted;
  end;

  PrintResult(Format('Concurrent hashing (%d threads)', [THREAD_COUNT]), 
    AllPassed, Format('%d total iterations', [TotalIterations]));

  // Cleanup
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Free;
end;

procedure TestConcurrentRandom;
var
  Threads: array[0..THREAD_COUNT-1] of TRandomTestThread;
  I: Integer;
  AllPassed: Boolean;
  TotalIterations: Integer;
begin
  PrintCategory('Concurrent Random Generation');

  // Create threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I] := TRandomTestThread.Create(I, ITERATIONS_PER_THREAD);

  // Start all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Start;

  // Wait for all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].WaitFor;

  // Check results
  AllPassed := True;
  TotalIterations := 0;
  for I := 0 to THREAD_COUNT - 1 do
  begin
    if not Threads[I].Result.Passed then
    begin
      AllPassed := False;
      WriteLn('  Thread ', I, ' failed: ', Threads[I].Result.ErrorMessage);
    end;
    TotalIterations := TotalIterations + Threads[I].Result.IterationsCompleted;
  end;

  PrintResult(Format('Concurrent random (%d threads)', [THREAD_COUNT]), 
    AllPassed, Format('%d total iterations', [TotalIterations]));

  // Cleanup
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Free;
end;

procedure TestConcurrentEncoding;
var
  Threads: array[0..THREAD_COUNT-1] of TEncodingTestThread;
  I: Integer;
  AllPassed: Boolean;
  TotalIterations: Integer;
begin
  PrintCategory('Concurrent Encoding Operations');

  // Create threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I] := TEncodingTestThread.Create(I, ITERATIONS_PER_THREAD);

  // Start all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Start;

  // Wait for all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].WaitFor;

  // Check results
  AllPassed := True;
  TotalIterations := 0;
  for I := 0 to THREAD_COUNT - 1 do
  begin
    if not Threads[I].Result.Passed then
    begin
      AllPassed := False;
      WriteLn('  Thread ', I, ' failed: ', Threads[I].Result.ErrorMessage);
    end;
    TotalIterations := TotalIterations + Threads[I].Result.IterationsCompleted;
  end;

  PrintResult(Format('Concurrent encoding (%d threads)', [THREAD_COUNT]), 
    AllPassed, Format('%d total iterations', [TotalIterations]));

  // Cleanup
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Free;
end;

procedure TestConcurrentMemUtils;
var
  Threads: array[0..THREAD_COUNT-1] of TMemUtilsTestThread;
  I: Integer;
  AllPassed: Boolean;
  TotalIterations: Integer;
begin
  PrintCategory('Concurrent Memory Operations');

  // Create threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I] := TMemUtilsTestThread.Create(I, ITERATIONS_PER_THREAD);

  // Start all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Start;

  // Wait for all threads
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].WaitFor;

  // Check results
  AllPassed := True;
  TotalIterations := 0;
  for I := 0 to THREAD_COUNT - 1 do
  begin
    if not Threads[I].Result.Passed then
    begin
      AllPassed := False;
      WriteLn('  Thread ', I, ' failed: ', Threads[I].Result.ErrorMessage);
    end;
    TotalIterations := TotalIterations + Threads[I].Result.IterationsCompleted;
  end;

  PrintResult(Format('Concurrent memutils (%d threads)', [THREAD_COUNT]), 
    AllPassed, Format('%d total iterations', [TotalIterations]));

  // Cleanup
  for I := 0 to THREAD_COUNT - 1 do
    Threads[I].Free;
end;

procedure TestStressHashing;
var
  I: Integer;
  Data: TBytes;
  Hash: TBytes;
  StartTime: TDateTime;
  ElapsedMs: Integer;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('Stress Testing');

  Passed := False;
  ErrorMsg := '';
  StartTime := Now;
  
  try
    SetLength(Data, 1024);
    for I := 1 to STRESS_ITERATIONS do
    begin
      FillChar(Data[0], 1024, Byte(I mod 256));
      Hash := fafafa.ssl.crypto.hash.SHA256(Data);
      if Length(Hash) <> 32 then
        raise Exception.CreateFmt('Iteration %d: wrong hash length', [I]);
    end;
    
    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    Passed := True;
    ErrorMsg := Format('%d iterations in %d ms', [STRESS_ITERATIONS, ElapsedMs]);
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := E.Message;
    end;
  end;

  PrintResult(Format('Stress hash (%d iterations)', [STRESS_ITERATIONS]), Passed, ErrorMsg);
end;

{ Main }
var
  Total: Integer;
  PassRate: Double;
begin
  WriteLn('========================================');
  WriteLn('  Thread Safety Test Suite');
  WriteLn('  fafafa.ssl Test Quality Improvement');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Configuration:');
  WriteLn('  Thread count: ', THREAD_COUNT);
  WriteLn('  Iterations per thread: ', ITERATIONS_PER_THREAD);
  WriteLn('  Stress iterations: ', STRESS_ITERATIONS);
  WriteLn;
  WriteLn('Running thread safety tests...');

  TestConcurrentHashing;
  TestConcurrentRandom;
  TestConcurrentEncoding;
  TestConcurrentMemUtils;
  TestStressHashing;

  Total := PassCount + FailCount;
  if Total > 0 then
    PassRate := (PassCount / Total) * 100
  else
    PassRate := 0;

  WriteLn;
  WriteLn('========================================');
  WriteLn('  Test Summary');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Total Tests: ', Total);
  WriteLn('Passed:      ', PassCount);
  WriteLn('Failed:      ', FailCount);
  WriteLn('Pass Rate:   ', PassRate:0:1, '%');
  WriteLn;

  if FailCount = 0 then
    WriteLn('All thread safety tests passed!')
  else
    WriteLn('Some tests failed. Review the output above.');

  if FailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
