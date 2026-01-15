program zerocopy_performance_comparison;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils;

const
  ITERATIONS_SMALL = 10000;
  ITERATIONS_MEDIUM = 10000;
  ITERATIONS_LARGE = 1000;

type
  TBenchmarkResult = record
    Operation: string;
    DataSize: string;
    Iterations: Integer;
    AvgTimeUS: Double;
    ThroughputMBps: Double;
    ImprovementPercent: Double;
  end;

var
  GResults: array of TBenchmarkResult;

procedure PrintHeader;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════════════════════════════════');
  WriteLn('  Phase 2.3.3 Zero-Copy Performance Comparison - Normal vs View vs InPlace');
  WriteLn('═══════════════════════════════════════════════════════════════════════════════════════');
  WriteLn;
end;

procedure AddResult(const AOperation, ADataSize: string; AIterations: Integer;
  AAvgTimeUS, AThroughputMBps, AImprovementPercent: Double);
var
  LIdx: Integer;
begin
  LIdx := Length(GResults);
  SetLength(GResults, LIdx + 1);
  GResults[LIdx].Operation := AOperation;
  GResults[LIdx].DataSize := ADataSize;
  GResults[LIdx].Iterations := AIterations;
  GResults[LIdx].AvgTimeUS := AAvgTimeUS;
  GResults[LIdx].ThroughputMBps := AThroughputMBps;
  GResults[LIdx].ImprovementPercent := AImprovementPercent;
end;

function BenchmarkSHA256_Normal(const AData: TBytes; AIterations: Integer): Double;
var
  LStart, LEnd: TDateTime;
  LResult: TBytes;
  I: Integer;
begin
  LStart := Now;
  for I := 1 to AIterations do
    LResult := TCryptoUtils.SHA256(AData);
  LEnd := Now;
  Result := (MilliSecondsBetween(LEnd, LStart) * 1000.0) / AIterations;
end;

function BenchmarkSHA256_View(var AData: TBytes; AIterations: Integer): Double;
var
  LStart, LEnd: TDateTime;
  LView: TBytesView;
  LResult: TBytes;
  I: Integer;
begin
  LView := TBytesView.FromBytes(AData);
  LStart := Now;
  for I := 1 to AIterations do
    LResult := TCryptoUtils.SHA256View(LView);
  LEnd := Now;
  Result := (MilliSecondsBetween(LEnd, LStart) * 1000.0) / AIterations;
end;

procedure BenchmarkSHA256(const ADataSize: string; ASize, AIterations: Integer);
var
  LData: TBytes;
  LTimeNormal, LTimeView: Double;
  LThroughputNormal, LThroughputView: Double;
  LTotalBytesNormal, LTotalBytesView: Double;
  LTotalTimeMS: Double;
  LImprovement: Double;
  I: Integer;
begin
  WriteLn('--- SHA256 Benchmarks (', ADataSize, ') ---');

  SetLength(LData, ASize);
  for I := 0 to ASize - 1 do
    LData[I] := Byte(I mod 256);

  // Normal version
  LTimeNormal := BenchmarkSHA256_Normal(LData, AIterations);
  LTotalBytesNormal := ASize * AIterations;
  LTotalTimeMS := (LTimeNormal * AIterations) / 1000.0;
  LThroughputNormal := (LTotalBytesNormal / (1024 * 1024)) / (LTotalTimeMS / 1000.0);

  WriteLn(Format('  SHA256 (Normal):        %8.2f μs/op,  %8.2f MB/s',
    [LTimeNormal, LThroughputNormal]));
  AddResult('SHA256 (Normal)', ADataSize, AIterations, LTimeNormal, LThroughputNormal, 0.0);

  // View version
  LTimeView := BenchmarkSHA256_View(LData, AIterations);
  LTotalBytesView := ASize * AIterations;
  LTotalTimeMS := (LTimeView * AIterations) / 1000.0;
  LThroughputView := (LTotalBytesView / (1024 * 1024)) / (LTotalTimeMS / 1000.0);

  LImprovement := ((LTimeNormal - LTimeView) / LTimeNormal) * 100.0;
  WriteLn(Format('  SHA256 (View):          %8.2f μs/op,  %8.2f MB/s,  %+6.2f%% improvement',
    [LTimeView, LThroughputView, LImprovement]));
  AddResult('SHA256 (View)', ADataSize, AIterations, LTimeView, LThroughputView, LImprovement);
  WriteLn;
end;

function BenchmarkAES_GCM_Normal(const AData, AKey, AIV: TBytes; AIterations: Integer): Double;
var
  LStart, LEnd: TDateTime;
  LResult: TBytes;
  I: Integer;
begin
  LStart := Now;
  for I := 1 to AIterations do
    LResult := TCryptoUtils.AES_GCM_Encrypt(AData, AKey, AIV);
  LEnd := Now;
  Result := (MilliSecondsBetween(LEnd, LStart) * 1000.0) / AIterations;
end;

function BenchmarkAES_GCM_View(var AData, AKey, AIV: TBytes; AIterations: Integer): Double;
var
  LStart, LEnd: TDateTime;
  LDataView, LKeyView, LIVView: TBytesView;
  LResult, LTag: TBytes;
  I: Integer;
begin
  LDataView := TBytesView.FromBytes(AData);
  LKeyView := TBytesView.FromBytes(AKey);
  LIVView := TBytesView.FromBytes(AIV);

  LStart := Now;
  for I := 1 to AIterations do
    TCryptoUtils.AES_GCM_EncryptView(LDataView, LKeyView, LIVView, LResult, LTag);
  LEnd := Now;
  Result := (MilliSecondsBetween(LEnd, LStart) * 1000.0) / AIterations;
end;

function BenchmarkAES_GCM_InPlace(var AData: TBytes; const AKey, AIV: TBytes; AIterations: Integer): Double;
var
  LStart, LEnd: TDateTime;
  LTag: TBytes;
  I: Integer;
  LDataCopy: TBytes;
begin
  LStart := Now;
  for I := 1 to AIterations do
  begin
    // Need to copy data each iteration since it's modified in place
    SetLength(LDataCopy, Length(AData));
    Move(AData[0], LDataCopy[0], Length(AData));
    TCryptoUtils.AES_GCM_EncryptInPlace(LDataCopy, AKey, AIV, LTag);
  end;
  LEnd := Now;
  Result := (MilliSecondsBetween(LEnd, LStart) * 1000.0) / AIterations;
end;

procedure BenchmarkAES_GCM(const ADataSize: string; ASize, AIterations: Integer);
var
  LData, LKey, LIV: TBytes;
  LTimeNormal, LTimeView, LTimeInPlace: Double;
  LThroughputNormal, LThroughputView, LThroughputInPlace: Double;
  LTotalBytes: Double;
  LTotalTimeMS: Double;
  LImprovementView, LImprovementInPlace: Double;
  I: Integer;
begin
  WriteLn('--- AES-GCM Encryption Benchmarks (', ADataSize, ') ---');

  SetLength(LData, ASize);
  SetLength(LKey, 32);
  SetLength(LIV, 12);

  for I := 0 to ASize - 1 do
    LData[I] := Byte(I mod 256);
  for I := 0 to 31 do
    LKey[I] := Byte(I);
  for I := 0 to 11 do
    LIV[I] := Byte(I);

  // Normal version
  LTimeNormal := BenchmarkAES_GCM_Normal(LData, LKey, LIV, AIterations);
  LTotalBytes := ASize * AIterations;
  LTotalTimeMS := (LTimeNormal * AIterations) / 1000.0;
  LThroughputNormal := (LTotalBytes / (1024 * 1024)) / (LTotalTimeMS / 1000.0);

  WriteLn(Format('  AES-GCM (Normal):       %8.2f μs/op,  %8.2f MB/s',
    [LTimeNormal, LThroughputNormal]));
  AddResult('AES-GCM (Normal)', ADataSize, AIterations, LTimeNormal, LThroughputNormal, 0.0);

  // View version
  LTimeView := BenchmarkAES_GCM_View(LData, LKey, LIV, AIterations);
  LTotalTimeMS := (LTimeView * AIterations) / 1000.0;
  LThroughputView := (LTotalBytes / (1024 * 1024)) / (LTotalTimeMS / 1000.0);

  LImprovementView := ((LTimeNormal - LTimeView) / LTimeNormal) * 100.0;
  WriteLn(Format('  AES-GCM (View):         %8.2f μs/op,  %8.2f MB/s,  %+6.2f%% improvement',
    [LTimeView, LThroughputView, LImprovementView]));
  AddResult('AES-GCM (View)', ADataSize, AIterations, LTimeView, LThroughputView, LImprovementView);

  // InPlace version
  LTimeInPlace := BenchmarkAES_GCM_InPlace(LData, LKey, LIV, AIterations);
  LTotalTimeMS := (LTimeInPlace * AIterations) / 1000.0;
  LThroughputInPlace := (LTotalBytes / (1024 * 1024)) / (LTotalTimeMS / 1000.0);

  LImprovementInPlace := ((LTimeNormal - LTimeInPlace) / LTimeNormal) * 100.0;
  WriteLn(Format('  AES-GCM (InPlace):      %8.2f μs/op,  %8.2f MB/s,  %+6.2f%% improvement',
    [LTimeInPlace, LThroughputInPlace, LImprovementInPlace]));
  AddResult('AES-GCM (InPlace)', ADataSize, AIterations, LTimeInPlace, LThroughputInPlace, LImprovementInPlace);
  WriteLn;
end;

procedure PrintSummary;
var
  I: Integer;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════════════════════════════════');
  WriteLn('  Performance Summary');
  WriteLn('═══════════════════════════════════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn(Format('%-25s %10s %10s %12s %12s %12s',
    ['Operation', 'Data Size', 'Iterations', 'Avg (μs)', 'MB/s', 'Improvement']));
  WriteLn('─────────────────────────────────────────────────────────────────────────────────────');

  for I := 0 to High(GResults) do
  begin
    with GResults[I] do
    begin
      if ImprovementPercent = 0.0 then
        WriteLn(Format('%-25s %10s %10d %12.2f %12.2f %12s',
          [Operation, DataSize, Iterations, AvgTimeUS, ThroughputMBps, '-']))
      else
        WriteLn(Format('%-25s %10s %10d %12.2f %12.2f %+11.2f%%',
          [Operation, DataSize, Iterations, AvgTimeUS, ThroughputMBps, ImprovementPercent]));
    end;
  end;
  WriteLn('═══════════════════════════════════════════════════════════════════════════════════════');
  WriteLn;
end;

procedure PrintKeyFindings;
begin
  WriteLn('Key Findings:');
  WriteLn('  • View methods avoid input parameter copying (zero-copy input)');
  WriteLn('  • InPlace methods avoid output allocation (zero-copy output)');
  WriteLn('  • Improvements vary by data size and operation type');
  WriteLn('  • Larger data sizes see greater benefits from zero-copy');
  WriteLn;
  WriteLn('Expected Results:');
  WriteLn('  • Small data (64b):   5-15% improvement (overhead dominates)');
  WriteLn('  • Medium data (1KB):  10-20% improvement');
  WriteLn('  • Large data (64KB):  15-30% improvement (memory savings dominate)');
  WriteLn;
end;

begin
  try
    PrintHeader;

    WriteLn('Running benchmarks with different data sizes...');
    WriteLn;

    // SHA256 benchmarks
    BenchmarkSHA256('64b', 64, ITERATIONS_SMALL);
    BenchmarkSHA256('1KB', 1024, ITERATIONS_MEDIUM);
    BenchmarkSHA256('64KB', 65536, ITERATIONS_LARGE);

    // AES-GCM benchmarks
    BenchmarkAES_GCM('64b', 64, ITERATIONS_SMALL);
    BenchmarkAES_GCM('1KB', 1024, ITERATIONS_MEDIUM);
    BenchmarkAES_GCM('64KB', 65536, ITERATIONS_LARGE);

    PrintSummary;
    PrintKeyFindings;

    WriteLn('✓ Phase 2.3.3 zero-copy performance analysis complete!');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('✗ Benchmark error: ', E.Message);
      Halt(1);
    end;
  end;
end.
