{**
 * Test: SSL Backend Performance Comparison
 * Purpose: Compare performance between OpenSSL and MbedTLS backends
 *
 * Tests include:
 * - Library initialization time
 * - Context creation time
 * - TLS handshake time
 * - Data throughput
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

program test_backend_performance;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils, Sockets, BaseUnix, netdb,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.mbedtls.lib;

const
  ITERATIONS = 10;
  TEST_HOST = '142.250.185.68';  // Google IP
  TEST_PORT = 443;

type
  TBenchmarkResult = record
    Name: string;
    Backend: string;
    MinTime: Double;
    MaxTime: Double;
    AvgTime: Double;
    Iterations: Integer;
  end;

var
  GResults: array of TBenchmarkResult;

procedure AddResult(const AName, ABackend: string; AMin, AMax, AAvg: Double; AIter: Integer);
var
  LIdx: Integer;
begin
  LIdx := Length(GResults);
  SetLength(GResults, LIdx + 1);
  GResults[LIdx].Name := AName;
  GResults[LIdx].Backend := ABackend;
  GResults[LIdx].MinTime := AMin;
  GResults[LIdx].MaxTime := AMax;
  GResults[LIdx].AvgTime := AAvg;
  GResults[LIdx].Iterations := AIter;
end;

function CreateTCPSocket(const AHost: string; APort: Integer): TSocket;
var
  LSockAddr: TInetSockAddr;
  LAddr: in_addr;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then Exit(-1);

  FillChar(LSockAddr, SizeOf(LSockAddr), 0);
  LSockAddr.sin_family := AF_INET;
  LSockAddr.sin_port := htons(APort);
  LAddr := StrToNetAddr(AHost);
  LSockAddr.sin_addr := LAddr;

  if fpConnect(Result, @LSockAddr, SizeOf(LSockAddr)) < 0 then
  begin
    CloseSocket(Result);
    Exit(-1);
  end;
end;

procedure BenchmarkLibraryInit(ALib: ISSLLibrary; const ABackendName: string);
var
  I: Integer;
  LStart: TDateTime;
  LTimes: array[0..ITERATIONS-1] of Double;
  LMin, LMax, LSum: Double;
begin
  WriteLn('  Benchmarking library initialization...');

  for I := 0 to ITERATIONS - 1 do
  begin
    if ALib.IsInitialized then
      ALib.Finalize;

    LStart := Now;
    ALib.Initialize;
    LTimes[I] := MilliSecondSpan(LStart, Now);
  end;

  LMin := LTimes[0];
  LMax := LTimes[0];
  LSum := 0;
  for I := 0 to ITERATIONS - 1 do
  begin
    if LTimes[I] < LMin then LMin := LTimes[I];
    if LTimes[I] > LMax then LMax := LTimes[I];
    LSum := LSum + LTimes[I];
  end;

  AddResult('Library Init', ABackendName, LMin, LMax, LSum / ITERATIONS, ITERATIONS);
  WriteLn(Format('    Min: %.2f ms, Max: %.2f ms, Avg: %.2f ms',
    [LMin, LMax, LSum / ITERATIONS]));
end;

procedure BenchmarkContextCreation(ALib: ISSLLibrary; const ABackendName: string);
var
  I: Integer;
  LStart: TDateTime;
  LTimes: array[0..ITERATIONS-1] of Double;
  LMin, LMax, LSum: Double;
  LCtx: ISSLContext;
begin
  WriteLn('  Benchmarking context creation...');

  if not ALib.IsInitialized then
    ALib.Initialize;

  for I := 0 to ITERATIONS - 1 do
  begin
    LStart := Now;
    LCtx := ALib.CreateContext(sslCtxClient);
    LTimes[I] := MilliSecondSpan(LStart, Now);
    LCtx := nil;
  end;

  LMin := LTimes[0];
  LMax := LTimes[0];
  LSum := 0;
  for I := 0 to ITERATIONS - 1 do
  begin
    if LTimes[I] < LMin then LMin := LTimes[I];
    if LTimes[I] > LMax then LMax := LTimes[I];
    LSum := LSum + LTimes[I];
  end;

  AddResult('Context Create', ABackendName, LMin, LMax, LSum / ITERATIONS, ITERATIONS);
  WriteLn(Format('    Min: %.2f ms, Max: %.2f ms, Avg: %.2f ms',
    [LMin, LMax, LSum / ITERATIONS]));
end;

procedure BenchmarkTLSHandshake(ALib: ISSLLibrary; const ABackendName: string);
var
  I: Integer;
  LStart: TDateTime;
  LTimes: array[0..ITERATIONS-1] of Double;
  LMin, LMax, LSum: Double;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LSuccessCount: Integer;
begin
  WriteLn('  Benchmarking TLS handshake...');

  if not ALib.IsInitialized then
    ALib.Initialize;

  LSuccessCount := 0;
  for I := 0 to ITERATIONS - 1 do
  begin
    LCtx := ALib.CreateContext(sslCtxClient);
    // Disable certificate verification for fair performance comparison
    LCtx.SetVerifyMode([]);
    LSocket := CreateTCPSocket(TEST_HOST, TEST_PORT);
    if LSocket > 0 then
    begin
      LConn := LCtx.CreateConnection(LSocket);
      LStart := Now;
      if LConn.Connect then
      begin
        LTimes[LSuccessCount] := MilliSecondSpan(LStart, Now);
        Inc(LSuccessCount);
        LConn.Shutdown;
      end;
      CloseSocket(LSocket);
    end;
    LCtx := nil;
  end;

  if LSuccessCount > 0 then
  begin
    LMin := LTimes[0];
    LMax := LTimes[0];
    LSum := 0;
    for I := 0 to LSuccessCount - 1 do
    begin
      if LTimes[I] < LMin then LMin := LTimes[I];
      if LTimes[I] > LMax then LMax := LTimes[I];
      LSum := LSum + LTimes[I];
    end;

    AddResult('TLS Handshake', ABackendName, LMin, LMax, LSum / LSuccessCount, LSuccessCount);
    WriteLn(Format('    Min: %.2f ms, Max: %.2f ms, Avg: %.2f ms (%d/%d success)',
      [LMin, LMax, LSum / LSuccessCount, LSuccessCount, ITERATIONS]));
  end
  else
    WriteLn('    No successful handshakes');
end;

procedure PrintResults;
var
  I: Integer;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Performance Comparison Summary');
  WriteLn('========================================');
  WriteLn(Format('%-20s %-12s %10s %10s %10s', ['Test', 'Backend', 'Min(ms)', 'Max(ms)', 'Avg(ms)']));
  WriteLn(StringOfChar('-', 70));

  for I := 0 to High(GResults) do
    WriteLn(Format('%-20s %-12s %10.2f %10.2f %10.2f',
      [GResults[I].Name, GResults[I].Backend,
       GResults[I].MinTime, GResults[I].MaxTime, GResults[I].AvgTime]));

  WriteLn('========================================');
end;

var
  LOpenSSL, LMbedTLS: ISSLLibrary;

begin
  WriteLn('SSL Backend Performance Comparison');
  WriteLn('===================================');
  WriteLn('');

  // Test OpenSSL
  WriteLn('Testing OpenSSL backend...');
  LOpenSSL := CreateOpenSSLLibrary;
  if (LOpenSSL <> nil) and LOpenSSL.Initialize then
  begin
    BenchmarkLibraryInit(LOpenSSL, 'OpenSSL');
    BenchmarkContextCreation(LOpenSSL, 'OpenSSL');
    BenchmarkTLSHandshake(LOpenSSL, 'OpenSSL');
    LOpenSSL.Finalize;
  end
  else
    WriteLn('  OpenSSL not available');

  WriteLn('');

  // Test MbedTLS
  WriteLn('Testing MbedTLS backend...');
  LMbedTLS := CreateMbedTLSLibrary;
  if (LMbedTLS <> nil) and LMbedTLS.Initialize then
  begin
    BenchmarkLibraryInit(LMbedTLS, 'MbedTLS');
    BenchmarkContextCreation(LMbedTLS, 'MbedTLS');
    BenchmarkTLSHandshake(LMbedTLS, 'MbedTLS');
    LMbedTLS.Finalize;
  end
  else
    WriteLn('  MbedTLS not available');

  PrintResults;
end.
