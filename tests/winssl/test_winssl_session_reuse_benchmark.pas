program test_winssl_session_reuse_benchmark;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  Windows, SysUtils, Classes, WinSock2, Math,

  fafafa.ssl.base,
  fafafa.ssl.winssl.lib;

type
  TSessionReuseMetrics = record
    WithoutSessionReuse: record
      TotalTime: Double;
      AvgTime: Double;
      MinTime: Double;
      MaxTime: Double;
      SuccessCount: Integer;
    end;
    WithSessionReuse: record
      TotalTime: Double;
      AvgTime: Double;
      MinTime: Double;
      MaxTime: Double;
      SuccessCount: Integer;
      ReuseCount: Integer;
    end;
    ImprovementPercent: Double;
  end;

var
  Frequency: Int64;

function GetTimestamp: Int64;
var
  LCounter: Int64;
begin
  QueryPerformanceCounter(LCounter);
  Result := LCounter;
end;

function TimestampToMilliseconds(aStart, aEnd: Int64): Double;
begin
  Result := ((aEnd - aStart) * 1000.0) / Frequency;
end;

procedure InitWinsock;
var
  LWSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2, 2), LWSAData) <> 0 then
  begin
    WriteLn('错误: 无法初始化 Winsock');
    Halt(1);
  end;
end;

procedure CleanupWinsock;
begin
  WSACleanup;
end;

function ConnectToHost(const aHost: string; aPort: Word; out aSocket: TSocket): Boolean;
var
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LInAddr: TInAddr;
  LTimeout: Integer;
begin
  Result := False;
  aSocket := INVALID_SOCKET;

  aSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aSocket = INVALID_SOCKET then
    Exit;

  LTimeout := 10000;
  setsockopt(aSocket, SOL_SOCKET, SO_RCVTIMEO, @LTimeout, SizeOf(LTimeout));
  setsockopt(aSocket, SOL_SOCKET, SO_SNDTIMEO, @LTimeout, SizeOf(LTimeout));

  LHostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if LHostEnt = nil then
  begin
    closesocket(aSocket);
    aSocket := INVALID_SOCKET;
    Exit;
  end;

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(aPort);
  Move(LHostEnt^.h_addr_list^^, LInAddr, SizeOf(LInAddr));
  LAddr.sin_addr := LInAddr;

  Result := connect(aSocket, @LAddr, SizeOf(LAddr)) = 0;
  if not Result then
  begin
    closesocket(aSocket);
    aSocket := INVALID_SOCKET;
  end;
end;

function BenchmarkWithoutSessionReuse(const aHost: string; aIterations: Integer): TSessionReuseMetrics;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  i: Integer;
  LStart, LEnd: Int64;
  LTime: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.WithoutSessionReuse.MinTime := MaxDouble;
  Result.WithoutSessionReuse.MaxTime := 0;

  WriteLn('【测试 1】无 Session 复用 - 每次完整握手');
  WriteLn('测试服务器: ', aHost);
  WriteLn('迭代次数: ', aIterations);
  WriteLn('---');

  LLib := CreateWinSSLLibrary;
  if not LLib.Initialize then
  begin
    WriteLn('错误: 无法初始化 WinSSL');
    Exit;
  end;

  for i := 1 to aIterations do
  begin
    LSocket := INVALID_SOCKET;

    // 每次创建新的 Context（不复用凭据）
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetServerName(aHost);
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.SetVerifyMode([]);

    if ConnectToHost(aHost, 443, LSocket) then
    begin
      LConn := LContext.CreateConnection(LSocket);

      LStart := GetTimestamp;
      if LConn.Connect then
      begin
        LEnd := GetTimestamp;
        LTime := TimestampToMilliseconds(LStart, LEnd);

        Inc(Result.WithoutSessionReuse.SuccessCount);
        Result.WithoutSessionReuse.TotalTime += LTime;

        if LTime < Result.WithoutSessionReuse.MinTime then
          Result.WithoutSessionReuse.MinTime := LTime;
        if LTime > Result.WithoutSessionReuse.MaxTime then
          Result.WithoutSessionReuse.MaxTime := LTime;

        LConn.Shutdown;
      end;

      closesocket(LSocket);
    end;

    if (i mod 10 = 0) then
      Write('.');
  end;

  WriteLn;

  if Result.WithoutSessionReuse.SuccessCount > 0 then
    Result.WithoutSessionReuse.AvgTime := Result.WithoutSessionReuse.TotalTime / Result.WithoutSessionReuse.SuccessCount;

  WriteLn('完成: ', Result.WithoutSessionReuse.SuccessCount, '/', aIterations, ' 次成功');
  WriteLn('平均握手时间: ', Format('%.2f ms', [Result.WithoutSessionReuse.AvgTime]));
  WriteLn;
end;

function BenchmarkWithSessionReuse(const aHost: string; aIterations: Integer): TSessionReuseMetrics;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LSession: ISSLSession;
  i: Integer;
  LStart, LEnd: Int64;
  LTime: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.WithSessionReuse.MinTime := MaxDouble;
  Result.WithSessionReuse.MaxTime := 0;

  WriteLn('【测试 2】有 Session 复用 - 快速握手');
  WriteLn('测试服务器: ', aHost);
  WriteLn('迭代次数: ', aIterations);
  WriteLn('---');

  LLib := CreateWinSSLLibrary;
  if not LLib.Initialize then
  begin
    WriteLn('错误: 无法初始化 WinSSL');
    Exit;
  end;

  // 复用同一个 Context（复用凭据句柄）
  LContext := LLib.CreateContext(sslCtxClient);
  LContext.SetServerName(aHost);
  LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  LContext.SetVerifyMode([]);

  // 首次连接获取 Session
  LSocket := INVALID_SOCKET;
  if ConnectToHost(aHost, 443, LSocket) then
  begin
    LConn := LContext.CreateConnection(LSocket);
    if LConn.Connect then
    begin
      LSession := LConn.GetSession;
      WriteLn('首次连接成功，获取 Session');
      LConn.Shutdown;
    end;
    closesocket(LSocket);
  end;

  // 后续连接复用 Session
  for i := 1 to aIterations do
  begin
    LSocket := INVALID_SOCKET;

    if ConnectToHost(aHost, 443, LSocket) then
    begin
      LConn := LContext.CreateConnection(LSocket);

      // 设置 Session 以启用复用
      if Assigned(LSession) then
        LConn.SetSession(LSession);

      LStart := GetTimestamp;
      if LConn.Connect then
      begin
        LEnd := GetTimestamp;
        LTime := TimestampToMilliseconds(LStart, LEnd);

        Inc(Result.WithSessionReuse.SuccessCount);
        Result.WithSessionReuse.TotalTime += LTime;

        if LTime < Result.WithSessionReuse.MinTime then
          Result.WithSessionReuse.MinTime := LTime;
        if LTime > Result.WithSessionReuse.MaxTime then
          Result.WithSessionReuse.MaxTime := LTime;

        // 检查是否真的复用了 Session
        if LConn.IsSessionResumed then
          Inc(Result.WithSessionReuse.ReuseCount);

        LConn.Shutdown;
      end;

      closesocket(LSocket);
    end;

    if (i mod 10 = 0) then
      Write('.');
  end;

  WriteLn;

  if Result.WithSessionReuse.SuccessCount > 0 then
    Result.WithSessionReuse.AvgTime := Result.WithSessionReuse.TotalTime / Result.WithSessionReuse.SuccessCount;

  WriteLn('完成: ', Result.WithSessionReuse.SuccessCount, '/', aIterations, ' 次成功');
  WriteLn('Session 复用: ', Result.WithSessionReuse.ReuseCount, '/', Result.WithSessionReuse.SuccessCount, ' 次 (',
    Format('%.1f%%', [Result.WithSessionReuse.ReuseCount * 100.0 / Result.WithSessionReuse.SuccessCount]), ')');
  WriteLn('平均握手时间: ', Format('%.2f ms', [Result.WithSessionReuse.AvgTime]));
  WriteLn;
end;

procedure PrintComparisonReport(const aMetrics: TSessionReuseMetrics);
var
  LImprovement: Double;
begin
  WriteLn('=========================================');
  WriteLn('Session 复用性能对比报告');
  WriteLn('=========================================');
  WriteLn;

  WriteLn('【无 Session 复用】');
  WriteLn('  成功连接: ', aMetrics.WithoutSessionReuse.SuccessCount);
  WriteLn('  平均时间: ', Format('%.2f ms', [aMetrics.WithoutSessionReuse.AvgTime]));
  WriteLn('  最小时间: ', Format('%.2f ms', [aMetrics.WithoutSessionReuse.MinTime]));
  WriteLn('  最大时间: ', Format('%.2f ms', [aMetrics.WithoutSessionReuse.MaxTime]));
  WriteLn('  总时间: ', Format('%.2f ms', [aMetrics.WithoutSessionReuse.TotalTime]));
  WriteLn;

  WriteLn('【有 Session 复用】');
  WriteLn('  成功连接: ', aMetrics.WithSessionReuse.SuccessCount);
  WriteLn('  Session 复用: ', aMetrics.WithSessionReuse.ReuseCount, ' 次 (',
    Format('%.1f%%', [aMetrics.WithSessionReuse.ReuseCount * 100.0 / aMetrics.WithSessionReuse.SuccessCount]), ')');
  WriteLn('  平均时间: ', Format('%.2f ms', [aMetrics.WithSessionReuse.AvgTime]));
  WriteLn('  最小时间: ', Format('%.2f ms', [aMetrics.WithSessionReuse.MinTime]));
  WriteLn('  最大时间: ', Format('%.2f ms', [aMetrics.WithSessionReuse.MaxTime]));
  WriteLn('  总时间: ', Format('%.2f ms', [aMetrics.WithSessionReuse.TotalTime]));
  WriteLn;

  if (aMetrics.WithoutSessionReuse.AvgTime > 0) and (aMetrics.WithSessionReuse.AvgTime > 0) then
  begin
    LImprovement := ((aMetrics.WithoutSessionReuse.AvgTime - aMetrics.WithSessionReuse.AvgTime) /
                     aMetrics.WithoutSessionReuse.AvgTime) * 100;

    WriteLn('【性能提升】');
    WriteLn('  时间减少: ', Format('%.2f ms', [aMetrics.WithoutSessionReuse.AvgTime - aMetrics.WithSessionReuse.AvgTime]));
    WriteLn('  性能提升: ', Format('%.1f%%', [LImprovement]));
    WriteLn;

    if LImprovement >= 70 then
      WriteLn('✓ 达到预期性能提升目标（70-90%）')
    else if LImprovement >= 50 then
      WriteLn('⚠ 性能提升显著但低于预期（50-70%）')
    else
      WriteLn('✗ 性能提升低于预期（< 50%）');
  end;

  WriteLn('=========================================');
end;

procedure RunSessionReuseBenchmark;
var
  LMetrics: TSessionReuseMetrics;
  LHost: string;
  LIterations: Integer;
begin
  WriteLn('=========================================');
  WriteLn('WinSSL Session 复用性能基准测试');
  WriteLn('测试日期: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn('=========================================');
  WriteLn;

  LHost := 'www.google.com';
  LIterations := 50;

  WriteLn('测试配置:');
  WriteLn('  目标服务器: ', LHost);
  WriteLn('  迭代次数: ', LIterations);
  WriteLn('  协议版本: TLS 1.2 / TLS 1.3');
  WriteLn;

  // 测试 1: 无 Session 复用
  LMetrics := BenchmarkWithoutSessionReuse(LHost, LIterations);

  // 测试 2: 有 Session 复用
  LMetrics := BenchmarkWithSessionReuse(LHost, LIterations);

  // 打印对比报告
  PrintComparisonReport(LMetrics);
end;

begin
  if not QueryPerformanceFrequency(Frequency) then
  begin
    WriteLn('错误: 系统不支持高精度计时器');
    Halt(1);
  end;

  InitWinsock;
  try
    RunSessionReuseBenchmark;
  finally
    CleanupWinsock;
  end;

  WriteLn;
  WriteLn('按回车键退出...');
  ReadLn;
end.
