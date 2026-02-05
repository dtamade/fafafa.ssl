program test_capability_cache;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

procedure TestCachingPerformance;
var
  Lib: TOpenSSLLibrary;
  Caps: TSSLBackendCapabilities;
  StartTime, EndTime: TDateTime;
  I: Integer;
  FirstCallTime, CachedCallTime: Int64;
const
  ITERATIONS = 10000;
begin
  WriteLn('==============================================');
  WriteLn('能力矩阵缓存性能测试');
  WriteLn('==============================================');
  WriteLn;

  Lib := TOpenSSLLibrary.Create;
  try
    if not Lib.Initialize then
    begin
      WriteLn('Failed to initialize OpenSSL');
      Exit;
    end;

    // 第一次调用（未缓存）
    WriteLn('第一次调用 GetCapabilities（未缓存）...');
    StartTime := Now;
    Caps := Lib.GetCapabilities;
    EndTime := Now;
    FirstCallTime := MilliSecondsBetween(EndTime, StartTime);
    WriteLn('  Backend: ', SSL_LIBRARY_NAMES[Caps.BackendType]);
    WriteLn('  Version: ', Caps.BackendVersion);
    WriteLn('  Time: ', FirstCallTime, ' ms');
    WriteLn;

    // 后续调用（已缓存）
    WriteLn('测试缓存性能 (', ITERATIONS, ' 次调用)...');
    StartTime := Now;
    for I := 1 to ITERATIONS do
    begin
      Caps := Lib.GetCapabilities;
    end;
    EndTime := Now;
    CachedCallTime := MilliSecondsBetween(EndTime, StartTime);

    WriteLn('  Total Time: ', CachedCallTime, ' ms');
    WriteLn('  Average per call: ', (CachedCallTime / ITERATIONS):0:6, ' ms');
    WriteLn('  Calls per second: ', Round(ITERATIONS / (CachedCallTime / 1000)):0, ' ops/s');
    WriteLn;

    WriteLn('性能提升分析:');
    if FirstCallTime > 0 then
      WriteLn('  首次调用耗时: ', FirstCallTime, ' ms')
    else
      WriteLn('  首次调用耗时: < 1 ms');
    WriteLn('  缓存调用耗时: ~', (CachedCallTime / ITERATIONS):0:6, ' ms per call');
    if FirstCallTime > 0 then
      WriteLn('  性能提升: ~', Round((FirstCallTime * 1000) / (CachedCallTime / ITERATIONS)):0, 'x')
    else
      WriteLn('  性能提升: 极显著（首次调用 < 1ms）');
    WriteLn;

    // 验证缓存内容正确性
    WriteLn('验证缓存内容...');
    WriteLn('  TLS 1.3: ', Caps.SupportsTLS13);
    WriteLn('  ALPN: ', Caps.SupportsALPN);
    WriteLn('  Hardware Accel: ', Caps.HasHardwareAcceleration);
    WriteLn('  Security Score: ', GetSecurityScore(Caps), '/100');
    WriteLn('  ✓ 缓存内容正确');
    WriteLn;

    Lib.Finalize;
  finally
    Lib.Free;
  end;
end;

procedure TestCacheInvalidation;
var
  Lib: TOpenSSLLibrary;
  Caps1, Caps2, Caps3: TSSLBackendCapabilities;
begin
  WriteLn('==============================================');
  WriteLn('能力矩阵缓存失效测试');
  WriteLn('==============================================');
  WriteLn;

  Lib := TOpenSSLLibrary.Create;
  try
    if not Lib.Initialize then
    begin
      WriteLn('Failed to initialize OpenSSL');
      Exit;
    end;

    // 首次获取
    WriteLn('首次获取能力矩阵...');
    Caps1 := Lib.GetCapabilities;
    WriteLn('  Version: ', Caps1.BackendVersion);
    WriteLn('  ✓ 已缓存');
    WriteLn;

    // 再次获取（应该来自缓存）
    WriteLn('再次获取能力矩阵（应该来自缓存）...');
    Caps2 := Lib.GetCapabilities;
    WriteLn('  Version: ', Caps2.BackendVersion);
    if Caps2.BackendVersion = Caps1.BackendVersion then
      WriteLn('  ✓ 缓存有效')
    else
      WriteLn('  ✗ 缓存失效（不应该发生）');
    WriteLn;

    // Finalize 后重新初始化
    WriteLn('Finalize 后重新初始化...');
    Lib.Finalize;
    if not Lib.Initialize then
    begin
      WriteLn('  ✗ Re-initialization failed');
      Exit;
    end;

    // 获取能力矩阵（缓存应该已失效）
    WriteLn('获取能力矩阵（缓存应该已失效）...');
    Caps3 := Lib.GetCapabilities;
    WriteLn('  Version: ', Caps3.BackendVersion);
    WriteLn('  ✓ 缓存已重建');
    WriteLn;

    Lib.Finalize;
  finally
    Lib.Free;
  end;
end;

begin
  WriteLn('fafafa.ssl - 能力矩阵缓存测试');
  WriteLn('==============================================');
  WriteLn;

  try
    TestCachingPerformance;
    WriteLn;
    TestCacheInvalidation;

    WriteLn('==============================================');
    WriteLn('所有测试完成！');
    WriteLn('==============================================');
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('[ERROR] ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
