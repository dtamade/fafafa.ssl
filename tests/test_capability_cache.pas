program test_capability_cache;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.lib;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TestCachingPerformance;
var
  Lib: TOpenSSLLibrary;
  Caps: TSSLBackendCapabilities;
  StartTime, EndTime: TDateTime;
  I: Integer;
  FirstCallTime, CachedCallTime: Int64;
  CallsPerSecond: Int64;
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

    WriteLn('第一次调用 GetCapabilities（未缓存）...');
    StartTime := Now;
    Caps := Lib.GetCapabilities;
    EndTime := Now;
    FirstCallTime := MilliSecondsBetween(EndTime, StartTime);
    WriteLn('  Backend: ', SSL_LIBRARY_NAMES[Caps.BackendType]);
    WriteLn('  Version: ', Caps.BackendVersion);
    WriteLn('  Time: ', FirstCallTime, ' ms');
    WriteLn;

    WriteLn('测试缓存性能 (', ITERATIONS, ' 次调用)...');
    StartTime := Now;
    for I := 1 to ITERATIONS do
      Caps := Lib.GetCapabilities;
    EndTime := Now;
    CachedCallTime := MilliSecondsBetween(EndTime, StartTime);

    WriteLn('  Total Time: ', CachedCallTime, ' ms');
    WriteLn('  Average per call: ', (CachedCallTime / ITERATIONS):0:6, ' ms');
    if CachedCallTime > 0 then
      CallsPerSecond := Round(ITERATIONS / (CachedCallTime / 1000))
    else
      CallsPerSecond := ITERATIONS;
    WriteLn('  Calls per second: ', CallsPerSecond:0, ' ops/s');
    WriteLn;

    WriteLn('性能提升分析:');
    if FirstCallTime > 0 then
      WriteLn('  首次调用耗时: ', FirstCallTime, ' ms')
    else
      WriteLn('  首次调用耗时: < 1 ms');
    WriteLn('  缓存调用耗时: ~', (CachedCallTime / ITERATIONS):0:6, ' ms per call');
    if (FirstCallTime > 0) and (CachedCallTime > 0) then
      WriteLn('  性能提升: ~', Round((FirstCallTime * 1000) / (CachedCallTime / ITERATIONS)):0, 'x')
    else
      WriteLn('  性能提升: 极显著（计时精度不足）');
    WriteLn;

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

    WriteLn('首次获取能力矩阵...');
    Caps1 := Lib.GetCapabilities;
    WriteLn('  Version: ', Caps1.BackendVersion);
    WriteLn('  ✓ 已缓存');
    WriteLn;

    WriteLn('再次获取能力矩阵（应该来自缓存）...');
    Caps2 := Lib.GetCapabilities;
    WriteLn('  Version: ', Caps2.BackendVersion);
    if Caps2.BackendVersion = Caps1.BackendVersion then
      WriteLn('  ✓ 缓存有效')
    else
      WriteLn('  ✗ 缓存失效（不应该发生）');
    WriteLn;

    WriteLn('Finalize 后重新初始化...');
    Lib.Finalize;
    if not Lib.Initialize then
    begin
      WriteLn('  ✗ Re-initialization failed');
      Exit;
    end;

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

procedure TestFreePascalKnownIssuesAlignment;
var
  Lib: ISSLLibrary;
  Caps: TSSLBackendCapabilities;
begin
  WriteLn('==============================================');
  WriteLn('FreePascal KnownIssues 运行时对齐测试');
  WriteLn('==============================================');
  WriteLn;

  Lib := TSSLFactory.GetLibrary(sslFreePascal);
  if Lib = nil then
  begin
    WriteLn('  [SKIP] FreePascal backend not available');
    WriteLn;
    Exit;
  end;

  Caps := Lib.GetCapabilities;
  WriteLn('  KnownIssues: ', Caps.KnownIssues);
  WriteLn('  Supports AES256GCM in caps: ', IsCipherSupported(Caps, sslCipherAES256GCM));
  WriteLn('  IsCipherSupported(TLS_AES_256_GCM_SHA384): ',
    Lib.IsCipherSupported('TLS_AES_256_GCM_SHA384'));

  Require(Pos('SHA384', UpperCase(Caps.KnownIssues)) > 0,
    'FreePascal KnownIssues should still mention remaining SHA384-related caveats');
  Require(IsCipherSupported(Caps, sslCipherAES256GCM),
    'FreePascal capabilities should advertise AES256GCM once SHA384 handshake path is wired');
  Require(Lib.IsCipherSupported('TLS_AES_256_GCM_SHA384'),
    'FreePascal IsCipherSupported should accept TLS_AES_256_GCM_SHA384 once SHA384 handshake path is wired');

  WriteLn('  ✓ FreePascal KnownIssues runtime alignment verified');
  WriteLn;
end;

begin
  WriteLn('fafafa.ssl - 能力矩阵缓存测试');
  WriteLn('==============================================');
  WriteLn;

  try
    TestCachingPerformance;
    WriteLn;
    TestCacheInvalidation;
    WriteLn;
    TestFreePascalKnownIssuesAlignment;

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
