{**
 * 测试: WinSSL 独立运行（不依赖 OpenSSL）
 *
 * 此测试验证修复后的 fafafa.ssl 库可以在没有 OpenSSL DLL 的情况下
 * 使用 WinSSL 后端正常工作。
 *
 * 修复前问题:
 * - fafafa.ssl.factory 无条件导入 fafafa.ssl.openssl.api.rand
 * - 导致程序启动时尝试加载 OpenSSL DLL
 * - 如果 OpenSSL 未安装，程序崩溃 (exit code -1073741511)
 *
 * 修复后:
 * - factory 使用平台无关的 fafafa.ssl.random 模块
 * - WinSSL 用户不再需要安装 OpenSSL
 *}

program test_winssl_standalone;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.autoregister;  // 自动注册 WinSSL 后端

var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LRandomBytes: TBytes;
  LAvailable: TSSLLibraryTypes;
  LLibType: TSSLLibraryType;
  LTestsPassed: Integer;
  LTestsFailed: Integer;

procedure TestPassed(const ATestName: string);
begin
  WriteLn('[PASS] ', ATestName);
  Inc(LTestsPassed);
end;

procedure TestFailed(const ATestName, AReason: string);
begin
  WriteLn('[FAIL] ', ATestName, ' - ', AReason);
  Inc(LTestsFailed);
end;

begin
  LTestsPassed := 0;
  LTestsFailed := 0;

  WriteLn('=== WinSSL Standalone Test ===');
  WriteLn('Testing that WinSSL works without OpenSSL DLLs');
  WriteLn('');

  // 测试 1: 程序启动成功（如果能到这里，说明没有崩溃）
  TestPassed('Program startup (no crash on DLL load)');

  // 测试 2: 检查 WinSSL 是否可用
  try
    if TSSLFactory.IsLibraryAvailable(sslWinSSL) then
      TestPassed('WinSSL backend is available')
    else
      TestFailed('WinSSL backend availability', 'WinSSL not available');
  except
    on E: Exception do
      TestFailed('WinSSL backend availability', E.Message);
  end;

  // 测试 3: 获取可用库列表
  try
    LAvailable := TSSLFactory.GetAvailableLibraries;
    WriteLn('  Available libraries:');
    for LLibType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
    begin
      if LLibType in LAvailable then
        WriteLn('    - ', TSSLFactory.GetLibraryDescription(LLibType));
    end;
    if sslWinSSL in LAvailable then
      TestPassed('Get available libraries')
    else
      TestFailed('Get available libraries', 'WinSSL not in list');
  except
    on E: Exception do
      TestFailed('Get available libraries', E.Message);
  end;

  // 测试 4: 创建 WinSSL 库实例
  try
    LLib := TSSLFactory.GetLibrary(sslWinSSL);
    if Assigned(LLib) then
    begin
      WriteLn('  Library version: ', LLib.GetVersionString);
      TestPassed('Create WinSSL library instance')
    end
    else
      TestFailed('Create WinSSL library instance', 'Returned nil');
  except
    on E: Exception do
      TestFailed('Create WinSSL library instance', E.Message);
  end;

  // 测试 5: 创建 SSL Context
  try
    LCtx := TSSLFactory.CreateContext(sslCtxClient, sslWinSSL);
    if Assigned(LCtx) then
      TestPassed('Create SSL context')
    else
      TestFailed('Create SSL context', 'Returned nil');
  except
    on E: Exception do
      TestFailed('Create SSL context', E.Message);
  end;

  // 测试 6: 生成安全随机数（使用平台无关实现）
  try
    LRandomBytes := TSSLHelper.GenerateRandomBytes(32);
    if Length(LRandomBytes) = 32 then
    begin
      WriteLn('  Generated 32 random bytes successfully');
      TestPassed('Generate secure random bytes')
    end
    else
      TestFailed('Generate secure random bytes', 'Wrong length');
  except
    on E: Exception do
      TestFailed('Generate secure random bytes', E.Message);
  end;

  // 测试 7: 获取系统信息
  try
    WriteLn('');
    WriteLn('System Info:');
    WriteLn(TSSLFactory.GetSystemInfo);
    TestPassed('Get system info');
  except
    on E: Exception do
      TestFailed('Get system info', E.Message);
  end;

  // 总结
  WriteLn('');
  WriteLn('=== Test Summary ===');
  WriteLn('Passed: ', LTestsPassed);
  WriteLn('Failed: ', LTestsFailed);
  WriteLn('');

  if LTestsFailed = 0 then
  begin
    WriteLn('SUCCESS: WinSSL works independently without OpenSSL!');
    WriteLn('The critical design flaw has been fixed.');
    Halt(0);
  end
  else
  begin
    WriteLn('FAILURE: Some tests failed.');
    Halt(1);
  end;
end.
