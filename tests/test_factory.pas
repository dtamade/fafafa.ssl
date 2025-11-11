program test_factory;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.types,
  fafafa.ssl.base,
  fafafa.ssl.intf,
  fafafa.ssl.factory;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure Pass(const TestName: string);
begin
  WriteLn('  [PASS] ', TestName);
  Inc(TestsPassed);
end;

procedure Fail(const TestName, Reason: string);
begin
  WriteLn('  [FAIL] ', TestName, ' - ', Reason);
  Inc(TestsFailed);
end;

procedure TestAutoDetection;
var
  LibType: TSSLLibraryType;
begin
  WriteLn('测试 1: 自动检测最佳库');
  try
    LibType := TSSLFactory.DetectBestLibrary;

    if LibType = sslAutoDetect then
      Fail('DetectBestLibrary', '未能检测到任何可用库')
    else
    begin
      WriteLn('    检测到库: ', SSL_LIBRARY_NAMES[LibType]);
      Pass('DetectBestLibrary');
    end;
  except
    on E: Exception do
      Fail('DetectBestLibrary', E.Message);
  end;
end;

procedure TestLibraryCreation;
var
  Lib: ISSLLibrary;
begin
  WriteLn('测试 2: 创建库实例（自动检测）');
  try
    Lib := CreateSSLLibrary;
    if Assigned(Lib) then
    begin
      if Lib.IsInitialized then
        Pass('CreateSSLLibrary with auto-detect')
      else
        Fail('CreateSSLLibrary', '库未初始化');
    end
    else
      Fail('CreateSSLLibrary', '返回 nil');
  except
    on E: Exception do
      Fail('CreateSSLLibrary', E.Message);
  end;
end;

procedure TestWinSSLLibrary;
var
  Lib: ISSLLibrary;
begin
  {$IFDEF WINDOWS}
  WriteLn('测试 3: 创建 WinSSL 库');
  try
    Lib := CreateSSLLibrary(sslWinSSL);
    if Assigned(Lib) then
    begin
      WriteLn('    版本: ', Lib.GetVersionString);
      WriteLn('    类型: ', SSL_LIBRARY_NAMES[Lib.GetLibraryType]);

      if Lib.GetLibraryType = sslWinSSL then
        Pass('WinSSL library creation and type')
      else
        Fail('WinSSL library', '库类型不匹配');
    end
    else
      Fail('WinSSL library', '返回 nil');
  except
    on E: Exception do
      Fail('WinSSL library', E.Message);
  end;
  {$ELSE}
  WriteLn('测试 3: 跳过（非 Windows 平台）');
  {$ENDIF}
end;

procedure TestOpenSSLLibrary;
var
  Lib: ISSLLibrary;
begin
  WriteLn('测试 4: 创建 OpenSSL 库');
  try
    Lib := CreateSSLLibrary(sslOpenSSL);
    if Assigned(Lib) then
    begin
      WriteLn('    版本: ', Lib.GetVersionString);
      WriteLn('    类型: ', SSL_LIBRARY_NAMES[Lib.GetLibraryType]);

      if Lib.GetLibraryType = sslOpenSSL then
        Pass('OpenSSL library creation and type')
      else
        Fail('OpenSSL library', '库类型不匹配');
    end
    else
      Fail('OpenSSL library', '返回 nil');
  except
    on E: Exception do
      Fail('OpenSSL library', E.Message);
  end;
end;

procedure TestContextCreation;
var
  Ctx: ISSLContext;
begin
  WriteLn('测试 5: 创建 SSL 上下文');
  try
    Ctx := CreateSSLContext(sslCtxClient);
    if Assigned(Ctx) then
    begin
      if Ctx.GetContextType = sslCtxClient then
        Pass('CreateSSLContext with client type')
      else
        Fail('CreateSSLContext', '上下文类型不匹配');
    end
    else
      Fail('CreateSSLContext', '返回 nil');
  except
    on E: Exception do
      Fail('CreateSSLContext', E.Message);
  end;
end;

procedure TestLibraryRegistration;
var
  Available: TSSLLibraryTypes;
  LibType: TSSLLibraryType;
begin
  WriteLn('测试 6: 库注册检查');
  try
    Available := TSSLFactory.GetAvailableLibraries;

    if Available = [] then
      Fail('Library registration', '没有可用库')
    else
    begin
      WriteLn('    可用库:');
      for LibType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
      begin
        if LibType in Available then
          WriteLn('      - ', SSL_LIBRARY_NAMES[LibType]);
      end;
      Pass('Library registration check');
    end;
  except
    on E: Exception do
      Fail('Library registration', E.Message);
  end;
end;

procedure TestGetVersionInfo;
var
  VersionInfo: string;
begin
  WriteLn('测试 7: 获取版本信息');
  try
    VersionInfo := TSSLFactory.GetVersionInfo;
    if Length(VersionInfo) > 0 then
    begin
      WriteLn('    版本信息:');
      WriteLn(VersionInfo);
      Pass('GetVersionInfo');
    end
    else
      Fail('GetVersionInfo', '返回空字符串');
  except
    on E: Exception do
      Fail('GetVersionInfo', E.Message);
  end;
end;

procedure TestProtocolSupport;
var
  Lib: ISSLLibrary;
begin
  WriteLn('测试 8: 协议支持检查');
  try
    Lib := CreateSSLLibrary;
    if Assigned(Lib) then
    begin
      WriteLn('    TLS 1.2: ', Lib.IsProtocolSupported(sslProtocolTLS12));
      WriteLn('    TLS 1.3: ', Lib.IsProtocolSupported(sslProtocolTLS13));
      Pass('Protocol support check');
    end
    else
      Fail('Protocol support', '库未创建');
  except
    on E: Exception do
      Fail('Protocol support', E.Message);
  end;
end;

procedure TestFeatureSupport;
var
  Lib: ISSLLibrary;
begin
  WriteLn('测试 9: 功能支持检查');
  try
    Lib := CreateSSLLibrary;
    if Assigned(Lib) then
    begin
      WriteLn('    SNI: ', Lib.IsFeatureSupported('SNI'));
      WriteLn('    ALPN: ', Lib.IsFeatureSupported('ALPN'));
      WriteLn('    Session Cache: ', Lib.IsFeatureSupported('session_cache'));
      Pass('Feature support check');
    end
    else
      Fail('Feature support', '库未创建');
  except
    on E: Exception do
      Fail('Feature support', E.Message);
  end;
end;

procedure TestSystemInfo;
var
  SystemInfo: string;
begin
  WriteLn('测试 10: 获取系统信息');
  try
    SystemInfo := TSSLFactory.GetSystemInfo;
    if Length(SystemInfo) > 0 then
    begin
      WriteLn('    系统信息:');
      WriteLn(SystemInfo);
      Pass('GetSystemInfo');
    end
    else
      Fail('GetSystemInfo', '返回空字符串');
  except
    on E: Exception do
      Fail('GetSystemInfo', E.Message);
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('  fafafa.ssl Factory 单元测试');
  WriteLn('========================================');
  WriteLn('');

  TestAutoDetection;
  TestLibraryCreation;
  TestWinSSLLibrary;
  TestOpenSSLLibrary;
  TestContextCreation;
  TestLibraryRegistration;
  TestGetVersionInfo;
  TestProtocolSupport;
  TestFeatureSupport;
  TestSystemInfo;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('  测试结果汇总');
  WriteLn('========================================');
  WriteLn('通过: ', TestsPassed);
  WriteLn('失败: ', TestsFailed);
  WriteLn('总计: ', TestsPassed + TestsFailed);

  if TestsFailed = 0 then
  begin
    WriteLn('');
    WriteLn('所有测试通过！✓');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('');
    WriteLn('有测试失败！✗');
    ExitCode := 1;
  end;
end.
