program example_factory_usage;

{$mode objfpc}{$H+}

{
  fafafa.ssl Factory 使用示例

  演示如何使用 Factory API 进行 SSL/TLS 编程：
  1. 自动检测和使用最佳库
  2. 创建 SSL 上下文
  3. 配置验证选项
  4. 显示库和系统信息
}

uses
  SysUtils, Classes, StrUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory;

procedure PrintSeparator(const Title: string = '');
begin
  WriteLn('');
  WriteLn('========================================');
  if Title <> '' then
    WriteLn('  ', Title);
  WriteLn('========================================');
end;

procedure ShowSystemInformation;
var
  Info: string;
begin
  PrintSeparator('系统信息');
  Info := TSSLFactory.GetSystemInfo;
  WriteLn(Info);
end;

procedure ShowAvailableLibraries;
var
  Available: TSSLLibraryTypes;
  LibType: TSSLLibraryType;
  Lib: ISSLLibrary;
begin
  PrintSeparator('可用的 SSL 库');

  Available := TSSLFactory.GetAvailableLibraries;

  if Available = [] then
  begin
    WriteLn('  没有可用的 SSL 库！');
    Exit;
  end;

  for LibType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if LibType in Available then
    begin
      try
        Lib := CreateSSLLibrary(LibType);
        WriteLn('  [', SSL_LIBRARY_NAMES[LibType], ']');
        WriteLn('    版本: ', Lib.GetVersionString);
        WriteLn('    编译标志: ', Lib.GetCompileFlags);
        WriteLn('    支持 TLS 1.2: ', Lib.IsProtocolSupported(sslProtocolTLS12));
        WriteLn('    支持 TLS 1.3: ', Lib.IsProtocolSupported(sslProtocolTLS13));
        WriteLn('    支持 SNI: ', Lib.IsFeatureSupported(sslFeatSNI));
        WriteLn('    支持 ALPN: ', Lib.IsFeatureSupported(sslFeatALPN));
      except
        on E: Exception do
          WriteLn('    错误: ', E.Message);
      end;
    end;
  end;
end;

procedure DemoAutoDetection;
var
  Lib: ISSLLibrary;
  BestLib: TSSLLibraryType;
begin
  PrintSeparator('自动检测演示');

  WriteLn('让 Factory 自动选择最佳库...');
  BestLib := TSSLFactory.DetectBestLibrary;
  WriteLn('检测到: ', SSL_LIBRARY_NAMES[BestLib]);

  WriteLn('');
  WriteLn('使用自动检测创建库实例...');
  Lib := CreateSSLLibrary;  // 默认参数 sslAutoDetect

  if Assigned(Lib) then
  begin
    WriteLn('成功！');
    WriteLn('  实际使用: ', SSL_LIBRARY_NAMES[Lib.GetLibraryType]);
    WriteLn('  版本: ', Lib.GetVersionString);
  end
  else
    WriteLn('失败：返回 nil');
end;

procedure DemoClientContext;
var
  Ctx: ISSLContext;
begin
  PrintSeparator('创建客户端上下文');

  WriteLn('创建客户端上下文（自动检测库）...');
  Ctx := CreateSSLContext(sslCtxClient);

  if Assigned(Ctx) then
  begin
    WriteLn('成功！');
    WriteLn('  上下文类型: ',
      IfThen(Ctx.GetContextType = sslCtxClient, '客户端', '服务端'));

    WriteLn('');
    WriteLn('配置上下文选项...');

    // 设置协议版本
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    WriteLn('  协议版本: TLS 1.2 + 1.3');

    // 设置验证模式
    Ctx.SetVerifyMode([sslVerifyPeer]);
    WriteLn('  验证模式: 验证对端证书');

    // 设置 SNI
    Ctx.SetServerName('www.example.com');
    WriteLn('  服务器名称: ', Ctx.GetServerName);

    // 设置 ALPN
    Ctx.SetALPNProtocols('http/1.1,http/2');
    WriteLn('  ALPN 协议: ', Ctx.GetALPNProtocols);

    WriteLn('');
    WriteLn('上下文配置完成，已准备好创建连接！');
  end
  else
    WriteLn('失败：返回 nil');
end;

procedure DemoExplicitLibrarySelection;
var
  WinSSLCtx, OpenSSLCtx: ISSLContext;
begin
  PrintSeparator('显式库选择');

  {$IFDEF WINDOWS}
  WriteLn('在 Windows 上，我们可以选择使用哪个库：');
  WriteLn('');

  WriteLn('1. 使用 Windows Schannel (零依赖):');
  try
    WinSSLCtx := TSSLFactory.CreateContext(sslCtxClient, sslWinSSL);
    if Assigned(WinSSLCtx) then
      WriteLn('   ✓ WinSSL 上下文创建成功')
    else
      WriteLn('   ✗ WinSSL 上下文创建失败');
  except
    on E: Exception do
      WriteLn('   ✗ 错误: ', E.Message);
  end;

  WriteLn('');
  WriteLn('2. 使用 OpenSSL (更多功能):');
  try
    OpenSSLCtx := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
    if Assigned(OpenSSLCtx) then
      WriteLn('   ✓ OpenSSL 上下文创建成功')
    else
      WriteLn('   ✗ OpenSSL 上下文创建失败');
  except
    on E: Exception do
      WriteLn('   ✗ 错误: ', E.Message);
  end;
  {$ELSE}
  WriteLn('在非 Windows 平台上，主要使用 OpenSSL:');
  try
    OpenSSLCtx := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
    if Assigned(OpenSSLCtx) then
      WriteLn('   ✓ OpenSSL 上下文创建成功')
    else
      WriteLn('   ✗ OpenSSL 上下文创建失败');
  except
    on E: Exception do
      WriteLn('   ✗ 错误: ', E.Message);
  end;
  {$ENDIF}
end;

procedure DemoConfiguration;
var
  Config: TSSLConfig;
  Ctx: ISSLContext;
begin
  PrintSeparator('使用配置对象');

  WriteLn('创建配置对象...');

  // 初始化配置
  FillChar(Config, SizeOf(Config), 0);

  Config.LibraryType := sslAutoDetect;
  Config.ContextType := sslCtxClient;
  Config.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  Config.PreferredVersion := sslProtocolTLS13;
  Config.VerifyMode := [sslVerifyPeer];
  Config.VerifyDepth := 10;
  Config.ServerName := 'www.google.com';
  Config.ALPNProtocols := 'h2,http/1.1';
  Config.BufferSize := 16384;
  Config.HandshakeTimeout := 30000;

  WriteLn('配置项:');
  WriteLn('  库类型: ', SSL_LIBRARY_NAMES[Config.LibraryType]);
  WriteLn('  上下文类型: 客户端');
  WriteLn('  协议版本: TLS 1.2 + 1.3');
  WriteLn('  验证深度: ', Config.VerifyDepth);
  WriteLn('  服务器名称: ', Config.ServerName);
  WriteLn('  ALPN: ', Config.ALPNProtocols);

  WriteLn('');
  WriteLn('使用配置创建上下文...');

  try
    Ctx := TSSLFactory.CreateContext(Config);
    if Assigned(Ctx) then
    begin
      WriteLn('成功！');
      WriteLn('  服务器名称已设置: ', Ctx.GetServerName);
      WriteLn('  ALPN 已设置: ', Ctx.GetALPNProtocols);
    end
    else
      WriteLn('失败：返回 nil');
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end;

procedure ShowUsageExample;
begin
  PrintSeparator('快速入门代码示例');
  WriteLn('');
  WriteLn('// 最简单的用法：自动检测');
  WriteLn('var');
  WriteLn('  Ctx: ISSLContext;');
  WriteLn('begin');
  WriteLn('  Ctx := CreateSSLContext(sslCtxClient);');
  WriteLn('  Ctx.SetServerName(''www.example.com'');');
  WriteLn('  // ... 使用 Ctx 创建连接');
  WriteLn('end;');
  WriteLn('');
  WriteLn('// 显式选择库');
  WriteLn('var');
  WriteLn('  Lib: ISSLLibrary;');
  WriteLn('begin');
  WriteLn('  {$IFDEF WINDOWS}');
  WriteLn('  Lib := CreateSSLLibrary(sslWinSSL);  // 使用 Windows 原生');
  WriteLn('  {$ELSE}');
  WriteLn('  Lib := CreateSSLLibrary(sslOpenSSL); // 使用 OpenSSL');
  WriteLn('  {$ENDIF}');
  WriteLn('end;');
end;

begin
  WriteLn('');
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║  fafafa.ssl Factory 使用示例          ║');
  WriteLn('║  多后端 SSL/TLS 抽象层                ║');
  WriteLn('╚════════════════════════════════════════╝');

  try
    ShowSystemInformation;
    ShowAvailableLibraries;
    DemoAutoDetection;
    DemoClientContext;
    DemoExplicitLibrarySelection;
    DemoConfiguration;
    ShowUsageExample;

    PrintSeparator('演示完成');
    WriteLn('');
    WriteLn('所有示例运行成功！');
    WriteLn('');
    WriteLn('提示：');
    WriteLn('  - 在 Windows 上默认使用 WinSSL（零依赖）');
    WriteLn('  - 在其他平台上使用 OpenSSL');
    WriteLn('  - 可以显式选择使用哪个库');
    WriteLn('  - 所有接口统一，切换库无需修改代码');
    WriteLn('');

  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('错误: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
