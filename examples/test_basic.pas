{
  test_basic - fafafa.ssl 基础功能测试程序
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    测试 fafafa.ssl 库的基本功能，包括：
    1. 库检测和加载
    2. 上下文创建
    3. 证书处理
    4. 基本连接（框架）
}

program test_basic;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes, 
  fafafa.ssl;  // 主库单元

procedure PrintSeparator;
begin
  WriteLn('========================================');
end;

procedure TestLibraryDetection;
var
  LAvailableLibs: TSSLLibraryTypes;
  LLibType: TSSLLibraryType;
begin
  PrintSeparator;
  WriteLn('测试 1: SSL 库检测');
  PrintSeparator;
  
  WriteLn('检查 SSL 支持...');
  if not CheckSSLSupport then
  begin
    WriteLn('❌ 错误: 没有可用的 SSL 库！');
    Exit;
  end;
  
  WriteLn('✅ SSL 支持可用');
  WriteLn;
  
  // 获取可用库列表
  LAvailableLibs := TSSLFactory.AvailableLibraries;
  
  WriteLn('可用的 SSL 库:');
  for LLibType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if (LLibType <> sslAutoDetect) and (LLibType in LAvailableLibs) then
    begin
      WriteLn('  ✓ ', LibraryTypeToString(LLibType));
      
      // 尝试获取版本信息
      try
        var LLib := TSSLFactory.GetLibraryInstance(LLibType);
        if Assigned(LLib) then
        begin
          WriteLn('    版本: ', LLib.GetVersionString);
          WriteLn('    编译标志: ', LLib.GetCompileFlags);
        end;
      except
        on E: Exception do
          WriteLn('    获取版本信息失败: ', E.Message);
      end;
    end
    else if LLibType <> sslAutoDetect then
    begin
      WriteLn('  ✗ ', LibraryTypeToString(LLibType), ' (不可用)');
    end;
  end;
  
  WriteLn;
  WriteLn('默认库: ', LibraryTypeToString(TSSLFactory.DefaultLibrary));
end;

procedure TestContextCreation;
var
  LContext: ISSLContext;
  LConfig: TSSLConfig;
begin
  PrintSeparator;
  WriteLn('测试 2: 上下文创建');
  PrintSeparator;
  
  try
    // 测试简单创建
    WriteLn('创建客户端上下文...');
    LContext := CreateSSLContext(sslCtxClient);
    if Assigned(LContext) then
    begin
      WriteLn('✅ 客户端上下文创建成功');
      WriteLn('  上下文类型: 客户端');
      WriteLn('  协议版本: ', SSL_PROTOCOL_NAMES[sslProtocolTLS12]);
    end;
    
    // 测试配置
    WriteLn;
    WriteLn('配置 SSL 参数...');
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.SetVerifyMode([sslVerifyPeer]);
    LContext.SetServerName('www.example.com');
    WriteLn('✅ SSL 参数配置成功');
    
    // 测试从配置创建
    WriteLn;
    WriteLn('使用配置结构创建上下文...');
    LConfig := CreateDefaultConfig(sslCtxServer);
    LConfig.LibraryType := TSSLFactory.DefaultLibrary;
    LConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    
    LContext := TSSLFactory.CreateContext(LConfig);
    if Assigned(LContext) then
    begin
      WriteLn('✅ 服务端上下文创建成功');
      WriteLn('  上下文类型: 服务端');
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误: ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

procedure TestCertificateHandling;
var
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
  LInfo: TSSLCertificateInfo;
begin
  PrintSeparator;
  WriteLn('测试 3: 证书处理');
  PrintSeparator;
  
  try
    // 创建证书对象
    WriteLn('创建证书对象...');
    LCert := CreateSSLCertificate;
    if Assigned(LCert) then
    begin
      WriteLn('✅ 证书对象创建成功');
      
      // 测试基本属性
      if LCert.IsExpired then
        WriteLn('  证书状态: 已过期')
      else
        WriteLn('  证书状态: 有效');
    end;
    
    // 创建证书存储
    WriteLn;
    WriteLn('创建证书存储...');
    LStore := TSSLFactory.CreateCertificateStore;
    if Assigned(LStore) then
    begin
      WriteLn('✅ 证书存储创建成功');
      WriteLn('  证书数量: ', LStore.GetCount);
      
      // 尝试加载系统证书
      {$IFDEF WINDOWS}
      WriteLn;
      WriteLn('尝试加载 Windows 系统证书存储...');
      if LStore.LoadSystemStore then
      begin
        WriteLn('✅ 系统证书加载成功');
        WriteLn('  证书数量: ', LStore.GetCount);
      end
      else
      begin
        WriteLn('⚠ 系统证书加载失败（可能未实现）');
      end;
      {$ENDIF}
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误: ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

procedure TestErrorHandling;
begin
  PrintSeparator;
  WriteLn('测试 4: 错误处理');
  PrintSeparator;
  
  try
    // 测试无效库类型
    WriteLn('尝试使用无效的库类型...');
    var LContext := TSSLFactory.CreateContext(sslCtxClient, TSSLLibraryType(99));
    WriteLn('⚠ 应该抛出异常但没有！');
  except
    on E: ESSLLibraryException do
    begin
      WriteLn('✅ 正确捕获库异常:');
      WriteLn('  错误消息: ', E.Message);
      WriteLn('  错误代码: ', SSLErrorToString(E.ErrorCode));
      WriteLn('  库类型: ', LibraryTypeToString(E.LibraryType));
    end;
    on E: ESSLException do
    begin
      WriteLn('✅ 正确捕获 SSL 异常:');
      WriteLn('  错误消息: ', E.Message);
      WriteLn('  错误代码: ', SSLErrorToString(E.ErrorCode));
    end;
    on E: Exception do
    begin
      WriteLn('⚠ 捕获未预期的异常: ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

procedure TestSystemInfo;
begin
  PrintSeparator;
  WriteLn('测试 5: 系统信息');
  PrintSeparator;
  
  WriteLn(GetSSLSupportInfo);
  
  // 测试特定功能支持
  WriteLn;
  WriteLn('功能支持检查:');
  
  var LLib := TSSLFactory.GetLibraryInstance;
  if Assigned(LLib) then
  begin
    WriteLn('  SNI (Server Name Indication): ',
      BoolToStr(LLib.IsFeatureSupported(sslFeatSNI), '支持', '不支持'));
    WriteLn('  ALPN (Application Layer Protocol Negotiation): ',
      BoolToStr(LLib.IsFeatureSupported(sslFeatALPN), '支持', '不支持'));
    WriteLn('  Session Resumption: ',
      BoolToStr(LLib.IsFeatureSupported(sslFeatSessionResumption), '支持', '不支持'));
    
    // 协议版本支持
    WriteLn;
    WriteLn('协议版本支持:');
    WriteLn('  TLS 1.0: ', BoolToStr(LLib.IsProtocolSupported(sslProtocolTLS10), '✓', '✗'));
    WriteLn('  TLS 1.1: ', BoolToStr(LLib.IsProtocolSupported(sslProtocolTLS11), '✓', '✗'));
    WriteLn('  TLS 1.2: ', BoolToStr(LLib.IsProtocolSupported(sslProtocolTLS12), '✓', '✗'));
    WriteLn('  TLS 1.3: ', BoolToStr(LLib.IsProtocolSupported(sslProtocolTLS13), '✓', '✗'));
  end;
end;

procedure TestMemoryManagement;
var
  LContext: ISSLContext;
  LCert: ISSLCertificate;
  LCount: Integer;
begin
  PrintSeparator;
  WriteLn('测试 6: 内存管理');
  PrintSeparator;
  
  WriteLn('创建和释放多个对象...');
  
  try
    for LCount := 1 to 10 do
    begin
      LContext := CreateSSLContext(sslCtxClient);
      LCert := CreateSSLCertificate;
      // 接口会自动释放
    end;
    
    WriteLn('✅ 创建并释放 10 个上下文和证书对象');
    
    // 测试库的释放和重新初始化
    WriteLn;
    WriteLn('测试库的释放和重新初始化...');
    TSSLFactory.ReleaseAllLibraries;
    WriteLn('  已释放所有库');
    
    // 重新创建
    LContext := CreateSSLContext(sslCtxClient);
    if Assigned(LContext) then
      WriteLn('✅ 库自动重新初始化成功');
      
  except
    on E: Exception do
      WriteLn('❌ 错误: ', E.Message);
  end;
end;

procedure RunAllTests;
begin
  WriteLn;
  WriteLn('================================');
  WriteLn('  fafafa.ssl 基础功能测试');
  WriteLn('================================');
  WriteLn;
  WriteLn('时间: ', DateTimeToStr(Now));
  WriteLn('平台: ', {$IFDEF WINDOWS}'Windows'{$ELSE}'Unix/Linux'{$ENDIF});
  WriteLn;
  
  // 运行所有测试
  TestLibraryDetection;
  WriteLn;
  TestContextCreation;
  WriteLn;
  TestCertificateHandling;
  WriteLn;
  TestErrorHandling;
  WriteLn;
  TestSystemInfo;
  WriteLn;
  TestMemoryManagement;
  
  PrintSeparator;
  WriteLn('测试完成！');
  PrintSeparator;
end;

begin
  try
    RunAllTests;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('致命错误: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn;
  WriteLn('按 Enter 键退出...');
  ReadLn;
end.