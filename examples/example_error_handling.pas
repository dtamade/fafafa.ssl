program example_error_handling;

{$mode objfpc}{$H+}

{**
 * fafafa.ssl 错误处理模式综合示例
 *
 * 本示例展示三种错误处理模式的实际应用：
 * 1. Result 模式 - 函数式编程风格
 * 2. Try* 模式 - 高性能、不抛异常
 * 3. 异常模式 - 传统 Pascal/Delphi 风格
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.cert.utils,
  fafafa.ssl.context.builder,
  fafafa.ssl.exceptions;

{ ==================== 辅助类 ==================== }

type
  { 数据验证器 - 用于 Result.IsOkAnd }
  TDataValidator = class
  public
    function IsNonEmpty(const AData: TBytes): Boolean;
    function IsValidHashSize(const AData: TBytes): Boolean;
  end;

  { 数据记录器 - 用于 Result.Inspect }
  TDataLogger = class
  private
    FLogCount: Integer;
  public
    constructor Create;
    procedure LogHashData(const AData: TBytes);
    procedure LogEncryptData(const AData: TBytes);
    function GetLogCount: Integer;
  end;

{ TDataValidator }

function TDataValidator.IsNonEmpty(const AData: TBytes): Boolean;
begin
  Result := Length(AData) > 0;
end;

function TDataValidator.IsValidHashSize(const AData: TBytes): Boolean;
begin
  // SHA-256 应该是 32 字节
  Result := Length(AData) = 32;
end;

{ TDataLogger }

constructor TDataLogger.Create;
begin
  inherited;
  FLogCount := 0;
end;

procedure TDataLogger.LogHashData(const AData: TBytes);
begin
  Inc(FLogCount);
  WriteLn('[LOG] Hash data: ', Length(AData), ' bytes');
end;

procedure TDataLogger.LogEncryptData(const AData: TBytes);
begin
  Inc(FLogCount);
  WriteLn('[LOG] Encrypted data: ', Length(AData), ' bytes');
end;

function TDataLogger.GetLogCount: Integer;
begin
  Result := FLogCount;
end;

{ ==================== 示例场景 ==================== }

{ 场景 1: Result 模式 - 密码学操作链 }
procedure Scenario1_ResultPattern;
var
  LInput: TBytes;
  LHash: TBytes;
  LSuccess: Boolean;
  LValidator: TDataValidator;
  LLogger: TDataLogger;
  LResult: TSSLOperationResult;
  LTempContext: ISSLContext;
begin
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║ 场景 1: Result 模式 - 使用 Try* 方法与 Result          ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  WriteLn;

  LValidator := TDataValidator.Create;
  LLogger := TDataLogger.Create;
  try
    // 准备测试数据
    SetLength(LInput, 100);
    FillByte(LInput[0], 100, $42);

    WriteLn('1. 使用 Try* 方法');
    WriteLn('----------------');

    // 计算哈希
    LSuccess := TCryptoUtils.TrySHA256(LInput, LHash);

    // 检查结果
    if LSuccess then
      WriteLn('✓ SHA-256 succeeded (', Length(LHash), ' bytes)')
    else
      WriteLn('✗ SHA-256 failed');

    WriteLn;
    WriteLn('2. 条件验证');
    WriteLn('-----------');

    // 检查哈希大小
    if LSuccess and LValidator.IsValidHashSize(LHash) then
      WriteLn('✓ Hash is valid and has correct size (32 bytes)')
    else
      WriteLn('✗ Hash is invalid or wrong size');

    WriteLn;
    WriteLn('3. 记录日志');
    WriteLn('----------');

    // 记录数据
    if LSuccess then
    begin
      LLogger.LogHashData(LHash);
      WriteLn('✓ Data logged');
      WriteLn('  Log count: ', LLogger.GetLogCount);
    end;

    WriteLn;
    WriteLn('4. Builder Try* 返回 Result');
    WriteLn('-------------------------');

    // context.builder 的 Try* 方法返回 TSSLOperationResult
    LResult := TSSLContextBuilder.Create.TryBuildClient(LTempContext);
    if LResult.IsOk then
      WriteLn('✓ Context creation result: OK')
    else
      WriteLn('✗ Context creation failed: ', LResult.ErrorMessage);

    WriteLn;

  finally
    LValidator.Free;
    LLogger.Free;
  end;
end;

{ 场景 2: Try* 模式 - 批量证书处理 }
procedure Scenario2_TryPattern;
var
  LCertFiles: TStringList;
  I: Integer;
  LCert, LKey: string;
  LInfo: TCertInfo;
  LSuccessCount: Integer;
  LTempFile: string;
begin
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║ 场景 2: Try* 模式 - 批量证书处理                         ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  WriteLn;

  LCertFiles := TStringList.Create;
  try
    WriteLn('1. 生成测试证书');
    WriteLn('---------------');

    // 生成几个测试证书
    for I := 1 to 3 do
    begin
      if TCertificateUtils.TryGenerateSelfSignedSimple(
        Format('test%d.local', [I]),
        'Test Org',
        30,
        LCert,
        LKey
      ) then
      begin
        LTempFile := Format('test_cert_%d.pem', [I]);
        if TCertificateUtils.SaveToFile(LTempFile, LCert) then
        begin
          LCertFiles.Add(LTempFile);
          WriteLn(Format('✓ Generated certificate #%d: %s', [I, LTempFile]));
        end;
      end
      else
        WriteLn(Format('✗ Failed to generate certificate #%d', [I]));
    end;

    WriteLn;
    WriteLn('2. 批量处理证书（Try* 模式）');
    WriteLn('--------------------------');

    LSuccessCount := 0;
    for I := 0 to LCertFiles.Count - 1 do
    begin
      WriteLn(Format('Processing: %s', [LCertFiles[I]]));

      // 加载证书 - 失败不影响其他证书
      if not TCertificateUtils.TryLoadFromFile(LCertFiles[I], LCert) then
      begin
        WriteLn('  ✗ Failed to load');
        Continue;
      end;

      // 提取信息 - Try* 方法不抛异常
      if TCertificateUtils.TryGetInfo(LCert, LInfo) then
      begin
        try
          WriteLn('  ✓ Subject: ', LInfo.Subject);
          WriteLn('  ✓ Issuer: ', LInfo.Issuer);
          WriteLn('  ✓ Valid: ', DateTimeToStr(LInfo.NotBefore),
                  ' to ', DateTimeToStr(LInfo.NotAfter));
          Inc(LSuccessCount);
        finally
          LInfo.SubjectAltNames.Free;
        end;
      end
      else
        WriteLn('  ✗ Failed to parse certificate');

      WriteLn;
    end;

    WriteLn(Format('✓ Successfully processed %d/%d certificates',
      [LSuccessCount, LCertFiles.Count]));

    WriteLn;
    WriteLn('3. 清理临时文件');
    WriteLn('---------------');

    // 清理
    for I := 0 to LCertFiles.Count - 1 do
    begin
      if FileExists(LCertFiles[I]) then
      begin
        DeleteFile(LCertFiles[I]);
        WriteLn('✓ Deleted: ', LCertFiles[I]);
      end;
    end;

    WriteLn;

  finally
    LCertFiles.Free;
  end;
end;

{ 场景 3: 异常模式 - 应用配置加载 }
procedure Scenario3_ExceptionPattern;
var
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LCert, LKey: string;
begin
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║ 场景 3: 异常模式 - 应用配置加载                          ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  WriteLn;

  WriteLn('1. 传统异常处理');
  WriteLn('---------------');

  try
    // 尝试加载不存在的证书 - 会抛出异常
    WriteLn('Attempting to load nonexistent certificate...');

    // 这里我们先生成一个测试证书
    WriteLn('Generating test certificate first...');
    if not TCertificateUtils.TryGenerateSelfSignedSimple(
      'secure.local',
      'Secure Corp',
      365,
      LCert,
      LKey
    ) then
      raise ESSLCertError.Create('Failed to generate test certificate');

    WriteLn('✓ Test certificate generated');

    // 现在使用异常模式构建服务器上下文
    WriteLn('Building server context (exception mode)...');
    LContext := TSSLContextBuilder.Create
      .WithCertificatePEM(LCert)
      .WithPrivateKeyPEM(LKey)
      .WithSafeDefaults
      .BuildServer;  // 可能抛出异常

    WriteLn('✓ Server context created successfully');

  except
    // 捕获特定异常
    on E: ESSLCertError do
    begin
      WriteLn('✗ Certificate Error: ', E.Message);
      WriteLn('  Reason: Invalid or missing certificate');
    end;

    on E: ESSLConfigurationException do
    begin
      WriteLn('✗ Configuration Error: ', E.Message);
      WriteLn('  Reason: Invalid SSL configuration');
    end;

    on E: ESSLException do
    begin
      WriteLn('✗ SSL Error: ', E.Message);
      WriteLn('  Class: ', E.ClassName);
    end;

    on E: Exception do
    begin
      WriteLn('✗ Unexpected Error: ', E.Message);
      WriteLn('  Class: ', E.ClassName);
    end;
  end;

  WriteLn;
  WriteLn('2. 异常模式 vs Try* 模式对比');
  WriteLn('---------------------------');

  // Try* 模式 - 不抛异常
  WriteLn('Using Try* mode (no exceptions):');
  LResult := TSSLContextBuilder.Create
    .TryBuildServer(LContext);

  if LResult.IsOk then
    WriteLn('  ✓ Context created (Try* mode)')
  else
    WriteLn('  ✗ Context creation failed: ', LResult.ErrorMessage);

  WriteLn;
end;

{ 场景 4: 混合模式 - 实际应用场景 }
procedure Scenario4_MixedPattern;
var
  LCert, LKey: string;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LData, LHash: TBytes;
  LSuccess: Boolean;
begin
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║ 场景 4: 混合模式 - 实际应用场景                          ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  WriteLn;

  WriteLn('任务: 创建安全的 HTTPS 服务器');
  WriteLn('-----------------------------');
  WriteLn;

  // 步骤 1: 生成证书（Try* 模式 - 可能失败但不致命）
  WriteLn('[1/4] Generating SSL certificate (Try* mode)...');
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'api.example.com',
    'Example Corp',
    365,
    LCert,
    LKey
  ) then
  begin
    WriteLn('✗ Failed to generate certificate - aborting');
    Exit;
  end;
  WriteLn('✓ Certificate generated');
  WriteLn;

  // 步骤 2: 保存证书（异常模式 - 失败应该报告）
  WriteLn('[2/4] Saving certificates (Exception mode)...');
  try
    if not TCertificateUtils.SaveToFile('server.crt', LCert) then
      raise Exception.Create('Failed to save certificate');
    if not TCertificateUtils.SaveToFile('server.key', LKey) then
      raise Exception.Create('Failed to save private key');
    WriteLn('✓ Certificates saved to disk');
  except
    on E: Exception do
    begin
      WriteLn('✗ Save failed: ', E.Message);
      Exit;
    end;
  end;
  WriteLn;

  // 步骤 3: 创建 SSL 上下文（Try* 模式 - 更好的错误信息）
  WriteLn('[3/4] Creating SSL context (Try* mode)...');
  LResult := TSSLContextBuilder.CreateWithSafeDefaults
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .WithTLS12And13
    .TryBuildServer(LContext);

  if not LResult.IsOk then
  begin
    WriteLn('✗ Failed to create context: ', LResult.ErrorMessage);
    Exit;
  end;
  WriteLn('✓ SSL context created');
  WriteLn;

  // 步骤 4: 生成会话令牌（Try* 模式）
  WriteLn('[4/4] Generating session token (Try* mode)...');
  SetLength(LData, 32);
  FillByte(LData[0], 32, $AA);

  LSuccess := TCryptoUtils.TrySHA256(LData, LHash);
  if LSuccess then
    WriteLn('✓ Session token generated: ', Length(LHash), ' bytes')
  else
    WriteLn('✗ Token generation failed');

  WriteLn;
  WriteLn('✓ Server setup complete!');
  WriteLn;

  // 清理
  WriteLn('Cleanup...');
  if FileExists('server.crt') then
    DeleteFile('server.crt');
  if FileExists('server.key') then
    DeleteFile('server.key');
  WriteLn('✓ Temporary files deleted');
  WriteLn;
end;

{ 主程序 }
begin
  WriteLn;
  WriteLn('════════════════════════════════════════════════════════════');
  WriteLn('  fafafa.ssl 错误处理模式综合示例');
  WriteLn('════════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('本示例演示三种错误处理模式的实际应用：');
  WriteLn('  1. Result 模式 - 函数式编程、链式调用');
  WriteLn('  2. Try* 模式 - 高性能、批量操作');
  WriteLn('  3. 异常模式 - 传统风格、例外情况');
  WriteLn('  4. 混合模式 - 实际应用场景');
  WriteLn;
  WriteLn('════════════════════════════════════════════════════════════');
  WriteLn;

  try
    // 运行所有场景
    Scenario1_ResultPattern;
    WriteLn;

    Scenario2_TryPattern;
    WriteLn;

    Scenario3_ExceptionPattern;
    WriteLn;

    Scenario4_MixedPattern;

    WriteLn('════════════════════════════════════════════════════════════');
    WriteLn('  所有示例运行完成！');
    WriteLn('════════════════════════════════════════════════════════════');
    WriteLn;

    ExitCode := 0;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('════════════════════════════════════════════════════════════');
      WriteLn('  FATAL ERROR');
      WriteLn('════════════════════════════════════════════════════════════');
      WriteLn('Class: ', E.ClassName);
      WriteLn('Message: ', E.Message);
      WriteLn('════════════════════════════════════════════════════════════');
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.
