program test_context_builder_try;

{$mode objfpc}{$H+}

{**
 * 测试 SSL Context Builder 的 Try* 方法
 * 验证 TryBuildClient 和 TryBuildServer 在成功和失败场景下的行为
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Assert(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('  ✓ ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('  ✗ ', AMessage);
  end;
end;

procedure TestTryBuildClient;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
begin
  WriteLn('=== TryBuildClient Tests ===');

  // 测试成功场景 - 基本配置
  LBuilder := TSSLContextBuilder.Create;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client context with defaults');
  Assert(LContext <> nil, 'Context should not be nil on success');
  LContext := nil;

  // 测试成功场景 - 带安全默认值
  LBuilder := TSSLContextBuilder.CreateWithSafeDefaults;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client context with safe defaults');
  Assert(LContext <> nil, 'Context should not be nil with safe defaults');
  LContext := nil;

  // 测试配置链
  LBuilder := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyPeer
    .WithSystemRoots;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client with chained configuration');
  Assert(LContext <> nil, 'Context should not be nil after chaining');
  LContext := nil;

  WriteLn;
end;

procedure TestTryBuildServer;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LCertPEM, LKeyPEM: string;
begin
  WriteLn('=== TryBuildServer Tests ===');

  // 测试失败场景 - 无证书
  LBuilder := TSSLContextBuilder.Create;
  LResult := LBuilder.TryBuildServer(LContext);
  Assert(LResult.IsErr, 'Should fail without certificate');
  Assert(LContext = nil, 'Context should be nil on failure');
  Assert(LResult.ErrorMessage <> '', 'Should provide error message');
  WriteLn('    Error: ', LResult.ErrorMessage);

  // 生成测试证书
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local',
    'Test Org',
    30,
    LCertPEM,
    LKeyPEM
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  // 测试成功场景 - 带证书
  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCertPEM)
    .WithPrivateKeyPEM(LKeyPEM);
  LResult := LBuilder.TryBuildServer(LContext);
  Assert(LResult.IsOk, 'Should create server context with certificate');
  Assert(LContext <> nil, 'Context should not be nil with certificate');
  LContext := nil;

  // 测试配置链 - 完整服务器配置
  LBuilder := TSSLContextBuilder.CreateWithSafeDefaults
    .WithCertificatePEM(LCertPEM)
    .WithPrivateKeyPEM(LKeyPEM)
    .WithTLS12And13
    .WithVerifyNone;
  LResult := LBuilder.TryBuildServer(LContext);
  Assert(LResult.IsOk, 'Should create server with full configuration');
  Assert(LContext <> nil, 'Context should not be nil with full config');
  LContext := nil;

  WriteLn;
end;

procedure TestResultMethods;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
begin
  WriteLn('=== Result Methods Tests ===');

  // 测试 IsOk/IsErr
  LBuilder := TSSLContextBuilder.Create;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Result should be Ok');
  Assert(not LResult.IsErr, 'Result should not be Err');

  // 测试失败的 IsErr
  LBuilder := TSSLContextBuilder.Create;
  LResult := LBuilder.TryBuildServer(LContext);
  Assert(LResult.IsErr, 'Result should be Err without certificate');
  Assert(not LResult.IsOk, 'Result should not be Ok on failure');

  WriteLn;
end;

procedure TestCipherConfiguration;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
begin
  WriteLn('=== Cipher Configuration Tests ===');

  // 测试自定义密码套件
  LBuilder := TSSLContextBuilder.Create
    .WithCipherList('HIGH:!aNULL:!MD5')
    .WithTLS13Ciphersuites('TLS_AES_256_GCM_SHA384');
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client with custom ciphers');
  Assert(LContext <> nil, 'Context should not be nil');
  LContext := nil;

  WriteLn;
end;

procedure TestProtocolVersions;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
begin
  WriteLn('=== Protocol Versions Tests ===');

  // TLS 1.2 only
  LBuilder := TSSLContextBuilder.Create.WithTLS12;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client with TLS 1.2');
  LContext := nil;

  // TLS 1.3 only
  LBuilder := TSSLContextBuilder.Create.WithTLS13;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client with TLS 1.3');
  LContext := nil;

  // Both TLS 1.2 and 1.3
  LBuilder := TSSLContextBuilder.Create.WithTLS12And13;
  LResult := LBuilder.TryBuildClient(LContext);
  Assert(LResult.IsOk, 'Should create client with TLS 1.2 and 1.3');
  LContext := nil;

  WriteLn;
end;

begin
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║   SSL Context Builder Try* Methods Tests                  ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn;

  try
    TestTryBuildClient;
    TestTryBuildServer;
    TestResultMethods;
    TestCipherConfiguration;
    TestProtocolVersions;

    WriteLn('╔════════════════════════════════════════════════════════════╗');
    WriteLn(Format('║   Tests Passed: %-3d  Failed: %-3d                         ║', [GTestsPassed, GTestsFailed]));
    WriteLn('╚════════════════════════════════════════════════════════════╝');

    if GTestsFailed > 0 then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
