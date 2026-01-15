{**
 * Unit: test_backend_contract
 * Purpose: 后端契约测试 - 验证所有 SSL 后端的行为一致性
 *
 * P1 任务：确保所有后端在相同输入下产生一致的行为
 *
 * 测试范围：
 * - 初始化/未初始化状态下的失败语义
 * - TLS 版本协商
 * - SNI/ALPN 支持
 * - 证书验证行为
 * - 错误码映射一致性
 *
 * 支持后端：OpenSSL, WinSSL, MbedTLS, WolfSSL
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-09
 *}

program test_backend_contract;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed  // 注册 OpenSSL 后端
  , fafafa.ssl.mbedtls.lib     // 注册 MbedTLS 后端
  , fafafa.ssl.wolfssl.lib     // 注册 WolfSSL 后端
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed  // 注册 OpenSSL 后端
  , fafafa.ssl.winssl.lib      // 注册 WinSSL 后端
  , fafafa.ssl.mbedtls.lib     // 注册 MbedTLS 后端
  , fafafa.ssl.wolfssl.lib     // 注册 WolfSSL 后端
  {$ENDIF}
  ;

type
  TContractTestResult = record
    TestName: string;
    Backend: TSSLLibraryType;
    Passed: Boolean;
    ErrorMessage: string;
  end;

var
  GResults: array of TContractTestResult;
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;

procedure AddResult(const ATestName: string; ABackend: TSSLLibraryType;
  APassed: Boolean; const AErrorMsg: string = '');
begin
  SetLength(GResults, Length(GResults) + 1);
  GResults[High(GResults)].TestName := ATestName;
  GResults[High(GResults)].Backend := ABackend;
  GResults[High(GResults)].Passed := APassed;
  GResults[High(GResults)].ErrorMessage := AErrorMsg;
  Inc(GTestCount);
  if APassed then
    Inc(GPassCount)
  else
    Inc(GFailCount);
end;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('=' + StringOfChar('=', 70));
  WriteLn(' ', ATitle);
  WriteLn('=' + StringOfChar('=', 70));
end;

procedure PrintSubHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('--- ', ATitle, ' ---');
end;

{**
 * Helpers
 *}
function TryCreateDirectLibraryInstance(ABackend: TSSLLibraryType;
  out ALib: ISSLLibrary): Boolean;
begin
  ALib := nil;

  case ABackend of
    sslOpenSSL:
      ALib := TOpenSSLLibrary.Create;

    {$IFDEF WINDOWS}
    sslWinSSL:
      ALib := TWinSSLLibrary.Create;
    {$ENDIF}

    sslMbedTLS:
      ALib := TMbedTLSLibrary.Create;

    sslWolfSSL:
      ALib := TWolfSSLLibrary.Create;
  else
    ALib := nil;
  end;

  Result := (ALib <> nil);
end;

function ArrayContains(const AValues: TSSLStringArray; const AExpected: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AValues) to High(AValues) do
  begin
    if SameText(AValues[I], AExpected) then
      Exit(True);
  end;
end;

function FindSanTestCertFile: string;
var
  LExeDir: string;
  LCandidates: array[0..3] of string;
  I: Integer;
begin
  Result := '';

  // Most tests are run from repo root, but make this robust for running from tests/bin
  LExeDir := ExtractFilePath(ParamStr(0));

  // 1) If running from tests/bin
  LCandidates[0] := ExpandFileName(LExeDir + '..' + PathDelim + 'certs' + PathDelim + 'san-test.pem');
  // 2) If running from repo root
  LCandidates[1] := ExpandFileName('tests' + PathDelim + 'certs' + PathDelim + 'san-test.pem');
  // 3) If running from tests/
  LCandidates[2] := ExpandFileName('certs' + PathDelim + 'san-test.pem');
  // 4) Relative to exe dir
  LCandidates[3] := ExpandFileName(LExeDir + 'san-test.pem');

  for I := Low(LCandidates) to High(LCandidates) do
  begin
    if FileExists(LCandidates[I]) then
      Exit(LCandidates[I]);
  end;
end;

{**
 * 契约测试 1: 直接创建后端库实例（不调用 Initialize）时，CreateContext 必须 fail-fast
 *
 * 依据：ISSLLibrary 文档约定“必须先 Initialize 才能调用其他功能”。
 * 最佳实践：不要 silent success / 返回 nil，应该抛出初始化相关异常。
 *}
procedure TestContract_Direct_CreateContext_NotInitialized(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  PrintSubHeader(Format('Contract 1: Direct CreateContext (Not Initialized) - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TryCreateDirectLibraryInstance(ABackend, LLib) then
  begin
    WriteLn('  [SKIP] Backend not supported on this platform');
    Exit;
  end;

  try
    LCtx := LLib.CreateContext(sslCtxClient);
    // If we reach here, the backend did not fail-fast
    if LCtx <> nil then
    begin
      WriteLn('  [FAIL] CreateContext succeeded without Initialize');
      AddResult('Direct_CreateContext_NotInitialized', ABackend, False,
        'CreateContext succeeded without Initialize');
    end
    else
    begin
      WriteLn('  [FAIL] CreateContext returned nil without Initialize');
      AddResult('Direct_CreateContext_NotInitialized', ABackend, False,
        'Returned nil without Initialize');
    end;
  except
    on E: ESSLInitError do
    begin
      WriteLn('  [PASS] Correct init error: ', E.ClassName);
      AddResult('Direct_CreateContext_NotInitialized', ABackend, True);
    end;
    on E: ESSLException do
    begin
      WriteLn('  [FAIL] Unexpected SSL exception: ', E.ClassName, ' - ', E.Message);
      AddResult('Direct_CreateContext_NotInitialized', ABackend, False,
        'Unexpected SSL exception: ' + E.ClassName);
    end;
    on E: Exception do
    begin
      WriteLn('  [FAIL] Unexpected exception: ', E.ClassName, ' - ', E.Message);
      AddResult('Direct_CreateContext_NotInitialized', ABackend, False,
        'Unexpected exception: ' + E.ClassName);
    end;
  end;
end;

{**
 * 契约测试 2: 工厂 GetLibrary 必须返回已初始化的库实例（fail-fast）
 *}
procedure TestContract_Factory_GetLibrary_IsInitialized(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
begin
  PrintSubHeader(Format('Contract 2: Factory GetLibrary (Initialized) - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('  [SKIP] Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    if (LLib <> nil) and LLib.IsInitialized and (LLib.GetLibraryType = ABackend) then
    begin
      WriteLn('  [PASS] GetLibrary returned initialized instance');
      AddResult('Factory_GetLibrary_IsInitialized', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] GetLibrary returned invalid/uninitialized instance');
      AddResult('Factory_GetLibrary_IsInitialized', ABackend, False,
        'Invalid or uninitialized library instance');
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('Factory_GetLibrary_IsInitialized', ABackend, False, E.Message);
    end;
  end;
end;

{**
 * 契约测试 3: 成功初始化后 CreateContext 必须返回有效对象
 *}
procedure TestContract_CreateContext_Initialized(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  PrintSubHeader(Format('Contract 3: CreateContext (Initialized) - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('  [SKIP] Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);

    // 测试客户端上下文
    LCtx := LLib.CreateContext(sslCtxClient);
    if LCtx <> nil then
    begin
      WriteLn('  [PASS] Client context created successfully');
      AddResult('CreateContext_Client', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Client context is nil (should raise exception instead)');
      AddResult('CreateContext_Client', ABackend, False, 'Returned nil instead of raising exception');
    end;

    // 测试服务器上下文
    LCtx := LLib.CreateContext(sslCtxServer);
    if LCtx <> nil then
    begin
      WriteLn('  [PASS] Server context created successfully');
      AddResult('CreateContext_Server', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Server context is nil');
      AddResult('CreateContext_Server', ABackend, False, 'Returned nil');
    end;

  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('CreateContext_Initialized', ABackend, False, E.Message);
    end;
  end;
end;

{**
 * 契约测试 4: GetCapabilities 返回值一致性
 *}
procedure TestContract_GetCapabilities(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  PrintSubHeader(Format('Contract 4: GetCapabilities - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('  [SKIP] Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LCaps := LLib.GetCapabilities;

    // 验证能力矩阵的合理性
    WriteLn('  TLS 1.3 Support: ', LCaps.SupportsTLS13);
    WriteLn('  ALPN Support: ', LCaps.SupportsALPN);
    WriteLn('  SNI Support: ', LCaps.SupportsSNI);
    WriteLn('  Min TLS: ', Ord(LCaps.MinTLSVersion));
    WriteLn('  Max TLS: ', Ord(LCaps.MaxTLSVersion));

    // 基本合理性检查
    if LCaps.MinTLSVersion <= LCaps.MaxTLSVersion then
    begin
      WriteLn('  [PASS] Capabilities are consistent');
      AddResult('GetCapabilities', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] MinTLS > MaxTLS');
      AddResult('GetCapabilities', ABackend, False, 'MinTLS > MaxTLS');
    end;

  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.Message);
      AddResult('GetCapabilities', ABackend, False, E.Message);
    end;
  end;
end;

{**
 * 契约测试 5: 证书 SAN 解析 + VerifyHostname 语义一致性（离线可重复）
 *}
procedure TestContract_Certificate_SAN_VerifyHostname(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
  LCertFile: string;
  LSANs: TSSLStringArray;
  LSanOk: Boolean;
  LVerifyOk: Boolean;
begin
  PrintSubHeader(Format('Contract 5: Certificate SAN + VerifyHostname - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('  [SKIP] Backend not available on this platform');
    Exit;
  end;

  LCertFile := FindSanTestCertFile;
  if LCertFile = '' then
  begin
    WriteLn('  [FAIL] Test certificate not found: san-test.pem');
    AddResult('Cert_SAN_File', ABackend, False, 'san-test.pem not found');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LCert := LLib.CreateCertificate;

    if LCert = nil then
    begin
      WriteLn('  [FAIL] CreateCertificate returned nil');
      AddResult('Cert_CreateCertificate', ABackend, False, 'Returned nil');
      Exit;
    end;

    if not LCert.LoadFromFile(LCertFile) then
    begin
      WriteLn('  [FAIL] LoadFromFile failed: ', LCertFile);
      AddResult('Cert_LoadFromFile', ABackend, False, 'LoadFromFile returned False');
      Exit;
    end;

    LSANs := LCert.GetSubjectAltNames;
    LSanOk :=
      ArrayContains(LSANs, 'san-test.local') and
      ArrayContains(LSANs, 'example.test') and
      ArrayContains(LSANs, '127.0.0.1');

    if LSanOk then
    begin
      WriteLn('  [PASS] SAN parsing returned expected entries');
      AddResult('Cert_SAN_Parsing', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] SAN parsing missing expected entries');
      AddResult('Cert_SAN_Parsing', ABackend, False, 'Missing expected SAN entries');
    end;

    LVerifyOk :=
      LCert.VerifyHostname('san-test.local') and
      LCert.VerifyHostname('example.test') and
      LCert.VerifyHostname('127.0.0.1') and
      (not LCert.VerifyHostname('wrong.test'));

    if LVerifyOk then
    begin
      WriteLn('  [PASS] VerifyHostname behavior is correct');
      AddResult('Cert_VerifyHostname', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] VerifyHostname behavior is inconsistent');
      AddResult('Cert_VerifyHostname', ABackend, False, 'VerifyHostname mismatch');
    end;

  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('Cert_SAN_VerifyHostname', ABackend, False, E.Message);
    end;
  end;
end;

{**
 * 契约测试 6: 安全默认值 (fail-closed)
 *
 * - Client context 默认必须启用 sslVerifyPeer
 * - 默认不应忽略 hostname
 * - 默认不应允许自签名证书
 *}
procedure TestContract_Context_SecureDefaults(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LVerifyMode: TSSLVerifyModes;
  LFlags: TSSLCertVerifyFlags;
begin
  PrintSubHeader(Format('Contract 6: Secure Defaults - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('  [SKIP] Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LCtx := LLib.CreateContext(sslCtxClient);

    LVerifyMode := LCtx.GetVerifyMode;
    if sslVerifyPeer in LVerifyMode then
    begin
      WriteLn('  [PASS] Default VerifyMode includes sslVerifyPeer');
      AddResult('Context_Default_VerifyPeer', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Default VerifyMode missing sslVerifyPeer');
      AddResult('Context_Default_VerifyPeer', ABackend, False,
        'sslVerifyPeer not enabled by default');
    end;

    LFlags := LCtx.GetCertVerifyFlags;

    if not (sslCertVerifyIgnoreHostname in LFlags) then
    begin
      WriteLn('  [PASS] Default CertVerifyFlags does not ignore hostname');
      AddResult('Context_Default_NoIgnoreHostname', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Default CertVerifyFlags ignores hostname');
      AddResult('Context_Default_NoIgnoreHostname', ABackend, False,
        'sslCertVerifyIgnoreHostname should not be default');
    end;

    if not (sslCertVerifyAllowSelfSigned in LFlags) then
    begin
      WriteLn('  [PASS] Default CertVerifyFlags does not allow self-signed');
      AddResult('Context_Default_NoAllowSelfSigned', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Default CertVerifyFlags allows self-signed');
      AddResult('Context_Default_NoAllowSelfSigned', ABackend, False,
        'sslCertVerifyAllowSelfSigned should not be default');
    end;

  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('Context_SecureDefaults', ABackend, False, E.Message);
    end;
  end;
end;

procedure PrintSummary;
var
  I: Integer;
  LPassRate: Double;
begin
  PrintHeader('Contract Test Summary');

  WriteLn;
  WriteLn(Format('Total Tests: %d', [GTestCount]));
  WriteLn(Format('Passed: %d', [GPassCount]));
  WriteLn(Format('Failed: %d', [GFailCount]));

  if GTestCount > 0 then
    LPassRate := GPassCount / GTestCount * 100
  else
    LPassRate := 0;
  WriteLn(Format('Pass Rate: %.1f%%', [LPassRate]));

  if GFailCount > 0 then
  begin
    WriteLn;
    WriteLn('Failed Tests:');
    for I := 0 to High(GResults) do
    begin
      if not GResults[I].Passed then
        WriteLn(Format('  - %s [%s]: %s',
          [GResults[I].TestName,
           SSL_LIBRARY_NAMES[GResults[I].Backend],
           GResults[I].ErrorMessage]));
    end;
  end;

  WriteLn;
  if GFailCount = 0 then
    WriteLn('All contract tests PASSED!')
  else
    WriteLn('Some contract tests FAILED - backend consistency issues detected');
end;

var
  LBackend: TSSLLibraryType;
begin
  PrintHeader('fafafa.ssl Backend Contract Tests');
  WriteLn('Date: ', DateTimeToStr(Now));
  WriteLn('Purpose: Verify consistent behavior across all SSL backends');

  // 运行所有后端的契约测试（不可用后端会明确 SKIP）
  for LBackend := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if LBackend = sslAutoDetect then
      Continue;

    PrintHeader(Format('Testing Backend: %s', [SSL_LIBRARY_NAMES[LBackend]]));

    // 1) Direct library semantics (no Initialize)
    TestContract_Direct_CreateContext_NotInitialized(LBackend);

    // 2) Factory semantics (must be initialized)
    TestContract_Factory_GetLibrary_IsInitialized(LBackend);

    // 3) Context creation
    TestContract_CreateContext_Initialized(LBackend);

    // 4) Capability invariants
    TestContract_GetCapabilities(LBackend);

    // 5) Offline certificate / hostname behavior
    TestContract_Certificate_SAN_VerifyHostname(LBackend);

    // 6) Secure defaults (fail-closed)
    TestContract_Context_SecureDefaults(LBackend);
  end;

  PrintSummary;

  // 返回退出码
  if GFailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
