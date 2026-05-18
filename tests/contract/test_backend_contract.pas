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
  fafafa.ssl.errors,
  fafafa.ssl.native_handle,
  fafafa.ssl.freepascal.lib,
  fafafa.ssl.freepascal.session
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed  // 注册 OpenSSL 后端
  , fafafa.ssl.openssl.base
  , fafafa.ssl.openssl.api.core
  , fafafa.ssl.openssl.session
  , fafafa.ssl.mbedtls.lib     // 注册 MbedTLS 后端
  , fafafa.ssl.mbedtls.base
  , fafafa.ssl.mbedtls.session
  , fafafa.ssl.wolfssl.lib     // 注册 WolfSSL 后端
  , fafafa.ssl.wolfssl.base
  , fafafa.ssl.wolfssl.session
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed  // 注册 OpenSSL 后端
  , fafafa.ssl.winssl.lib      // 注册 WinSSL 后端
  , fafafa.ssl.mbedtls.lib     // 注册 MbedTLS 后端
  , fafafa.ssl.wolfssl.lib     // 注册 WolfSSL 后端
  , fafafa.ssl.winssl.connection
  {$ENDIF}
  ;

type
  TContractTestResult = record
    TestName: string;
    Backend: TSSLLibraryType;
    Passed: Boolean;
    ErrorMessage: string;
  end;

  THTTPHookStub = class
  public
    function HTTPGet(const AURL: string; ATimeoutMs: Integer): TSSLDataResult;
    function HTTPPost(const AURL, AContentType: string;
      const ABody: TBytes; ATimeoutMs: Integer): TSSLDataResult;
  end;

var
  GResults: array of TContractTestResult;
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;

function THTTPHookStub.HTTPGet(const AURL: string;
  ATimeoutMs: Integer): TSSLDataResult;
var
  LEmpty: TBytes;
begin
  SetLength(LEmpty, 0);
  Result := TSSLDataResult.Ok(LEmpty);
end;

function THTTPHookStub.HTTPPost(const AURL, AContentType: string;
  const ABody: TBytes; ATimeoutMs: Integer): TSSLDataResult;
var
  LEmpty: TBytes;
begin
  SetLength(LEmpty, 0);
  Result := TSSLDataResult.Ok(LEmpty);
end;

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

procedure AddSkip(const AReason: string);
begin
  Inc(GSkipCount);
  WriteLn('  [SKIP] ', AReason);
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

function FeatureLevelPresent(ALevel: TSSLFeatureSupportLevel): Boolean;
begin
  Result := ALevel <> sslSupportNone;
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

    sslFreePascal:
      ALib := TFreePascalSSLLibrary.Create;
  else
    ALib := nil;
  end;

  Result := (ALib <> nil);
end;

procedure TestHarness_DirectInstanceCoverage_FreePascal;
var
  LLib: ISSLLibrary;
begin
  PrintSubHeader('Harness: Direct instance coverage - FreePascal');

  if TryCreateDirectLibraryInstance(sslFreePascal, LLib) then
  begin
    WriteLn('  [PASS] TryCreateDirectLibraryInstance supports FreePascal backend');
    AddResult('Harness_DirectInstanceCoverage_FreePascal', sslFreePascal, True);
  end
  else
  begin
    WriteLn('  [FAIL] TryCreateDirectLibraryInstance should support FreePascal backend');
    AddResult('Harness_DirectInstanceCoverage_FreePascal', sslFreePascal, False,
      'Direct instance helper does not handle sslFreePascal');
  end;
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

function CertificatePublicIdentityMatches(const ALeft,
  ARight: ISSLCertificate): Boolean;
begin
  if (ALeft = nil) or (ARight = nil) then
    Exit((ALeft = nil) and (ARight = nil));

  Result :=
    SameText(ALeft.GetSubject, ARight.GetSubject) and
    SameText(ALeft.GetIssuer, ARight.GetIssuer) and
    SameText(ALeft.GetSerialNumber, ARight.GetSerialNumber);
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
    AddSkip('Backend not supported on this platform');
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
    AddSkip('Backend not available on this platform');
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
    AddSkip('Backend not available on this platform');
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
    AddSkip('Backend not available on this platform');
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
    AddSkip('Backend not available on this platform');
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
    AddSkip('Backend not available on this platform');
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

{**
 * 契约测试 7: 明确不支持的可选接口不应暴露假阳性 Supports(...)
 *
 * 当前先锁定已经有明确存根/限制说明的后端：
 * - MbedTLS: Early Data / manual server OCSP stapling
 * - WinSSL: Early Data / manual server OCSP stapling
 *}
procedure TestContract_UnsupportedOptionalInterfacesAbsent(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LClientCtx: ISSLContext;
  LServerCtx: ISSLContext;
  LEarlyDataCtx: ISSLEarlyDataContext;
  LServerStaplingCtx: ISSLServerOCSPStaplingContext;
begin
  PrintSubHeader(Format('Contract 7: Unsupported Optional Interfaces Absent - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not (ABackend in [sslMbedTLS, sslWinSSL]) then
  begin
    AddSkip('Contract only applies to backends with explicit unsupported/stub optional interfaces');
    Exit;
  end;

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LClientCtx := LLib.CreateContext(sslCtxClient);
    LServerCtx := LLib.CreateContext(sslCtxServer);

    if not Supports(LClientCtx, ISSLEarlyDataContext, LEarlyDataCtx) then
    begin
      WriteLn('  [PASS] Unsupported early-data context interface is absent');
      AddResult('UnsupportedOptional_EarlyDataAbsent', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Unsupported early-data context interface is still exposed');
      AddResult('UnsupportedOptional_EarlyDataAbsent', ABackend, False,
        'ISSLEarlyDataContext should not be exposed when backend only has stub/not-supported implementation');
    end;

    if not Supports(LServerCtx, ISSLServerOCSPStaplingContext, LServerStaplingCtx) then
    begin
      WriteLn('  [PASS] Unsupported server-OCSP-stapling context interface is absent');
      AddResult('UnsupportedOptional_ServerOCSPStaplingAbsent', ABackend, True);
    end
    else
    begin
      WriteLn('  [FAIL] Unsupported server-OCSP-stapling interface is still exposed');
      AddResult('UnsupportedOptional_ServerOCSPStaplingAbsent', ABackend, False,
        'ISSLServerOCSPStaplingContext should not be exposed when backend only has stub/not-supported implementation');
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('UnsupportedOptionalInterfacesAbsent', ABackend, False, E.Message);
    end;
  end;
end;

procedure TestContract_ClientConnectionSNIInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LProbeStream: TMemoryStream;
const
  PROBE_SERVER_NAME = 'contract.example.test';
begin
  PrintSubHeader(Format('Contract 8: Client connection SNI interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCaps := LLib.GetCapabilities;
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if FeatureLevelPresent(LCaps.SNISupport) then
      begin
        if not Supports(LConn, ISSLClientConnection, LClientConn) then
        begin
          WriteLn('  [FAIL] SNI-capable backend does not expose ISSLClientConnection');
          AddResult('ClientConnectionSNIInterfaceAligned', ABackend, False,
            'SNISupport<>None but connection does not expose ISSLClientConnection');
          Exit;
        end;

        LClientConn.SetServerName(PROBE_SERVER_NAME);
        if SameText(LClientConn.GetServerName, PROBE_SERVER_NAME) then
        begin
          WriteLn('  [PASS] SNI-capable backend exposes per-connection server-name surface');
          AddResult('ClientConnectionSNIInterfaceAligned', ABackend, True);
        end
        else
        begin
          WriteLn('  [FAIL] ISSLClientConnection round-trip failed');
          AddResult('ClientConnectionSNIInterfaceAligned', ABackend, False,
            'ISSLClientConnection.GetServerName did not return the configured per-connection value');
        end;
      end
      else
      begin
        if Supports(LConn, ISSLClientConnection, LClientConn) then
        begin
          WriteLn('  [FAIL] Backend without SNI capability still exposes ISSLClientConnection');
          AddResult('ClientConnectionSNIInterfaceAligned', ABackend, False,
            'SNISupport=None but connection still exposes ISSLClientConnection');
        end
        else
        begin
          WriteLn('  [PASS] Backend without SNI capability keeps ISSLClientConnection absent');
          AddResult('ClientConnectionSNIInterfaceAligned', ABackend, True);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ClientConnectionSNIInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_ClientConnectionCTInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LCT: ISSLCertificateTransparency;
  LCTValidation: ISSLCertificateTransparencyValidation;
  LProbeStream: TMemoryStream;
begin
  PrintSubHeader(Format('Contract 9: Client connection CT interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCaps := LLib.GetCapabilities;
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if FeatureLevelPresent(LCaps.CertTransparencySupport) then
      begin
        if not Supports(LConn, ISSLCertificateTransparency, LCT) then
        begin
          WriteLn('  [FAIL] CT-capable backend does not expose ISSLCertificateTransparency');
          AddResult('ClientConnectionCTInterfaceAligned', ABackend, False,
            'CertTransparencySupport<>None but connection does not expose ISSLCertificateTransparency');
        end
        else if SameText(LCT.GetCertificateTransparencyStatus, 'Not Supported') then
        begin
          WriteLn('  [FAIL] CT-capable backend still falls back to base CT stub');
          AddResult('ClientConnectionCTInterfaceAligned', ABackend, False,
            'CertTransparencySupport<>None but CT status still reports Not Supported');
        end
        else
        begin
          WriteLn('  [PASS] CT-capable backend exposes non-stub CT surface');
          AddResult('ClientConnectionCTInterfaceAligned', ABackend, True);
        end;
      end
      else
      begin
        if Supports(LConn, ISSLCertificateTransparency, LCT) then
        begin
          WriteLn('  [FAIL] Backend without CT capability still exposes ISSLCertificateTransparency');
          AddResult('ClientConnectionCTInterfaceAligned', ABackend, False,
            'CertTransparencySupport=None but connection still exposes ISSLCertificateTransparency');
        end
        else
        begin
          WriteLn('  [PASS] Backend without CT capability keeps ISSLCertificateTransparency absent');
          AddResult('ClientConnectionCTInterfaceAligned', ABackend, True);
        end;
      end;

      if LCaps.CertTransparencySupport <> sslSupportNone then
      begin
        if not Supports(LConn, ISSLCertificateTransparencyValidation, LCTValidation) then
        begin
          WriteLn('  [FAIL] CT-validation-capable backend does not expose ISSLCertificateTransparencyValidation');
          AddResult('ClientConnectionCTValidationInterfaceAligned', ABackend, False,
            'CertTransparencySupport<>None but connection does not expose ISSLCertificateTransparencyValidation');
        end
        else if SameText(LCTValidation.GetCertificateTransparencyValidationStatus, 'Not Supported') then
        begin
          WriteLn('  [FAIL] CT-validation-capable backend still falls back to base validation stub');
          AddResult('ClientConnectionCTValidationInterfaceAligned', ABackend, False,
            'CertTransparencySupport<>None but CT validation status still reports Not Supported');
        end
        else
        begin
          WriteLn('  [PASS] CT-validation-capable backend exposes non-stub CT validation surface');
          AddResult('ClientConnectionCTValidationInterfaceAligned', ABackend, True);
        end;
      end
      else
      begin
        if Supports(LConn, ISSLCertificateTransparencyValidation, LCTValidation) then
        begin
          WriteLn('  [FAIL] Backend without CT validation capability still exposes ISSLCertificateTransparencyValidation');
          AddResult('ClientConnectionCTValidationInterfaceAligned', ABackend, False,
            'CertTransparencySupport=None but connection still exposes ISSLCertificateTransparencyValidation');
        end
        else
        begin
          WriteLn('  [PASS] Backend without CT validation capability keeps validation interface absent');
          AddResult('ClientConnectionCTValidationInterfaceAligned', ABackend, True);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ClientConnectionCTInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_ClientConnectionOCSPInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LOCSP: ISSLOCSPStapling;
  LProbeStream: TMemoryStream;
begin
  PrintSubHeader(Format('Contract 10: Client connection OCSP interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCaps := LLib.GetCapabilities;
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if FeatureLevelPresent(LCaps.OCSPStaplingSupport) then
      begin
        if not Supports(LConn, ISSLOCSPStapling, LOCSP) then
        begin
          WriteLn('  [FAIL] OCSP-capable backend does not expose ISSLOCSPStapling');
          AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, False,
            'OCSPStaplingSupport<>None but connection does not expose ISSLOCSPStapling');
        end
        else if SameText(LOCSP.GetOCSPResponseStatus, 'Not Supported') then
        begin
          WriteLn('  [FAIL] OCSP-capable backend still falls back to base OCSP stub');
          AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, False,
            'OCSPStaplingSupport<>None but OCSP status still reports Not Supported');
        end
        else
        begin
          WriteLn('  [PASS] OCSP-capable backend exposes non-stub OCSP surface');
          AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, True);
        end;
      end
      else
      begin
        if Supports(LConn, ISSLOCSPStapling, LOCSP) then
        begin
          WriteLn('  [FAIL] Backend without OCSP capability still exposes ISSLOCSPStapling');
          AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, False,
            'OCSPStaplingSupport=None but connection still exposes ISSLOCSPStapling');
        end
        else
        begin
          WriteLn('  [PASS] Backend without OCSP capability keeps ISSLOCSPStapling absent');
          AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, True);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ClientConnectionOCSPInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_ConnectionNativeHandleInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LProbeStream: TMemoryStream;
begin
  PrintSubHeader(Format('Contract 11: Connection native-handle interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if ABackend = sslFreePascal then
      begin
        if Supports(LConn, ISSLNativeHandleAccess, LNative) then
        begin
          WriteLn('  [FAIL] Pure backend connection should not expose ISSLNativeHandleAccess');
          AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, False,
            'Pure FreePascal connection unexpectedly exposes ISSLNativeHandleAccess');
        end
        else
        begin
          WriteLn('  [PASS] Pure backend connection keeps ISSLNativeHandleAccess absent');
          AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, True);
        end;
        Exit;
      end;

      if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
      begin
        WriteLn('  [FAIL] C-library backend connection does not expose ISSLNativeHandleAccess');
        AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, False,
          'C-library backend connection does not expose ISSLNativeHandleAccess');
      end
      else if LNative.GetBackendType <> ABackend then
      begin
        WriteLn('  [FAIL] Native-handle backend type does not match connection backend');
        AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, False,
          'ISSLNativeHandleAccess.GetBackendType does not match the connection backend');
      end
      else if LNative.GetNativeHandle = nil then
      begin
        WriteLn('  [FAIL] Native-handle interface returned nil handle');
        AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, False,
          'ISSLNativeHandleAccess.GetNativeHandle returned nil');
      end
      else
      begin
        WriteLn('  [PASS] C-library backend connection exposes native-handle surface');
        AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, True);
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ConnectionNativeHandleInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_ContextOptionalInterfacesAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LClientCtx: ISSLContext;
  LServerCtx: ISSLContext;
  LConn: ISSLConnection;
  LEarlyDataCtx: ISSLEarlyDataContext;
  LEarlyDataConn: ISSLEarlyDataConnection;
  LServerStaplingCtx: ISSLServerOCSPStaplingContext;
  LProbeStream: TMemoryStream;
begin
  PrintSubHeader(Format('Contract 12: Context optional interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LCaps := LLib.GetCapabilities;
    LClientCtx := LLib.CreateContext(sslCtxClient);
    LServerCtx := LLib.CreateContext(sslCtxServer);
    LProbeStream := TMemoryStream.Create;
    try
      LConn := LClientCtx.CreateConnection(LProbeStream);

      if LCaps.EarlyDataSupport <> sslSupportNone then
      begin
        if not Supports(LClientCtx, ISSLEarlyDataContext, LEarlyDataCtx) then
        begin
          WriteLn('  [FAIL] Early-data-capable backend does not expose ISSLEarlyDataContext');
          AddResult('ContextOptional_EarlyDataAligned', ABackend, False,
            'EarlyDataSupport<>None but client context does not expose ISSLEarlyDataContext');
        end
        else
        begin
          WriteLn('  [PASS] Early-data-capable backend exposes ISSLEarlyDataContext');
          AddResult('ContextOptional_EarlyDataAligned', ABackend, True);
        end;

        if not Supports(LConn, ISSLEarlyDataConnection, LEarlyDataConn) then
        begin
          WriteLn('  [FAIL] Early-data-capable backend does not expose ISSLEarlyDataConnection');
          AddResult('ContextOptional_EarlyDataConnectionAligned', ABackend, False,
            'EarlyDataSupport<>None but client connection does not expose ISSLEarlyDataConnection');
        end
        else
        begin
          WriteLn('  [PASS] Early-data-capable backend exposes ISSLEarlyDataConnection');
          AddResult('ContextOptional_EarlyDataConnectionAligned', ABackend, True);
        end;
      end
      else
      begin
        if Supports(LClientCtx, ISSLEarlyDataContext, LEarlyDataCtx) then
        begin
          WriteLn('  [FAIL] Backend without early-data capability still exposes ISSLEarlyDataContext');
          AddResult('ContextOptional_EarlyDataAligned', ABackend, False,
            'EarlyDataSupport=None but client context still exposes ISSLEarlyDataContext');
        end
        else
        begin
          WriteLn('  [PASS] Backend without early-data capability keeps ISSLEarlyDataContext absent');
          AddResult('ContextOptional_EarlyDataAligned', ABackend, True);
        end;

        if Supports(LConn, ISSLEarlyDataConnection, LEarlyDataConn) then
        begin
          WriteLn('  [FAIL] Backend without early-data capability still exposes ISSLEarlyDataConnection');
          AddResult('ContextOptional_EarlyDataConnectionAligned', ABackend, False,
            'EarlyDataSupport=None but client connection still exposes ISSLEarlyDataConnection');
        end
        else
        begin
          WriteLn('  [PASS] Backend without early-data capability keeps ISSLEarlyDataConnection absent');
          AddResult('ContextOptional_EarlyDataConnectionAligned', ABackend, True);
        end;
      end;

      if LCaps.OCSPStaplingSupport <> sslSupportNone then
      begin
        if not Supports(LServerCtx, ISSLServerOCSPStaplingContext, LServerStaplingCtx) then
        begin
          WriteLn('  [FAIL] OCSP-capable backend does not expose ISSLServerOCSPStaplingContext');
          AddResult('ContextOptional_ServerOCSPAligned', ABackend, False,
            'OCSPStaplingSupport<>None but server context does not expose ISSLServerOCSPStaplingContext');
        end
        else
        begin
          WriteLn('  [PASS] OCSP-capable backend exposes ISSLServerOCSPStaplingContext');
          AddResult('ContextOptional_ServerOCSPAligned', ABackend, True);
        end;
      end
      else
      begin
        if Supports(LServerCtx, ISSLServerOCSPStaplingContext, LServerStaplingCtx) then
        begin
          WriteLn('  [FAIL] Backend without OCSP capability still exposes ISSLServerOCSPStaplingContext');
          AddResult('ContextOptional_ServerOCSPAligned', ABackend, False,
            'OCSPStaplingSupport=None but server context still exposes ISSLServerOCSPStaplingContext');
        end
        else
        begin
          WriteLn('  [PASS] Backend without OCSP capability keeps ISSLServerOCSPStaplingContext absent');
          AddResult('ContextOptional_ServerOCSPAligned', ABackend, True);
        end;
      end;
    finally
      LProbeStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('ContextOptionalInterfacesAligned', ABackend, False, E.Message);
    end;
  end;
end;

procedure TestContract_ContextNativeHandleInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LClientCtx: ISSLContext;
  LServerCtx: ISSLContext;

  function ValidateContext(const ALabel: string; ACtx: ISSLContext;
    out AError: string): Boolean;
  var
    LNative: ISSLNativeHandleAccess;
  begin
    Result := False;
    AError := '';

    if ABackend = sslFreePascal then
    begin
      if Supports(ACtx, ISSLNativeHandleAccess, LNative) then
        AError := ALabel + ' pure backend context unexpectedly exposes ISSLNativeHandleAccess'
      else
        Result := True;
      Exit;
    end;

    if not Supports(ACtx, ISSLNativeHandleAccess, LNative) then
      AError := ALabel + ' C-library backend context does not expose ISSLNativeHandleAccess'
    else if LNative.GetBackendType <> ABackend then
      AError := ALabel + ' ISSLNativeHandleAccess.GetBackendType does not match the context backend'
    else if not LNative.IsNativeHandleValid then
      AError := ALabel + ' ISSLNativeHandleAccess.IsNativeHandleValid returned False'
    else if LNative.GetNativeHandle = nil then
      AError := ALabel + ' ISSLNativeHandleAccess.GetNativeHandle returned nil'
    else
      Result := True;
  end;

var
  LError: string;
begin
  PrintSubHeader(Format('Contract 13: Context native-handle interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LClientCtx := LLib.CreateContext(sslCtxClient);
    LServerCtx := LLib.CreateContext(sslCtxServer);

    if not ValidateContext('client', LClientCtx, LError) then
    begin
      WriteLn('  [FAIL] ', LError);
      AddResult('ContextNativeHandleInterfaceAligned', ABackend, False, LError);
    end
    else if not ValidateContext('server', LServerCtx, LError) then
    begin
      WriteLn('  [FAIL] ', LError);
      AddResult('ContextNativeHandleInterfaceAligned', ABackend, False, LError);
    end
    else if ABackend = sslFreePascal then
    begin
      WriteLn('  [PASS] Pure backend contexts keep ISSLNativeHandleAccess absent');
      AddResult('ContextNativeHandleInterfaceAligned', ABackend, True);
    end
    else
    begin
      WriteLn('  [PASS] C-library backend contexts expose native-handle surface');
      AddResult('ContextNativeHandleInterfaceAligned', ABackend, True);
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('ContextNativeHandleInterfaceAligned', ABackend, False, E.Message);
    end;
  end;
end;

procedure TestContract_ContextHTTPHooksInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LClientCtx: ISSLContext;
  LServerCtx: ISSLContext;
  LHooks: ISSLHttpHooksAccess;
  LHookStub: THTTPHookStub;
  LExpectedPresent: Boolean;

  function ValidateContext(const ALabel: string; ACtx: ISSLContext;
    out AError: string): Boolean;
  var
    LGet: TSSLHTTPGetCallback;
    LPost: TSSLHTTPPostCallback;
  begin
    Result := False;
    AError := '';

    if not Supports(ACtx, ISSLHttpHooksAccess, LHooks) then
    begin
      if LExpectedPresent then
        AError := ALabel + ' context does not expose ISSLHttpHooksAccess'
      else
        Result := True;
      Exit;
    end;

    if not LExpectedPresent then
    begin
      AError := ALabel + ' context unexpectedly exposes ISSLHttpHooksAccess';
      Exit;
    end;

    LHooks.SetHTTPGetCallback(@LHookStub.HTTPGet);
    LHooks.SetHTTPPostCallback(@LHookStub.HTTPPost);
    LGet := LHooks.GetHTTPGetCallback;
    LPost := LHooks.GetHTTPPostCallback;

    if not Assigned(LGet) then
      AError := ALabel + ' HTTP GET callback round-trip lost assignment'
    else if not Assigned(LPost) then
      AError := ALabel + ' HTTP POST callback round-trip lost assignment'
    else
      Result := True;
  end;

var
  LError: string;
begin
  PrintSubHeader(Format('Contract 14: Context HTTP hooks interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LExpectedPresent := ABackend in [sslOpenSSL, sslFreePascal];
  LHookStub := THTTPHookStub.Create;

  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LClientCtx := LLib.CreateContext(sslCtxClient);
      LServerCtx := LLib.CreateContext(sslCtxServer);

      if not ValidateContext('client', LClientCtx, LError) then
      begin
        WriteLn('  [FAIL] ', LError);
        AddResult('ContextHTTPHooksInterfaceAligned', ABackend, False, LError);
      end
      else if not ValidateContext('server', LServerCtx, LError) then
      begin
        WriteLn('  [FAIL] ', LError);
        AddResult('ContextHTTPHooksInterfaceAligned', ABackend, False, LError);
      end
      else if LExpectedPresent then
      begin
        WriteLn('  [PASS] HTTP-hooks backend contexts expose ISSLHttpHooksAccess');
        AddResult('ContextHTTPHooksInterfaceAligned', ABackend, True);
      end
      else
      begin
        WriteLn('  [PASS] Backend without HTTP-hooks surface keeps ISSLHttpHooksAccess absent');
        AddResult('ContextHTTPHooksInterfaceAligned', ABackend, True);
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ContextHTTPHooksInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LHookStub.Free;
  end;
end;

procedure TestContract_SessionNativeHandleInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LSession: ISSLSession;
  LOwnedProbeHandle: Pointer;

  function CreateSessionProbe(out AOwnedRawHandle: Pointer): ISSLSession;
  var
    LOpenSSLSession: PSSL_SESSION;
  begin
    Result := nil;
    AOwnedRawHandle := nil;

    case ABackend of
      sslOpenSSL:
        begin
          if not Assigned(SSL_SESSION_new) then
            Exit(nil);

          LOpenSSLSession := SSL_SESSION_new();
          if LOpenSSLSession = nil then
            Exit(nil);

          Result := TOpenSSLSession.Create(LOpenSSLSession, Assigned(SSL_SESSION_free));
        end;

      sslMbedTLS:
        begin
          GetMem(AOwnedRawHandle, SizeOf(Byte));
          PByte(AOwnedRawHandle)^ := 0;
          Result := TMbedTLSSession.Create(Pmbedtls_ssl_session(AOwnedRawHandle), False);
        end;

      sslWolfSSL:
        begin
          GetMem(AOwnedRawHandle, SizeOf(Byte));
          PByte(AOwnedRawHandle)^ := 0;
          Result := TWolfSSLSession.Create(PWOLFSSL_SESSION(AOwnedRawHandle), False);
        end;

      sslFreePascal:
        Result := TFreePascalSession.Create;
    else
      Result := nil;
    end;
  end;

  function ValidateSession(ASession: ISSLSession; out AError: string): Boolean;
  var
    LNative: ISSLNativeHandleAccess;
    LHelperHandle: Pointer;
    LInterfaceHandle: Pointer;
  begin
    Result := False;
    AError := '';

    if ABackend = sslFreePascal then
    begin
      if Supports(ASession, ISSLNativeHandleAccess, LNative) then
        AError := 'Pure backend session unexpectedly exposes ISSLNativeHandleAccess'
      else if TryGetNativeHandle(ASession, LHelperHandle) then
        AError := 'Pure backend session unexpectedly returns a native handle'
      else
        Result := True;
      Exit;
    end;

    if not Supports(ASession, ISSLNativeHandleAccess, LNative) then
      AError := 'C-library backend session does not expose ISSLNativeHandleAccess'
    else if LNative.GetBackendType <> ABackend then
      AError := 'ISSLNativeHandleAccess.GetBackendType does not match the session backend'
    else if not LNative.IsNativeHandleValid then
      AError := 'ISSLNativeHandleAccess.IsNativeHandleValid returned False'
    else
    begin
      LInterfaceHandle := LNative.GetNativeHandle;
      if LInterfaceHandle = nil then
        AError := 'ISSLNativeHandleAccess.GetNativeHandle returned nil'
      else if not TryGetNativeHandle(ASession, LHelperHandle) then
        AError := 'TryGetNativeHandle returned False for a C-library session'
      else if LHelperHandle = nil then
        AError := 'TryGetNativeHandle returned nil for a C-library session'
      else if LHelperHandle <> LInterfaceHandle then
        AError := 'TryGetNativeHandle did not round-trip the native handle'
      else
        Result := True;
    end;
  end;

var
  LError: string;
begin
  PrintSubHeader(Format('Contract 15: Session native-handle interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if ABackend = sslWinSSL then
  begin
    AddSkip('WinSSL session truth split requires a dedicated Windows-focused batch');
    Exit;
  end;

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LOwnedProbeHandle := nil;
  LSession := nil;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    if LLib = nil then
      raise Exception.Create('Failed to load backend library');

    LSession := CreateSessionProbe(LOwnedProbeHandle);
    if LSession = nil then
    begin
      AddSkip('Could not create session probe on this runtime');
      Exit;
    end;

    if not ValidateSession(LSession, LError) then
    begin
      WriteLn('  [FAIL] ', LError);
      AddResult('SessionNativeHandleInterfaceAligned', ABackend, False, LError);
    end
    else if ABackend = sslFreePascal then
    begin
      WriteLn('  [PASS] Pure backend sessions keep ISSLNativeHandleAccess absent');
      AddResult('SessionNativeHandleInterfaceAligned', ABackend, True);
    end
    else
    begin
      WriteLn('  [PASS] C-library backend sessions expose native-handle surface');
      AddResult('SessionNativeHandleInterfaceAligned', ABackend, True);
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('SessionNativeHandleInterfaceAligned', ABackend, False, E.Message);
    end;
  end;

  LSession := nil;
  if LOwnedProbeHandle <> nil then
    FreeMem(LOwnedProbeHandle);
end;

procedure TestContract_CertificateNativeHandleInterfaceAligned(ABackend: TSSLLibraryType);
const
  CSignerCertFixture = 'tests/certificate/test_certs/signer_cert.pem';
var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
  LNative: ISSLNativeHandleAccess;
  LHelperHandle: Pointer;
  LInterfaceHandle: Pointer;
begin
  PrintSubHeader(Format('Contract 16: Certificate native-handle interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  if not FileExists(CSignerCertFixture) then
  begin
    AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
      'Certificate fixture not found: ' + CSignerCertFixture);
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LCert := LLib.CreateCertificate;
    if LCert = nil then
      raise Exception.Create('CreateCertificate returned nil');

    if not LCert.LoadFromFile(CSignerCertFixture) then
      raise Exception.Create('LoadFromFile returned False for certificate fixture');

    if ABackend = sslFreePascal then
    begin
      if Supports(LCert, ISSLNativeHandleAccess, LNative) then
      begin
        WriteLn('  [FAIL] Pure backend certificate should not expose ISSLNativeHandleAccess');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'Pure FreePascal certificate unexpectedly exposes ISSLNativeHandleAccess');
      end
      else if TryGetNativeHandle(LCert, LHelperHandle) then
      begin
        WriteLn('  [FAIL] Pure backend certificate unexpectedly returned a native handle');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'Pure FreePascal certificate unexpectedly returns a native handle');
      end
      else
      begin
        WriteLn('  [PASS] Pure backend certificates keep ISSLNativeHandleAccess absent');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, True);
      end;
      Exit;
    end;

    if not Supports(LCert, ISSLNativeHandleAccess, LNative) then
    begin
      WriteLn('  [FAIL] C-library certificate does not expose ISSLNativeHandleAccess');
      AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
        'C-library certificate does not expose ISSLNativeHandleAccess');
    end
    else if LNative.GetBackendType <> ABackend then
    begin
      WriteLn('  [FAIL] Native-handle backend type does not match certificate backend');
      AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
        'ISSLNativeHandleAccess.GetBackendType does not match the certificate backend');
    end
    else if not LNative.IsNativeHandleValid then
    begin
      WriteLn('  [FAIL] Certificate native handle reports invalid');
      AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
        'ISSLNativeHandleAccess.IsNativeHandleValid returned False');
    end
    else
    begin
      LInterfaceHandle := LNative.GetNativeHandle;
      if LInterfaceHandle = nil then
      begin
        WriteLn('  [FAIL] Native-handle interface returned nil certificate handle');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'ISSLNativeHandleAccess.GetNativeHandle returned nil');
      end
      else if not TryGetNativeHandle(LCert, LHelperHandle) then
      begin
        WriteLn('  [FAIL] Helper failed to round-trip certificate native handle');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle returned False for a C-library certificate');
      end
      else if LHelperHandle = nil then
      begin
        WriteLn('  [FAIL] Helper returned nil certificate native handle');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle returned nil for a C-library certificate');
      end
      else if LHelperHandle <> LInterfaceHandle then
      begin
        WriteLn('  [FAIL] Helper did not round-trip certificate native handle');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle did not round-trip the certificate native handle');
      end
      else
      begin
        WriteLn('  [PASS] C-library certificates expose native-handle surface');
        AddResult('CertificateNativeHandleInterfaceAligned', ABackend, True);
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('CertificateNativeHandleInterfaceAligned', ABackend, False, E.Message);
    end;
  end;
end;

procedure TestContract_CertificateStoreNativeHandleInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LStore: ISSLCertificateStore;
  LNative: ISSLNativeHandleAccess;
  LHelperHandle: Pointer;
  LInterfaceHandle: Pointer;
begin
  PrintSubHeader(Format('Contract 17: Certificate-store native-handle interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
    LStore := LLib.CreateCertificateStore;
    if LStore = nil then
      raise Exception.Create('CreateCertificateStore returned nil');

    if ABackend = sslFreePascal then
    begin
      if Supports(LStore, ISSLNativeHandleAccess, LNative) then
      begin
        WriteLn('  [FAIL] Pure backend certificate store should not expose ISSLNativeHandleAccess');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'Pure FreePascal certificate store unexpectedly exposes ISSLNativeHandleAccess');
      end
      else if TryGetNativeHandle(LStore, LHelperHandle) then
      begin
        WriteLn('  [FAIL] Pure backend certificate store unexpectedly returned a native handle');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'Pure FreePascal certificate store unexpectedly returns a native handle');
      end
      else
      begin
        WriteLn('  [PASS] Pure backend certificate stores keep ISSLNativeHandleAccess absent');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, True);
      end;
      Exit;
    end;

    if not Supports(LStore, ISSLNativeHandleAccess, LNative) then
    begin
      WriteLn('  [FAIL] C-library certificate store does not expose ISSLNativeHandleAccess');
      AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
        'C-library certificate store does not expose ISSLNativeHandleAccess');
    end
    else if LNative.GetBackendType <> ABackend then
    begin
      WriteLn('  [FAIL] Native-handle backend type does not match certificate-store backend');
      AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
        'ISSLNativeHandleAccess.GetBackendType does not match the certificate-store backend');
    end
    else if not LNative.IsNativeHandleValid then
    begin
      WriteLn('  [FAIL] Certificate-store native handle reports invalid');
      AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
        'ISSLNativeHandleAccess.IsNativeHandleValid returned False');
    end
    else
    begin
      LInterfaceHandle := LNative.GetNativeHandle;
      if LInterfaceHandle = nil then
      begin
        WriteLn('  [FAIL] Native-handle interface returned nil certificate-store handle');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'ISSLNativeHandleAccess.GetNativeHandle returned nil');
      end
      else if not TryGetNativeHandle(LStore, LHelperHandle) then
      begin
        WriteLn('  [FAIL] Helper failed to round-trip certificate-store native handle');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle returned False for a C-library certificate store');
      end
      else if LHelperHandle = nil then
      begin
        WriteLn('  [FAIL] Helper returned nil certificate-store native handle');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle returned nil for a C-library certificate store');
      end
      else if LHelperHandle <> LInterfaceHandle then
      begin
        WriteLn('  [FAIL] Helper did not round-trip certificate-store native handle');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False,
          'TryGetNativeHandle did not round-trip the certificate-store native handle');
      end
      else
      begin
        WriteLn('  [PASS] C-library certificate stores expose native-handle surface');
        AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, True);
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
      AddResult('CertificateStoreNativeHandleInterfaceAligned', ABackend, False, E.Message);
    end;
  end;
end;

procedure TestContract_DiagnosticsInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LDiag: ISSLDiagnostics;
  LProbeStream: TMemoryStream;
  LHealth: TSSLHealthStatus;
  LPerf: TSSLPerformanceMetrics;
  LInfo: TSSLDiagnosticInfo;
  LExpectedHealthy: Boolean;
begin
  PrintSubHeader(Format('Contract 18: Diagnostics interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if not Supports(LConn, ISSLDiagnostics, LDiag) then
      begin
        WriteLn('  [FAIL] Connection does not expose ISSLDiagnostics');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'Connection does not expose ISSLDiagnostics');
        Exit;
      end;

      LHealth := LDiag.GetHealthStatus;
      LPerf := LDiag.GetPerformanceMetrics;
      LInfo := LDiag.GetDiagnosticInfo;
      LExpectedHealthy := LHealth.IsConnected and LHealth.HandshakeComplete and
        (LHealth.LastError = sslErrNone);

      if LHealth.IsConnected <> LConn.IsConnected then
      begin
        WriteLn('  [FAIL] HealthStatus.IsConnected does not match connection state');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'HealthStatus.IsConnected does not match ISSLConnection.IsConnected');
      end
      else if LPerf.SessionReused <> LConn.IsSessionReused then
      begin
        WriteLn('  [FAIL] PerformanceMetrics.SessionReused does not match connection state');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'PerformanceMetrics.SessionReused does not match ISSLConnection.IsSessionReused');
      end
      else if LDiag.IsHealthy <> LExpectedHealthy then
      begin
        WriteLn('  [FAIL] IsHealthy does not match HealthStatus fields');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'ISSLDiagnostics.IsHealthy does not match HealthStatus-derived expectation');
      end
      else if LInfo.HealthStatus.IsConnected <> LHealth.IsConnected then
      begin
        WriteLn('  [FAIL] DiagnosticInfo.HealthStatus drifted from direct health getter');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'DiagnosticInfo.HealthStatus.IsConnected does not match GetHealthStatus');
      end
      else if LInfo.HealthStatus.HandshakeComplete <> LHealth.HandshakeComplete then
      begin
        WriteLn('  [FAIL] DiagnosticInfo.HandshakeComplete drifted from direct health getter');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'DiagnosticInfo.HealthStatus.HandshakeComplete does not match GetHealthStatus');
      end
      else if LInfo.PerformanceMetrics.TotalBytesTransferred <> LPerf.TotalBytesTransferred then
      begin
        WriteLn('  [FAIL] DiagnosticInfo.PerformanceMetrics drifted from direct metrics getter');
        AddResult('DiagnosticsInterfaceAligned', ABackend, False,
          'DiagnosticInfo.PerformanceMetrics.TotalBytesTransferred does not match GetPerformanceMetrics');
      end
      else
      begin
        WriteLn('  [PASS] Connection diagnostics surface is self-consistent');
        AddResult('DiagnosticsInterfaceAligned', ABackend, True);
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('DiagnosticsInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_ConnectionInfoInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LConnInfoAccess: ISSLConnectionInfo;
  LProbeStream: TMemoryStream;
  LCoreInfo: TSSLConnectionInfo;
  LCoreALPN: string;
  LOptionalInfo: TSSLConnectionInfo;
  LOptionalCtx: ISSLContext;
  LCoreCtx: ISSLContext;
begin
  PrintSubHeader(Format('Contract 19: Connection-info interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if not Supports(LConn, ISSLConnectionInfo, LConnInfoAccess) then
      begin
        WriteLn('  [FAIL] Connection does not expose ISSLConnectionInfo');
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
          'Connection does not expose ISSLConnectionInfo');
        Exit;
      end;

      LOptionalInfo := LConnInfoAccess.GetConnectionInfo;
      LOptionalCtx := LConnInfoAccess.GetContext;

      if LOptionalInfo.ALPNProtocol <> LConnInfoAccess.GetSelectedALPNProtocol then
      begin
        WriteLn('  [FAIL] Optional owner ALPN field drifted from optional ALPN getter');
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
          'ISSLConnectionInfo.GetConnectionInfo.ALPNProtocol does not match ISSLConnectionInfo.GetSelectedALPNProtocol');
      end
      else if Trim(LConnInfoAccess.GetStateString) = '' then
      begin
        WriteLn('  [FAIL] Optional interface state string is empty');
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
          'ISSLConnectionInfo.GetStateString returned an empty string');
      end
      else if LOptionalCtx = nil then
      begin
        WriteLn('  [FAIL] Optional context owner returned nil');
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
          'ISSLConnectionInfo.GetContext returned nil');
      end
      else if LOptionalCtx.GetContextType <> LCtx.GetContextType then
      begin
        WriteLn('  [FAIL] Optional interface context type drifted from creation context');
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
          'ISSLConnectionInfo.GetContext.GetContextType does not match the creation context type');
      end
      else
      begin
        // INTENTIONAL_CORE_SURFACE: keep one direct core GetConnectionInfo read
        // here so Contract 19 continues to prove the compiler-deprecated core
        // mirror stays aligned with the ISSLConnectionInfo owner path.
        {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
        LCoreInfo := LConn.GetConnectionInfo;
        {$POP}
        if LCoreInfo.ProtocolVersion <> LOptionalInfo.ProtocolVersion then
        begin
          WriteLn('  [FAIL] Core GetConnectionInfo mirror drifted from optional owner protocol version');
          AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
            'ISSLConnection.GetConnectionInfo.ProtocolVersion does not mirror ISSLConnectionInfo.GetConnectionInfo');
        end
        else if LCoreInfo.CipherSuite <> LOptionalInfo.CipherSuite then
        begin
          WriteLn('  [FAIL] Core GetConnectionInfo mirror drifted from optional owner cipher suite');
          AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
            'ISSLConnection.GetConnectionInfo.CipherSuite does not mirror ISSLConnectionInfo.GetConnectionInfo');
        end
        else if LCoreInfo.ALPNProtocol <> LOptionalInfo.ALPNProtocol then
        begin
          WriteLn('  [FAIL] Core GetConnectionInfo mirror drifted from optional owner ALPN field');
          AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
            'ISSLConnection.GetConnectionInfo.ALPNProtocol does not mirror ISSLConnectionInfo.GetConnectionInfo');
        end
        else
        begin
          // INTENTIONAL_CORE_SURFACE: keep this direct core GetSelectedALPNProtocol
          // read as the single ALPN mirror proof while the public core declaration
          // is compiler-deprecated in favor of ISSLConnectionInfo.GetSelectedALPNProtocol.
          {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
          LCoreALPN := LConn.GetSelectedALPNProtocol;
          {$POP}
          if LConnInfoAccess.GetSelectedALPNProtocol <> LCoreALPN then
          begin
            WriteLn('  [FAIL] Optional interface ALPN getter drifted from core getter');
            AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
              'ISSLConnectionInfo.GetSelectedALPNProtocol does not match ISSLConnection.GetSelectedALPNProtocol');
          end
          else
          begin
          {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
          if LConnInfoAccess.GetStateString <> LConn.GetStateString then
          {$POP}
          begin
            WriteLn('  [FAIL] Optional interface state string drifted from core getter');
            AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
              'ISSLConnectionInfo.GetStateString does not match ISSLConnection.GetStateString');
          end
          else
          begin
          // INTENTIONAL_CORE_SURFACE: keep this direct core GetContext read as
          // the single mirror-equality proof while the public core declaration
          // is compiler-deprecated in favor of ISSLConnectionInfo.GetContext.
          {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
          LCoreCtx := LConn.GetContext;
          {$POP}
          if LCoreCtx = nil then
          begin
            WriteLn('  [FAIL] Core GetContext mirror returned nil');
            AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
              'ISSLConnection.GetContext mirror returned nil');
          end
          else if LCoreCtx.GetContextType <> LOptionalCtx.GetContextType then
          begin
            WriteLn('  [FAIL] Core GetContext mirror drifted from optional owner');
            AddResult('ConnectionInfoInterfaceAligned', ABackend, False,
              'ISSLConnection.GetContext does not mirror ISSLConnectionInfo.GetContext');
          end
            else
            begin
              WriteLn('  [PASS] Connection-info surface is self-consistent');
              AddResult('ConnectionInfoInterfaceAligned', ABackend, True);
            end;
          end;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('ConnectionInfoInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_SessionResumptionInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSessionResumption: ISSLSessionResumption;
  LProbeStream: TMemoryStream;
  LCoreSession: ISSLSession;
  LOptionalSession: ISSLSession;
  LCoreSessionCert: ISSLCertificate;
  LOptionalSessionCert: ISSLCertificate;
  LCoreInfo: TSSLConnectionInfo;
begin
  PrintSubHeader(Format('Contract 20: Session-resumption interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if not Supports(LConn, ISSLSessionResumption, LSessionResumption) then
      begin
        WriteLn('  [FAIL] Connection does not expose ISSLSessionResumption');
        AddResult('SessionResumptionInterfaceAligned', ABackend, False,
          'Connection does not expose ISSLSessionResumption');
        Exit;
      end;

      LCoreSession := LConn.GetSession;
      LOptionalSession := LSessionResumption.GetSession;

      if (LCoreSession = nil) <> (LOptionalSession = nil) then
      begin
        WriteLn('  [FAIL] Optional interface session getter drifted from core getter');
        AddResult('SessionResumptionInterfaceAligned', ABackend, False,
          'ISSLSessionResumption.GetSession nil/non-nil result does not match ISSLConnection.GetSession');
      end
      else if LSessionResumption.IsSessionReused <> LConn.IsSessionReused then
      begin
        WriteLn('  [FAIL] Optional interface reused flag drifted from core getter');
        AddResult('SessionResumptionInterfaceAligned', ABackend, False,
          'ISSLSessionResumption.IsSessionReused does not match ISSLConnection.IsSessionReused');
      end
      else
      begin
        // INTENTIONAL_CORE_SURFACE: keep this direct core GetConnectionInfo read
        // as a resumed-flag mirror proof while the public source declaration is
        // compiler-deprecated.
        {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
        LCoreInfo := LConn.GetConnectionInfo;
        {$POP}

        if LCoreInfo.IsResumed <> LConn.IsSessionReused then
        begin
          WriteLn('  [FAIL] Connection-info resumed flag drifted from session getter');
          AddResult('SessionResumptionInterfaceAligned', ABackend, False,
            'ISSLConnection.GetConnectionInfo.IsResumed does not match ISSLConnection.IsSessionReused');
        end
        else if LCoreSession <> nil then
        begin
          LCoreSessionCert := LCoreSession.GetPeerCertificate;
          LOptionalSessionCert := LOptionalSession.GetPeerCertificate;

          if LOptionalSession.IsValid <> LCoreSession.IsValid then
          begin
            WriteLn('  [FAIL] Optional interface session validity drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.IsValid does not match ISSLConnection.GetSession');
          end
          else if LOptionalSession.IsResumable <> LCoreSession.IsResumable then
          begin
            WriteLn('  [FAIL] Optional interface session resumable flag drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.IsResumable does not match ISSLConnection.GetSession');
          end
          else if LOptionalSession.GetProtocolVersion <> LCoreSession.GetProtocolVersion then
          begin
            WriteLn('  [FAIL] Optional interface session protocol drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.GetProtocolVersion does not match ISSLConnection.GetSession');
          end
          else if LOptionalSession.GetCipherName <> LCoreSession.GetCipherName then
          begin
            WriteLn('  [FAIL] Optional interface session cipher drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.GetCipherName does not match ISSLConnection.GetSession');
          end
          else if LOptionalSession.GetTimeout <> LCoreSession.GetTimeout then
          begin
            WriteLn('  [FAIL] Optional interface session timeout drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.GetTimeout does not match ISSLConnection.GetSession');
          end
          else if (LOptionalSessionCert = nil) <> (LCoreSessionCert = nil) then
          begin
            WriteLn('  [FAIL] Optional interface session peer certificate presence drifted from core getter');
            AddResult('SessionResumptionInterfaceAligned', ABackend, False,
              'ISSLSessionResumption.GetSession.GetPeerCertificate nil/non-nil result does not match ISSLConnection.GetSession');
          end
          else
          begin
            WriteLn('  [PASS] Session-resumption surface is self-consistent');
            AddResult('SessionResumptionInterfaceAligned', ABackend, True);
          end;
        end
        else
        begin
          WriteLn('  [PASS] Session-resumption surface is self-consistent');
          AddResult('SessionResumptionInterfaceAligned', ABackend, True);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('SessionResumptionInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure TestContract_CertificateVerificationInterfaceAligned(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LCertVerify: ISSLCertificateVerification;
  LProbeStream: TMemoryStream;
  LCoreChain: TSSLCertificateArray;
  LOptionalChain: TSSLCertificateArray;
  LCoreIssuerCert: ISSLCertificate;
  LOptionalIssuerCert: ISSLCertificate;
  I: Integer;
begin
  PrintSubHeader(Format('Contract 21: Certificate-verification interface alignment - %s',
    [SSL_LIBRARY_NAMES[ABackend]]));

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    AddSkip('Backend not available on this platform');
    Exit;
  end;

  LProbeStream := TMemoryStream.Create;
  try
    try
      LLib := TSSLFactory.GetLibrary(ABackend);
      LCtx := LLib.CreateContext(sslCtxClient);
      LConn := LCtx.CreateConnection(LProbeStream);

      if not Supports(LConn, ISSLCertificateVerification, LCertVerify) then
      begin
        WriteLn('  [FAIL] Connection does not expose ISSLCertificateVerification');
        AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
          'Connection does not expose ISSLCertificateVerification');
        Exit;
      end;

      if LCertVerify.GetVerifyResult <> LConn.GetVerifyResult then
      begin
        WriteLn('  [FAIL] Optional interface verify result drifted from core getter');
        AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
          'ISSLCertificateVerification.GetVerifyResult does not match ISSLConnection.GetVerifyResult');
        Exit;
      end;

      if LCertVerify.GetVerifyResultString <> LConn.GetVerifyResultString then
      begin
        WriteLn('  [FAIL] Optional interface verify result string drifted from core getter');
        AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
          'ISSLCertificateVerification.GetVerifyResultString does not match ISSLConnection.GetVerifyResultString');
        Exit;
      end;

      LCoreChain := LConn.GetPeerCertificateChain;
      LOptionalChain := LCertVerify.GetPeerCertificateChain;

      if Length(LOptionalChain) <> Length(LCoreChain) then
      begin
        WriteLn('  [FAIL] Optional interface peer certificate chain length drifted from core getter');
        AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
          'ISSLCertificateVerification.GetPeerCertificateChain length does not match ISSLConnection.GetPeerCertificateChain');
        Exit;
      end;

      for I := 0 to High(LCoreChain) do
      begin
        if (LOptionalChain[I] = nil) <> (LCoreChain[I] = nil) then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate chain nilness drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain nil/non-nil result does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;

        if LCoreChain[I] = nil then
          Continue;

        if LOptionalChain[I].GetSubject <> LCoreChain[I].GetSubject then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate subject drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain subject does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;

        if LOptionalChain[I].GetIssuer <> LCoreChain[I].GetIssuer then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate issuer drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain issuer does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;

        if LOptionalChain[I].GetSerialNumber <> LCoreChain[I].GetSerialNumber then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate serial drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain serial does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;

        LCoreIssuerCert := LCoreChain[I].GetIssuerCertificate;
        LOptionalIssuerCert := LOptionalChain[I].GetIssuerCertificate;

        if (LOptionalIssuerCert = nil) <> (LCoreIssuerCert = nil) then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate issuer-link nilness drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain issuer-link nil/non-nil result does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;

        if (LCoreIssuerCert <> nil) and
           (not CertificatePublicIdentityMatches(LOptionalIssuerCert, LCoreIssuerCert)) then
        begin
          WriteLn('  [FAIL] Optional interface peer certificate issuer-link truth drifted from core getter');
          AddResult('CertificateVerificationInterfaceAligned', ABackend, False,
            'ISSLCertificateVerification.GetPeerCertificateChain issuer-link certificate identity does not match ISSLConnection.GetPeerCertificateChain');
          Exit;
        end;
      end;

      WriteLn('  [PASS] Certificate-verification surface is self-consistent');
      AddResult('CertificateVerificationInterfaceAligned', ABackend, True);
    except
      on E: Exception do
      begin
        WriteLn('  [FAIL] Exception: ', E.ClassName, ' - ', E.Message);
        AddResult('CertificateVerificationInterfaceAligned', ABackend, False, E.Message);
      end;
    end;
  finally
    LProbeStream.Free;
  end;
end;

procedure PrintSummary;
var
  I: Integer;
  LPassRate: Double;
begin
  PrintHeader('Contract Test Summary');

  WriteLn;
  WriteLn(Format('Total Tests: %d', [GPassCount + GFailCount + GSkipCount]));
  WriteLn(Format('Passed: %d', [GPassCount]));
  WriteLn(Format('Failed: %d', [GFailCount]));
  WriteLn(Format('Skipped: %d', [GSkipCount]));

  if (GPassCount + GFailCount + GSkipCount) > 0 then
    LPassRate := GPassCount / (GPassCount + GFailCount + GSkipCount) * 100
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

  TestHarness_DirectInstanceCoverage_FreePascal;

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

    // 7) Unsupported optional interfaces must not lie via Supports(...)
    TestContract_UnsupportedOptionalInterfacesAbsent(LBackend);

    // 8) SNI-capable backends must expose the per-connection client interface
    TestContract_ClientConnectionSNIInterfaceAligned(LBackend);

    // 9) CT-capable backends must expose non-stub CT optional interfaces
    TestContract_ClientConnectionCTInterfaceAligned(LBackend);

    // 10) OCSP-capable backends must expose non-stub OCSP optional interface
    TestContract_ClientConnectionOCSPInterfaceAligned(LBackend);

    // 11) C-library backend connections must expose native-handle interface
    TestContract_ConnectionNativeHandleInterfaceAligned(LBackend);

    // 12) Context optional surfaces must match backend capability truth
    TestContract_ContextOptionalInterfacesAligned(LBackend);

    // 13) C-library backend contexts must expose native-handle interface
    TestContract_ContextNativeHandleInterfaceAligned(LBackend);

    // 14) HTTP-hooks-capable backends must expose context hook surface
    TestContract_ContextHTTPHooksInterfaceAligned(LBackend);

    // 15) Session wrappers must expose native-handle truth consistently
    TestContract_SessionNativeHandleInterfaceAligned(LBackend);

    // 16) Certificate wrappers must expose native-handle truth consistently
    TestContract_CertificateNativeHandleInterfaceAligned(LBackend);

    // 17) Certificate-store wrappers must expose native-handle truth consistently
    TestContract_CertificateStoreNativeHandleInterfaceAligned(LBackend);

    // 18) Connection diagnostics surface must stay exposed and self-consistent
    TestContract_DiagnosticsInterfaceAligned(LBackend);

    // 19) Connection-info optional surface must stay exposed and self-consistent
    TestContract_ConnectionInfoInterfaceAligned(LBackend);

    // 20) Session-resumption optional surface must stay exposed and self-consistent
    TestContract_SessionResumptionInterfaceAligned(LBackend);

    // 21) Certificate-verification optional surface must stay exposed and self-consistent
    TestContract_CertificateVerificationInterfaceAligned(LBackend);
  end;

  PrintSummary;

  // 返回退出码
  if GFailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
