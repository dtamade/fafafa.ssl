{**
 * Program: test_capabilities_contract
 * Purpose: 跨后端 GetCapabilities 运行时契约测试
 *
 * 验证所有可用后端 GetCapabilities 返回合理值：
 * - MaxTLSVersion >= TLS12
 * - MinTLSVersion <= MaxTLSVersion
 * - SupportsSNI / SupportsALPN 为 True（主要后端）
 * - BackendType 与后端一致
 * - Capability 字段完整性（无零值异常）
 *}

program test_capabilities_contract;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.errors,
  fafafa.ssl.freepascal.lib,
  fafafa.ssl.freepascal.session
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.openssl.base
  , fafafa.ssl.openssl.api.core
  , fafafa.ssl.openssl.session
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.mbedtls.base
  , fafafa.ssl.mbedtls.session
  , fafafa.ssl.wolfssl.lib
  , fafafa.ssl.wolfssl.base
  , fafafa.ssl.wolfssl.session
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.winssl.lib
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  , fafafa.ssl.winssl.connection
  {$ENDIF}
  ;

const
  { 主要后端：应支持 SNI、ALPN、ECDHE }
  MajorSNIWatchBackends: set of TSSLLibraryType =
    [sslOpenSSL, sslWolfSSL, sslMbedTLS, sslFreePascal
     {$IFDEF WINDOWS}, sslWinSSL{$ENDIF}];
  MajorECDHEBackends: set of TSSLLibraryType =
    [sslOpenSSL, sslWolfSSL, sslMbedTLS
     {$IFDEF WINDOWS}, sslWinSSL{$ENDIF}];

var
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;

procedure WriteResult(const AName: string; APassed: Boolean;
  const ADetail: string = '');
begin
  if APassed then
  begin
    Inc(GPassCount);
    WriteLn('  [PASS] ', AName);
  end
  else
  begin
    Inc(GFailCount);
    WriteLn('  [FAIL] ', AName, ' - ', ADetail);
  end;
end;

procedure WriteSkip(const AReason: string);
begin
  Inc(GSkipCount);
  WriteLn('  [SKIP] ', AReason);
end;

procedure TestCapabilities(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
  LName: string;
begin
  LName := SSL_LIBRARY_NAMES[ABackend];
  WriteLn;
  WriteLn('--- Testing: ', LName, ' ---');

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteSkip(LName + ' not available on this platform');
    Exit;
  end;

  try
    LLib := TSSLFactory.GetLibrary(ABackend);
  except
    on E: Exception do
    begin
      WriteSkip(LName + ' library creation failed: ' + E.Message);
      Exit;
    end;
  end;

  LCaps := LLib.GetCapabilities;

  // 1. BackendType matches
  WriteResult('BackendType matches',
    LCaps.BackendType = ABackend,
    Format('Expected %d, got %d', [Ord(ABackend), Ord(LCaps.BackendType)]));

  // 2. MaxTLSVersion >= TLS12
  WriteResult('MaxTLSVersion >= TLS12',
    Ord(LCaps.MaxTLSVersion) >= Ord(sslProtocolTLS12),
    Format('MaxTLS=%d (TLS12=%d)', [Ord(LCaps.MaxTLSVersion), Ord(sslProtocolTLS12)]));

  // 3. MinTLSVersion <= MaxTLSVersion
  WriteResult('MinTLSVersion <= MaxTLSVersion',
    Ord(LCaps.MinTLSVersion) <= Ord(LCaps.MaxTLSVersion),
    Format('Min=%d, Max=%d', [Ord(LCaps.MinTLSVersion), Ord(LCaps.MaxTLSVersion)]));

  // 4. SNI support (major backends should support SNI)
  if ABackend in MajorSNIWatchBackends then
    WriteResult('SupportsSNI = True',
      LCaps.SupportsSNI,
      Format('SupportsSNI=%s', [BoolToStr(LCaps.SupportsSNI, True)]));

  // 5. ALPN support (major backends should support ALPN)
  if ABackend in MajorSNIWatchBackends then
    WriteResult('SupportsALPN = True',
      LCaps.SupportsALPN,
      Format('SupportsALPN=%s', [BoolToStr(LCaps.SupportsALPN, True)]));

  // 6. BackendVersion non-empty and not placeholder
  WriteResult('BackendVersion non-empty',
    (LCaps.BackendVersion <> '') and
    (LowerCase(LCaps.BackendVersion) <> 'unknown') and
    (LowerCase(LCaps.BackendVersion) <> 'n/a'),
    'BackendVersion is empty or placeholder');

  // 7. BackendImplType in valid enum range (robust to future extension)
  WriteResult('BackendImplType valid',
    (Ord(LCaps.BackendImplType) >= Ord(Low(TSSLBackendImplType))) and
    (Ord(LCaps.BackendImplType) <= Ord(High(TSSLBackendImplType))),
    Format('BackendImplType=%d', [Ord(LCaps.BackendImplType)]));

  // 8. CompatibilityLevel > 0
  WriteResult('CompatibilityLevel > 0',
    LCaps.CompatibilityLevel > 0,
    Format('CompatibilityLevel=%d', [LCaps.CompatibilityLevel]));

  // 9. ECDHE support (modern backends)
  if ABackend in MajorECDHEBackends then
    WriteResult('SupportsECDHE = True',
      LCaps.SupportsECDHE,
      Format('SupportsECDHE=%s', [BoolToStr(LCaps.SupportsECDHE, True)]));

  // 10. Feature support levels in valid range
  WriteResult('SNISupport level valid',
    (Ord(LCaps.SNISupport) >= Ord(Low(TSSLFeatureSupportLevel))) and
    (Ord(LCaps.SNISupport) <= Ord(High(TSSLFeatureSupportLevel))),
    Format('SNISupport=%d', [Ord(LCaps.SNISupport)]));

  WriteResult('ALPNSupport level valid',
    (Ord(LCaps.ALPNSupport) >= Ord(Low(TSSLFeatureSupportLevel))) and
    (Ord(LCaps.ALPNSupport) <= Ord(High(TSSLFeatureSupportLevel))),
    Format('ALPNSupport=%d', [Ord(LCaps.ALPNSupport)]));
end;

var
  LBackend: TSSLLibraryType;
begin
  WriteLn('========================================');
  WriteLn(' Cross-Backend GetCapabilities Contract');
  WriteLn('========================================');

  for LBackend := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if LBackend = sslAutoDetect then
      Continue;
    TestCapabilities(LBackend);
  end;

  WriteLn;
  WriteLn('========================================');
  WriteLn(Format(' Results: %d passed, %d failed, %d skipped',
    [GPassCount, GFailCount, GSkipCount]));
  WriteLn('========================================');

  if GFailCount > 0 then
    Halt(1);
end.
