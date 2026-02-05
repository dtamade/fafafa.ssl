program test_capability_matrix_v12;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory;

procedure TestBackendCapabilities(const ABackendName: string; AType: TSSLLibraryType);
var
  Lib: ISSLLibrary;
  Caps: TSSLBackendCapabilities;
  Desc: string;
begin
  WriteLn('========================================');
  WriteLn('Testing: ', ABackendName);
  WriteLn('========================================');

  try
    Lib := TSSLFactory.GetLibrary(AType);
    if not Assigned(Lib) then
    begin
      WriteLn('  [SKIP] Backend not available');
      WriteLn;
      Exit;
    end;

    Caps := Lib.GetCapabilities;

    // v1.1.0 字段（向后兼容性验证）
    WriteLn('[v1.1.0 Fields - Backward Compatibility]');
    WriteLn('  SupportsTLS13: ', Caps.SupportsTLS13);
    WriteLn('  SupportsALPN: ', Caps.SupportsALPN);
    WriteLn('  SupportsSNI: ', Caps.SupportsSNI);
    WriteLn('  SupportsOCSPStapling: ', Caps.SupportsOCSPStapling);
    WriteLn('  SupportsECDHE: ', Caps.SupportsECDHE);
    WriteLn('  MinTLSVersion: ', Ord(Caps.MinTLSVersion));
    WriteLn('  MaxTLSVersion: ', Ord(Caps.MaxTLSVersion));
    WriteLn;

    // v1.2.0 新字段
    WriteLn('[v1.2.0 New Fields]');
    WriteLn('  BackendType: ', Ord(Caps.BackendType));
    WriteLn('  BackendImplType: ', Ord(Caps.BackendImplType));
    WriteLn('  BackendVersion: ', Caps.BackendVersion);
    WriteLn('  SupportsDTLS: ', Caps.SupportsDTLS);
    WriteLn;

    // 功能支持级别
    WriteLn('[Feature Support Levels]');
    WriteLn('  SNISupport: ', Ord(Caps.SNISupport));
    WriteLn('  ALPNSupport: ', Ord(Caps.ALPNSupport));
    WriteLn('  OCSPStaplingSupport: ', Ord(Caps.OCSPStaplingSupport));
    WriteLn('  CertTransparencySupport: ', Ord(Caps.CertTransparencySupport));
    WriteLn('  SessionTicketsSupport: ', Ord(Caps.SessionTicketsSupport));
    WriteLn;

    // 算法支持
    WriteLn('[Algorithm Support]');
    WriteLn('  Ciphers: ', IsCipherSupported(Caps, sslCipherAES256), ' (AES256), ',
            IsCipherSupported(Caps, sslCipherCHACHA20_POLY1305), ' (ChaCha20)');
    WriteLn('  Hashes: ', IsHashSupported(Caps, sslHashSHA256), ' (SHA256), ',
            IsHashSupported(Caps, sslHashSHA512), ' (SHA512)');
    WriteLn('  KeyExchange: ', IsKeyExchangeSupported(Caps, sslKexECDHE_RSA), ' (ECDHE-RSA)');
    WriteLn;

    // 性能和安全特性
    WriteLn('[Performance & Security Features]');
    WriteLn('  HasHardwareAcceleration: ', Caps.HasHardwareAcceleration);
    WriteLn('  HasSIMDOptimization: ', Caps.HasSIMDOptimization);
    WriteLn('  HasConstantTimeOperations: ', Caps.HasConstantTimeOperations);
    WriteLn('  SupportsFIPSMode: ', Caps.SupportsFIPSMode);
    WriteLn;

    // 平台特性
    WriteLn('[Platform Features]');
    WriteLn('  RequiresExternalLibrary: ', Caps.RequiresExternalLibrary);
    WriteLn('  SupportsSystemCertStore: ', Caps.SupportsSystemCertStore);
    WriteLn('  SupportsPKCS11: ', Caps.SupportsPKCS11);
    WriteLn('  SupportsTPM: ', Caps.SupportsTPM);
    WriteLn;

    // 辅助查询函数
    WriteLn('[Helper Function Tests]');
    WriteLn('  IsNativeBackend: ', IsNativeBackend(Caps));
    WriteLn('  IsCLibraryBackend: ', IsCLibraryBackend(Caps));
    WriteLn('  RequiresExternalDependencies: ', RequiresExternalDependencies(Caps));
    WriteLn('  SecurityScore: ', GetSecurityScore(Caps), '/100');
    WriteLn('  PerformanceScore: ', GetPerformanceScore(Caps), '/100');
    WriteLn;

    // 完整描述
    WriteLn('[Capabilities Description]');
    Desc := GetCapabilitiesDescription(Caps);
    WriteLn(Desc);
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn('  [ERROR] ', E.ClassName, ': ', E.Message);
      WriteLn;
    end;
  end;
end;

begin
  WriteLn('fafafa.ssl - Capability Matrix v1.2.0 Test');
  WriteLn('==========================================');
  WriteLn;

  // 测试所有后端
  TestBackendCapabilities('OpenSSL', sslOpenSSL);
  TestBackendCapabilities('WolfSSL', sslWolfSSL);
  TestBackendCapabilities('MbedTLS', sslMbedTLS);
  TestBackendCapabilities('WinSSL', sslWinSSL);

  WriteLn('========================================');
  WriteLn('All tests completed!');
  WriteLn('========================================');
end.
