{**
 * Test: Hardware Acceleration Framework
 * Purpose: Verify CPU feature detection and hardware acceleration capabilities
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

program test_hardware_acceleration;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.hardware;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;

procedure Test(const AName: string; ACondition: Boolean);
begin
  Inc(GTestCount);
  Write(AName, ': ');
  if ACondition then
  begin
    WriteLn('PASS');
    Inc(GPassCount);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(GFailCount);
  end;
end;

procedure TestCPUFeatureDetection;
var
  LFeatures: TCPUFeatures;
  LVendor: string;
begin
  WriteLn('');
  WriteLn('=== CPU Feature Detection ===');

  LFeatures := DetectCPUFeatures;
  LVendor := GetCPUVendor;

  WriteLn('  CPU Vendor: ', LVendor);
  Test('CPU vendor detected', LVendor <> '');
  Test('CPU vendor not Unknown', LVendor <> 'Unknown');

  // 显示检测到的特性
  WriteLn('  Detected features:');
  if cpuAESNI in LFeatures then WriteLn('    - AES-NI');
  if cpuSHANI in LFeatures then WriteLn('    - SHA-NI');
  if cpuAVX in LFeatures then WriteLn('    - AVX');
  if cpuAVX2 in LFeatures then WriteLn('    - AVX2');
  if cpuAVX512 in LFeatures then WriteLn('    - AVX-512');
  if cpuRDRAND in LFeatures then WriteLn('    - RDRAND');
  if cpuRDSEED in LFeatures then WriteLn('    - RDSEED');
  if cpuPCLMULQDQ in LFeatures then WriteLn('    - PCLMULQDQ');
  if cpuARMCrypto in LFeatures then WriteLn('    - ARM Crypto');
  if cpuARMNEON in LFeatures then WriteLn('    - ARM NEON');

  // 测试便捷函数
  Test('HasAESNI returns consistent result', HasAESNI = (cpuAESNI in LFeatures));
  Test('HasSHANI returns consistent result', HasSHANI = (cpuSHANI in LFeatures));
  Test('HasRDRAND returns consistent result', HasRDRAND = (cpuRDRAND in LFeatures));
  Test('HasAVX returns consistent result', HasAVX = (cpuAVX in LFeatures));
  Test('HasAVX2 returns consistent result', HasAVX2 = (cpuAVX2 in LFeatures));
end;

procedure TestCipherSuiteRecommendation;
var
  LCipherSuites: string;
begin
  WriteLn('');
  WriteLn('=== Cipher Suite Recommendation ===');

  LCipherSuites := GetRecommendedCipherSuites;
  Test('Cipher suites not empty', LCipherSuites <> '');

  WriteLn('  Recommended cipher suites:');
  WriteLn('    ', LCipherSuites);

  // 验证推荐逻辑
  if HasAESNI then
  begin
    Test('AES-GCM prioritized with AES-NI', Pos('AES_256_GCM', LCipherSuites) < Pos('CHACHA20', LCipherSuites));
    WriteLn('  (AES-NI detected: AES-GCM prioritized)');
  end
  else
  begin
    Test('ChaCha20 prioritized without AES-NI', Pos('CHACHA20', LCipherSuites) < Pos('AES_256_GCM', LCipherSuites));
    WriteLn('  (No AES-NI: ChaCha20 prioritized)');
  end;
end;

procedure TestProviderConfig;
var
  LConfig: TProviderConfig;
begin
  WriteLn('');
  WriteLn('=== Provider Configuration ===');

  LConfig := CreateProviderConfig(provDefault);
  Test('Default provider name is "default"', LConfig.Name = 'default');
  Test('Default provider type is provDefault', LConfig.ProviderType = provDefault);

  LConfig := CreateProviderConfig(provLegacy);
  Test('Legacy provider name is "legacy"', LConfig.Name = 'legacy');

  LConfig := CreateProviderConfig(provFIPS);
  Test('FIPS provider name is "fips"', LConfig.Name = 'fips');

  LConfig := CreateProviderConfig(provCustom, 'my_provider');
  Test('Custom provider name is "my_provider"', LConfig.Name = 'my_provider');
end;

procedure TestHardwareKeyRef;
var
  LKeyRef: THardwareKeyRef;
begin
  WriteLn('');
  WriteLn('=== Hardware Key Reference ===');

  LKeyRef := CreateHardwareKeyRef(hksPKCS11, 'slot0-key1', 0);
  Test('Key store type is PKCS11', LKeyRef.StoreType = hksPKCS11);
  Test('Key ID is "slot0-key1"', LKeyRef.KeyID = 'slot0-key1');
  Test('Slot ID is 0', LKeyRef.SlotID = 0);
  Test('Default is private key', LKeyRef.IsPrivate = True);

  LKeyRef := CreateHardwareKeyRef(hksTPM, 'tpm-key', 1);
  Test('TPM key store type', LKeyRef.StoreType = hksTPM);
  Test('TPM slot ID is 1', LKeyRef.SlotID = 1);
end;

procedure TestHardwareCapabilitiesRecord;
var
  LCaps: THardwareCapabilities;
begin
  WriteLn('');
  WriteLn('=== Hardware Capabilities Record ===');

  FillChar(LCaps, SizeOf(LCaps), 0);

  // 测试记录字段
  LCaps.CPUFeatures := [cpuAESNI, cpuAVX];
  Test('CPUFeatures can store AES-NI', cpuAESNI in LCaps.CPUFeatures);
  Test('CPUFeatures can store AVX', cpuAVX in LCaps.CPUFeatures);

  LCaps.KeyStoreTypes := [hksPKCS11, hksEngine];
  Test('KeyStoreTypes can store PKCS11', hksPKCS11 in LCaps.KeyStoreTypes);
  Test('KeyStoreTypes can store Engine', hksEngine in LCaps.KeyStoreTypes);

  LCaps.AvailableProviders := [provDefault, provLegacy];
  Test('AvailableProviders can store default', provDefault in LCaps.AvailableProviders);
  Test('AvailableProviders can store legacy', provLegacy in LCaps.AvailableProviders);

  LCaps.HasHardwareAES := True;
  LCaps.AESSpeedup := 3.5;
  Test('HasHardwareAES can be set', LCaps.HasHardwareAES = True);
  Test('AESSpeedup can be set', Abs(LCaps.AESSpeedup - 3.5) < 0.001);
end;

procedure PrintSummary;
var
  LPassRate: Double;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Hardware Acceleration Test Summary');
  WriteLn('========================================');
  WriteLn(Format('Total:  %d', [GTestCount]));
  WriteLn(Format('Passed: %d', [GPassCount]));
  WriteLn(Format('Failed: %d', [GFailCount]));

  if GTestCount > 0 then
    LPassRate := (GPassCount / GTestCount) * 100
  else
    LPassRate := 0;

  WriteLn(Format('Rate:   %.1f%%', [LPassRate]));
  WriteLn('========================================');
end;

begin
  WriteLn('Hardware Acceleration Framework Tests');
  WriteLn('=====================================');
  WriteLn('Testing CPU feature detection and hardware capabilities');

  TestCPUFeatureDetection;
  TestCipherSuiteRecommendation;
  TestProviderConfig;
  TestHardwareKeyRef;
  TestHardwareCapabilitiesRecord;

  PrintSummary;

  if GFailCount > 0 then
    Halt(1);
end.
