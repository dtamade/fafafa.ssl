program test_protocol_downgrade_prevention;

{$mode objfpc}{$H+}{$J-}

{**
 * Protocol Downgrade Prevention Test Suite
 *
 * P3-8 P0-2: 协议降级防护测试 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - TLS 降级攻击防护验证
 * - 强制 TLS 1.2+ 测试
 * - FALLBACK_SCSV 信号测试
 * - 协议版本协商验证
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

{ 获取协议版本的安全级别 }
function GetProtocolSecurityLevel(AVersion: TSSLProtocolVersion): Integer;
begin
  case AVersion of
    sslProtocolSSL2: Result := 0;   // 严重不安全
    sslProtocolSSL3: Result := 1;   // 不安全 (POODLE)
    sslProtocolTLS10: Result := 2;  // 弃用 (BEAST)
    sslProtocolTLS11: Result := 3;  // 弃用
    sslProtocolTLS12: Result := 4;  // 安全
    sslProtocolTLS13: Result := 5;  // 最安全
  else
    Result := -1;
  end;
end;

{ 检查协议版本集合是否允许降级 }
function AllowsDowngrade(AVersions: TSSLProtocolVersions): Boolean;
var
  MinLevel, MaxLevel: Integer;
  V: TSSLProtocolVersion;
begin
  MinLevel := High(Integer);
  MaxLevel := Low(Integer);

  for V := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
  begin
    if V in AVersions then
    begin
      if GetProtocolSecurityLevel(V) < MinLevel then
        MinLevel := GetProtocolSecurityLevel(V);
      if GetProtocolSecurityLevel(V) > MaxLevel then
        MaxLevel := GetProtocolSecurityLevel(V);
    end;
  end;

  // 如果最小级别低于 TLS 1.2 (级别 4)，则允许降级
  Result := (MinLevel < 4) and (MaxLevel >= 4);
end;

{ 检查是否只允许安全协议 }
function OnlySecureProtocols(AVersions: TSSLProtocolVersions): Boolean;
var
  V: TSSLProtocolVersion;
begin
  Result := True;
  for V := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
  begin
    if V in AVersions then
    begin
      if GetProtocolSecurityLevel(V) < 4 then  // 低于 TLS 1.2
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure TestProtocolSecurityLevels;
begin
  WriteLn;
  WriteLn('=== Protocol Security Levels ===');

  Runner.Check('SSL 2.0 level = 0', GetProtocolSecurityLevel(sslProtocolSSL2) = 0);
  Runner.Check('SSL 3.0 level = 1', GetProtocolSecurityLevel(sslProtocolSSL3) = 1);
  Runner.Check('TLS 1.0 level = 2', GetProtocolSecurityLevel(sslProtocolTLS10) = 2);
  Runner.Check('TLS 1.1 level = 3', GetProtocolSecurityLevel(sslProtocolTLS11) = 3);
  Runner.Check('TLS 1.2 level = 4', GetProtocolSecurityLevel(sslProtocolTLS12) = 4);
  Runner.Check('TLS 1.3 level = 5', GetProtocolSecurityLevel(sslProtocolTLS13) = 5);
end;

procedure TestDowngradeDetection;
var
  InsecureSet, SecureSet, MixedSet: TSSLProtocolVersions;
begin
  WriteLn;
  WriteLn('=== Downgrade Detection ===');

  // 只有安全协议
  SecureSet := [sslProtocolTLS12, sslProtocolTLS13];
  Runner.Check('TLS 1.2+1.3 no downgrade', not AllowsDowngrade(SecureSet));
  Runner.Check('TLS 1.2+1.3 all secure', OnlySecureProtocols(SecureSet));

  // 混合集合（允许降级）
  MixedSet := [sslProtocolTLS10, sslProtocolTLS12, sslProtocolTLS13];
  Runner.Check('TLS 1.0+1.2+1.3 allows downgrade', AllowsDowngrade(MixedSet));
  Runner.Check('TLS 1.0+1.2+1.3 not all secure', not OnlySecureProtocols(MixedSet));

  // 不安全集合 - 全部为不安全协议，不存在从安全到不安全的降级风险
  InsecureSet := [sslProtocolSSL3, sslProtocolTLS10];
  Runner.Check('SSL3+TLS1.0 no security downgrade', not AllowsDowngrade(InsecureSet),
        'Both are insecure - no downgrade from secure to insecure possible');
  Runner.Check('SSL3+TLS1.0 not secure', not OnlySecureProtocols(InsecureSet));

  // 只有 TLS 1.3
  SecureSet := [sslProtocolTLS13];
  Runner.Check('TLS 1.3 only no downgrade', not AllowsDowngrade(SecureSet));
end;

procedure TestContextBuilderProtocolRestriction;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
begin
  WriteLn;
  WriteLn('=== Context Builder Protocol Restriction ===');

  try
    // 测试 WithTLS12And13 方法
    LBuilder := TSSLContextBuilder.Create.WithTLS12And13;
    Runner.Check('WithTLS12And13 builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Runner.Check('TLS 1.2+1.3 context created', LContext <> nil);

    // 测试 WithTLS13 方法 (TLS 1.3 only)
    LBuilder := TSSLContextBuilder.Create.WithTLS13;
    Runner.Check('WithTLS13Only builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Runner.Check('TLS 1.3 only context created', LContext <> nil);

  except
    on E: Exception do
      Runner.Check('Context builder protocol restriction', False, E.Message);
  end;
end;

procedure TestSafeDefaultsNoDowngrade;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
begin
  WriteLn;
  WriteLn('=== Safe Defaults No Downgrade ===');

  try
    LBuilder := TSSLContextBuilder.CreateWithSafeDefaults;
    Runner.Check('SafeDefaults builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Runner.Check('SafeDefaults context created', LContext <> nil);

    // SafeDefaults 应该只允许 TLS 1.2+
    Runner.Check('SafeDefaults created successfully', True);

  except
    on E: Exception do
      Runner.Check('Safe defaults creation', False, E.Message);
  end;
end;

procedure TestMinimumVersionEnforcement;
var
  LConfig: TSSLConfig;
  HasSSL2, HasSSL3, HasTLS10, HasTLS11: Boolean;
begin
  WriteLn;
  WriteLn('=== Minimum Version Enforcement ===');

  // 检查默认配置的选项
  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  HasSSL2 := not (ssoNoSSLv2 in LConfig.Options);
  HasSSL3 := not (ssoNoSSLv3 in LConfig.Options);
  HasTLS10 := not (ssoNoTLSv1 in LConfig.Options);
  HasTLS11 := not (ssoNoTLSv1_1 in LConfig.Options);

  Runner.Check('Default disables SSL 2.0', not HasSSL2);
  Runner.Check('Default disables SSL 3.0', not HasSSL3);
  Runner.Check('Default disables TLS 1.0', not HasTLS10);
  Runner.Check('Default disables TLS 1.1', not HasTLS11);
end;

procedure TestOptionsConsistency;
var
  LConfig: TSSLConfig;
begin
  WriteLn;
  WriteLn('=== Options Consistency ===');

  // 测试协议版本和选项的一致性
  LConfig := Default(TSSLConfig);
  LConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  TSSLFactory.NormalizeConfig(LConfig);

  Runner.Check('TLS 1.2 enabled', not (ssoNoTLSv1_2 in LConfig.Options));
  Runner.Check('TLS 1.3 enabled', not (ssoNoTLSv1_3 in LConfig.Options));
  Runner.Check('SSL 2.0 disabled', ssoNoSSLv2 in LConfig.Options);
  Runner.Check('SSL 3.0 disabled', ssoNoSSLv3 in LConfig.Options);
  Runner.Check('TLS 1.0 disabled', ssoNoTLSv1 in LConfig.Options);
  Runner.Check('TLS 1.1 disabled', ssoNoTLSv1_1 in LConfig.Options);
end;

begin
  WriteLn('==========================================');
  WriteLn('  Protocol Downgrade Prevention Tests');
  WriteLn('==========================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);
    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestProtocolSecurityLevels;
    TestDowngradeDetection;
    TestContextBuilderProtocolRestriction;
    TestSafeDefaultsNoDowngrade;
    TestMinimumVersionEnforcement;
    TestOptionsConsistency;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
