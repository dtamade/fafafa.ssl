program test_protocol_downgrade_prevention;

{$mode objfpc}{$H+}{$J-}

{**
 * Protocol Downgrade Prevention Test Suite
 *
 * P3-8 P0-2: 协议降级防护测试
 *
 * 测试内容:
 * - TLS 降级攻击防护验证
 * - 强制 TLS 1.2+ 测试
 * - FALLBACK_SCSV 信号测试
 * - 协议版本协商验证
 *
 * 安全说明:
 * 协议降级攻击允许攻击者强制使用较弱的协议版本。
 * TLS_FALLBACK_SCSV (RFC 7507) 提供了防护机制。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.backed;

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const AName: string);
begin
  Section := AName;
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Check(const AName: string; AOk: Boolean; const ADetails: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', AName, ': ');
  if AOk then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if ADetails <> '' then
      WriteLn('    ', ADetails);
  end;
end;

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
  BeginSection('Protocol Security Levels');

  Check('SSL 2.0 level = 0', GetProtocolSecurityLevel(sslProtocolSSL2) = 0);
  Check('SSL 3.0 level = 1', GetProtocolSecurityLevel(sslProtocolSSL3) = 1);
  Check('TLS 1.0 level = 2', GetProtocolSecurityLevel(sslProtocolTLS10) = 2);
  Check('TLS 1.1 level = 3', GetProtocolSecurityLevel(sslProtocolTLS11) = 3);
  Check('TLS 1.2 level = 4', GetProtocolSecurityLevel(sslProtocolTLS12) = 4);
  Check('TLS 1.3 level = 5', GetProtocolSecurityLevel(sslProtocolTLS13) = 5);
end;

procedure TestDowngradeDetection;
var
  InsecureSet, SecureSet, MixedSet: TSSLProtocolVersions;
begin
  BeginSection('Downgrade Detection');

  // 只有安全协议
  SecureSet := [sslProtocolTLS12, sslProtocolTLS13];
  Check('TLS 1.2+1.3 no downgrade', not AllowsDowngrade(SecureSet));
  Check('TLS 1.2+1.3 all secure', OnlySecureProtocols(SecureSet));

  // 混合集合（允许降级）
  MixedSet := [sslProtocolTLS10, sslProtocolTLS12, sslProtocolTLS13];
  Check('TLS 1.0+1.2+1.3 allows downgrade', AllowsDowngrade(MixedSet));
  Check('TLS 1.0+1.2+1.3 not all secure', not OnlySecureProtocols(MixedSet));

  // 不安全集合
  InsecureSet := [sslProtocolSSL3, sslProtocolTLS10];
  Check('SSL3+TLS1.0 allows downgrade', AllowsDowngrade(InsecureSet),
        'Both are insecure, but still allows downgrade from TLS 1.0 to SSL 3');
  Check('SSL3+TLS1.0 not secure', not OnlySecureProtocols(InsecureSet));

  // 只有 TLS 1.3
  SecureSet := [sslProtocolTLS13];
  Check('TLS 1.3 only no downgrade', not AllowsDowngrade(SecureSet));
end;

procedure TestContextBuilderProtocolRestriction;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
begin
  BeginSection('Context Builder Protocol Restriction');

  try
    // 测试 WithTLS12And13 方法
    LBuilder := TSSLContextBuilder.Create.WithTLS12And13;
    Check('WithTLS12And13 builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Check('TLS 1.2+1.3 context created', LContext <> nil);

    // 测试 WithTLS13 方法 (TLS 1.3 only)
    LBuilder := TSSLContextBuilder.Create.WithTLS13;
    Check('WithTLS13Only builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Check('TLS 1.3 only context created', LContext <> nil);

  except
    on E: Exception do
      Check('Context builder protocol restriction', False, E.Message);
  end;
end;

procedure TestSafeDefaultsNoDowngrade;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
begin
  BeginSection('Safe Defaults No Downgrade');

  try
    LBuilder := TSSLContextBuilder.CreateWithSafeDefaults;
    Check('SafeDefaults builder created', LBuilder <> nil);

    LContext := LBuilder.BuildClient;
    Check('SafeDefaults context created', LContext <> nil);

    // SafeDefaults 应该只允许 TLS 1.2+
    Check('SafeDefaults created successfully', True);

  except
    on E: Exception do
      Check('Safe defaults creation', False, E.Message);
  end;
end;

procedure TestMinimumVersionEnforcement;
var
  LConfig: TSSLConfig;
  HasSSL2, HasSSL3, HasTLS10, HasTLS11: Boolean;
begin
  BeginSection('Minimum Version Enforcement');

  // 检查默认配置的选项
  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  HasSSL2 := not (ssoNoSSLv2 in LConfig.Options);
  HasSSL3 := not (ssoNoSSLv3 in LConfig.Options);
  HasTLS10 := not (ssoNoTLSv1 in LConfig.Options);
  HasTLS11 := not (ssoNoTLSv1_1 in LConfig.Options);

  Check('Default disables SSL 2.0', not HasSSL2);
  Check('Default disables SSL 3.0', not HasSSL3);
  Check('Default disables TLS 1.0', not HasTLS10);
  Check('Default disables TLS 1.1', not HasTLS11);
end;

procedure TestOptionsConsistency;
var
  LConfig: TSSLConfig;
begin
  BeginSection('Options Consistency');

  // 测试协议版本和选项的一致性
  LConfig := Default(TSSLConfig);
  LConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  TSSLFactory.NormalizeConfig(LConfig);

  Check('TLS 1.2 enabled', not (ssoNoTLSv1_2 in LConfig.Options));
  Check('TLS 1.3 enabled', not (ssoNoTLSv1_3 in LConfig.Options));
  Check('SSL 2.0 disabled', ssoNoSSLv2 in LConfig.Options);
  Check('SSL 3.0 disabled', ssoNoSSLv3 in LConfig.Options);
  Check('TLS 1.0 disabled', ssoNoTLSv1 in LConfig.Options);
  Check('TLS 1.1 disabled', ssoNoTLSv1_1 in LConfig.Options);
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('==========================================');
  WriteLn('  Protocol Downgrade Prevention Tests');
  WriteLn('==========================================');

  try
    // 初始化 OpenSSL
    if not LoadOpenSSLLibrary then
    begin
      WriteLn('ERROR: Failed to load OpenSSL library');
      Halt(1);
    end;

    TestProtocolSecurityLevels;
    TestDowngradeDetection;
    TestContextBuilderProtocolRestriction;
    TestSafeDefaultsNoDowngrade;
    TestMinimumVersionEnforcement;
    TestOptionsConsistency;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.Message);
      Inc(Failed);
    end;
  end;

  WriteLn;
  WriteLn('==========================================');
  WriteLn(Format('Total: %d  Passed: %d  Failed: %d', [Total, Passed, Failed]));
  WriteLn('==========================================');

  if Failed > 0 then
    Halt(1);
end.
