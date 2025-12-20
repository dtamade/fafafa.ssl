program test_default_config;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl;

procedure AssertTrue(const AName: string; AValue: Boolean);
begin
  if AValue then
    WriteLn('  [PASS] ', AName)
  else
  begin
    WriteLn('  [FAIL] ', AName);
    Halt(1);
  end;
end;

procedure TestDefaultConfigSecurityBaseline;
var
  Cfg: TSSLConfig;
begin
  Cfg := CreateDefaultConfig(sslCtxClient);

  AssertTrue('CreateDefaultConfig returns correct context type', Cfg.ContextType = sslCtxClient);
  AssertTrue('Default options contains ssoDisableCompression', ssoDisableCompression in Cfg.Options);
  AssertTrue('Default options contains ssoDisableRenegotiation', ssoDisableRenegotiation in Cfg.Options);

  AssertTrue('Default options disables SSLv2', ssoNoSSLv2 in Cfg.Options);
  AssertTrue('Default options disables SSLv3', ssoNoSSLv3 in Cfg.Options);
  AssertTrue('Default options disables TLSv1.0', ssoNoTLSv1 in Cfg.Options);
  AssertTrue('Default options disables TLSv1.1', ssoNoTLSv1_1 in Cfg.Options);

  AssertTrue('VerifyDepth non-zero', Cfg.VerifyDepth > 0);
  AssertTrue('CipherList not empty', Cfg.CipherList <> '');
  AssertTrue('CipherSuites not empty', Cfg.CipherSuites <> '');
end;

begin
  WriteLn('========================================');
  WriteLn('  fafafa.ssl DefaultConfig 单元测试');
  WriteLn('========================================');

  TestDefaultConfigSecurityBaseline;

  WriteLn('所有测试通过！✓');
end.
