program test_default_config;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib;

type
  TLogProbe = class
    procedure HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
  end;

procedure TLogProbe.HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

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


procedure TestDefaultConfigRequestPathCompatibility;
var
  Cfg: TSSLConfig;
  Ctx: ISSLContext;
begin
  Cfg := CreateDefaultConfig(sslCtxClient);
  Cfg.LibraryType := sslFreePascal;

  AssertTrue('CreateDefaultConfig request path clears LogLevel',
    Cfg.LogLevel = sslLogNone);
  AssertTrue('CreateDefaultConfig request path clears LogCallback',
    not Assigned(Cfg.LogCallback));

  Ctx := TSSLFactory.CreateContext(Cfg);
  AssertTrue('CreateDefaultConfig remains accepted by TSSLFactory.CreateContext',
    Ctx <> nil);
end;

procedure TestDefaultConfigIgnoresLibraryScopedLoggingDefaults;
var
  Cfg: TSSLConfig;
  LLib: ISSLLibrary;
  LSavedConfig: TSSLConfig;
  LOverrideConfig: TSSLConfig;
  LSavedDefaultLibrary: TSSLLibraryType;
  LProbe: TLogProbe;
  LLogLevelCleared: Boolean;
  LLogCallbackCleared: Boolean;
begin
  LProbe := TLogProbe.Create;
  try
    LSavedDefaultLibrary := TSSLFactory.GetDefaultLibrary;
    TSSLFactory.SetDefaultLibrary(sslFreePascal);

    LLib := TSSLFactory.GetLibraryInstance(sslFreePascal);
    LSavedConfig := LLib.GetDefaultConfig;

    try
      LOverrideConfig := LSavedConfig;
      LOverrideConfig.LogLevel := sslLogInfo;
      LOverrideConfig.LogCallback := @LProbe.HandleLog;
      LLib.SetDefaultConfig(LOverrideConfig);

      Cfg := CreateDefaultConfig(sslCtxClient);
      LLogLevelCleared := Cfg.LogLevel = sslLogNone;
      LLogCallbackCleared := not Assigned(Cfg.LogCallback);
    finally
      LLib.SetDefaultConfig(LSavedConfig);
      TSSLFactory.SetDefaultLibrary(LSavedDefaultLibrary);
    end;
  finally
    LProbe.Free;
  end;

  AssertTrue('CreateDefaultConfig ignores library-scoped LogLevel defaults',
    LLogLevelCleared);
  AssertTrue('CreateDefaultConfig ignores library-scoped LogCallback defaults',
    LLogCallbackCleared);
end;

begin
  WriteLn('========================================');
  WriteLn('  fafafa.ssl DefaultConfig 单元测试');
  WriteLn('========================================');

  TestDefaultConfigSecurityBaseline;
  TestDefaultConfigRequestPathCompatibility;
  TestDefaultConfigIgnoresLibraryScopedLoggingDefaults;

  WriteLn('所有测试通过！✓');
end.
