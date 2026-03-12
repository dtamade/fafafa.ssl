program test_factory_request_config_logging_scope_visibleization;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.lib;

type
  TLogProbe = class
  public
    procedure HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
  end;

procedure TLogProbe.HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure InitBaseConfig(out AConfig: TSSLConfig);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := sslFreePascal;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
end;

procedure ExpectRequestConfigError(const AName: string; const AConfig: TSSLConfig; const AExpectedText: string);
var
  LRaised: Boolean;
  LContext: ISSLContext;
begin
  LRaised := False;
  LContext := nil;
  try
    LContext := TSSLFactory.CreateContext(AConfig);
  except
    on E: ESSLConfigurationException do
    begin
      LRaised := True;
      Require(Pos(AExpectedText, E.Message) > 0,
        AName + ' should mention ' + AExpectedText + ', actual=' + E.Message);
    end;
  end;

  Require(LRaised, AName + ' should raise ESSLConfigurationException');
  Require(LContext = nil, AName + ' should not return a context');
end;

procedure TestRequestLogLevelRejected;
var
  LConfig: TSSLConfig;
begin
  InitBaseConfig(LConfig);
  LConfig.LogLevel := sslLogInfo;
  ExpectRequestConfigError('request LogLevel', LConfig, 'LogLevel');
end;

procedure TestRequestLogCallbackRejected;
var
  LConfig: TSSLConfig;
  LProbe: TLogProbe;
begin
  LProbe := TLogProbe.Create;
  try
    InitBaseConfig(LConfig);
    LConfig.LogCallback := @LProbe.HandleLog;
    ExpectRequestConfigError('request LogCallback', LConfig, 'LogCallback');
  finally
    LProbe.Free;
  end;
end;

procedure TestLibraryDefaultLoggingStillAllowed;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
  LProbe: TLogProbe;
begin
  LProbe := TLogProbe.Create;
  try
    TSSLFactory.ReleaseLibrary(sslFreePascal);
    LLib := TSSLFactory.GetLibraryInstance(sslFreePascal);
    Require(LLib <> nil, 'library instance should not be nil');

    InitBaseConfig(LConfig);
    LConfig.LogLevel := sslLogInfo;
    LConfig.LogCallback := @LProbe.HandleLog;
    LLib.SetDefaultConfig(LConfig);

    Require(LLib.GetDefaultConfig.LogLevel = sslLogInfo,
      'library default config should keep custom LogLevel');
    Require(Assigned(LLib.GetDefaultConfig.LogCallback),
      'library default config should keep LogCallback');
  finally
    LProbe.Free;
  end;
end;

begin
  WriteLn('fafafa.ssl - request config logging scope visibleization');
  TestRequestLogLevelRejected;
  TestRequestLogCallbackRejected;
  TestLibraryDefaultLoggingStillAllowed;
  WriteLn('[PASS] request config logging scope visibleization');
end.
