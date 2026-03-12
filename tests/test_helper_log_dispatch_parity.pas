program test_helper_log_dispatch_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.context;

{$I helpers/test_fake_default_backend_fixture.inc}
{$I helpers/test_backend_store_fake_fixture.inc}

type
  TLogProbe = class
  public
    Count: Integer;
    LastLevel: TSSLLogLevel;
    LastMessage: string;
    procedure HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
  end;

procedure TLogProbe.HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  Inc(Count);
  LastLevel := ALevel;
  LastMessage := AMessage;
end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure InitLogConfig(var AConfig: TSSLConfig; ALibType: TSSLLibraryType);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := ALibType;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
  AConfig.LogLevel := sslLogInfo;
end;

procedure CheckLibrary(const AName: string; ALib: ISSLLibrary; ALibType: TSSLLibraryType);
var
  LConfig: TSSLConfig;
  LProbe: TLogProbe;
begin
  Require(ALib <> nil, AName + ' library should not be nil');

  InitLogConfig(LConfig, ALibType);
  ALib.SetDefaultConfig(LConfig);

  LProbe := TLogProbe.Create;
  try
    ALib.SetLogCallback(@LProbe.HandleLog);

    ALib.Log(sslLogInfo, 'visible');
    Require(LProbe.Count = 1, AName + ' info log should dispatch callback');
    Require(LProbe.LastLevel = sslLogInfo, AName + ' should preserve callback level');
    Require(LProbe.LastMessage = 'visible', AName + ' should preserve callback message');

    ALib.Log(sslLogDebug, 'hidden');
    Require(LProbe.Count = 1, AName + ' debug log above LogLevel should be filtered');

    ALib.SetLogCallback(nil);
    ALib.Log(sslLogError, 'cleared');
    Require(LProbe.Count = 1, AName + ' cleared callback should suppress dispatch');
  finally
    LProbe.Free;
  end;
end;

procedure TestDefaultFixture;
var
  LLib: ISSLLibrary;
begin
  WriteLn('--- default helper fixture');
  RegisterTestDefaultFakeLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    CheckLibrary('default helper fixture', LLib, sslFreePascal);
  finally
    CleanupTestDefaultFakeLibrary;
  end;
end;

procedure TestBackendStoreFixture;
var
  LDefaultLib: ISSLLibrary;
  LExplicitLib: ISSLLibrary;
begin
  WriteLn('--- backend store helper fixture');
  RegisterFakeLibraries;
  try
    LDefaultLib := TSSLFactory.GetLibrary(sslMbedTLS);
    LExplicitLib := TSSLFactory.GetLibrary(sslFreePascal);
    CheckLibrary('backend store default helper fixture', LDefaultLib, sslMbedTLS);
    CheckLibrary('backend store explicit helper fixture', LExplicitLib, sslFreePascal);
  finally
    CleanupFakeLibraries;
  end;
end;

begin
  WriteLn('fafafa.ssl - helper log dispatch parity');
  TestDefaultFixture;
  TestBackendStoreFixture;
  WriteLn('[PASS] helper log dispatch parity');
end.
