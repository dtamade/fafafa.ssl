program test_factory_config_server_name_isolation;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Assert(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('  PASS: ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('  FAIL: ', AMessage);
  end;
end;

procedure TestHeader(const AName: string);
begin
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

function ConnectionServerName(ACtx: ISSLContext): string;
var
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  Conn := ACtx.CreateConnection(THandle(-1));
  ClientConn := Conn as ISSLClientConnection;
  Result := ClientConn.GetServerName;
end;

procedure Test_ExplicitDefaultConfig_PersistsToDefaultPath;
var
  Lib: ISSLLibrary;
  OriginalConfig: TSSLConfig;
  DefaultConfig: TSSLConfig;
  Ctx: ISSLContext;
begin
  TestHeader('Explicit SetDefaultConfig persists to default CreateContext path');

  Lib := TSSLFactory.GetLibrary(sslFreePascal);
  OriginalConfig := Lib.GetDefaultConfig;
  try
    DefaultConfig := OriginalConfig;
    DefaultConfig.ServerName := 'default.example.com';
    Lib.SetDefaultConfig(DefaultConfig);

    Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);

    Assert(Ctx.GetServerName = 'default.example.com',
      'Default-path context inherits explicit library default ServerName');
    Assert(ConnectionServerName(Ctx) = 'default.example.com',
      'Default-path connection inherits explicit library default ServerName');
  finally
    Lib.SetDefaultConfig(OriginalConfig);
  end;
end;

procedure Test_OneShotConfig_DoesNotLeakIntoSharedDefaults;
var
  Lib: ISSLLibrary;
  OriginalConfig: TSSLConfig;
  OneShotConfig: TSSLConfig;
  OneShotCtx: ISSLContext;
  FreshCtx: ISSLContext;
begin
  TestHeader('One-shot factory config does not leak into shared defaults');

  Lib := TSSLFactory.GetLibrary(sslFreePascal);
  OriginalConfig := Lib.GetDefaultConfig;
  try
    OneShotConfig := CreateDefaultConfig(sslCtxClient);
    OneShotConfig.LibraryType := sslFreePascal;
    OneShotConfig.ContextType := sslCtxClient;
    OneShotConfig.ServerName := 'sticky.example.com';

    OneShotCtx := TSSLFactory.CreateContext(OneShotConfig);
    Assert(OneShotCtx.GetServerName = 'sticky.example.com',
      'One-shot context applies configured ServerName');
    Assert(ConnectionServerName(OneShotCtx) = 'sticky.example.com',
      'One-shot connection inherits configured ServerName');

    FreshCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    Assert(FreshCtx.GetServerName = '',
      'Subsequent default-path context does not inherit one-shot ServerName');
    Assert(ConnectionServerName(FreshCtx) = '',
      'Subsequent default-path connection does not inherit one-shot ServerName');
  finally
    Lib.SetDefaultConfig(OriginalConfig);
  end;
end;

begin
  try
    Test_ExplicitDefaultConfig_PersistsToDefaultPath;
    Test_OneShotConfig_DoesNotLeakIntoSharedDefaults;

    WriteLn;
    WriteLn('Tests Passed: ', GTestsPassed);
    WriteLn('Tests Failed: ', GTestsFailed);

    if GTestsFailed > 0 then
      Halt(1);

    WriteLn('All tests passed.');
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.Message);
      Halt(1);
    end;
  end;
end.
