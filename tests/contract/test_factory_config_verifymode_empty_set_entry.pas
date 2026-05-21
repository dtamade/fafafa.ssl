program test_factory_config_verifymode_empty_set_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl;

var
  Config: TSSLConfig;
  OriginalConfig: TSSLConfig;
  LibraryConfig: TSSLConfig;
  Ctx: ISSLContext;
  Lib: ISSLLibrary;
  Mode: TSSLVerifyModes;
begin
  Config := CreateDefaultConfig(sslCtxClient);
  Config.LibraryType := sslFreePascal;
  Config.ContextType := sslCtxClient;
  Config.VerifyMode := [];

  Ctx := TSSLFactory.CreateContext(Config);
  if Ctx = nil then
    Halt(1);

  Mode := Ctx.GetVerifyMode;
  if Mode <> [] then
    Halt(2);

  Lib := TSSLFactory.GetLibrary(sslFreePascal);
  OriginalConfig := Lib.GetDefaultConfig;
  try
    LibraryConfig := OriginalConfig;
    LibraryConfig.VerifyMode := [];
    Lib.SetDefaultConfig(LibraryConfig);

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
      Halt(3);

    Mode := Ctx.GetVerifyMode;
    if Mode <> [] then
      Halt(4);
  finally
    Lib.SetDefaultConfig(OriginalConfig);
  end;
end.
