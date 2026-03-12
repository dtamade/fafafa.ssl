program test_helper_fake_log_callback_snapshot_parity;

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

procedure CheckLibrary(const AName: string; ALib: ISSLLibrary; ACallback: TSSLLogCallback);
begin
  Require(ALib <> nil, AName + ' library instance should not be nil');

  ALib.SetLogCallback(ACallback);
  Require(Assigned(ALib.GetDefaultConfig.LogCallback),
    AName + ' SetLogCallback should visibleize in GetDefaultConfig');

  ALib.SetLogCallback(nil);
  Require(not Assigned(ALib.GetDefaultConfig.LogCallback),
    AName + ' clearing SetLogCallback should clear GetDefaultConfig snapshot');
end;

procedure TestDefaultFixture;
var
  LLib: ISSLLibrary;
  LProbe: TLogProbe;
begin
  WriteLn('--- default helper fixture');
  RegisterTestDefaultFakeLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    LProbe := TLogProbe.Create;
    try
      CheckLibrary('test default helper fixture', LLib, @LProbe.HandleLog);
    finally
      LProbe.Free;
    end;
  finally
    CleanupTestDefaultFakeLibrary;
  end;
end;

procedure TestBackendStoreFixture;
var
  LDefaultLib: ISSLLibrary;
  LExplicitLib: ISSLLibrary;
  LProbe: TLogProbe;
begin
  WriteLn('--- backend store helper fixture');
  RegisterFakeLibraries;
  try
    LDefaultLib := TSSLFactory.GetLibrary(sslMbedTLS);
    LExplicitLib := TSSLFactory.GetLibrary(sslFreePascal);
    LProbe := TLogProbe.Create;
    try
      CheckLibrary('backend store default helper fixture', LDefaultLib, @LProbe.HandleLog);
      CheckLibrary('backend store explicit helper fixture', LExplicitLib, @LProbe.HandleLog);
    finally
      LProbe.Free;
    end;
  finally
    CleanupFakeLibraries;
  end;
end;

begin
  WriteLn('fafafa.ssl - helper fake log callback snapshot parity');
  TestDefaultFixture;
  TestBackendStoreFixture;
  WriteLn('[PASS] helper fake log callback snapshot parity');
end.
