program test_library_log_callback_roundtrip_visibleization;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.freepascal.lib
  {$IFDEF ENABLE_MBEDTLS}, fafafa.ssl.mbedtls.lib{$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}, fafafa.ssl.wolfssl.lib{$ENDIF}
  {$IFDEF WINDOWS}, fafafa.ssl.winssl.lib{$ENDIF};

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

procedure CheckBackend(const ABackend: string; ALibType: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LSaved: TSSLConfig;
  LProbe: TLogProbe;
begin
  if not TSSLFactory.IsLibraryAvailable(ALibType) then
  begin
    WriteLn('[SKIP] ', ABackend, ' not available');
    Exit;
  end;

  TSSLFactory.ReleaseLibrary(ALibType);
  LLib := TSSLFactory.GetLibrary(ALibType);
  Require(LLib <> nil, ABackend + ' library instance should not be nil');
  LSaved := LLib.GetDefaultConfig;

  LProbe := TLogProbe.Create;
  try
    LLib.SetLogCallback(@LProbe.HandleLog);
    Require(Assigned(LLib.GetDefaultConfig.LogCallback),
      ABackend + ' SetLogCallback should visibleize in GetDefaultConfig');

    LLib.SetLogCallback(nil);
    Require(not Assigned(LLib.GetDefaultConfig.LogCallback),
      ABackend + ' clearing SetLogCallback should clear GetDefaultConfig snapshot');
  finally
    LLib.SetDefaultConfig(LSaved);
    LProbe.Free;
  end;

  WriteLn('[PASS] ', ABackend, ' log callback roundtrip visibleization');
end;

begin
  WriteLn('fafafa.ssl - library log callback roundtrip visibleization');
  CheckBackend('FreePascal', sslFreePascal);
  CheckBackend('OpenSSL', sslOpenSSL);
  {$IFDEF ENABLE_MBEDTLS}
  CheckBackend('MbedTLS', sslMbedTLS);
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  CheckBackend('WolfSSL', sslWolfSSL);
  {$ENDIF}
  {$IFDEF WINDOWS}
  CheckBackend('WinSSL', sslWinSSL);
  {$ENDIF}
  WriteLn('[PASS] library log callback roundtrip visibleization');
end.
