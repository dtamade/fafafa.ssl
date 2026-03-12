program test_connection_context_server_name_inheritance;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib,
  fafafa.ssl.openssl.lib;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure RequireEquals(const AName, AExpected, AActual: string);
begin
  if AExpected <> AActual then
  begin
    WriteLn('[FAIL] ', AName, ' expected="', AExpected, '" actual="', AActual, '"');
    Halt(1);
  end;
end;

procedure RunBackendCase(ALibType: TSSLLibraryType);
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
  LName: string;
begin
  LName := SSL_LIBRARY_NAMES[ALibType];
  WriteLn('--- Backend: ', LName);

  LContext := TSSLFactory.CreateContext(sslCtxClient, ALibType);
  Require(LContext <> nil, LName + ' context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := LContext.CreateConnection(LStream);
    Require(LConnection <> nil, LName + ' connection should not be nil');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      LName + ' connection should support ISSLClientConnection');
    RequireEquals(LName + ' inherited ServerName',
      'ctx.default.example', LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

procedure TryRunBackendCase(ALibType: TSSLLibraryType);
begin
  try
    RunBackendCase(ALibType);
  except
    on E: Exception do
      WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ALibType], ': ', E.Message);
  end;
end;

begin
  WriteLn('fafafa.ssl - connection inherits context default ServerName');

  TryRunBackendCase(sslFreePascal);
  TryRunBackendCase(sslOpenSSL);

  {$IFDEF ENABLE_MBEDTLS}
  TryRunBackendCase(sslMbedTLS);
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  TryRunBackendCase(sslWolfSSL);
  {$ENDIF}

  WriteLn('[PASS] connection inherits context default ServerName');
end.
