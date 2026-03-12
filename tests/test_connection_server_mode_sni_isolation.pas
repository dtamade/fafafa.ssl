program test_connection_server_mode_sni_isolation;

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

procedure RunClientCase(ALibType: TSSLLibraryType);
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
  LName: string;
begin
  LName := SSL_LIBRARY_NAMES[ALibType];
  WriteLn('--- Client Backend: ', LName);

  LContext := TSSLFactory.CreateContext(sslCtxClient, ALibType);
  Require(LContext <> nil, LName + ' client context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('client.default.example');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := LContext.CreateConnection(LStream);
    Require(LConnection <> nil, LName + ' client connection should not be nil');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      LName + ' client connection should support ISSLClientConnection');
    RequireEquals(LName + ' client inherited ServerName',
      'client.default.example', LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

procedure RunServerCase(ALibType: TSSLLibraryType);
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
  LName: string;
begin
  LName := SSL_LIBRARY_NAMES[ALibType];
  WriteLn('--- Server Backend: ', LName);

  LContext := TSSLFactory.CreateContext(sslCtxServer, ALibType);
  Require(LContext <> nil, LName + ' server context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('server.context.should.not.flow');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := LContext.CreateConnection(LStream);
    Require(LConnection <> nil, LName + ' server connection should not be nil');

    if Supports(LConnection, ISSLClientConnection, LClientConn) then
      RequireEquals(LName + ' server connection should not inherit client SNI',
        '', LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

procedure TryRunBackendCase(ALibType: TSSLLibraryType);
begin
  try
    RunClientCase(ALibType);
    RunServerCase(ALibType);
  except
    on E: Exception do
      WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ALibType], ': ', E.Message);
  end;
end;

begin
  WriteLn('fafafa.ssl - server mode should isolate client SNI defaults');

  TryRunBackendCase(sslFreePascal);
  TryRunBackendCase(sslOpenSSL);

  {$IFDEF ENABLE_MBEDTLS}
  TryRunBackendCase(sslMbedTLS);
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  TryRunBackendCase(sslWolfSSL);
  {$ENDIF}

  WriteLn('[PASS] server mode isolates client SNI defaults');
end.
