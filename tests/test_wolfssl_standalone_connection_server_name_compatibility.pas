program test_wolfssl_standalone_connection_server_name_compatibility;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.wolfssl.connection;

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

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
  LInfo: TSSLConnectionInfo;
begin
  WriteLn('fafafa.ssl - WolfSSL standalone connection should match runtime ServerName contract');

  try
    LContext := TSSLFactory.CreateContext(sslCtxClient, sslWolfSSL);
  except
    on E: Exception do
    begin
      WriteLn('[SKIP] WolfSSL unavailable: ', E.Message);
      Halt(0);
    end;
  end;
  Require(LContext <> nil, 'WolfSSL context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := fafafa.ssl.wolfssl.connection.TWolfSSLConnection.Create(LContext, LStream);
    Require(LConnection <> nil, 'standalone WolfSSL connection should not be nil');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'standalone WolfSSL connection should support ISSLClientConnection');

    RequireEquals('Field after create', 'ctx.default.example', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Connection info after create', 'ctx.default.example', LInfo.ServerName);

    LClientConn.SetServerName('override.example');
    RequireEquals('Field after override', 'override.example', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Connection info after override', 'override.example', LInfo.ServerName);

    LClientConn.SetServerName('');
    RequireEquals('Field after clear', '', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Connection info after clear', '', LInfo.ServerName);
  finally
    LStream.Free;
  end;

  WriteLn('[PASS] WolfSSL standalone connection matches runtime ServerName contract');
end.
