program test_openssl_connection_server_name_clear_override;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
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

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
  LInfo: TSSLConnectionInfo;
begin
  WriteLn('fafafa.ssl - OpenSSL clear override should clear native ServerName');

  LContext := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
  Require(LContext <> nil, 'OpenSSL context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := LContext.CreateConnection(LStream);
    Require(LConnection <> nil, 'OpenSSL connection should not be nil');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'OpenSSL connection should support ISSLClientConnection');

    RequireEquals('Field after create', 'ctx.default.example', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Native info after create', 'ctx.default.example', LInfo.ServerName);

    LClientConn.SetServerName('override.example');
    RequireEquals('Field after override', 'override.example', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Native info after override', 'override.example', LInfo.ServerName);

    LClientConn.SetServerName('');
    RequireEquals('Field after clear', '', LClientConn.GetServerName);
    LInfo := LConnection.GetConnectionInfo;
    RequireEquals('Native info after clear', '', LInfo.ServerName);
  finally
    LStream.Free;
  end;

  WriteLn('[PASS] OpenSSL clear override clears native ServerName');
end.
