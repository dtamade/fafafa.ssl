program test_freepascal_connection_server_name_observability;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib;

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
  WriteLn('fafafa.ssl - FreePascal ServerName should be observable via connection info');

  LContext := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Require(LContext <> nil, 'FreePascal context should not be nil');

  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConnection := LContext.CreateConnection(LStream);
    Require(LConnection <> nil, 'FreePascal connection should not be nil');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'FreePascal connection should support ISSLClientConnection');

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

  WriteLn('[PASS] FreePascal connection info exposes effective ServerName');
end.
