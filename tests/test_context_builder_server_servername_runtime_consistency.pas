program test_context_builder_server_servername_runtime_consistency;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils,
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
  try
    if not Supports(Conn, ISSLClientConnection, ClientConn) then
      raise Exception.Create('Connection does not support ISSLClientConnection');
    Result := ClientConn.GetServerName;
  finally
    Conn := nil;
  end;
end;

procedure Test_BuilderClientWithSNI_PreservesServerName;
var
  Ctx: ISSLContext;
begin
  TestHeader('Builder BuildClient keeps explicit WithSNI ServerName');

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithSNI('builder-client.example.com')
    .BuildClient;

  Assert(Ctx.GetServerName = 'builder-client.example.com',
    'BuildClient preserves explicit WithSNI ServerName on the built context');
  Assert(ConnectionServerName(Ctx) = 'builder-client.example.com',
    'Client connection inherits the builder-configured ServerName');
end;

procedure Test_BuilderServerWithSNI_PreservesServerName;
var
  Ctx: ISSLContext;
  CertPEM: string;
  KeyPEM: string;
begin
  TestHeader('Builder BuildServer keeps context ServerName but not server connection SNI');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'builder-server.local', 'Test Org', 30, CertPEM, KeyPEM
  ) then
    raise Exception.Create('Failed to generate self-signed certificate');

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithCertificatePEM(CertPEM)
    .WithPrivateKeyPEM(KeyPEM)
    .WithSNI('builder-server.example.com')
    .BuildServer;

  Assert(Ctx.GetServerName = 'builder-server.example.com',
    'BuildServer preserves explicit WithSNI ServerName on the built context');
  Assert(ConnectionServerName(Ctx) = '',
    'Server connection ignores the builder-configured client-only ServerName');
end;

procedure Test_DirectServerContext_IgnoresLegacyContextServerName;
var
  Ctx: ISSLContext;
begin
  TestHeader('Direct server context keeps legacy ServerName state off new connections');

  Ctx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  // INTENTIONAL_COMPAT: keep the deprecated context-level setter observable
  // while proving server-side CreateConnection no longer inherits it.
  Ctx.SetServerName('direct-server.example.com');

  Assert(Ctx.GetServerName = 'direct-server.example.com',
    'Direct server context still retains the configured legacy ServerName');
  Assert(ConnectionServerName(Ctx) = '',
    'Server connection ignores direct-context legacy ServerName');
end;

begin
  try
    Test_BuilderClientWithSNI_PreservesServerName;
    Test_BuilderServerWithSNI_PreservesServerName;
    Test_DirectServerContext_IgnoresLegacyContextServerName;

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
