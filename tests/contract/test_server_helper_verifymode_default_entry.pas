program test_server_helper_verifymode_default_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.base;

const
  TEST_CERT = 'tests/certificate/test_certs/signer_cert.pem';
  TEST_KEY  = 'tests/certificate/test_certs/signer_key.pem';

procedure AssertSameVerifyMode(const AExpected, AActual: TSSLVerifyModes; AExitCode: Integer);
begin
  if AActual <> AExpected then
    Halt(AExitCode);
end;

var
  ClientDefaultConfig: TSSLConfig;
  ServerDefaultConfig: TSSLConfig;
  DirectLibrary: ISSLLibrary;
  RawClient: ISSLContext;
  RawServer: ISSLContext;
  DirectClient: ISSLContext;
  DirectServer: ISSLContext;
  HelperServer: ISSLContext;
  QuickServerCtx: ISSLContext;
  BuilderClient: ISSLContext;
  BuilderServer: ISSLContext;
  DefaultBuilderJSON: string;
  DefaultBuilderINI: string;
begin
  ClientDefaultConfig := CreateDefaultConfig(sslCtxClient);
  AssertSameVerifyMode([sslVerifyPeer], ClientDefaultConfig.VerifyMode, 1);

  ServerDefaultConfig := CreateDefaultConfig(sslCtxServer);
  AssertSameVerifyMode([], ServerDefaultConfig.VerifyMode, 2);

  RawClient := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertSameVerifyMode([sslVerifyPeer], RawClient.GetVerifyMode, 3);

  RawServer := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  AssertSameVerifyMode([], RawServer.GetVerifyMode, 4);

  DirectLibrary := TSSLFactory.GetLibrary(sslFreePascal);
  DirectClient := DirectLibrary.CreateContext(sslCtxClient);
  AssertSameVerifyMode([sslVerifyPeer], DirectClient.GetVerifyMode, 5);

  DirectServer := DirectLibrary.CreateContext(sslCtxServer);
  AssertSameVerifyMode([], DirectServer.GetVerifyMode, 6);

  HelperServer := TSSLFactory.CreateServerContext(TEST_CERT, TEST_KEY, sslFreePascal);
  AssertSameVerifyMode([], HelperServer.GetVerifyMode, 7);

  QuickServerCtx := QuickServer(TEST_CERT, TEST_KEY, 0);
  AssertSameVerifyMode([], QuickServerCtx.GetVerifyMode, 8);

  BuilderClient := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .BuildClient;
  AssertSameVerifyMode([sslVerifyPeer], BuilderClient.GetVerifyMode, 9);

  BuilderServer := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithCertificate(TEST_CERT)
    .WithPrivateKey(TEST_KEY)
    .BuildServer;
  AssertSameVerifyMode([], BuilderServer.GetVerifyMode, 10);

  DefaultBuilderJSON := TSSLContextBuilder.Create.ExportToJSON;
  BuilderClient := TSSLContextBuilder.Create
    .ImportFromJSON(DefaultBuilderJSON)
    .WithBackend(sslFreePascal)
    .BuildClient;
  AssertSameVerifyMode([sslVerifyPeer], BuilderClient.GetVerifyMode, 11);

  BuilderServer := TSSLContextBuilder.Create
    .ImportFromJSON(DefaultBuilderJSON)
    .WithBackend(sslFreePascal)
    .WithCertificate(TEST_CERT)
    .WithPrivateKey(TEST_KEY)
    .BuildServer;
  AssertSameVerifyMode([], BuilderServer.GetVerifyMode, 12);

  DefaultBuilderINI := TSSLContextBuilder.Create.ExportToINI;
  BuilderClient := TSSLContextBuilder.Create
    .ImportFromINI(DefaultBuilderINI)
    .WithBackend(sslFreePascal)
    .BuildClient;
  AssertSameVerifyMode([sslVerifyPeer], BuilderClient.GetVerifyMode, 13);

  BuilderServer := TSSLContextBuilder.Create
    .ImportFromINI(DefaultBuilderINI)
    .WithBackend(sslFreePascal)
    .WithCertificate(TEST_CERT)
    .WithPrivateKey(TEST_KEY)
    .BuildServer;
  AssertSameVerifyMode([], BuilderServer.GetVerifyMode, 14);
end.
