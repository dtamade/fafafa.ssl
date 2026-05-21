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
  DefaultConfig: TSSLConfig;
  RawServer: ISSLContext;
  HelperServer: ISSLContext;
  QuickServerCtx: ISSLContext;
  BuilderServer: ISSLContext;
  RawMode: TSSLVerifyModes;
begin
  DefaultConfig := CreateDefaultConfig(sslCtxServer);
  if not (sslVerifyPeer in DefaultConfig.VerifyMode) then
    Halt(1);
  if sslVerifyNone in DefaultConfig.VerifyMode then
    Halt(2);

  RawServer := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  RawMode := RawServer.GetVerifyMode;
  AssertSameVerifyMode(DefaultConfig.VerifyMode, RawMode, 3);

  HelperServer := TSSLFactory.CreateServerContext(TEST_CERT, TEST_KEY, sslFreePascal);
  AssertSameVerifyMode(RawMode, HelperServer.GetVerifyMode, 4);

  QuickServerCtx := QuickServer(TEST_CERT, TEST_KEY, 0);
  AssertSameVerifyMode(RawMode, QuickServerCtx.GetVerifyMode, 5);

  BuilderServer := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithCertificate(TEST_CERT)
    .WithPrivateKey(TEST_KEY)
    .BuildServer;
  AssertSameVerifyMode(RawMode, BuilderServer.GetVerifyMode, 6);
end.
