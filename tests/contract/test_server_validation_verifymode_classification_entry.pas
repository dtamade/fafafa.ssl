program test_server_validation_verifymode_classification_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils;

function WarningsContain(const AResult: TBuildValidationResult; const AFragment: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AResult.WarningCount - 1 do
    if Pos(AFragment, AResult.Warnings[I]) > 0 then
      Exit(True);
end;

var
  Builder: ISSLContextBuilder;
  Validation: TBuildValidationResult;
  Ctx: ISSLContext;
  CertPEM, KeyPEM: string;
begin
  Builder := TSSLContextBuilder.Create
    .WithVerifyNone;
  Validation := Builder.ValidateClient;
  if not Validation.HasWarnings then
    Halt(1);
  if not WarningsContain(Validation, 'Certificate verification is disabled') then
    Halt(2);

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-validation.test', 'Test Org', 30, CertPEM, KeyPEM
  ) then
    Halt(3);

  Builder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyNone
    .WithCertificatePEM(CertPEM)
    .WithPrivateKeyPEM(KeyPEM);
  Validation := Builder.ValidateServer;
  if not Validation.IsValid then
    Halt(4);
  if WarningsContain(Validation, 'Certificate verification is disabled') then
    Halt(5);
  Ctx := Builder.BuildServerWithValidation(Validation);
  if Ctx.GetVerifyMode <> [sslVerifyNone] then
    Halt(6);
  if WarningsContain(Validation, 'Certificate verification is disabled') then
    Halt(7);

  Builder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithCertificatePEM(CertPEM)
    .WithPrivateKeyPEM(KeyPEM);
  Validation := Builder.ValidateServer;
  if not Validation.HasWarnings then
    Halt(8);
  if not WarningsContain(Validation, 'Client verification enabled but no CA certificates configured') then
    Halt(9);
end.
