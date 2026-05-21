program test_certificate_builder_safety_key_config_entry;

{$mode ObjFPC}{$H+}{$J-}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.cert.builder,
  fafafa.ssl.exceptions;

var
  LKeyPair: IKeyPairWithCertificate;
  LInvalidRaised: Boolean;
begin
  LKeyPair := TCertificateBuilder.Create
    .WithCommonName('localhost-rsa')
    .ValidFor(1)
    .WithRSAKey(TKeySize.Bits(2048))
    .SelfSigned;
  if Pos('BEGIN CERTIFICATE', LKeyPair.Certificate.ToPEM) = 0 then
    Halt(1);
  if Pos('PRIVATE KEY', LKeyPair.PrivateKey.ToPEM) = 0 then
    Halt(2);

  LKeyPair := TCertificateBuilder.Create
    .WithCommonName('localhost-ecdsa')
    .ValidFor(1)
    .WithECDSAKey(ec_P256)
    .SelfSigned;
  if Pos('BEGIN CERTIFICATE', LKeyPair.Certificate.ToPEM) = 0 then
    Halt(3);
  if Pos('PRIVATE KEY', LKeyPair.PrivateKey.ToPEM) = 0 then
    Halt(4);

  LInvalidRaised := False;
  try
    TCertificateBuilder.Create
      .WithCommonName('localhost-invalid')
      .ValidFor(1)
      .WithECDSAKey(ec_X25519);
  except
    on E: ESSLInvalidArgument do
      LInvalidRaised := True;
  end;

  if not LInvalidRaised then
    Halt(5);
end.
