program test_tls12_rsa_verify;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.x509,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.servercertverify,
  fafafa.ssl.tls12.rsa.verify;

procedure Fail(const AMessage: string);
begin
  WriteLn('❌ ', AMessage);
  Halt(1);
end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    Fail(AMessage);
end;

function ReadFileBytes(const AFileName: string): TBytes;
var
  LStream: TFileStream;
begin
  Result := nil;
  if not FileExists(AFileName) then
    Fail('Missing fixture: ' + AFileName);

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);
    if Length(Result) > 0 then
      LStream.ReadBuffer(Result[0], Length(Result));
  finally
    LStream.Free;
  end;
end;

function BytesFromAnsi(const AValue: AnsiString): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[1], Result[0], Length(AValue));
end;

procedure TestVerifyRSAPKCS1v15SHA256Signature;
var
  LCert: TX509Certificate;
  LKeyBlob: TBytes;
  LMessage: TBytes;
  LSignature: TBytes;
  LError: string;
begin
  LCert := TX509Certificate.Create;
  try
    LCert.LoadFromFile('tests/certificate/test_certs/signer_cert.pem');
    LKeyBlob := ReadFileBytes('tests/certificate/test_certs/signer_key.pem');
    LMessage := BytesFromAnsi('tls12-rsa-verify-probe');

    AssertTrue(
      TryBuildTLS13CertificateVerifySignature(
        TLS13_SIG_RSA_PKCS1_SHA256,
        LKeyBlob,
        LMessage,
        LSignature,
        LError
      ),
      'RSA PKCS1 signer should produce signature: ' + LError
    );

    AssertTrue(
      TryVerifyTLS12RSAPKCS1v15SHA256Signature(
        LMessage,
        LCert.PublicKeyInfo.RSAModulus,
        LCert.PublicKeyInfo.RSAExponent,
        LSignature,
        LError
      ),
      'RSA PKCS1 verifier should accept matching signature: ' + LError
    );

    LSignature[0] := LSignature[0] xor $01;
    AssertTrue(
      not TryVerifyTLS12RSAPKCS1v15SHA256Signature(
        LMessage,
        LCert.PublicKeyInfo.RSAModulus,
        LCert.PublicKeyInfo.RSAExponent,
        LSignature,
        LError
      ),
      'RSA PKCS1 verifier should reject modified signature'
    );
  finally
    LCert.Free;
  end;
end;

begin
  WriteLn('Testing TLS 1.2 RSA verify helpers...');

  TestVerifyRSAPKCS1v15SHA256Signature;

  WriteLn('✅ TLS 1.2 RSA verify checks passed');
end.
