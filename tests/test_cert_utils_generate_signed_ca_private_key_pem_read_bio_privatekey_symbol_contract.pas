program test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl,
  fafafa.ssl.exceptions,
  fafafa.ssl.cert.utils,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp;

var
  GLib: ISSLLibrary = nil;
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  SkippedTests: Integer = 0;
  GOriginalBIONewMemBuf: TBIO_new_mem_buf = nil;
  GOriginalPEMReadPrivateKey: TPEM_read_bio_PrivateKey = nil;
  GBIONewMemBufCallCount: Integer = 0;

procedure AssertTrue(const AName: string; ACondition: Boolean; const ADetail: string = '');
begin
  Inc(TotalTests);
  if ACondition then
  begin
    Inc(PassedTests);
    WriteLn('[PASS] ', AName);
  end
  else
  begin
    Inc(FailedTests);
    WriteLn('[FAIL] ', AName);
    if ADetail <> '' then
      WriteLn('       ', ADetail);
  end;
end;

procedure MarkSkip(const AName, AReason: string);
begin
  Inc(TotalTests);
  Inc(SkippedTests);
  WriteLn('[SKIP] [capability] ', AName, ' - ', AReason);
end;

function BuildCAOptions: TCertGenOptions;
begin
  Result := TCertificateUtils.DefaultGenOptions;
  Result.CommonName := 'signed-ca-private-key-pem-read-root.local';
  Result.Organization := 'fafafa.ssl contract';
  Result.IsCA := True;
  Result.ValidDays := 30;
end;

function BuildLeafOptions: TCertGenOptions;
begin
  Result := TCertificateUtils.DefaultGenOptions;
  Result.CommonName := 'signed-ca-private-key-pem-read-leaf.local';
  Result.Organization := 'fafafa.ssl contract';
  Result.IsCA := False;
  Result.ValidDays := 30;
end;

procedure WarmupGenerateSignedMaterials(
  out ACACertPEM: string;
  out ACAKeyPEM: string
);
var
  LCAOptions: TCertGenOptions;
  LLeafOptions: TCertGenOptions;
  LLeafCertPEM: string;
  LLeafKeyPEM: string;
begin
  LCAOptions := BuildCAOptions;
  if not TCertificateUtils.GenerateSelfSigned(LCAOptions, ACACertPEM, ACAKeyPEM) then
    raise Exception.Create('GenerateSelfSigned warmup returned False');
  if (ACACertPEM = '') or (ACAKeyPEM = '') then
    raise Exception.Create('GenerateSelfSigned warmup returned empty CA material');

  LLeafOptions := BuildLeafOptions;
  if not TCertificateUtils.GenerateSigned(
    LLeafOptions,
    ACACertPEM,
    ACAKeyPEM,
    LLeafCertPEM,
    LLeafKeyPEM
  ) then
    raise Exception.Create('GenerateSigned warmup returned False');
  if (LLeafCertPEM = '') or (LLeafKeyPEM = '') then
    raise Exception.Create('GenerateSigned warmup returned empty leaf material');
end;

function DisableCAPrivateKeyPEMReadAfterBIOConstructor(const ABuf: Pointer; ALen: Integer): PBIO; cdecl;
begin
  Inc(GBIONewMemBufCallCount);
  Result := GOriginalBIONewMemBuf(ABuf, ALen);
  if (GBIONewMemBufCallCount = 2) and (Result <> nil) then
    PEM_read_bio_PrivateKey := nil;
end;

procedure InstallCAPrivateKeyPEMReadFailureWrapper;
begin
  GBIONewMemBufCallCount := 0;
  PEM_read_bio_PrivateKey := GOriginalPEMReadPrivateKey;
  BIO_new_mem_buf := @DisableCAPrivateKeyPEMReadAfterBIOConstructor;
end;

procedure AssertGenerateSignedControlledFailure(
  const AName: string;
  const AOptions: TCertGenOptions;
  const ACACertPEM, ACAKeyPEM: string
);
var
  LRaised: Boolean;
  LControlled: Boolean;
  LDetail: string;
  LCertPEM: string;
  LKeyPEM: string;
  LTryRaised: Boolean;
  LTryDetail: string;
  LTryResult: Boolean;
begin
  InstallCAPrivateKeyPEMReadFailureWrapper;

  LRaised := False;
  LControlled := False;
  LDetail := '';
  LCertPEM := '';
  LKeyPEM := '';
  try
    TCertificateUtils.GenerateSigned(AOptions, ACACertPEM, ACAKeyPEM, LCertPEM, LKeyPEM);
  except
    on E: Exception do
    begin
      LRaised := True;
      LControlled := E is ESSLCertError;
      LDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' should raise', LRaised,
    'expected GenerateSigned(...) to fail');
  AssertTrue(AName + ' should raise controlled ESSLCertError', LControlled, LDetail);

  InstallCAPrivateKeyPEMReadFailureWrapper;

  LTryRaised := False;
  LTryDetail := '';
  LTryResult := True;
  LCertPEM := 'sentinel-cert';
  LKeyPEM := 'sentinel-key';
  try
    LTryResult := TCertificateUtils.TryGenerateSigned(
      AOptions,
      ACACertPEM,
      ACAKeyPEM,
      LCertPEM,
      LKeyPEM
    );
  except
    on E: Exception do
    begin
      LTryRaised := True;
      LTryDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' TryGenerateSigned should not raise', not LTryRaised, LTryDetail);
  AssertTrue(AName + ' TryGenerateSigned should return False', not LTryResult,
    'expected TryGenerateSigned to return False');
  AssertTrue(AName + ' TryGenerateSigned should clear cert output', LCertPEM = '',
    'expected cleared certificate output');
  AssertTrue(AName + ' TryGenerateSigned should clear key output', LKeyPEM = '',
    'expected cleared key output');
end;

procedure TestGenerateSignedShouldFailGracefullyWhenCAPrivateKeyPEMReadBecomesUnavailable;
var
  LLeafOptions: TCertGenOptions;
  LCACertPEM: string;
  LCAKeyPEM: string;
begin
  WriteLn;
  WriteLn('=== Certificate utils GenerateSigned CA private-key PEM_read_bio_PrivateKey symbol guard ===');

  if (not Assigned(BIO_new_mem_buf)) or (not Assigned(PEM_read_bio_PrivateKey)) then
  begin
    MarkSkip('certificate utils generate-signed CA private-key PEM_read_bio_PrivateKey contract',
      'required baseline BIO_new_mem_buf/PEM_read_bio_PrivateKey helpers are unavailable');
    Exit;
  end;

  LLeafOptions := BuildLeafOptions;
  WarmupGenerateSignedMaterials(LCACertPEM, LCAKeyPEM);

  GOriginalBIONewMemBuf := BIO_new_mem_buf;
  GOriginalPEMReadPrivateKey := PEM_read_bio_PrivateKey;
  try
    AssertGenerateSignedControlledFailure(
      'GenerateSigned when CA private-key PEM_read_bio_PrivateKey becomes unavailable',
      LLeafOptions,
      LCACertPEM,
      LCAKeyPEM
    );
  finally
    BIO_new_mem_buf := GOriginalBIONewMemBuf;
    PEM_read_bio_PrivateKey := GOriginalPEMReadPrivateKey;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('Certificate Utils GenerateSigned CA Private-Key PEM_read_bio_PrivateKey Symbol Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('certificate utils generate-signed CA private-key PEM_read_bio_PrivateKey contract',
        'failed to initialize OpenSSL library');

    if SkippedTests = 0 then
    begin
      LoadOpenSSLCore();
      LoadOpenSSLBIO();
      LoadOpenSSLX509();
      if not LoadOpenSSLPEM(GetCryptoLibHandle) then
        raise Exception.Create('failed to load PEM support');
      if not LoadEVP(GetCryptoLibHandle) then
        raise Exception.Create('failed to load EVP support');
    end;

    if SkippedTests = 0 then
      TestGenerateSignedShouldFailGracefullyWhenCAPrivateKeyPEMReadBecomesUnavailable;

    WriteLn;
    WriteLn('========================================');
    WriteLn('Summary');
    WriteLn('========================================');
    WriteLn('Total tests: ', TotalTests);
    WriteLn('Passed: ', PassedTests);
    WriteLn('Failed: ', FailedTests);
    WriteLn('Skipped: ', SkippedTests);

    if FailedTests > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('[FATAL] ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
