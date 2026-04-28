program test_cert_utils_generate_selfsigned_certificate_pem_export_pem_write_bio_x509_symbol_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl,
  fafafa.ssl.exceptions,
  fafafa.ssl.cert.utils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp;

var
  GLib: ISSLLibrary = nil;
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  SkippedTests: Integer = 0;
  GOriginalBIONew: TBIO_new = nil;
  GOriginalPEMWriteBioX509: TPEM_write_bio_X509 = nil;
  GBIONewCallCount: Integer = 0;

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

function BuildOptions: TCertGenOptions;
begin
  Result := TCertificateUtils.DefaultGenOptions;
  Result.CommonName := 'selfsigned-certificate-export-pem-write-bio-x509-contract.local';
  Result.Organization := 'fafafa.ssl contract';
  Result.ValidDays := 30;
end;

procedure WarmupGenerateSelfSigned(const AOptions: TCertGenOptions);
var
  LCertPEM: string;
  LKeyPEM: string;
begin
  if not TCertificateUtils.GenerateSelfSigned(AOptions, LCertPEM, LKeyPEM) then
    raise Exception.Create('GenerateSelfSigned warmup returned False');
  if (LCertPEM = '') or (LKeyPEM = '') then
    raise Exception.Create('GenerateSelfSigned warmup returned empty PEM output');
end;

function DisablePEMWriteX509AfterCertificateExportBIOConstructor(const AType: PBIO_METHOD): PBIO; cdecl;
begin
  Inc(GBIONewCallCount);
  Result := GOriginalBIONew(AType);
  if (GBIONewCallCount = 1) and (Result <> nil) then
    PEM_write_bio_X509 := nil;
end;

procedure InstallCertificateExportPEMWriteFailureWrapper;
begin
  GBIONewCallCount := 0;
  PEM_write_bio_X509 := GOriginalPEMWriteBioX509;
  BIO_new := @DisablePEMWriteX509AfterCertificateExportBIOConstructor;
end;

procedure AssertGenerateSelfSignedControlledFailure(
  const AName: string;
  const AOptions: TCertGenOptions
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
  LTrySimpleRaised: Boolean;
  LTrySimpleDetail: string;
  LTrySimpleResult: Boolean;
begin
  InstallCertificateExportPEMWriteFailureWrapper;

  LRaised := False;
  LControlled := False;
  LDetail := '';
  LCertPEM := '';
  LKeyPEM := '';
  try
    TCertificateUtils.GenerateSelfSigned(AOptions, LCertPEM, LKeyPEM);
  except
    on E: Exception do
    begin
      LRaised := True;
      LControlled := E is ESSLCertError;
      LDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' should raise', LRaised,
    'expected GenerateSelfSigned(...) to fail');
  AssertTrue(AName + ' should raise controlled ESSLCertError', LControlled, LDetail);

  InstallCertificateExportPEMWriteFailureWrapper;

  LTryRaised := False;
  LTryDetail := '';
  LTryResult := True;
  LCertPEM := 'sentinel-cert';
  LKeyPEM := 'sentinel-key';
  try
    LTryResult := TCertificateUtils.TryGenerateSelfSigned(AOptions, LCertPEM, LKeyPEM);
  except
    on E: Exception do
    begin
      LTryRaised := True;
      LTryDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' TryGenerateSelfSigned should not raise', not LTryRaised, LTryDetail);
  AssertTrue(AName + ' TryGenerateSelfSigned should return False', not LTryResult,
    'expected TryGenerateSelfSigned to return False');
  AssertTrue(AName + ' TryGenerateSelfSigned should clear cert output', LCertPEM = '',
    'expected cleared certificate output');
  AssertTrue(AName + ' TryGenerateSelfSigned should clear key output', LKeyPEM = '',
    'expected cleared key output');

  InstallCertificateExportPEMWriteFailureWrapper;

  LTrySimpleRaised := False;
  LTrySimpleDetail := '';
  LTrySimpleResult := True;
  LCertPEM := 'sentinel-cert';
  LKeyPEM := 'sentinel-key';
  try
    LTrySimpleResult := TCertificateUtils.TryGenerateSelfSignedSimple(
      AOptions.CommonName,
      AOptions.Organization,
      AOptions.ValidDays,
      LCertPEM,
      LKeyPEM
    );
  except
    on E: Exception do
    begin
      LTrySimpleRaised := True;
      LTrySimpleDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' TryGenerateSelfSignedSimple should not raise',
    not LTrySimpleRaised, LTrySimpleDetail);
  AssertTrue(AName + ' TryGenerateSelfSignedSimple should return False', not LTrySimpleResult,
    'expected TryGenerateSelfSignedSimple to return False');
  AssertTrue(AName + ' TryGenerateSelfSignedSimple should clear cert output', LCertPEM = '',
    'expected cleared certificate output');
  AssertTrue(AName + ' TryGenerateSelfSignedSimple should clear key output', LKeyPEM = '',
    'expected cleared key output');
end;

procedure TestGenerateSelfSignedShouldFailGracefullyWhenCertificateExportPEMWriteBecomesUnavailable;
var
  LOptions: TCertGenOptions;
begin
  WriteLn;
  WriteLn('=== Certificate utils GenerateSelfSigned certificate PEM export PEM_write_bio_X509 symbol guard ===');

  if (not Assigned(BIO_new)) or (not Assigned(PEM_write_bio_X509)) then
  begin
    MarkSkip('certificate utils GenerateSelfSigned certificate PEM export PEM_write_bio_X509 contract',
      'required baseline BIO_new/PEM_write_bio_X509 helpers are unavailable');
    Exit;
  end;

  LOptions := BuildOptions;
  WarmupGenerateSelfSigned(LOptions);

  GOriginalBIONew := BIO_new;
  GOriginalPEMWriteBioX509 := PEM_write_bio_X509;
  try
    AssertGenerateSelfSignedControlledFailure(
      'GenerateSelfSigned when certificate PEM export PEM_write_bio_X509 becomes unavailable',
      LOptions
    );
  finally
    BIO_new := GOriginalBIONew;
    PEM_write_bio_X509 := GOriginalPEMWriteBioX509;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('Certificate Utils GenerateSelfSigned Certificate PEM Export PEM_write_bio_X509 Symbol Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('certificate utils GenerateSelfSigned certificate PEM export PEM_write_bio_X509 contract',
        'failed to initialize OpenSSL library');

    if SkippedTests = 0 then
    begin
      LoadOpenSSLCore();
      LoadOpenSSLBIO();
      LoadOpenSSLX509();
      LoadX509V3Functions(GetCryptoLibHandle);
      if not LoadOpenSSLPEM(GetCryptoLibHandle) then
        raise Exception.Create('failed to load PEM support');
      if not LoadEVP(GetCryptoLibHandle) then
        raise Exception.Create('failed to load EVP support');
    end;

    if SkippedTests = 0 then
      TestGenerateSelfSignedShouldFailGracefullyWhenCertificateExportPEMWriteBecomesUnavailable;

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
