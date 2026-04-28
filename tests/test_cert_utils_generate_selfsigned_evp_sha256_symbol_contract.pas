program test_cert_utils_generate_selfsigned_evp_sha256_symbol_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl,
  fafafa.ssl.exceptions,
  fafafa.ssl.cert.utils,
  fafafa.ssl.openssl.loader,
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
  Result.CommonName := 'selfsigned-evp-sha256-contract.local';
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

procedure TestGenerateSelfSignedShouldFailControlledWhenEVPSHA256IsUnavailable;
var
  LOptions: TCertGenOptions;
  LOriginalEVPSHA256: TEVP_sha256;
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
  WriteLn;
  WriteLn('=== Certificate utils GenerateSelfSigned EVP_sha256 symbol guard ===');

  if (not Assigned(X509_sign)) or
     (not Assigned(EVP_sha256)) or
     (not Assigned(BIO_new)) or
     (not Assigned(BIO_s_mem)) or
     (not Assigned(BIO_free)) or
     (not Assigned(PEM_write_bio_X509)) or
     (not Assigned(PEM_write_bio_PrivateKey)) then
  begin
    MarkSkip('certificate utils selfsigned EVP_sha256 symbol contract',
      'required baseline OpenSSL signing/export helpers are unavailable');
    Exit;
  end;

  LOptions := BuildOptions;
  WarmupGenerateSelfSigned(LOptions);

  LOriginalEVPSHA256 := EVP_sha256;
  try
    EVP_sha256 := nil;

    LRaised := False;
    LControlled := False;
    LDetail := '';
    LCertPEM := '';
    LKeyPEM := '';
    try
      TCertificateUtils.GenerateSelfSigned(LOptions, LCertPEM, LKeyPEM);
    except
      on E: Exception do
      begin
        LRaised := True;
        LControlled := E is ESSLCertError;
        LDetail := E.ClassName + ': ' + E.Message;
      end;
    end;

    AssertTrue('GenerateSelfSigned when EVP_sha256 is unavailable should raise',
      LRaised, 'expected GenerateSelfSigned(...) to fail');
    AssertTrue('GenerateSelfSigned when EVP_sha256 is unavailable should raise controlled ESSLCertError',
      LControlled, LDetail);

    LTryRaised := False;
    LTryDetail := '';
    LTryResult := True;
    LCertPEM := 'sentinel-cert';
    LKeyPEM := 'sentinel-key';
    try
      LTryResult := TCertificateUtils.TryGenerateSelfSigned(LOptions, LCertPEM, LKeyPEM);
    except
      on E: Exception do
      begin
        LTryRaised := True;
        LTryDetail := E.ClassName + ': ' + E.Message;
      end;
    end;

    AssertTrue('TryGenerateSelfSigned when EVP_sha256 is unavailable should not raise',
      not LTryRaised, LTryDetail);
    AssertTrue('TryGenerateSelfSigned when EVP_sha256 is unavailable should return False',
      not LTryResult, 'expected TryGenerateSelfSigned to return False');
    AssertTrue('TryGenerateSelfSigned when EVP_sha256 is unavailable should clear cert output',
      LCertPEM = '', 'expected cleared certificate output');
    AssertTrue('TryGenerateSelfSigned when EVP_sha256 is unavailable should clear key output',
      LKeyPEM = '', 'expected cleared key output');

    LTrySimpleRaised := False;
    LTrySimpleDetail := '';
    LTrySimpleResult := True;
    LCertPEM := 'sentinel-cert';
    LKeyPEM := 'sentinel-key';
    try
      LTrySimpleResult := TCertificateUtils.TryGenerateSelfSignedSimple(
        LOptions.CommonName,
        LOptions.Organization,
        LOptions.ValidDays,
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

    AssertTrue('TryGenerateSelfSignedSimple when EVP_sha256 is unavailable should not raise',
      not LTrySimpleRaised, LTrySimpleDetail);
    AssertTrue('TryGenerateSelfSignedSimple when EVP_sha256 is unavailable should return False',
      not LTrySimpleResult, 'expected TryGenerateSelfSignedSimple to return False');
    AssertTrue('TryGenerateSelfSignedSimple when EVP_sha256 is unavailable should clear cert output',
      LCertPEM = '', 'expected cleared certificate output');
    AssertTrue('TryGenerateSelfSignedSimple when EVP_sha256 is unavailable should clear key output',
      LKeyPEM = '', 'expected cleared key output');
  finally
    EVP_sha256 := LOriginalEVPSHA256;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('Certificate Utils GenerateSelfSigned EVP_sha256 Symbol Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('certificate utils selfsigned EVP_sha256 symbol contract',
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
      TestGenerateSelfSignedShouldFailControlledWhenEVPSHA256IsUnavailable;

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
      WriteLn('FATAL: ', E.ClassName, ': ', E.Message);
      Halt(2);
    end;
  end;
end.
