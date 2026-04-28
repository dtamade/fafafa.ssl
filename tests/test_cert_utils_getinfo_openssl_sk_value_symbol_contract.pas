program test_cert_utils_getinfo_openssl_sk_value_symbol_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl,
  fafafa.ssl.cert.utils,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.stack;

const
  CERT_FIXTURE_PATH = 'tests/certs/san-test.pem';

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

function LoadFixturePEM: string;
var
  LText: TStringList;
begin
  LText := TStringList.Create;
  try
    LText.LoadFromFile(CERT_FIXTURE_PATH);
    Result := LText.Text;
  finally
    LText.Free;
  end;
end;

procedure WarmupGetInfo(
  const APEM: string;
  out AExpectedSubject: string;
  out AExpectedIssuer: string;
  out AExpectedVersion: Integer;
  out AExpectedNotBefore: TDateTime;
  out AExpectedNotAfter: TDateTime;
  out AExpectedSerialNumber: string;
  out AExpectedSignatureAlgorithm: string;
  out AExpectedPublicKeyType: string;
  out AExpectedPublicKeyBits: Integer;
  out AExpectedIsCA: Boolean;
  out AExpectedKeyUsage: string
);
var
  LInfo: TCertInfo;
begin
  LInfo := TCertificateUtils.GetInfo(APEM);
  try
    if LInfo.Subject = '' then
      raise Exception.Create('GetInfo warmup returned empty subject');
    if LInfo.Issuer = '' then
      raise Exception.Create('GetInfo warmup returned empty issuer');
    if LInfo.Version = 0 then
      raise Exception.Create('GetInfo warmup returned zero version');
    if LInfo.NotBefore = 0 then
      raise Exception.Create('GetInfo warmup returned zero NotBefore');
    if LInfo.NotAfter = 0 then
      raise Exception.Create('GetInfo warmup returned zero NotAfter');
    if LInfo.SerialNumber = '' then
      raise Exception.Create('GetInfo warmup returned empty serial number');
    if LInfo.PublicKeyType = '' then
      raise Exception.Create('GetInfo warmup returned empty public key type');
    if LInfo.PublicKeyBits = 0 then
      raise Exception.Create('GetInfo warmup returned zero public key bits');
    if LInfo.KeyUsage = '' then
      raise Exception.Create('GetInfo warmup returned empty key usage');
    if not Assigned(LInfo.SubjectAltNames) then
      raise Exception.Create('GetInfo warmup returned nil SubjectAltNames');
    if LInfo.SubjectAltNames.Count = 0 then
      raise Exception.Create('GetInfo warmup returned empty SubjectAltNames');
    AExpectedSubject := LInfo.Subject;
    AExpectedIssuer := LInfo.Issuer;
    AExpectedVersion := LInfo.Version;
    AExpectedNotBefore := LInfo.NotBefore;
    AExpectedNotAfter := LInfo.NotAfter;
    AExpectedSerialNumber := LInfo.SerialNumber;
    AExpectedSignatureAlgorithm := LInfo.SignatureAlgorithm;
    AExpectedPublicKeyType := LInfo.PublicKeyType;
    AExpectedPublicKeyBits := LInfo.PublicKeyBits;
    AExpectedIsCA := LInfo.IsCA;
    AExpectedKeyUsage := LInfo.KeyUsage;
  finally
    if Assigned(LInfo.SubjectAltNames) then
      LInfo.SubjectAltNames.Free;
  end;
end;

procedure AssertOpenSSLStackValueGuardInfo(
  const AName: string;
  const AInfo: TCertInfo;
  const AExpectedSubject: string;
  const AExpectedIssuer: string;
  const AExpectedVersion: Integer;
  const AExpectedNotBefore: TDateTime;
  const AExpectedNotAfter: TDateTime;
  const AExpectedSerialNumber: string;
  const AExpectedSignatureAlgorithm: string;
  const AExpectedPublicKeyType: string;
  const AExpectedPublicKeyBits: Integer;
  const AExpectedIsCA: Boolean;
  const AExpectedKeyUsage: string
);
begin
  AssertTrue(AName + ' should preserve subject', AInfo.Subject = AExpectedSubject,
    'Subject=' + AInfo.Subject);
  AssertTrue(AName + ' should preserve issuer', AInfo.Issuer = AExpectedIssuer,
    'Issuer=' + AInfo.Issuer);
  AssertTrue(AName + ' should preserve version', AInfo.Version = AExpectedVersion,
    'Version=' + IntToStr(AInfo.Version));
  AssertTrue(AName + ' should preserve NotBefore', AInfo.NotBefore = AExpectedNotBefore,
    'NotBefore=' + DateTimeToStr(AInfo.NotBefore));
  AssertTrue(AName + ' should preserve NotAfter', AInfo.NotAfter = AExpectedNotAfter,
    'NotAfter=' + DateTimeToStr(AInfo.NotAfter));
  AssertTrue(AName + ' should preserve serial number', AInfo.SerialNumber = AExpectedSerialNumber,
    'SerialNumber=' + AInfo.SerialNumber);
  AssertTrue(AName + ' should preserve signature algorithm',
    AInfo.SignatureAlgorithm = AExpectedSignatureAlgorithm,
    'SignatureAlgorithm=' + AInfo.SignatureAlgorithm);
  AssertTrue(AName + ' should preserve public key type',
    AInfo.PublicKeyType = AExpectedPublicKeyType,
    'PublicKeyType=' + AInfo.PublicKeyType);
  AssertTrue(AName + ' should preserve public key bits',
    AInfo.PublicKeyBits = AExpectedPublicKeyBits,
    'PublicKeyBits=' + IntToStr(AInfo.PublicKeyBits));
  AssertTrue(AName + ' should preserve IsCA',
    AInfo.IsCA = AExpectedIsCA,
    'IsCA=' + BoolToStr(AInfo.IsCA, True));
  AssertTrue(AName + ' should preserve key usage',
    AInfo.KeyUsage = AExpectedKeyUsage,
    'KeyUsage=' + AInfo.KeyUsage);
  AssertTrue(AName + ' should allocate SubjectAltNames', Assigned(AInfo.SubjectAltNames),
    'SubjectAltNames=nil');
  if Assigned(AInfo.SubjectAltNames) then
    AssertTrue(AName + ' should keep SubjectAltNames empty', AInfo.SubjectAltNames.Count = 0,
      'Count=' + IntToStr(AInfo.SubjectAltNames.Count));
end;

procedure AssertGetInfoSafeDegrade(
  const AName, APEM, AExpectedSubject, AExpectedIssuer: string;
  const AExpectedVersion: Integer;
  const AExpectedNotBefore: TDateTime;
  const AExpectedNotAfter: TDateTime;
  const AExpectedSerialNumber: string;
  const AExpectedSignatureAlgorithm: string;
  const AExpectedPublicKeyType: string;
  const AExpectedPublicKeyBits: Integer;
  const AExpectedIsCA: Boolean;
  const AExpectedKeyUsage: string
);
var
  LRaised: Boolean;
  LDetail: string;
  LInfo: TCertInfo;
  LTryRaised: Boolean;
  LTryDetail: string;
  LTryInfo: TCertInfo;
  LTryResult: Boolean;
begin
  LRaised := False;
  LDetail := '';
  try
    LInfo := TCertificateUtils.GetInfo(APEM);
  except
    on E: Exception do
    begin
      LRaised := True;
      LDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' should not raise', not LRaised, LDetail);
  if not LRaised then
  begin
    try
      AssertOpenSSLStackValueGuardInfo(
        AName,
        LInfo,
        AExpectedSubject,
        AExpectedIssuer,
        AExpectedVersion,
        AExpectedNotBefore,
        AExpectedNotAfter,
        AExpectedSerialNumber,
        AExpectedSignatureAlgorithm,
        AExpectedPublicKeyType,
        AExpectedPublicKeyBits,
        AExpectedIsCA,
        AExpectedKeyUsage
      );
    finally
      if Assigned(LInfo.SubjectAltNames) then
        LInfo.SubjectAltNames.Free;
    end;
  end;

  LTryRaised := False;
  LTryDetail := '';
  LTryResult := False;
  FillChar(LTryInfo, SizeOf(LTryInfo), 0);
  try
    LTryResult := TCertificateUtils.TryGetInfo(APEM, LTryInfo);
  except
    on E: Exception do
    begin
      LTryRaised := True;
      LTryDetail := E.ClassName + ': ' + E.Message;
    end;
  end;

  AssertTrue(AName + ' Try wrapper should not raise', not LTryRaised, LTryDetail);
  if not LTryRaised then
  begin
    try
      AssertOpenSSLStackValueGuardInfo(
        AName + ' Try wrapper',
        LTryInfo,
        AExpectedSubject,
        AExpectedIssuer,
        AExpectedVersion,
        AExpectedNotBefore,
        AExpectedNotAfter,
        AExpectedSerialNumber,
        AExpectedSignatureAlgorithm,
        AExpectedPublicKeyType,
        AExpectedPublicKeyBits,
        AExpectedIsCA,
        AExpectedKeyUsage
      );
      AssertTrue(AName + ' Try wrapper should return True',
        LTryResult,
        'TryGetInfo returned False');
    finally
      if Assigned(LTryInfo.SubjectAltNames) then
        LTryInfo.SubjectAltNames.Free;
    end;
  end;
end;

procedure TestGetInfoShouldFailGracefullyWhenOpenSSLStackValueIsUnavailable;
var
  LFixturePEM: string;
  LExpectedSubject: string;
  LExpectedIssuer: string;
  LExpectedVersion: Integer;
  LExpectedNotBefore: TDateTime;
  LExpectedNotAfter: TDateTime;
  LExpectedSerialNumber: string;
  LExpectedSignatureAlgorithm: string;
  LExpectedPublicKeyType: string;
  LExpectedPublicKeyBits: Integer;
  LExpectedIsCA: Boolean;
  LExpectedKeyUsage: string;
  LOriginalOpenSSLStackValue: TOPENSSL_sk_value;
begin
  WriteLn;
  WriteLn('=== Certificate utils GetInfo OPENSSL_sk_value symbol guard ===');

  if (not Assigned(BIO_new_mem_buf)) or
     (not Assigned(BIO_free)) or
     (not Assigned(PEM_read_bio_X509)) or
     (not Assigned(X509_get_subject_name)) or
     (not Assigned(X509_get_issuer_name)) or
     (not Assigned(X509_get_version)) or
     (not Assigned(X509_get_notBefore)) or
     (not Assigned(X509_get_notAfter)) or
     (not Assigned(X509_get_serialNumber)) or
     (not Assigned(X509_get_pubkey)) or
     (not Assigned(EVP_PKEY_get_id)) or
     (not Assigned(EVP_PKEY_get_bits)) or
     (not Assigned(X509_get_ext_d2i)) or
     (not Assigned(OPENSSL_sk_num)) or
     (not Assigned(OPENSSL_sk_value)) then
  begin
    MarkSkip('certificate utils getinfo OPENSSL_sk_value symbol contract',
      'required baseline OpenSSL BIO/PEM/X509/EVP/stack helpers are unavailable');
    Exit;
  end;

  LFixturePEM := LoadFixturePEM;
  if LFixturePEM = '' then
    raise Exception.Create('certificate fixture is empty');

  WarmupGetInfo(
    LFixturePEM,
    LExpectedSubject,
    LExpectedIssuer,
    LExpectedVersion,
    LExpectedNotBefore,
    LExpectedNotAfter,
    LExpectedSerialNumber,
    LExpectedSignatureAlgorithm,
    LExpectedPublicKeyType,
    LExpectedPublicKeyBits,
    LExpectedIsCA,
    LExpectedKeyUsage
  );

  LOriginalOpenSSLStackValue := OPENSSL_sk_value;
  OPENSSL_sk_value := nil;
  try
    AssertGetInfoSafeDegrade(
      'GetInfo when OPENSSL_sk_value is unavailable',
      LFixturePEM,
      LExpectedSubject,
      LExpectedIssuer,
      LExpectedVersion,
      LExpectedNotBefore,
      LExpectedNotAfter,
      LExpectedSerialNumber,
      LExpectedSignatureAlgorithm,
      LExpectedPublicKeyType,
      LExpectedPublicKeyBits,
      LExpectedIsCA,
      LExpectedKeyUsage
    );
  finally
    OPENSSL_sk_value := LOriginalOpenSSLStackValue;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('Certificate Utils GetInfo OPENSSL_sk_value Symbol Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('certificate utils getinfo OPENSSL_sk_value symbol contract',
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
      LoadStackFunctions();
    end;

    if SkippedTests = 0 then
      TestGetInfoShouldFailGracefullyWhenOpenSSLStackValueIsUnavailable;

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
      WriteLn('FATAL: ', E.ClassName + ': ' + E.Message);
      Halt(2);
    end;
  end;
end.
