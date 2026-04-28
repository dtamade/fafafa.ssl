program test_cert_utils_getinfo_x509_get_notbefore_symbol_contract;

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
  fafafa.ssl.openssl.api.evp;

const
  CERT_FIXTURE_PATH = 'tests/certificate/test_certs/signer_cert.pem';

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
  out AExpectedVersion: Integer
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
    if not Assigned(LInfo.SubjectAltNames) then
      raise Exception.Create('GetInfo warmup returned nil SubjectAltNames');
    AExpectedSubject := LInfo.Subject;
    AExpectedIssuer := LInfo.Issuer;
    AExpectedVersion := LInfo.Version;
  finally
    if Assigned(LInfo.SubjectAltNames) then
      LInfo.SubjectAltNames.Free;
  end;
end;

procedure AssertNotBeforeGuardInfo(
  const AName: string;
  const AInfo: TCertInfo;
  const AExpectedSubject: string;
  const AExpectedIssuer: string;
  const AExpectedVersion: Integer
);
begin
  AssertTrue(AName + ' should preserve subject', AInfo.Subject = AExpectedSubject,
    'Subject=' + AInfo.Subject);
  AssertTrue(AName + ' should preserve issuer', AInfo.Issuer = AExpectedIssuer,
    'Issuer=' + AInfo.Issuer);
  AssertTrue(AName + ' should preserve version', AInfo.Version = AExpectedVersion,
    'Version=' + IntToStr(AInfo.Version));
  AssertTrue(AName + ' should return empty serial number', AInfo.SerialNumber = '',
    'SerialNumber=' + AInfo.SerialNumber);
  AssertTrue(AName + ' should return empty key usage', AInfo.KeyUsage = '',
    'KeyUsage=' + AInfo.KeyUsage);
  AssertTrue(AName + ' should keep NotBefore at default', AInfo.NotBefore = 0,
    'NotBefore=' + DateTimeToStr(AInfo.NotBefore));
  AssertTrue(AName + ' should keep NotAfter at default', AInfo.NotAfter = 0,
    'NotAfter=' + DateTimeToStr(AInfo.NotAfter));
  AssertTrue(AName + ' should allocate SubjectAltNames', Assigned(AInfo.SubjectAltNames),
    'SubjectAltNames=nil');
  if Assigned(AInfo.SubjectAltNames) then
    AssertTrue(AName + ' should keep SubjectAltNames empty', AInfo.SubjectAltNames.Count = 0,
      'Count=' + IntToStr(AInfo.SubjectAltNames.Count));
end;

procedure AssertGetInfoSafeDegrade(
  const AName, APEM, AExpectedSubject, AExpectedIssuer: string;
  const AExpectedVersion: Integer
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
      AssertNotBeforeGuardInfo(AName, LInfo, AExpectedSubject, AExpectedIssuer, AExpectedVersion);
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
      AssertNotBeforeGuardInfo(
        AName + ' Try wrapper',
        LTryInfo,
        AExpectedSubject,
        AExpectedIssuer,
        AExpectedVersion
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

procedure TestGetInfoShouldFailGracefullyWhenX509GetNotBeforeIsUnavailable;
var
  LFixturePEM: string;
  LExpectedSubject: string;
  LExpectedIssuer: string;
  LExpectedVersion: Integer;
  LOriginalX509GetNotBefore: TX509_get_notBefore;
begin
  WriteLn;
  WriteLn('=== Certificate utils GetInfo X509_get_notBefore symbol guard ===');

  if (not Assigned(BIO_new_mem_buf)) or
     (not Assigned(BIO_free)) or
     (not Assigned(PEM_read_bio_X509)) or
     (not Assigned(X509_get_subject_name)) or
     (not Assigned(X509_get_issuer_name)) or
     (not Assigned(X509_get_version)) or
     (not Assigned(X509_get_notBefore)) then
  begin
    MarkSkip('certificate utils getinfo X509_get_notBefore symbol contract',
      'required baseline OpenSSL BIO/PEM/X509 helpers are unavailable');
    Exit;
  end;

  LFixturePEM := LoadFixturePEM;
  if LFixturePEM = '' then
    raise Exception.Create('certificate fixture is empty');

  WarmupGetInfo(LFixturePEM, LExpectedSubject, LExpectedIssuer, LExpectedVersion);

  LOriginalX509GetNotBefore := X509_get_notBefore;
  X509_get_notBefore := nil;
  try
    AssertGetInfoSafeDegrade(
      'GetInfo when X509_get_notBefore is unavailable',
      LFixturePEM,
      LExpectedSubject,
      LExpectedIssuer,
      LExpectedVersion
    );
  finally
    X509_get_notBefore := LOriginalX509GetNotBefore;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('Certificate Utils GetInfo X509_get_notBefore Symbol Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('certificate utils getinfo X509_get_notBefore symbol contract',
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
      TestGetInfoShouldFailGracefullyWhenX509GetNotBeforeIsUnavailable;

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
