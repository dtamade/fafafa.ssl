program test_crl_revocation_semantics;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder,
  fafafa.ssl.openssl.cert.builder,
  fafafa.ssl.cert.advanced,
  fafafa.ssl,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pem;

const
  FIXTURE_CRL = './tests/fixtures/p2/crl/revoked_list_keycompromise_v1.txt';
  FIXTURE_CERT = './tests/fixtures/p2/crl/revoked_cert_keycompromise_v1.txt';

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  GCRLNowForTest: TDateTime = 0;

function TestCRLNowProvider: TDateTime;
begin
  Result := GCRLNowForTest;
end;

type
  TNilX509Certificate = class(TInterfacedObject, fafafa.ssl.openssl.cert.builder.ICertificateEx)
  public
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetSubjectAltNames: TStringArray;
    function IsCA: Boolean;
    function IsValidAt(ATime: TDateTime): Boolean;
    function IsExpired: Boolean;
    function ToPEM: string;
    function ToDER: TBytes;
    procedure SaveToFile(const AFile: string);
    function GetX509Handle: Pointer;
  end;

function TNilX509Certificate.GetSubject: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetIssuer: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetSerialNumber: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetNotBefore: TDateTime;
begin
  Result := 0;
end;

function TNilX509Certificate.GetNotAfter: TDateTime;
begin
  Result := 0;
end;

function TNilX509Certificate.GetSubjectAltNames: TStringArray;
begin
  SetLength(Result, 0);
end;

function TNilX509Certificate.IsCA: Boolean;
begin
  Result := False;
end;

function TNilX509Certificate.IsValidAt(ATime: TDateTime): Boolean;
begin
  Result := False;
end;

function TNilX509Certificate.IsExpired: Boolean;
begin
  Result := True;
end;

function TNilX509Certificate.ToPEM: string;
begin
  Result := '';
end;

function TNilX509Certificate.ToDER: TBytes;
begin
  SetLength(Result, 0);
end;

procedure TNilX509Certificate.SaveToFile(const AFile: string);
begin
  // No-op test double.
end;

function TNilX509Certificate.GetX509Handle: Pointer;
begin
  Result := nil;
end;

procedure Pass(const AName: string);
begin
  Inc(TestsPassed);
  WriteLn('[PASS] ', AName);
end;

procedure Fail(const AName, ADetail: string);
begin
  Inc(TestsFailed);
  WriteLn('[FAIL] ', AName, ': ', ADetail);
end;

procedure Check(const AName: string; ACondition: Boolean; const ADetail: string = '');
begin
  if ACondition then
    Pass(AName)
  else
    Fail(AName, ADetail);
end;

var
  LLib: ISSLLibrary;
  LCRL: ICRLManager;
  LCRLNoNextUpdate: ICRLManager;
  LCert: ICertificate;
  LMissingFileRaised: Boolean;
  LEmptyFileRaised: Boolean;
  LEmptyPEMRaised: Boolean;
  LWhitespacePEMRaised: Boolean;
  LEmptyPEMReloadRaised: Boolean;
  LWhitespacePEMReloadRaised: Boolean;
  LNilCertRaised: Boolean;
  LNilRevokedDateRaised: Boolean;
  LNilReasonRaised: Boolean;
  LNilX509HandleRaised: Boolean;
  LNoCRLIsRevokedRaised: Boolean;
  LNoCRLRevokedDateRaised: Boolean;
  LNoCRLReasonRaised: Boolean;
  LInvalidReloadRaised: Boolean;
  LMissingBIOApiRaised: Boolean;
  LMissingGetByCertApiRaised: Boolean;
  LMissingCRLFreeApiRaised: Boolean;
  LEmptyCRLFile: string;
  LRevokedAt: TDateTime;
  LNextUpdate: TDateTime;
  LReason: string;
  LNilX509Cert: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LSavedBIO_new_mem_buf: TBIO_new_mem_buf;
  LSavedCRLFreeAccessor: TX509_CRL_free;
  LSavedGetByCertAccessor: TX509_CRL_get0_by_cert;
  LSavedRevocationDateAccessor: TX509_REVOKED_get0_revocationDate;
  LSavedRevocationReasonAccessor: TX509_REVOKED_get_ext_d2i;
  LSavedASN1IntegerGetAccessor: TASN1_INTEGER_get;
  LSavedASN1IntegerGetInt64Accessor: TASN1_INTEGER_get_int64;
  LSavedASN1StringLengthAccessor: TASN1_STRING_length;
  LSavedASN1StringGet0DataAccessor: TASN1_STRING_get0_data;
  LSavedNextUpdateAccessor: TX509_CRL_get0_nextUpdate;
  LSavedCRLNowProviderHook: TCRLNowProvider;
begin
  WriteLn('====================================');
  WriteLn('  CRL Revocation Semantics Test');
  WriteLn('====================================');
  WriteLn;

  Check('fixture crl exists', FileExists(FIXTURE_CRL), FIXTURE_CRL);
  Check('fixture cert exists', FileExists(FIXTURE_CERT), FIXTURE_CERT);

  try
    LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (LLib = nil) or (not LLib.Initialize) then
    begin
      Fail('openssl initialize', 'failed to initialize openssl backend');
      Halt(1);
    end;

    try
      LoadOpenSSLCore;
      LoadOpenSSLBIO;
      LoadOpenSSLX509;
      Check('load openssl x509 module', Assigned(X509_CRL_get0_by_cert), 'X509_CRL_get0_by_cert not available');
      Check('load openssl asn1 module', LoadOpenSSLASN1(GetCryptoLibHandle), 'LoadOpenSSLASN1 failed');
      Check('load openssl pem module', LoadOpenSSLPEM(GetCryptoLibHandle), 'LoadOpenSSLPEM failed');
      Check('BIO_new_mem_buf available', Assigned(BIO_new_mem_buf), 'BIO_new_mem_buf missing');
      Check('PEM_read_bio_X509_CRL available', Assigned(PEM_read_bio_X509_CRL), 'PEM_read_bio_X509_CRL missing');
    except
      on E: Exception do
      begin
        Fail('openssl low-level module setup', E.Message);
        Halt(1);
      end;
    end;

    // Missing low-level API contract:
    // If BIO_new_mem_buf is unavailable, CRL parse path must fail with a controlled error,
    // not access violation.
    LSavedBIO_new_mem_buf := BIO_new_mem_buf;
    BIO_new_mem_buf := nil;
    LMissingBIOApiRaised := False;
    try
      LCRLNoNextUpdate := CreateCRLManager;
      Check('create crl manager for missing BIO api contract',
        LCRLNoNextUpdate <> nil, 'CreateCRLManager returned nil');
      if LCRLNoNextUpdate <> nil then
        LCRLNoNextUpdate.LoadFromPEM(
          '-----BEGIN X509 CRL-----' + LineEnding +
          'INVALID' + LineEnding +
          '-----END X509 CRL-----');
    except
      on E: Exception do
      begin
        LMissingBIOApiRaised := True;
        Check('missing BIO api returns controlled error',
          Pos('BIO_new_mem_buf', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    BIO_new_mem_buf := LSavedBIO_new_mem_buf;
    Check('missing BIO api must fail closed', LMissingBIOApiRaised,
      'expected controlled exception when BIO_new_mem_buf is unavailable');

    LCRL := CreateCRLManager;
    Check('create crl manager', LCRL <> nil, 'CreateCRLManager returned nil');
    if LCRL = nil then
      Halt(1);

    // No-CRL-loaded contract:
    // all revocation APIs must fail closed with consistent no-CRL semantic.
    LNoCRLIsRevokedRaised := False;
    try
      LCRL.IsRevoked(nil);
    except
      on E: Exception do
      begin
        LNoCRLIsRevokedRaised := True;
        Check('isRevoked no-crl returns invalid-data semantic',
          Pos('CRL (no CRL loaded)', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('isRevoked no-crl must fail closed', LNoCRLIsRevokedRaised, 'expected controlled exception');

    LNoCRLRevokedDateRaised := False;
    try
      LCRL.GetRevokedDate(nil);
    except
      on E: Exception do
      begin
        LNoCRLRevokedDateRaised := True;
        Check('getRevokedDate no-crl returns invalid-data semantic',
          Pos('CRL (no CRL loaded)', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('getRevokedDate no-crl must fail closed', LNoCRLRevokedDateRaised, 'expected controlled exception');

    LNoCRLReasonRaised := False;
    try
      LCRL.GetRevocationReason(nil);
    except
      on E: Exception do
      begin
        LNoCRLReasonRaised := True;
        Check('getRevocationReason no-crl returns invalid-data semantic',
          Pos('CRL (no CRL loaded)', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('getRevocationReason no-crl must fail closed', LNoCRLReasonRaised,
      'expected controlled exception');

    // File loading boundary contracts:
    // 1) Missing file must fail with a controlled load error.
    // 2) Empty file must fail with explicit invalid-data semantic.
    LMissingFileRaised := False;
    try
      LCRL.LoadFromFile('./tests/fixtures/p2/crl/nonexistent_contract_input.crl');
    except
      on E: Exception do
      begin
        LMissingFileRaised := True;
        Check('missing crl file returns controlled load error',
          Pos('Failed to load', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('missing crl file must fail closed', LMissingFileRaised, 'expected controlled exception');

    LEmptyCRLFile := './tmp/test_crl_empty_input_contract.crl';
    with TFileStream.Create(LEmptyCRLFile, fmCreate) do
      Free;
    try
      LEmptyFileRaised := False;
      try
        LCRL.LoadFromFile(LEmptyCRLFile);
      except
        on E: Exception do
        begin
          LEmptyFileRaised := True;
          Check('empty crl file returns invalid-data semantic',
            Pos('Invalid or corrupted data', E.Message) > 0,
            'unexpected message: ' + E.Message);
        end;
      end;
      Check('empty crl file must fail closed', LEmptyFileRaised, 'expected controlled exception');
    finally
      if FileExists(LEmptyCRLFile) then
        DeleteFile(LEmptyCRLFile);
    end;

    // Input boundary contract:
    // Empty PEM payload must return explicit invalid-data semantic.
    LEmptyPEMRaised := False;
    try
      LCRL.LoadFromPEM('');
    except
      on E: Exception do
      begin
        LEmptyPEMRaised := True;
        Check('empty crl pem returns invalid-data semantic',
          Pos('Invalid or corrupted data', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('empty crl pem must fail closed', LEmptyPEMRaised, 'expected controlled exception');

    // Input boundary contract:
    // Whitespace-only PEM payload must keep explicit invalid-data semantic.
    LWhitespacePEMRaised := False;
    try
      LCRL.LoadFromPEM('   ' + #9 + LineEnding + LineEnding);
    except
      on E: Exception do
      begin
        LWhitespacePEMRaised := True;
        Check('whitespace-only crl pem returns invalid-data semantic',
          Pos('Invalid or corrupted data', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('whitespace-only crl pem must fail closed', LWhitespacePEMRaised,
      'expected controlled exception');

    try
      LCRL.LoadFromFile(FIXTURE_CRL);
      Pass('load crl from fixture');
    except
      on E: Exception do
      begin
        Fail('load crl from fixture', E.Message);
        Halt(1);
      end;
    end;

    LNextUpdate := LCRL.GetNextUpdate;
    Check('nextUpdate extracted from fixture', LNextUpdate > 0,
      Format('expected non-zero nextUpdate got=%s', [DateTimeToStr(LNextUpdate)]));

    // Revocation query argument contract:
    // nil certificate input must fail closed with controlled certificate-access semantic.
    LNilCertRaised := False;
    try
      LCRL.IsRevoked(nil);
    except
      on E: Exception do
      begin
        LNilCertRaised := True;
        Check('nil cert returns controlled certificate-access error',
          Pos('Certificate handle access', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('nil cert revocation query must fail closed', LNilCertRaised,
      'expected controlled exception for nil cert');

    LNilRevokedDateRaised := False;
    try
      LCRL.GetRevokedDate(nil);
    except
      on E: Exception do
      begin
        LNilRevokedDateRaised := True;
        Check('nil cert revoked-date returns controlled certificate-access error',
          Pos('Certificate handle access', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('nil cert revoked-date query must fail closed', LNilRevokedDateRaised,
      'expected controlled exception for nil cert');

    LNilReasonRaised := False;
    try
      LCRL.GetRevocationReason(nil);
    except
      on E: Exception do
      begin
        LNilReasonRaised := True;
        Check('nil cert revocation-reason returns controlled certificate-access error',
          Pos('Certificate handle access', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('nil cert revocation-reason query must fail closed', LNilReasonRaised,
      'expected controlled exception for nil cert');

    // Revocation query handle contract:
    // Certificate object with nil X509 handle must fail closed with controlled error.
    LNilX509Cert := TNilX509Certificate.Create;
    LNilX509HandleRaised := False;
    try
      LCRL.IsRevoked(ICertificate(LNilX509Cert));
    except
      on E: Exception do
      begin
        LNilX509HandleRaised := True;
        Check('nil x509 handle returns controlled certificate-access error',
          Pos('Certificate handle access', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('nil x509 handle revocation query must fail closed', LNilX509HandleRaised,
      'expected controlled exception for nil X509 handle');

    // Reload boundary contract:
    // Empty PEM reload must fail closed and clear loaded CRL state.
    LEmptyPEMReloadRaised := False;
    try
      LCRL.LoadFromPEM('');
    except
      on E: Exception do
      begin
        LEmptyPEMReloadRaised := True;
        Check('empty crl pem reload returns invalid-data semantic',
          Pos('Invalid or corrupted data', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('empty crl pem reload must fail closed', LEmptyPEMReloadRaised,
      'expected controlled exception');
    LNextUpdate := LCRL.GetNextUpdate;
    Check('nextUpdate cleared after empty pem reload failure', LNextUpdate = 0,
      Format('expected 0 got=%s', [DateTimeToStr(LNextUpdate)]));
    Check('isExpired true after empty pem reload failure', LCRL.IsExpired,
      'empty pem reload failure should leave manager in no-CRL state');

    // Re-load fixture so subsequent contracts can exercise reload paths on loaded CRL.
    LCRL.LoadFromFile(FIXTURE_CRL);
    LWhitespacePEMReloadRaised := False;
    try
      LCRL.LoadFromPEM('   ' + #9 + LineEnding);
    except
      on E: Exception do
      begin
        LWhitespacePEMReloadRaised := True;
        Check('whitespace-only crl pem reload returns invalid-data semantic',
          Pos('Invalid or corrupted data', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('whitespace-only crl pem reload must fail closed', LWhitespacePEMReloadRaised,
      'expected controlled exception');
    LNextUpdate := LCRL.GetNextUpdate;
    Check('nextUpdate cleared after whitespace pem reload failure', LNextUpdate = 0,
      Format('expected 0 got=%s', [DateTimeToStr(LNextUpdate)]));
    Check('isExpired true after whitespace pem reload failure', LCRL.IsExpired,
      'whitespace pem reload failure should leave manager in no-CRL state');

    // Re-load fixture again so subsequent contracts can exercise reload paths on loaded CRL.
    LCRL.LoadFromFile(FIXTURE_CRL);
    LNextUpdate := LCRL.GetNextUpdate;

    // Boundary semantics contract:
    // CRL is considered expired at boundary (now == nextUpdate), and not expired
    // immediately before boundary.
    LSavedCRLNowProviderHook := CRLNowProviderHook;
    CRLNowProviderHook := @TestCRLNowProvider;
    try
      GCRLNowForTest := LNextUpdate;
      Check('isExpired true when now equals nextUpdate', LCRL.IsExpired,
        'expected expired at boundary now==nextUpdate');

      GCRLNowForTest := IncSecond(LNextUpdate, -1);
      Check('isExpired false when now is before nextUpdate', not LCRL.IsExpired,
        'expected not expired before nextUpdate');
    finally
      CRLNowProviderHook := LSavedCRLNowProviderHook;
    end;

    // Missing low-level API contract:
    // When replacing an already loaded CRL, missing X509_CRL_free must produce
    // a controlled unsupported error (no access violation).
    LSavedCRLFreeAccessor := X509_CRL_free;
    X509_CRL_free := nil;
    LMissingCRLFreeApiRaised := False;
    try
      LCRL.LoadFromPEM(
        '-----BEGIN X509 CRL-----' + LineEnding +
        'INVALID' + LineEnding +
        '-----END X509 CRL-----');
    except
      on E: Exception do
      begin
        LMissingCRLFreeApiRaised := True;
        Check('missing crl_free api returns controlled error',
          Pos('X509_CRL_free', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    X509_CRL_free := LSavedCRLFreeAccessor;
    Check('missing crl_free api must fail closed', LMissingCRLFreeApiRaised,
      'expected controlled exception when X509_CRL_free is unavailable');

    LInvalidReloadRaised := False;
    try
      LCRL.LoadFromPEM(
        '-----BEGIN X509 CRL-----' + LineEnding +
        'INVALID' + LineEnding +
        '-----END X509 CRL-----');
    except
      on E: Exception do
      begin
        LInvalidReloadRaised := True;
        Pass('invalid crl reload raises parse error');
      end;
    end;
    Check('invalid crl reload must fail', LInvalidReloadRaised, 'expected parse error');

    LNextUpdate := LCRL.GetNextUpdate;
    Check('nextUpdate cleared after failed crl reload', LNextUpdate = 0,
      Format('expected 0 got=%s', [DateTimeToStr(LNextUpdate)]));
    Check('isExpired true after failed crl reload', LCRL.IsExpired,
      'failed reload should leave manager in no-CRL state');

    // Re-load fixture for remaining metadata contracts.
    LCRL.LoadFromFile(FIXTURE_CRL);

    try
      LCert := TCertificate.LoadFromFile(FIXTURE_CERT);
    except
      on E: Exception do
      begin
        Fail('load fixture certificate', E.Message);
        Halt(1);
      end;
    end;
    Check('load fixture certificate', LCert <> nil, 'TCertificate.LoadFromFile returned nil');
    if LCert = nil then
      Halt(1);

    // Missing low-level API contract:
    // If revoked-entry query API is unavailable, revocation check must fail with a controlled
    // unsupported error (no access violation).
    LSavedGetByCertAccessor := X509_CRL_get0_by_cert;
    X509_CRL_get0_by_cert := nil;
    LMissingGetByCertApiRaised := False;
    try
      LCRL.IsRevoked(LCert);
    except
      on E: Exception do
      begin
        LMissingGetByCertApiRaised := True;
        Check('missing get0_by_cert api returns controlled error',
          Pos('X509_CRL_get0_by_cert', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    X509_CRL_get0_by_cert := LSavedGetByCertAccessor;
    Check('missing get0_by_cert api must fail closed', LMissingGetByCertApiRaised,
      'expected controlled exception when X509_CRL_get0_by_cert is unavailable');

    Check('fixture cert is revoked', LCRL.IsRevoked(LCert), 'expected revoked certificate');

    LRevokedAt := LCRL.GetRevokedDate(LCert);
    Check('revoked date extracted', LRevokedAt > 0, 'expected non-zero revoked date');
    Check('revoked year is pinned fixture year', YearOf(LRevokedAt) = 2024,
      Format('expected year=2024 got=%d', [YearOf(LRevokedAt)]));

    LReason := LCRL.GetRevocationReason(LCert);
    Check('revocation reason extracted', LReason = 'KeyCompromise',
      Format('expected KeyCompromise got=%s', [LReason]));

    // Unknown semantics contract:
    // If revocation reason accessor is unavailable at runtime, reason must remain
    // explicit unknown ('') instead of synthesized fallback value.
    LSavedRevocationReasonAccessor := X509_REVOKED_get_ext_d2i;
    X509_REVOKED_get_ext_d2i := nil;
    try
      LReason := LCRL.GetRevocationReason(LCert);
      Check('revocation reason unknown when accessor is missing', LReason = '',
        Format('expected empty reason got=%s', [LReason]));
    finally
      X509_REVOKED_get_ext_d2i := LSavedRevocationReasonAccessor;
    end;

    // Unknown semantics contract:
    // If all reason decode accessors are unavailable, reason must remain explicit
    // unknown ('') instead of synthetic placeholders.
    LSavedASN1IntegerGetAccessor := ASN1_INTEGER_get;
    LSavedASN1IntegerGetInt64Accessor := ASN1_INTEGER_get_int64;
    LSavedASN1StringLengthAccessor := ASN1_STRING_length;
    LSavedASN1StringGet0DataAccessor := ASN1_STRING_get0_data;
    ASN1_INTEGER_get := nil;
    ASN1_INTEGER_get_int64 := nil;
    ASN1_STRING_length := nil;
    ASN1_STRING_get0_data := nil;
    try
      LReason := LCRL.GetRevocationReason(LCert);
      Check('revocation reason unknown when decode accessors are missing', LReason = '',
        Format('expected empty reason got=%s', [LReason]));
    finally
      ASN1_INTEGER_get := LSavedASN1IntegerGetAccessor;
      ASN1_INTEGER_get_int64 := LSavedASN1IntegerGetInt64Accessor;
      ASN1_STRING_length := LSavedASN1StringLengthAccessor;
      ASN1_STRING_get0_data := LSavedASN1StringGet0DataAccessor;
    end;

    // Unknown semantics contract:
    // If revocation date accessor is unavailable at runtime, date must remain explicit unknown (0),
    // not synthesized current time.
    LSavedRevocationDateAccessor := X509_REVOKED_get0_revocationDate;
    X509_REVOKED_get0_revocationDate := nil;
    try
      LRevokedAt := LCRL.GetRevokedDate(LCert);
      Check('revoked date is unknown when accessor is missing', LRevokedAt = 0,
        Format('expected 0 got=%s', [DateTimeToStr(LRevokedAt)]));
    finally
      X509_REVOKED_get0_revocationDate := LSavedRevocationDateAccessor;
    end;

    // Unknown semantics contract:
    // If CRL next-update accessor is unavailable, manager should expose explicit unknown (0)
    // instead of synthesized future timestamp.
    LSavedNextUpdateAccessor := X509_CRL_get0_nextUpdate;
    X509_CRL_get0_nextUpdate := nil;
    try
      LCRLNoNextUpdate := CreateCRLManager;
      Check('create crl manager without nextUpdate accessor',
        LCRLNoNextUpdate <> nil, 'CreateCRLManager returned nil');
      if LCRLNoNextUpdate <> nil then
      begin
        LCRLNoNextUpdate.LoadFromFile(FIXTURE_CRL);
        LNextUpdate := LCRLNoNextUpdate.GetNextUpdate;
        Check('nextUpdate is unknown when accessor is missing', LNextUpdate = 0,
          Format('expected 0 got=%s', [DateTimeToStr(LNextUpdate)]));
      end;
    finally
      X509_CRL_get0_nextUpdate := LSavedNextUpdateAccessor;
    end;
  except
    on E: Exception do
      Fail('unexpected exception', E.Message);
  end;

  WriteLn;
  WriteLn('====================================');
  WriteLn(Format('Results: %d passed, %d failed', [TestsPassed, TestsFailed]));
  WriteLn('====================================');

  if TestsFailed > 0 then
    Halt(1);
end.
