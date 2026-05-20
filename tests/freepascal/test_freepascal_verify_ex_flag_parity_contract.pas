program test_freepascal_verify_ex_flag_parity_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
  WriteLn('[PASS] ', AMessage);
end;

function ContainsTextInsensitive(const AText, ASubText: string): Boolean;
begin
  Result := Pos(LowerCase(ASubText), LowerCase(AText)) > 0;
end;

function CreateFreePascalCertificate: ISSLCertificate;
var
  LLib: ISSLLibrary;
begin
  LLib := TSSLFactory.GetLibrary(sslFreePascal);
  Check(LLib <> nil, 'FreePascal library should be available');
  Result := LLib.CreateCertificate;
  Check(Result <> nil, 'FreePascal certificate instance should be created');
end;

function CreateFreePascalStore: ISSLCertificateStore;
var
  LLib: ISSLLibrary;
begin
  LLib := TSSLFactory.GetLibrary(sslFreePascal);
  Check(LLib <> nil, 'FreePascal library should be available for store creation');
  Result := LLib.CreateCertificateStore;
  Check(Result <> nil, 'FreePascal certificate store should be created');
end;

procedure TestAllowSelfSignedIsHonored;
var
  LLeafCert: ISSLCertificate;
  LEmptyStore: ISSLCertificateStore;
  LVerifyResult: TSSLCertVerifyResult;
  LVerified: Boolean;
begin
  WriteLn('=== FreePascal VerifyEx AllowSelfSigned / OCSP Parity ===');

  LLeafCert := CreateFreePascalCertificate;
  Check(
    LLeafCert.LoadFromFile('tests/certs/version1-cert.pem'),
    'Self-signed FreePascal fixture should load'
  );

  LEmptyStore := CreateFreePascalStore;

  LVerified := LLeafCert.VerifyEx(LEmptyStore, [], LVerifyResult);
  Check(
    (not LVerified) and (not LVerifyResult.Success),
    'Self-signed leaf without AllowSelfSigned should fail'
  );
  Check(
    ContainsTextInsensitive(LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo, 'verification') or
    ContainsTextInsensitive(LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo, 'chain') or
    ContainsTextInsensitive(LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo, 'trusted'),
    'Self-signed failure should expose trust diagnostic'
  );

  LVerified := LLeafCert.VerifyEx(LEmptyStore, [sslCertVerifyAllowSelfSigned], LVerifyResult);
  Check(
    LVerified and LVerifyResult.Success,
    Format(
      'Self-signed leaf with AllowSelfSigned should succeed; actual verified=%s success=%s error=%d msg=%s details=%s',
      [
        BoolToStr(LVerified, True),
        BoolToStr(LVerifyResult.Success, True),
        LVerifyResult.ErrorCode,
        LVerifyResult.ErrorMessage,
        LVerifyResult.DetailedInfo
      ]
    )
  );
end;

procedure TestOCSPFailsClosed;
var
  LLeafCert: ISSLCertificate;
  LCACert: ISSLCertificate;
  LStore: ISSLCertificateStore;
  LVerifyResult: TSSLCertVerifyResult;
  LVerified: Boolean;
begin
  LLeafCert := CreateFreePascalCertificate;
  Check(
    LLeafCert.LoadFromFile('tests/certificate/test_certs/signer_cert.pem'),
    'CA-signed FreePascal leaf fixture should load'
  );

  LCACert := CreateFreePascalCertificate;
  Check(
    LCACert.LoadFromFile('tests/certificate/test_certs/ca_cert.pem'),
    'FreePascal CA fixture should load'
  );

  LStore := CreateFreePascalStore;
  Check(
    LStore.AddCertificate(LCACert),
    'FreePascal CA fixture should be added to store'
  );

  LVerified := LLeafCert.VerifyEx(LStore, [], LVerifyResult);
  Check(
    LVerified and LVerifyResult.Success,
    'CA-signed leaf without OCSP flag should succeed'
  );

  LVerified := LLeafCert.VerifyEx(LStore, [sslCertVerifyCheckOCSP], LVerifyResult);
  Check(
    (not LVerified) and (not LVerifyResult.Success),
    Format(
      'CheckOCSP should fail-closed when certificate-level OCSP verification is unavailable; actual verified=%s success=%s error=%d msg=%s details=%s',
      [
        BoolToStr(LVerified, True),
        BoolToStr(LVerifyResult.Success, True),
        LVerifyResult.ErrorCode,
        LVerifyResult.ErrorMessage,
        LVerifyResult.DetailedInfo
      ]
    )
  );
  Check(
    ContainsTextInsensitive(LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo, 'ocsp') or
    ContainsTextInsensitive(LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo, 'revocation'),
    'CheckOCSP fail-closed diagnostic should mention OCSP or revocation'
  );
end;

begin
  try
    TestAllowSelfSignedIsHonored;
    TestOCSPFailsClosed;
    WriteLn;
    WriteLn('[PASS] FreePascal VerifyEx AllowSelfSigned / OCSP parity contract is satisfied.');
  except
    on E: Exception do
    begin
      WriteLn('[FAIL] Unhandled exception: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
