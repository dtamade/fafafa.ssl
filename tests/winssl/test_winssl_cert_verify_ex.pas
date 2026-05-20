program test_winssl_cert_verify_ex;

{$mode objfpc}{$H+}

uses
  Windows,
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.winssl.certificate;

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

function ResolveRepoFixturePath(const ARepoRelativePath: string): string;
const
  CandidatePrefixes: array[0..3] of string = (
    '',
    '../',
    '../../',
    '../../../'
  );
var
  I: Integer;
  LCandidate: string;
begin
  Result := '';
  for I := Low(CandidatePrefixes) to High(CandidatePrefixes) do
  begin
    LCandidate := ExpandFileName(CandidatePrefixes[I] + ARepoRelativePath);
    if FileExists(LCandidate) then
    begin
      Result := LCandidate;
      Exit;
    end;
  end;
end;

function FormatVerifyState(AVerified: Boolean; const AResult: TSSLCertVerifyResult): string;
begin
  Result := Format(
    'actual verified=%s success=%s error=%d msg=%s details=%s',
    [
      BoolToStr(AVerified, True),
      BoolToStr(AResult.Success, True),
      AResult.ErrorCode,
      AResult.ErrorMessage,
      AResult.DetailedInfo
    ]
  );
end;

function CreateMemoryBackedStore: TWinSSLCertificateStore;
var
  LStoreHandle: HCERTSTORE;
begin
  Result := nil;
  LStoreHandle := CertOpenStore(
    CERT_STORE_PROV_MEMORY,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    0,
    nil
  );
  if LStoreHandle <> nil then
    Result := TWinSSLCertificateStore.Create(LStoreHandle, True);
end;

function CreateWinSSLCertificate: ISSLCertificate;
begin
  Result := TWinSSLCertificate.Create(nil, False);
  Check(Result <> nil, 'WinSSL certificate object should be created');
end;

function LoadFixtureCertificate(const ARepoRelativePath, ALabel: string): ISSLCertificate;
var
  LFixturePath: string;
begin
  Result := CreateWinSSLCertificate;
  LFixturePath := ResolveRepoFixturePath(ARepoRelativePath);
  Check(LFixturePath <> '', ALabel + ' fixture path should resolve');
  Check(Result.LoadFromFile(LFixturePath), ALabel + ' fixture should load');
end;

procedure TestIgnoreExpiryIsPerCallAndHonored;
var
  LExpiredLeaf: ISSLCertificate;
  LCACert: ISSLCertificate;
  LStore: TWinSSLCertificateStore;
  LVerifyResult: TSSLCertVerifyResult;
  LVerified: Boolean;
  LDiag: string;
begin
  WriteLn('=== WinSSL VerifyEx Flag Parity ===');

  LExpiredLeaf := LoadFixtureCertificate('tests/certs/expired-signer.pem', 'Expired leaf');
  LCACert := LoadFixtureCertificate('tests/certificate/test_certs/ca_cert.pem', 'CA');

  LStore := CreateMemoryBackedStore;
  Check(LStore <> nil, 'WinSSL memory-backed store should be created');
  Check(LStore.AddCertificate(LCACert), 'CA fixture should be added to memory-backed store');

  LVerified := LExpiredLeaf.VerifyEx(LStore, [], LVerifyResult);
  Check((not LVerified) and (not LVerifyResult.Success),
    'Expired leaf without IgnoreExpiry should fail; ' + FormatVerifyState(LVerified, LVerifyResult));
  LDiag := LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo;
  Check(
    ContainsTextInsensitive(LDiag, 'expired') or
    ContainsTextInsensitive(LDiag, 'time'),
    'Expired leaf failure should expose an expiry diagnostic'
  );

  LVerified := LExpiredLeaf.VerifyEx(LStore, [sslCertVerifyIgnoreExpiry], LVerifyResult);
  Check(LVerified and LVerifyResult.Success,
    'Expired leaf with IgnoreExpiry should succeed; ' + FormatVerifyState(LVerified, LVerifyResult));

  LVerified := LExpiredLeaf.VerifyEx(LStore, [], LVerifyResult);
  Check((not LVerified) and (not LVerifyResult.Success),
    'IgnoreExpiry must stay per-call and not leak into a later unflagged verify; ' +
      FormatVerifyState(LVerified, LVerifyResult));
end;

procedure TestAllowSelfSignedIsPerCallAndHonored;
var
  LSelfSignedLeaf: ISSLCertificate;
  LEmptyStore: TWinSSLCertificateStore;
  LVerifyResult: TSSLCertVerifyResult;
  LVerified: Boolean;
  LDiag: string;
begin
  LSelfSignedLeaf := LoadFixtureCertificate('tests/certs/version1-cert.pem', 'Self-signed leaf');

  LEmptyStore := CreateMemoryBackedStore;
  Check(LEmptyStore <> nil, 'Empty WinSSL memory-backed store should be created');

  LVerified := LSelfSignedLeaf.VerifyEx(LEmptyStore, [], LVerifyResult);
  Check((not LVerified) and (not LVerifyResult.Success),
    'Self-signed leaf without AllowSelfSigned should fail; ' + FormatVerifyState(LVerified, LVerifyResult));
  LDiag := LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo;
  Check(
    ContainsTextInsensitive(LDiag, 'untrusted') or
    ContainsTextInsensitive(LDiag, 'trusted') or
    ContainsTextInsensitive(LDiag, 'root'),
    'Self-signed failure should expose a trust diagnostic'
  );

  LVerified := LSelfSignedLeaf.VerifyEx(LEmptyStore, [sslCertVerifyAllowSelfSigned], LVerifyResult);
  Check(LVerified and LVerifyResult.Success,
    'Self-signed leaf with AllowSelfSigned should succeed; ' + FormatVerifyState(LVerified, LVerifyResult));

  LVerified := LSelfSignedLeaf.VerifyEx(LEmptyStore, [], LVerifyResult);
  Check((not LVerified) and (not LVerifyResult.Success),
    'AllowSelfSigned must stay per-call and not leak into a later unflagged verify; ' +
      FormatVerifyState(LVerified, LVerifyResult));
end;

procedure TestStrictChainRequiresServerAuthUsage;
var
  LLeafCert: ISSLCertificate;
  LCACert: ISSLCertificate;
  LStore: TWinSSLCertificateStore;
  LVerifyResult: TSSLCertVerifyResult;
  LVerified: Boolean;
  LDiag: string;
begin
  LLeafCert := LoadFixtureCertificate('tests/certificate/test_certs/signer_cert.pem', 'CA-signed leaf');
  LCACert := LoadFixtureCertificate('tests/certificate/test_certs/ca_cert.pem', 'CA');

  LStore := CreateMemoryBackedStore;
  Check(LStore <> nil, 'Strict-chain memory-backed store should be created');
  Check(LStore.AddCertificate(LCACert), 'Strict-chain CA fixture should be added to store');

  LVerified := LLeafCert.VerifyEx(LStore, [], LVerifyResult);
  Check(LVerified and LVerifyResult.Success,
    'CA-signed leaf without strict-chain should succeed; ' + FormatVerifyState(LVerified, LVerifyResult));

  LVerified := LLeafCert.VerifyEx(LStore, [sslCertVerifyStrictChain], LVerifyResult);
  Check((not LVerified) and (not LVerifyResult.Success),
    'Strict-chain should fail when the leaf certificate lacks serverAuth EKU; ' +
      FormatVerifyState(LVerified, LVerifyResult));
  LDiag := LVerifyResult.ErrorMessage + ' ' + LVerifyResult.DetailedInfo;
  Check(
    ContainsTextInsensitive(LDiag, 'strict') or
    ContainsTextInsensitive(LDiag, 'serverauth') or
    ContainsTextInsensitive(LDiag, 'extended key usage'),
    'Strict-chain failure should mention strict-chain or serverAuth extended key usage'
  );
end;

begin
  try
    TestIgnoreExpiryIsPerCallAndHonored;
    TestAllowSelfSignedIsPerCallAndHonored;
    TestStrictChainRequiresServerAuthUsage;
    WriteLn;
    WriteLn('[PASS] WinSSL VerifyEx ignore-expiry / allow-self-signed / strict-chain parity contract is satisfied.');
  except
    on E: Exception do
    begin
      WriteLn('[FAIL] Unhandled exception: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
