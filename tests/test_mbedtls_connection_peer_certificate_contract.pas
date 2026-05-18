program test_mbedtls_connection_peer_certificate_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.connection,
  fafafa.ssl.mbedtls.certificate,
  fafafa.ssl.mbedtls.native_handle;

var
  GLib: ISSLLibrary = nil;
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  SkippedTests: Integer = 0;
  GStubPeerCert: Pmbedtls_x509_crt = nil;

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

function StubMbedTLSSSLGetPeerCert(ssl: Pmbedtls_ssl_context): Pmbedtls_x509_crt; cdecl;
begin
  Result := GStubPeerCert;
end;

function CaptureCertHandle(const ACert: ISSLCertificate): Pointer;
var
  LNative: ISSLNativeHandleAccess;
begin
  Result := nil;
  if (ACert <> nil) and Supports(ACert, ISSLNativeHandleAccess, LNative) then
    Result := LNative.GetNativeHandle;
end;

procedure TestConnectionPeerCertificateMustMaterializeOwnedCopy;
var
  LFixture: TMbedTLSCertificate;
  LStream: TMemoryStream;
  LConn: TMbedTLSConnection;
  LCert: ISSLCertificate;
  LChain: TSSLCertificateArray;
  LExpectedFingerprint: string;
  LFixtureHandle: Pointer;
  LOriginalGetPeerCert: Tmbedtls_ssl_get_peer_cert;
  LOriginalParse: Tmbedtls_x509_crt_parse;
begin
  WriteLn;
  WriteLn('=== MbedTLS connection peer certificate materialization ===');

  if (not Assigned(mbedtls_ssl_set_bio)) or
     (not Assigned(mbedtls_x509_crt_parse)) then
  begin
    MarkSkip('mbedtls connection peer certificate contract',
      'required baseline MbedTLS SSL/X509 helpers are unavailable');
    Exit;
  end;

  LFixture := TMbedTLSCertificate.Create;
  if not LFixture.LoadFromFile('tests/certs/server-cert.pem') then
  begin
    LFixture.Free;
    MarkSkip('mbedtls connection peer certificate contract',
      'failed to load tests/certs/server-cert.pem fixture');
    Exit;
  end;

  LExpectedFingerprint := LFixture.GetFingerprintSHA256;
  LFixtureHandle := LFixture.GetNativeHandle;
  GStubPeerCert := Pmbedtls_x509_crt(LFixtureHandle);

  LOriginalGetPeerCert := mbedtls_ssl_get_peer_cert;
  LOriginalParse := mbedtls_x509_crt_parse;
  LStream := TMemoryStream.Create;
  LConn := nil;
  try
    LConn := TMbedTLSConnection.Create(nil, nil, LStream);

    mbedtls_ssl_get_peer_cert := @StubMbedTLSSSLGetPeerCert;

    LCert := LConn.GetPeerCertificate;
    AssertTrue('GetPeerCertificate should materialize a certificate',
      LCert <> nil);
    AssertTrue('GetPeerCertificate fingerprint should match the fixture',
      (LCert <> nil) and SameText(LCert.GetFingerprintSHA256, LExpectedFingerprint));
    AssertTrue('GetPeerCertificate must return an owned copy instead of the borrowed source handle',
      (LCert <> nil) and (CaptureCertHandle(LCert) <> nil) and
      (CaptureCertHandle(LCert) <> LFixtureHandle));

    LChain := LConn.GetPeerCertificateChain;
    AssertTrue('GetPeerCertificateChain should expose exactly the peer leaf',
      Length(LChain) = 1,
      Format('expected chain length 1 but got %d', [Length(LChain)]));
    AssertTrue('GetPeerCertificateChain leaf fingerprint should match the fixture',
      (Length(LChain) = 1) and SameText(LChain[0].GetFingerprintSHA256, LExpectedFingerprint));
    AssertTrue('GetPeerCertificateChain leaf must also be an owned copy',
      (Length(LChain) = 1) and (CaptureCertHandle(LChain[0]) <> nil) and
      (CaptureCertHandle(LChain[0]) <> LFixtureHandle));

    mbedtls_x509_crt_parse := nil;
    LCert := LConn.GetPeerCertificate;
    AssertTrue('GetPeerCertificate should fail closed when cert-copy helper is unavailable',
      LCert = nil);
    LChain := LConn.GetPeerCertificateChain;
    AssertTrue('GetPeerCertificateChain should fail closed when cert-copy helper is unavailable',
      Length(LChain) = 0,
      Format('expected empty chain but got %d entries', [Length(LChain)]));
  finally
    mbedtls_ssl_get_peer_cert := LOriginalGetPeerCert;
    mbedtls_x509_crt_parse := LOriginalParse;
    if Assigned(LConn) then
      LConn.Free;
    LStream.Free;
    LFixture.Free;
    GStubPeerCert := nil;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('MbedTLS Connection Peer Certificate Contract Test');
  WriteLn('========================================');

  try
    GLib := CreateMbedTLSLibrary;
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('mbedtls connection peer certificate contract',
        'failed to initialize MbedTLS library');

    if SkippedTests = 0 then
      TestConnectionPeerCertificateMustMaterializeOwnedCopy;

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
