program test_openssl_features;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.consts;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function StubSetMinProtoPolicy(ctx: PSSL_CTX; version: Integer): Integer; cdecl;
begin
  if (version = TLS1_VERSION) or (version = TLS1_1_VERSION) then
    Result := 0
  else
    Result := 1;
end;

function StubSetMaxProtoPolicy(ctx: PSSL_CTX; version: Integer): Integer; cdecl;
begin
  if (version = TLS1_VERSION) or (version = TLS1_1_VERSION) then
    Result := 0
  else
    Result := 1;
end;

procedure TestCertificateStore;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Count: Integer;
  LoadedSystem: Boolean;
begin
  WriteLn;
  WriteLn('Testing Certificate Store');
  WriteLn('==========================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  Store := SSLLib.CreateCertificateStore;
  Require(Store <> nil, 'CreateCertificateStore returned nil');
  WriteLn('Store created: TRUE');

  LoadedSystem := Store.LoadSystemStore;
  WriteLn('System store loaded: ', BoolToStr(LoadedSystem, True));

  Count := Store.GetCount;
  Require(Count >= 0, 'Certificate count must be >= 0');
  WriteLn('Certificate count: ', Count);

  Require(not Store.VerifyCertificate(nil),
    'VerifyCertificate(nil) should return False');
  WriteLn('VerifyCertificate(nil): FALSE (contract verified)');

  WriteLn('✅ Certificate Store test completed');
end;

procedure TestSession;
var
  SSLLib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  WriteLn;
  WriteLn('Testing Session Serialization');
  WriteLn('==============================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  Ctx := SSLLib.CreateContext(sslCtxClient);
  Require(Ctx <> nil, 'CreateContext returned nil');
  WriteLn('Context created: TRUE');

  Ctx.SetSessionCacheMode(True);
  Require(Ctx.GetSessionCacheMode,
    'Session cache should be enabled after SetSessionCacheMode(True)');

  Ctx.SetSessionTimeout(1800);
  Require(Ctx.GetSessionTimeout = 1800,
    'Session timeout should persist after SetSessionTimeout');

  Ctx.SetSessionCacheSize(256);
  Require(Ctx.GetSessionCacheSize = 256,
    'Session cache size should persist after SetSessionCacheSize');

  WriteLn('[SKIP] Session object retrieval requires a completed TLS handshake');
  WriteLn('✅ Session test completed (context session contracts verified)');
end;

procedure TestCertificateVerify;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
  LoadedSystem: Boolean;
  Chain: TSSLCertificateArray;
begin
  WriteLn;
  WriteLn('Testing Certificate Verification');
  WriteLn('=================================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  Store := SSLLib.CreateCertificateStore;
  Require(Store <> nil, 'CreateCertificateStore returned nil');

  LoadedSystem := Store.LoadSystemStore;
  WriteLn('System store loaded for verification: ', BoolToStr(LoadedSystem, True));

  Require(Store.GetCount >= 0, 'Store.GetCount should be >= 0');

  Require(not Store.VerifyCertificate(nil),
    'VerifyCertificate(nil) should return False');

  Chain := Store.BuildCertificateChain(nil);
  Require(Length(Chain) = 0,
    'BuildCertificateChain(nil) should return empty chain');

  WriteLn('[SKIP] End-to-end certificate verification requires real certificate chain');
  WriteLn('✅ Verification test completed (store contracts verified)');
end;

procedure TestCipherSupportContract;
var
  SSLLib: ISSLLibrary;
  LKnownGood: Boolean;
  LKnownBad: Boolean;
begin
  WriteLn;
  WriteLn('Testing Cipher Support Contract');
  WriteLn('===============================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  LKnownGood := SSLLib.IsCipherSupported('TLS_AES_128_GCM_SHA256');
  Require(LKnownGood,
    'TLS_AES_128_GCM_SHA256 should be reported supported on initialized OpenSSL backend');

  LKnownBad := SSLLib.IsCipherSupported('TLS_FAKE_AES_128_GCM_SHA256');
  Require(not LKnownBad,
    'Unknown fake cipher must not be accepted only because name contains AES/GCM keywords');

  WriteLn('✅ Cipher support contract verified');
end;

procedure TestFeatureSupportRuntimeDriftContract;
var
  SSLLib: ISSLLibrary;
  LOrigSNI: TSSL_set_tlsext_host_name;
  LOrigALPN: TSSL_CTX_set_alpn_protos;
  LOrigReneg: TSSL_renegotiate;
begin
  WriteLn;
  WriteLn('Testing Feature Support Runtime Drift Contract');
  WriteLn('==============================================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  LOrigSNI := SSL_set_tlsext_host_name;
  if not Assigned(LOrigSNI) then
    WriteLn('[SKIP] SNI symbol unavailable in current build; skip pointer-drift check')
  else
  begin
    SSL_set_tlsext_host_name := nil;
    try
      Require(not SSLLib.IsFeatureSupported(sslFeatSNI),
        'SNI must be reported unsupported when SSL_set_tlsext_host_name is missing at runtime');
    finally
      SSL_set_tlsext_host_name := LOrigSNI;
    end;
  end;

  LOrigALPN := SSL_CTX_set_alpn_protos;
  if not Assigned(LOrigALPN) then
    WriteLn('[SKIP] ALPN symbol unavailable in current build; skip pointer-drift check')
  else
  begin
    SSL_CTX_set_alpn_protos := nil;
    try
      Require(not SSLLib.IsFeatureSupported(sslFeatALPN),
        'ALPN must be reported unsupported when SSL_CTX_set_alpn_protos is missing at runtime');
    finally
      SSL_CTX_set_alpn_protos := LOrigALPN;
    end;
  end;

  LOrigReneg := SSL_renegotiate;
  if not Assigned(LOrigReneg) then
    WriteLn('[SKIP] Renegotiation symbol unavailable in current build; skip pointer-drift check')
  else
  begin
    SSL_renegotiate := nil;
    try
      Require(not SSLLib.IsFeatureSupported(sslFeatRenegotiation),
        'Renegotiation must be reported unsupported when SSL_renegotiate is missing at runtime');
    finally
      SSL_renegotiate := LOrigReneg;
    end;
  end;

  WriteLn('✅ Feature runtime drift contract verified');
end;

procedure TestProtocolSupportPolicyAwareContract;
var
  SSLLib: ISSLLibrary;
  LOrigSetMin: TSSL_CTX_set_min_proto_version;
  LOrigSetMax: TSSL_CTX_set_max_proto_version;
begin
  WriteLn;
  WriteLn('Testing Protocol Support Policy-Aware Contract');
  WriteLn('==============================================');

  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  Require(SSLLib <> nil, 'OpenSSL library instance is nil');

  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;

  LOrigSetMin := SSL_CTX_set_min_proto_version;
  LOrigSetMax := SSL_CTX_set_max_proto_version;

  if (not Assigned(LOrigSetMin)) or (not Assigned(LOrigSetMax)) then
  begin
    WriteLn('[SKIP] Proto version setter symbols unavailable; skip policy-aware probe check');
    Exit;
  end;

  SSL_CTX_set_min_proto_version := @StubSetMinProtoPolicy;
  SSL_CTX_set_max_proto_version := @StubSetMaxProtoPolicy;
  try
    Require(not SSLLib.IsProtocolSupported(sslProtocolTLS10),
      'TLS1.0 should be unsupported when runtime policy rejects TLS1.0 setters');
    Require(not SSLLib.IsProtocolSupported(sslProtocolTLS11),
      'TLS1.1 should be unsupported when runtime policy rejects TLS1.1 setters');
    Require(SSLLib.IsProtocolSupported(sslProtocolTLS12),
      'TLS1.2 should remain supported when runtime policy allows TLS1.2 setters');
    Require(not SSLLib.IsProtocolSupported(sslProtocolUnknown),
      'Unknown protocol enum must never be reported as supported');
  finally
    SSL_CTX_set_min_proto_version := LOrigSetMin;
    SSL_CTX_set_max_proto_version := LOrigSetMax;
  end;

  WriteLn('✅ Protocol policy-aware contract verified');
end;

begin
  WriteLn('Testing OpenSSL Advanced Features');
  WriteLn('==================================');
  WriteLn;

  try
    TestCertificateStore;
    TestSession;
    TestCertificateVerify;
    TestCipherSupportContract;
    TestFeatureSupportRuntimeDriftContract;
    TestProtocolSupportPolicyAwareContract;

    WriteLn;
    WriteLn('===================');
    WriteLn('✅ All tests passed');
    WriteLn('===================');
  except
    on E: Exception do
    begin
      WriteLn('❌ Test failed: ', E.Message);
      Halt(1);
    end;
  end;
end.
