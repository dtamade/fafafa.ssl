program test_capability_matrix_v12;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory;

var
  GBackendsExecuted: Integer = 0;
  GBackendsSkipped: Integer = 0;
  GBackendsErrors: Integer = 0;
  GSkipBackendUnavailable: Integer = 0;
  GContractChecks: Integer = 0;
  GContractFailures: Integer = 0;

function IsBackendUnavailableError(const AMessage: string): Boolean;
var
  LMsg: string;
begin
  LMsg := LowerCase(AMessage);
  Result := (Pos('not registered', LMsg) > 0) or
            (Pos('not enabled', LMsg) > 0) or
            (Pos('backend not available', LMsg) > 0) or
            (Pos('failed to load', LMsg) > 0);
end;

procedure RecordContract(const AName: string; ACondition: Boolean; const AFailureDetail: string = '');
begin
  Inc(GContractChecks);
  if ACondition then
    WriteLn('  [PASS] ', AName)
  else
  begin
    Inc(GContractFailures);
    if AFailureDetail <> '' then
      WriteLn('  [FAIL] ', AName, ' - ', AFailureDetail)
    else
      WriteLn('  [FAIL] ', AName);
  end;
end;

procedure TestBackendCapabilities(const ABackendName: string; AType: TSSLLibraryType);
var
  Lib: ISSLLibrary;
  Caps: TSSLBackendCapabilities;
  Desc: string;
  LKnownGood: Boolean;
  LKnownBad: Boolean;
  LEmptyName: Boolean;
begin
  WriteLn('========================================');
  WriteLn('Testing: ', ABackendName);
  WriteLn('========================================');

  try
    Lib := TSSLFactory.GetLibrary(AType);
    if not Assigned(Lib) then
    begin
      Inc(GBackendsSkipped);
      Inc(GSkipBackendUnavailable);
      WriteLn('  [SKIP] [backend-not-available] ', ABackendName, ' backend not available');
      WriteLn;
      Exit;
    end;

    if not Lib.Initialize then
    begin
      Inc(GBackendsSkipped);
      Inc(GSkipBackendUnavailable);
      WriteLn('  [SKIP] [backend-not-available] ', ABackendName,
        ' initialize failed: ', Lib.GetLastErrorString);
      WriteLn;
      Exit;
    end;

    Inc(GBackendsExecuted);

    Caps := Lib.GetCapabilities;

    WriteLn('[v1.1.0 Fields - Backward Compatibility]');
    WriteLn('  SupportsTLS13: ', Caps.SupportsTLS13);
    WriteLn('  SupportsALPN: ', Caps.SupportsALPN);
    WriteLn('  SupportsSNI: ', Caps.SupportsSNI);
    WriteLn('  SupportsOCSPStapling: ', Caps.SupportsOCSPStapling);
    WriteLn('  SupportsECDHE: ', Caps.SupportsECDHE);
    WriteLn('  MinTLSVersion: ', Ord(Caps.MinTLSVersion));
    WriteLn('  MaxTLSVersion: ', Ord(Caps.MaxTLSVersion));
    WriteLn;

    WriteLn('[v1.2.0 New Fields]');
    WriteLn('  BackendType: ', Ord(Caps.BackendType));
    WriteLn('  BackendImplType: ', Ord(Caps.BackendImplType));
    WriteLn('  BackendVersion: ', Caps.BackendVersion);
    WriteLn('  SupportsDTLS: ', Caps.SupportsDTLS);
    WriteLn;

    WriteLn('[Feature Support Levels]');
    WriteLn('  SNISupport: ', Ord(Caps.SNISupport));
    WriteLn('  ALPNSupport: ', Ord(Caps.ALPNSupport));
    WriteLn('  OCSPStaplingSupport: ', Ord(Caps.OCSPStaplingSupport));
    WriteLn('  CertTransparencySupport: ', Ord(Caps.CertTransparencySupport));
    WriteLn('  SessionTicketsSupport: ', Ord(Caps.SessionTicketsSupport));
    WriteLn;

    WriteLn('[Algorithm Support]');
    WriteLn('  Ciphers: ', IsCipherSupported(Caps, sslCipherAES256), ' (AES256), ',
            IsCipherSupported(Caps, sslCipherCHACHA20_POLY1305), ' (ChaCha20)');
    WriteLn('  Hashes: ', IsHashSupported(Caps, sslHashSHA256), ' (SHA256), ',
            IsHashSupported(Caps, sslHashSHA512), ' (SHA512)');
    WriteLn('  KeyExchange: ', IsKeyExchangeSupported(Caps, sslKexECDHE_RSA), ' (ECDHE-RSA)');
    WriteLn;

    WriteLn('[Cipher API Contract]');
    LKnownGood := Lib.IsCipherSupported('TLS_AES_128_GCM_SHA256');
    LKnownBad := Lib.IsCipherSupported('TLS_FAKE_AES_128_GCM_SHA256');
    LEmptyName := Lib.IsCipherSupported('');
    WriteLn('  KnownGood(TLS_AES_128_GCM_SHA256): ', LKnownGood);
    WriteLn('  FakeCipher(TLS_FAKE_AES_128_GCM_SHA256): ', LKnownBad);
    WriteLn('  EmptyName(""): ', LEmptyName);

    if AType = sslWinSSL then
    begin
      RecordContract('WinSSL known-good deferred true semantics', LKnownGood,
        'WinSSL currently defers cipher enforcement to runtime handshake policy');
      RecordContract('WinSSL fake-cipher deferred true semantics', LKnownBad,
        'WinSSL currently defers cipher enforcement to runtime handshake policy');
      RecordContract('WinSSL empty-name deferred true semantics', LEmptyName,
        'WinSSL currently defers cipher enforcement to runtime handshake policy');
    end
    else
    begin
      RecordContract(ABackendName + ' known-good cipher accepted', LKnownGood,
        'Known TLS1.3 cipher should be accepted');
      RecordContract(ABackendName + ' fake cipher rejected', not LKnownBad,
        'Unknown fake cipher should be rejected');
      RecordContract(ABackendName + ' empty name rejected', not LEmptyName,
        'Empty cipher name should be rejected');
    end;
    WriteLn;

    WriteLn('[Performance & Security Features]');
    WriteLn('  HasHardwareAcceleration: ', Caps.HasHardwareAcceleration);
    WriteLn('  HasSIMDOptimization: ', Caps.HasSIMDOptimization);
    WriteLn('  HasConstantTimeOperations: ', Caps.HasConstantTimeOperations);
    WriteLn('  SupportsFIPSMode: ', Caps.SupportsFIPSMode);
    WriteLn;

    WriteLn('[Platform Features]');
    WriteLn('  RequiresExternalLibrary: ', Caps.RequiresExternalLibrary);
    WriteLn('  SupportsSystemCertStore: ', Caps.SupportsSystemCertStore);
    WriteLn('  SupportsPKCS11: ', Caps.SupportsPKCS11);
    WriteLn('  SupportsTPM: ', Caps.SupportsTPM);
    WriteLn;

    WriteLn('[Helper Function Tests]');
    WriteLn('  IsNativeBackend: ', IsNativeBackend(Caps));
    WriteLn('  IsCLibraryBackend: ', IsCLibraryBackend(Caps));
    WriteLn('  RequiresExternalDependencies: ', RequiresExternalDependencies(Caps));
    WriteLn('  SecurityScore: ', GetSecurityScore(Caps), '/100');
    WriteLn('  PerformanceScore: ', GetPerformanceScore(Caps), '/100');
    WriteLn;

    WriteLn('[Capabilities Description]');
    Desc := GetCapabilitiesDescription(Caps);
    WriteLn(Desc);
    WriteLn;

    Lib.Finalize;
  except
    on E: Exception do
    begin
      if IsBackendUnavailableError(E.Message) then
      begin
        Inc(GBackendsSkipped);
        Inc(GSkipBackendUnavailable);
        WriteLn('  [SKIP] [backend-not-available] ', ABackendName, ' backend unavailable: ', E.Message);
      end
      else
      begin
        Inc(GBackendsErrors);
        WriteLn('  [ERROR] ', E.ClassName, ': ', E.Message);
      end;
      WriteLn;
    end;
  end;
end;

begin
  WriteLn('fafafa.ssl - Capability Matrix v1.2.0 Test');
  WriteLn('==========================================');
  WriteLn;

  TestBackendCapabilities('OpenSSL', sslOpenSSL);
  TestBackendCapabilities('WolfSSL', sslWolfSSL);
  TestBackendCapabilities('MbedTLS', sslMbedTLS);
  TestBackendCapabilities('WinSSL', sslWinSSL);

  WriteLn('========================================');
  WriteLn('Backends executed: ', GBackendsExecuted);
  WriteLn('Backends skipped:  ', GBackendsSkipped,
    ' (backend-not-available=', GSkipBackendUnavailable, ')');
  WriteLn('Backends errors:   ', GBackendsErrors);
  WriteLn('Contract checks:   ', GContractChecks);
  WriteLn('Contract failures: ', GContractFailures);

  if (GBackendsErrors = 0) and (GContractFailures = 0) then
    WriteLn('✅ Capability matrix contract checks passed')
  else
    WriteLn('❌ Capability matrix contract checks failed');

  WriteLn('========================================');

  if (GBackendsErrors > 0) or (GContractFailures > 0) then
    Halt(1);
end.
