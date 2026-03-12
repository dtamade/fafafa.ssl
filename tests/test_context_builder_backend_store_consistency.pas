program test_context_builder_backend_store_consistency;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils,
  fafafa.ssl.freepascal.context,
  fafafa.ssl.freepascal.context.material,
  fafafa.ssl.freepascal.lib;

{$I helpers/test_backend_store_fake_fixture.inc}

procedure AssertEquals(const AName: string; AExpected, AActual: Integer);
begin
  if AExpected <> AActual then
  begin
    WriteLn('[FAIL] ', AName, ' expected=', AExpected, ' actual=', AActual);
    Halt(1);
  end;
  WriteLn('[PASS] ', AName, ' = ', AActual);
end;

procedure AssertTrue(const AName: string; AValue: Boolean);
begin
  if not AValue then
  begin
    WriteLn('[FAIL] ', AName);
    Halt(1);
  end;
  WriteLn('[PASS] ', AName);
end;

procedure TestBuildClientUsesExplicitBackendStore;
var
  LContext: ISSLContext;
begin
  ResetCounters;
  RegisterFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithSystemRoots
      .BuildClient;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildClient returned nil');
      Halt(1);
    end;

    AssertEquals('explicit backend client context creates', 1, GCounters[sslFreePascal].ContextCreates);
    AssertEquals('explicit backend client store creates', 1, GCounters[sslFreePascal].StoreCreates);
    AssertEquals('default backend client store creates', 0, GCounters[sslMbedTLS].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerUsesExplicitBackendStoreAndLoadsPEM;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LCert, LKey: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithSystemRoots
      .WithCertificatePEM(LCert)
      .WithPrivateKeyPEM(LKey)
      .BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil');
      Halt(1);
    end;

    AssertEquals('explicit backend server context creates', 1, GCounters[sslFreePascal].ContextCreates);
    AssertEquals('explicit backend server store creates', 1, GCounters[sslFreePascal].StoreCreates);
    AssertEquals('default backend server store creates', 0, GCounters[sslMbedTLS].StoreCreates);

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] context should expose FreePascal material view');
      Halt(1);
    end;

    AssertTrue('server certificate PEM loaded into context material', LMaterial.HasCertificateMaterial);
    AssertTrue('server private key PEM loaded into context material', LMaterial.HasPrivateKeyMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;


procedure TestBuildClientUsesResolvedExplicitAutoDetectBackendStore;
var
  LContext: ISSLContext;
begin
  ResetCounters;
  RegisterDriftingFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslAutoDetect)
      .WithSystemRoots
      .BuildClient;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildClient returned nil');
      Halt(1);
    end;

    AssertEquals('resolved explicit autodetect client context creates on default backend', 1, GCounters[sslMbedTLS].ContextCreates);
    AssertEquals('resolved explicit autodetect client store creates on default backend', 1, GCounters[sslMbedTLS].StoreCreates);
    AssertEquals('resolved explicit autodetect client store does not drift to explicit backend', 0, GCounters[sslFreePascal].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerUsesResolvedExplicitAutoDetectBackendStore;
var
  LContext: ISSLContext;
  LCert, LKey: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-explicit-autodetect.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material');
    Halt(1);
  end;

  ResetCounters;
  RegisterDriftingFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslAutoDetect)
      .WithSystemRoots
      .WithCertificatePEM(LCert)
      .WithPrivateKeyPEM(LKey)
      .BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil');
      Halt(1);
    end;

    AssertEquals('resolved explicit autodetect server context creates on default backend', 1, GCounters[sslMbedTLS].ContextCreates);
    AssertEquals('resolved explicit autodetect server store creates on default backend', 1, GCounters[sslMbedTLS].StoreCreates);
    AssertEquals('resolved explicit autodetect server store does not drift to explicit backend', 0, GCounters[sslFreePascal].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildClientUsesResolvedImplicitDefaultBackendStore;
var
  LContext: ISSLContext;
begin
  ResetCounters;
  RegisterDriftingFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithSystemRoots
      .BuildClient;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildClient returned nil');
      Halt(1);
    end;

    AssertEquals('resolved implicit backend client context creates on default backend', 1, GCounters[sslMbedTLS].ContextCreates);
    AssertEquals('resolved implicit backend client store creates on default backend', 1, GCounters[sslMbedTLS].StoreCreates);
    AssertEquals('resolved implicit backend client store does not drift to explicit backend', 0, GCounters[sslFreePascal].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerUsesResolvedImplicitDefaultBackendStore;
var
  LContext: ISSLContext;
  LCert, LKey: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-drift.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material');
    Halt(1);
  end;

  ResetCounters;
  RegisterDriftingFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithSystemRoots
      .WithCertificatePEM(LCert)
      .WithPrivateKeyPEM(LKey)
      .BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil');
      Halt(1);
    end;

    AssertEquals('resolved implicit backend server context creates on default backend', 1, GCounters[sslMbedTLS].ContextCreates);
    AssertEquals('resolved implicit backend server store creates on default backend', 1, GCounters[sslMbedTLS].StoreCreates);
    AssertEquals('resolved implicit backend server store does not drift to explicit backend', 0, GCounters[sslFreePascal].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerUsesDefaultBackendStoreWhenImplicit;
var
  LContext: ISSLContext;
  LCert, LKey: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithSystemRoots
      .WithCertificatePEM(LCert)
      .WithPrivateKeyPEM(LKey)
      .BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil');
      Halt(1);
    end;

    AssertEquals('implicit backend server context creates on default backend', 1, GCounters[sslMbedTLS].ContextCreates);
    AssertEquals('implicit backend server store creates on default backend', 1, GCounters[sslMbedTLS].StoreCreates);
    AssertEquals('implicit backend server store does not use explicit backend', 0, GCounters[sslFreePascal].StoreCreates);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerPrefersPrivateKeyPEMOverFile;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LCert, LKey: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-prefer-pem.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material for private key precedence test');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LContext := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificatePEM(LCert)
      .WithPrivateKey('/definitely/missing/private-key.pem')
      .WithPrivateKeyPEM(LKey)
      .BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil when PEM private key should override file');
      Halt(1);
    end;

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] context should expose FreePascal material view for private key precedence test');
      Halt(1);
    end;

    AssertTrue('server private key PEM overrides missing file path', LMaterial.HasPrivateKeyMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildClientPrefersImportedCertificatePEMOverFile;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LBuilder: ISSLContextBuilder;
  LCert, LKey: string;
  LCertJSON: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'client-cert-merge-prefer-pem.local', 'Client Cert Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate client certificate material for certificate precedence test');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LBuilder := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificate('/definitely/missing/client-certificate.pem');
    LCertJSON := StringReplace(LCert, '\', '\\', [rfReplaceAll]);
    LCertJSON := StringReplace(LCertJSON, LineEnding, '\n', [rfReplaceAll]);
    LBuilder.ImportFromJSON('{"certificate_pem":"' + LCertJSON + '"}');

    LContext := LBuilder.BuildClient;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildClient returned nil when imported PEM certificate should override file');
      Halt(1);
    end;

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] client context should expose FreePascal material view for certificate precedence test');
      Halt(1);
    end;

    AssertTrue('client imported certificate PEM overrides missing file path', LMaterial.HasCertificateMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerPrefersImportedCertificatePEMOverFile;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LBuilder: ISSLContextBuilder;
  LCert, LKey: string;
  LCertJSON: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-cert-merge-prefer-pem.local', 'Server Cert Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material for certificate precedence test');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LBuilder := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificate('/definitely/missing/server-certificate.pem')
      .WithPrivateKeyPEM(LKey);
    LCertJSON := StringReplace(LCert, '\', '\\', [rfReplaceAll]);
    LCertJSON := StringReplace(LCertJSON, LineEnding, '\n', [rfReplaceAll]);
    LBuilder.ImportFromJSON('{"certificate_pem":"' + LCertJSON + '"}');

    LContext := LBuilder.BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil when imported PEM certificate should override file');
      Halt(1);
    end;

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] server context should expose FreePascal material view for certificate precedence test');
      Halt(1);
    end;

    AssertTrue('server imported certificate PEM overrides missing file path', LMaterial.HasCertificateMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildClientPrefersMergedPrivateKeyPEMOverFile;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LBase: ISSLContextBuilder;
  LCert, LKey: string;
  LKeyJSON: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'client-merge-prefer-pem.local', 'Client Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate client certificate material for merge private key precedence test');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LBase := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificatePEM(LCert)
      .WithPrivateKey('/definitely/missing/client-private-key.pem');
    LKeyJSON := StringReplace(LKey, '\', '\\', [rfReplaceAll]);
    LKeyJSON := StringReplace(LKeyJSON, LineEnding, '\n', [rfReplaceAll]);
    LBase.ImportFromJSON('{"private_key_pem":"' + LKeyJSON + '"}');

    LContext := LBase.BuildClient;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildClient returned nil when merged PEM private key should override file');
      Halt(1);
    end;

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] client context should expose FreePascal material view for merge private key precedence test');
      Halt(1);
    end;

    AssertTrue('client merged private key PEM overrides missing file path', LMaterial.HasPrivateKeyMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;

procedure TestBuildServerPrefersMergedPrivateKeyPEMOverFile;
var
  LContext: ISSLContext;
  LMaterial: IFreePascalContextMaterial;
  LBase: ISSLContextBuilder;
  LCert, LKey: string;
  LKeyJSON: string;
begin
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'server-merge-prefer-pem.local', 'Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('[FAIL] failed to generate server certificate material for merge private key precedence test');
    Halt(1);
  end;

  ResetCounters;
  RegisterFakeLibraries;
  try
    LBase := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificatePEM(LCert)
      .WithPrivateKey('/definitely/missing/server-private-key.pem');
    LKeyJSON := StringReplace(LKey, '\', '\\', [rfReplaceAll]);
    LKeyJSON := StringReplace(LKeyJSON, LineEnding, '\n', [rfReplaceAll]);
    LBase.ImportFromJSON('{"private_key_pem":"' + LKeyJSON + '"}');

    LContext := LBase.BuildServer;

    if LContext = nil then
    begin
      WriteLn('[FAIL] BuildServer returned nil when merged PEM private key should override file');
      Halt(1);
    end;

    if not Supports(LContext, IFreePascalContextMaterial, LMaterial) then
    begin
      WriteLn('[FAIL] server context should expose FreePascal material view for merge private key precedence test');
      Halt(1);
    end;

    AssertTrue('server merged private key PEM overrides missing file path', LMaterial.HasPrivateKeyMaterial);
  finally
    CleanupFakeLibraries;
  end;
end;

begin
  TestBuildClientUsesExplicitBackendStore;
  TestBuildServerUsesExplicitBackendStoreAndLoadsPEM;
  TestBuildClientUsesResolvedExplicitAutoDetectBackendStore;
  TestBuildServerUsesResolvedExplicitAutoDetectBackendStore;
  TestBuildClientUsesResolvedImplicitDefaultBackendStore;
  TestBuildServerUsesResolvedImplicitDefaultBackendStore;
  TestBuildServerUsesDefaultBackendStoreWhenImplicit;
  TestBuildServerPrefersPrivateKeyPEMOverFile;
  TestBuildClientPrefersImportedCertificatePEMOverFile;
  TestBuildServerPrefersImportedCertificatePEMOverFile;
  TestBuildClientPrefersMergedPrivateKeyPEMOverFile;
  TestBuildServerPrefersMergedPrivateKeyPEMOverFile;
end.
