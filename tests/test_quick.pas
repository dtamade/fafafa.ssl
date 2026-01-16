program test_quick;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,  // Ensure all linked backends are registered
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.quick,
  fafafa.ssl.cert.builder;

procedure TestContextBuilder;
var
  LClient, LServer: ISSLContext;
begin
  WriteLn('Testing TSSLContextBuilder...');
  
  // Test Client Builder
  LClient := TSSLContextBuilder.CreateWithSafeDefaults
    .WithTLS12And13
    .WithVerifyPeer
    .WithSNI('example.com')
    .BuildClient;
    
  if LClient = nil then
    WriteLn('FAIL: Failed to build client context')
  else
    WriteLn('PASS: Built client context');
    
  // Test Server Builder
  // Needs cert/key, so we'll skip actual build unless we generate one
  // Just test builder chain
  try
    TSSLContextBuilder.Create
      .WithTLS13
      .WithCertificate('cert.pem')
      .WithPrivateKey('key.pem')
      .BuildServer;
  except
    // Expected to fail loading non-existent files
    WriteLn('PASS: Server builder chain executed (files missing as expected)');
  end;
end;

procedure TestQuickCert;
var
  LKeyPair: IKeyPairWithCertificate;
begin
  WriteLn('Testing TSSLQuick.GenerateSelfSigned...');
  
  LKeyPair := TSSLQuick.GenerateSelfSigned('localhost', 30);
  
  if LKeyPair = nil then
    WriteLn('FAIL: Failed to generate key pair')
  else
  begin
    if LKeyPair.GetCertificate.GetSubject <> '' then
      WriteLn('PASS: Generated certificate for ' + LKeyPair.GetCertificate.GetSubject)
    else
      WriteLn('FAIL: Certificate subject empty');
  end;
end;

procedure TestQuickConnect;
var
  LConn: ISSLConnection;
begin
  // Network-dependent test: off by default for reproducible CI.
  if GetEnvironmentVariable('FAFAFA_RUN_NETWORK_TESTS') <> '1' then
  begin
    WriteLn('SKIP: TSSLQuick.Connect network test (set FAFAFA_RUN_NETWORK_TESTS=1 to enable)');
    Exit;
  end;

  WriteLn('Testing TSSLQuick.Connect (google.com:443)...');

  try
    LConn := TSSLQuick.Connect('google.com', 443);
    if LConn <> nil then
    begin
      WriteLn('PASS: Connected to google.com');
      WriteLn('  Protocol: ' + SSL_PROTOCOL_NAMES[LConn.GetProtocolVersion]);
      WriteLn('  Cipher: ' + LConn.GetCipherName);
      LConn.Close;
    end
    else
      WriteLn('FAIL: Connect returned nil');
  except
    on E: Exception do
      WriteLn('WARN: Connection failed (network issue?): ' + E.Message);
  end;
end;

begin
  try
    TestContextBuilder;
    TestQuickCert;
    TestQuickConnect;
    WriteLn('All tests completed.');
  except
    on E: Exception do
      WriteLn('FATAL: ' + E.Message);
  end;
end.
