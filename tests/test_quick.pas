program test_quick;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,  // 使用主单元以确保所有后端被注册
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.quick,
  fafafa.ssl.cert.builder;

procedure TestContextBuilder;
var
  LClient: ISSLContext;
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

begin
  try
    TestContextBuilder;
    TestQuickCert;
    // Note: TSSLQuick.Connect was removed - use TSSLContextBuilder for connections
    WriteLn('All tests completed.');
  except
    on E: Exception do
      WriteLn('FATAL: ' + E.Message);
  end;
end.
