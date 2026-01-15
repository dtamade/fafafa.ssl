program test_pem;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.bio,
  fafafa.ssl.openssl.pem,
  fafafa.ssl.openssl.rsa,
  fafafa.ssl.openssl.evp;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  if Passed then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

procedure TestPEMLoading;
var
  Handle: THandle;
  Success: Boolean;
begin
  WriteLn('Testing PEM module loading...');
  
  Handle := GetCryptoLibHandle;
  Success := (Handle <> 0) and LoadOpenSSLPEM(Handle);
  
  TestResult('PEM module loading', Success);
end;

procedure TestPEMRSAKeyReadWrite;
var
  Bio: PBIO;
  Key: PRSA;
  OutBio: PBIO;
  WriteResult: Integer;
  BufLen: Integer;
  Buf: array[0..4095] of AnsiChar;
  PemData: AnsiString;
begin
  WriteLn('Testing PEM RSA key read/write...');
  
  // Generate RSA key for testing
  LoadRSAFunctions;
  Key := RSA_new();
  if Key = nil then
  begin
    TestResult('RSA key generation for PEM test', False);
    Exit;
  end;
  
  // Create a memory BIO for writing
  OutBio := BIO_new(BIO_s_mem());
  if OutBio = nil then
  begin
    RSA_free(Key);
    TestResult('BIO creation for PEM write', False);
    Exit;
  end;
  
  // Write RSA public key to PEM format
  WriteResult := PEM_write_bio_RSAPublicKey(OutBio, Key);
  TestResult('PEM write RSA public key', WriteResult = 1);
  
  // Read the PEM data back
  if WriteResult = 1 then
  begin
    BufLen := BIO_read(OutBio, @Buf[0], SizeOf(Buf) - 1);
    if BufLen > 0 then
    begin
      Buf[BufLen] := #0;
      PemData := AnsiString(PAnsiChar(@Buf[0]));
      TestResult('PEM data contains BEGIN marker', Pos('BEGIN', PemData) > 0);
      TestResult('PEM data contains END marker', Pos('END', PemData) > 0);
    end
    else
      TestResult('PEM data read back', False);
  end;
  
  BIO_free(OutBio);
  RSA_free(Key);
end;

procedure TestPEMPrivateKeyReadWrite;
var
  Key: PEVP_PKEY;
  Bio: PBIO;
  WriteResult: Integer;
  ReadKey: PEVP_PKEY;
begin
  WriteLn('Testing PEM private key read/write...');
  
  // Generate RSA key using EVP
  LoadEVP;
  Key := EVP_PKEY_new();
  if Key = nil then
  begin
    TestResult('EVP key creation', False);
    Exit;
  end;
  
  // Create memory BIO
  Bio := BIO_new(BIO_s_mem());
  if Bio = nil then
  begin
    EVP_PKEY_free(Key);
    TestResult('BIO creation for EVP key', False);
    Exit;
  end;
  
  // Write unencrypted private key to PEM
  WriteResult := PEM_write_bio_PrivateKey(Bio, Key, nil, nil, 0, nil, nil);
  TestResult('PEM write private key (unencrypted)', WriteResult = 1);
  
  // Try to read it back
  if WriteResult = 1 then
  begin
    ReadKey := PEM_read_bio_PrivateKey(Bio, nil, nil, nil);
    TestResult('PEM read private key back', ReadKey <> nil);
    if ReadKey <> nil then
      EVP_PKEY_free(ReadKey);
  end;
  
  BIO_free(Bio);
  EVP_PKEY_free(Key);
end;

procedure TestPEMPublicKeyReadWrite;
var
  Key: PEVP_PKEY;
  Bio: PBIO;
  WriteResult: Integer;
  ReadKey: PEVP_PKEY;
begin
  WriteLn('Testing PEM public key read/write...');
  
  Key := EVP_PKEY_new();
  if Key = nil then
  begin
    TestResult('EVP key creation for public key test', False);
    Exit;
  end;
  
  Bio := BIO_new(BIO_s_mem());
  if Bio = nil then
  begin
    EVP_PKEY_free(Key);
    TestResult('BIO creation for public key', False);
    Exit;
  end;
  
  // Write public key to PEM
  WriteResult := PEM_write_bio_PUBKEY(Bio, Key);
  TestResult('PEM write public key', WriteResult = 1);
  
  // Read it back
  if WriteResult = 1 then
  begin
    ReadKey := PEM_read_bio_PUBKEY(Bio, nil, nil, nil);
    TestResult('PEM read public key back', ReadKey <> nil);
    if ReadKey <> nil then
      EVP_PKEY_free(ReadKey);
  end;
  
  BIO_free(Bio);
  EVP_PKEY_free(Key);
end;

begin
  WriteLn('========================================');
  WriteLn('OpenSSL PEM Module Test');
  WriteLn('========================================');
  WriteLn;
  
  try
    // Load OpenSSL core
    WriteLn('Loading OpenSSL core...');
    LoadOpenSSLCore;
    TestResult('OpenSSL core loaded', GetCryptoLibHandle <> 0);
    WriteLn;
    
    // Load BIO module
    LoadBIOFunctions;
    
    // Run tests
    TestPEMLoading;
    WriteLn;
    
    TestPEMRSAKeyReadWrite;
    WriteLn;
    
    TestPEMPrivateKeyReadWrite;
    WriteLn;
    
    TestPEMPublicKeyReadWrite;
    WriteLn;
    
    // Summary
    WriteLn('========================================');
    WriteLn('Test Summary');
    WriteLn('========================================');
    WriteLn('Total tests: ', TestsPassed + TestsFailed);
    WriteLn('Passed: ', TestsPassed);
    WriteLn('Failed: ', TestsFailed);
    WriteLn('Pass rate: ', (TestsPassed * 100) div (TestsPassed + TestsFailed), '%');
    WriteLn('========================================');
    
    if TestsFailed = 0 then
      ExitCode := 0
    else
      ExitCode := 1;
      
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
