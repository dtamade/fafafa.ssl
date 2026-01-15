program test_kdf;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.kdf,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.rand;

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

function BytesToHex(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Bytes) do
    Result := Result + IntToHex(Bytes[I], 2);
end;

procedure TestKDFLoading;
begin
  WriteLn('Testing KDF module loading...');
  
  LoadKDFFunctions;
  
  TestResult('PKCS5_PBKDF2_HMAC loaded', Assigned(PKCS5_PBKDF2_HMAC));
  TestResult('PKCS5_PBKDF2_HMAC_SHA1 loaded', Assigned(PKCS5_PBKDF2_HMAC_SHA1));
  TestResult('EVP_PBE_scrypt loaded', Assigned(EVP_PBE_scrypt));
end;

procedure TestPBKDF2;
var
  Password: string;
  Salt: TBytes;
  DerivedKey: TBytes;
  ExpectedHex: string;
  ActualHex: string;
begin
  WriteLn('Testing PBKDF2...');
  
  // RFC 6070 Test Vector #1
  Password := 'password';
  SetLength(Salt, 4);
  Salt[0] := $73; Salt[1] := $61; Salt[2] := $6C; Salt[3] := $74; // 'salt'
  
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 1, 20, EVP_sha1());
  ExpectedHex := 'C88ABF3182595AAD4E42E46AB52B1E5FC5A6F2C5'; // From RFC 6070
  ActualHex := BytesToHex(DerivedKey);
  
  TestResult('PBKDF2-HMAC-SHA1 (1 iteration)', 
             (Length(DerivedKey) = 20) and (UpperCase(ActualHex) = ExpectedHex));
  
  // RFC 6070 Test Vector #2
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 2, 20, EVP_sha1());
  ExpectedHex := 'EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957';
  ActualHex := BytesToHex(DerivedKey);
  
  TestResult('PBKDF2-HMAC-SHA1 (2 iterations)',
             (Length(DerivedKey) = 20) and (UpperCase(ActualHex) = ExpectedHex));
  
  // Test with SHA-256
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 1, 32, EVP_sha256());
  TestResult('PBKDF2-HMAC-SHA256',
             Length(DerivedKey) = 32);
  
  // Test with more iterations
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 4096, 32, EVP_sha256());
  TestResult('PBKDF2-HMAC-SHA256 (4096 iterations)',
             Length(DerivedKey) = 32);
end;

procedure TestPBKDF2Direct;
var
  Password: AnsiString;
  Salt: array[0..3] of Byte;
  OutKey: array[0..19] of Byte;
  Result: Integer;
  ExpectedHex: string;
  ActualHex: string;
  I: Integer;
begin
  WriteLn('Testing PBKDF2 direct API...');
  
  if not Assigned(PKCS5_PBKDF2_HMAC_SHA1) then
  begin
    WriteLn('  PKCS5_PBKDF2_HMAC_SHA1 not available, skipping');
    Exit;
  end;
  
  Password := 'password';
  Salt[0] := $73; Salt[1] := $61; Salt[2] := $6C; Salt[3] := $74;
  
  Result := PKCS5_PBKDF2_HMAC_SHA1(PAnsiChar(Password), Length(Password),
                                   @Salt[0], Length(Salt),
                                   1, 20, @OutKey[0]);
  
  TestResult('PKCS5_PBKDF2_HMAC_SHA1 returned success', Result = 1);
  
  if Result = 1 then
  begin
    ActualHex := '';
    for I := 0 to 19 do
      ActualHex := ActualHex + IntToHex(OutKey[I], 2);
    
    ExpectedHex := 'C88ABF3182595AAD4E42E46AB52B1E5FC5A6F2C5';
    TestResult('PBKDF2 direct API output matches RFC 6070',
               UpperCase(ActualHex) = ExpectedHex);
  end;
end;

procedure TestScrypt;
var
  Password: string;
  Salt: TBytes;
  DerivedKey: TBytes;
begin
  WriteLn('Testing scrypt...');
  
  if not Assigned(EVP_PBE_scrypt) then
  begin
    WriteLn('  EVP_PBE_scrypt not available, skipping scrypt tests');
    Exit;
  end;
  
  Password := 'password';
  SetLength(Salt, 8);
  Salt[0] := $73; Salt[1] := $61; Salt[2] := $6C; Salt[3] := $74;
  Salt[4] := $73; Salt[5] := $61; Salt[6] := $6C; Salt[7] := $74;
  
  // Test with small N for speed
  DerivedKey := DeriveKeyScrypt(Password, Salt, 16, 1, 1, 32, 1024 * 1024);
  TestResult('scrypt (N=16, r=1, p=1)',
             Length(DerivedKey) = 32);
  
  // Test with slightly larger N
  DerivedKey := DeriveKeyScrypt(Password, Salt, 256, 8, 1, 32, 32 * 1024 * 1024);
  TestResult('scrypt (N=256, r=8, p=1)',
             Length(DerivedKey) = 32);
  
  // Test different output lengths
  DerivedKey := DeriveKeyScrypt(Password, Salt, 16, 1, 1, 64, 1024 * 1024);
  TestResult('scrypt 64-byte output',
             Length(DerivedKey) = 64);
end;

procedure TestSaltGeneration;
var
  Salt1, Salt2: TBytes;
begin
  WriteLn('Testing salt generation...');
  
  Salt1 := GenerateSalt(8);
  TestResult('Generate 8-byte salt', Length(Salt1) = 8);
  
  Salt2 := GenerateSalt(8);
  TestResult('Generate another 8-byte salt', Length(Salt2) = 8);
  
  TestResult('Two generated salts are different', BytesToHex(Salt1) <> BytesToHex(Salt2));
  
  Salt1 := GenerateSalt(16);
  TestResult('Generate 16-byte salt', Length(Salt1) = 16);
  
  Salt1 := GenerateSalt(32);
  TestResult('Generate 32-byte salt', Length(Salt1) = 32);
end;

procedure TestKDFIntegration;
var
  Password: string;
  Salt: TBytes;
  Key1, Key2: TBytes;
begin
  WriteLn('Testing KDF integration...');
  
  Password := 'MySecretPassword123!';
  Salt := GenerateSalt(16);
  
  // Derive the same key twice to verify consistency
  Key1 := DeriveKeyPBKDF2(Password, Salt, 10000, 32);
  Key2 := DeriveKeyPBKDF2(Password, Salt, 10000, 32);
  
  TestResult('PBKDF2 consistency check',
             (Length(Key1) = 32) and (Length(Key2) = 32) and
             (BytesToHex(Key1) = BytesToHex(Key2)));
  
  if Assigned(EVP_PBE_scrypt) then
  begin
    // Test scrypt consistency
    Key1 := DeriveKeyScrypt(Password, Salt, 256, 8, 1, 32, 32 * 1024 * 1024);
    Key2 := DeriveKeyScrypt(Password, Salt, 256, 8, 1, 32, 32 * 1024 * 1024);
    
    TestResult('scrypt consistency check',
               (Length(Key1) = 32) and (Length(Key2) = 32) and
               (BytesToHex(Key1) = BytesToHex(Key2)));
  end;
end;

procedure TestKDFErrorHandling;
var
  Password: string;
  Salt: TBytes;
  DerivedKey: TBytes;
begin
  WriteLn('Testing KDF error handling...');
  
  Password := 'test';
  SetLength(Salt, 8);
  Salt[0] := $01; Salt[1] := $02; Salt[2] := $03; Salt[3] := $04;
  Salt[4] := $05; Salt[5] := $06; Salt[6] := $07; Salt[7] := $08;
  
  // Test with zero key length
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 1000, 0);
  TestResult('PBKDF2 with zero key length returns empty',
             Length(DerivedKey) = 0);
  
  // Test with negative key length
  DerivedKey := DeriveKeyPBKDF2(Password, Salt, 1000, -1);
  TestResult('PBKDF2 with negative key length returns empty',
             Length(DerivedKey) = 0);
  
  // Test with empty password
  DerivedKey := DeriveKeyPBKDF2('', Salt, 1000, 32);
  TestResult('PBKDF2 with empty password succeeds',
             Length(DerivedKey) = 32);
end;

begin
  WriteLn('========================================');
  WriteLn('OpenSSL KDF Module Test');
  WriteLn('========================================');
  WriteLn;
  
  try
    // Load OpenSSL core
    WriteLn('Loading OpenSSL core...');
    LoadOpenSSLCore;
    TestResult('OpenSSL core loaded', GetCryptoLibHandle <> 0);
    WriteLn;
    
    // Load EVP for hash functions
    LoadEVP(GetCryptoLibHandle);
    
    // Run tests
    TestKDFLoading;
    WriteLn;
    
    TestPBKDF2;
    WriteLn;
    
    TestPBKDF2Direct;
    WriteLn;
    
    TestScrypt;
    WriteLn;
    
    TestSaltGeneration;
    WriteLn;
    
    TestKDFIntegration;
    WriteLn;
    
    TestKDFErrorHandling;
    WriteLn;
    
    // Summary
    WriteLn('========================================');
    WriteLn('Test Summary');
    WriteLn('========================================');
    WriteLn('Total tests: ', TestsPassed + TestsFailed);
    WriteLn('Passed: ', TestsPassed);
    WriteLn('Failed: ', TestsFailed);
    if (TestsPassed + TestsFailed) > 0 then
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
