program test_rsa_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.types,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure LogTest(const TestName: string; Passed: Boolean; const Details: string = '');
begin
  Inc(TotalTests);
  if Passed then
  begin
    Inc(PassedTests);
    Write('[PASS] ');
  end
  else
  begin
    Inc(FailedTests);
    Write('[FAIL] ');
  end;
  WriteLn(TestName);
  if Details <> '' then
    WriteLn('  ', Details);
end;

procedure TestRSAKeyGeneration;
var
  Rsa: PRSA;
  Exponent: PBIGNUM;
  KeySize: Integer;
begin
  WriteLn;
  WriteLn('=== RSA Key Generation Tests ===');
  WriteLn;
  
  // Test 2048-bit key generation using RSA_generate_key_ex
  Rsa := RSA_new();
  LogTest('Create RSA structure', Rsa <> nil);
  
  if Rsa <> nil then
  begin
    Exponent := BN_new();
    if Exponent <> nil then
    begin
      if BN_set_word(Exponent, 65537) = 1 then
      begin
        if RSA_generate_key_ex(Rsa, 2048, Exponent, nil) = 1 then
        begin
          KeySize := RSA_size(Rsa);
          LogTest('Generate 2048-bit RSA key', True, 
                  Format('Generated key with size %d bytes (%d bits)', 
                         [KeySize, KeySize * 8]));
        end
        else
          LogTest('Generate 2048-bit RSA key', False, 'RSA_generate_key_ex failed');
      end
      else
        LogTest('Set exponent', False);
        
      BN_free(Exponent);
    end
    else
      LogTest('Create exponent', False);
      
    RSA_free(Rsa);
  end;
end;

procedure TestRSASignVerify;
var
  Rsa: PRSA;
  Exponent: PBIGNUM;
  Digest: array[0..31] of Byte;  // SHA-256 hash
  Signature: array[0..511] of Byte;
  SigLen: Cardinal;
  I: Integer;
  VerifyResult: Integer;
begin
  WriteLn;
  WriteLn('=== RSA Sign/Verify Tests ===');
  WriteLn;
  
  // Generate test key
  Rsa := RSA_new();
  if Rsa = nil then
  begin
    LogTest('Create RSA structure for signing', False);
    Exit;
  end;
  
  Exponent := BN_new();
  if Exponent = nil then
  begin
    RSA_free(Rsa);
    LogTest('Create exponent for signing', False);
    Exit;
  end;
  
  try
    // Generate key
    if BN_set_word(Exponent, 65537) <> 1 then
    begin
      LogTest('Set exponent for signing', False);
      Exit;
    end;
    
    if RSA_generate_key_ex(Rsa, 2048, Exponent, nil) <> 1 then
    begin
      LogTest('Generate key for signing', False);
      Exit;
    end;
    
    // Create fake digest (normally this would be output from SHA-256)
    for I := 0 to 31 do
      Digest[I] := Byte(I);
    
    // Sign the digest
    SigLen := SizeOf(Signature);
    if RSA_sign(NID_sha256, @Digest[0], 32, @Signature[0], @SigLen, Rsa) = 1 then
    begin
      LogTest('Sign digest', True, Format('Generated %d byte signature', [SigLen]));
      
      // Verify the signature
      VerifyResult := RSA_verify(NID_sha256, @Digest[0], 32, @Signature[0], SigLen, Rsa);
      LogTest('Verify signature', VerifyResult = 1,
              Format('Verification result: %d', [VerifyResult]));
    end
    else
      LogTest('Sign digest', False, 'RSA_sign failed');
    
  finally
    BN_free(Exponent);
    RSA_free(Rsa);
  end;
end;

procedure TestRSAEncryptDecrypt;
var
  Rsa: PRSA;
  Exponent: PBIGNUM;
  Plaintext: AnsiString;
  Ciphertext: array[0..511] of Byte;
  Decrypted: array[0..511] of Byte;
  CiphertextLen, DecryptedLen: Integer;
  DecryptedStr: AnsiString;
begin
  WriteLn;
  WriteLn('=== RSA Encrypt/Decrypt Tests ===');
  WriteLn;
  
  // Generate test key
  Rsa := RSA_new();
  if Rsa = nil then
  begin
    LogTest('Create RSA structure for encryption', False);
    Exit;
  end;
  
  Exponent := BN_new();
  if Exponent = nil then
  begin
    RSA_free(Rsa);
    LogTest('Create exponent for encryption', False);
    Exit;
  end;
  
  try
    // Generate key
    if BN_set_word(Exponent, 65537) <> 1 then
    begin
      LogTest('Set exponent for encryption', False);
      Exit;
    end;
    
    if RSA_generate_key_ex(Rsa, 2048, Exponent, nil) <> 1 then
    begin
      LogTest('Generate key for encryption', False);
      Exit;
    end;
    
    Plaintext := 'Hello RSA!';
    
    // Encrypt (using PKCS#1 v1.5 padding)
    CiphertextLen := RSA_public_encrypt(Length(Plaintext), @Plaintext[1],
                                        @Ciphertext[0], Rsa, RSA_PKCS1_PADDING);
    
    if CiphertextLen >= 0 then
    begin
      LogTest('Encrypt data', True, Format('Encrypted to %d bytes', [CiphertextLen]));
      
      // Decrypt
      DecryptedLen := RSA_private_decrypt(CiphertextLen, @Ciphertext[0],
                                          @Decrypted[0], Rsa, RSA_PKCS1_PADDING);
      
      if DecryptedLen >= 0 then
      begin
        LogTest('Decrypt data', True, Format('Decrypted %d bytes', [DecryptedLen]));
        
        SetString(DecryptedStr, PAnsiChar(@Decrypted[0]), DecryptedLen);
        LogTest('Decrypted data matches', DecryptedStr = Plaintext,
                Format('Original: "%s", Decrypted: "%s"', [Plaintext, DecryptedStr]));
      end
      else
        LogTest('Decrypt data', False, Format('RSA_private_decrypt returned %d', [DecryptedLen]));
    end
    else
      LogTest('Encrypt data', False, Format('RSA_public_encrypt returned %d', [CiphertextLen]));
    
  finally
    BN_free(Exponent);
    RSA_free(Rsa);
  end;
end;

begin
  WriteLn('RSA Simple Integration Tests');
  WriteLn('===========================');
  WriteLn;
  
  // Load core OpenSSL libraries first
  LoadOpenSSLCore;
  
  // Load RSA module
  if not LoadOpenSSLRSA then
  begin
    WriteLn('ERROR: Failed to load RSA module');
    Halt(1);
  end;
  
  // Load BN module
  if not LoadOpenSSLBN then
  begin
    WriteLn('ERROR: Failed to load BN module');
    Halt(1);
  end;
  
  try
    // Run test suites
    TestRSAKeyGeneration;
    TestRSASignVerify;
    TestRSAEncryptDecrypt;
    
    // Print summary
    WriteLn;
    WriteLn('=== Test Summary ===');
    WriteLn(Format('Total:  %d', [TotalTests]));
    WriteLn(Format('Passed: %d', [PassedTests]));
    WriteLn(Format('Failed: %d', [FailedTests]));
    WriteLn;
    
    if FailedTests > 0 then
    begin
      WriteLn('RESULT: FAILED');
      Halt(1);
    end
    else
    begin
      WriteLn('RESULT: ALL TESTS PASSED');
      Halt(0);
    end;
    
  finally
    // OpenSSL cleanup if needed
  end;
end.
