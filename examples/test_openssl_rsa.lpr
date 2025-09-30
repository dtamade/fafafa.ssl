program test_openssl_rsa;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.rsa,
  fafafa.ssl.openssl.bn,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.api;

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

function TestRSAFunctionLoad: Boolean;
begin
  Result := Assigned(RSA_new) and
            Assigned(RSA_free) and
            Assigned(RSA_generate_key_ex) and
            Assigned(RSA_public_encrypt) and
            Assigned(RSA_private_decrypt) and
            Assigned(RSA_sign) and
            Assigned(RSA_verify);
end;

function TestRSAKeyGeneration: Boolean;
var
  rsa: PRSA;
  e: PBIGNUM;
begin
  Result := False;
  
  rsa := RSA_new();
  if not Assigned(rsa) then Exit;
  
  try
    e := BN_new();
    if not Assigned(e) then Exit;
    
    try
      // Set public exponent to 65537 (RSA_F4)
      if BN_set_word(e, RSA_F4) <> 1 then Exit;
      
      // Generate 2048-bit RSA key
      if RSA_generate_key_ex(rsa, 2048, e, nil) = 1 then
      begin
        // Verify key was generated
        if RSA_bits(rsa) = 2048 then
          Result := True;
      end;
    finally
      BN_free(e);
    end;
  finally
    RSA_free(rsa);
  end;
end;

function TestRSAEncryptDecrypt: Boolean;
var
  rsa: PRSA;
  e: PBIGNUM;
  plaintext: string;
  encrypted, decrypted: array[0..255] of Byte;
  enc_len, dec_len: Integer;
  i: Integer;
begin
  Result := False;
  plaintext := 'Hello, RSA!';
  
  rsa := RSA_new();
  if not Assigned(rsa) then Exit;
  
  try
    e := BN_new();
    if not Assigned(e) then Exit;
    
    try
      if BN_set_word(e, RSA_F4) <> 1 then Exit;
      if RSA_generate_key_ex(rsa, 2048, e, nil) <> 1 then Exit;
      
      // Encrypt with public key
      enc_len := RSA_public_encrypt(
        Length(plaintext),
        @plaintext[1],
        @encrypted[0],
        rsa,
        RSA_PKCS1_PADDING
      );
      
      if enc_len <= 0 then Exit;
      
      // Decrypt with private key
      dec_len := RSA_private_decrypt(
        enc_len,
        @encrypted[0],
        @decrypted[0],
        rsa,
        RSA_PKCS1_PADDING
      );
      
      if dec_len <= 0 then Exit;
      
      // Verify decrypted data matches original
      Result := True;
      for i := 0 to Length(plaintext) - 1 do
      begin
        if decrypted[i] <> Ord(plaintext[i + 1]) then
        begin
          Result := False;
          Break;
        end;
      end;
    finally
      BN_free(e);
    end;
  finally
    RSA_free(rsa);
  end;
end;

function TestRSASignVerify: Boolean;
var
  rsa: PRSA;
  e: PBIGNUM;
  message: string;
  signature: array[0..255] of Byte;
  sig_len: Cardinal;
  hash_type: Integer;
begin
  Result := False;
  message := 'Test message for RSA signature';
  hash_type := 4; // NID_md5 for testing
  
  rsa := RSA_new();
  if not Assigned(rsa) then Exit;
  
  try
    e := BN_new();
    if not Assigned(e) then Exit;
    
    try
      if BN_set_word(e, RSA_F4) <> 1 then Exit;
      if RSA_generate_key_ex(rsa, 2048, e, nil) <> 1 then Exit;
      
      // Sign message
      if RSA_sign(
        hash_type,
        @message[1],
        Length(message),
        @signature[0],
        @sig_len,
        rsa
      ) <> 1 then Exit;
      
      // Verify signature
      if RSA_verify(
        hash_type,
        @message[1],
        Length(message),
        @signature[0],
        sig_len,
        rsa
      ) = 1 then
        Result := True;
    finally
      BN_free(e);
    end;
  finally
    RSA_free(rsa);
  end;
end;

function TestRSAKeySizes: Boolean;
var
  rsa_1024, rsa_2048, rsa_4096: PRSA;
  e: PBIGNUM;
  success: Boolean;
begin
  Result := False;
  success := True;
  
  e := BN_new();
  if not Assigned(e) then Exit;
  
  try
    if BN_set_word(e, RSA_F4) <> 1 then Exit;
    
    // Test 1024-bit key
    rsa_1024 := RSA_new();
    if Assigned(rsa_1024) then
    begin
      try
        if RSA_generate_key_ex(rsa_1024, 1024, e, nil) = 1 then
        begin
          if RSA_bits(rsa_1024) <> 1024 then
            success := False;
        end
        else
          success := False;
      finally
        RSA_free(rsa_1024);
      end;
    end
    else
      success := False;
    
    // Test 2048-bit key
    rsa_2048 := RSA_new();
    if Assigned(rsa_2048) then
    begin
      try
        if RSA_generate_key_ex(rsa_2048, 2048, e, nil) = 1 then
        begin
          if RSA_bits(rsa_2048) <> 2048 then
            success := False;
        end
        else
          success := False;
      finally
        RSA_free(rsa_2048);
      end;
    end
    else
      success := False;
    
    // Test 4096-bit key (optional, may be slow)
    rsa_4096 := RSA_new();
    if Assigned(rsa_4096) then
    begin
      try
        if RSA_generate_key_ex(rsa_4096, 4096, e, nil) = 1 then
        begin
          if RSA_bits(rsa_4096) <> 4096 then
            success := False;
        end
        else
          success := False;
      finally
        RSA_free(rsa_4096);
      end;
    end
    else
      success := False;
    
    Result := success;
  finally
    BN_free(e);
  end;
end;

function TestRSAInvalidInputs: Boolean;
var
  rsa: PRSA;
  e: PBIGNUM;
  encrypted: array[0..255] of Byte;
  enc_len: Integer;
begin
  Result := False;
  
  rsa := RSA_new();
  if not Assigned(rsa) then Exit;
  
  try
    e := BN_new();
    if not Assigned(e) then Exit;
    
    try
      if BN_set_word(e, RSA_F4) <> 1 then Exit;
      if RSA_generate_key_ex(rsa, 2048, e, nil) <> 1 then Exit;
      
      // Try to encrypt data that's too large for the key
      // RSA can encrypt up to (key_size - 11) bytes with PKCS1 padding
      // For 2048-bit key, that's 245 bytes
      SetLength(encrypted, 300);
      enc_len := RSA_public_encrypt(
        300,
        @encrypted[0],
        @encrypted[0],
        rsa,
        RSA_PKCS1_PADDING
      );
      
      // Should fail (return -1)
      Result := (enc_len = -1);
    finally
      BN_free(e);
    end;
  finally
    RSA_free(rsa);
  end;
end;

function TestRSAKeySize: Boolean;
var
  rsa: PRSA;
  e: PBIGNUM;
  key_size: Integer;
begin
  Result := False;
  
  rsa := RSA_new();
  if not Assigned(rsa) then Exit;
  
  try
    e := BN_new();
    if not Assigned(e) then Exit;
    
    try
      if BN_set_word(e, RSA_F4) <> 1 then Exit;
      if RSA_generate_key_ex(rsa, 2048, e, nil) <> 1 then Exit;
      
      // RSA_size returns key size in bytes
      key_size := RSA_size(rsa);
      
      // 2048 bits = 256 bytes
      Result := (key_size = 256);
    finally
      BN_free(e);
    end;
  finally
    RSA_free(rsa);
  end;
end;

begin
  WriteLn('OpenSSL RSA Module Test');
  WriteLn('=======================');
  WriteLn;
  
  LoadOpenSSLCore;
  WriteLn('OpenSSL version: ', GetOpenSSLVersion);
  WriteLn;
  
  if not LoadOpenSSLBN then
  begin
    WriteLn('Failed to load BIGNUM functions');
    Halt(1);
  end;
  
  if not LoadOpenSSLRSA then
  begin
    WriteLn('Failed to load RSA functions');
    Halt(1);
  end;
  
  TestResult('RSA - Load functions', TestRSAFunctionLoad);
  TestResult('RSA - Key generation (2048-bit)', TestRSAKeyGeneration);
  TestResult('RSA - Encrypt/Decrypt', TestRSAEncryptDecrypt);
  TestResult('RSA - Sign/Verify', TestRSASignVerify);
  TestResult('RSA - Different key sizes (1024, 2048, 4096)', TestRSAKeySizes);
  TestResult('RSA - Key size calculation', TestRSAKeySize);
  TestResult('RSA - Invalid inputs handling', TestRSAInvalidInputs);
  
  WriteLn;
  WriteLn('====================================');
  WriteLn('Test Summary');
  WriteLn('====================================');
  WriteLn('Tests passed: ', TestsPassed);
  WriteLn('Tests failed: ', TestsFailed);
  WriteLn('Total tests:  ', TestsPassed + TestsFailed);
  
  UnloadOpenSSLRSA;
  UnloadOpenSSLBN;
  UnloadOpenSSLCore;
  
  if TestsFailed > 0 then
    Halt(1);
end.
