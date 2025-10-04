program test_rsa_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.rsa,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.bn,
  fafafa.ssl.openssl.pem,
  fafafa.ssl.openssl.err;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure LogTest(const TestName: string; Passed: Boolean; const Details: string = '');
begin
  if Passed then
  begin
    Inc(TestsPassed);
    WriteLn('[PASS] ', TestName);
    if Details <> '' then
      WriteLn('       ', Details);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('[FAIL] ', TestName);
    if Details <> '' then
      WriteLn('       ', Details);
  end;
end;

function Test_RSA_KeyGen_2048: Boolean;
var
  Key: PEVP_PKEY;
  Ctx: PEVP_PKEY_CTX;
begin
  Result := False;
  WriteLn('Testing RSA 2048-bit key generation...');
  
  if not Assigned(EVP_PKEY_CTX_new_id) then
  begin
    WriteLn('  ERROR: EVP_PKEY_CTX_new_id not loaded');
    Exit;
  end;
  
  Ctx := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nil);
  if Ctx = nil then
  begin
    WriteLn('  ERROR: Failed to create PKEY context');
    Exit;
  end;
  
  try
    if EVP_PKEY_keygen_init(Ctx) <= 0 then
    begin
      WriteLn('  ERROR: Failed to initialize keygen');
      Exit;
    end;
    
    if EVP_PKEY_CTX_set_rsa_keygen_bits(Ctx, 2048) <= 0 then
    begin
      WriteLn('  ERROR: Failed to set key size to 2048');
      Exit;
    end;
    
    Key := nil;
    if EVP_PKEY_keygen(Ctx, @Key) <= 0 then
    begin
      WriteLn('  ERROR: Failed to generate key');
      Exit;
    end;
    
    if Key <> nil then
    begin
      WriteLn('  SUCCESS: Generated 2048-bit RSA key');
      EVP_PKEY_free(Key);
      Result := True;
    end;
    
  finally
    EVP_PKEY_CTX_free(Ctx);
  end;
end;

function Test_RSA_Sign_Verify: Boolean;
var
  Key: PEVP_PKEY;
  Ctx: PEVP_PKEY_CTX;
  SignCtx, VerifyCtx: PEVP_MD_CTX;
  Signature: array[0..511] of Byte;
  SigLen: size_t;
  Data: AnsiString;
  MD: PEVP_MD;
begin
  Result := False;
  WriteLn('Testing RSA signature and verification...');
  
  // Generate key first
  Ctx := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nil);
  if Ctx = nil then Exit;
  
  try
    if (EVP_PKEY_keygen_init(Ctx) <= 0) or
       (EVP_PKEY_CTX_set_rsa_keygen_bits(Ctx, 2048) <= 0) then
      Exit;
      
    Key := nil;
    if EVP_PKEY_keygen(Ctx, @Key) <= 0 then Exit;
    if Key = nil then Exit;
    
    try
      // Get SHA-256
      MD := EVP_sha256();
      if MD = nil then
      begin
        WriteLn('  ERROR: Failed to get SHA-256');
        Exit;
      end;
      
      // Sign data
      Data := 'Test data for RSA signature';
      SignCtx := EVP_MD_CTX_new();
      if SignCtx = nil then Exit;
      
      try
        if (EVP_DigestSignInit(SignCtx, nil, MD, nil, Key) <= 0) or
           (EVP_DigestSignUpdate(SignCtx, @Data[1], Length(Data)) <= 0) then
        begin
          WriteLn('  ERROR: Failed to sign data');
          Exit;
        end;
        
        SigLen := 0;
        if EVP_DigestSignFinal(SignCtx, nil, @SigLen) <= 0 then
        begin
          WriteLn('  ERROR: Failed to get signature length');
          Exit;
        end;
        
        if SigLen > SizeOf(Signature) then
        begin
          WriteLn('  ERROR: Signature too large');
          Exit;
        end;
        
        if EVP_DigestSignFinal(SignCtx, @Signature[0], @SigLen) <= 0 then
        begin
          WriteLn('  ERROR: Failed to finalize signature');
          Exit;
        end;
        
        WriteLn('  Signature length: ', SigLen, ' bytes');
        
        // Verify signature
        VerifyCtx := EVP_MD_CTX_new();
        if VerifyCtx = nil then Exit;
        
        try
          if (EVP_DigestVerifyInit(VerifyCtx, nil, MD, nil, Key) <= 0) or
             (EVP_DigestVerifyUpdate(VerifyCtx, @Data[1], Length(Data)) <= 0) then
          begin
            WriteLn('  ERROR: Failed to initialize verification');
            Exit;
          end;
          
          if EVP_DigestVerifyFinal(VerifyCtx, @Signature[0], SigLen) = 1 then
          begin
            WriteLn('  SUCCESS: Signature verified correctly');
            Result := True;
          end
          else
            WriteLn('  ERROR: Signature verification failed');
            
        finally
          EVP_MD_CTX_free(VerifyCtx);
        end;
        
      finally
        EVP_MD_CTX_free(SignCtx);
      end;
      
    finally
      EVP_PKEY_free(Key);
    end;
    
  finally
    EVP_PKEY_CTX_free(Ctx);
  end;
end;

function Test_RSA_Encrypt_Decrypt: Boolean;
var
  Key: PEVP_PKEY;
  Ctx: PEVP_PKEY_CTX;
  EncCtx, DecCtx: PEVP_PKEY_CTX;
  Plaintext: AnsiString;
  Ciphertext: array[0..511] of Byte;
  Decrypted: array[0..511] of Byte;
  CipherLen, DecryptLen: size_t;
begin
  Result := False;
  WriteLn('Testing RSA encryption and decryption...');
  
  // Generate key
  Ctx := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nil);
  if Ctx = nil then Exit;
  
  try
    if (EVP_PKEY_keygen_init(Ctx) <= 0) or
       (EVP_PKEY_CTX_set_rsa_keygen_bits(Ctx, 2048) <= 0) then
      Exit;
      
    Key := nil;
    if EVP_PKEY_keygen(Ctx, @Key) <= 0 then Exit;
    if Key = nil then Exit;
    
    try
      Plaintext := 'Test data for RSA encryption';
      
      // Encrypt
      EncCtx := EVP_PKEY_CTX_new(Key, nil);
      if EncCtx = nil then Exit;
      
      try
        if EVP_PKEY_encrypt_init(EncCtx) <= 0 then
        begin
          WriteLn('  ERROR: Failed to initialize encryption');
          Exit;
        end;
        
        if EVP_PKEY_CTX_set_rsa_padding(EncCtx, RSA_PKCS1_OAEP_PADDING) <= 0 then
        begin
          WriteLn('  ERROR: Failed to set padding');
          Exit;
        end;
        
        CipherLen := 0;
        if EVP_PKEY_encrypt(EncCtx, nil, @CipherLen, @Plaintext[1], Length(Plaintext)) <= 0 then
        begin
          WriteLn('  ERROR: Failed to get ciphertext length');
          Exit;
        end;
        
        if CipherLen > SizeOf(Ciphertext) then
        begin
          WriteLn('  ERROR: Ciphertext too large');
          Exit;
        end;
        
        if EVP_PKEY_encrypt(EncCtx, @Ciphertext[0], @CipherLen, @Plaintext[1], Length(Plaintext)) <= 0 then
        begin
          WriteLn('  ERROR: Failed to encrypt');
          Exit;
        end;
        
        WriteLn('  Encrypted ', Length(Plaintext), ' bytes -> ', CipherLen, ' bytes');
        
        // Decrypt
        DecCtx := EVP_PKEY_CTX_new(Key, nil);
        if DecCtx = nil then Exit;
        
        try
          if EVP_PKEY_decrypt_init(DecCtx) <= 0 then
          begin
            WriteLn('  ERROR: Failed to initialize decryption');
            Exit;
          end;
          
          if EVP_PKEY_CTX_set_rsa_padding(DecCtx, RSA_PKCS1_OAEP_PADDING) <= 0 then
          begin
            WriteLn('  ERROR: Failed to set padding for decryption');
            Exit;
          end;
          
          DecryptLen := 0;
          if EVP_PKEY_decrypt(DecCtx, nil, @DecryptLen, @Ciphertext[0], CipherLen) <= 0 then
          begin
            WriteLn('  ERROR: Failed to get decrypted length');
            Exit;
          end;
          
          if DecryptLen > SizeOf(Decrypted) then
          begin
            WriteLn('  ERROR: Decrypted data too large');
            Exit;
          end;
          
          if EVP_PKEY_decrypt(DecCtx, @Decrypted[0], @DecryptLen, @Ciphertext[0], CipherLen) <= 0 then
          begin
            WriteLn('  ERROR: Failed to decrypt');
            Exit;
          end;
          
          // Compare
          if (DecryptLen = size_t(Length(Plaintext))) and
             (CompareMem(@Decrypted[0], @Plaintext[1], DecryptLen)) then
          begin
            WriteLn('  SUCCESS: Decrypted data matches original');
            Result := True;
          end
          else
            WriteLn('  ERROR: Decrypted data does not match');
            
        finally
          EVP_PKEY_CTX_free(DecCtx);
        end;
        
      finally
        EVP_PKEY_CTX_free(EncCtx);
      end;
      
    finally
      EVP_PKEY_free(Key);
    end;
    
  finally
    EVP_PKEY_CTX_free(Ctx);
  end;
end;

procedure RunTests;
begin
  WriteLn('========================================');
  WriteLn('RSA Integration Tests');
  WriteLn('========================================');
  WriteLn;
  
  // Initialize OpenSSL
  if not InitializeOpenSSL then
  begin
    WriteLn('ERROR: Failed to initialize OpenSSL');
    Halt(1);
  end;
  
  WriteLn('OpenSSL initialized successfully');
  WriteLn;
  
  // Run tests
  LogTest('RSA 2048-bit Key Generation', Test_RSA_KeyGen_2048);
  WriteLn;
  
  LogTest('RSA Sign/Verify', Test_RSA_Sign_Verify);
  WriteLn;
  
  LogTest('RSA Encrypt/Decrypt', Test_RSA_Encrypt_Decrypt);
  WriteLn;
  
  // Summary
  WriteLn('========================================');
  WriteLn('Test Results:');
  WriteLn('  Passed: ', TestsPassed);
  WriteLn('  Failed: ', TestsFailed);
  WriteLn('  Total:  ', TestsPassed + TestsFailed);
  if TestsFailed = 0 then
    WriteLn('  Status: ALL TESTS PASSED ✓')
  else
    WriteLn('  Status: SOME TESTS FAILED ✗');
  WriteLn('========================================');
  
  // Cleanup
  FinalizeOpenSSL;
  
  if TestsFailed > 0 then
    Halt(1);
end;

begin
  try
    RunTests;
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
