program test_dsa_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.dsa,
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

procedure TestDSAKeyGeneration;
var
  Dsa: PDSA;
  KeyBits: Integer;
begin
  WriteLn;
  WriteLn('=== DSA Key Generation Tests ===');
  WriteLn;
  
  // Test 1024-bit DSA key generation (minimum for DSA)
  Dsa := DSA_new();
  LogTest('Create DSA structure', Dsa <> nil);
  
  if Dsa <> nil then
  begin
    // Generate 1024-bit parameters
    if DSA_generate_parameters_ex(Dsa, 1024, nil, 0, nil, nil, nil) = 1 then
    begin
      LogTest('Generate 1024-bit DSA parameters', True);
      
      // Generate key pair
      if DSA_generate_key(Dsa) = 1 then
      begin
        KeyBits := DSA_bits(Dsa);
        LogTest('Generate DSA key pair', True,
                Format('Key size: %d bits', [KeyBits]));
        LogTest('Verify key size', KeyBits = 1024);
      end
      else
        LogTest('Generate DSA key pair', False);
    end
    else
      LogTest('Generate 1024-bit DSA parameters', False);
      
    DSA_free(Dsa);
  end;
  
  // Test 2048-bit DSA key (recommended size)
  Dsa := DSA_new();
  if Dsa <> nil then
  begin
    if DSA_generate_parameters_ex(Dsa, 2048, nil, 0, nil, nil, nil) = 1 then
    begin
      if DSA_generate_key(Dsa) = 1 then
      begin
        KeyBits := DSA_bits(Dsa);
        LogTest('Generate 2048-bit DSA key', KeyBits = 2048,
                Format('Key size: %d bits', [KeyBits]));
      end
      else
        LogTest('Generate 2048-bit DSA key', False);
    end
    else
      LogTest('Generate 2048-bit DSA parameters', False);
      
    DSA_free(Dsa);
  end;
end;

procedure TestDSASignVerify;
var
  Dsa: PDSA;
  Digest: array[0..19] of Byte;  // SHA-1 hash (20 bytes for DSA)
  Signature: array[0..255] of Byte;
  SigLen: Cardinal;
  I: Integer;
  VerifyResult: Integer;
begin
  WriteLn;
  WriteLn('=== DSA Sign/Verify Tests ===');
  WriteLn;
  
  // Generate test key
  Dsa := DSA_new();
  if Dsa = nil then
  begin
    LogTest('Create DSA structure for signing', False);
    Exit;
  end;
  
  try
    // Generate parameters and key (using 1024-bit for speed)
    if DSA_generate_parameters_ex(Dsa, 1024, nil, 0, nil, nil, nil) <> 1 then
    begin
      LogTest('Generate DSA parameters for signing', False);
      Exit;
    end;
    
    if DSA_generate_key(Dsa) <> 1 then
    begin
      LogTest('Generate DSA key for signing', False);
      Exit;
    end;
    
    LogTest('Generate DSA key for signing', True);
    
    // Create fake digest (normally SHA-1 output)
    for I := 0 to 19 do
      Digest[I] := Byte(I);
    
    // Sign the digest
    SigLen := SizeOf(Signature);
    if DSA_sign(0, @Digest[0], 20, @Signature[0], @SigLen, Dsa) = 1 then
    begin
      LogTest('Sign digest', True, Format('Generated %d byte signature', [SigLen]));
      
      // Verify the signature
      VerifyResult := DSA_verify(0, @Digest[0], 20, @Signature[0], SigLen, Dsa);
      LogTest('Verify signature', VerifyResult = 1,
              Format('Verification result: %d', [VerifyResult]));
      
      // Test tamper detection: modify digest
      Digest[0] := Digest[0] xor $FF;
      VerifyResult := DSA_verify(0, @Digest[0], 20, @Signature[0], SigLen, Dsa);
      LogTest('Detect tampered data', VerifyResult = 0,
              Format('Tamper detection result: %d (should be 0)', [VerifyResult]));
    end
    else
      LogTest('Sign digest', False, 'DSA_sign failed');
    
  finally
    DSA_free(Dsa);
  end;
end;

procedure TestDSADoSignVerify;
var
  Dsa: PDSA;
  Digest: array[0..19] of Byte;
  Sig: PDSA_SIG;
  I: Integer;
  VerifyResult: Integer;
  R, S: PBIGNUM;
begin
  WriteLn;
  WriteLn('=== DSA_do_sign/verify Tests ===');
  WriteLn;
  
  // Generate test key
  Dsa := DSA_new();
  if Dsa = nil then
  begin
    LogTest('Create DSA structure for do_sign', False);
    Exit;
  end;
  
  try
    // Generate parameters and key
    if DSA_generate_parameters_ex(Dsa, 1024, nil, 0, nil, nil, nil) <> 1 then
    begin
      LogTest('Generate DSA parameters for do_sign', False);
      Exit;
    end;
    
    if DSA_generate_key(Dsa) <> 1 then
    begin
      LogTest('Generate DSA key for do_sign', False);
      Exit;
    end;
    
    // Create digest
    for I := 0 to 19 do
      Digest[I] := Byte(19 - I);
    
    // Use DSA_do_sign to generate signature
    Sig := DSA_do_sign(@Digest[0], 20, Dsa);
    if Sig <> nil then
    begin
      LogTest('DSA_do_sign generate signature', True);
      
      // Get R and S values
      R := nil;
      S := nil;
      DSA_SIG_get0(Sig, @R, @S);
      LogTest('Get signature R and S values', (R <> nil) and (S <> nil));
      
      // Verify signature
      VerifyResult := DSA_do_verify(@Digest[0], 20, Sig, Dsa);
      LogTest('DSA_do_verify verification', VerifyResult = 1,
              Format('Verification result: %d', [VerifyResult]));
      
      DSA_SIG_free(Sig);
    end
    else
      LogTest('DSA_do_sign generate signature', False);
    
  finally
    DSA_free(Dsa);
  end;
end;

procedure TestDSASize;
var
  Dsa: PDSA;
  SigSize: Integer;
begin
  WriteLn;
  WriteLn('=== DSA Signature Size Tests ===');
  WriteLn;
  
  // 1024-bit
  Dsa := DSA_new();
  if Dsa <> nil then
  begin
    if (DSA_generate_parameters_ex(Dsa, 1024, nil, 0, nil, nil, nil) = 1) and
       (DSA_generate_key(Dsa) = 1) then
    begin
      SigSize := DSA_size(Dsa);
      LogTest('1024-bit signature size', SigSize > 0,
              Format('Max signature size: %d bytes', [SigSize]));
    end;
    DSA_free(Dsa);
  end;
  
  // 2048-bit
  Dsa := DSA_new();
  if Dsa <> nil then
  begin
    if (DSA_generate_parameters_ex(Dsa, 2048, nil, 0, nil, nil, nil) = 1) and
       (DSA_generate_key(Dsa) = 1) then
    begin
      SigSize := DSA_size(Dsa);
      LogTest('2048-bit signature size', SigSize > 0,
              Format('Max signature size: %d bytes', [SigSize]));
    end;
    DSA_free(Dsa);
  end;
end;

procedure TestDSAParameters;
var
  Dsa: PDSA;
  P, Q, G: PBIGNUM;
  PubKey, PrivKey: PBIGNUM;
begin
  WriteLn;
  WriteLn('=== DSA Parameter Access Tests ===');
  WriteLn;
  
  Dsa := DSA_new();
  if Dsa = nil then
  begin
    LogTest('Create DSA for parameter test', False);
    Exit;
  end;
  
  try
    // Generate parameters and key
    if (DSA_generate_parameters_ex(Dsa, 1024, nil, 0, nil, nil, nil) = 1) and
       (DSA_generate_key(Dsa) = 1) then
    begin
      // Get parameters (p, q, g)
      P := nil;
      Q := nil;
      G := nil;
      DSA_get0_pqg(Dsa, @P, @Q, @G);
      LogTest('Get DSA parameters (p, q, g)', (P <> nil) and (Q <> nil) and (G <> nil));
      
      // Get keys
      PubKey := nil;
      PrivKey := nil;
      DSA_get0_key(Dsa, @PubKey, @PrivKey);
      LogTest('Get DSA public key', PubKey <> nil);
      LogTest('Get DSA private key', PrivKey <> nil);
      
      // Individual accessors
      LogTest('Get DSA p parameter', DSA_get0_p(Dsa) <> nil);
      LogTest('Get DSA q parameter', DSA_get0_q(Dsa) <> nil);
      LogTest('Get DSA g parameter', DSA_get0_g(Dsa) <> nil);
      LogTest('Get DSA public key accessor', DSA_get0_pub_key(Dsa) <> nil);
      LogTest('Get DSA private key accessor', DSA_get0_priv_key(Dsa) <> nil);
    end
    else
      LogTest('Generate DSA for parameter test', False);
    
  finally
    DSA_free(Dsa);
  end;
end;

begin
  WriteLn('DSA (Digital Signature Algorithm) Integration Tests');
  WriteLn('===================================================');
  WriteLn;
  
  // Load core OpenSSL library
  LoadOpenSSLCore;
  
  // Load DSA module
  if not LoadOpenSSLDSA then
  begin
    WriteLn('ERROR: Failed to load DSA module');
    Halt(1);
  end;
  
  // Load BN module
  if not LoadOpenSSLBN then
  begin
    WriteLn('ERROR: Failed to load BN module');
    Halt(1);
  end;
  
  WriteLn('NOTE: DSA key generation may take some time...');
  WriteLn;
  
  try
    // Run test suites
    TestDSAKeyGeneration;
    TestDSASignVerify;
    TestDSADoSignVerify;
    TestDSASize;
    TestDSAParameters;
    
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
