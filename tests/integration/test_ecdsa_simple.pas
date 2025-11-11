program test_ecdsa_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.types,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.ecdsa,
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

procedure TestECDSAKeyGeneration;
var
  Key: PEC_KEY;
  Group: PEC_GROUP;
begin
  WriteLn;
  WriteLn('=== ECDSA Key Generation Tests ===');
  WriteLn;
  
  // Test P-256 (NIST prime256v1) curve key generation
  Key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  LogTest('Create P-256 EC_KEY structure', Key <> nil);
  
  if Key <> nil then
  begin
    // Generate key pair
    if EC_KEY_generate_key(Key) = 1 then
    begin
      LogTest('Generate P-256 key pair', True);
      
      // Get group info
      Group := EC_KEY_get0_group(Key);
      LogTest('Get EC group', Group <> nil);
      
      // Validate key
      if EC_KEY_check_key(Key) = 1 then
        LogTest('Validate key', True)
      else
        LogTest('Validate key', False);
    end
    else
      LogTest('Generate P-256 key pair', False, 'EC_KEY_generate_key failed');
      
    EC_KEY_free(Key);
  end;
  
  // Test secp384r1 curve
  Key := EC_KEY_new_by_curve_name(NID_secp384r1);
  LogTest('Create secp384r1 EC_KEY structure', Key <> nil);
  
  if Key <> nil then
  begin
    if EC_KEY_generate_key(Key) = 1 then
      LogTest('Generate secp384r1 key pair', True)
    else
      LogTest('Generate secp384r1 key pair', False);
      
    EC_KEY_free(Key);
  end;
end;

procedure TestECDSASignVerify;
var
  Key: PEC_KEY;
  Digest: array[0..31] of Byte;
  Signature: array[0..255] of Byte;
  SigLen: Cardinal;
  I: Integer;
  VerifyResult: Integer;
begin
  WriteLn;
  WriteLn('=== ECDSA Sign/Verify Tests ===');
  WriteLn;
  
  // Generate test key
  Key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if Key = nil then
  begin
    LogTest('Create signing EC_KEY', False);
    Exit;
  end;
  
  try
    // Generate key
    if EC_KEY_generate_key(Key) <> 1 then
    begin
      LogTest('Generate signing key', False);
      Exit;
    end;
    
    LogTest('Generate signing key', True);
    
    // Create fake digest (normally SHA-256 output)
    for I := 0 to 31 do
      Digest[I] := Byte(I);
    
    // Sign digest
    SigLen := SizeOf(Signature);
    if ECDSA_sign(0, @Digest[0], 32, @Signature[0], @SigLen, Key) = 1 then
    begin
      LogTest('Sign digest', True, Format('Generated %d byte signature', [SigLen]));
      
      // Verify signature
      VerifyResult := ECDSA_verify(0, @Digest[0], 32, @Signature[0], SigLen, Key);
      LogTest('Verify signature', VerifyResult = 1,
              Format('Verification result: %d', [VerifyResult]));
      
      // Test tamper detection: modify digest
      Digest[0] := Digest[0] xor $FF;
      VerifyResult := ECDSA_verify(0, @Digest[0], 32, @Signature[0], SigLen, Key);
      LogTest('Detect tampered data', VerifyResult = 0,
              Format('Tamper detection result: %d (should be 0)', [VerifyResult]));
    end
    else
      LogTest('Sign digest', False, 'ECDSA_sign failed');
    
  finally
    EC_KEY_free(Key);
  end;
end;

procedure TestECDSADoSignVerify;
var
  Key: PEC_KEY;
  Digest: array[0..31] of Byte;
  Sig: PECDSA_SIG;
  I: Integer;
  VerifyResult: Integer;
  R, S: PBIGNUM;
begin
  WriteLn;
  WriteLn('=== ECDSA_do_sign/verify Tests ===');
  WriteLn;
  
  // Generate test key
  Key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if Key = nil then
  begin
    LogTest('Create do_sign EC_KEY', False);
    Exit;
  end;
  
  try
    // Generate key
    if EC_KEY_generate_key(Key) <> 1 then
    begin
      LogTest('Generate do_sign key', False);
      Exit;
    end;
    
    // Create digest
    for I := 0 to 31 do
      Digest[I] := Byte(31 - I);
    
    // Use ECDSA_do_sign to generate signature
    Sig := ECDSA_do_sign(@Digest[0], 32, Key);
    if Sig <> nil then
    begin
      LogTest('ECDSA_do_sign generate signature', True);
      
      // Get R and S values
      R := ECDSA_SIG_get0_r(Sig);
      S := ECDSA_SIG_get0_s(Sig);
      LogTest('Get signature R and S values', (R <> nil) and (S <> nil));
      
      // Verify signature
      VerifyResult := ECDSA_do_verify(@Digest[0], 32, Sig, Key);
      LogTest('ECDSA_do_verify verification', VerifyResult = 1,
              Format('Verification result: %d', [VerifyResult]));
      
      ECDSA_SIG_free(Sig);
    end
    else
      LogTest('ECDSA_do_sign generate signature', False);
    
  finally
    EC_KEY_free(Key);
  end;
end;

procedure TestECDSASize;
var
  Key: PEC_KEY;
  SigSize: Integer;
begin
  WriteLn;
  WriteLn('=== ECDSA Signature Size Tests ===');
  WriteLn;
  
  // P-256
  Key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if Key <> nil then
  begin
    if EC_KEY_generate_key(Key) = 1 then
    begin
      SigSize := ECDSA_size(Key);
      LogTest('P-256 signature size', SigSize > 0,
              Format('Max signature size: %d bytes', [SigSize]));
    end;
    EC_KEY_free(Key);
  end;
  
  // secp384r1
  Key := EC_KEY_new_by_curve_name(NID_secp384r1);
  if Key <> nil then
  begin
    if EC_KEY_generate_key(Key) = 1 then
    begin
      SigSize := ECDSA_size(Key);
      LogTest('secp384r1 signature size', SigSize > 0,
              Format('Max signature size: %d bytes', [SigSize]));
    end;
    EC_KEY_free(Key);
  end;
  
  // secp521r1
  Key := EC_KEY_new_by_curve_name(NID_secp521r1);
  if Key <> nil then
  begin
    if EC_KEY_generate_key(Key) = 1 then
    begin
      SigSize := ECDSA_size(Key);
      LogTest('secp521r1 signature size', SigSize > 0,
              Format('Max signature size: %d bytes', [SigSize]));
    end;
    EC_KEY_free(Key);
  end;
end;

begin
  WriteLn('ECDSA Elliptic Curve Digital Signature Integration Tests');
  WriteLn('==========================================================');
  WriteLn;
  
  // Load core OpenSSL library
  LoadOpenSSLCore;
  
  // Load EC module
  if not LoadECFunctions(GetCryptoLibHandle) then
  begin
    WriteLn('ERROR: Failed to load EC module');
    Halt(1);
  end;
  
  // Load ECDSA module
  if not LoadOpenSSLECDSA then
  begin
    WriteLn('ERROR: Failed to load ECDSA module');
    Halt(1);
  end;
  
  // Load BN module
  if not LoadOpenSSLBN then
  begin
    WriteLn('ERROR: Failed to load BN module');
    Halt(1);
  end;
  
  try
    // 运行测试套件
    TestECDSAKeyGeneration;
    TestECDSASignVerify;
    TestECDSADoSignVerify;
    TestECDSASize;
    
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
    // OpenSSL清理（如果需要）
  end;
end.
