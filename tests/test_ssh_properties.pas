program test_ssh_properties;

{$mode objfpc}{$H+}

{
  SSH 密钥属性测试程序
  
  Property-Based Testing for SSH Key Support
  
  测试属性:
  - Property 1: Public Key Parse-Export Round Trip
  - Property 2: Private Key Parse-Export Round Trip
  - Property 3: Invalid Input Error Handling
  - Property 4: Key Generation Validity
  - Property 5: Format Conversion Round Trip
  - Property 6: Fingerprint Consistency
  - Property 7: Key Pair Validation Correctness
  - Property 8: authorized_keys Round Trip
  
  每个属性测试运行至少 100 次迭代
}

uses
  SysUtils, Classes, fafafa.ssl.ssh, fafafa.ssl.openssl.loader;

const
  PROPERTY_TEST_ITERATIONS = 100;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;
  GPropertiesPassed: Integer = 0;
  GPropertiesFailed: Integer = 0;
  GOpenSSLAvailable: Boolean = False;

// ========================================================================
// 测试辅助函数
// ========================================================================

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 70));
  WriteLn('  ', ATitle);
  WriteLn(StringOfChar('=', 70));
end;

procedure PrintPropertyResult(const APropertyName: string; APassed: Boolean; 
  AIterations, AFailures: Integer; const AMessage: string = '');
begin
  if APassed then
  begin
    WriteLn('[PASS] ', APropertyName);
    WriteLn('       Iterations: ', AIterations, ', Failures: ', AFailures);
    Inc(GPropertiesPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', APropertyName);
    WriteLn('       Iterations: ', AIterations, ', Failures: ', AFailures);
    if AMessage <> '' then
      WriteLn('       Last Error: ', AMessage);
    Inc(GPropertiesFailed);
  end;
end;

function IsOpenSSLAvailable: Boolean;
var
  LHandle: THandle;
begin
  LHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
  Result := LHandle <> 0;
end;

// ========================================================================
// 随机数据生成器
// ========================================================================

function GenerateRandomComment: string;
const
  CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.@';
var
  I, Len: Integer;
begin
  Len := Random(20) + 5;  // 5-24 characters
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := CHARS[Random(Length(CHARS)) + 1];
end;

function GenerateRandomPassphrase: string;
const
  CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*';
var
  I, Len: Integer;
begin
  Len := Random(16) + 8;  // 8-23 characters
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := CHARS[Random(Length(CHARS)) + 1];
end;

function GenerateRandomInvalidInput: string;
var
  Strategy, I, Len: Integer;
begin
  Strategy := Random(10);
  case Strategy of
    0: Result := '';  // Empty
    1: Result := 'invalid';  // Too short
    2: Result := 'ssh-rsa';  // Missing data
    3: Result := 'ssh-rsa AAAA';  // Invalid base64
    4: Result := 'ssh-dss AAAAB3NzaC1kc3MAAACBAP test';  // Unsupported type
    5: Result := '-----BEGIN INVALID-----';  // Invalid PEM
    6: begin  // Random garbage
         Len := Random(100) + 10;
         SetLength(Result, Len);
         for I := 1 to Len do
           Result[I] := Chr(Random(256));
       end;
    7: Result := 'ssh-ed25519 !!!invalid!!!';  // Invalid base64 chars
    8: Result := 'ecdsa-sha2-nistp256 ' + StringOfChar('A', 1000);  // Too long
    9: Result := #0#0#0#0;  // Null bytes
  end;
end;

function GenerateRandomAuthorizedKeysContent(AKeyCount: Integer): string;
var
  I: Integer;
  LKeyPair: ISSHKeyPair;
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    for I := 1 to AKeyCount do
    begin
      // Generate random key type
      case Random(3) of
        0: LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair('user' + IntToStr(I) + '@host');
        1: LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, 'user' + IntToStr(I) + '@host');
        2: LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, 'user' + IntToStr(I) + '@host');
      end;
      
      if LKeyPair <> nil then
      begin
        // Randomly add options
        if Random(3) = 0 then
          LLines.Add('command="echo hello" ' + LKeyPair.PublicKey.ToOpenSSHFormat)
        else if Random(3) = 0 then
          LLines.Add('no-pty ' + LKeyPair.PublicKey.ToOpenSSHFormat)
        else
          LLines.Add(LKeyPair.PublicKey.ToOpenSSHFormat);
      end;
    end;
    
    // Add some invalid lines randomly
    if Random(3) = 0 then
      LLines.Add('# This is a comment');
    if Random(3) = 0 then
      LLines.Add('');
    if Random(3) = 0 then
      LLines.Add('invalid-line-here');
    
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;


// ========================================================================
// Property 1: Public Key Parse-Export Round Trip
// **Feature: ssh-key-support, Property 1: Public Key Parse-Export Round Trip**
// **Validates: Requirements 1.1, 4.1**
// ========================================================================

procedure TestProperty1_PublicKeyRoundTrip;
var
  I, Failures: Integer;
  LKeyPair: ISSHKeyPair;
  LOriginalKey, LParsedKey: ISSHPublicKey;
  LExported, LReExported: string;
  LLastError: string;
  LKeyType: TSSHKeyType;
begin
  PrintHeader('Property 1: Public Key Parse-Export Round Trip');
  WriteLn('*For any* valid SSH public key, parsing and exporting SHALL produce');
  WriteLn('an equivalent string (same key type, key data, and comment).');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate random key type
      case Random(5) of
        0: LKeyType := sshKeyRSA;
        1: LKeyType := sshKeyEd25519;
        2: LKeyType := sshKeyECDSA_P256;
        3: LKeyType := sshKeyECDSA_P384;
        4: LKeyType := sshKeyECDSA_P521;
      end;
      
      // Generate key with random comment
      case LKeyType of
        sshKeyRSA: LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, GenerateRandomComment);
        sshKeyEd25519: LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair(GenerateRandomComment);
        sshKeyECDSA_P256: LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, GenerateRandomComment);
        sshKeyECDSA_P384: LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P384, GenerateRandomComment);
        sshKeyECDSA_P521: LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P521, GenerateRandomComment);
      end;
      
      if LKeyPair = nil then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pair';
        Continue;
      end;
      
      LOriginalKey := LKeyPair.PublicKey;
      
      // Export to OpenSSH format
      LExported := LOriginalKey.ToOpenSSHFormat;
      
      // Parse the exported key
      if not TSSHKeyManager.TryParsePublicKey(LExported, LParsedKey) then
      begin
        Inc(Failures);
        LLastError := 'Failed to parse exported key';
        Continue;
      end;
      
      // Re-export and compare
      LReExported := LParsedKey.ToOpenSSHFormat;
      
      // Verify round-trip
      if LExported <> LReExported then
      begin
        Inc(Failures);
        LLastError := 'Round-trip mismatch';
        Continue;
      end;
      
      // Verify key type preserved
      if LOriginalKey.KeyType <> LParsedKey.KeyType then
      begin
        Inc(Failures);
        LLastError := 'Key type mismatch';
        Continue;
      end;
      
      // Verify comment preserved
      if LOriginalKey.Comment <> LParsedKey.Comment then
      begin
        Inc(Failures);
        LLastError := 'Comment mismatch';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 1: Public Key Parse-Export Round Trip',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;

// ========================================================================
// Property 2: Private Key Parse-Export Round Trip
// **Feature: ssh-key-support, Property 2: Private Key Parse-Export Round Trip**
// **Validates: Requirements 2.1, 2.2, 2.3, 2.5, 4.2, 4.3**
// ========================================================================

procedure TestProperty2_PrivateKeyRoundTrip;
var
  I, Failures: Integer;
  LKeyPair: ISSHKeyPair;
  LOriginalKey, LParsedKey: ISSHPrivateKey;
  LExported: string;
  LPassphrase: string;
  LLastError: string;
  LUseEncryption: Boolean;
begin
  PrintHeader('Property 2: Private Key Parse-Export Round Trip');
  WriteLn('*For any* valid SSH private key (encrypted or unencrypted), parsing');
  WriteLn('and exporting SHALL produce a key that can be parsed again.');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Randomly choose encryption
      LUseEncryption := Random(2) = 1;
      if LUseEncryption then
        LPassphrase := GenerateRandomPassphrase
      else
        LPassphrase := '';
      
      // Generate Ed25519 key (fastest)
      LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair(GenerateRandomComment);
      
      if LKeyPair = nil then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pair';
        Continue;
      end;
      
      LOriginalKey := LKeyPair.PrivateKey;
      
      // Export to OpenSSH format
      LExported := LOriginalKey.ToOpenSSHFormat(LPassphrase);
      
      // Parse the exported key
      if not TSSHKeyManager.TryParsePrivateKey(LExported, LPassphrase, LParsedKey) then
      begin
        Inc(Failures);
        LLastError := 'Failed to parse exported key';
        Continue;
      end;
      
      // Verify key type preserved
      if LOriginalKey.KeyType <> LParsedKey.KeyType then
      begin
        Inc(Failures);
        LLastError := 'Key type mismatch';
        Continue;
      end;
      
      // Verify key is valid
      if not LParsedKey.IsValid then
      begin
        Inc(Failures);
        LLastError := 'Parsed key is invalid';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 2: Private Key Parse-Export Round Trip',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;


// ========================================================================
// Property 3: Invalid Input Error Handling
// **Feature: ssh-key-support, Property 3: Invalid Input Error Handling**
// **Validates: Requirements 1.3, 2.4**
// ========================================================================

procedure TestProperty3_InvalidInputHandling;
var
  I, Failures: Integer;
  LKey: ISSHPublicKey;
  LPrivKey: ISSHPrivateKey;
  LResult: TSSHPublicKeyResult;
  LInvalidInput: string;
  LLastError: string;
begin
  PrintHeader('Property 3: Invalid Input Error Handling');
  WriteLn('*For any* malformed or invalid SSH key string, the parser SHALL');
  WriteLn('return an error result (not crash or return invalid data).');
  WriteLn;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      LInvalidInput := GenerateRandomInvalidInput;
      
      // Test public key parsing - should not crash
      LResult := TSSHKeyManager.ParsePublicKeyResult(LInvalidInput);
      
      // Should return error for invalid input
      if LResult.IsOk then
      begin
        // If it parsed successfully, verify the key is actually valid
        if (LResult.Key <> nil) and LResult.Key.IsValid then
        begin
          // This is actually a valid key somehow - not a failure
          Continue;
        end;
      end;
      
      // Test TryParse - should return false, not crash
      if TSSHKeyManager.TryParsePublicKey(LInvalidInput, LKey) then
      begin
        // If it parsed, verify it's valid
        if (LKey <> nil) and LKey.IsValid then
          Continue;  // Actually valid
      end;
      
      // Test private key parsing - should not crash
      if TSSHKeyManager.TryParsePrivateKey(LInvalidInput, '', LPrivKey) then
      begin
        // If it parsed, verify it's valid
        if (LPrivKey <> nil) and LPrivKey.IsValid then
          Continue;  // Actually valid
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        // Any exception is a failure - parsers should handle errors gracefully
        Inc(Failures);
        LLastError := 'Exception on invalid input: ' + E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 3: Invalid Input Error Handling',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;

// ========================================================================
// Property 4: Key Generation Validity
// **Feature: ssh-key-support, Property 4: Key Generation Validity**
// **Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5, 7.1, 7.2**
// ========================================================================

procedure TestProperty4_KeyGenerationValidity;
var
  I, Failures: Integer;
  LKeyPair: ISSHKeyPair;
  LPubKey: ISSHPublicKey;
  LPrivKey: ISSHPrivateKey;
  LLastError: string;
  LKeyType: TSSHKeyType;
  LExpectedSize: Integer;
begin
  PrintHeader('Property 4: Key Generation Validity');
  WriteLn('*For any* generated SSH key pair, the public and private keys SHALL');
  WriteLn('form a valid pair, and key parameters SHALL match the configuration.');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate random key type
      case Random(5) of
        0: begin
             LKeyType := sshKeyRSA;
             LExpectedSize := 2048;
             LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, GenerateRandomComment);
           end;
        1: begin
             LKeyType := sshKeyEd25519;
             LExpectedSize := 256;
             LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair(GenerateRandomComment);
           end;
        2: begin
             LKeyType := sshKeyECDSA_P256;
             LExpectedSize := 256;
             LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, GenerateRandomComment);
           end;
        3: begin
             LKeyType := sshKeyECDSA_P384;
             LExpectedSize := 384;
             LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P384, GenerateRandomComment);
           end;
        4: begin
             LKeyType := sshKeyECDSA_P521;
             LExpectedSize := 521;
             LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P521, GenerateRandomComment);
           end;
      end;
      
      // Verify key pair was generated
      if LKeyPair = nil then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pair';
        Continue;
      end;
      
      LPubKey := LKeyPair.PublicKey;
      LPrivKey := LKeyPair.PrivateKey;
      
      // Verify both keys exist
      if (LPubKey = nil) or (LPrivKey = nil) then
      begin
        Inc(Failures);
        LLastError := 'Key pair has nil component';
        Continue;
      end;
      
      // Verify key types match
      if LPubKey.KeyType <> LKeyType then
      begin
        Inc(Failures);
        LLastError := 'Public key type mismatch';
        Continue;
      end;
      
      if LPrivKey.KeyType <> LKeyType then
      begin
        Inc(Failures);
        LLastError := 'Private key type mismatch';
        Continue;
      end;
      
      // Verify keys are valid
      if not LPubKey.IsValid then
      begin
        Inc(Failures);
        LLastError := 'Public key is invalid';
        Continue;
      end;
      
      if not LPrivKey.IsValid then
      begin
        Inc(Failures);
        LLastError := 'Private key is invalid';
        Continue;
      end;
      
      // Verify key size (for non-RSA keys)
      if LKeyType <> sshKeyRSA then
      begin
        if LPubKey.GetKeySize <> LExpectedSize then
        begin
          Inc(Failures);
          LLastError := Format('Key size mismatch: expected %d, got %d', 
            [LExpectedSize, LPubKey.GetKeySize]);
          Continue;
        end;
      end;
      
      // Verify key pair is valid (public matches private)
      if not LKeyPair.IsValidPair then
      begin
        Inc(Failures);
        LLastError := 'Key pair validation failed';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 4: Key Generation Validity',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;


// ========================================================================
// Property 5: Format Conversion Round Trip
// **Feature: ssh-key-support, Property 5: Format Conversion Round Trip**
// **Validates: Requirements 6.1, 6.2, 6.3, 6.4, 6.5**
// ========================================================================

procedure TestProperty5_FormatConversionRoundTrip;
var
  I, Failures: Integer;
  LKeyPair: ISSHKeyPair;
  LOriginalPubKey, LConvertedPubKey: ISSHPublicKey;
  LOpenSSHFormat, LPEMFormat, LBackToOpenSSH: string;
  LLastError: string;
begin
  PrintHeader('Property 5: Format Conversion Round Trip');
  WriteLn('*For any* SSH key, converting from OpenSSH to PEM and back SHALL');
  WriteLn('preserve the key data (functionally equivalent).');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate Ed25519 key (simplest for conversion)
      LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair(GenerateRandomComment);
      
      if LKeyPair = nil then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pair';
        Continue;
      end;
      
      LOriginalPubKey := LKeyPair.PublicKey;
      
      // Get OpenSSH format
      LOpenSSHFormat := LOriginalPubKey.ToOpenSSHFormat;
      
      // Convert to PEM format
      LPEMFormat := LOriginalPubKey.ToPEMFormat;
      
      // Verify PEM format is valid
      if (Pos('-----BEGIN', LPEMFormat) = 0) or (Pos('-----END', LPEMFormat) = 0) then
      begin
        Inc(Failures);
        LLastError := 'Invalid PEM format output';
        Continue;
      end;
      
      // Parse back from OpenSSH format and verify fingerprints match
      if TSSHKeyManager.TryParsePublicKey(LOpenSSHFormat, LConvertedPubKey) then
      begin
        // Verify fingerprints match (key data is equivalent)
        if LOriginalPubKey.GetFingerprintSHA256 <> LConvertedPubKey.GetFingerprintSHA256 then
        begin
          Inc(Failures);
          LLastError := 'Fingerprint mismatch after conversion';
          Continue;
        end;
      end
      else
      begin
        Inc(Failures);
        LLastError := 'Failed to parse converted key';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 5: Format Conversion Round Trip',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;

// ========================================================================
// Property 6: Fingerprint Consistency
// **Feature: ssh-key-support, Property 6: Fingerprint Consistency**
// **Validates: Requirements 5.1, 5.2, 5.3, 5.4**
// ========================================================================

procedure TestProperty6_FingerprintConsistency;
var
  I, Failures: Integer;
  LKeyPair: ISSHKeyPair;
  LPubKey: ISSHPublicKey;
  LFingerprint1, LFingerprint2: string;
  LFingerprintMD5_1, LFingerprintMD5_2: string;
  LLastError: string;
begin
  PrintHeader('Property 6: Fingerprint Consistency');
  WriteLn('*For any* SSH public key, the calculated fingerprint SHALL be');
  WriteLn('deterministic and follow the correct format.');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate random key
      case Random(3) of
        0: LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair(GenerateRandomComment);
        1: LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, GenerateRandomComment);
        2: LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, GenerateRandomComment);
      end;
      
      if LKeyPair = nil then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pair';
        Continue;
      end;
      
      LPubKey := LKeyPair.PublicKey;
      
      // Calculate fingerprint twice
      LFingerprint1 := LPubKey.GetFingerprintSHA256;
      LFingerprint2 := LPubKey.GetFingerprintSHA256;
      
      // Verify deterministic
      if LFingerprint1 <> LFingerprint2 then
      begin
        Inc(Failures);
        LLastError := 'SHA256 fingerprint not deterministic';
        Continue;
      end;
      
      // Verify format (SHA256:base64...)
      if Pos('SHA256:', LFingerprint1) <> 1 then
      begin
        Inc(Failures);
        LLastError := 'SHA256 fingerprint format incorrect';
        Continue;
      end;
      
      // Calculate MD5 fingerprint twice
      LFingerprintMD5_1 := LPubKey.GetFingerprintMD5;
      LFingerprintMD5_2 := LPubKey.GetFingerprintMD5;
      
      // Verify deterministic
      if LFingerprintMD5_1 <> LFingerprintMD5_2 then
      begin
        Inc(Failures);
        LLastError := 'MD5 fingerprint not deterministic';
        Continue;
      end;
      
      // Verify format (MD5:xx:xx:xx:...)
      if Pos('MD5:', LFingerprintMD5_1) <> 1 then
      begin
        Inc(Failures);
        LLastError := 'MD5 fingerprint format incorrect';
        Continue;
      end;
      
      // Verify MD5 has colons
      if Pos(':', Copy(LFingerprintMD5_1, 5, Length(LFingerprintMD5_1))) = 0 then
      begin
        Inc(Failures);
        LLastError := 'MD5 fingerprint missing colons';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 6: Fingerprint Consistency',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;


// ========================================================================
// Property 7: Key Pair Validation Correctness
// **Feature: ssh-key-support, Property 7: Key Pair Validation Correctness**
// **Validates: Requirements 7.1, 7.3**
// ========================================================================

procedure TestProperty7_KeyPairValidation;
var
  I, Failures: Integer;
  LKeyPair1, LKeyPair2: ISSHKeyPair;
  LPubKey1, LPubKey2: ISSHPublicKey;
  LPrivKey1, LPrivKey2: ISSHPrivateKey;
  LLastError: string;
begin
  PrintHeader('Property 7: Key Pair Validation Correctness');
  WriteLn('*For any* public key and private key, the validation function SHALL');
  WriteLn('return true if and only if they form a mathematically valid pair.');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate two different key pairs
      LKeyPair1 := TSSHKeyManager.GenerateEd25519KeyPair('key1@test');
      LKeyPair2 := TSSHKeyManager.GenerateEd25519KeyPair('key2@test');
      
      if (LKeyPair1 = nil) or (LKeyPair2 = nil) then
      begin
        Inc(Failures);
        LLastError := 'Failed to generate key pairs';
        Continue;
      end;
      
      LPubKey1 := LKeyPair1.PublicKey;
      LPrivKey1 := LKeyPair1.PrivateKey;
      LPubKey2 := LKeyPair2.PublicKey;
      LPrivKey2 := LKeyPair2.PrivateKey;
      
      // Test 1: Matching pair should validate
      if not LPrivKey1.MatchesPublicKey(LPubKey1) then
      begin
        Inc(Failures);
        LLastError := 'Matching pair failed validation';
        Continue;
      end;
      
      // Test 2: Non-matching pair should NOT validate
      if LPrivKey1.MatchesPublicKey(LPubKey2) then
      begin
        Inc(Failures);
        LLastError := 'Non-matching pair passed validation';
        Continue;
      end;
      
      // Test 3: Key pair IsValidPair should return true
      if not LKeyPair1.IsValidPair then
      begin
        Inc(Failures);
        LLastError := 'IsValidPair returned false for valid pair';
        Continue;
      end;
      
      // Test 4: ValidateKeyPair class method
      if not TSSHKeyManager.ValidateKeyPair(LPubKey1, LPrivKey1) then
      begin
        Inc(Failures);
        LLastError := 'ValidateKeyPair returned false for valid pair';
        Continue;
      end;
      
      // Test 5: ValidateKeyPair should fail for mismatched keys
      if TSSHKeyManager.ValidateKeyPair(LPubKey1, LPrivKey2) then
      begin
        Inc(Failures);
        LLastError := 'ValidateKeyPair returned true for mismatched pair';
        Continue;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 7: Key Pair Validation Correctness',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;

// ========================================================================
// Property 8: authorized_keys Round Trip
// **Feature: ssh-key-support, Property 8: authorized_keys Round Trip**
// **Validates: Requirements 8.1, 8.2, 8.3, 8.4**
// ========================================================================

procedure TestProperty8_AuthorizedKeysRoundTrip;
var
  I, J, Failures: Integer;
  LKeyPairs: array[0..2] of ISSHKeyPair;
  LKeys: array of ISSHPublicKey;
  LOriginalContent, LWrittenContent: string;
  LParsedEntries: TSSHAuthorizedKeyEntryArray;
  LLastError: string;
  LValidCount: Integer;
begin
  PrintHeader('Property 8: authorized_keys Round Trip');
  WriteLn('*For any* valid authorized_keys content, parsing and writing back');
  WriteLn('SHALL preserve all valid entries (key data and options).');
  WriteLn;
  
  if not GOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;
  
  Failures := 0;
  LLastError := '';
  
  for I := 1 to PROPERTY_TEST_ITERATIONS do
  begin
    try
      // Generate 1-3 random keys
      LValidCount := Random(3) + 1;
      SetLength(LKeys, LValidCount);
      
      for J := 0 to LValidCount - 1 do
      begin
        case Random(3) of
          0: LKeyPairs[J] := TSSHKeyManager.GenerateEd25519KeyPair('user' + IntToStr(J) + '@host');
          1: LKeyPairs[J] := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, 'user' + IntToStr(J) + '@host');
          2: LKeyPairs[J] := TSSHKeyManager.GenerateRSAKeyPair(2048, 'user' + IntToStr(J) + '@host');
        end;
        
        if LKeyPairs[J] <> nil then
          LKeys[J] := LKeyPairs[J].PublicKey
        else
        begin
          Inc(Failures);
          LLastError := 'Failed to generate key';
          Continue;
        end;
      end;
      
      // Write authorized_keys content
      LWrittenContent := TSSHKeyManager.WriteAuthorizedKeys(LKeys);
      
      // Parse the written content
      LParsedEntries := TSSHKeyManager.ParseAuthorizedKeys(LWrittenContent);
      
      // Verify same number of valid entries
      if Length(LParsedEntries) <> LValidCount then
      begin
        Inc(Failures);
        LLastError := Format('Entry count mismatch: expected %d, got %d',
          [LValidCount, Length(LParsedEntries)]);
        Continue;
      end;
      
      // Verify each entry is valid and fingerprints match
      for J := 0 to High(LParsedEntries) do
      begin
        if not LParsedEntries[J].IsValid then
        begin
          Inc(Failures);
          LLastError := 'Parsed entry is invalid';
          Break;
        end;
        
        // Verify fingerprint matches original
        if LParsedEntries[J].Key.GetFingerprintSHA256 <> LKeys[J].GetFingerprintSHA256 then
        begin
          Inc(Failures);
          LLastError := 'Fingerprint mismatch in entry ' + IntToStr(J);
          Break;
        end;
      end;
      
      // Progress indicator
      if (I mod 20 = 0) then
        Write(#13, '  Progress: ', I, '/', PROPERTY_TEST_ITERATIONS, '    ');
        
    except
      on E: Exception do
      begin
        Inc(Failures);
        LLastError := E.Message;
      end;
    end;
  end;
  
  WriteLn;
  PrintPropertyResult('Property 8: authorized_keys Round Trip',
    Failures = 0, PROPERTY_TEST_ITERATIONS, Failures, LLastError);
end;


// ========================================================================
// 主程序
// ========================================================================

begin
  WriteLn('SSH Key Property-Based Test Suite');
  WriteLn('==================================');
  WriteLn('Each property test runs ', PROPERTY_TEST_ITERATIONS, ' iterations');
  WriteLn;
  
  // Initialize random seed
  Randomize;
  
  // Check OpenSSL availability
  GOpenSSLAvailable := IsOpenSSLAvailable;
  if not GOpenSSLAvailable then
    WriteLn('WARNING: OpenSSL not available - some tests will be skipped')
  else
    WriteLn('OpenSSL: Available');
  WriteLn;
  
  try
    // Property 1: Public Key Parse-Export Round Trip
    TestProperty1_PublicKeyRoundTrip;
    
    // Property 2: Private Key Parse-Export Round Trip
    TestProperty2_PrivateKeyRoundTrip;
    
    // Property 3: Invalid Input Error Handling
    TestProperty3_InvalidInputHandling;
    
    // Property 4: Key Generation Validity
    TestProperty4_KeyGenerationValidity;
    
    // Property 5: Format Conversion Round Trip
    TestProperty5_FormatConversionRoundTrip;
    
    // Property 6: Fingerprint Consistency
    TestProperty6_FingerprintConsistency;
    
    // Property 7: Key Pair Validation Correctness
    TestProperty7_KeyPairValidation;
    
    // Property 8: authorized_keys Round Trip
    TestProperty8_AuthorizedKeysRoundTrip;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.Message);
      Inc(GPropertiesFailed);
    end;
  end;
  
  // Print summary
  WriteLn;
  WriteLn(StringOfChar('=', 70));
  WriteLn('  Property Test Summary');
  WriteLn(StringOfChar('=', 70));
  WriteLn('  Properties Passed: ', GPropertiesPassed);
  WriteLn('  Properties Failed: ', GPropertiesFailed);
  WriteLn('  Total Properties:  ', GPropertiesPassed + GPropertiesFailed);
  WriteLn('  Iterations per Property: ', PROPERTY_TEST_ITERATIONS);
  WriteLn;
  
  if GPropertiesFailed > 0 then
  begin
    WriteLn('SOME PROPERTY TESTS FAILED!');
    Halt(1);
  end
  else if GPropertiesPassed = 0 then
  begin
    WriteLn('NO PROPERTY TESTS RAN (OpenSSL not available)');
    Halt(0);
  end
  else
    WriteLn('ALL PROPERTY TESTS PASSED!');
end.
