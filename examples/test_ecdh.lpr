program test_ecdh;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.bn,
  fafafa.ssl.openssl.ec,
  fafafa.ssl.openssl.ecdh,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.err;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure PrintTestResult(const TestName: string; Passed: Boolean);
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

function TestECDHLoadFunctions: Boolean;
var
  LLib: TLibHandle;
begin
  Result := False;
  
  try
    // Load core library first
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('Failed to load OpenSSL Core: ', E.Message);
      PrintTestResult('ECDH - Load functions', False);
      Exit;
    end;
  end;
  
  // Get crypto library handle
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    WriteLn('Failed to get crypto library handle');
    PrintTestResult('ECDH - Load functions', False);
    Exit;
  end;
  
  // Load EC functions first (required by ECDH)
  if not LoadECFunctions(LLib) then
  begin
    WriteLn('Failed to load EC functions');
    PrintTestResult('ECDH - Load functions', False);
    Exit;
  end;
  
  // Load ECDH functions
  if not LoadOpenSSLECDH then
  begin
    WriteLn('Failed to load ECDH functions');
    PrintTestResult('ECDH - Load functions', False);
    Exit;
  end;
  
  Result := True;
  PrintTestResult('ECDH - Load functions', Result);
end;

function TestECDHBasicKeyExchange: Boolean;
var
  LKey1, LKey2: PEC_KEY;
  LGroup: PEC_GROUP;
  LPubKey1, LPubKey2: PEC_POINT;
  LSecret1, LSecret2: TBytes;
  i: Integer;
begin
  Result := False;
  
  // Load required modules
  if not LoadOpenSSLBN then Exit;
  if not LoadOpenSSLECDH then Exit;
  
  LKey1 := nil;
  LKey2 := nil;
  LGroup := nil;
  
  try
    // Create EC group using secp256r1 (prime256v1)
    LGroup := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
    if not Assigned(LGroup) then Exit;
    
    // Generate first key pair
    LKey1 := EC_KEY_new();
    if not Assigned(LKey1) then Exit;
    
    if EC_KEY_set_group(LKey1, LGroup) <> 1 then Exit;
    if EC_KEY_generate_key(LKey1) <> 1 then Exit;
    
    // Generate second key pair
    LKey2 := EC_KEY_new();
    if not Assigned(LKey2) then Exit;
    
    if EC_KEY_set_group(LKey2, LGroup) <> 1 then Exit;
    if EC_KEY_generate_key(LKey2) <> 1 then Exit;
    
    // Get public keys
    LPubKey1 := EC_KEY_get0_public_key(LKey1);
    LPubKey2 := EC_KEY_get0_public_key(LKey2);
    
    if not Assigned(LPubKey1) or not Assigned(LPubKey2) then Exit;
    
    // Compute shared secrets
    if not ECDH_ComputeSharedSecret(LKey1, LPubKey2, LSecret1) then Exit;
    if not ECDH_ComputeSharedSecret(LKey2, LPubKey1, LSecret2) then Exit;
    
    // Verify both secrets match
    if Length(LSecret1) <> Length(LSecret2) then Exit;
    if Length(LSecret1) = 0 then Exit;
    
    for i := 0 to Length(LSecret1) - 1 do
      if LSecret1[i] <> LSecret2[i] then Exit;
    
    Result := True;
  finally
    if Assigned(LKey1) then EC_KEY_free(LKey1);
    if Assigned(LKey2) then EC_KEY_free(LKey2);
    if Assigned(LGroup) then EC_GROUP_free(LGroup);
  end;
  
  PrintTestResult('ECDH - Basic key exchange', Result);
end;

function TestECDHDifferentCurves: Boolean;
var
  LKey1, LKey2: PEC_KEY;
  LGroup: PEC_GROUP;
  LPubKey1, LPubKey2: PEC_POINT;
  LSecret1, LSecret2: TBytes;
  i: Integer;
  LCurves: array[0..2] of Integer;
  LCurveIdx: Integer;
begin
  Result := True;
  
  // Test with different curves
  LCurves[0] := NID_X9_62_prime256v1;  // secp256r1
  LCurves[1] := NID_secp384r1;          // secp384r1
  LCurves[2] := NID_secp521r1;          // secp521r1
  
  for LCurveIdx := 0 to High(LCurves) do
  begin
    LKey1 := nil;
    LKey2 := nil;
    LGroup := nil;
    
    try
      LGroup := EC_GROUP_new_by_curve_name(LCurves[LCurveIdx]);
      if not Assigned(LGroup) then
      begin
        Result := False;
        Break;
      end;
      
      // Generate first key pair
      LKey1 := EC_KEY_new();
      if not Assigned(LKey1) then
      begin
        Result := False;
        Break;
      end;
      
      if EC_KEY_set_group(LKey1, LGroup) <> 1 then
      begin
        Result := False;
        Break;
      end;
      
      if EC_KEY_generate_key(LKey1) <> 1 then
      begin
        Result := False;
        Break;
      end;
      
      // Generate second key pair
      LKey2 := EC_KEY_new();
      if not Assigned(LKey2) then
      begin
        Result := False;
        Break;
      end;
      
      if EC_KEY_set_group(LKey2, LGroup) <> 1 then
      begin
        Result := False;
        Break;
      end;
      
      if EC_KEY_generate_key(LKey2) <> 1 then
      begin
        Result := False;
        Break;
      end;
      
      // Get public keys
      LPubKey1 := EC_KEY_get0_public_key(LKey1);
      LPubKey2 := EC_KEY_get0_public_key(LKey2);
      
      if not Assigned(LPubKey1) or not Assigned(LPubKey2) then
      begin
        Result := False;
        Break;
      end;
      
      // Compute shared secrets
      if not ECDH_ComputeSharedSecret(LKey1, LPubKey2, LSecret1) then
      begin
        Result := False;
        Break;
      end;
      
      if not ECDH_ComputeSharedSecret(LKey2, LPubKey1, LSecret2) then
      begin
        Result := False;
        Break;
      end;
      
      // Verify both secrets match
      if Length(LSecret1) <> Length(LSecret2) then
      begin
        Result := False;
        Break;
      end;
      
      if Length(LSecret1) = 0 then
      begin
        Result := False;
        Break;
      end;
      
      for i := 0 to Length(LSecret1) - 1 do
        if LSecret1[i] <> LSecret2[i] then
        begin
          Result := False;
          Break;
        end;
        
      if not Result then Break;
      
    finally
      if Assigned(LKey1) then EC_KEY_free(LKey1);
      if Assigned(LKey2) then EC_KEY_free(LKey2);
      if Assigned(LGroup) then EC_GROUP_free(LGroup);
    end;
  end;
  
  PrintTestResult('ECDH - Different curves (secp256r1, secp384r1, secp521r1)', Result);
end;

function TestECDHCofactorMode: Boolean;
var
  LKey: PEC_KEY;
  LGroup: PEC_GROUP;
  LFlags: Integer;
begin
  Result := False;
  
  if not LoadOpenSSLECDH then Exit;
  
  LKey := nil;
  LGroup := nil;
  
  try
    LGroup := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
    if not Assigned(LGroup) then Exit;
    
    LKey := EC_KEY_new();
    if not Assigned(LKey) then Exit;
    
    if EC_KEY_set_group(LKey, LGroup) <> 1 then Exit;
    if EC_KEY_generate_key(LKey) <> 1 then Exit;
    
    // Test setting cofactor mode
    if not ECDH_SetCofactorMode(LKey, True) then Exit;
    
    // Verify flag is set (if functions are available)
    if Assigned(EC_KEY_get_flags) then
    begin
      LFlags := EC_KEY_get_flags(LKey);
      if (LFlags and ECDH_FLAG_COFACTOR_ECDH) = 0 then Exit;
    end;
    
    // Test clearing cofactor mode
    if not ECDH_SetCofactorMode(LKey, False) then Exit;
    
    // Verify flag is cleared (if functions are available)
    if Assigned(EC_KEY_get_flags) then
    begin
      LFlags := EC_KEY_get_flags(LKey);
      if (LFlags and ECDH_FLAG_COFACTOR_ECDH) <> 0 then Exit;
    end;
    
    Result := True;
  finally
    if Assigned(LKey) then EC_KEY_free(LKey);
    if Assigned(LGroup) then EC_GROUP_free(LGroup);
  end;
  
  PrintTestResult('ECDH - Cofactor mode', Result);
end;

function TestECDHComputeKeyRawAPI: Boolean;
var
  LKey1, LKey2: PEC_KEY;
  LGroup: PEC_GROUP;
  LPubKey2: PEC_POINT;
  LSecret: array[0..255] of Byte;
  LSecretLen: Integer;
begin
  Result := False;
  
  if not LoadOpenSSLBN then Exit;
  if not LoadOpenSSLECDH then Exit;
  
  if not Assigned(ECDH_compute_key) then
  begin
    // Function might not be available in newer OpenSSL versions
    Result := True;
    PrintTestResult('ECDH - Compute key raw API (skipped - not available)', Result);
    Exit;
  end;
  
  LKey1 := nil;
  LKey2 := nil;
  LGroup := nil;
  
  try
    LGroup := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
    if not Assigned(LGroup) then Exit;
    
    // Generate first key pair
    LKey1 := EC_KEY_new();
    if not Assigned(LKey1) then Exit;
    if EC_KEY_set_group(LKey1, LGroup) <> 1 then Exit;
    if EC_KEY_generate_key(LKey1) <> 1 then Exit;
    
    // Generate second key pair
    LKey2 := EC_KEY_new();
    if not Assigned(LKey2) then Exit;
    if EC_KEY_set_group(LKey2, LGroup) <> 1 then Exit;
    if EC_KEY_generate_key(LKey2) <> 1 then Exit;
    
    // Get public key from second key
    LPubKey2 := EC_KEY_get0_public_key(LKey2);
    if not Assigned(LPubKey2) then Exit;
    
    // Compute shared secret using raw API
    LSecretLen := ECDH_compute_key(@LSecret[0], SizeOf(LSecret), LPubKey2, LKey1, nil);
    if LSecretLen <= 0 then Exit;
    
    Result := True;
  finally
    if Assigned(LKey1) then EC_KEY_free(LKey1);
    if Assigned(LKey2) then EC_KEY_free(LKey2);
    if Assigned(LGroup) then EC_GROUP_free(LGroup);
  end;
  
  PrintTestResult('ECDH - Compute key raw API', Result);
end;

function TestECDHInvalidInputs: Boolean;
var
  LSecret: TBytes;
begin
  Result := True;
  
  if not LoadOpenSSLECDH then Exit;
  
  // Test with nil key
  if ECDH_ComputeSharedSecret(nil, nil, LSecret) then
    Result := False;
  
  PrintTestResult('ECDH - Invalid inputs handling', Result);
end;

procedure RunAllTests;
begin
  WriteLn('====================================');
  WriteLn('ECDH Module Tests');
  WriteLn('====================================');
  WriteLn;
  
  TestECDHLoadFunctions;
  TestECDHBasicKeyExchange;
  TestECDHDifferentCurves;
  TestECDHCofactorMode;
  TestECDHComputeKeyRawAPI;
  TestECDHInvalidInputs;
  
  WriteLn;
  WriteLn('====================================');
  WriteLn('Test Summary');
  WriteLn('====================================');
  WriteLn('Tests passed: ', TestsPassed);
  WriteLn('Tests failed: ', TestsFailed);
  WriteLn('Total tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

begin
  try
    RunAllTests;
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.