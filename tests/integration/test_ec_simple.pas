program test_ec_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.bn;

procedure PrintTestHeader(const TestName: string);
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test: ', TestName);
  WriteLn('========================================');
end;

procedure PrintTestResult(const TestName: string; Success: Boolean);
begin
  if Success then
    WriteLn('[PASS] ', TestName)
  else
    WriteLn('[FAIL] ', TestName);
end;

// Test 1: EC Key Generation for Different Curves
function Test_EC_KeyGeneration: Boolean;
var
  key256, key384, key521: PEC_KEY;
begin
  PrintTestHeader('EC Key Generation');
  Result := False;
  
  // Test secp256r1 (P-256)
  Write('Testing secp256r1 key generation... ');
  key256 := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if key256 = nil then
  begin
    WriteLn('FAILED to create key');
    Exit;
  end;
  
  if EC_KEY_generate_key(key256) <> 1 then
  begin
    WriteLn('FAILED to generate key');
    EC_KEY_free(key256);
    Exit;
  end;
  WriteLn('OK');
  
  // Verify key
  Write('Verifying generated key... ');
  if EC_KEY_check_key(key256) <> 1 then
  begin
    WriteLn('FAILED');
    EC_KEY_free(key256);
    Exit;
  end;
  WriteLn('OK');
  EC_KEY_free(key256);
  
  // Test secp384r1 (P-384)
  Write('Testing secp384r1 key generation... ');
  key384 := EC_KEY_new_by_curve_name(NID_secp384r1);
  if key384 = nil then
  begin
    WriteLn('FAILED to create key');
    Exit;
  end;
  
  if EC_KEY_generate_key(key384) <> 1 then
  begin
    WriteLn('FAILED to generate key');
    EC_KEY_free(key384);
    Exit;
  end;
  WriteLn('OK');
  EC_KEY_free(key384);
  
  // Test secp521r1 (P-521)
  Write('Testing secp521r1 key generation... ');
  key521 := EC_KEY_new_by_curve_name(NID_secp521r1);
  if key521 = nil then
  begin
    WriteLn('FAILED to create key');
    Exit;
  end;
  
  if EC_KEY_generate_key(key521) <> 1 then
  begin
    WriteLn('FAILED to generate key');
    EC_KEY_free(key521);
    Exit;
  end;
  WriteLn('OK');
  EC_KEY_free(key521);
  
  Result := True;
  PrintTestResult('EC Key Generation', Result);
end;

// Test 2: EC Key Copy and Duplication
function Test_EC_KeyCopyDup: Boolean;
var
  key1, key2, key3: PEC_KEY;
begin
  PrintTestHeader('EC Key Copy and Duplication');
  Result := False;
  
  // Create and generate key
  Write('Creating original key... ');
  key1 := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if key1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  
  if EC_KEY_generate_key(key1) <> 1 then
  begin
    WriteLn('FAILED to generate');
    EC_KEY_free(key1);
    Exit;
  end;
  WriteLn('OK');
  
  // Test dup
  Write('Testing EC_KEY_dup... ');
  key2 := EC_KEY_dup(key1);
  if key2 = nil then
  begin
    WriteLn('FAILED');
    EC_KEY_free(key1);
    Exit;
  end;
  
  if EC_KEY_check_key(key2) <> 1 then
  begin
    WriteLn('FAILED verification');
    EC_KEY_free(key1);
    EC_KEY_free(key2);
    Exit;
  end;
  WriteLn('OK');
  
  // Test copy
  Write('Testing EC_KEY_copy... ');
  key3 := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if key3 = nil then
  begin
    WriteLn('FAILED to create destination');
    EC_KEY_free(key1);
    EC_KEY_free(key2);
    Exit;
  end;
  
  if EC_KEY_copy(key3, key1) = nil then
  begin
    WriteLn('FAILED copy');
    EC_KEY_free(key1);
    EC_KEY_free(key2);
    EC_KEY_free(key3);
    Exit;
  end;
  
  if EC_KEY_check_key(key3) <> 1 then
  begin
    WriteLn('FAILED verification after copy');
    EC_KEY_free(key1);
    EC_KEY_free(key2);
    EC_KEY_free(key3);
    Exit;
  end;
  WriteLn('OK');
  
  EC_KEY_free(key1);
  EC_KEY_free(key2);
  EC_KEY_free(key3);
  
  Result := True;
  PrintTestResult('EC Key Copy and Duplication', Result);
end;

// Test 3: EC Group Operations
function Test_EC_GroupOperations: Boolean;
var
  group: PEC_GROUP;
  nid: Integer;
  degree: Integer;
  order, cofactor: PBIGNUM;
  ctx: PBN_CTX;
begin
  PrintTestHeader('EC Group Operations');
  Result := False;
  
  // Create group
  Write('Creating EC group (P-256)... ');
  group := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
  if group = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  // Get curve name
  Write('Getting curve NID... ');
  nid := EC_GROUP_get_curve_name(group);
  if nid <> NID_X9_62_prime256v1 then
  begin
    WriteLn('FAILED (expected ', NID_X9_62_prime256v1, ', got ', nid, ')');
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK (NID=', nid, ')');
  
  // Get degree
  Write('Getting curve degree... ');
  degree := EC_GROUP_get_degree(group);
  if degree <= 0 then
  begin
    WriteLn('FAILED');
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK (degree=', degree, ' bits)');
  
  // Get order
  Write('Getting curve order... ');
  ctx := BN_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED to create BN_CTX');
    EC_GROUP_free(group);
    Exit;
  end;
  
  order := BN_new();
  if order = nil then
  begin
    WriteLn('FAILED to create BIGNUM for order');
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  
  if EC_GROUP_get_order(group, order, ctx) <> 1 then
  begin
    WriteLn('FAILED to get order');
    BN_free(order);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK (order bits=', BN_num_bits(order), ')');
  
  // Get cofactor
  Write('Getting curve cofactor... ');
  cofactor := BN_new();
  if cofactor = nil then
  begin
    WriteLn('FAILED to create BIGNUM for cofactor');
    BN_free(order);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  
  if EC_GROUP_get_cofactor(group, cofactor, ctx) <> 1 then
  begin
    WriteLn('FAILED to get cofactor');
    BN_free(order);
    BN_free(cofactor);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK (cofactor=', BN_get_word(cofactor), ')');
  
  BN_free(order);
  BN_free(cofactor);
  BN_CTX_free(ctx);
  EC_GROUP_free(group);
  
  Result := True;
  PrintTestResult('EC Group Operations', Result);
end;

// Test 4: EC Point Operations
function Test_EC_PointOperations: Boolean;
var
  group: PEC_GROUP;
  point1, point2, point3: PEC_POINT;
  ctx: PBN_CTX;
begin
  PrintTestHeader('EC Point Operations');
  Result := False;
  
  // Create group and context
  Write('Creating EC group and context... ');
  group := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
  if group = nil then
  begin
    WriteLn('FAILED to create group');
    Exit;
  end;
  
  ctx := BN_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED to create context');
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK');
  
  // Create points
  Write('Creating EC points... ');
  point1 := EC_POINT_new(group);
  point2 := EC_POINT_new(group);
  point3 := EC_POINT_new(group);
  if (point1 = nil) or (point2 = nil) or (point3 = nil) then
  begin
    WriteLn('FAILED');
    if point1 <> nil then EC_POINT_free(point1);
    if point2 <> nil then EC_POINT_free(point2);
    if point3 <> nil then EC_POINT_free(point3);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK');
  
  // Test point copy
  Write('Testing EC_POINT_copy... ');
  if EC_POINT_copy(point2, point1) <> 1 then
  begin
    WriteLn('FAILED');
    EC_POINT_free(point1);
    EC_POINT_free(point2);
    EC_POINT_free(point3);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK');
  
  // Test point addition
  Write('Testing EC_POINT_add... ');
  if EC_POINT_add(group, point3, point1, point2, ctx) <> 1 then
  begin
    WriteLn('FAILED');
    EC_POINT_free(point1);
    EC_POINT_free(point2);
    EC_POINT_free(point3);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK');
  
  // Test point doubling
  Write('Testing EC_POINT_dbl... ');
  if EC_POINT_dbl(group, point3, point1, ctx) <> 1 then
  begin
    WriteLn('FAILED');
    EC_POINT_free(point1);
    EC_POINT_free(point2);
    EC_POINT_free(point3);
    BN_CTX_free(ctx);
    EC_GROUP_free(group);
    Exit;
  end;
  WriteLn('OK');
  
  EC_POINT_free(point1);
  EC_POINT_free(point2);
  EC_POINT_free(point3);
  BN_CTX_free(ctx);
  EC_GROUP_free(group);
  
  Result := True;
  PrintTestResult('EC Point Operations', Result);
end;

// Test 5: EC Key Component Access
function Test_EC_KeyComponentAccess: Boolean;
var
  key: PEC_KEY;
  group: PEC_GROUP;
  priv_key: PBIGNUM;
  pub_key: PEC_POINT;
begin
  PrintTestHeader('EC Key Component Access');
  Result := False;
  
  // Create and generate key
  Write('Creating and generating key... ');
  key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if key = nil then
  begin
    WriteLn('FAILED to create key');
    Exit;
  end;
  
  if EC_KEY_generate_key(key) <> 1 then
  begin
    WriteLn('FAILED to generate key');
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  // Access group
  Write('Accessing EC group... ');
  group := EC_KEY_get0_group(key);
  if group = nil then
  begin
    WriteLn('FAILED');
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  // Access private key
  Write('Accessing private key... ');
  priv_key := EC_KEY_get0_private_key(key);
  if priv_key = nil then
  begin
    WriteLn('FAILED');
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK (', BN_num_bits(priv_key), ' bits)');
  
  // Access public key
  Write('Accessing public key... ');
  pub_key := EC_KEY_get0_public_key(key);
  if pub_key = nil then
  begin
    WriteLn('FAILED');
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  EC_KEY_free(key);
  
  Result := True;
  PrintTestResult('EC Key Component Access', Result);
end;

// Test 6: Multiple Curve Types
function Test_EC_MultipleCurves: Boolean;
var
  key: PEC_KEY;
  group: PEC_GROUP;
  nid: Integer;
  
  procedure TestCurve(CurveNID: Integer; const CurveName: string);
  begin
    Write('Testing curve ', CurveName, '... ');
    key := EC_KEY_new_by_curve_name(CurveNID);
    if key = nil then
    begin
      WriteLn('FAILED to create key');
      Exit;
    end;
    
    if EC_KEY_generate_key(key) <> 1 then
    begin
      WriteLn('FAILED to generate key');
      EC_KEY_free(key);
      Exit;
    end;
    
    if EC_KEY_check_key(key) <> 1 then
    begin
      WriteLn('FAILED validation');
      EC_KEY_free(key);
      Exit;
    end;
    
    group := EC_KEY_get0_group(key);
    if group = nil then
    begin
      WriteLn('FAILED to get group');
      EC_KEY_free(key);
      Exit;
    end;
    
    nid := EC_GROUP_get_curve_name(group);
    if nid <> CurveNID then
    begin
      WriteLn('FAILED curve mismatch');
      EC_KEY_free(key);
      Exit;
    end;
    
    WriteLn('OK');
    EC_KEY_free(key);
  end;
  
begin
  PrintTestHeader('Multiple Curve Types');
  Result := False;
  
  // NIST/SECG curves
  TestCurve(NID_X9_62_prime256v1, 'P-256 (prime256v1)');
  TestCurve(NID_secp384r1, 'P-384 (secp384r1)');
  TestCurve(NID_secp521r1, 'P-521 (secp521r1)');
  TestCurve(NID_secp256k1, 'secp256k1 (Bitcoin)');
  
  // Additional SECG curves
  TestCurve(NID_secp224r1, 'P-224 (secp224r1)');
  
  Result := True;
  PrintTestResult('Multiple Curve Types', Result);
end;

// Test 7: EC Point Serialization
function Test_EC_PointSerialization: Boolean;
var
  key: PEC_KEY;
  group: PEC_GROUP;
  pub_key, restored_point: PEC_POINT;
  buf: array[0..255] of Byte;
  buf_len: NativeUInt;
  ctx: PBN_CTX;
begin
  PrintTestHeader('EC Point Serialization');
  Result := False;
  
  // Create and generate key
  Write('Creating and generating key... ');
  key := EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if key = nil then
  begin
    WriteLn('FAILED to create key');
    Exit;
  end;
  
  if EC_KEY_generate_key(key) <> 1 then
  begin
    WriteLn('FAILED to generate key');
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  group := EC_KEY_get0_group(key);
  pub_key := EC_KEY_get0_public_key(key);
  
  ctx := BN_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED to create context');
    EC_KEY_free(key);
    Exit;
  end;
  
  // Serialize point to octet string
  Write('Serializing public key point... ');
  buf_len := EC_POINT_point2oct(group, pub_key, POINT_CONVERSION_UNCOMPRESSED, @buf[0], SizeOf(buf), ctx);
  if buf_len = 0 then
  begin
    WriteLn('FAILED');
    BN_CTX_free(ctx);
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK (', buf_len, ' bytes)');
  
  // Deserialize point from octet string
  Write('Deserializing public key point... ');
  restored_point := EC_POINT_new(group);
  if restored_point = nil then
  begin
    WriteLn('FAILED to create point');
    BN_CTX_free(ctx);
    EC_KEY_free(key);
    Exit;
  end;
  
  if EC_POINT_oct2point(group, restored_point, @buf[0], buf_len, ctx) <> 1 then
  begin
    WriteLn('FAILED');
    EC_POINT_free(restored_point);
    BN_CTX_free(ctx);
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  // Compare points
  Write('Comparing original and restored points... ');
  if EC_POINT_cmp(group, pub_key, restored_point, ctx) <> 0 then
  begin
    WriteLn('FAILED (points differ)');
    EC_POINT_free(restored_point);
    BN_CTX_free(ctx);
    EC_KEY_free(key);
    Exit;
  end;
  WriteLn('OK');
  
  EC_POINT_free(restored_point);
  BN_CTX_free(ctx);
  EC_KEY_free(key);
  
  Result := True;
  PrintTestResult('EC Point Serialization', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('EC Module Integration Test');
  WriteLn('==========================');
  WriteLn('');
  
  // Initialize OpenSSL Core
  Write('Initializing OpenSSL Core... ');
  try
    LoadOpenSSLCore;
    WriteLn('OK (', GetOpenSSLVersionString, ')');
  except
    on E: Exception do
    begin
      WriteLn('FAILED');
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  // Load EC functions
  Write('Loading EC functions... ');
  if not LoadECFunctions(GetCryptoLibHandle) then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load EC functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  // Load BN functions
  Write('Loading BN functions... ');
  if not LoadOpenSSLBN then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load BN functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 7;
  
  try
    // Run all tests
    if Test_EC_KeyGeneration then Inc(PassedTests);
    if Test_EC_KeyCopyDup then Inc(PassedTests);
    if Test_EC_GroupOperations then Inc(PassedTests);
    if Test_EC_PointOperations then Inc(PassedTests);
    if Test_EC_KeyComponentAccess then Inc(PassedTests);
    if Test_EC_MultipleCurves then Inc(PassedTests);
    if Test_EC_PointSerialization then Inc(PassedTests);
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
    end;
  end;
  
  // Summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test Summary');
  WriteLn('========================================');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', TotalTests - PassedTests);
  WriteLn('Success rate: ', (PassedTests * 100) div TotalTests, '%');
  
  if PassedTests = TotalTests then
  begin
    WriteLn('');
    WriteLn('ALL TESTS PASSED!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('');
    WriteLn('SOME TESTS FAILED!');
    ExitCode := 1;
  end;
  
  UnloadOpenSSLCore;
end.
