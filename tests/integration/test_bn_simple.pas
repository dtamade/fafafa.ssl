program test_bn_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.types,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.crypto,
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

procedure TestBNCreation;
var
  A, B: PBIGNUM;
begin
  WriteLn;
  WriteLn('=== BN Creation and Memory Tests ===');
  WriteLn;
  
  // Test BN_new
  A := BN_new();
  LogTest('Create BIGNUM', A <> nil);
  
  if A <> nil then
  begin
    // Test BN_set_word
    if BN_set_word(A, 12345) = 1 then
    begin
      LogTest('Set word value', True);
      LogTest('Get word value', BN_get_word(A) = 12345,
              Format('Value: %d', [BN_get_word(A)]));
    end
    else
      LogTest('Set word value', False);
    
    BN_free(A);
  end;
  
  // Test BN_dup
  A := BN_new();
  if A <> nil then
  begin
    BN_set_word(A, 999);
    B := BN_dup(A);
    LogTest('Duplicate BIGNUM', B <> nil);
    if B <> nil then
    begin
      LogTest('Duplicated value matches', BN_get_word(B) = 999);
      BN_free(B);
    end;
    BN_free(A);
  end;
end;

procedure TestBNArithmetic;
var
  A, B, R: PBIGNUM;
  Ctx: PBN_CTX;
  Result: Integer;
begin
  WriteLn;
  WriteLn('=== BN Arithmetic Tests ===');
  WriteLn;
  
  A := BN_new();
  B := BN_new();
  R := BN_new();
  Ctx := BN_CTX_new();
  
  if (A = nil) or (B = nil) or (R = nil) or (Ctx = nil) then
  begin
    LogTest('Create BN structures for arithmetic', False);
    Exit;
  end;
  
  try
    // Test addition: 100 + 50 = 150
    BN_set_word(A, 100);
    BN_set_word(B, 50);
    Result := BN_add(R, A, B);
    LogTest('Addition (100 + 50)', (Result = 1) and (BN_get_word(R) = 150),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Test subtraction: 100 - 30 = 70
    BN_set_word(A, 100);
    BN_set_word(B, 30);
    Result := BN_sub(R, A, B);
    LogTest('Subtraction (100 - 30)', (Result = 1) and (BN_get_word(R) = 70),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Test multiplication: 25 * 4 = 100
    BN_set_word(A, 25);
    BN_set_word(B, 4);
    Result := BN_mul(R, A, B, Ctx);
    LogTest('Multiplication (25 * 4)', (Result = 1) and (BN_get_word(R) = 100),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Test division: 100 / 5 = 20
    BN_set_word(A, 100);
    BN_set_word(B, 5);
    Result := BN_div(R, nil, A, B, Ctx);
    LogTest('Division (100 / 5)', (Result = 1) and (BN_get_word(R) = 20),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Test modulo: 100 mod 7 = 2 (using div with remainder)
    BN_set_word(A, 100);
    BN_set_word(B, 7);
    Result := BN_div(nil, R, A, B, Ctx);
    LogTest('Modulo (100 mod 7)', (Result = 1) and (BN_get_word(R) = 2),
            Format('Result: %d', [BN_get_word(R)]));
    
  finally
    BN_free(A);
    BN_free(B);
    BN_free(R);
    BN_CTX_free(Ctx);
  end;
end;

procedure TestBNComparison;
var
  A, B: PBIGNUM;
begin
  WriteLn;
  WriteLn('=== BN Comparison Tests ===');
  WriteLn;
  
  A := BN_new();
  B := BN_new();
  
  if (A = nil) or (B = nil) then
  begin
    LogTest('Create BN structures for comparison', False);
    Exit;
  end;
  
  try
    // Test equality
    BN_set_word(A, 42);
    BN_set_word(B, 42);
    LogTest('Equal numbers (42 == 42)', BN_cmp(A, B) = 0);
    
    // Test less than
    BN_set_word(A, 10);
    BN_set_word(B, 20);
    LogTest('Less than (10 < 20)', BN_cmp(A, B) < 0);
    
    // Test greater than
    BN_set_word(A, 30);
    BN_set_word(B, 15);
    LogTest('Greater than (30 > 15)', BN_cmp(A, B) > 0);
    
    // Test is_zero
    BN_set_word(A, 0);
    LogTest('Is zero', BN_is_zero(A) = 1);
    
    // Test is_one
    BN_set_word(A, 1);
    LogTest('Is one', BN_is_one(A) = 1);
    
    // Test is_odd
    BN_set_word(A, 7);
    LogTest('Is odd (7)', BN_is_odd(A) = 1);
    
    BN_set_word(A, 8);
    LogTest('Is even (8)', BN_is_odd(A) = 0);
    
  finally
    BN_free(A);
    BN_free(B);
  end;
end;

procedure TestBNModularArithmetic;
var
  A, B, M, R: PBIGNUM;
  Ctx: PBN_CTX;
  Result: Integer;
begin
  WriteLn;
  WriteLn('=== BN Modular Arithmetic Tests ===');
  WriteLn;
  
  A := BN_new();
  B := BN_new();
  M := BN_new();
  R := BN_new();
  Ctx := BN_CTX_new();
  
  if (A = nil) or (B = nil) or (M = nil) or (R = nil) or (Ctx = nil) then
  begin
    LogTest('Create BN structures for modular arithmetic', False);
    Exit;
  end;
  
  try
    // Modular addition: (10 + 7) mod 11 = 6
    BN_set_word(A, 10);
    BN_set_word(B, 7);
    BN_set_word(M, 11);
    Result := BN_mod_add(R, A, B, M, Ctx);
    LogTest('Modular addition ((10 + 7) mod 11)', 
            (Result = 1) and (BN_get_word(R) = 6),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Modular subtraction: (15 - 8) mod 11 = 7
    BN_set_word(A, 15);
    BN_set_word(B, 8);
    BN_set_word(M, 11);
    Result := BN_mod_sub(R, A, B, M, Ctx);
    LogTest('Modular subtraction ((15 - 8) mod 11)', 
            (Result = 1) and (BN_get_word(R) = 7),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Modular multiplication: (7 * 8) mod 11 = 1
    BN_set_word(A, 7);
    BN_set_word(B, 8);
    BN_set_word(M, 11);
    Result := BN_mod_mul(R, A, B, M, Ctx);
    LogTest('Modular multiplication ((7 * 8) mod 11)', 
            (Result = 1) and (BN_get_word(R) = 1),
            Format('Result: %d', [BN_get_word(R)]));
    
    // Modular exponentiation: 2^10 mod 1000 = 24
    BN_set_word(A, 2);
    BN_set_word(B, 10);
    BN_set_word(M, 1000);
    Result := BN_mod_exp(R, A, B, M, Ctx);
    LogTest('Modular exponentiation (2^10 mod 1000)', 
            (Result = 1) and (BN_get_word(R) = 24),
            Format('Result: %d', [BN_get_word(R)]));
    
  finally
    BN_free(A);
    BN_free(B);
    BN_free(M);
    BN_free(R);
    BN_CTX_free(Ctx);
  end;
end;

procedure TestBNBitOperations;
var
  A, R: PBIGNUM;
  NumBits: Integer;
begin
  WriteLn;
  WriteLn('=== BN Bit Operations Tests ===');
  WriteLn;
  
  A := BN_new();
  R := BN_new();
  
  if (A = nil) or (R = nil) then
  begin
    LogTest('Create BN structures for bit operations', False);
    Exit;
  end;
  
  try
    // Test num_bits
    BN_set_word(A, 255);  // 0xFF = 8 bits
    NumBits := BN_num_bits(A);
    LogTest('Number of bits (255)', NumBits = 8,
            Format('Bits: %d', [NumBits]));
    
    // Test left shift: 1 << 10 = 1024
    BN_set_word(A, 1);
    if BN_lshift(R, A, 10) = 1 then
      LogTest('Left shift (1 << 10)', BN_get_word(R) = 1024,
              Format('Result: %d', [BN_get_word(R)]))
    else
      LogTest('Left shift (1 << 10)', False);
    
    // Test right shift: 1024 >> 10 = 1
    BN_set_word(A, 1024);
    if BN_rshift(R, A, 10) = 1 then
      LogTest('Right shift (1024 >> 10)', BN_get_word(R) = 1,
              Format('Result: %d', [BN_get_word(R)]))
    else
      LogTest('Right shift (1024 >> 10)', False);
    
    // Test set_bit
    BN_set_word(A, 0);  // Clear to zero
    if BN_set_bit(A, 5) = 1 then  // Set bit 5 = 32
      LogTest('Set bit 5', BN_get_word(A) = 32,
              Format('Result: %d', [BN_get_word(A)]))
    else
      LogTest('Set bit 5', False);
    
    // Test is_bit_set
    BN_set_word(A, 32);  // bit 5 is set
    LogTest('Test bit 5 is set', BN_is_bit_set(A, 5) = 1);
    LogTest('Test bit 4 is not set', BN_is_bit_set(A, 4) = 0);
    
  finally
    BN_free(A);
    BN_free(R);
  end;
end;

procedure TestBNConversion;
var
  A: PBIGNUM;
  HexStr, DecStr: PAnsiChar;
  Bin: array[0..31] of Byte;
  BinLen: Integer;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== BN Conversion Tests ===');
  WriteLn;
  
  A := BN_new();
  
  if A = nil then
  begin
    LogTest('Create BN for conversion', False);
    Exit;
  end;
  
  try
    // Test dec2bn
    if BN_dec2bn(@A, '12345') > 0 then
    begin
      LogTest('Decimal string to BN', True);
      LogTest('Verify decimal value', BN_get_word(A) = 12345);
    end
    else
      LogTest('Decimal string to BN', False);
    
    // Test bn2dec
    BN_set_word(A, 67890);
    DecStr := BN_bn2dec(A);
    if DecStr <> nil then
    begin
      LogTest('BN to decimal string', string(DecStr) = '67890',
              Format('String: %s', [DecStr]));
      // Note: String memory managed by OpenSSL, freed on cleanup
    end
    else
      LogTest('BN to decimal string', False);
    
    // Test hex2bn
    if BN_hex2bn(@A, 'FF') > 0 then
    begin
      LogTest('Hex string to BN', True);
      LogTest('Verify hex value (0xFF)', BN_get_word(A) = 255);
    end
    else
      LogTest('Hex string to BN', False);
    
    // Test bn2hex
    BN_set_word(A, 4095);  // 0xFFF
    HexStr := BN_bn2hex(A);
    if HexStr <> nil then
    begin
      // OpenSSL 3.x may add leading zero, accept both 'FFF' and '0FFF'
      LogTest('BN to hex string', (string(HexStr) = 'FFF') or (string(HexStr) = '0FFF'),
              Format('String: %s', [HexStr]));
      // Note: String memory managed by OpenSSL, freed on cleanup
    end
    else
      LogTest('BN to hex string', False);
    
    // Test bn2bin and bin2bn
    BN_set_word(A, 256);
    FillChar(Bin, SizeOf(Bin), 0);
    BinLen := BN_bn2bin(A, @Bin[0]);
    LogTest('BN to binary', BinLen > 0,
            Format('Binary length: %d bytes', [BinLen]));
    
    if BinLen > 0 then
    begin
      BN_set_word(A, 0);  // Clear to zero
      if BN_bin2bn(@Bin[0], BinLen, A) <> nil then
        LogTest('Binary to BN', BN_get_word(A) = 256)
      else
        LogTest('Binary to BN', False);
    end;
    
  finally
    BN_free(A);
  end;
end;

begin
  WriteLn('BN (Big Number) Integration Tests');
  WriteLn('==================================');
  WriteLn;
  
  // Load core OpenSSL library
  LoadOpenSSLCore;
  
  // Load BN module
  if not LoadOpenSSLBN then
  begin
    WriteLn('ERROR: Failed to load BN module');
    Halt(1);
  end;
  
  try
    // Run test suites
    TestBNCreation;
    TestBNArithmetic;
    TestBNComparison;
    TestBNModularArithmetic;
    TestBNBitOperations;
    TestBNConversion;
    
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
