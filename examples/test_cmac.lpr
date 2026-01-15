program test_cmac;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.cmac,
  fafafa.ssl.openssl.aes,
  fafafa.ssl.openssl.evp;

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

function HexToBytes(const Hex: string): TBytes;
var
  I: Integer;
  Len: Integer;
begin
  Len := Length(Hex) div 2;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

procedure TestCMACLoading;
begin
  WriteLn('Testing CMAC module loading...');
  
  if not LoadCMACFunctions then
  begin
    WriteLn('  Warning: CMAC functions not fully loaded');
    WriteLn('  This may be expected for OpenSSL 3.x where CMAC is deprecated');
  end;
  
  TestResult('CMAC module loaded', IsCMACLoaded or Assigned(EVP_MAC_fetch));
  TestResult('CMAC_CTX_new available', Assigned(CMAC_CTX_new) or Assigned(EVP_MAC_fetch));
end;

procedure TestCMAC_AES128_RFC4493;
var
  Key: TBytes;
  Data: TBytes;
  MAC: TBytes;
  ExpectedMAC: string;
begin
  WriteLn('Testing CMAC-AES128 (RFC 4493 test vectors)...');
  
  // RFC 4493 Example 1: empty message
  Key := HexToBytes('2b7e151628aed2a6abf7158809cf4f3c');
  SetLength(Data, 0);
  
  try
    MAC := ComputeCMAC_AES128(Key, Data);
    ExpectedMAC := 'BB1D6929E95937287FA37D129B756746';
    TestResult('CMAC-AES128 empty message',
               (Length(MAC) = 16) and (UpperCase(BytesToHex(MAC)) = ExpectedMAC));
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC-AES128 empty message', False);
    end;
  end;
  
  // RFC 4493 Example 2: 16-byte message
  SetLength(Data, 16);
  Data[0] := $6b; Data[1] := $c1; Data[2] := $be; Data[3] := $e2;
  Data[4] := $2e; Data[5] := $40; Data[6] := $9f; Data[7] := $96;
  Data[8] := $e9; Data[9] := $3d; Data[10] := $7e; Data[11] := $11;
  Data[12] := $73; Data[13] := $93; Data[14] := $17; Data[15] := $2a;
  
  try
    MAC := ComputeCMAC_AES128(Key, Data);
    ExpectedMAC := '070A16B46B4D4144F79BDD9DD04A287C';
    TestResult('CMAC-AES128 16-byte message',
               (Length(MAC) = 16) and (UpperCase(BytesToHex(MAC)) = ExpectedMAC));
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC-AES128 16-byte message', False);
    end;
  end;
  
  // RFC 4493 Example 3: 40-byte message
  SetLength(Data, 40);
  Data[0] := $6b; Data[1] := $c1; Data[2] := $be; Data[3] := $e2;
  Data[4] := $2e; Data[5] := $40; Data[6] := $9f; Data[7] := $96;
  Data[8] := $e9; Data[9] := $3d; Data[10] := $7e; Data[11] := $11;
  Data[12] := $73; Data[13] := $93; Data[14] := $17; Data[15] := $2a;
  Data[16] := $ae; Data[17] := $2d; Data[18] := $8a; Data[19] := $57;
  Data[20] := $1e; Data[21] := $03; Data[22] := $ac; Data[23] := $9c;
  Data[24] := $9e; Data[25] := $b7; Data[26] := $6f; Data[27] := $ac;
  Data[28] := $45; Data[29] := $af; Data[30] := $8e; Data[31] := $51;
  Data[32] := $30; Data[33] := $c8; Data[34] := $1c; Data[35] := $46;
  Data[36] := $a3; Data[37] := $5c; Data[38] := $e4; Data[39] := $11;
  
  try
    MAC := ComputeCMAC_AES128(Key, Data);
    ExpectedMAC := 'DFA66747DE9AE63030CA32611497C827';
    TestResult('CMAC-AES128 40-byte message',
               (Length(MAC) = 16) and (UpperCase(BytesToHex(MAC)) = ExpectedMAC));
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC-AES128 40-byte message', False);
    end;
  end;
  
  // RFC 4493 Example 4: 64-byte message
  SetLength(Data, 64);
  Data[0] := $6b; Data[1] := $c1; Data[2] := $be; Data[3] := $e2;
  Data[4] := $2e; Data[5] := $40; Data[6] := $9f; Data[7] := $96;
  Data[8] := $e9; Data[9] := $3d; Data[10] := $7e; Data[11] := $11;
  Data[12] := $73; Data[13] := $93; Data[14] := $17; Data[15] := $2a;
  Data[16] := $ae; Data[17] := $2d; Data[18] := $8a; Data[19] := $57;
  Data[20] := $1e; Data[21] := $03; Data[22] := $ac; Data[23] := $9c;
  Data[24] := $9e; Data[25] := $b7; Data[26] := $6f; Data[27] := $ac;
  Data[28] := $45; Data[29] := $af; Data[30] := $8e; Data[31] := $51;
  Data[32] := $30; Data[33] := $c8; Data[34] := $1c; Data[35] := $46;
  Data[36] := $a3; Data[37] := $5c; Data[38] := $e4; Data[39] := $11;
  Data[40] := $e5; Data[41] := $fb; Data[42] := $c1; Data[43] := $19;
  Data[44] := $1a; Data[45] := $0a; Data[46] := $52; Data[47] := $ef;
  Data[48] := $f6; Data[49] := $9f; Data[50] := $24; Data[51] := $45;
  Data[52] := $df; Data[53] := $4f; Data[54] := $9b; Data[55] := $17;
  Data[56] := $ad; Data[57] := $2b; Data[58] := $41; Data[59] := $7b;
  Data[60] := $e6; Data[61] := $6c; Data[62] := $37; Data[63] := $10;
  
  try
    MAC := ComputeCMAC_AES128(Key, Data);
    ExpectedMAC := '51F0BEBF7E3B9D92FC49741779363CFE';
    TestResult('CMAC-AES128 64-byte message',
               (Length(MAC) = 16) and (UpperCase(BytesToHex(MAC)) = ExpectedMAC));
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC-AES128 64-byte message', False);
    end;
  end;
end;

procedure TestCMAC_AES256;
var
  Key: TBytes;
  Data: TBytes;
  MAC: TBytes;
begin
  WriteLn('Testing CMAC-AES256...');
  
  // Test with AES-256 key
  SetLength(Key, 32);
  FillChar(Key[0], 32, $00);
  Key[0] := $01; Key[1] := $23; Key[2] := $45; Key[3] := $67;
  
  SetLength(Data, 32);
  FillChar(Data[0], 32, $AA);
  
  try
    MAC := ComputeCMAC_AES256(Key, Data);
    TestResult('CMAC-AES256 compute', Length(MAC) = 16);
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC-AES256 compute', False);
    end;
  end;
end;

procedure TestCMACConsistency;
var
  Key: TBytes;
  Data: TBytes;
  MAC1, MAC2: TBytes;
begin
  WriteLn('Testing CMAC consistency...');
  
  SetLength(Key, 16);
  FillChar(Key[0], 16, $42);
  
  SetLength(Data, 100);
  FillChar(Data[0], 100, $AB);
  
  try
    MAC1 := ComputeCMAC_AES128(Key, Data);
    MAC2 := ComputeCMAC_AES128(Key, Data);
    
    TestResult('CMAC consistency check',
               (Length(MAC1) = Length(MAC2)) and
               (BytesToHex(MAC1) = BytesToHex(MAC2)));
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC consistency check', False);
    end;
  end;
end;

procedure TestCMACIncremental;
var
  ctx: PCMAC_CTX;
  Key: TBytes;
  Data1, Data2: TBytes;
  mac_len: size_t;
  MACOnePass, MACIncremental: TBytes;
  AllData: TBytes;
begin
  WriteLn('Testing CMAC incremental update...');
  
  if not Assigned(CMAC_CTX_new) then
  begin
    WriteLn('  CMAC incremental API not available, skipping');
    Exit;
  end;
  
  SetLength(Key, 16);
  FillChar(Key[0], 16, $11);
  
  SetLength(Data1, 32);
  FillChar(Data1[0], 32, $AA);
  
  SetLength(Data2, 32);
  FillChar(Data2[0], 32, $BB);
  
  try
    // Compute CMAC in one pass
    SetLength(AllData, 64);
    Move(Data1[0], AllData[0], 32);
    Move(Data2[0], AllData[32], 32);
    MACOnePass := ComputeCMAC_AES128(Key, AllData);
    
    // Compute CMAC incrementally
    ctx := CMAC_CTX_new();
    if ctx <> nil then
    begin
      try
        if CMAC_Init(ctx, @Key[0], Length(Key), EVP_aes_128_cbc(), nil) = 1 then
        begin
          if CMAC_Update(ctx, @Data1[0], Length(Data1)) = 1 then
          begin
            if CMAC_Update(ctx, @Data2[0], Length(Data2)) = 1 then
            begin
              SetLength(MACIncremental, 16);
              mac_len := 16;
              if CMAC_Final(ctx, @MACIncremental[0], @mac_len) = 1 then
              begin
                SetLength(MACIncremental, mac_len);
                TestResult('CMAC incremental matches one-pass',
                           BytesToHex(MACOnePass) = BytesToHex(MACIncremental));
              end
              else
                TestResult('CMAC incremental matches one-pass', False);
            end
            else
              TestResult('CMAC incremental matches one-pass', False);
          end
          else
            TestResult('CMAC incremental matches one-pass', False);
        end
        else
          TestResult('CMAC incremental matches one-pass', False);
      finally
        CMAC_CTX_free(ctx);
      end;
    end
    else
      TestResult('CMAC incremental matches one-pass', False);
  except
    on E: Exception do
    begin
      WriteLn('  Exception: ', E.Message);
      TestResult('CMAC incremental matches one-pass', False);
    end;
  end;
end;

procedure TestCMACErrorHandling;
var
  Key: TBytes;
  Data: TBytes;
  MAC: TBytes;
  ErrorCaught: Boolean;
begin
  WriteLn('Testing CMAC error handling...');
  
  // Test with empty key
  SetLength(Key, 0);
  SetLength(Data, 16);
  FillChar(Data[0], 16, $AA);
  
  ErrorCaught := False;
  try
    MAC := ComputeCMAC_AES128(Key, Data);
  except
    on E: Exception do
      ErrorCaught := True;
  end;
  
  TestResult('CMAC rejects empty key', ErrorCaught);
  
  // Test with wrong key size for AES-128
  SetLength(Key, 10);  // Should be 16 for AES-128
  FillChar(Key[0], 10, $42);
  
  ErrorCaught := False;
  try
    MAC := ComputeCMAC_AES128(Key, Data);
  except
    on E: Exception do
      ErrorCaught := True;
  end;
  
  TestResult('CMAC rejects wrong key size', ErrorCaught);
end;

begin
  WriteLn('========================================');
  WriteLn('OpenSSL CMAC Module Test');
  WriteLn('========================================');
  WriteLn;
  
  try
    // Load OpenSSL core
    WriteLn('Loading OpenSSL core...');
    LoadOpenSSLCore;
    TestResult('OpenSSL core loaded', GetCryptoLibHandle <> 0);
    WriteLn;
    
    // Load AES functions
    LoadAESFunctions(GetCryptoLibHandle);
    
    // Run tests
    TestCMACLoading;
    WriteLn;
    
    TestCMAC_AES128_RFC4493;
    WriteLn;
    
    TestCMAC_AES256;
    WriteLn;
    
    TestCMACConsistency;
    WriteLn;
    
    TestCMACIncremental;
    WriteLn;
    
    TestCMACErrorHandling;
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
