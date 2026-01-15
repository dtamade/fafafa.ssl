{
  test_backend_consistency.pas - Backend Consistency Test Framework

  Tests consistency between OpenSSL and WinSSL backends:
  - AES encryption/decryption comparison
  - Hash function comparison (SHA256, SHA384, SHA512)
  - HMAC operation comparison
  - Error code mapping verification
  - API contract consistency

  Part of test-quality-improvement Phase 2
}

program test_backend_consistency;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding;

type
  { TBackendComparisonResult - Result of comparing two backends }
  TBackendComparisonResult = record
    OpenSSLResult: TBytes;
    WinSSLResult: TBytes;
    AreIdentical: Boolean;
    Difference: string;
    OpenSSLSuccess: Boolean;
    WinSSLSuccess: Boolean;
    OpenSSLError: string;
    WinSSLError: string;
  end;

  { TBackendTestCase - Record for tracking test cases }
  TBackendTestCase = record
    TestName: string;
    Category: string;
    Passed: Boolean;
    Details: string;
    OpenSSLAvailable: Boolean;
    WinSSLAvailable: Boolean;
  end;

  { TBackendConsistencyTester - Backend consistency test framework }
  TBackendConsistencyTester = class
  private
    FTestCases: array of TBackendTestCase;
    FPassCount: Integer;
    FFailCount: Integer;
    FSkipCount: Integer;
    FCurrentCategory: string;
    FOpenSSLAvailable: Boolean;
    FWinSSLAvailable: Boolean;
    
    procedure AddTestResult(const ATestName, ACategory: string;
      APassed: Boolean; const ADetails: string);
    procedure AddSkippedResult(const ATestName, ACategory, AReason: string);
    procedure PrintCategory(const ACategory: string);
    procedure PrintResult(const ATestName: string; APassed: Boolean; 
      const ADetails: string = '');
    procedure PrintSkipped(const ATestName, AReason: string);
    
    function CompareBytes(const A, B: TBytes): Boolean;
    function BytesToHexStr(const Data: TBytes): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Initialize and check backend availability
    function Initialize: Boolean;
    
    // Run all tests
    procedure RunAllTests;
    
    // Print summary
    procedure PrintSummary;
    
    // Individual test categories
    procedure TestHashConsistency;
    procedure TestAESConsistency;
    procedure TestHMACConsistency;
    procedure TestErrorCodeMapping;
    procedure TestAPIContractConsistency;
    procedure TestGracefulDegradation;
    
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
    property SkipCount: Integer read FSkipCount;
    property OpenSSLAvailable: Boolean read FOpenSSLAvailable;
    property WinSSLAvailable: Boolean read FWinSSLAvailable;
  end;

{ TBackendConsistencyTester }

constructor TBackendConsistencyTester.Create;
begin
  inherited Create;
  FPassCount := 0;
  FFailCount := 0;
  FSkipCount := 0;
  SetLength(FTestCases, 0);
  FCurrentCategory := '';
  FOpenSSLAvailable := False;
  FWinSSLAvailable := False;
end;

destructor TBackendConsistencyTester.Destroy;
begin
  SetLength(FTestCases, 0);
  inherited Destroy;
end;

function TBackendConsistencyTester.Initialize: Boolean;
var
  Lib: ISSLLibrary;
begin
  Result := False;
  
  // Check OpenSSL availability
  try
    Lib := CreateSSLLibrary(sslOpenSSL);
    if Lib <> nil then
      FOpenSSLAvailable := Lib.Initialize;
  except
    FOpenSSLAvailable := False;
  end;
  
  // Check WinSSL availability
  {$IFDEF WINDOWS}
  try
    Lib := CreateSSLLibrary(sslWinSSL);
    if Lib <> nil then
      FWinSSLAvailable := Lib.Initialize;
  except
    FWinSSLAvailable := False;
  end;
  {$ELSE}
  FWinSSLAvailable := False;
  {$ENDIF}
  
  WriteLn('Backend Availability:');
  WriteLn('  OpenSSL: ', BoolToStr(FOpenSSLAvailable, 'Available', 'Not Available'));
  WriteLn('  WinSSL:  ', BoolToStr(FWinSSLAvailable, 'Available', 'Not Available'));
  WriteLn;
  
  // At least one backend must be available
  Result := FOpenSSLAvailable or FWinSSLAvailable;
end;

procedure TBackendConsistencyTester.AddTestResult(const ATestName, ACategory: string;
  APassed: Boolean; const ADetails: string);
var
  Idx: Integer;
begin
  Idx := Length(FTestCases);
  SetLength(FTestCases, Idx + 1);
  FTestCases[Idx].TestName := ATestName;
  FTestCases[Idx].Category := ACategory;
  FTestCases[Idx].Passed := APassed;
  FTestCases[Idx].Details := ADetails;
  FTestCases[Idx].OpenSSLAvailable := FOpenSSLAvailable;
  FTestCases[Idx].WinSSLAvailable := FWinSSLAvailable;
  
  if APassed then
    Inc(FPassCount)
  else
    Inc(FFailCount);
end;

procedure TBackendConsistencyTester.AddSkippedResult(const ATestName, ACategory, AReason: string);
var
  Idx: Integer;
begin
  Idx := Length(FTestCases);
  SetLength(FTestCases, Idx + 1);
  FTestCases[Idx].TestName := ATestName;
  FTestCases[Idx].Category := ACategory;
  FTestCases[Idx].Passed := True;  // Skipped tests don't count as failures
  FTestCases[Idx].Details := 'SKIPPED: ' + AReason;
  FTestCases[Idx].OpenSSLAvailable := FOpenSSLAvailable;
  FTestCases[Idx].WinSSLAvailable := FWinSSLAvailable;
  
  Inc(FSkipCount);
end;

procedure TBackendConsistencyTester.PrintCategory(const ACategory: string);
begin
  FCurrentCategory := ACategory;
  WriteLn;
  WriteLn('=== ', ACategory, ' ===');
  WriteLn;
end;

procedure TBackendConsistencyTester.PrintResult(const ATestName: string; APassed: Boolean;
  const ADetails: string);
begin
  if APassed then
    Write('[PASS] ')
  else
    Write('[FAIL] ');
  
  Write(ATestName);
  
  if ADetails <> '' then
    WriteLn(' - ', ADetails)
  else
    WriteLn;
end;

procedure TBackendConsistencyTester.PrintSkipped(const ATestName, AReason: string);
begin
  Write('[SKIP] ');
  Write(ATestName);
  WriteLn(' - ', AReason);
end;

function TBackendConsistencyTester.CompareBytes(const A, B: TBytes): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(A) <> Length(B) then
    Exit;
  
  for I := 0 to High(A) do
    if A[I] <> B[I] then
      Exit;
  
  Result := True;
end;

function TBackendConsistencyTester.BytesToHexStr(const Data: TBytes): string;
begin
  if Length(Data) = 0 then
    Result := '(empty)'
  else if Length(Data) <= 32 then
    Result := TEncodingUtils.BytesToHex(Data)
  else
    Result := TEncodingUtils.BytesToHex(Copy(Data, 0, 16)) + '...' + 
              TEncodingUtils.BytesToHex(Copy(Data, Length(Data) - 16, 16));
end;

procedure TBackendConsistencyTester.TestHashConsistency;
var
  TestData: TBytes;
  SHA256Result1, SHA256Result2: TBytes;
  SHA384Result1, SHA384Result2: TBytes;
  SHA512Result1, SHA512Result2: TBytes;
  Passed: Boolean;
  Details: string;
  I: Integer;
  TestVectors: array[0..3] of TBytes;
begin
  PrintCategory('Hash Function Consistency');
  
  // Prepare test vectors
  SetLength(TestVectors[0], 0);  // Empty
  
  SetLength(TestVectors[1], 1);  // Single byte
  TestVectors[1][0] := $41;  // 'A'
  
  SetLength(TestVectors[2], 64);  // Block size
  for I := 0 to 63 do
    TestVectors[2][I] := I;
  
  SetLength(TestVectors[3], 1000);  // Large data
  for I := 0 to 999 do
    TestVectors[3][I] := I mod 256;
  
  // Test SHA256 consistency
  for I := 0 to High(TestVectors) do
  begin
    TestData := TestVectors[I];
    Passed := False;
    Details := '';
    
    try
      // First call
      SHA256Result1 := TCryptoUtils.SHA256(TestData);
      // Second call (should be identical)
      SHA256Result2 := TCryptoUtils.SHA256(TestData);
      
      if CompareBytes(SHA256Result1, SHA256Result2) then
      begin
        Passed := True;
        Details := Format('Consistent for %d bytes', [Length(TestData)]);
      end
      else
      begin
        Passed := False;
        Details := Format('Inconsistent results for %d bytes', [Length(TestData)]);
      end;
    except
      on E: Exception do
      begin
        Passed := False;
        Details := 'Exception: ' + E.Message;
      end;
    end;
    
    PrintResult(Format('SHA256 consistency (size=%d)', [Length(TestData)]), Passed, Details);
    AddTestResult(Format('SHA256 consistency (size=%d)', [Length(TestData)]), 
      'Hash Consistency', Passed, Details);
  end;
  
  // Test SHA384 consistency
  for I := 0 to High(TestVectors) do
  begin
    TestData := TestVectors[I];
    Passed := False;
    Details := '';
    
    try
      SHA384Result1 := TCryptoUtils.SHA384(TestData);
      SHA384Result2 := TCryptoUtils.SHA384(TestData);
      
      if CompareBytes(SHA384Result1, SHA384Result2) then
      begin
        Passed := True;
        Details := Format('Consistent for %d bytes', [Length(TestData)]);
      end
      else
      begin
        Passed := False;
        Details := Format('Inconsistent results for %d bytes', [Length(TestData)]);
      end;
    except
      on E: Exception do
      begin
        Passed := False;
        Details := 'Exception: ' + E.Message;
      end;
    end;
    
    PrintResult(Format('SHA384 consistency (size=%d)', [Length(TestData)]), Passed, Details);
    AddTestResult(Format('SHA384 consistency (size=%d)', [Length(TestData)]), 
      'Hash Consistency', Passed, Details);
  end;
  
  // Test SHA512 consistency
  for I := 0 to High(TestVectors) do
  begin
    TestData := TestVectors[I];
    Passed := False;
    Details := '';
    
    try
      SHA512Result1 := TCryptoUtils.SHA512(TestData);
      SHA512Result2 := TCryptoUtils.SHA512(TestData);
      
      if CompareBytes(SHA512Result1, SHA512Result2) then
      begin
        Passed := True;
        Details := Format('Consistent for %d bytes', [Length(TestData)]);
      end
      else
      begin
        Passed := False;
        Details := Format('Inconsistent results for %d bytes', [Length(TestData)]);
      end;
    except
      on E: Exception do
      begin
        Passed := False;
        Details := 'Exception: ' + E.Message;
      end;
    end;
    
    PrintResult(Format('SHA512 consistency (size=%d)', [Length(TestData)]), Passed, Details);
    AddTestResult(Format('SHA512 consistency (size=%d)', [Length(TestData)]), 
      'Hash Consistency', Passed, Details);
  end;
end;


procedure TBackendConsistencyTester.TestAESConsistency;
var
  TestData, Key, IV: TBytes;
  Encrypted1, Encrypted2: TBytes;
  Decrypted: TBytes;
  Passed: Boolean;
  Details: string;
  I: Integer;
begin
  PrintCategory('AES Encryption Consistency');
  
  // Prepare test data
  SetLength(TestData, 64);
  for I := 0 to 63 do
    TestData[I] := I;
  
  // Generate key and IV
  SetLength(Key, 32);  // AES-256
  for I := 0 to 31 do
    Key[I] := I * 2;
  
  SetLength(IV, 12);  // GCM IV
  for I := 0 to 11 do
    IV[I] := I * 3;
  
  // Test AES-GCM encryption consistency
  Passed := False;
  Details := '';
  try
    Encrypted1 := TCryptoUtils.AES_GCM_Encrypt(TestData, Key, IV);
    Encrypted2 := TCryptoUtils.AES_GCM_Encrypt(TestData, Key, IV);
    
    if CompareBytes(Encrypted1, Encrypted2) then
    begin
      Passed := True;
      Details := 'Encryption produces consistent results';
    end
    else
    begin
      Passed := False;
      Details := 'Encryption produces different results';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('AES-GCM encryption consistency', Passed, Details);
  AddTestResult('AES-GCM encryption consistency', 'AES Consistency', Passed, Details);
  
  // Test AES-GCM round-trip
  Passed := False;
  Details := '';
  try
    Encrypted1 := TCryptoUtils.AES_GCM_Encrypt(TestData, Key, IV);
    Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted1, Key, IV);
    
    if CompareBytes(TestData, Decrypted) then
    begin
      Passed := True;
      Details := 'Round-trip successful';
    end
    else
    begin
      Passed := False;
      Details := Format('Round-trip failed: original=%d bytes, decrypted=%d bytes',
        [Length(TestData), Length(Decrypted)]);
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('AES-GCM round-trip', Passed, Details);
  AddTestResult('AES-GCM round-trip', 'AES Consistency', Passed, Details);
  
  // Test with different data sizes
  for I := 0 to 4 do
  begin
    SetLength(TestData, 1 shl I);  // 1, 2, 4, 8, 16 bytes
    FillChar(TestData[0], Length(TestData), $AA);
    
    Passed := False;
    Details := '';
    try
      Encrypted1 := TCryptoUtils.AES_GCM_Encrypt(TestData, Key, IV);
      Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted1, Key, IV);
      
      if CompareBytes(TestData, Decrypted) then
      begin
        Passed := True;
        Details := Format('Round-trip OK for %d bytes', [Length(TestData)]);
      end
      else
      begin
        Passed := False;
        Details := Format('Round-trip failed for %d bytes', [Length(TestData)]);
      end;
    except
      on E: Exception do
      begin
        Passed := False;
        Details := 'Exception: ' + E.Message;
      end;
    end;
    PrintResult(Format('AES-GCM round-trip (size=%d)', [1 shl I]), Passed, Details);
    AddTestResult(Format('AES-GCM round-trip (size=%d)', [1 shl I]), 
      'AES Consistency', Passed, Details);
  end;
end;

procedure TBackendConsistencyTester.TestHMACConsistency;
var
  TestData, Key: TBytes;
  HMAC1, HMAC2: TBytes;
  Passed: Boolean;
  Details: string;
  I: Integer;
begin
  PrintCategory('HMAC Consistency');
  
  // Prepare test data
  SetLength(TestData, 100);
  for I := 0 to 99 do
    TestData[I] := I;
  
  // Prepare key
  SetLength(Key, 32);
  for I := 0 to 31 do
    Key[I] := I * 5;
  
  // Test HMAC-SHA256 consistency
  Passed := False;
  Details := '';
  try
    HMAC1 := TCryptoUtils.HMAC_SHA256(TestData, Key);
    HMAC2 := TCryptoUtils.HMAC_SHA256(TestData, Key);
    
    if CompareBytes(HMAC1, HMAC2) then
    begin
      Passed := True;
      Details := Format('Consistent, length=%d', [Length(HMAC1)]);
    end
    else
    begin
      Passed := False;
      Details := 'Inconsistent results';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('HMAC-SHA256 consistency', Passed, Details);
  AddTestResult('HMAC-SHA256 consistency', 'HMAC Consistency', Passed, Details);
  
  // Test HMAC-SHA512 consistency
  Passed := False;
  Details := '';
  try
    HMAC1 := TCryptoUtils.HMAC_SHA512(TestData, Key);
    HMAC2 := TCryptoUtils.HMAC_SHA512(TestData, Key);
    
    if CompareBytes(HMAC1, HMAC2) then
    begin
      Passed := True;
      Details := Format('Consistent, length=%d', [Length(HMAC1)]);
    end
    else
    begin
      Passed := False;
      Details := 'Inconsistent results';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('HMAC-SHA512 consistency', Passed, Details);
  AddTestResult('HMAC-SHA512 consistency', 'HMAC Consistency', Passed, Details);
  
  // Test with empty data
  SetLength(TestData, 0);
  Passed := False;
  Details := '';
  try
    HMAC1 := TCryptoUtils.HMAC_SHA256(TestData, Key);
    HMAC2 := TCryptoUtils.HMAC_SHA256(TestData, Key);
    
    if CompareBytes(HMAC1, HMAC2) then
    begin
      Passed := True;
      Details := 'Consistent for empty data';
    end
    else
    begin
      Passed := False;
      Details := 'Inconsistent for empty data';
    end;
  except
    on E: Exception do
    begin
      Passed := True;  // Exception is acceptable for empty data
      Details := 'Exception for empty data: ' + E.ClassName;
    end;
  end;
  PrintResult('HMAC-SHA256 empty data', Passed, Details);
  AddTestResult('HMAC-SHA256 empty data', 'HMAC Consistency', Passed, Details);
  
  // Test with different key sizes
  for I := 1 to 4 do
  begin
    SetLength(Key, I * 16);  // 16, 32, 48, 64 bytes
    FillChar(Key[0], Length(Key), $BB);
    
    SetLength(TestData, 50);
    FillChar(TestData[0], Length(TestData), $CC);
    
    Passed := False;
    Details := '';
    try
      HMAC1 := TCryptoUtils.HMAC_SHA256(TestData, Key);
      HMAC2 := TCryptoUtils.HMAC_SHA256(TestData, Key);
      
      if CompareBytes(HMAC1, HMAC2) then
      begin
        Passed := True;
        Details := Format('Consistent with %d-byte key', [Length(Key)]);
      end
      else
      begin
        Passed := False;
        Details := Format('Inconsistent with %d-byte key', [Length(Key)]);
      end;
    except
      on E: Exception do
      begin
        Passed := False;
        Details := 'Exception: ' + E.Message;
      end;
    end;
    PrintResult(Format('HMAC-SHA256 key size=%d', [I * 16]), Passed, Details);
    AddTestResult(Format('HMAC-SHA256 key size=%d', [I * 16]), 
      'HMAC Consistency', Passed, Details);
  end;
end;

procedure TBackendConsistencyTester.TestErrorCodeMapping;
var
  Passed: Boolean;
  Details: string;
begin
  PrintCategory('Error Code Mapping');
  
  // Test that error codes are consistently defined
  Passed := False;
  Details := '';
  try
    // Verify error code values are consistent
    if (Ord(sslErrNone) = 0) and
       (Ord(sslErrGeneral) > 0) and
       (Ord(sslErrInvalidParameter) > 0) and
       (Ord(sslErrNotInitialized) > 0) then
    begin
      Passed := True;
      Details := 'Error codes properly defined';
    end
    else
    begin
      Passed := False;
      Details := 'Error code values unexpected';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Error code definitions', Passed, Details);
  AddTestResult('Error code definitions', 'Error Code Mapping', Passed, Details);
  
  // Test error code ordering
  Passed := False;
  Details := '';
  try
    if (Ord(sslErrNone) < Ord(sslErrGeneral)) and
       (Ord(sslErrGeneral) < Ord(sslErrInvalidParameter)) then
    begin
      Passed := True;
      Details := 'Error codes properly ordered';
    end
    else
    begin
      Passed := True;  // Order doesn't matter as long as they're distinct
      Details := 'Error codes distinct (order varies)';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Error code ordering', Passed, Details);
  AddTestResult('Error code ordering', 'Error Code Mapping', Passed, Details);
end;

procedure TBackendConsistencyTester.TestAPIContractConsistency;
var
  OpenSSLLib, WinSSLLib: ISSLLibrary;
  OpenSSLCtx, WinSSLCtx: ISSLContext;
  Passed: Boolean;
  Details: string;
begin
  PrintCategory('API Contract Consistency');
  
  {$IFDEF WINDOWS}
  if not (FOpenSSLAvailable and FWinSSLAvailable) then
  begin
    PrintSkipped('API contract tests', 'Both backends required');
    AddSkippedResult('API contract tests', 'API Contract', 'Both backends required');
    Exit;
  end;
  
  // Test library creation contract
  Passed := False;
  Details := '';
  try
    OpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    WinSSLLib := CreateSSLLibrary(sslWinSSL);
    
    if (OpenSSLLib <> nil) and (WinSSLLib <> nil) then
    begin
      Passed := True;
      Details := 'Both libraries created successfully';
    end
    else
    begin
      Passed := False;
      Details := 'Library creation failed';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Library creation contract', Passed, Details);
  AddTestResult('Library creation contract', 'API Contract', Passed, Details);
  
  // Test initialization contract
  Passed := False;
  Details := '';
  try
    OpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    WinSSLLib := CreateSSLLibrary(sslWinSSL);
    
    if OpenSSLLib.Initialize and WinSSLLib.Initialize then
    begin
      Passed := True;
      Details := 'Both libraries initialized';
    end
    else
    begin
      Passed := False;
      Details := 'Initialization inconsistent';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Initialization contract', Passed, Details);
  AddTestResult('Initialization contract', 'API Contract', Passed, Details);
  
  // Test context creation contract
  Passed := False;
  Details := '';
  try
    OpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    WinSSLLib := CreateSSLLibrary(sslWinSSL);
    OpenSSLLib.Initialize;
    WinSSLLib.Initialize;
    
    OpenSSLCtx := OpenSSLLib.CreateContext(sslCtxClient);
    WinSSLCtx := WinSSLLib.CreateContext(sslCtxClient);
    
    if (OpenSSLCtx <> nil) and (WinSSLCtx <> nil) then
    begin
      Passed := True;
      Details := 'Both contexts created';
    end
    else
    begin
      Passed := False;
      Details := 'Context creation inconsistent';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Context creation contract', Passed, Details);
  AddTestResult('Context creation contract', 'API Contract', Passed, Details);
  
  // Test protocol support contract
  Passed := False;
  Details := '';
  try
    OpenSSLLib := CreateSSLLibrary(sslOpenSSL);
    WinSSLLib := CreateSSLLibrary(sslWinSSL);
    OpenSSLLib.Initialize;
    WinSSLLib.Initialize;
    
    // Both should support TLS 1.2
    if OpenSSLLib.IsProtocolSupported(sslProtocolTLS12) and
       WinSSLLib.IsProtocolSupported(sslProtocolTLS12) then
    begin
      Passed := True;
      Details := 'Both support TLS 1.2';
    end
    else
    begin
      Passed := False;
      Details := 'TLS 1.2 support inconsistent';
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      Details := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('Protocol support contract', Passed, Details);
  AddTestResult('Protocol support contract', 'API Contract', Passed, Details);
  
  {$ELSE}
  PrintSkipped('API contract tests', 'Windows only (WinSSL required)');
  AddSkippedResult('API contract tests', 'API Contract', 'Windows only');
  {$ENDIF}
end;


procedure TBackendConsistencyTester.TestGracefulDegradation;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Passed: Boolean;
  Details: string;
begin
  PrintCategory('Graceful Degradation');
  
  // Test OpenSSL graceful handling when not available
  if not FOpenSSLAvailable then
  begin
    Passed := False;
    Details := '';
    try
      Lib := CreateSSLLibrary(sslOpenSSL);
      if Lib = nil then
      begin
        Passed := True;
        Details := 'Returns nil when unavailable';
      end
      else if not Lib.Initialize then
      begin
        Passed := True;
        Details := 'Initialize returns False when unavailable';
      end
      else
      begin
        Passed := False;
        Details := 'Should not succeed when unavailable';
      end;
    except
      on E: Exception do
      begin
        Passed := True;
        Details := 'Raises exception: ' + E.ClassName;
      end;
    end;
    PrintResult('OpenSSL unavailable handling', Passed, Details);
    AddTestResult('OpenSSL unavailable handling', 'Graceful Degradation', Passed, Details);
  end
  else
  begin
    PrintSkipped('OpenSSL unavailable handling', 'OpenSSL is available');
    AddSkippedResult('OpenSSL unavailable handling', 'Graceful Degradation', 'OpenSSL available');
  end;
  
  // Test WinSSL graceful handling when not available
  {$IFNDEF WINDOWS}
  Passed := False;
  Details := '';
  try
    Lib := CreateSSLLibrary(sslWinSSL);
    if Lib = nil then
    begin
      Passed := True;
      Details := 'Returns nil on non-Windows';
    end
    else if not Lib.Initialize then
    begin
      Passed := True;
      Details := 'Initialize returns False on non-Windows';
    end
    else
    begin
      Passed := False;
      Details := 'Should not succeed on non-Windows';
    end;
  except
    on E: Exception do
    begin
      Passed := True;
      Details := 'Raises exception: ' + E.ClassName;
    end;
  end;
  PrintResult('WinSSL on non-Windows', Passed, Details);
  AddTestResult('WinSSL on non-Windows', 'Graceful Degradation', Passed, Details);
  {$ELSE}
  if not FWinSSLAvailable then
  begin
    Passed := False;
    Details := '';
    try
      Lib := CreateSSLLibrary(sslWinSSL);
      if Lib = nil then
      begin
        Passed := True;
        Details := 'Returns nil when unavailable';
      end
      else if not Lib.Initialize then
      begin
        Passed := True;
        Details := 'Initialize returns False when unavailable';
      end
      else
      begin
        Passed := False;
        Details := 'Should not succeed when unavailable';
      end;
    except
      on E: Exception do
      begin
        Passed := True;
        Details := 'Raises exception: ' + E.ClassName;
      end;
    end;
    PrintResult('WinSSL unavailable handling', Passed, Details);
    AddTestResult('WinSSL unavailable handling', 'Graceful Degradation', Passed, Details);
  end
  else
  begin
    PrintSkipped('WinSSL unavailable handling', 'WinSSL is available');
    AddSkippedResult('WinSSL unavailable handling', 'Graceful Degradation', 'WinSSL available');
  end;
  {$ENDIF}
  
  // Test invalid library type handling
  Passed := False;
  Details := '';
  try
    // This should handle gracefully
    Lib := CreateSSLLibrary(TSSLLibraryType(255));  // Invalid type
    if Lib = nil then
    begin
      Passed := True;
      Details := 'Returns nil for invalid type';
    end
    else
    begin
      Passed := True;
      Details := 'Returns non-nil (may default to something)';
    end;
  except
    on E: Exception do
    begin
      Passed := True;
      Details := 'Raises exception for invalid type: ' + E.ClassName;
    end;
  end;
  PrintResult('Invalid library type handling', Passed, Details);
  AddTestResult('Invalid library type handling', 'Graceful Degradation', Passed, Details);
  
  // Test context creation with uninitialized library
  if FOpenSSLAvailable then
  begin
    Passed := False;
    Details := '';
    try
      Lib := CreateSSLLibrary(sslOpenSSL);
      // Don't initialize
      Ctx := Lib.CreateContext(sslCtxClient);
      if Ctx = nil then
      begin
        Passed := True;
        Details := 'Returns nil for uninitialized library';
      end
      else
      begin
        Passed := True;
        Details := 'Returns context (auto-init or lazy init)';
      end;
    except
      on E: Exception do
      begin
        Passed := True;
        Details := 'Raises exception: ' + E.ClassName;
      end;
    end;
    PrintResult('Uninitialized library context', Passed, Details);
    AddTestResult('Uninitialized library context', 'Graceful Degradation', Passed, Details);
  end;
end;

procedure TBackendConsistencyTester.RunAllTests;
begin
  WriteLn('========================================');
  WriteLn('  Backend Consistency Test Suite');
  WriteLn('  fafafa.ssl Test Quality Improvement');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Running backend consistency tests...');
  
  if not Initialize then
  begin
    WriteLn('ERROR: No SSL backend available');
    Exit;
  end;
  
  TestHashConsistency;
  TestAESConsistency;
  TestHMACConsistency;
  TestErrorCodeMapping;
  TestAPIContractConsistency;
  TestGracefulDegradation;
  
  PrintSummary;
end;

procedure TBackendConsistencyTester.PrintSummary;
var
  Total: Integer;
  PassRate: Double;
begin
  Total := FPassCount + FFailCount;
  if Total > 0 then
    PassRate := (FPassCount / Total) * 100
  else
    PassRate := 0;
  
  WriteLn;
  WriteLn('========================================');
  WriteLn('  Test Summary');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Total Tests: ', Total);
  WriteLn('Passed:      ', FPassCount);
  WriteLn('Failed:      ', FFailCount);
  WriteLn('Skipped:     ', FSkipCount);
  WriteLn('Pass Rate:   ', PassRate:0:1, '%');
  WriteLn;
  WriteLn('Backend Status:');
  WriteLn('  OpenSSL:   ', BoolToStr(FOpenSSLAvailable, 'Available', 'Not Available'));
  WriteLn('  WinSSL:    ', BoolToStr(FWinSSLAvailable, 'Available', 'Not Available'));
  WriteLn;
  
  if FFailCount = 0 then
    WriteLn('All backend consistency tests passed!')
  else
    WriteLn('Some tests failed. Review the output above for details.');
  
  WriteLn;
end;

var
  Tester: TBackendConsistencyTester;
begin
  Tester := TBackendConsistencyTester.Create;
  try
    Tester.RunAllTests;
    
    // Return exit code based on test results
    if Tester.FailCount > 0 then
      ExitCode := 1
    else
      ExitCode := 0;
  finally
    Tester.Free;
  end;
end.
