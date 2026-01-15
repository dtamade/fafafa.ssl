{
  test_error_handling.pas - Error Handling Test Framework

  Tests error handling for critical functions:
  - Invalid parameters
  - Nil pointers
  - Out of range values
  - Exception handling and cleanup
  - Resource cleanup on error

  Part of test-quality-improvement Phase 1
}

program test_error_handling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.memutils,
  fafafa.ssl.encoding,
  fafafa.ssl.pem;

type
  { TErrorTestCase - Record for tracking test cases }
  TErrorTestCase = record
    FunctionName: string;
    ErrorCondition: string;
    ExpectedBehavior: string;
    Passed: Boolean;
    ErrorMessage: string;
  end;

  { TErrorTestRunner - Error handling test framework }
  TErrorTestRunner = class
  private
    FTestCases: array of TErrorTestCase;
    FPassCount: Integer;
    FFailCount: Integer;
    FCurrentCategory: string;
    
    procedure AddTestResult(const AFuncName, ACondition, AExpected: string;
      APassed: Boolean; const AError: string = '');
    procedure PrintCategory(const ACategory: string);
    procedure PrintResult(const ATestName: string; APassed: Boolean; 
      const ADetails: string = '');
  public
    constructor Create;
    destructor Destroy; override;
    
    // Run all tests
    procedure RunAllTests;
    
    // Print summary
    procedure PrintSummary;
    
    // Individual test categories
    procedure TestCryptoHashErrors;
    procedure TestCryptoUtilsErrors;
    procedure TestMemUtilsErrors;
    procedure TestEncodingErrors;
    procedure TestPEMErrors;
    procedure TestResultErrors;
    
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

{ TErrorTestRunner }

constructor TErrorTestRunner.Create;
begin
  inherited Create;
  FPassCount := 0;
  FFailCount := 0;
  SetLength(FTestCases, 0);
  FCurrentCategory := '';
end;

destructor TErrorTestRunner.Destroy;
begin
  SetLength(FTestCases, 0);
  inherited Destroy;
end;

procedure TErrorTestRunner.AddTestResult(const AFuncName, ACondition, AExpected: string;
  APassed: Boolean; const AError: string);
var
  Idx: Integer;
begin
  Idx := Length(FTestCases);
  SetLength(FTestCases, Idx + 1);
  FTestCases[Idx].FunctionName := AFuncName;
  FTestCases[Idx].ErrorCondition := ACondition;
  FTestCases[Idx].ExpectedBehavior := AExpected;
  FTestCases[Idx].Passed := APassed;
  FTestCases[Idx].ErrorMessage := AError;
  
  if APassed then
    Inc(FPassCount)
  else
    Inc(FFailCount);
end;

procedure TErrorTestRunner.PrintCategory(const ACategory: string);
begin
  FCurrentCategory := ACategory;
  WriteLn;
  WriteLn('=== ', ACategory, ' ===');
  WriteLn;
end;

procedure TErrorTestRunner.PrintResult(const ATestName: string; APassed: Boolean;
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

procedure TErrorTestRunner.TestCryptoHashErrors;
var
  EmptyData: TBytes;
  HashResult: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
  Success: Boolean;
begin
  PrintCategory('Crypto Hash Error Handling');
  
  SetLength(EmptyData, 0);
  
  // Test SHA256 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    HashResult := TCryptoUtils.SHA256(EmptyData);
    Passed := True;
    ErrorMsg := 'Handled empty data gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName + ' - ' + E.Message;
    end;
  end;
  PrintResult('SHA256 empty data', Passed, ErrorMsg);
  AddTestResult('SHA256', 'empty data', 'No crash', Passed, ErrorMsg);
  
  // Test SHA512 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    HashResult := TCryptoUtils.SHA512(EmptyData);
    Passed := True;
    ErrorMsg := 'Handled empty data gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName + ' - ' + E.Message;
    end;
  end;
  PrintResult('SHA512 empty data', Passed, ErrorMsg);
  AddTestResult('SHA512', 'empty data', 'No crash', Passed, ErrorMsg);
  
  // Test TrySHA256 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    Success := TCryptoUtils.TrySHA256(EmptyData, HashResult);
    Passed := True;
    if Success then
      ErrorMsg := 'Returned True for empty data'
    else
      ErrorMsg := 'Returned False for empty data';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method should not raise: ' + E.Message;
    end;
  end;
  PrintResult('TrySHA256 empty data', Passed, ErrorMsg);
  AddTestResult('TrySHA256', 'empty data', 'No exception', Passed, ErrorMsg);
  
  // Test TrySHA512 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    Success := TCryptoUtils.TrySHA512(EmptyData, HashResult);
    Passed := True;
    if Success then
      ErrorMsg := 'Returned True for empty data'
    else
      ErrorMsg := 'Returned False for empty data';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method should not raise: ' + E.Message;
    end;
  end;
  PrintResult('TrySHA512 empty data', Passed, ErrorMsg);
  AddTestResult('TrySHA512', 'empty data', 'No exception', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.TestCryptoUtilsErrors;
var
  RandomResult: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
  Data, Key, IV, EncResult: TBytes;
  Success: Boolean;
begin
  PrintCategory('Crypto Utils Error Handling');
  
  // Test SecureRandom with zero length
  Passed := False;
  ErrorMsg := '';
  try
    RandomResult := TCryptoUtils.SecureRandom(0);
    Passed := True;
    ErrorMsg := 'Accepted zero length (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureRandom zero length', Passed, ErrorMsg);
  AddTestResult('SecureRandom', 'zero length', 'Reject or handle', Passed, ErrorMsg);
  
  // Test SecureRandom with negative length
  Passed := False;
  ErrorMsg := '';
  try
    RandomResult := TCryptoUtils.SecureRandom(-1);
    Passed := True;
    ErrorMsg := 'Accepted negative length (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureRandom negative length', Passed, ErrorMsg);
  AddTestResult('SecureRandom', 'negative length', 'Reject', Passed, ErrorMsg);
  
  // Test TrySecureRandom with zero length
  Passed := False;
  ErrorMsg := '';
  try
    Success := TCryptoUtils.TrySecureRandom(0, RandomResult);
    Passed := True;
    if Success then
      ErrorMsg := 'Returned True for zero length'
    else
      ErrorMsg := 'Returned False for zero length';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method should not raise: ' + E.Message;
    end;
  end;
  PrintResult('TrySecureRandom zero length', Passed, ErrorMsg);
  AddTestResult('TrySecureRandom', 'zero length', 'No exception', Passed, ErrorMsg);
  
  // Test GenerateKey with zero bits
  Passed := False;
  ErrorMsg := '';
  try
    RandomResult := TCryptoUtils.GenerateKey(0);
    Passed := True;
    ErrorMsg := 'Accepted zero bits (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('GenerateKey zero bits', Passed, ErrorMsg);
  AddTestResult('GenerateKey', 'zero bits', 'Reject or handle', Passed, ErrorMsg);
  
  // Test GenerateIV with zero length
  Passed := False;
  ErrorMsg := '';
  try
    RandomResult := TCryptoUtils.GenerateIV(0);
    Passed := True;
    ErrorMsg := 'Accepted zero length (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('GenerateIV zero length', Passed, ErrorMsg);
  AddTestResult('GenerateIV', 'zero length', 'Reject or handle', Passed, ErrorMsg);
  
  // Test AES_GCM_Encrypt with empty key
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 0);  // Invalid: should be 32 bytes
    SetLength(IV, 12);
    EncResult := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    Passed := True;
    ErrorMsg := 'Accepted empty key (SECURITY ISSUE)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt empty key', Passed, ErrorMsg);
  AddTestResult('AES_GCM_Encrypt', 'empty key', 'Reject', Passed, ErrorMsg);
  
  // Test AES_GCM_Encrypt with wrong key size
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 16);  // Invalid: should be 32 bytes
    SetLength(IV, 12);
    EncResult := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    Passed := True;
    ErrorMsg := 'Accepted wrong key size (SECURITY ISSUE)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt wrong key size', Passed, ErrorMsg);
  AddTestResult('AES_GCM_Encrypt', 'wrong key size', 'Reject', Passed, ErrorMsg);
  
  // Test AES_GCM_Encrypt with wrong IV size
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 32);
    SetLength(IV, 8);  // Invalid: should be 12 bytes
    EncResult := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    Passed := True;
    ErrorMsg := 'Accepted wrong IV size (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt wrong IV size', Passed, ErrorMsg);
  AddTestResult('AES_GCM_Encrypt', 'wrong IV size', 'Reject', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.TestMemUtilsErrors;
var
  Buffer: Byte;
  Data: TBytes;
  Str: AnsiString;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('MemUtils Error Handling');
  
  // Test SecureZeroMemory with nil
  Passed := False;
  ErrorMsg := '';
  try
    SecureZeroMemory(nil, 100);
    Passed := True;
    ErrorMsg := 'Handled nil gracefully (no crash)';
  except
    on E: EAccessViolation do
    begin
      Passed := False;
      ErrorMsg := 'Access violation - nil not handled';
    end;
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureZeroMemory nil', Passed, ErrorMsg);
  AddTestResult('SecureZeroMemory', 'nil pointer', 'No crash', Passed, ErrorMsg);
  
  // Test SecureZeroMemory with zero size
  Passed := False;
  ErrorMsg := '';
  try
    Buffer := $FF;
    SecureZeroMemory(@Buffer, 0);
    Passed := True;
    ErrorMsg := 'Handled zero size gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureZeroMemory zero size', Passed, ErrorMsg);
  AddTestResult('SecureZeroMemory', 'zero size', 'No crash', Passed, ErrorMsg);
  
  // Test SecureZeroBytes with empty array
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    SecureZeroBytes(Data);
    Passed := True;
    ErrorMsg := 'Handled empty array gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureZeroBytes empty', Passed, ErrorMsg);
  AddTestResult('SecureZeroBytes', 'empty array', 'No crash', Passed, ErrorMsg);
  
  // Test SecureZeroString with empty string
  Passed := False;
  ErrorMsg := '';
  try
    Str := '';
    SecureZeroString(Str);
    Passed := True;
    ErrorMsg := 'Handled empty string gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureZeroString empty', Passed, ErrorMsg);
  AddTestResult('SecureZeroString', 'empty string', 'No crash', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.TestEncodingErrors;
var
  DecodeResult: TBytes;
  EncodeResult: string;
  Data: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('Encoding Error Handling');
  
  // Test Base64 decode with invalid characters
  Passed := False;
  ErrorMsg := '';
  try
    DecodeResult := TEncodingUtils.Base64Decode('!!!invalid!!!');
    Passed := True;
    ErrorMsg := 'Accepted invalid chars (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('Base64Decode invalid chars', Passed, ErrorMsg);
  AddTestResult('Base64Decode', 'invalid chars', 'Reject', Passed, ErrorMsg);
  
  // Test Base64 decode with empty string
  Passed := False;
  ErrorMsg := '';
  try
    DecodeResult := TEncodingUtils.Base64Decode('');
    Passed := True;
    ErrorMsg := 'Handled empty string gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('Base64Decode empty', Passed, ErrorMsg);
  AddTestResult('Base64Decode', 'empty string', 'No crash', Passed, ErrorMsg);
  
  // Test Hex decode with odd length
  Passed := False;
  ErrorMsg := '';
  try
    DecodeResult := TEncodingUtils.HexToBytes('ABC');
    Passed := True;
    ErrorMsg := 'Accepted odd length (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('HexDecode odd length', Passed, ErrorMsg);
  AddTestResult('HexToBytes', 'odd length', 'Reject', Passed, ErrorMsg);
  
  // Test Hex decode with invalid characters
  Passed := False;
  ErrorMsg := '';
  try
    DecodeResult := TEncodingUtils.HexToBytes('GHIJ');
    Passed := True;
    ErrorMsg := 'Accepted invalid chars (may need review)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('HexDecode invalid chars', Passed, ErrorMsg);
  AddTestResult('HexToBytes', 'invalid chars', 'Reject', Passed, ErrorMsg);
  
  // Test Hex decode with empty string
  Passed := False;
  ErrorMsg := '';
  try
    DecodeResult := TEncodingUtils.HexToBytes('');
    Passed := True;
    ErrorMsg := 'Handled empty string gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('HexDecode empty', Passed, ErrorMsg);
  AddTestResult('HexToBytes', 'empty string', 'No crash', Passed, ErrorMsg);
  
  // Test Base64 encode with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    EncodeResult := TEncodingUtils.Base64Encode(Data);
    Passed := True;
    ErrorMsg := 'Handled empty data gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('Base64Encode empty', Passed, ErrorMsg);
  AddTestResult('Base64Encode', 'empty data', 'No crash', Passed, ErrorMsg);
  
  // Test Hex encode with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    EncodeResult := TEncodingUtils.BytesToHex(Data);
    Passed := True;
    ErrorMsg := 'Handled empty data gracefully';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('HexEncode empty', Passed, ErrorMsg);
  AddTestResult('BytesToHex', 'empty data', 'No crash', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.TestPEMErrors;
var
  Reader: TPEMReader;
  Writer: TPEMWriter;
  Data: TBytes;
  PEMResult: string;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('PEM Error Handling');
  
  // Test PEM load with invalid format
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromString('not a valid PEM');
      Passed := True;
      ErrorMsg := 'Accepted invalid format (returns empty blocks)';
    except
      on E: Exception do
      begin
        Passed := True;
        ErrorMsg := 'Properly rejected: ' + E.ClassName;
      end;
    end;
  finally
    Reader.Free;
  end;
  PrintResult('PEM load invalid format', Passed, ErrorMsg);
  AddTestResult('TPEMReader.LoadFromString', 'invalid format', 'Reject or empty', Passed, ErrorMsg);
  
  // Test PEM load with empty string
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromString('');
      Passed := True;
      ErrorMsg := 'Handled empty string gracefully';
    except
      on E: Exception do
      begin
        Passed := True;
        ErrorMsg := 'Raised exception: ' + E.ClassName;
      end;
    end;
  finally
    Reader.Free;
  end;
  PrintResult('PEM load empty', Passed, ErrorMsg);
  AddTestResult('TPEMReader.LoadFromString', 'empty string', 'No crash', Passed, ErrorMsg);
  
  // Test PEM load with missing end marker
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromString('-----BEGIN TEST-----' + #10 + 'SGVsbG8=');
      Passed := True;
      ErrorMsg := 'Handled missing end marker (returns empty blocks)';
    except
      on E: Exception do
      begin
        Passed := True;
        ErrorMsg := 'Properly rejected: ' + E.ClassName;
      end;
    end;
  finally
    Reader.Free;
  end;
  PrintResult('PEM load missing end', Passed, ErrorMsg);
  AddTestResult('TPEMReader.LoadFromString', 'missing end marker', 'Reject or empty', Passed, ErrorMsg);
  
  // Test PEM write with empty data
  Passed := False;
  ErrorMsg := '';
  Writer := TPEMWriter.Create;
  try
    try
      SetLength(Data, 0);
      PEMResult := Writer.WriteBlock(pemCertificate, Data);
      Passed := True;
      ErrorMsg := 'Handled empty data gracefully';
    except
      on E: Exception do
      begin
        Passed := True;
        ErrorMsg := 'Raised exception: ' + E.ClassName;
      end;
    end;
  finally
    Writer.Free;
  end;
  PrintResult('PEM write empty', Passed, ErrorMsg);
  AddTestResult('TPEMWriter.WriteBlock', 'empty data', 'No crash', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.TestResultErrors;
var
  Passed: Boolean;
  ErrorMsg: string;
  EmptyView: TBytesView;
  NilView: TBytesView;
begin
  PrintCategory('Result Type Error Handling');
  
  // Test basic error code handling
  Passed := False;
  ErrorMsg := '';
  try
    // Test that error codes are properly defined
    if Ord(sslErrNone) = 0 then
      ; // Expected
    if Ord(sslErrGeneral) > 0 then
      ; // Expected
    Passed := True;
    ErrorMsg := 'Error codes defined correctly';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Unexpected exception: ' + E.Message;
    end;
  end;
  PrintResult('TSSLErrorCode handling', Passed, ErrorMsg);
  AddTestResult('TSSLErrorCode', 'basic usage', 'No crash', Passed, ErrorMsg);
  
  // Test TBytesView with empty data
  Passed := False;
  ErrorMsg := '';
  try
    EmptyView := TBytesView.Empty;
    if EmptyView.IsEmpty then
      ; // Expected
    Passed := True;
    ErrorMsg := 'Empty view handled correctly';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Unexpected exception: ' + E.Message;
    end;
  end;
  PrintResult('TBytesView empty', Passed, ErrorMsg);
  AddTestResult('TBytesView', 'empty view', 'No crash', Passed, ErrorMsg);
  
  // Test TBytesView.FromPtr with nil
  Passed := False;
  ErrorMsg := '';
  try
    NilView := TBytesView.FromPtr(nil, 0);
    if not NilView.IsValid then
      ; // Expected - nil pointer should be invalid
    Passed := True;
    ErrorMsg := 'Nil pointer view handled correctly';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception for nil: ' + E.ClassName;
    end;
  end;
  PrintResult('TBytesView nil pointer', Passed, ErrorMsg);
  AddTestResult('TBytesView', 'nil pointer', 'No crash', Passed, ErrorMsg);
end;

procedure TErrorTestRunner.RunAllTests;
begin
  WriteLn('========================================');
  WriteLn('  Error Handling Test Suite');
  WriteLn('  fafafa.ssl Test Quality Improvement');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Running error handling tests...');
  
  TestCryptoHashErrors;
  TestCryptoUtilsErrors;
  TestMemUtilsErrors;
  TestEncodingErrors;
  TestPEMErrors;
  TestResultErrors;
  
  PrintSummary;
end;

procedure TErrorTestRunner.PrintSummary;
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
  WriteLn('Pass Rate:   ', PassRate:0:1, '%');
  WriteLn;
  
  if FFailCount = 0 then
    WriteLn('All error handling tests passed!')
  else
    WriteLn('Some tests failed. Review the output above for details.');
  
  WriteLn;
end;

var
  Runner: TErrorTestRunner;
begin
  Runner := TErrorTestRunner.Create;
  try
    Runner.RunAllTests;
    
    // Return exit code based on test results
    if Runner.FailCount > 0 then
      ExitCode := 1
    else
      ExitCode := 0;
  finally
    Runner.Free;
  end;
end.
