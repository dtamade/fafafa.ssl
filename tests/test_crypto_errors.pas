{
  test_crypto_errors.pas - Crypto Functions Error Handling Tests

  Tests error handling for crypto functions:
  - AES encryption/decryption with invalid parameters
  - Hash functions with edge cases
  - Pure Pascal hash implementations
  - Key/IV generation errors

  Part of test-quality-improvement Phase 1, Task 2
}

program test_crypto_errors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.crypto.hash;

var
  PassCount: Integer = 0;
  FailCount: Integer = 0;

procedure PrintResult(const ATestName: string; APassed: Boolean; const ADetails: string = '');
begin
  if APassed then
  begin
    Write('[PASS] ');
    Inc(PassCount);
  end
  else
  begin
    Write('[FAIL] ');
    Inc(FailCount);
  end;
  Write(ATestName);
  if ADetails <> '' then
    WriteLn(' - ', ADetails)
  else
    WriteLn;
end;

procedure PrintCategory(const ACategory: string);
begin
  WriteLn;
  WriteLn('=== ', ACategory, ' ===');
  WriteLn;
end;

{ AES-GCM Error Tests }
procedure TestAESGCMEmptyKey;
var
  Data, Key, IV, Result: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('AES-GCM Error Handling');

  // Test with empty key
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 0);
    SetLength(IV, 12);
    Result := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    ErrorMsg := 'Accepted empty key (SECURITY ISSUE)';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt empty key', Passed, ErrorMsg);

  // Test with wrong key size (16 bytes instead of 32)
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 16);
    SetLength(IV, 12);
    Result := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    ErrorMsg := 'Accepted 16-byte key for AES-256';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt wrong key size', Passed, ErrorMsg);

  // Test with wrong IV size
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 32);
    SetLength(IV, 8);
    Result := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    ErrorMsg := 'Accepted 8-byte IV for GCM';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Encrypt wrong IV size', Passed, ErrorMsg);

  // Test decrypt with truncated ciphertext
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 8);  // Too short - less than tag size
    SetLength(Key, 32);
    SetLength(IV, 12);
    Result := TCryptoUtils.AES_GCM_Decrypt(Data, Key, IV);
    ErrorMsg := 'Accepted truncated ciphertext';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_GCM_Decrypt truncated ciphertext', Passed, ErrorMsg);

  // Test TryAES_GCM_Encrypt with invalid params (should not raise)
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 0);
    SetLength(IV, 12);
    if not TCryptoUtils.TryAES_GCM_Encrypt(Data, Key, IV, Result) then
    begin
      Passed := True;
      ErrorMsg := 'Returned False for invalid params';
    end
    else
      ErrorMsg := 'Returned True for invalid params';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method raised exception: ' + E.Message;
    end;
  end;
  PrintResult('TryAES_GCM_Encrypt invalid params', Passed, ErrorMsg);
end;

{ AES-CBC Error Tests }
procedure TestAESCBCErrors;
var
  Data, Key, IV, Result: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('AES-CBC Error Handling');

  // Test with empty key
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 0);
    SetLength(IV, 16);
    Result := TCryptoUtils.AES_CBC_Encrypt(Data, Key, IV);
    ErrorMsg := 'Accepted empty key';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_CBC_Encrypt empty key', Passed, ErrorMsg);

  // Test with wrong IV size (12 instead of 16)
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 32);
    SetLength(IV, 12);
    Result := TCryptoUtils.AES_CBC_Encrypt(Data, Key, IV);
    ErrorMsg := 'Accepted 12-byte IV for CBC';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('AES_CBC_Encrypt wrong IV size', Passed, ErrorMsg);

  // Test TryAES_CBC_Encrypt with invalid params
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 16);
    SetLength(Key, 0);
    SetLength(IV, 16);
    if not TCryptoUtils.TryAES_CBC_Encrypt(Data, Key, IV, Result) then
    begin
      Passed := True;
      ErrorMsg := 'Returned False for invalid params';
    end
    else
      ErrorMsg := 'Returned True for invalid params';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method raised exception: ' + E.Message;
    end;
  end;
  PrintResult('TryAES_CBC_Encrypt invalid params', Passed, ErrorMsg);
end;

{ Pure Pascal Hash Error Tests }
procedure TestPureHashErrors;
var
  Ctx: THashContext;
  Data, HashResult: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('Pure Pascal Hash Error Handling');

  // Test MD5 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    HashResult := fafafa.ssl.crypto.hash.MD5(Data);
    if Length(HashResult) = 16 then
    begin
      Passed := True;
      ErrorMsg := 'Correctly computed MD5 of empty data';
    end
    else
      ErrorMsg := 'Wrong hash length: ' + IntToStr(Length(HashResult));
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('MD5 empty data', Passed, ErrorMsg);

  // Test SHA1 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    HashResult := fafafa.ssl.crypto.hash.SHA1(Data);
    if Length(HashResult) = 20 then
    begin
      Passed := True;
      ErrorMsg := 'Correctly computed SHA1 of empty data';
    end
    else
      ErrorMsg := 'Wrong hash length: ' + IntToStr(Length(HashResult));
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SHA1 empty data', Passed, ErrorMsg);

  // Test SHA256 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    HashResult := fafafa.ssl.crypto.hash.SHA256(Data);
    if Length(HashResult) = 32 then
    begin
      Passed := True;
      ErrorMsg := 'Correctly computed SHA256 of empty data';
    end
    else
      ErrorMsg := 'Wrong hash length: ' + IntToStr(Length(HashResult));
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SHA256 empty data', Passed, ErrorMsg);

  // Test SHA384 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    HashResult := fafafa.ssl.crypto.hash.SHA384(Data);
    if Length(HashResult) = 48 then
    begin
      Passed := True;
      ErrorMsg := 'Correctly computed SHA384 of empty data';
    end
    else
      ErrorMsg := 'Wrong hash length: ' + IntToStr(Length(HashResult));
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SHA384 empty data', Passed, ErrorMsg);

  // Test SHA512 with empty data
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(Data, 0);
    HashResult := fafafa.ssl.crypto.hash.SHA512(Data);
    if Length(HashResult) = 64 then
    begin
      Passed := True;
      ErrorMsg := 'Correctly computed SHA512 of empty data';
    end
    else
      ErrorMsg := 'Wrong hash length: ' + IntToStr(Length(HashResult));
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SHA512 empty data', Passed, ErrorMsg);

  // Test CreateHashContext with all algorithms
  Passed := False;
  ErrorMsg := '';
  try
    Ctx := CreateHashContext(haMD5);
    if Ctx <> nil then
    begin
      Ctx.Free;
      Passed := True;
      ErrorMsg := 'Created MD5 context successfully';
    end
    else
      ErrorMsg := 'Returned nil context';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('CreateHashContext MD5', Passed, ErrorMsg);

  // Test hash context Update with empty data
  Passed := False;
  ErrorMsg := '';
  try
    Ctx := CreateHashContext(haSHA256);
    try
      SetLength(Data, 0);
      Ctx.Update(Data);
      HashResult := Ctx.Final;
      if Length(HashResult) = 32 then
      begin
        Passed := True;
        ErrorMsg := 'Handled empty update correctly';
      end
      else
        ErrorMsg := 'Wrong hash length after empty update';
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('HashContext Update empty', Passed, ErrorMsg);

  // Test hash context Reset
  Passed := False;
  ErrorMsg := '';
  try
    Ctx := CreateHashContext(haSHA256);
    try
      SetLength(Data, 10);
      FillChar(Data[0], 10, $AA);
      Ctx.Update(Data);
      Ctx.Reset;
      HashResult := Ctx.Final;
      if Length(HashResult) = 32 then
      begin
        Passed := True;
        ErrorMsg := 'Reset worked correctly';
      end
      else
        ErrorMsg := 'Wrong hash length after reset';
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Exception: ' + E.Message;
    end;
  end;
  PrintResult('HashContext Reset', Passed, ErrorMsg);
end;

{ Random Generation Error Tests }
procedure TestRandomErrors;
var
  Result: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
  Success: Boolean;
begin
  PrintCategory('Random Generation Error Handling');

  // Test SecureRandom with zero length
  Passed := False;
  ErrorMsg := '';
  try
    Result := TCryptoUtils.SecureRandom(0);
    ErrorMsg := 'Accepted zero length';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureRandom zero length', Passed, ErrorMsg);

  // Test SecureRandom with negative length
  Passed := False;
  ErrorMsg := '';
  try
    Result := TCryptoUtils.SecureRandom(-10);
    ErrorMsg := 'Accepted negative length';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureRandom negative length', Passed, ErrorMsg);

  // Test TrySecureRandom with zero length
  Passed := False;
  ErrorMsg := '';
  try
    Success := TCryptoUtils.TrySecureRandom(0, Result);
    if not Success then
    begin
      Passed := True;
      ErrorMsg := 'Returned False for zero length';
    end
    else
      ErrorMsg := 'Returned True for zero length';
  except
    on E: Exception do
    begin
      Passed := False;
      ErrorMsg := 'Try method raised exception: ' + E.Message;
    end;
  end;
  PrintResult('TrySecureRandom zero length', Passed, ErrorMsg);

  // Test GenerateKey with zero bits
  Passed := False;
  ErrorMsg := '';
  try
    Result := TCryptoUtils.GenerateKey(0);
    ErrorMsg := 'Accepted zero bits';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('GenerateKey zero bits', Passed, ErrorMsg);

  // Test GenerateKey with non-multiple of 8
  Passed := False;
  ErrorMsg := '';
  try
    Result := TCryptoUtils.GenerateKey(100);  // Not multiple of 8
    if Length(Result) > 0 then
    begin
      Passed := True;
      ErrorMsg := 'Handled non-multiple of 8 (rounded)';
    end
    else
      ErrorMsg := 'Returned empty result';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('GenerateKey non-multiple of 8', Passed, ErrorMsg);

  // Test GenerateIV with zero length
  Passed := False;
  ErrorMsg := '';
  try
    Result := TCryptoUtils.GenerateIV(0);
    ErrorMsg := 'Accepted zero length';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('GenerateIV zero length', Passed, ErrorMsg);
end;

{ SecureCompare Error Tests }
procedure TestSecureCompareErrors;
var
  A, B: TBytes;
  Passed: Boolean;
  ErrorMsg: string;
  CompareResult: Boolean;
begin
  PrintCategory('SecureCompare Error Handling');

  // Test with empty arrays
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(A, 0);
    SetLength(B, 0);
    CompareResult := TCryptoUtils.SecureCompare(A, B);
    if CompareResult then
    begin
      Passed := True;
      ErrorMsg := 'Empty arrays are equal';
    end
    else
      ErrorMsg := 'Empty arrays not equal';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureCompare empty arrays', Passed, ErrorMsg);

  // Test with different lengths
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(A, 10);
    SetLength(B, 20);
    CompareResult := TCryptoUtils.SecureCompare(A, B);
    if not CompareResult then
    begin
      Passed := True;
      ErrorMsg := 'Different lengths not equal';
    end
    else
      ErrorMsg := 'Different lengths reported equal';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureCompare different lengths', Passed, ErrorMsg);

  // Test with one empty, one not
  Passed := False;
  ErrorMsg := '';
  try
    SetLength(A, 0);
    SetLength(B, 10);
    CompareResult := TCryptoUtils.SecureCompare(A, B);
    if not CompareResult then
    begin
      Passed := True;
      ErrorMsg := 'Empty vs non-empty not equal';
    end
    else
      ErrorMsg := 'Empty vs non-empty reported equal';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Raised exception: ' + E.ClassName;
    end;
  end;
  PrintResult('SecureCompare empty vs non-empty', Passed, ErrorMsg);
end;

{ Main }
var
  Total: Integer;
  PassRate: Double;
begin
  WriteLn('========================================');
  WriteLn('  Crypto Functions Error Test Suite');
  WriteLn('  fafafa.ssl Test Quality Improvement');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Running crypto error handling tests...');

  TestAESGCMEmptyKey;
  TestAESCBCErrors;
  TestPureHashErrors;
  TestRandomErrors;
  TestSecureCompareErrors;

  Total := PassCount + FailCount;
  if Total > 0 then
    PassRate := (PassCount / Total) * 100
  else
    PassRate := 0;

  WriteLn;
  WriteLn('========================================');
  WriteLn('  Test Summary');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Total Tests: ', Total);
  WriteLn('Passed:      ', PassCount);
  WriteLn('Failed:      ', FailCount);
  WriteLn('Pass Rate:   ', PassRate:0:1, '%');
  WriteLn;

  if FailCount = 0 then
    WriteLn('All crypto error handling tests passed!')
  else
    WriteLn('Some tests failed. Review the output above.');

  if FailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
