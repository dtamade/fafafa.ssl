{
  test_base.pas - fafafa.ssl.base 模块单元测试

  测试基础类型、枚举、记录和 Result 类型的功能
  
  Part of test-quality-improvement Phase 3 Task 10
}

program test_base;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions;

var
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;

procedure Check(const ATestName: string; ACondition: Boolean; const ADetails: string = '');
begin
  if ACondition then
  begin
    Inc(GPassCount);
    Write('[PASS] ');
  end
  else
  begin
    Inc(GFailCount);
    Write('[FAIL] ');
  end;
  Write(ATestName);
  if ADetails <> '' then
    WriteLn(' - ', ADetails)
  else
    WriteLn;
end;

// ============================================================================
// TBytesView Tests
// ============================================================================

procedure TestBytesViewFromBytes;
var
  Data: TBytes;
  View: TBytesView;
begin
  WriteLn;
  WriteLn('=== TBytesView.FromBytes Tests ===');
  
  // Test with normal data
  SetLength(Data, 10);
  FillChar(Data[0], 10, $AA);
  View := TBytesView.FromBytes(Data);
  Check('FromBytes creates valid view', View.IsValid);
  Check('FromBytes correct length', View.Length = 10);
  Check('FromBytes correct data', View.GetByte(0) = $AA);
  
  // Test with empty data
  SetLength(Data, 0);
  View := TBytesView.FromBytes(Data);
  Check('FromBytes empty is empty', View.IsEmpty);
end;

procedure TestBytesViewFromPtr;
var
  Data: TBytes;
  View: TBytesView;
begin
  WriteLn;
  WriteLn('=== TBytesView.FromPtr Tests ===');
  
  SetLength(Data, 5);
  Data[0] := 1; Data[1] := 2; Data[2] := 3; Data[3] := 4; Data[4] := 5;
  
  View := TBytesView.FromPtr(@Data[0], 5);
  Check('FromPtr creates valid view', View.IsValid);
  Check('FromPtr correct length', View.Length = 5);
  Check('FromPtr correct data[0]', View.GetByte(0) = 1);
  Check('FromPtr correct data[4]', View.GetByte(4) = 5);
  
  // Test nil pointer
  View := TBytesView.FromPtr(nil, 0);
  Check('FromPtr nil is not valid', not View.IsValid);
end;

procedure TestBytesViewEmpty;
var
  View: TBytesView;
begin
  WriteLn;
  WriteLn('=== TBytesView.Empty Tests ===');
  
  View := TBytesView.Empty;
  Check('Empty view is empty', View.IsEmpty);
  Check('Empty view length is 0', View.Length = 0);
  Check('Empty view is not valid', not View.IsValid);
end;

procedure TestBytesViewSlice;
var
  Data: TBytes;
  View, SliceView: TBytesView;
begin
  WriteLn;
  WriteLn('=== TBytesView.Slice Tests ===');
  
  SetLength(Data, 10);
  FillChar(Data[0], 10, 0);
  Data[3] := $BB;
  Data[4] := $CC;
  Data[5] := $DD;
  
  View := TBytesView.FromBytes(Data);
  SliceView := View.Slice(3, 3);
  
  Check('Slice correct length', SliceView.Length = 3);
  Check('Slice correct data[0]', SliceView.GetByte(0) = $BB);
  Check('Slice correct data[1]', SliceView.GetByte(1) = $CC);
  Check('Slice correct data[2]', SliceView.GetByte(2) = $DD);
end;

procedure TestBytesViewAsBytes;
var
  Data: TBytes;
  View: TBytesView;
  Copied: TBytes;
begin
  WriteLn;
  WriteLn('=== TBytesView.AsBytes Tests ===');
  
  SetLength(Data, 5);
  Data[0] := 10; Data[1] := 20; Data[2] := 30; Data[3] := 40; Data[4] := 50;
  
  View := TBytesView.FromBytes(Data);
  Copied := View.AsBytes;
  
  Check('AsBytes correct length', Length(Copied) = 5);
  Check('AsBytes correct data', (Copied[0] = 10) and (Copied[4] = 50));
  
  // Modify original, copy should be independent
  Data[0] := 99;
  Check('AsBytes is independent copy', Copied[0] = 10);
end;

// ============================================================================
// TSSLOperationResult Tests
// ============================================================================

procedure TestOperationResultOk;
var
  R: TSSLOperationResult;
begin
  WriteLn;
  WriteLn('=== TSSLOperationResult.Ok Tests ===');
  
  R := TSSLOperationResult.Ok;
  Check('Ok is success', R.Success);
  Check('Ok IsOk returns true', R.IsOk);
  Check('Ok IsErr returns false', not R.IsErr);
  Check('Ok error code is None', R.ErrorCode = sslErrNone);
end;

procedure TestOperationResultErr;
var
  R: TSSLOperationResult;
begin
  WriteLn;
  WriteLn('=== TSSLOperationResult.Err Tests ===');
  
  R := TSSLOperationResult.Err(sslErrInvalidParam, 'Test error');
  Check('Err is not success', not R.Success);
  Check('Err IsOk returns false', not R.IsOk);
  Check('Err IsErr returns true', R.IsErr);
  Check('Err has correct code', R.ErrorCode = sslErrInvalidParam);
  Check('Err has correct message', R.ErrorMessage = 'Test error');
end;

procedure TestOperationResultExpect;
var
  R: TSSLOperationResult;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== TSSLOperationResult.Expect Tests ===');
  
  // Ok should not raise
  R := TSSLOperationResult.Ok;
  ExceptionRaised := False;
  try
    R.Expect('Should not fail');
  except
    ExceptionRaised := True;
  end;
  Check('Expect on Ok does not raise', not ExceptionRaised);
  
  // Err should raise
  R := TSSLOperationResult.Err(sslErrGeneral, 'Error');
  ExceptionRaised := False;
  try
    R.Expect('Custom message');
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      Check('Expect raises with custom message', Pos('Custom message', E.Message) > 0);
    end;
  end;
  Check('Expect on Err raises exception', ExceptionRaised);
end;

// ============================================================================
// TSSLDataResult Tests
// ============================================================================

procedure TestDataResultOk;
var
  R: TSSLDataResult;
  Data: TBytes;
begin
  WriteLn;
  WriteLn('=== TSSLDataResult.Ok Tests ===');
  
  SetLength(Data, 3);
  Data[0] := 1; Data[1] := 2; Data[2] := 3;
  
  R := TSSLDataResult.Ok(Data);
  Check('Ok is success', R.Success);
  Check('Ok IsOk returns true', R.IsOk);
  Check('Ok has correct data length', Length(R.Data) = 3);
  Check('Ok has correct data', (R.Data[0] = 1) and (R.Data[2] = 3));
end;

procedure TestDataResultErr;
var
  R: TSSLDataResult;
begin
  WriteLn;
  WriteLn('=== TSSLDataResult.Err Tests ===');
  
  R := TSSLDataResult.Err(sslErrDecryptionFailed, 'Decryption failed');
  Check('Err is not success', not R.Success);
  Check('Err IsErr returns true', R.IsErr);
  Check('Err has correct code', R.ErrorCode = sslErrDecryptionFailed);
  Check('Err data is empty', Length(R.Data) = 0);
end;

procedure TestDataResultUnwrap;
var
  R: TSSLDataResult;
  Data, Unwrapped: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== TSSLDataResult.Unwrap Tests ===');
  
  SetLength(Data, 2);
  Data[0] := $AA; Data[1] := $BB;
  
  R := TSSLDataResult.Ok(Data);
  Unwrapped := R.Unwrap;
  Check('Unwrap returns correct data', (Unwrapped[0] = $AA) and (Unwrapped[1] = $BB));
  
  // Unwrap on error should raise
  R := TSSLDataResult.Err(sslErrGeneral, 'Error');
  ExceptionRaised := False;
  try
    Unwrapped := R.Unwrap;
  except
    ExceptionRaised := True;
  end;
  Check('Unwrap on Err raises exception', ExceptionRaised);
end;

procedure TestDataResultUnwrapOr;
var
  R: TSSLDataResult;
  Data, Default, Result: TBytes;
begin
  WriteLn;
  WriteLn('=== TSSLDataResult.UnwrapOr Tests ===');
  
  SetLength(Data, 1);
  Data[0] := $11;
  SetLength(Default, 1);
  Default[0] := $FF;
  
  // Ok returns actual data
  R := TSSLDataResult.Ok(Data);
  Result := R.UnwrapOr(Default);
  Check('UnwrapOr on Ok returns data', Result[0] = $11);
  
  // Err returns default
  R := TSSLDataResult.Err(sslErrGeneral, 'Error');
  Result := R.UnwrapOr(Default);
  Check('UnwrapOr on Err returns default', Result[0] = $FF);
end;

// ============================================================================
// TSSLStringResult Tests
// ============================================================================

procedure TestStringResultOk;
var
  R: TSSLStringResult;
begin
  WriteLn;
  WriteLn('=== TSSLStringResult.Ok Tests ===');
  
  R := TSSLStringResult.Ok('Hello');
  Check('Ok is success', R.Success);
  Check('Ok has correct value', R.Value = 'Hello');
  Check('Ok Unwrap returns value', R.Unwrap = 'Hello');
end;

procedure TestStringResultErr;
var
  R: TSSLStringResult;
begin
  WriteLn;
  WriteLn('=== TSSLStringResult.Err Tests ===');
  
  R := TSSLStringResult.Err(sslErrParseFailed, 'Parse error');
  Check('Err is not success', not R.Success);
  Check('Err has correct code', R.ErrorCode = sslErrParseFailed);
  Check('Err value is empty', R.Value = '');
end;

procedure TestStringResultUnwrapOr;
var
  R: TSSLStringResult;
begin
  WriteLn;
  WriteLn('=== TSSLStringResult.UnwrapOr Tests ===');
  
  R := TSSLStringResult.Ok('Value');
  Check('UnwrapOr on Ok returns value', R.UnwrapOr('Default') = 'Value');
  
  R := TSSLStringResult.Err(sslErrGeneral, 'Error');
  Check('UnwrapOr on Err returns default', R.UnwrapOr('Default') = 'Default');
end;

// ============================================================================
// TBuildValidationResult Tests
// ============================================================================

procedure TestBuildValidationResultOk;
var
  R: TBuildValidationResult;
begin
  WriteLn;
  WriteLn('=== TBuildValidationResult.Ok Tests ===');
  
  R := TBuildValidationResult.Ok;
  Check('Ok is valid', R.IsValid);
  Check('Ok has no warnings', not R.HasWarnings);
  Check('Ok has no errors', not R.HasErrors);
end;

procedure TestBuildValidationResultWithWarnings;
var
  R: TBuildValidationResult;
begin
  WriteLn;
  WriteLn('=== TBuildValidationResult.WithWarnings Tests ===');
  
  R := TBuildValidationResult.WithWarnings(['Warning 1', 'Warning 2']);
  Check('WithWarnings is valid', R.IsValid);
  Check('WithWarnings has warnings', R.HasWarnings);
  Check('WithWarnings warning count', R.WarningCount = 2);
  Check('WithWarnings has no errors', not R.HasErrors);
end;

procedure TestBuildValidationResultWithErrors;
var
  R: TBuildValidationResult;
begin
  WriteLn;
  WriteLn('=== TBuildValidationResult.WithErrors Tests ===');
  
  R := TBuildValidationResult.WithErrors(['Error 1']);
  Check('WithErrors is not valid', not R.IsValid);
  Check('WithErrors has errors', R.HasErrors);
  Check('WithErrors error count', R.ErrorCount = 1);
end;

procedure TestBuildValidationResultAddMethods;
var
  R: TBuildValidationResult;
begin
  WriteLn;
  WriteLn('=== TBuildValidationResult Add Methods Tests ===');
  
  R := TBuildValidationResult.Ok;
  R.AddWarning('New warning');
  Check('AddWarning increases count', R.WarningCount = 1);
  Check('AddWarning keeps valid', R.IsValid);
  
  R.AddError('New error');
  Check('AddError increases count', R.ErrorCount = 1);
  Check('AddError makes invalid', not R.IsValid);
end;

// ============================================================================
// Enum Tests
// ============================================================================

procedure TestEnumValues;
begin
  WriteLn;
  WriteLn('=== Enum Value Tests ===');
  
  // TSSLErrorCode
  Check('sslErrNone is 0', Ord(sslErrNone) = 0);
  Check('Error codes are distinct', Ord(sslErrGeneral) <> Ord(sslErrMemory));
  
  // TSSLProtocolVersion
  Check('Protocol versions ordered', Ord(sslProtocolTLS12) < Ord(sslProtocolTLS13));
  
  // TSSLLibraryType
  Check('Library types defined', Ord(sslOpenSSL) > 0);
  
  // TSSLLogLevel
  Check('Log levels ordered', Ord(sslLogError) < Ord(sslLogDebug));
end;

// ============================================================================
// Main
// ============================================================================

procedure PrintSummary;
var
  Total: Integer;
begin
  Total := GPassCount + GFailCount;
  WriteLn;
  WriteLn('========================================');
  WriteLn('  Test Summary');
  WriteLn('========================================');
  WriteLn('Total:  ', Total);
  WriteLn('Passed: ', GPassCount);
  WriteLn('Failed: ', GFailCount);
  if Total > 0 then
    WriteLn('Rate:   ', (GPassCount * 100) div Total, '%');
  WriteLn;
  
  if GFailCount = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed.');
end;

begin
  WriteLn('fafafa.ssl.base Unit Tests');
  WriteLn('==========================');
  
  // TBytesView tests
  TestBytesViewFromBytes;
  TestBytesViewFromPtr;
  TestBytesViewEmpty;
  TestBytesViewSlice;
  TestBytesViewAsBytes;
  
  // TSSLOperationResult tests
  TestOperationResultOk;
  TestOperationResultErr;
  TestOperationResultExpect;
  
  // TSSLDataResult tests
  TestDataResultOk;
  TestDataResultErr;
  TestDataResultUnwrap;
  TestDataResultUnwrapOr;
  
  // TSSLStringResult tests
  TestStringResultOk;
  TestStringResultErr;
  TestStringResultUnwrapOr;
  
  // TBuildValidationResult tests
  TestBuildValidationResultOk;
  TestBuildValidationResultWithWarnings;
  TestBuildValidationResultWithErrors;
  TestBuildValidationResultAddMethods;
  
  // Enum tests
  TestEnumValues;
  
  PrintSummary;
  
  if GFailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
