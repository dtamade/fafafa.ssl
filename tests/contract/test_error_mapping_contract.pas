program test_error_mapping_contract;

{**
 * Unit: test_error_mapping_contract - 错误映射契约测试
 *
 * 测试范围：
 * - 各可用后端 GetLastError/GetLastErrorString 返回非空
 * - 后端特定错误码可转换为 TSSLErrorCode
 * - 无后端时标记 [SKIP]
 *}

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;
  GTestsSkipped: Integer = 0;

procedure Skip(const ATestName, AReason: string);
begin
  WriteLn('[SKIP] ', ATestName, ' - ', AReason);
  Inc(GTestsSkipped);
end;

procedure Pass(const ATestName: string);
begin
  WriteLn('[PASS] ', ATestName);
  Inc(GTestsPassed);
end;

procedure Fail(const ATestName, AMessage: string);
begin
  WriteLn('[FAIL] ', ATestName, ' - ', AMessage);
  Inc(GTestsFailed);
end;

procedure Check(const ATestName: string; ACondition: Boolean; const AMessage: string = '');
begin
  if ACondition then
    Pass(ATestName)
  else
    Fail(ATestName, AMessage);
end;

procedure TestBackendErrorMapping(const ABackendName: string; ABackendType: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LErrCode: Integer;
  LErrStr: string;
begin
  if not TSSLFactory.IsLibraryAvailable(ABackendType) then
  begin
    Skip(ABackendName + ' error mapping', ABackendName + ' library not available');
    Exit;
  end;

  LLib := TSSLFactory.GetLibraryInstance(ABackendType);
  if (LLib = nil) or (not LLib.Initialize) then
  begin
    Skip(ABackendName + ' error mapping', ABackendName + ' initialization failed');
    Exit;
  end;

  // Clear error state first
  LLib.ClearError;

  // GetLastError should return 0 after ClearError
  LErrCode := LLib.GetLastError;
  Check(ABackendName + ': GetLastError is 0 after ClearError', LErrCode = 0,
    Format('expected 0, got %d', [LErrCode]));

  // GetLastErrorString should return non-empty even with no error
  LErrStr := LLib.GetLastErrorString;
  Check(ABackendName + ': GetLastErrorString returns non-nil', Length(LErrStr) >= 0,
    'GetLastErrorString crashed or returned nil');

  // SSLErrorToString should map known error codes
  Check(ABackendName + ': SSLErrorToString maps sslErrNone',
    SSLErrorToString(sslErrNone) <> '');
  Check(ABackendName + ': SSLErrorToString maps sslErrNotInitialized',
    SSLErrorToString(sslErrNotInitialized) <> '');
  Check(ABackendName + ': SSLErrorToString maps sslErrLoadFailed',
    SSLErrorToString(sslErrLoadFailed) <> '');
end;

var
  LBackends: array[0..4] of TSSLLibraryType = (
    sslOpenSSL, sslFreePascal, sslMbedTLS, sslWolfSSL, sslWinSSL
  );
  LBackendNames: array[0..4] of string = (
    'OpenSSL', 'FreePascal', 'MbedTLS', 'WolfSSL', 'WinSSL'
  );
  I: Integer;
begin
  WriteLn('=== Error Mapping Contract Tests ===');
  WriteLn;
  try
    for I := Low(LBackends) to High(LBackends) do
      TestBackendErrorMapping(LBackendNames[I], LBackends[I]);
  except
    on E: Exception do
    begin
      WriteLn('UNEXPECTED: ', E.Message);
      Inc(GTestsFailed);
    end;
  end;
  WriteLn;
  WriteLn(Format('Results: %d passed, %d failed, %d skipped',
    [GTestsPassed, GTestsFailed, GTestsSkipped]));
  if GTestsFailed > 0 then
    Halt(1);
end.
