program test_winssl_certificate_san;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  WinSSL SAN Parsing Test

  Purpose:
    Validate that the WinSSL backend parses SubjectAltName (SAN) extensions
    consistently with the OpenSSL backend, including DNS and IP entries.

  Notes:
    - This test must be run on Windows (uses Schannel APIs).
    - It loads a DER-encoded test certificate generated from tests/certs/san-test.pem:
        tests/certs/winssl-san-test.cer
}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ELSE}
  {$ERROR 'This test requires Windows platform'}
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib,
  fafafa.ssl.winssl.certificate;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure AssertTrue(const TestName: string; Condition: Boolean);
begin
  Write('  [TEST] ', TestName, '... ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(TestsFailed);
  end;
end;

procedure AssertNotNil(const TestName: string; Ptr: Pointer);
begin
  AssertTrue(TestName, Ptr <> nil);
end;

procedure TestWinSSLSAN;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  SANs: TStringList;
  Info: TSSLCertificateInfo;
  HasSanTest, HasExample, HasIp: Boolean;
  I: Integer;
  InInfo: Boolean;
  CertPath: string;
begin
  WriteLn;
  WriteLn('=== WinSSL Certificate SAN Parsing Tests ===');

  Lib := CreateWinSSLLibrary;
  AssertNotNil('CreateWinSSLLibrary returns instance', Pointer(Lib));
  if (Lib = nil) or (not Lib.Initialize) then
  begin
    WriteLn('  Note: WinSSL library initialization failed, skipping SAN tests');
    AssertTrue('Initialize WinSSL library', False);
    Exit;
  end;

  Cert := Lib.CreateCertificate;
  if Cert = nil then
  begin
    WriteLn('  Note: CreateCertificate returned nil, skipping SAN tests');
    AssertTrue('CreateCertificate returns non-nil', False);
    Exit;
  end;

  CertPath := 'tests' + DirectorySeparator + 'certs' + DirectorySeparator + 'winssl-san-test.cer';
  if not FileExists(CertPath) then
  begin
    WriteLn('  Note: SAN test certificate not found at ', CertPath);
    AssertTrue('SAN test certificate file exists', False);
    Exit;
  end;

  if not Cert.LoadFromFile(CertPath) then
  begin
    WriteLn('  Note: LoadFromFile failed for SAN test certificate');
    AssertTrue('LoadFromFile succeeds', False);
    Exit;
  end;

  SANs := Cert.GetSubjectAltNames;
  try
    AssertNotNil('GetSubjectAltNames returns non-nil list', Pointer(SANs));
    HasSanTest := SANs.IndexOf('san-test.local') <> -1;
    HasExample := SANs.IndexOf('example.test') <> -1;
    HasIp := SANs.IndexOf('127.0.0.1') <> -1;
    AssertTrue('SANs contain DNS:san-test.local', HasSanTest);
    AssertTrue('SANs contain DNS:example.test', HasExample);
    AssertTrue('SANs contain IP:127.0.0.1', HasIp);
  finally
    SANs.Free;
  end;

  Info := Cert.GetInfo;
  InInfo := False;
  for I := 0 to High(Info.SubjectAltNames) do
  begin
    if Info.SubjectAltNames[I] = 'san-test.local' then
    begin
      InInfo := True;
      Break;
    end;
  end;
  AssertTrue('Info.SubjectAltNames contains DNS:san-test.local', InInfo);
end;

procedure PrintSummary;
var
  Total: Integer;
begin
  Total := TestsPassed + TestsFailed;
  WriteLn;
  WriteLn('========================================');
  WriteLn('WinSSL SAN Test Summary');
  WriteLn('========================================');
  WriteLn('Total tests: ', Total);
  WriteLn('Passed:      ', TestsPassed);
  WriteLn('Failed:      ', TestsFailed);
  if Total > 0 then
    WriteLn('Success rate: ', (TestsPassed * 100) div Total, '%');

  if TestsFailed = 0 then
  begin
    WriteLn('✅ ALL SAN TESTS PASSED');
    Halt(0);
  end
  else
  begin
    WriteLn('❌ SOME SAN TESTS FAILED');
    Halt(1);
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('WinSSL SAN Parsing Test');
  WriteLn('========================================');

  try
    TestWinSSLSAN;
    PrintSummary;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
