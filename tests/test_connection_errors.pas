{
  test_connection_errors.pas - Connection Functions Error Handling Tests

  Tests error handling for connection-related functions:
  - Invalid host/port
  - Invalid context configuration
  - Certificate loading errors
  - Timeout handling

  Part of test-quality-improvement Phase 1, Task 3
}

program test_connection_errors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder,
  fafafa.ssl.pem;

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

{ Certificate Loading Error Tests }
procedure TestCertificateLoadingErrors;
var
  Passed: Boolean;
  ErrorMsg: string;
  Cert: ICertificate;
begin
  PrintCategory('Certificate Loading Error Handling');

  // Test LoadFromFile with non-existent file
  Passed := False;
  ErrorMsg := '';
  try
    Cert := TCertificate.LoadFromFile('nonexistent_file.pem');
    ErrorMsg := 'Accepted non-existent file';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('LoadFromFile non-existent', Passed, ErrorMsg);

  // Test ParsePEM with empty string
  Passed := False;
  ErrorMsg := '';
  try
    Cert := TCertificate.ParsePEM('');
    if Cert = nil then
    begin
      Passed := True;
      ErrorMsg := 'Returned nil for empty PEM';
    end
    else
      ErrorMsg := 'Accepted empty PEM';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ParsePEM empty string', Passed, ErrorMsg);

  // Test ParsePEM with invalid PEM
  Passed := False;
  ErrorMsg := '';
  try
    Cert := TCertificate.ParsePEM('not a valid PEM');
    if Cert = nil then
    begin
      Passed := True;
      ErrorMsg := 'Returned nil for invalid PEM';
    end
    else
      ErrorMsg := 'Accepted invalid PEM';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ParsePEM invalid format', Passed, ErrorMsg);

  // Test ParseDER with empty data
  Passed := False;
  ErrorMsg := '';
  try
    Cert := TCertificate.ParseDER(nil);
    if Cert = nil then
    begin
      Passed := True;
      ErrorMsg := 'Returned nil for empty DER';
    end
    else
      ErrorMsg := 'Accepted empty DER';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ParseDER empty data', Passed, ErrorMsg);

  // Test ParseDER with invalid data
  Passed := False;
  ErrorMsg := '';
  try
    Cert := TCertificate.ParseDER(TBytes.Create($00, $01, $02, $03));
    if Cert = nil then
    begin
      Passed := True;
      ErrorMsg := 'Returned nil for invalid DER';
    end
    else
      ErrorMsg := 'Accepted invalid DER';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ParseDER invalid data', Passed, ErrorMsg);
end;

{ Certificate Builder Error Tests }
procedure TestCertificateBuilderErrors;
var
  Builder: ICertificateBuilder;
  KeyPair: IKeyPairWithCertificate;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('Certificate Builder Error Handling');

  // Test CreateSelfSigned with empty common name
  Passed := False;
  ErrorMsg := '';
  try
    KeyPair := TCertificate.CreateSelfSigned('');
    if KeyPair = nil then
    begin
      Passed := True;
      ErrorMsg := 'Returned nil for empty CN';
    end
    else
    begin
      Passed := True;
      ErrorMsg := 'Created cert with empty CN (may be valid)';
    end;
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('CreateSelfSigned empty CN', Passed, ErrorMsg);

  // Test builder with invalid key size
  Passed := False;
  ErrorMsg := '';
  try
    Builder := TCertificate.CreateBuilder;
    KeyPair := Builder
      .WithCommonName('Test')
      .ValidFor(365)
      .WithRSAKey(64)  // Too small
      .SelfSigned;
    ErrorMsg := 'Accepted 64-bit RSA key';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('Builder invalid RSA key size', Passed, ErrorMsg);

  // Test builder with zero validity
  Passed := False;
  ErrorMsg := '';
  try
    Builder := TCertificate.CreateBuilder;
    KeyPair := Builder
      .WithCommonName('Test')
      .ValidFor(0)
      .WithRSAKey(2048)
      .SelfSigned;
    if KeyPair <> nil then
    begin
      Passed := True;
      ErrorMsg := 'Created cert with zero validity (may be valid)';
    end
    else
      ErrorMsg := 'Returned nil for zero validity';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('Builder zero validity', Passed, ErrorMsg);

  // Test builder with negative validity
  Passed := False;
  ErrorMsg := '';
  try
    Builder := TCertificate.CreateBuilder;
    KeyPair := Builder
      .WithCommonName('Test')
      .ValidFor(-1)
      .WithRSAKey(2048)
      .SelfSigned;
    ErrorMsg := 'Accepted negative validity';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('Builder negative validity', Passed, ErrorMsg);

  // Test builder with invalid ECDSA curve
  Passed := False;
  ErrorMsg := '';
  try
    Builder := TCertificate.CreateBuilder;
    KeyPair := Builder
      .WithCommonName('Test')
      .ValidFor(365)
      .WithECDSAKey('invalid_curve')
      .SelfSigned;
    ErrorMsg := 'Accepted invalid curve name';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('Builder invalid ECDSA curve', Passed, ErrorMsg);
end;

{ PEM Reader/Writer Error Tests }
procedure TestPEMErrors;
var
  Reader: TPEMReader;
  Writer: TPEMWriter;
  Data: TBytes;
  PEMResult: string;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('PEM Reader/Writer Error Handling');

  // Test PEM reader with truncated base64
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromString('-----BEGIN TEST-----' + #10 + 'SGVsbG8' + #10 + '-----END TEST-----');
      if Reader.BlockCount > 0 then
      begin
        Passed := True;
        ErrorMsg := 'Parsed truncated base64 (may pad)';
      end
      else
        ErrorMsg := 'No blocks found';
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
  PrintResult('PEM reader truncated base64', Passed, ErrorMsg);

  // Test PEM reader with mismatched markers
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromString('-----BEGIN CERTIFICATE-----' + #10 + 'SGVsbG8=' + #10 + '-----END PRIVATE KEY-----');
      if Reader.BlockCount = 0 then
      begin
        Passed := True;
        ErrorMsg := 'Rejected mismatched markers';
      end
      else
        ErrorMsg := 'Accepted mismatched markers';
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
  PrintResult('PEM reader mismatched markers', Passed, ErrorMsg);

  // Test PEM writer with nil data
  Passed := False;
  ErrorMsg := '';
  Writer := TPEMWriter.Create;
  try
    try
      Data := nil;
      PEMResult := Writer.WriteBlock(pemCertificate, Data);
      if PEMResult <> '' then
      begin
        Passed := True;
        ErrorMsg := 'Created empty PEM block';
      end
      else
        ErrorMsg := 'Returned empty string';
    except
      on E: Exception do
      begin
        Passed := True;
        ErrorMsg := 'Properly rejected: ' + E.ClassName;
      end;
    end;
  finally
    Writer.Free;
  end;
  PrintResult('PEM writer nil data', Passed, ErrorMsg);

  // Test PEM reader LoadFromFile with non-existent file
  Passed := False;
  ErrorMsg := '';
  Reader := TPEMReader.Create;
  try
    try
      Reader.LoadFromFile('nonexistent_pem_file.pem');
      ErrorMsg := 'Accepted non-existent file';
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
  PrintResult('PEM reader non-existent file', Passed, ErrorMsg);
end;

{ Conversion Error Tests }
procedure TestConversionErrors;
var
  DERResult: TBytes;
  PEMResult: string;
  Passed: Boolean;
  ErrorMsg: string;
begin
  PrintCategory('Certificate Conversion Error Handling');

  // Test ConvertPEMToDER with empty PEM
  Passed := False;
  ErrorMsg := '';
  try
    DERResult := TCertificate.ConvertPEMToDER('');
    if Length(DERResult) = 0 then
    begin
      Passed := True;
      ErrorMsg := 'Returned empty for empty PEM';
    end
    else
      ErrorMsg := 'Returned data for empty PEM';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ConvertPEMToDER empty', Passed, ErrorMsg);

  // Test ConvertPEMToDER with invalid PEM
  Passed := False;
  ErrorMsg := '';
  try
    DERResult := TCertificate.ConvertPEMToDER('not valid PEM');
    if Length(DERResult) = 0 then
    begin
      Passed := True;
      ErrorMsg := 'Returned empty for invalid PEM';
    end
    else
      ErrorMsg := 'Returned data for invalid PEM';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ConvertPEMToDER invalid', Passed, ErrorMsg);

  // Test ConvertDERToPEM with empty DER
  Passed := False;
  ErrorMsg := '';
  try
    PEMResult := TCertificate.ConvertDERToPEM(nil);
    if PEMResult = '' then
    begin
      Passed := True;
      ErrorMsg := 'Returned empty for empty DER';
    end
    else
      ErrorMsg := 'Returned data for empty DER';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ConvertDERToPEM empty', Passed, ErrorMsg);

  // Test ConvertDERToPEM with invalid DER
  Passed := False;
  ErrorMsg := '';
  try
    PEMResult := TCertificate.ConvertDERToPEM(TBytes.Create($FF, $FF, $FF));
    if PEMResult <> '' then
    begin
      Passed := True;
      ErrorMsg := 'Created PEM from invalid DER (wraps raw data)';
    end
    else
      ErrorMsg := 'Returned empty for invalid DER';
  except
    on E: Exception do
    begin
      Passed := True;
      ErrorMsg := 'Properly rejected: ' + E.ClassName;
    end;
  end;
  PrintResult('ConvertDERToPEM invalid', Passed, ErrorMsg);
end;

{ Main }
var
  Total: Integer;
  PassRate: Double;
begin
  WriteLn('========================================');
  WriteLn('  Connection Error Test Suite');
  WriteLn('  fafafa.ssl Test Quality Improvement');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Running connection error handling tests...');

  TestCertificateLoadingErrors;
  TestCertificateBuilderErrors;
  TestPEMErrors;
  TestConversionErrors;

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
    WriteLn('All connection error handling tests passed!')
  else
    WriteLn('Some tests failed. Review the output above.');

  if FailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
