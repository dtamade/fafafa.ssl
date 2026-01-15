{
  test_utils.pas - fafafa.ssl.utils 模块单元测试

  测试 SSL 工具函数：PEM/DER 转换、网络工具、错误格式化
  
  Part of test-quality-improvement Phase 3 Task 10
}

program test_utils;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.utils,
  fafafa.ssl.encoding;

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
// PEM/DER Format Detection Tests
// ============================================================================

procedure TestIsPEMFormat;
const
  ValidPEM = '-----BEGIN CERTIFICATE-----'#10'MIIB...'#10'-----END CERTIFICATE-----';
  InvalidPEM = 'Not a PEM format';
begin
  WriteLn;
  WriteLn('=== IsPEMFormat Tests ===');
  
  Check('Valid PEM detected', TSSLUtils.IsPEMFormat(ValidPEM));
  Check('Invalid PEM rejected', not TSSLUtils.IsPEMFormat(InvalidPEM));
  Check('Empty string rejected', not TSSLUtils.IsPEMFormat(''));
  Check('Partial PEM rejected', not TSSLUtils.IsPEMFormat('-----BEGIN CERTIFICATE-----'));
end;

procedure TestIsDERFormat;
var
  ValidDER, InvalidDER: TBytes;
begin
  WriteLn;
  WriteLn('=== IsDERFormat Tests ===');
  
  // DER starts with 0x30 (SEQUENCE)
  SetLength(ValidDER, 10);
  ValidDER[0] := $30;
  ValidDER[1] := $08;
  
  SetLength(InvalidDER, 10);
  InvalidDER[0] := $00;
  
  Check('Valid DER detected', TSSLUtils.IsDERFormat(ValidDER));
  Check('Invalid DER rejected', not TSSLUtils.IsDERFormat(InvalidDER));
  
  SetLength(InvalidDER, 0);
  Check('Empty data rejected', not TSSLUtils.IsDERFormat(InvalidDER));
end;

// ============================================================================
// PEM/DER Conversion Tests
// ============================================================================

procedure TestPEMToDER;
const
  // Simple test PEM with base64 data
  TestPEM = '-----BEGIN CERTIFICATE-----'#10 +
            'SGVsbG8gV29ybGQ='#10 +  // "Hello World" in base64
            '-----END CERTIFICATE-----';
var
  DER: TBytes;
begin
  WriteLn;
  WriteLn('=== PEMToDER Tests ===');
  
  DER := TSSLUtils.PEMToDER(TestPEM);
  Check('PEMToDER produces output', Length(DER) > 0);
  Check('PEMToDER correct length', Length(DER) = 11);  // "Hello World" = 11 bytes
  
  // Empty PEM
  DER := TSSLUtils.PEMToDER('');
  Check('Empty PEM returns empty', Length(DER) = 0);
  
  // Invalid PEM
  DER := TSSLUtils.PEMToDER('Not a PEM');
  Check('Invalid PEM returns empty', Length(DER) = 0);
end;

procedure TestDERToPEM;
var
  DER: TBytes;
  PEM: string;
begin
  WriteLn;
  WriteLn('=== DERToPEM Tests ===');
  
  // Create test DER data
  SetLength(DER, 5);
  DER[0] := $01; DER[1] := $02; DER[2] := $03; DER[3] := $04; DER[4] := $05;
  
  PEM := TSSLUtils.DERToPEM(DER, 'CERTIFICATE');
  Check('DERToPEM produces output', Length(PEM) > 0);
  Check('DERToPEM has BEGIN marker', Pos('-----BEGIN CERTIFICATE-----', PEM) > 0);
  Check('DERToPEM has END marker', Pos('-----END CERTIFICATE-----', PEM) > 0);
  
  // Empty DER
  SetLength(DER, 0);
  PEM := TSSLUtils.DERToPEM(DER);
  Check('Empty DER returns empty', PEM = '');
end;

procedure TestExtractPEMBlock;
const
  MultiBlockPEM = '-----BEGIN CERTIFICATE-----'#10 +
                  'CERT_DATA'#10 +
                  '-----END CERTIFICATE-----'#10 +
                  '-----BEGIN PRIVATE KEY-----'#10 +
                  'KEY_DATA'#10 +
                  '-----END PRIVATE KEY-----';
var
  Block: string;
begin
  WriteLn;
  WriteLn('=== ExtractPEMBlock Tests ===');
  
  Block := TSSLUtils.ExtractPEMBlock(MultiBlockPEM, 'CERTIFICATE');
  Check('Extract CERTIFICATE block', Pos('CERT_DATA', Block) > 0);
  Check('CERTIFICATE block has markers', 
        (Pos('BEGIN CERTIFICATE', Block) > 0) and (Pos('END CERTIFICATE', Block) > 0));
  
  Block := TSSLUtils.ExtractPEMBlock(MultiBlockPEM, 'PRIVATE KEY');
  Check('Extract PRIVATE KEY block', Pos('KEY_DATA', Block) > 0);
  
  Block := TSSLUtils.ExtractPEMBlock(MultiBlockPEM, 'NONEXISTENT');
  Check('Nonexistent block returns empty', Block = '');
end;

// ============================================================================
// IP Address Tests
// ============================================================================

procedure TestIsIPv4Address;
begin
  WriteLn;
  WriteLn('=== IsIPv4Address Tests ===');
  
  Check('Valid IPv4 192.168.1.1', TSSLUtils.IsIPv4Address('192.168.1.1'));
  Check('Valid IPv4 0.0.0.0', TSSLUtils.IsIPv4Address('0.0.0.0'));
  Check('Valid IPv4 255.255.255.255', TSSLUtils.IsIPv4Address('255.255.255.255'));
  Check('Valid IPv4 127.0.0.1', TSSLUtils.IsIPv4Address('127.0.0.1'));
  
  Check('Invalid: too few octets', not TSSLUtils.IsIPv4Address('192.168.1'));
  Check('Invalid: too many octets', not TSSLUtils.IsIPv4Address('192.168.1.1.1'));
  Check('Invalid: octet > 255', not TSSLUtils.IsIPv4Address('192.168.1.256'));
  Check('Invalid: negative octet', not TSSLUtils.IsIPv4Address('192.168.-1.1'));
  Check('Invalid: non-numeric', not TSSLUtils.IsIPv4Address('192.168.1.abc'));
  Check('Invalid: empty', not TSSLUtils.IsIPv4Address(''));
  Check('Invalid: hostname', not TSSLUtils.IsIPv4Address('example.com'));
end;

procedure TestIsIPv6Address;
begin
  WriteLn;
  WriteLn('=== IsIPv6Address Tests ===');
  
  Check('Valid IPv6 full', TSSLUtils.IsIPv6Address('2001:0db8:85a3:0000:0000:8a2e:0370:7334'));
  Check('Valid IPv6 compressed', TSSLUtils.IsIPv6Address('2001:db8::1'));
  Check('Valid IPv6 loopback', TSSLUtils.IsIPv6Address('::1'));
  Check('Valid IPv6 all zeros', TSSLUtils.IsIPv6Address('::'));
  
  Check('Invalid: IPv4', not TSSLUtils.IsIPv6Address('192.168.1.1'));
  Check('Invalid: hostname', not TSSLUtils.IsIPv6Address('example.com'));
  Check('Invalid: empty', not TSSLUtils.IsIPv6Address(''));
end;

procedure TestIsIPAddress;
begin
  WriteLn;
  WriteLn('=== IsIPAddress Tests ===');
  
  Check('IPv4 is IP', TSSLUtils.IsIPAddress('192.168.1.1'));
  Check('IPv6 is IP', TSSLUtils.IsIPAddress('::1'));
  Check('Hostname is not IP', not TSSLUtils.IsIPAddress('example.com'));
end;

// ============================================================================
// Hostname Validation Tests
// ============================================================================

procedure TestIsValidHostname;
begin
  WriteLn;
  WriteLn('=== IsValidHostname Tests ===');
  
  Check('Valid: example.com', TSSLUtils.IsValidHostname('example.com'));
  Check('Valid: sub.example.com', TSSLUtils.IsValidHostname('sub.example.com'));
  Check('Valid: localhost', TSSLUtils.IsValidHostname('localhost'));
  Check('Valid: with hyphen', TSSLUtils.IsValidHostname('my-server.example.com'));
  Check('Valid: with numbers', TSSLUtils.IsValidHostname('server1.example.com'));
  
  Check('Invalid: empty', not TSSLUtils.IsValidHostname(''));
  Check('Invalid: underscore', not TSSLUtils.IsValidHostname('my_server.com'));
  Check('Invalid: space', not TSSLUtils.IsValidHostname('my server.com'));
  Check('Invalid: special chars', not TSSLUtils.IsValidHostname('server@example.com'));
end;

procedure TestNormalizeHostname;
begin
  WriteLn;
  WriteLn('=== NormalizeHostname Tests ===');
  
  Check('Lowercase conversion', TSSLUtils.NormalizeHostname('EXAMPLE.COM') = 'example.com');
  Check('Trim whitespace', TSSLUtils.NormalizeHostname('  example.com  ') = 'example.com');
  Check('Mixed case', TSSLUtils.NormalizeHostname('ExAmPlE.CoM') = 'example.com');
end;

// ============================================================================
// URL Parsing Tests
// ============================================================================

procedure TestParseURL;
var
  Protocol, Host, Path: string;
  Port: Integer;
  Success: Boolean;
begin
  WriteLn;
  WriteLn('=== ParseURL Tests ===');
  
  // Full HTTPS URL
  Success := TSSLUtils.ParseURL('https://example.com:8443/path/to/resource', 
                                 Protocol, Host, Port, Path);
  Check('Parse full HTTPS URL', Success);
  Check('Protocol is https', Protocol = 'https');
  Check('Host is example.com', Host = 'example.com');
  Check('Port is 8443', Port = 8443);
  Check('Path is /path/to/resource', Path = '/path/to/resource');
  
  // Default HTTPS port
  Success := TSSLUtils.ParseURL('https://example.com/page', 
                                 Protocol, Host, Port, Path);
  Check('Parse URL with default port', Success);
  Check('Default HTTPS port is 443', Port = 443);
  
  // HTTP URL
  Success := TSSLUtils.ParseURL('http://example.com/', 
                                 Protocol, Host, Port, Path);
  Check('Parse HTTP URL', Success);
  Check('Protocol is http', Protocol = 'http');
  Check('Default HTTP port is 80', Port = 80);
  
  // URL without protocol
  Success := TSSLUtils.ParseURL('example.com/page', 
                                 Protocol, Host, Port, Path);
  Check('Parse URL without protocol', Success);
  Check('Default protocol is https', Protocol = 'https');
  
  // URL without path
  Success := TSSLUtils.ParseURL('https://example.com', 
                                 Protocol, Host, Port, Path);
  Check('Parse URL without path', Success);
  Check('Default path is /', Path = '/');
end;

// ============================================================================
// Certificate Subject Formatting Tests
// ============================================================================

procedure TestFormatCertificateSubject;
var
  Formatted: string;
begin
  WriteLn;
  WriteLn('=== FormatCertificateSubject Tests ===');
  
  Formatted := TSSLUtils.FormatCertificateSubject('CN=example.com,O=Example Inc,C=US');
  Check('Format comma-separated DN', Length(Formatted) > 0);
  Check('Contains CN', Pos('CN=example.com', Formatted) > 0);
  
  Formatted := TSSLUtils.FormatCertificateSubject('/CN=example.com/O=Example Inc/C=US');
  Check('Format slash-separated DN', Length(Formatted) > 0);
end;

procedure TestParseDistinguishedName;
var
  Parts: TStringList;
begin
  WriteLn;
  WriteLn('=== ParseDistinguishedName Tests ===');
  
  Parts := TSSLUtils.ParseDistinguishedName('CN=example.com,O=Example Inc,C=US');
  try
    Check('Parse DN parts count', Parts.Count = 3);
    Check('First part is CN', Pos('CN=', Parts[0]) > 0);
  finally
    Parts.Free;
  end;
  
  Parts := TSSLUtils.ParseDistinguishedName('');
  try
    Check('Empty DN returns empty list', Parts.Count = 0);
  finally
    Parts.Free;
  end;
end;

// ============================================================================
// Error Formatting Tests
// ============================================================================

procedure TestFormatSSLError;
var
  Formatted: string;
begin
  WriteLn;
  WriteLn('=== FormatSSLError Tests ===');
  
  Formatted := TSSLUtils.FormatSSLError(sslErrNone);
  Check('Format sslErrNone', Length(Formatted) > 0);
  
  Formatted := TSSLUtils.FormatSSLError(sslErrCertificate, 'Loading certificate');
  Check('Format with context', Pos('Loading certificate', Formatted) > 0);
  
  Formatted := TSSLUtils.FormatSSLError(sslErrTimeout);
  Check('Format sslErrTimeout', Length(Formatted) > 0);
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
  WriteLn('fafafa.ssl.utils Unit Tests');
  WriteLn('===========================');
  
  // PEM/DER format detection
  TestIsPEMFormat;
  TestIsDERFormat;
  
  // PEM/DER conversion
  TestPEMToDER;
  TestDERToPEM;
  TestExtractPEMBlock;
  
  // IP address validation
  TestIsIPv4Address;
  TestIsIPv6Address;
  TestIsIPAddress;
  
  // Hostname validation
  TestIsValidHostname;
  TestNormalizeHostname;
  
  // URL parsing
  TestParseURL;
  
  // Certificate subject formatting
  TestFormatCertificateSubject;
  TestParseDistinguishedName;
  
  // Error formatting
  TestFormatSSLError;
  
  PrintSummary;
  
  if GFailCount > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
