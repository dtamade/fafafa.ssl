program test_x509_basic;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, ctypes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.asn1,
  DynLibs;

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

function CreateTestKey: PEVP_PKEY;
var
  rsa: PRSA;
  exp: PBIGNUM;
  pkey: PEVP_PKEY;
begin
  Result := nil;
  rsa := RSA_new();
  if rsa = nil then Exit;
  
  exp := BN_new();
  if exp = nil then
  begin
    RSA_free(rsa);
    Exit;
  end;
  
  try
    if BN_set_word(exp, RSA_F4) = 1 then
    begin
      if RSA_generate_key_ex(rsa, 2048, exp, nil) = 1 then
      begin
        pkey := EVP_PKEY_new();
        if pkey <> nil then
        begin
          if EVP_PKEY_assign(pkey, EVP_PKEY_RSA, rsa) = 1 then
            Result := pkey
          else
            EVP_PKEY_free(pkey);
        end;
      end;
    end;
  finally
    BN_free(exp);
    if Result = nil then
      RSA_free(rsa);
  end;
end;

procedure TestX509Creation;
var
  cert: PX509;
begin
  WriteLn;
  WriteLn('=== X.509 Certificate Creation Tests ===');
  WriteLn;
  
  // Test 1: Create new certificate
  cert := X509_new();
  LogTest('Create X.509 certificate', cert <> nil);
  
  if cert <> nil then
  begin
    // Test 2: Set version (X.509 v3 = version 2)
    LogTest('Set certificate version',
            X509_set_version(cert, 2) = 1,
            'Version 3 (value=2)');
    
    X509_free(cert);
  end;
end;

procedure TestX509Names;
var
  name: PX509_NAME;
  entry: PX509_NAME_ENTRY;
  entry_count: Integer;
  text: array[0..255] of AnsiChar;
  text_len: Integer;
begin
  WriteLn;
  WriteLn('=== X.509 Name Tests ===');
  WriteLn;
  
  // Test 3: Create name
  name := X509_NAME_new();
  LogTest('Create X.509_NAME', name <> nil);
  
  if name <> nil then
  begin
    // Test 4: Add Country Name
    LogTest('Add Country Name (C)',
            X509_NAME_add_entry_by_txt(name, 'C', MBSTRING_ASC,
                                       PByte(PAnsiChar('US')), 2, -1, 0) = 1);
    
    // Test 5: Add Organization Name
    LogTest('Add Organization (O)',
            X509_NAME_add_entry_by_txt(name, 'O', MBSTRING_ASC,
                                       PByte(PAnsiChar('Test Org')), 8, -1, 0) = 1);
    
    // Test 6: Add Common Name
    LogTest('Add Common Name (CN)',
            X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC,
                                       PByte(PAnsiChar('test.example.com')), 16, -1, 0) = 1);
    
    // Test 7: Check entry count
    entry_count := X509_NAME_entry_count(name);
    LogTest('Verify entry count', entry_count = 3,
            Format('Expected 3, got %d', [entry_count]));
    
    // Test 8: Get entry by index
    entry := X509_NAME_get_entry(name, 2);  // CN should be at index 2
    LogTest('Get entry by index', entry <> nil,
            'Retrieved CN entry');
    
    // Test 9: Get text by NID
    text_len := X509_NAME_get_text_by_NID(name, NID_commonName, @text[0], 256);
    LogTest('Get CN text by NID', text_len > 0,
            Format('CN text length: %d', [text_len]));
    
    X509_NAME_free(name);
  end;
end;

procedure TestX509BasicFields;
var
  cert: PX509;
  pkey: PEVP_PKEY;
  name: PX509_NAME;
  serial: PASN1_INTEGER;
  notBefore, notAfter: PASN1_TIME;
  retrieved_name: PX509_NAME;
  oneline: PAnsiChar;
  version: clong;
begin
  WriteLn;
  WriteLn('=== X.509 Basic Fields Tests ===');
  WriteLn;
  
  cert := X509_new();
  if cert = nil then
  begin
    LogTest('Create certificate for field tests', False);
    Exit;
  end;
  
  try
    // Test 10: Set serial number
    serial := ASN1_INTEGER_new();
    if serial <> nil then
    begin
      ASN1_INTEGER_set(serial, 12345);
      LogTest('Set serial number', X509_set_serialNumber(cert, serial) = 1);
      ASN1_INTEGER_free(serial);
    end;
    
    // Test 11: Get and verify serial number
    serial := X509_get_serialNumber(cert);
    LogTest('Get serial number', 
            (serial <> nil) and (ASN1_INTEGER_get(serial) = 12345),
            Format('Serial: %d', [ASN1_INTEGER_get(serial)]));
    
    // Test 12: Create and set subject name
    name := X509_NAME_new();
    if name <> nil then
    begin
      X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC,
                                 PByte(PAnsiChar('Test Subject')), 12, -1, 0);
      LogTest('Set subject name', X509_set_subject_name(cert, name) = 1);
      X509_NAME_free(name);
    end;
    
    // Test 13: Create and set issuer name
    name := X509_NAME_new();
    if name <> nil then
    begin
      X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC,
                                 PByte(PAnsiChar('Test Issuer')), 11, -1, 0);
      LogTest('Set issuer name', X509_set_issuer_name(cert, name) = 1);
      X509_NAME_free(name);
    end;
    
    // Test 14: Get subject name
    retrieved_name := X509_get_subject_name(cert);
    LogTest('Get subject name', retrieved_name <> nil);
    if retrieved_name <> nil then
    begin
      oneline := X509_NAME_oneline(retrieved_name, nil, 0);
      if oneline <> nil then
      begin
        WriteLn('  Subject: ', oneline);
        CRYPTO_free(oneline, nil, 0);
      end;
    end;
    
    // Test 15: Get issuer name
    retrieved_name := X509_get_issuer_name(cert);
    LogTest('Get issuer name', retrieved_name <> nil);
    if retrieved_name <> nil then
    begin
      oneline := X509_NAME_oneline(retrieved_name, nil, 0);
      if oneline <> nil then
      begin
        WriteLn('  Issuer: ', oneline);
        CRYPTO_free(oneline, nil, 0);
      end;
    end;
    
    // Test 16: Set validity period (not before/after)
    notBefore := ASN1_TIME_new();
    notAfter := ASN1_TIME_new();
    if (notBefore <> nil) and (notAfter <> nil) then
    begin
      X509_gmtime_adj(notBefore, 0);  // Now
      X509_gmtime_adj(notAfter, 365 * 24 * 3600);  // +1 year
      LogTest('Set validity period',
              (X509_set1_notBefore(cert, notBefore) = 1) and
              (X509_set1_notAfter(cert, notAfter) = 1),
              'Valid for 1 year from now');
      ASN1_TIME_free(notBefore);
      ASN1_TIME_free(notAfter);
    end;
    
    // Test 17: Set public key
    pkey := CreateTestKey();
    if pkey <> nil then
    begin
      LogTest('Set public key', X509_set_pubkey(cert, pkey) = 1);
      EVP_PKEY_free(pkey);
    end
    else
      LogTest('Create test key', False);
    
    // Test 18: Get version
    version := X509_get_version(cert);
    LogTest('Get certificate version', version = 2,
            Format('Version: %d (X.509 v3)', [version]));
    
  finally
    X509_free(cert);
  end;
end;

procedure TestX509SelfSignedCert;
var
  cert: PX509;
  pkey: PEVP_PKEY;
  name: PX509_NAME;
  serial: PASN1_INTEGER;
  notBefore, notAfter: PASN1_TIME;
  md: PEVP_MD;
  verify_result: Integer;
begin
  WriteLn;
  WriteLn('=== X.509 Self-Signed Certificate Tests ===');
  WriteLn;
  
  // Create key pair
  pkey := CreateTestKey();
  if pkey = nil then
  begin
    LogTest('Create key pair for self-signed cert', False);
    Exit;
  end;
  
  cert := X509_new();
  if cert = nil then
  begin
    EVP_PKEY_free(pkey);
    LogTest('Create certificate for self-signed test', False);
    Exit;
  end;
  
  try
    // Set up certificate
    X509_set_version(cert, 2);
    
    serial := ASN1_INTEGER_new();
    ASN1_INTEGER_set(serial, 1);
    X509_set_serialNumber(cert, serial);
    ASN1_INTEGER_free(serial);
    
    name := X509_NAME_new();
    X509_NAME_add_entry_by_txt(name, 'C', MBSTRING_ASC, PByte(PAnsiChar('US')), 2, -1, 0);
    X509_NAME_add_entry_by_txt(name, 'O', MBSTRING_ASC, PByte(PAnsiChar('Test')), 4, -1, 0);
    X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC,
                               PByte(PAnsiChar('test.example.com')), 16, -1, 0);
    
    X509_set_issuer_name(cert, name);
    X509_set_subject_name(cert, name);
    X509_NAME_free(name);
    
    notBefore := ASN1_TIME_new();
    notAfter := ASN1_TIME_new();
    X509_gmtime_adj(notBefore, 0);
    X509_gmtime_adj(notAfter, 365 * 24 * 3600);
    X509_set1_notBefore(cert, notBefore);
    X509_set1_notAfter(cert, notAfter);
    ASN1_TIME_free(notBefore);
    ASN1_TIME_free(notAfter);
    
    X509_set_pubkey(cert, pkey);
    
    // Test 19: Sign certificate
    md := EVP_sha256();
    LogTest('Sign certificate with SHA-256',
            X509_sign(cert, pkey, md) > 0,
            'Self-signed certificate created');
    
    // Test 20: Verify self-signed certificate
    verify_result := X509_verify(cert, pkey);
    LogTest('Verify self-signed certificate', verify_result = 1,
            Format('Verification result: %d', [verify_result]));
    
    // Test 21: Check that cert matches private key
    LogTest('Check private key matches certificate',
            X509_check_private_key(cert, pkey) = 1,
            'Private key and certificate match');
    
  finally
    X509_free(cert);
    EVP_PKEY_free(pkey);
  end;
end;

begin
  WriteLn('X.509 Certificate Basic Tests');
  WriteLn('==============================');
  WriteLn;

  // Load OpenSSL core first
  LoadOpenSSLCore;
  if not IsOpenSSLCoreLoaded then
  begin
    WriteLn('ERROR: Failed to load OpenSSL core library');
    Halt(1);
  end;

  // Load required modules
  LoadOpenSSLX509;
  LoadOpenSSLASN1(GetCryptoLibHandle);
  LoadEVP(GetCryptoLibHandle);
  LoadOpenSSLRSA;
  LoadOpenSSLBN;
  LoadOpenSSLCrypto;

  try
    // Run test suites
    TestX509Creation;         // Tests 1-2
    TestX509Names;            // Tests 3-9
    TestX509BasicFields;      // Tests 10-18
    TestX509SelfSignedCert;   // Tests 19-21
    
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
    
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.Message);
      Halt(1);
    end;
  end;
end.
