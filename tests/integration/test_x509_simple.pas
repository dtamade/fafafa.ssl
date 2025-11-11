program test_x509_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.api.core;

type
  { X.509 Basic Function Types }
  TX509_new = function: PX509; cdecl;
  TX509_free = procedure(x: PX509); cdecl;
  TX509_set_version = function(x: PX509; version: clong): Integer; cdecl;
  TX509_get_version = function(const x: PX509): clong; cdecl;
  TX509_set_serialNumber = function(x: PX509; serial: PASN1_INTEGER): Integer; cdecl;
  TX509_get_serialNumber = function(x: PX509): PASN1_INTEGER; cdecl;
  TX509_set_issuer_name = function(x: PX509; name: PX509_NAME): Integer; cdecl;
  TX509_get_issuer_name = function(const a: PX509): PX509_NAME; cdecl;
  TX509_set_subject_name = function(x: PX509; name: PX509_NAME): Integer; cdecl;
  TX509_get_subject_name = function(const a: PX509): PX509_NAME; cdecl;
  TX509_set_pubkey = function(x: PX509; pkey: PEVP_PKEY): Integer; cdecl;
  TX509_get_pubkey = function(x: PX509): PEVP_PKEY; cdecl;
  TX509_sign = function(x: PX509; pkey: PEVP_PKEY; const md: PEVP_MD): Integer; cdecl;
  TX509_verify = function(a: PX509; r: PEVP_PKEY): Integer; cdecl;
  TX509_dup = function(x: PX509): PX509; cdecl;
  TX509_cmp = function(const a: PX509; const b: PX509): Integer; cdecl;
  
  { X.509 Name Functions }
  TX509_NAME_new = function: PX509_NAME; cdecl;
  TX509_NAME_free = procedure(a: PX509_NAME); cdecl;
  TX509_NAME_add_entry_by_txt = function(name: PX509_NAME; const field: PAnsiChar; &type: Integer; const bytes: PByte; len: Integer; loc: Integer; &set: Integer): Integer; cdecl;
  TX509_NAME_oneline = function(const a: PX509_NAME; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl;
  
  { ASN.1 Functions }
  TASN1_INTEGER_new = function: PASN1_INTEGER; cdecl;
  TASN1_INTEGER_free = procedure(a: PASN1_INTEGER); cdecl;
  TASN1_INTEGER_set = function(a: PASN1_INTEGER; v: clong): Integer; cdecl;
  TASN1_INTEGER_get = function(const a: PASN1_INTEGER): clong; cdecl;
  
  { EVP Functions }
  TEVP_PKEY_new = function: PEVP_PKEY; cdecl;
  TEVP_PKEY_free = procedure(pkey: PEVP_PKEY); cdecl;
  TEVP_PKEY_assign = function(pkey: PEVP_PKEY; &type: Integer; key: Pointer): Integer; cdecl;
  
  { RSA Functions for key generation }
  TRSA_new = function: Pointer; cdecl;
  TRSA_free = procedure(r: Pointer); cdecl;
  TRSA_generate_key_ex = function(rsa: Pointer; bits: Integer; e: PBIGNUM; cb: Pointer): Integer; cdecl;
  
  { BIGNUM Functions }
  TBN_new = function: PBIGNUM; cdecl;
  TBN_free = procedure(a: PBIGNUM); cdecl;
  TBN_set_word = function(a: PBIGNUM; w: culong): Integer; cdecl;
  
  { EVP_MD Functions }
  TEVP_sha256 = function: PEVP_MD; cdecl;

var
  { Function pointers }
  X509_new: TX509_new = nil;
  X509_free: TX509_free = nil;
  X509_set_version: TX509_set_version = nil;
  X509_get_version: TX509_get_version = nil;
  X509_set_serialNumber: TX509_set_serialNumber = nil;
  X509_get_serialNumber: TX509_get_serialNumber = nil;
  X509_set_issuer_name: TX509_set_issuer_name = nil;
  X509_get_issuer_name: TX509_get_issuer_name = nil;
  X509_set_subject_name: TX509_set_subject_name = nil;
  X509_get_subject_name: TX509_get_subject_name = nil;
  X509_set_pubkey: TX509_set_pubkey = nil;
  X509_get_pubkey: TX509_get_pubkey = nil;
  X509_sign: TX509_sign = nil;
  X509_verify: TX509_verify = nil;
  X509_dup: TX509_dup = nil;
  X509_cmp: TX509_cmp = nil;
  
  X509_NAME_new: TX509_NAME_new = nil;
  X509_NAME_free: TX509_NAME_free = nil;
  X509_NAME_add_entry_by_txt: TX509_NAME_add_entry_by_txt = nil;
  X509_NAME_oneline: TX509_NAME_oneline = nil;
  
  ASN1_INTEGER_new: TASN1_INTEGER_new = nil;
  ASN1_INTEGER_free: TASN1_INTEGER_free = nil;
  ASN1_INTEGER_set: TASN1_INTEGER_set = nil;
  ASN1_INTEGER_get: TASN1_INTEGER_get = nil;
  
  EVP_PKEY_new: TEVP_PKEY_new = nil;
  EVP_PKEY_free: TEVP_PKEY_free = nil;
  EVP_PKEY_assign: TEVP_PKEY_assign = nil;
  
  RSA_new: TRSA_new = nil;
  RSA_free: TRSA_free = nil;
  RSA_generate_key_ex: TRSA_generate_key_ex = nil;
  
  BN_new: TBN_new = nil;
  BN_free: TBN_free = nil;
  BN_set_word: TBN_set_word = nil;
  
  EVP_sha256: TEVP_sha256 = nil;

function LoadX509Functions: Boolean;
var
  LibHandle: TLibHandle;
begin
  Result := False;
  LibHandle := GetCryptoLibHandle;
  if LibHandle = 0 then Exit;
  
  // Load X509 certificate functions
  X509_new := TX509_new(GetProcedureAddress(LibHandle, 'X509_new'));
  X509_free := TX509_free(GetProcedureAddress(LibHandle, 'X509_free'));
  X509_set_version := TX509_set_version(GetProcedureAddress(LibHandle, 'X509_set_version'));
  X509_get_version := TX509_get_version(GetProcedureAddress(LibHandle, 'X509_get_version'));
  X509_set_serialNumber := TX509_set_serialNumber(GetProcedureAddress(LibHandle, 'X509_set_serialNumber'));
  X509_get_serialNumber := TX509_get_serialNumber(GetProcedureAddress(LibHandle, 'X509_get_serialNumber'));
  X509_set_issuer_name := TX509_set_issuer_name(GetProcedureAddress(LibHandle, 'X509_set_issuer_name'));
  X509_get_issuer_name := TX509_get_issuer_name(GetProcedureAddress(LibHandle, 'X509_get_issuer_name'));
  X509_set_subject_name := TX509_set_subject_name(GetProcedureAddress(LibHandle, 'X509_set_subject_name'));
  X509_get_subject_name := TX509_get_subject_name(GetProcedureAddress(LibHandle, 'X509_get_subject_name'));
  X509_set_pubkey := TX509_set_pubkey(GetProcedureAddress(LibHandle, 'X509_set_pubkey'));
  X509_get_pubkey := TX509_get_pubkey(GetProcedureAddress(LibHandle, 'X509_get_pubkey'));
  X509_sign := TX509_sign(GetProcedureAddress(LibHandle, 'X509_sign'));
  X509_verify := TX509_verify(GetProcedureAddress(LibHandle, 'X509_verify'));
  X509_dup := TX509_dup(GetProcedureAddress(LibHandle, 'X509_dup'));
  X509_cmp := TX509_cmp(GetProcedureAddress(LibHandle, 'X509_cmp'));
  
  // Load X509_NAME functions
  X509_NAME_new := TX509_NAME_new(GetProcedureAddress(LibHandle, 'X509_NAME_new'));
  X509_NAME_free := TX509_NAME_free(GetProcedureAddress(LibHandle, 'X509_NAME_free'));
  X509_NAME_add_entry_by_txt := TX509_NAME_add_entry_by_txt(GetProcedureAddress(LibHandle, 'X509_NAME_add_entry_by_txt'));
  X509_NAME_oneline := TX509_NAME_oneline(GetProcedureAddress(LibHandle, 'X509_NAME_oneline'));
  
  // Load ASN1 functions
  ASN1_INTEGER_new := TASN1_INTEGER_new(GetProcedureAddress(LibHandle, 'ASN1_INTEGER_new'));
  ASN1_INTEGER_free := TASN1_INTEGER_free(GetProcedureAddress(LibHandle, 'ASN1_INTEGER_free'));
  ASN1_INTEGER_set := TASN1_INTEGER_set(GetProcedureAddress(LibHandle, 'ASN1_INTEGER_set'));
  ASN1_INTEGER_get := TASN1_INTEGER_get(GetProcedureAddress(LibHandle, 'ASN1_INTEGER_get'));
  
  // Load EVP functions
  EVP_PKEY_new := TEVP_PKEY_new(GetProcedureAddress(LibHandle, 'EVP_PKEY_new'));
  EVP_PKEY_free := TEVP_PKEY_free(GetProcedureAddress(LibHandle, 'EVP_PKEY_free'));
  EVP_PKEY_assign := TEVP_PKEY_assign(GetProcedureAddress(LibHandle, 'EVP_PKEY_assign'));
  
  // Load RSA functions
  RSA_new := TRSA_new(GetProcedureAddress(LibHandle, 'RSA_new'));
  RSA_free := TRSA_free(GetProcedureAddress(LibHandle, 'RSA_free'));
  RSA_generate_key_ex := TRSA_generate_key_ex(GetProcedureAddress(LibHandle, 'RSA_generate_key_ex'));
  
  // Load BIGNUM functions
  BN_new := TBN_new(GetProcedureAddress(LibHandle, 'BN_new'));
  BN_free := TBN_free(GetProcedureAddress(LibHandle, 'BN_free'));
  BN_set_word := TBN_set_word(GetProcedureAddress(LibHandle, 'BN_set_word'));
  
  // Load EVP_MD functions
  EVP_sha256 := TEVP_sha256(GetProcedureAddress(LibHandle, 'EVP_sha256'));
  
  Result := Assigned(X509_new) and Assigned(X509_free);
end;

procedure PrintTestHeader(const TestName: string);
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test: ', TestName);
  WriteLn('========================================');
end;

procedure PrintTestResult(const TestName: string; Success: Boolean);
begin
  if Success then
    WriteLn('[PASS] ', TestName)
  else
    WriteLn('[FAIL] ', TestName);
end;

// Test 1: X509 Certificate Creation and Destruction
function Test_X509_CreateFree: Boolean;
var
  cert: PX509;
begin
  PrintTestHeader('X509 Certificate Creation and Destruction');
  Result := False;
  
  Write('Creating X509 certificate... ');
  cert := X509_new();
  if cert = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Freeing X509 certificate... ');
  X509_free(cert);
  WriteLn('OK');
  
  Result := True;
  PrintTestResult('X509 Certificate Creation and Destruction', Result);
end;

// Test 2: X509 Version Operations
function Test_X509_Version: Boolean;
var
  cert: PX509;
  version: clong;
begin
  PrintTestHeader('X509 Version Operations');
  Result := False;
  
  cert := X509_new();
  if cert = nil then
  begin
    WriteLn('FAILED to create certificate');
    Exit;
  end;
  
  Write('Setting certificate version to 2 (X509v3)... ');
  if X509_set_version(cert, 2) <> 1 then
  begin
    WriteLn('FAILED');
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Getting certificate version... ');
  version := X509_get_version(cert);
  if version <> 2 then
  begin
    WriteLn('FAILED (expected 2, got ', version, ')');
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK (version=', version, ')');
  
  X509_free(cert);
  Result := True;
  PrintTestResult('X509 Version Operations', Result);
end;

// Test 3: X509 Serial Number Operations
function Test_X509_SerialNumber: Boolean;
var
  cert: PX509;
  serial: PASN1_INTEGER;
  retrieved_serial: PASN1_INTEGER;
  serial_value: clong;
begin
  PrintTestHeader('X509 Serial Number Operations');
  Result := False;
  
  cert := X509_new();
  if cert = nil then
  begin
    WriteLn('FAILED to create certificate');
    Exit;
  end;
  
  Write('Creating ASN1_INTEGER for serial number... ');
  serial := ASN1_INTEGER_new();
  if serial = nil then
  begin
    WriteLn('FAILED');
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting serial number value to 12345... ');
  if ASN1_INTEGER_set(serial, 12345) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(serial);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting certificate serial number... ');
  if X509_set_serialNumber(cert, serial) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(serial);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Getting certificate serial number... ');
  retrieved_serial := X509_get_serialNumber(cert);
  if retrieved_serial = nil then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(serial);
    X509_free(cert);
    Exit;
  end;
  
  serial_value := ASN1_INTEGER_get(retrieved_serial);
  if serial_value <> 12345 then
  begin
    WriteLn('FAILED (expected 12345, got ', serial_value, ')');
    ASN1_INTEGER_free(serial);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK (serial=', serial_value, ')');
  
  ASN1_INTEGER_free(serial);
  X509_free(cert);
  Result := True;
  PrintTestResult('X509 Serial Number Operations', Result);
end;

// Test 4: X509 Name Operations
function Test_X509_Name: Boolean;
const
  MBSTRING_UTF8 = $1000 or 1;
var
  name: PX509_NAME;
  buf: array[0..255] of AnsiChar;
  name_str: PAnsiChar;
begin
  PrintTestHeader('X509 Name Operations');
  Result := False;
  
  Write('Creating X509_NAME... ');
  name := X509_NAME_new();
  if name = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Adding CN (Common Name) entry... ');
  if X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_UTF8, PByte(PAnsiChar('Test Certificate')), -1, -1, 0) <> 1 then
  begin
    WriteLn('FAILED');
    X509_NAME_free(name);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Adding O (Organization) entry... ');
  if X509_NAME_add_entry_by_txt(name, 'O', MBSTRING_UTF8, PByte(PAnsiChar('Test Organization')), -1, -1, 0) <> 1 then
  begin
    WriteLn('FAILED');
    X509_NAME_free(name);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Converting name to string... ');
  name_str := X509_NAME_oneline(name, @buf[0], SizeOf(buf));
  if name_str = nil then
  begin
    WriteLn('FAILED');
    X509_NAME_free(name);
    Exit;
  end;
  WriteLn('OK');
  WriteLn('Name string: ', name_str);
  
  X509_NAME_free(name);
  Result := True;
  PrintTestResult('X509 Name Operations', Result);
end;

// Test 5: X509 Subject and Issuer Names
function Test_X509_SubjectIssuer: Boolean;
const
  MBSTRING_UTF8 = $1000 or 1;
var
  cert: PX509;
  subject, issuer: PX509_NAME;
  retrieved_subject, retrieved_issuer: PX509_NAME;
  buf: array[0..255] of AnsiChar;
begin
  PrintTestHeader('X509 Subject and Issuer Names');
  Result := False;
  
  cert := X509_new();
  if cert = nil then
  begin
    WriteLn('FAILED to create certificate');
    Exit;
  end;
  
  // Create subject name
  Write('Creating subject name... ');
  subject := X509_NAME_new();
  if subject = nil then
  begin
    WriteLn('FAILED');
    X509_free(cert);
    Exit;
  end;
  X509_NAME_add_entry_by_txt(subject, 'CN', MBSTRING_UTF8, PByte(PAnsiChar('Subject CN')), -1, -1, 0);
  WriteLn('OK');
  
  // Create issuer name
  Write('Creating issuer name... ');
  issuer := X509_NAME_new();
  if issuer = nil then
  begin
    WriteLn('FAILED');
    X509_NAME_free(subject);
    X509_free(cert);
    Exit;
  end;
  X509_NAME_add_entry_by_txt(issuer, 'CN', MBSTRING_UTF8, PByte(PAnsiChar('Issuer CN')), -1, -1, 0);
  WriteLn('OK');
  
  // Set subject and issuer
  Write('Setting subject name... ');
  if X509_set_subject_name(cert, subject) <> 1 then
  begin
    WriteLn('FAILED');
    X509_NAME_free(subject);
    X509_NAME_free(issuer);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting issuer name... ');
  if X509_set_issuer_name(cert, issuer) <> 1 then
  begin
    WriteLn('FAILED');
    X509_NAME_free(subject);
    X509_NAME_free(issuer);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  
  // Retrieve and verify
  Write('Retrieving subject name... ');
  retrieved_subject := X509_get_subject_name(cert);
  if retrieved_subject = nil then
  begin
    WriteLn('FAILED');
    X509_NAME_free(subject);
    X509_NAME_free(issuer);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  WriteLn('Subject: ', X509_NAME_oneline(retrieved_subject, @buf[0], SizeOf(buf)));
  
  Write('Retrieving issuer name... ');
  retrieved_issuer := X509_get_issuer_name(cert);
  if retrieved_issuer = nil then
  begin
    WriteLn('FAILED');
    X509_NAME_free(subject);
    X509_NAME_free(issuer);
    X509_free(cert);
    Exit;
  end;
  WriteLn('OK');
  WriteLn('Issuer: ', X509_NAME_oneline(retrieved_issuer, @buf[0], SizeOf(buf)));
  
  X509_NAME_free(subject);
  X509_NAME_free(issuer);
  X509_free(cert);
  Result := True;
  PrintTestResult('X509 Subject and Issuer Names', Result);
end;

// Test 6: X509 Certificate Duplication
function Test_X509_Dup: Boolean;
var
  cert1, cert2: PX509;
  serial: PASN1_INTEGER;
begin
  PrintTestHeader('X509 Certificate Duplication');
  Result := False;
  
  // Create and configure first certificate
  cert1 := X509_new();
  if cert1 = nil then
  begin
    WriteLn('FAILED to create certificate');
    Exit;
  end;
  
  X509_set_version(cert1, 2);
  serial := ASN1_INTEGER_new();
  ASN1_INTEGER_set(serial, 99999);
  X509_set_serialNumber(cert1, serial);
  ASN1_INTEGER_free(serial);
  
  // Duplicate certificate
  Write('Duplicating certificate... ');
  cert2 := X509_dup(cert1);
  if cert2 = nil then
  begin
    WriteLn('FAILED');
    X509_free(cert1);
    Exit;
  end;
  WriteLn('OK');
  
  // Verify duplication
  Write('Verifying duplicated certificate... ');
  if X509_get_version(cert2) <> 2 then
  begin
    WriteLn('FAILED (version mismatch)');
    X509_free(cert1);
    X509_free(cert2);
    Exit;
  end;
  
  if ASN1_INTEGER_get(X509_get_serialNumber(cert2)) <> 99999 then
  begin
    WriteLn('FAILED (serial number mismatch)');
    X509_free(cert1);
    X509_free(cert2);
    Exit;
  end;
  WriteLn('OK');
  
  // Compare certificates
  Write('Comparing certificates... ');
  if X509_cmp(cert1, cert2) <> 0 then
  begin
    WriteLn('FAILED (certificates differ)');
    X509_free(cert1);
    X509_free(cert2);
    Exit;
  end;
  WriteLn('OK (certificates are identical)');
  
  X509_free(cert1);
  X509_free(cert2);
  Result := True;
  PrintTestResult('X509 Certificate Duplication', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('X509 Module Integration Test');
  WriteLn('============================');
  WriteLn('');
  
  // Initialize OpenSSL Core
  Write('Initializing OpenSSL Core... ');
  try
    LoadOpenSSLCore;
    WriteLn('OK (', GetOpenSSLVersionString, ')');
  except
    on E: Exception do
    begin
      WriteLn('FAILED');
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  // Load X509 functions
  Write('Loading X509 functions... ');
  if not LoadX509Functions then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load X509 functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 6;
  
  try
    // Run all tests
    if Test_X509_CreateFree then Inc(PassedTests);
    if Test_X509_Version then Inc(PassedTests);
    if Test_X509_SerialNumber then Inc(PassedTests);
    if Test_X509_Name then Inc(PassedTests);
    if Test_X509_SubjectIssuer then Inc(PassedTests);
    if Test_X509_Dup then Inc(PassedTests);
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
    end;
  end;
  
  // Summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test Summary');
  WriteLn('========================================');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', TotalTests - PassedTests);
  WriteLn('Success rate: ', (PassedTests * 100) div TotalTests, '%');
  
  if PassedTests = TotalTests then
  begin
    WriteLn('');
    WriteLn('ALL TESTS PASSED!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('');
    WriteLn('SOME TESTS FAILED!');
    ExitCode := 1;
  end;
  
  UnloadOpenSSLCore;
end.
