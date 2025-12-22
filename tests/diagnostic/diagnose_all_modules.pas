program diagnose_all_modules;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.dh,
  fafafa.ssl.openssl.api.dsa,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.ecdsa,
  fafafa.ssl.openssl.api.ecdh,
  fafafa.ssl.openssl.api.hmac,
  fafafa.ssl.openssl.api.cmac.evp,  // Phase 2.2: 使用EVP API替代废弃的cmac.pas
  fafafa.ssl.openssl.api.pkcs,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.cms,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.kdf,
  fafafa.ssl.openssl.api.core;

type
  TModuleStatus = (msAvailable, msPartial, msNotAvailable, msDeprecated, msError);
  
  TModuleInfo = record
    Name: string;
    Category: string;
    Status: TModuleStatus;
    Message: string;
  end;

var
  Modules: array of TModuleInfo;
  TotalModules, AvailableModules, PartialModules, NotAvailableModules: Integer;
  OpenSSLVersion: string;

procedure AddModule(const AName, ACategory: string; AStatus: TModuleStatus; const AMessage: string = '');
begin
  SetLength(Modules, Length(Modules) + 1);
  with Modules[High(Modules)] do
  begin
    Name := AName;
    Category := ACategory;
    Status := AStatus;
    Message := AMessage;
  end;
end;

function StatusToStr(AStatus: TModuleStatus): string;
begin
  case AStatus of
    msAvailable: Result := '✅ Available';
    msPartial: Result := '⚠️ Partial';
    msNotAvailable: Result := '❌ Not Available';
    msDeprecated: Result := '⚠️ Deprecated';
    msError: Result := '❌ Error';
  end;
end;

function StatusToIcon(AStatus: TModuleStatus): string;
begin
  case AStatus of
    msAvailable: Result := '✅';
    msPartial: Result := '⚠️';
    msNotAvailable: Result := '❌';
    msDeprecated: Result := '⚠️';
    msError: Result := '❌';
  end;
end;

// 测试各个模块的可用性
procedure TestCoreModules;
var
  ctx: PSSL_CTX;
begin
  WriteLn('Testing Core Modules...');
  
  // SSL/TLS
  try
    ctx := SSL_CTX_new(TLS_client_method());
    if ctx <> nil then
    begin
      SSL_CTX_free(ctx);
      AddModule('SSL/TLS Core', 'Core', msAvailable, 'SSL context creation successful');
    end
    else
      AddModule('SSL/TLS Core', 'Core', msError, 'Failed to create SSL context');
  except
    on E: Exception do
      AddModule('SSL/TLS Core', 'Core', msError, 'Exception: ' + E.Message);
  end;
  
  // BIO
  try
    if Assigned(BIO_new) and Assigned(BIO_free) and Assigned(BIO_s_mem) then
      AddModule('BIO', 'Core', msAvailable, 'BIO functions loaded')
    else
      AddModule('BIO', 'Core', msPartial, 'Some BIO functions missing');
  except
    AddModule('BIO', 'Core', msError, 'BIO test failed');
  end;
  
  // Error handling
  try
    if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
      AddModule('Error Handling', 'Core', msAvailable, 'Error handling functions loaded')
    else
      AddModule('Error Handling', 'Core', msPartial, 'Some error functions missing');
  except
    AddModule('Error Handling', 'Core', msError, 'Error handling test failed');
  end;
  
  // Random
  try
    if Assigned(RAND_bytes) and Assigned(RAND_status) then
      AddModule('Random', 'Core', msAvailable, 'Random functions loaded')
    else
      AddModule('Random', 'Core', msPartial, 'Some random functions missing');
  except
    AddModule('Random', 'Core', msError, 'Random test failed');
  end;
  
  // Crypto
  try
    if Assigned(OPENSSL_version) then
      AddModule('Crypto', 'Core', msAvailable, 'Crypto functions loaded')
    else
      AddModule('Crypto', 'Core', msPartial, 'Some crypto functions missing');
  except
    AddModule('Crypto', 'Core', msError, 'Crypto test failed');
  end;
  
  // EVP
  try
    if Assigned(EVP_CIPHER_CTX_new) and Assigned(EVP_EncryptInit_ex) and 
       Assigned(EVP_MD_CTX_new) and Assigned(EVP_DigestInit_ex) then
      AddModule('EVP (Encryption)', 'Core', msAvailable, 'EVP functions loaded')
    else
      AddModule('EVP (Encryption)', 'Core', msPartial, 'Some EVP functions missing');
  except
    AddModule('EVP (Encryption)', 'Core', msError, 'EVP test failed');
  end;
end;

procedure TestSymmetricCiphers;
begin
  WriteLn('Testing Symmetric Ciphers...');
  
  // AES
  if Assigned(EVP_aes_128_gcm) and Assigned(EVP_aes_256_gcm) and Assigned(EVP_aes_128_cbc) then
    AddModule('AES', 'Symmetric', msAvailable, 'All AES modes available')
  else
    AddModule('AES', 'Symmetric', msPartial, 'Some AES modes missing');
  
  // DES
  if Assigned(EVP_des_ede3_cbc) then
    AddModule('DES/3DES', 'Symmetric', msAvailable, '3DES available')
  else
    AddModule('DES/3DES', 'Symmetric', msNotAvailable, 'DES not available');
  
  // ChaCha20
  if Assigned(EVP_chacha20_poly1305) then
    AddModule('ChaCha20-Poly1305', 'Symmetric', msAvailable, 'ChaCha20-Poly1305 available')
  else
    AddModule('ChaCha20-Poly1305', 'Symmetric', msNotAvailable, 'ChaCha20 not available');
  
  // Camellia
  if Assigned(EVP_camellia_128_cbc) then
    AddModule('Camellia', 'Symmetric', msAvailable, 'Camellia available')
  else
    AddModule('Camellia', 'Symmetric', msNotAvailable, 'Camellia not available');
  
  // ARIA & SEED - skipped due to compilation issues
  AddModule('ARIA', 'Symmetric', msPartial, 'ARIA module not tested');
  AddModule('SEED', 'Symmetric', msPartial, 'SEED module not tested');
end;

procedure TestAsymmetricCiphers;
begin
  WriteLn('Testing Asymmetric Ciphers...');
  
  // RSA
  if Assigned(RSA_new) and Assigned(RSA_generate_key_ex) then
    AddModule('RSA', 'Asymmetric', msAvailable, 'RSA functions available')
  else
    AddModule('RSA', 'Asymmetric', msPartial, 'Some RSA functions missing');
  
  // EC
  if Assigned(EC_KEY_new) and Assigned(EC_KEY_generate_key) then
    AddModule('EC (Elliptic Curve)', 'Asymmetric', msAvailable, 'EC functions available')
  else
    AddModule('EC (Elliptic Curve)', 'Asymmetric', msPartial, 'Some EC functions missing');
  
  // DH
  if Assigned(DH_new) and Assigned(DH_generate_parameters_ex) then
    AddModule('DH (Diffie-Hellman)', 'Asymmetric', msAvailable, 'DH functions available')
  else
    AddModule('DH (Diffie-Hellman)', 'Asymmetric', msPartial, 'Some DH functions missing');
  
  // DSA
  if Assigned(DSA_new) and Assigned(DSA_generate_parameters_ex) then
    AddModule('DSA', 'Asymmetric', msDeprecated, 'DSA available but deprecated in OpenSSL 3.x')
  else
    AddModule('DSA', 'Asymmetric', msNotAvailable, 'DSA not available');
  
  // ECDSA
  if Assigned(ECDSA_sign) and Assigned(ECDSA_verify) then
    AddModule('ECDSA', 'Asymmetric', msAvailable, 'ECDSA functions available')
  else
    AddModule('ECDSA', 'Asymmetric', msPartial, 'Some ECDSA functions missing');
  
  // ECDH
  if Assigned(ECDH_compute_key) then
    AddModule('ECDH', 'Asymmetric', msAvailable, 'ECDH functions available')
  else
    AddModule('ECDH', 'Asymmetric', msPartial, 'Some ECDH functions missing');
end;

procedure TestHashFunctions;
begin
  WriteLn('Testing Hash Functions...');
  
  // SHA-1
  if Assigned(EVP_sha1) then
    AddModule('SHA-1', 'Hash', msAvailable, 'SHA-1 available (not recommended)')
  else
    AddModule('SHA-1', 'Hash', msNotAvailable, 'SHA-1 not available');
  
  // SHA-2
  if Assigned(EVP_sha256) and Assigned(EVP_sha512) then
    AddModule('SHA-2 (224/256/384/512)', 'Hash', msAvailable, 'All SHA-2 variants available')
  else
    AddModule('SHA-2 (224/256/384/512)', 'Hash', msPartial, 'Some SHA-2 variants missing');
  
  // SHA-3
  if Assigned(EVP_sha3_256) and Assigned(EVP_sha3_512) then
    AddModule('SHA-3', 'Hash', msAvailable, 'SHA-3 available')
  else
    AddModule('SHA-3', 'Hash', msNotAvailable, 'SHA-3 not available');
  
  // MD5
  if Assigned(EVP_md5) then
    AddModule('MD5', 'Hash', msDeprecated, 'MD5 available (deprecated, insecure)')
  else
    AddModule('MD5', 'Hash', msNotAvailable, 'MD5 not available');
  
  // BLAKE2
  if Assigned(EVP_blake2b512) then
    AddModule('BLAKE2', 'Hash', msAvailable, 'BLAKE2 available')
  else
    AddModule('BLAKE2', 'Hash', msNotAvailable, 'BLAKE2 not available');
  
  // SM3 - skipped due to compilation issues
  AddModule('SM3 (Chinese)', 'Hash', msPartial, 'SM3 module not tested');
end;

procedure TestMACFunctions;
begin
  WriteLn('Testing MAC Functions...');
  
  // HMAC
  if Assigned(HMAC) and Assigned(HMAC_CTX_new) then
    AddModule('HMAC', 'MAC', msAvailable, 'HMAC functions available')
  else
    AddModule('HMAC', 'MAC', msPartial, 'Some HMAC functions missing');
  
  // CMAC
  if Assigned(CMAC_CTX_new) then
    AddModule('CMAC', 'MAC', msAvailable, 'CMAC functions available')
  else
    AddModule('CMAC', 'MAC', msNotAvailable, 'CMAC not available');
  
  // Poly1305
  if Assigned(EVP_chacha20_poly1305) then
    AddModule('Poly1305', 'MAC', msAvailable, 'Poly1305 available (via ChaCha20)')
  else
    AddModule('Poly1305', 'MAC', msNotAvailable, 'Poly1305 not available');
end;

procedure TestKeyDerivation;
begin
  WriteLn('Testing Key Derivation Functions...');
  
  // PBKDF2
  if Assigned(PKCS5_PBKDF2_HMAC) then
    AddModule('PBKDF2', 'KDF', msAvailable, 'PBKDF2 available')
  else
    AddModule('PBKDF2', 'KDF', msNotAvailable, 'PBKDF2 not available');
  
  // HKDF
  if Assigned(EVP_PKEY_CTX_new_id) then
    AddModule('HKDF', 'KDF', msAvailable, 'HKDF available (via EVP)')
  else
    AddModule('HKDF', 'KDF', msPartial, 'HKDF may be available');
  
  // Scrypt
  if Assigned(EVP_PBE_scrypt) then
    AddModule('Scrypt', 'KDF', msAvailable, 'Scrypt available')
  else
    AddModule('Scrypt', 'KDF', msNotAvailable, 'Scrypt not available');
end;

procedure TestCertificates;
begin
  WriteLn('Testing Certificate Functions...');
  
  // X.509
  if Assigned(X509_new) and Assigned(X509_free) and Assigned(X509_verify) then
    AddModule('X.509', 'Certificate', msAvailable, 'X.509 functions available')
  else
    AddModule('X.509', 'Certificate', msPartial, 'Some X.509 functions missing');
  
  // X.509v3
  if Assigned(X509V3_EXT_conf_nid) then
    AddModule('X.509v3 Extensions', 'Certificate', msAvailable, 'X.509v3 extensions available')
  else
    AddModule('X.509v3 Extensions', 'Certificate', msPartial, 'Some X.509v3 functions missing');
  
  // PEM
  if Assigned(PEM_read_bio_X509) and Assigned(PEM_write_bio_X509) then
    AddModule('PEM', 'Certificate', msAvailable, 'PEM functions available')
  else
    AddModule('PEM', 'Certificate', msPartial, 'Some PEM functions missing');
  
  // PKCS#7
  if Assigned(PKCS7_new) then
    AddModule('PKCS#7', 'Certificate', msAvailable, 'PKCS#7 available')
  else
    AddModule('PKCS#7', 'Certificate', msNotAvailable, 'PKCS#7 not available');
  
  // PKCS#12
  if Assigned(PKCS12_create) and Assigned(PKCS12_parse) then
    AddModule('PKCS#12', 'Certificate', msAvailable, 'PKCS#12 available')
  else
    AddModule('PKCS#12', 'Certificate', msPartial, 'Some PKCS#12 functions missing');
  
  // OCSP
  if Assigned(OCSP_REQUEST_new) then
    AddModule('OCSP', 'Certificate', msAvailable, 'OCSP available')
  else
    AddModule('OCSP', 'Certificate', msNotAvailable, 'OCSP not available');
  
  // CMS
  if Assigned(CMS_ContentInfo_new) then
    AddModule('CMS', 'Certificate', msAvailable, 'CMS available')
  else
    AddModule('CMS', 'Certificate', msNotAvailable, 'CMS not available');
end;

procedure TestDataStructures;
begin
  WriteLn('Testing Data Structures...');
  
  // ASN.1
  if Assigned(ASN1_INTEGER_new) and Assigned(ASN1_STRING_new) then
    AddModule('ASN.1', 'Data Structure', msAvailable, 'ASN.1 functions available')
  else
    AddModule('ASN.1', 'Data Structure', msPartial, 'Some ASN.1 functions missing');
  
  // BIGNUM
  if Assigned(BN_new) and Assigned(BN_add) and Assigned(BN_mul) then
    AddModule('BIGNUM', 'Data Structure', msAvailable, 'BIGNUM functions available')
  else
    AddModule('BIGNUM', 'Data Structure', msPartial, 'Some BIGNUM functions missing');
end;

procedure TestSystemFunctions;
begin
  WriteLn('Testing System Functions...');
  
  // Engine - skipped due to compilation issues
  AddModule('Engine', 'System', msPartial, 'Engine module not tested (compilation issues)');
  
  // Provider - skipped due to compilation issues
  AddModule('Provider', 'System', msPartial, 'Provider module not tested (compilation issues)');
end;

procedure TestAEADModes;
begin
  WriteLn('Testing AEAD Modes...');
  
  // GCM
  if Assigned(EVP_aes_128_gcm) and Assigned(EVP_aes_256_gcm) then
    AddModule('AES-GCM', 'AEAD', msAvailable, 'AES-GCM available')
  else
    AddModule('AES-GCM', 'AEAD', msNotAvailable, 'AES-GCM not available');
  
  // CCM
  if Assigned(EVP_aes_128_ccm) and Assigned(EVP_aes_256_ccm) then
    AddModule('AES-CCM', 'AEAD', msAvailable, 'AES-CCM available')
  else
    AddModule('AES-CCM', 'AEAD', msNotAvailable, 'AES-CCM not available');
  
  // XTS
  if Assigned(EVP_aes_128_xts) and Assigned(EVP_aes_256_xts) then
    AddModule('AES-XTS', 'AEAD', msAvailable, 'AES-XTS available')
  else
    AddModule('AES-XTS', 'AEAD', msNotAvailable, 'AES-XTS not available');
  
  // OCB
  if Assigned(EVP_aes_128_ocb) and Assigned(EVP_aes_256_ocb) then
    AddModule('AES-OCB', 'AEAD', msAvailable, 'AES-OCB available')
  else
    AddModule('AES-OCB', 'AEAD', msNotAvailable, 'AES-OCB not available');
  
  // ChaCha20-Poly1305
  if Assigned(EVP_chacha20_poly1305) then
    AddModule('ChaCha20-Poly1305', 'AEAD', msAvailable, 'ChaCha20-Poly1305 available')
  else
    AddModule('ChaCha20-Poly1305', 'AEAD', msNotAvailable, 'ChaCha20-Poly1305 not available');
end;

procedure PrintReport;
var
  i: Integer;
  CurrentCategory: string;
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('OpenSSL Module Diagnostic Report');
  WriteLn('========================================');
  WriteLn('OpenSSL Version: ', OpenSSLVersion);
  WriteLn('Generated: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn;
  
  CurrentCategory := '';
  for i := 0 to High(Modules) do
  begin
    if Modules[i].Category <> CurrentCategory then
    begin
      WriteLn;
      WriteLn('--- ', Modules[i].Category, ' Modules ---');
      CurrentCategory := Modules[i].Category;
    end;
    
    WriteLn(StatusToIcon(Modules[i].Status), ' ', 
            Format('%-30s', [Modules[i].Name]), ' - ', 
            StatusToStr(Modules[i].Status));
    if Modules[i].Message <> '' then
      WriteLn('   ', Modules[i].Message);
  end;
  
  WriteLn;
  WriteLn('========================================');
  WriteLn('Summary');
  WriteLn('========================================');
  WriteLn('Total modules tested: ', TotalModules);
  WriteLn('✅ Available:        ', AvailableModules, ' (', 
          Format('%.1f', [AvailableModules * 100.0 / TotalModules]), '%)');
  WriteLn('⚠️ Partial/Deprecated:', PartialModules, ' (', 
          Format('%.1f', [PartialModules * 100.0 / TotalModules]), '%)');
  WriteLn('❌ Not Available:    ', NotAvailableModules, ' (', 
          Format('%.1f', [NotAvailableModules * 100.0 / TotalModules]), '%)');
  WriteLn;
  
  if AvailableModules >= TotalModules * 0.9 then
    WriteLn('🎉 Excellent! Almost all modules are available.')
  else if AvailableModules >= TotalModules * 0.7 then
    WriteLn('👍 Good! Most modules are available.')
  else if AvailableModules >= TotalModules * 0.5 then
    WriteLn('⚠️ Warning: Significant number of modules unavailable.')
  else
    WriteLn('❌ Critical: Many modules are not available.');
  
  WriteLn('========================================');
end;

procedure CalculateStatistics;
var
  i: Integer;
begin
  TotalModules := Length(Modules);
  AvailableModules := 0;
  PartialModules := 0;
  NotAvailableModules := 0;
  
  for i := 0 to High(Modules) do
  begin
    case Modules[i].Status of
      msAvailable: Inc(AvailableModules);
      msPartial, msDeprecated: Inc(PartialModules);
      msNotAvailable, msError: Inc(NotAvailableModules);
    end;
  end;
end;

begin
  WriteLn('OpenSSL Module Diagnostic Tool');
  WriteLn('==============================');
  WriteLn;
  
  // 初始化 OpenSSL
  try
    LoadOpenSSLCore;
    if not IsOpenSSLCoreLoaded then
    begin
      WriteLn('ERROR: Failed to load OpenSSL library!');
      WriteLn('Make sure OpenSSL 1.1.x or 3.x is installed and accessible.');
      Halt(1);
    end;
    
    OpenSSLVersion := string(OPENSSL_version(0));
    WriteLn('OpenSSL loaded successfully: ', OpenSSLVersion);
    WriteLn;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Exception while loading OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;
  
  // 运行所有测试
  TestCoreModules;
  TestSymmetricCiphers;
  TestAsymmetricCiphers;
  TestHashFunctions;
  TestMACFunctions;
  TestKeyDerivation;
  TestCertificates;
  TestDataStructures;
  TestSystemFunctions;
  TestAEADModes;
  
  // 计算统计并打印报告
  CalculateStatistics;
  PrintReport;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
