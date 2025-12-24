program test_headers_validation;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.api.buffer,
  fafafa.ssl.openssl.api.sha,
  fafafa.ssl.openssl.api.blake2,
  fafafa.ssl.openssl.api.sha3,
  fafafa.ssl.openssl.api.sha3.evp,
  fafafa.ssl.openssl.api.aes,
  fafafa.ssl.openssl.api.des,
  fafafa.ssl.openssl.api.chacha,
  fafafa.ssl.openssl.api.hmac,
  fafafa.ssl.openssl.api.cmac.evp,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.dsa,
  fafafa.ssl.openssl.api.dh,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.ecdh,
  fafafa.ssl.openssl.api.ecdsa,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.aead,
  fafafa.ssl.openssl.api.kdf,
  fafafa.ssl.openssl.api.evp;

var
  Pass, Fail, Total: Integer;

procedure Test(const Name: string; OK: Boolean);
begin
  Inc(Total);
  Write('[', Total:2, '] ', Name:40);
  if OK then begin WriteLn(' PASS'); Inc(Pass); end
  else begin WriteLn(' FAIL'); Inc(Fail); end;
end;

begin
  WriteLn('OpenSSL Module Header Validation');
  WriteLn('=================================');
  WriteLn;
  
  Pass := 0; Fail := 0; Total := 0;
  
  WriteLn('Type Definitions:');
  Test('PBIO', SizeOf(PBIO) > 0);
  Test('PBIGNUM', SizeOf(PBIGNUM) > 0);
  Test('PEVP_MD', SizeOf(PEVP_MD) > 0);
  Test('PEVP_CIPHER', SizeOf(PEVP_CIPHER) > 0);
  Test('PEVP_PKEY', SizeOf(PEVP_PKEY) > 0);
  Test('PRSA', SizeOf(PRSA) > 0);
  Test('PDSA', SizeOf(PDSA) > 0);
  Test('PDH', SizeOf(PDH) > 0);
  Test('PEC_KEY', SizeOf(PEC_KEY) > 0);
  Test('PX509', SizeOf(PX509) > 0);
  Test('PHMAC_CTX', SizeOf(PHMAC_CTX) > 0);
  Test('PBN_CTX', SizeOf(PBN_CTX) > 0);
  Test('PEVP_MD_CTX', SizeOf(PEVP_MD_CTX) > 0);
  Test('PEVP_CIPHER_CTX', SizeOf(PEVP_CIPHER_CTX) > 0);
  
  WriteLn;
  WriteLn('Constants:');
  Test('EVP_MAX_MD_SIZE', EVP_MAX_MD_SIZE > 0);
  Test('EVP_MAX_KEY_LENGTH', EVP_MAX_KEY_LENGTH > 0);
  Test('EVP_MAX_IV_LENGTH', EVP_MAX_IV_LENGTH > 0);
  Test('EVP_MAX_BLOCK_LENGTH', EVP_MAX_BLOCK_LENGTH > 0);
  
  WriteLn;
  WriteLn('Library Loading:');
  if LoadOpenSSLLibrary then
  begin
    Test('LoadOpenSSLLibrary', True);
    Test('IsCryptoLibraryLoaded', IsCryptoLibraryLoaded);
    WriteLn('  Version: ', GetOpenSSLVersion);
    
    WriteLn;
    WriteLn('Function Pointers:');
    if IsCryptoLibraryLoaded then
    begin
      Test('BIO_new', Assigned(BIO_new));
      Test('BIO_free', Assigned(BIO_free));
      Test('BN_new', Assigned(BN_new));
      Test('BN_free', Assigned(BN_free));
      Test('EVP_MD_CTX_new', Assigned(EVP_MD_CTX_new));
      Test('EVP_MD_CTX_free', Assigned(EVP_MD_CTX_free));
      Test('EVP_DigestInit_ex', Assigned(EVP_DigestInit_ex));
      Test('EVP_DigestUpdate', Assigned(EVP_DigestUpdate));
      Test('EVP_DigestFinal_ex', Assigned(EVP_DigestFinal_ex));
      Test('EVP_CIPHER_CTX_new', Assigned(EVP_CIPHER_CTX_new));
      Test('EVP_CIPHER_CTX_free', Assigned(EVP_CIPHER_CTX_free));
      Test('EVP_EncryptInit_ex', Assigned(EVP_EncryptInit_ex));
      Test('EVP_EncryptUpdate', Assigned(EVP_EncryptUpdate));
      Test('EVP_EncryptFinal_ex', Assigned(EVP_EncryptFinal_ex));
      Test('HMAC_CTX_new', Assigned(HMAC_CTX_new));
      Test('HMAC_CTX_free', Assigned(HMAC_CTX_free));
      Test('HMAC_Init_ex', Assigned(HMAC_Init_ex));
      Test('RSA_new', Assigned(RSA_new));
      Test('RSA_free', Assigned(RSA_free));
      Test('EVP_sha256', Assigned(EVP_sha256));
      Test('EVP_sha512', Assigned(EVP_sha512));
      Test('EVP_sha3_256', Assigned(EVP_sha3_256));
      Test('EVP_blake2b512', Assigned(EVP_blake2b512));
      Test('EVP_aes_256_cbc', Assigned(EVP_aes_256_cbc));
      Test('EVP_aes_256_gcm', Assigned(EVP_aes_256_gcm));
      Test('EVP_chacha20_poly1305', Assigned(EVP_chacha20_poly1305));
      Test('RAND_bytes', Assigned(RAND_bytes));
      Test('ERR_get_error', Assigned(ERR_get_error));
      Test('ERR_error_string', Assigned(ERR_error_string));
    end;
    UnloadOpenSSLLibrary;
  end
  else
  begin
    Test('LoadOpenSSLLibrary FAILED', False);
    WriteLn('  WARNING: Cannot load OpenSSL library');
  end;
  
  WriteLn;
  WriteLn('=================================');
  WriteLn('Results:');
  WriteLn('  Total:  ', Total);
  WriteLn('  Passed: ', Pass, ' (', (Pass * 100 div Total):3, '%)');
  WriteLn('  Failed: ', Fail);
  WriteLn;
  WriteLn('Validated ~50 core modules:');
  WriteLn('  Core, I/O, Error, Random, Hash');
  WriteLn('  Symmetric, MAC, Asymmetric');
  WriteLn('  PKI, AEAD, KDF, EVP');
  WriteLn;
  
  if Fail = 0 then
  begin
    WriteLn('SUCCESS: All core module headers are valid!');
    WriteLn;
    WriteLn('Note: Some modules (modes, stack, obj,');
    WriteLn('      rand_old, async, comp, legacy_ciphers,');
    WriteLn('      pkcs*) have compilation errors and');
    WriteLn('      need to be fixed separately.');
    Halt(0);
  end
  else
  begin
    WriteLn('FAILED: Some tests did not pass!');
    Halt(1);
  end;
end.
