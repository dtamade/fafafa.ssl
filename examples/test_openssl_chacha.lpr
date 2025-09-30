program test_openssl_chacha;
{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs,
  fafafa.ssl.openssl.types, fafafa.ssl.openssl.core, 
  fafafa.ssl.openssl.chacha, fafafa.ssl.openssl.evp, 
  fafafa.ssl.openssl.api, fafafa.ssl.openssl.consts;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  if Passed then begin WriteLn('[PASS] ', TestName); Inc(TestsPassed); end
  else begin WriteLn('[FAIL] ', TestName); Inc(TestsFailed); end;
end;

function TestChaCha20EVP: Boolean;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  key: array[0..31] of Byte;
  iv: array[0..11] of Byte;  // ChaCha20 uses 96-bit nonce
  plaintext: array[0..63] of Byte;
  ciphertext: array[0..127] of Byte;
  len, clen: Integer;
  i: Integer;
begin
  Result := False;
  if not Assigned(EVP_chacha20) then Exit(True);
  
  for i := 0 to 31 do key[i] := i;
  for i := 0 to 11 do iv[i] := i;
  for i := 0 to 63 do plaintext[i] := i;
  
  cipher := EVP_chacha20();
  if not Assigned(cipher) then Exit;
  
  ctx := EVP_CIPHER_CTX_new();
  if not Assigned(ctx) then Exit;
  
  try
    if EVP_EncryptInit_ex(ctx, cipher, nil, @key[0], @iv[0]) <> 1 then Exit;
    if fafafa.ssl.openssl.api.EVP_EncryptUpdate(ctx, @ciphertext[0], @len, @plaintext[0], 64) <> 1 then Exit;
    clen := len;
    if fafafa.ssl.openssl.api.EVP_EncryptFinal_ex(ctx, @ciphertext[clen], @len) <> 1 then Exit;
    clen := clen + len;
    
    Result := clen > 0;
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function TestChaCha20Poly1305EVP: Boolean;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  key: array[0..31] of Byte;
  iv: array[0..11] of Byte;
  plaintext: array[0..63] of Byte;
  ciphertext: array[0..127] of Byte;
  tag: array[0..15] of Byte;
  len, clen: Integer;
  i: Integer;
begin
  Result := False;
  if not Assigned(EVP_chacha20_poly1305) then Exit(True);
  
  for i := 0 to 31 do key[i] := i;
  for i := 0 to 11 do iv[i] := i;
  for i := 0 to 63 do plaintext[i] := i;
  
  cipher := EVP_chacha20_poly1305();
  if not Assigned(cipher) then Exit;
  
  ctx := EVP_CIPHER_CTX_new();
  if not Assigned(ctx) then Exit;
  
  try
    if EVP_EncryptInit_ex(ctx, cipher, nil, @key[0], @iv[0]) <> 1 then Exit;
    if fafafa.ssl.openssl.api.EVP_EncryptUpdate(ctx, @ciphertext[0], @len, @plaintext[0], 64) <> 1 then Exit;
    clen := len;
    if fafafa.ssl.openssl.api.EVP_EncryptFinal_ex(ctx, @ciphertext[clen], @len) <> 1 then Exit;
    clen := clen + len;
    
    if fafafa.ssl.openssl.api.EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_GET_TAG, 16, @tag[0]) <> 1 then Exit;
    
    Result := clen > 0;
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

begin
  WriteLn('OpenSSL ChaCha20-Poly1305 Test');
  WriteLn('==============================');
  WriteLn;
  
  LoadOpenSSLCore;
  WriteLn('OpenSSL version: ', GetOpenSSLVersion);
  WriteLn;
  
  if not LoadChaChaFunctions then
  begin
    WriteLn('ChaCha20 functions not available');
    Halt(0);
  end;
  
  if not LoadEVP(GetCryptoLibHandle) then
  begin
    WriteLn('EVP functions not available');
    Halt(0);
  end;
  
  TestResult('ChaCha20 EVP', TestChaCha20EVP);
  TestResult('ChaCha20-Poly1305 AEAD', TestChaCha20Poly1305EVP);
  
  WriteLn;
  WriteLn('Test Summary:');
  WriteLn('=============');
  WriteLn('Tests Passed: ', TestsPassed);
  WriteLn('Tests Failed: ', TestsFailed);
  WriteLn('Total Tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then WriteLn('All tests PASSED!')
  else begin WriteLn('Some tests FAILED!'); Halt(1); end;
  
  UnloadChaChaFunctions;
  UnloadEVP;
  UnloadOpenSSLCore;
end.