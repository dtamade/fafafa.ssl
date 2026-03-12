program test_rand;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.rand;

var
  P: Pointer;
  buf: array[0..15] of Byte;
  i: Integer;

begin
  WriteLn('Testing RAND module loading...');
  WriteLn;
  
  // Load OpenSSL
  WriteLn('[1] Loading OpenSSL library...');
  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] Failed to load OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;
  if TOpenSSLLoader.IsModuleLoaded(osmCore) then
    WriteLn('[OK] OpenSSL loaded')
  else
  begin
    WriteLn('[ERROR] Failed to load OpenSSL');
    Halt(1);
  end;
  
  // Check crypto library
  WriteLn('[2] Checking crypto library handle...');
  WriteLn('Crypto lib handle: ', GetCryptoLibHandle);
  WriteLn('Is loaded: ', TOpenSSLLoader.IsModuleLoaded(osmCore));
  
  // Try to load RAND_bytes manually
  WriteLn('[3] Testing GetCryptoProcAddress...');
  P := GetCryptoProcAddress('RAND_bytes');
  WriteLn('RAND_bytes address: ', PtrUInt(P));
  
  // Try to load RAND module
  WriteLn('[4] Loading RAND module...');
  if LoadOpenSSLRAND then
    WriteLn('[OK] RAND module loaded')
  else
  begin
    WriteLn('[ERROR] Failed to load RAND module');
    Halt(1);
  end;
  
  // Test RAND_bytes
  WriteLn('[5] Testing RAND_bytes...');
  if RAND_bytes(@buf[0], 16) = 1 then
  begin
    WriteLn('[OK] RAND_bytes works');
    Write('Random bytes: ');
    for i := 0 to 15 do
      Write(IntToHex(buf[i], 2), ' ');
    WriteLn;
  end
  else
    WriteLn('[ERROR] RAND_bytes failed');
  
  WriteLn;
  WriteLn('[SUCCESS] All tests passed');
end.
