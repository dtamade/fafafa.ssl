program test_openssl_debug;

{$mode objfpc}{$H+}

uses
  SysUtils, dynlibs,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

var
  Lib: TOpenSSLLibrary;
  CryptoHandle, SSLHandle: TLibHandle;
begin
  WriteLn('=== OpenSSL Library Debug Test ===');
  WriteLn;
  
  // Step 1: Test dynamic library loading
  WriteLn('Step 1: Testing dynamic library loading...');
  CryptoHandle := LoadLibrary('libcrypto.so.3');
  SSLHandle := LoadLibrary('libssl.so.3');
  
  if CryptoHandle <> 0 then
    WriteLn('  OK: libcrypto.so.3 loaded')
  else
    WriteLn('  FAIL: libcrypto.so.3 not loaded');
    
  if SSLHandle <> 0 then
    WriteLn('  OK: libssl.so.3 loaded')
  else
    WriteLn('  FAIL: libssl.so.3 not loaded');
  
  if CryptoHandle <> 0 then UnloadLibrary(CryptoHandle);
  if SSLHandle <> 0 then UnloadLibrary(SSLHandle);
  WriteLn;
  
  // Step 2: Test TOpenSSLLibrary creation
  WriteLn('Step 2: Testing TOpenSSLLibrary creation...');
  try
    Lib := TOpenSSLLibrary.Create;
    WriteLn('  OK: TOpenSSLLibrary.Create succeeded');
  except
    on E: Exception do
    begin
      WriteLn('  FAIL: TOpenSSLLibrary.Create failed: ', E.Message);
      Halt(1);
    end;
  end;
  WriteLn;
  
  // Step 3: Test Initialize method
  WriteLn('Step 3: Testing Initialize method...');
  try
    if Lib.Initialize then
    begin
      WriteLn('  OK: Initialize succeeded');
      WriteLn('  Version: ', Lib.GetVersionString);
    end
    else
    begin
      WriteLn('  FAIL: Initialize returned False');
      WriteLn('  Last Error: ', Lib.GetLastErrorString);
    end;
  except
    on E: Exception do
      WriteLn('  EXCEPTION: ', E.ClassName, ': ', E.Message);
  end;
  
  Lib.Free;
  WriteLn;
  WriteLn('=== Test Complete ===');
end.
