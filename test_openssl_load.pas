program test_openssl_load;

{$mode objfpc}{$H+}

uses
  SysUtils, dynlibs;

var
  CryptoHandle, SSLHandle: TLibHandle;
  LibPath: string;
begin
  WriteLn('Testing OpenSSL library loading...');
  WriteLn;
  
  // Test 1: Try to load libcrypto.so.3
  WriteLn('Test 1: Loading libcrypto.so.3');
  CryptoHandle := LoadLibrary('libcrypto.so.3');
  if CryptoHandle <> 0 then
  begin
    WriteLn('SUCCESS: libcrypto.so.3 loaded');
    UnloadLibrary(CryptoHandle);
  end
  else
    WriteLn('FAIL: Could not load libcrypto.so.3');
  WriteLn;
  
  // Test 2: Try to load libssl.so.3
  WriteLn('Test 2: Loading libssl.so.3');
  SSLHandle := LoadLibrary('libssl.so.3');
  if SSLHandle <> 0 then
  begin
    WriteLn('SUCCESS: libssl.so.3 loaded');
    UnloadLibrary(SSLHandle);
  end
  else
    WriteLn('FAIL: Could not load libssl.so.3');
  WriteLn;
  
  // Test 3: Check system paths
  WriteLn('Test 3: Checking common library paths');
  WriteLn('/usr/lib/x86_64-linux-gnu/libcrypto.so.3 exists: ', FileExists('/usr/lib/x86_64-linux-gnu/libcrypto.so.3'));
  WriteLn('/usr/lib/x86_64-linux-gnu/libssl.so.3 exists: ', FileExists('/usr/lib/x86_64-linux-gnu/libssl.so.3'));
  WriteLn('/usr/lib64/libcrypto.so.3 exists: ', FileExists('/usr/lib64/libcrypto.so.3'));
  WriteLn('/usr/lib64/libssl.so.3 exists: ', FileExists('/usr/lib64/libssl.so.3'));
end.
