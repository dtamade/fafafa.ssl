program test_load_rand_detailed;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.api;

var
  LProc: Pointer;
  LBuf: array[0..7] of Byte;

begin
  WriteLn('=== Detailed RAND Loading Test ===');
  WriteLn;
  
  WriteLn('[1] Load Core');
  LoadOpenSSLCore();
  WriteLn('  IsCryptoLibraryLoaded: ', IsCryptoLibraryLoaded);
  WriteLn;
  
  WriteLn('[2] Check library handle');
  WriteLn('  GetCryptoLibHandle: ', GetCryptoLibHandle);
  WriteLn;
  
  WriteLn('[3] Try to get RAND_bytes directly');
  LProc := GetCryptoProcAddress('RAND_bytes');
  WriteLn('  RAND_bytes via GetCryptoProcAddress: ', PtrUInt(LProc));
  WriteLn;
  
  WriteLn('[4] Call LoadOpenSSLRAND');
  if LoadOpenSSLRAND() then
  begin
    WriteLn('  ✓ LoadOpenSSLRAND returned True');
    WriteLn('  RAND_bytes assigned: ', Assigned(RAND_bytes));
    
    // 测试RAND_bytes
    if RAND_bytes(@LBuf[0], 8) = 1 then
      WriteLn('  ✓ RAND_bytes works!')
    else
      WriteLn('  ✗ RAND_bytes failed');
  end
  else
  begin
    WriteLn('  ✗ LoadOpenSSLRAND returned False');
    WriteLn('  RAND_bytes assigned: ', Assigned(RAND_bytes));
  end;
end.
