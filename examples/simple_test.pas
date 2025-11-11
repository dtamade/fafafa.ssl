program simple_test;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  fafafa.ssl,
  fafafa.ssl.types,
  fafafa.ssl.utils;

var
  LData: TBytes;
  LHash: string;
  LBase64: string;

begin
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║     fafafa.ssl Simple Test                           ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  WriteLn;
  
  // Test 1: Hash Data
  WriteLn('Test 1: Hash Data (SHA256)');
  LData := TEncoding.UTF8.GetBytes('Hello, World!');
  
  try
    LHash := TSSLHelper.HashData(LData, sslHashSHA256);
    WriteLn('  Input:  "Hello, World!"');
    WriteLn('  SHA256: ', LHash);
    if LHash <> '' then
      WriteLn('  Result: ✅ PASS')
    else
      WriteLn('  Result: ❌ FAIL');
  except
    on E: Exception do
      WriteLn('  Result: ❌ ERROR: ', E.Message);
  end;
  WriteLn;
  
  // Test 2: Base64 Encode/Decode
  WriteLn('Test 2: Base64 Encode/Decode');
  try
    LBase64 := TSSLUtils.BytesToBase64(LData);
    WriteLn('  Input:   "Hello, World!"');
    WriteLn('  Base64:  ', LBase64);
    
    LData := TSSLUtils.Base64ToBytes(LBase64);
    WriteLn('  Decoded: ', TEncoding.UTF8.GetString(LData));
    
    if TEncoding.UTF8.GetString(LData) = 'Hello, World!' then
      WriteLn('  Result: ✅ PASS')
    else
      WriteLn('  Result: ❌ FAIL');
  except
    on E: Exception do
      WriteLn('  Result: ❌ ERROR: ', E.Message);
  end;
  WriteLn;
  
  // Test 3: SSL Support Check
  WriteLn('Test 3: SSL Support Check');
  try
    if CheckSSLSupport then
    begin
      WriteLn('  SSL Support: ✅ Available');
      WriteLn('  Info: ', GetSSLSupportInfo);
    end
    else
      WriteLn('  SSL Support: ❌ Not Available');
  except
    on E: Exception do
      WriteLn('  Result: ❌ ERROR: ', E.Message);
  end;
  WriteLn;
  
  WriteLn('╔══════════════════════════════════════════════════════════╗');
  WriteLn('║                   Test Complete                       ║');
  WriteLn('╚══════════════════════════════════════════════════════════╝');
  
  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
  {$ENDIF}
end.


