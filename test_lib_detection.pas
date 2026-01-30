program test_lib_detection;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed;

var
  Lib: ISSLLibrary;
  AvailableLibs: TSSLLibraryTypes;
  LibType: TSSLLibraryType;
begin
  WriteLn('Testing SSL Library Detection...');
  WriteLn;
  
  // Test 1: Check if OpenSSL is registered
  WriteLn('Test 1: Check available libraries');
  AvailableLibs := TSSLFactory.GetAvailableLibraries;
  WriteLn('Available libraries: ', Integer(AvailableLibs));
  
  for LibType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if LibType in AvailableLibs then
      WriteLn('  - ', SSL_LIBRARY_NAMES[LibType]);
  end;
  WriteLn;
  
  // Test 2: Try to get library
  WriteLn('Test 2: Try to get OpenSSL library');
  try
    Lib := TSSLFactory.GetLibrary(sslOpenSSL);
    if Assigned(Lib) then
      WriteLn('SUCCESS: Got OpenSSL library')
    else
      WriteLn('FAIL: Library is nil');
  except
    on E: Exception do
      WriteLn('FAIL: ', E.Message);
  end;
  WriteLn;
  
  // Test 3: Try auto-detect
  WriteLn('Test 3: Try auto-detect');
  try
    Lib := TSSLFactory.GetLibrary(sslAutoDetect);
    if Assigned(Lib) then
      WriteLn('SUCCESS: Auto-detected library')
    else
      WriteLn('FAIL: Library is nil');
  except
    on E: Exception do
      WriteLn('FAIL: ', E.Message);
  end;
end.
