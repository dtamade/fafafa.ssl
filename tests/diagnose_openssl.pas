program diagnose_openssl;

{$mode Delphi}{$H+}

uses
  SysUtils, Dynlibs;

type
  TOPENSSL_VERSION_NUMBER = function: LongWord; cdecl;
  TOpenSSL_version = function(vtype: Integer): PAnsiChar; cdecl;

var
  libHandle: TLibHandle;
  dllNames: array[0..3] of string = ('libcrypto-3.dll', 'libcrypto-3-x64.dll', 'libcrypto-1_1-x64.dll', 'libcrypto.dll');
  i: Integer;
  dllName: string;
  OpenSSL_version_num: TOPENSSL_VERSION_NUMBER;
  OpenSSL_version: TOpenSSL_version;
  version_num: LongWord;
  version_str: PAnsiChar;

begin
  WriteLn('OpenSSL Diagnostics');
  WriteLn('==================');
  WriteLn;
  
  // Try to load OpenSSL library
  libHandle := 0;
  dllName := '';
  
  for i := 0 to High(dllNames) do
  begin
    WriteLn('Trying: ', dllNames[i]);
    libHandle := LoadLibrary(dllNames[i]);
    if libHandle <> 0 then
    begin
      dllName := dllNames[i];
      WriteLn('SUCCESS: Loaded ', dllNames[i]);
      Break;
    end
    else
      WriteLn('  Failed');
  end;
  
  if libHandle = 0 then
  begin
    WriteLn;
    WriteLn('ERROR: Could not load any OpenSSL library');
    Halt(1);
  end;
  
  try
    WriteLn;
    WriteLn('Loaded: ', dllName);
    WriteLn('Handle: 0x', IntToHex(libHandle, 8));
    WriteLn;
    
    // Get version information
    OpenSSL_version_num := TOPENSSL_VERSION_NUMBER(GetProcAddress(libHandle, 'OpenSSL_version_num'));
    OpenSSL_version := TOpenSSL_version(GetProcAddress(libHandle, 'OpenSSL_version'));
    
    if Assigned(OpenSSL_version_num) then
    begin
      version_num := OpenSSL_version_num();
      WriteLn('OpenSSL version number: 0x', IntToHex(version_num, 8));
      WriteLn('  Major: ', (version_num shr 28) and $F);
      WriteLn('  Minor: ', (version_num shr 20) and $FF);
      WriteLn('  Patch: ', (version_num shr 4) and $FFFF);
    end
    else
      WriteLn('OpenSSL_version_num: Not available');
    
    WriteLn;
    
    if Assigned(OpenSSL_version) then
    begin
      // OPENSSL_VERSION = 0
      version_str := OpenSSL_version(0);
      if version_str <> nil then
        WriteLn('Version string: ', version_str)
      else
        WriteLn('Version string: Not available');
    end
    else
      WriteLn('OpenSSL_version: Not available');
    
    WriteLn;
    WriteLn('Checking for specific functions:');
    
    // Check for EVP_MD_fetch
    if GetProcAddress(libHandle, 'EVP_MD_fetch') <> nil then
      WriteLn('  EVP_MD_fetch: Available (OpenSSL 3.x)')
    else
      WriteLn('  EVP_MD_fetch: NOT available (< OpenSSL 3.x)');
    
    // Check for EVP_get_digestbyname  
    if GetProcAddress(libHandle, 'EVP_get_digestbyname') <> nil then
      WriteLn('  EVP_get_digestbyname: Available')
    else
      WriteLn('  EVP_get_digestbyname: NOT available');
    
    // Check for SHA3 specific functions
    if GetProcAddress(libHandle, 'EVP_sha3_256') <> nil then
      WriteLn('  EVP_sha3_256: Available')
    else
      WriteLn('  EVP_sha3_256: NOT available');
    
    // Check for low-level SHA3
    if GetProcAddress(libHandle, 'SHA3_256_Init') <> nil then
      WriteLn('  SHA3_256_Init: Available (low-level API)')
    else
      WriteLn('  SHA3_256_Init: NOT available');
    
    WriteLn;
    WriteLn('Analysis:');
    if GetProcAddress(libHandle, 'EVP_MD_fetch') <> nil then
      WriteLn('  This is OpenSSL 3.x - SHA3 should use EVP_MD_fetch')
    else if GetProcAddress(libHandle, 'EVP_sha3_256') <> nil then
      WriteLn('  This is OpenSSL 1.1.1+ - SHA3 available via EVP_sha3_*')
    else
      WriteLn('  SHA3 may not be available in this OpenSSL version');
    
  finally
    FreeLibrary(libHandle);
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
