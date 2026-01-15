program test_version_detection;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.core;

procedure TestAutoVersionDetection;
var
  VersionStr: string;
  Version: TOpenSSLVersion;
begin
  WriteLn('====================================');
  WriteLn('OpenSSL Version Detection Test');
  WriteLn('====================================');
  WriteLn;
  
  try
    WriteLn('Attempting to load OpenSSL with auto-detection...');
    LoadOpenSSLCore;
    
    if IsOpenSSLCoreLoaded then
    begin
      Version := GetOpenSSLVersion;
      VersionStr := GetOpenSSLVersionString;
      
      WriteLn('[PASS] OpenSSL loaded successfully!');
      WriteLn('  Detected version: ', VersionStr);
      WriteLn('  Version enum: ', Ord(Version));
      WriteLn('  Crypto handle: ', PtrUInt(GetCryptoLibHandle));
      WriteLn('  SSL handle: ', PtrUInt(GetSSLLibHandle));
      
      // Try to get version number from OpenSSL itself
      if Assigned(OpenSSL_version_num) then
      begin
        WriteLn('  OpenSSL version number: $', IntToHex(OpenSSL_version_num(), 8));
      end;
    end
    else
    begin
      WriteLn('[FAIL] OpenSSL not loaded');
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ', E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  WriteLn;
  WriteLn('====================================');
  WriteLn('Test completed successfully');
  WriteLn('====================================');
end;

procedure TestSpecificVersionLoading;
begin
  WriteLn;
  WriteLn('====================================');
  WriteLn('Testing Specific Version Loading');
  WriteLn('====================================');
  WriteLn;
  
  // First unload current version
  UnloadOpenSSLCore;
  
  // Try loading OpenSSL 1.1.x specifically
  try
    WriteLn('Attempting to load OpenSSL 1.1.x specifically...');
    LoadOpenSSLCoreWithVersion(sslVersion_1_1);
    WriteLn('[PASS] Loaded: ', GetOpenSSLVersionString);
    UnloadOpenSSLCore;
  except
    on E: Exception do
      WriteLn('[INFO] Could not load 1.1.x: ', E.Message);
  end;
  
  // Try loading OpenSSL 3.x specifically
  try
    WriteLn('Attempting to load OpenSSL 3.x specifically...');
    LoadOpenSSLCoreWithVersion(sslVersion_3_0);
    WriteLn('[PASS] Loaded: ', GetOpenSSLVersionString);
    UnloadOpenSSLCore;
  except
    on E: Exception do
      WriteLn('[INFO] Could not load 3.x: ', E.Message);
  end;
end;

begin
  try
    TestAutoVersionDetection;
    TestSpecificVersionLoading;
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.