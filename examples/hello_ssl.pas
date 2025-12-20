program hello_ssl;

{$mode objfpc}{$H+}

{ ============================================================================
  fafafa.ssl - Quick Start Example
  
  Purpose: Demonstrate how to load OpenSSL and get version info
  Compile: fpc -Fusrc -Fusrc\openssl examples\hello_ssl.pas
  ============================================================================ }

uses
  SysUtils,
  fafafa.ssl.openssl.api;

var
  LVersion: string;

begin
  WriteLn('=============================================================================');
  WriteLn('  fafafa.ssl - OpenSSL Pascal Bindings');
  WriteLn('  Quick Start Example');
  WriteLn('=============================================================================');
  WriteLn;
  
  WriteLn('[Step 1] Loading OpenSSL library...');
  
  if LoadOpenSSLLibrary then
  begin
    WriteLn('         SUCCESS');
    WriteLn;
    
    // Get version info
    LVersion := GetOpenSSLVersion;
    WriteLn('[Step 2] Get version info...');
    WriteLn('         Version: ', LVersion);
    WriteLn;
    
    WriteLn('[Step 3] Check backend support...');
    WriteLn('         - OpenSSL:  Available');
    {$IFDEF WINDOWS}
    WriteLn('         - WinSSL:   Available (Windows platform)');
    {$ENDIF}
    {$IFDEF LINUX}
    WriteLn('         - WinSSL:   Not available (Windows only)');
    {$ENDIF}
    
    WriteLn;
    WriteLn('=============================================================================');
    WriteLn('  Test Result: PASSED');
    WriteLn('  Your environment is correctly configured!');
    WriteLn('=============================================================================');
    WriteLn;
    WriteLn('Next Steps:');
    WriteLn('  - Check examples/ directory for more examples');
    WriteLn('  - Read QUICK_START.md for core features');
    WriteLn('  - Run run_all_tests.ps1 for complete validation');
    WriteLn;
    
    ExitCode := 0;
  end
  else
  begin
    WriteLn('         FAILED');
    WriteLn;
    WriteLn('[ERROR] Cannot load OpenSSL library');
    WriteLn;
    WriteLn('Possible reasons:');
    WriteLn('  1. OpenSSL not installed');
    WriteLn('  2. OpenSSL DLL not in system PATH');
    WriteLn('  3. Incompatible OpenSSL version (need 1.1.1+ or 3.x)');
    WriteLn;
    WriteLn('Solutions:');
    {$IFDEF WINDOWS}
    WriteLn('  Windows:');
    WriteLn('    - Download from: https://slproweb.com/products/Win32OpenSSL.html');
    WriteLn('    - Or copy libcrypto-*.dll and libssl-*.dll to program directory');
    {$ENDIF}
    {$IFDEF LINUX}
    WriteLn('  Linux:');
    WriteLn('    - Ubuntu/Debian: sudo apt install libssl3 libssl-dev');
    WriteLn('    - Fedora/RHEL:   sudo dnf install openssl openssl-devel');
    {$ENDIF}
    WriteLn;
    WriteLn('=============================================================================');
    
    ExitCode := 1;
  end;
end.
