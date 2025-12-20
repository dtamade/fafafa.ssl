program diagnose_whirlpool;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.evp;

var
  md: PEVP_MD;
  version: PAnsiChar;

begin
  WriteLn('========================================');
  WriteLn('  Whirlpool Availability Diagnostic');
  WriteLn('========================================');
  WriteLn;
  
  try
    if not LoadOpenSSLLibrary then
    begin
      WriteLn('ERROR: Failed to load OpenSSL library');
      Halt(1);
    end;
    
    if not LoadEVP(GetCryptoLibHandle) then
    begin
      WriteLn('ERROR: Failed to load EVP functions');
      Halt(1);
    end;
    
    // Get OpenSSL version
    version := OpenSSL_version(0);
    WriteLn('OpenSSL Version: ', string(version));
    WriteLn;
    
    // Test 1: Try EVP_MD_fetch (OpenSSL 3.x)
    WriteLn('Test 1: EVP_MD_fetch("whirlpool")');
    if Assigned(EVP_MD_fetch) then
    begin
      md := EVP_MD_fetch(nil, 'whirlpool', nil);
      if md <> nil then
      begin
        WriteLn('  ✓ AVAILABLE via EVP_MD_fetch');
        EVP_MD_free(md);
      end
      else
        WriteLn('  ✗ NOT AVAILABLE via EVP_MD_fetch');
    end
    else
      WriteLn('  EVP_MD_fetch not available (OpenSSL 1.x)');
    WriteLn;
    
    // Test 2: Try EVP_get_digestbyname
    WriteLn('Test 2: EVP_get_digestbyname("whirlpool")');
    md := EVP_get_digestbyname('whirlpool');
    if md <> nil then
      WriteLn('  ✓ AVAILABLE via EVP_get_digestbyname')
    else
      WriteLn('  ✗ NOT AVAILABLE via EVP_get_digestbyname');
    WriteLn;
    
    // Test 3: Try uppercase
    WriteLn('Test 3: EVP_get_digestbyname("WHIRLPOOL")');
    md := EVP_get_digestbyname('WHIRLPOOL');
    if md <> nil then
      WriteLn('  ✓ AVAILABLE via EVP_get_digestbyname (uppercase)')
    else
      WriteLn('  ✗ NOT AVAILABLE via EVP_get_digestbyname (uppercase)');
    WriteLn;
    
    WriteLn('========================================');
    WriteLn('CONCLUSION:');
    WriteLn('Whirlpool is likely in the legacy provider in OpenSSL 3.x.');
    WriteLn('It may need to be explicitly loaded via:');
    WriteLn('  OSSL_PROVIDER_load(NULL, "legacy")');
    WriteLn('========================================');
    
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
