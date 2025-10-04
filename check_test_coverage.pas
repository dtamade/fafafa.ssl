program check_test_coverage;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  SourceModules, TestModules, MissingTests: TStringList;
  ModuleName: string;
  i: Integer;
  HasTest: Boolean;

begin
  SourceModules := TStringList.Create;
  TestModules := TStringList.Create;
  MissingTests := TStringList.Create;
  
  try
    // All 65 source modules
    SourceModules.Add('aead');
    SourceModules.Add('aes');
    SourceModules.Add('api');
    SourceModules.Add('aria');
    SourceModules.Add('asn1');
    SourceModules.Add('async');
    SourceModules.Add('bio');
    SourceModules.Add('blake2');
    SourceModules.Add('bn');
    SourceModules.Add('buffer');
    SourceModules.Add('chacha');
    SourceModules.Add('cmac');
    SourceModules.Add('cmac.evp');
    SourceModules.Add('cms');
    SourceModules.Add('comp');
    SourceModules.Add('conf');
    SourceModules.Add('consts');
    SourceModules.Add('core');
    SourceModules.Add('crypto');
    SourceModules.Add('ct');
    SourceModules.Add('des');
    SourceModules.Add('dh');
    SourceModules.Add('dsa');
    SourceModules.Add('dso');
    SourceModules.Add('ec');
    SourceModules.Add('ecdh');
    SourceModules.Add('ecdsa');
    SourceModules.Add('engine');
    SourceModules.Add('err');
    SourceModules.Add('evp');
    SourceModules.Add('hmac');
    SourceModules.Add('kdf');
    SourceModules.Add('legacy_ciphers');
    SourceModules.Add('lhash');
    SourceModules.Add('md');
    SourceModules.Add('modes');
    SourceModules.Add('obj');
    SourceModules.Add('ocsp');
    SourceModules.Add('param');
    SourceModules.Add('pem');
    SourceModules.Add('pkcs');
    SourceModules.Add('pkcs12');
    SourceModules.Add('pkcs7');
    SourceModules.Add('provider');
    SourceModules.Add('rand');
    SourceModules.Add('rand_old');
    SourceModules.Add('rsa');
    SourceModules.Add('scrypt_whirlpool');
    SourceModules.Add('seed');
    SourceModules.Add('sha');
    SourceModules.Add('sha3');
    SourceModules.Add('sha3.evp');
    SourceModules.Add('sm');
    SourceModules.Add('srp');
    SourceModules.Add('ssl');
    SourceModules.Add('stack');
    SourceModules.Add('store');
    SourceModules.Add('thread');
    SourceModules.Add('ts');
    SourceModules.Add('txt_db');
    SourceModules.Add('types');
    SourceModules.Add('ui');
    SourceModules.Add('utils');
    SourceModules.Add('x509');
    SourceModules.Add('x509v3');
    
    // Modules with existing tests (based on test file names)
    TestModules.Add('aead');
    TestModules.Add('algorithm_availability');
    TestModules.Add('algorithms_batch');
    TestModules.Add('blake2');
    TestModules.Add('blowfish');
    TestModules.Add('camellia');
    TestModules.Add('chacha20');
    TestModules.Add('cmac.evp');
    TestModules.Add('crypto_basics');
    TestModules.Add('ecdsa');
    TestModules.Add('evp');
    TestModules.Add('gcm');
    TestModules.Add('hash');
    TestModules.Add('hmac');
    TestModules.Add('kdf');
    TestModules.Add('openssl_load');
    TestModules.Add('provider');
    TestModules.Add('ripemd');
    TestModules.Add('sha3');
    TestModules.Add('signature');
    TestModules.Add('sm3');
    TestModules.Add('sm4');
    TestModules.Add('whirlpool');
    
    // Check for missing tests
    for i := 0 to SourceModules.Count - 1 do
    begin
      ModuleName := SourceModules[i];
      HasTest := False;
      
      // Check if there's a test that covers this module
      if TestModules.IndexOf(ModuleName) >= 0 then
        HasTest := True
      else if TestModules.IndexOf(StringReplace(ModuleName, '.', '_', [rfReplaceAll])) >= 0 then
        HasTest := True
      else if TestModules.IndexOf(StringReplace(ModuleName, '.evp', '', [])) >= 0 then
        HasTest := True;
        
      if not HasTest then
        MissingTests.Add(ModuleName);
    end;
    
    // Print results
    WriteLn('========================================');
    WriteLn('  Test Coverage Analysis');
    WriteLn('========================================');
    WriteLn;
    WriteLn('Total Source Modules: ', SourceModules.Count);
    WriteLn('Modules with Tests: ', SourceModules.Count - MissingTests.Count);
    WriteLn('Modules WITHOUT Tests: ', MissingTests.Count);
    WriteLn;
    WriteLn('========================================');
    WriteLn('  Modules Missing Test Coverage:');
    WriteLn('========================================');
    for i := 0 to MissingTests.Count - 1 do
      WriteLn('  ', (i + 1):3, '. ', MissingTests[i]);
    WriteLn;
    
  finally
    SourceModules.Free;
    TestModules.Free;
    MissingTests.Free;
  end;
end.
