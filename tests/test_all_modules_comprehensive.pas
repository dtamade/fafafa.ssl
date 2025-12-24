program test_all_modules_comprehensive;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TModuleStatus = (msNotTested, msCompileOK, msCompileFail, msLoadOK, msLoadFail);
  
  TModuleInfo = record
    Name: string;
    FilePath: string;
    Status: TModuleStatus;
    Category: string;
    Priority: Integer; // 1=High, 2=Medium, 3=Low
    Note: string;
  end;

var
  Modules: array of TModuleInfo;

procedure AddModule(const Name, FilePath, Category: string; Priority: Integer);
begin
  SetLength(Modules, Length(Modules) + 1);
  with Modules[High(Modules)] do
  begin
    Name := Name;
    FilePath := FilePath;
    Status := msNotTested;
    Category := Category;
    Priority := Priority;
    Note := '';
  end;
end;

procedure InitializeModules;
begin
  // Core Infrastructure (Priority 1)
  AddModule('types', 'fafafa.ssl.openssl.base', 'Core', 1);
  AddModule('consts', 'fafafa.ssl.openssl.api.consts', 'Core', 1);
  AddModule('utils', 'fafafa.ssl.openssl.api.utils', 'Core', 1);
  AddModule('core', 'fafafa.ssl.openssl.api.core', 'Core', 1);
  AddModule('api', 'fafafa.ssl.openssl.api', 'Core', 1);
  AddModule('err', 'fafafa.ssl.openssl.api.err', 'Core', 1);
  
  // Basic Crypto (Priority 1)
  AddModule('evp', 'fafafa.ssl.openssl.api.evp', 'Crypto-Core', 1);
  AddModule('bn', 'fafafa.ssl.openssl.api.bn', 'Crypto-Core', 1);
  AddModule('bio', 'fafafa.ssl.openssl.api.bio', 'Crypto-Core', 1);
  AddModule('rand', 'fafafa.ssl.openssl.api.rand', 'Crypto-Core', 1);
  
  // Hash Algorithms (Priority 1)
  AddModule('md', 'fafafa.ssl.openssl.api.md', 'Hash', 1);
  AddModule('sha', 'fafafa.ssl.openssl.api.sha', 'Hash', 1);
  AddModule('sha3', 'fafafa.ssl.openssl.api.sha3', 'Hash', 1);
  AddModule('sha3.evp', 'fafafa.ssl.openssl.api.sha3.evp', 'Hash', 1);
  AddModule('blake2', 'fafafa.ssl.openssl.api.blake2', 'Hash', 1);
  AddModule('sm', 'fafafa.ssl.openssl.api.sm', 'Hash', 1);
  
  // Symmetric Ciphers (Priority 1)
  AddModule('aes', 'fafafa.ssl.openssl.api.aes', 'Cipher', 1);
  AddModule('des', 'fafafa.ssl.openssl.api.des', 'Cipher', 1);
  AddModule('chacha', 'fafafa.ssl.openssl.api.chacha', 'Cipher', 1);
  AddModule('aria', 'fafafa.ssl.openssl.api.aria', 'Cipher', 2);
  AddModule('seed', 'fafafa.ssl.openssl.api.seed', 'Cipher', 2);
  AddModule('legacy_ciphers', 'fafafa.ssl.openssl.legacy_ciphers', 'Cipher', 3);
  
  // Asymmetric Crypto (Priority 1)
  AddModule('rsa', 'fafafa.ssl.openssl.api.rsa', 'PKI', 1);
  AddModule('dsa', 'fafafa.ssl.openssl.api.dsa', 'PKI', 1);
  AddModule('dh', 'fafafa.ssl.openssl.api.dh', 'PKI', 1);
  AddModule('ec', 'fafafa.ssl.openssl.api.ec', 'PKI', 1);
  AddModule('ecdh', 'fafafa.ssl.openssl.api.ecdh', 'PKI', 1);
  AddModule('ecdsa', 'fafafa.ssl.openssl.api.ecdsa', 'PKI', 1);
  
  // MAC & KDF (Priority 1)
  AddModule('hmac', 'fafafa.ssl.openssl.api.hmac', 'MAC', 1);
  // AddModule('cmac', 'fafafa.ssl.openssl.api.cmac', 'MAC', 1);  // Phase 2.2: 废弃，使用cmac.evp
  AddModule('cmac.evp', 'fafafa.ssl.openssl.api.cmac.evp', 'MAC', 1);
  AddModule('kdf', 'fafafa.ssl.openssl.api.kdf', 'KDF', 1);
  AddModule('scrypt_whirlpool', 'fafafa.ssl.openssl.api.scrypt_whirlpool', 'KDF', 2);
  
  // AEAD & Modes (Priority 1)
  AddModule('aead', 'fafafa.ssl.openssl.api.aead', 'AEAD', 1);
  AddModule('modes', 'fafafa.ssl.openssl.api.modes', 'Modes', 1);
  
  // X.509 & PKI (Priority 1)
  AddModule('x509', 'fafafa.ssl.openssl.api.x509', 'PKI', 1);
  AddModule('x509v3', 'fafafa.ssl.openssl.api.x509v3', 'PKI', 1);
  AddModule('asn1', 'fafafa.ssl.openssl.api.asn1', 'Encoding', 1);
  AddModule('pem', 'fafafa.ssl.openssl.api.pem', 'Encoding', 1);
  
  // PKCS Standards (Priority 2)
  AddModule('pkcs', 'fafafa.ssl.openssl.api.pkcs', 'PKCS', 2);
  AddModule('pkcs7', 'fafafa.ssl.openssl.api.pkcs7', 'PKCS', 2);
  AddModule('pkcs12', 'fafafa.ssl.openssl.api.pkcs12', 'PKCS', 2);
  AddModule('cms', 'fafafa.ssl.openssl.api.cms', 'PKCS', 2);
  
  // SSL/TLS (Priority 2)
  AddModule('ssl', 'fafafa.ssl.openssl.api.ssl', 'SSL', 2);
  
  // Certificate Services (Priority 2)
  AddModule('ocsp', 'fafafa.ssl.openssl.api.ocsp', 'Certificate', 2);
  AddModule('ct', 'fafafa.ssl.openssl.api.ct', 'Certificate', 2);
  AddModule('ts', 'fafafa.ssl.openssl.api.ts', 'Certificate', 2);
  
  // Advanced Features (Priority 2-3)
  AddModule('engine', 'fafafa.ssl.openssl.api.engine', 'Advanced', 2);
  AddModule('provider', 'fafafa.ssl.openssl.api.provider', 'Advanced', 1);
  AddModule('param', 'fafafa.ssl.openssl.api.param', 'Advanced', 1);
  AddModule('store', 'fafafa.ssl.openssl.api.store', 'Advanced', 2);
  AddModule('async', 'fafafa.ssl.openssl.api.async', 'Advanced', 3);
  AddModule('comp', 'fafafa.ssl.openssl.api.comp', 'Advanced', 3);
  
  // Utilities (Priority 2-3)
  AddModule('buffer', 'fafafa.ssl.openssl.api.buffer', 'Utility', 2);
  AddModule('stack', 'fafafa.ssl.openssl.api.stack', 'Utility', 2);
  AddModule('lhash', 'fafafa.ssl.openssl.api.lhash', 'Utility', 2);
  AddModule('obj', 'fafafa.ssl.openssl.api.obj', 'Utility', 2);
  AddModule('conf', 'fafafa.ssl.openssl.api.conf', 'Utility', 2);
  AddModule('txt_db', 'fafafa.ssl.openssl.api.txt_db', 'Utility', 3);
  AddModule('ui', 'fafafa.ssl.openssl.api.ui', 'Utility', 3);
  AddModule('dso', 'fafafa.ssl.openssl.api.dso', 'Utility', 3);
  AddModule('srp', 'fafafa.ssl.openssl.api.srp', 'Utility', 3);
  AddModule('thread', 'fafafa.ssl.openssl.api.thread', 'Utility', 2);
  AddModule('crypto', 'fafafa.ssl.openssl.api.crypto', 'Utility', 1);
  AddModule('rand_old', 'fafafa.ssl.openssl.api.rand_old', 'Utility', 3);
end;

procedure PrintModuleList;
var
  i: Integer;
  Categories: TStringList;
  Cat: string;
  Count: Integer;
begin
  Categories := TStringList.Create;
  Categories.Sorted := True;
  Categories.Duplicates := dupIgnore;
  
  try
    // Collect unique categories
    for i := 0 to High(Modules) do
      Categories.Add(Modules[i].Category);
    
    WriteLn('========================================');
    WriteLn('  All OpenSSL Modules');
    WriteLn('========================================');
    WriteLn;
    WriteLn('Total Modules: ', Length(Modules));
    WriteLn;
    
    // Print by category
    for Cat in Categories do
    begin
      Count := 0;
      for i := 0 to High(Modules) do
        if Modules[i].Category = Cat then
          Inc(Count);
      
      WriteLn(Cat, ': ', Count, ' modules');
      WriteLn('----------------------------------------');
      
      for i := 0 to High(Modules) do
      begin
        if Modules[i].Category = Cat then
        begin
          Write('  ', Modules[i].Name:20);
          Write('  [P', Modules[i].Priority, ']');
          WriteLn;
        end;
      end;
      WriteLn;
    end;
    
  finally
    Categories.Free;
  end;
end;

procedure PrintStatistics;
var
  i: Integer;
  Total, P1, P2, P3: Integer;
  CoreCount, CryptoCount, PKICount, UtilCount: Integer;
begin
  Total := Length(Modules);
  P1 := 0; P2 := 0; P3 := 0;
  CoreCount := 0; CryptoCount := 0; PKICount := 0; UtilCount := 0;
  
  for i := 0 to High(Modules) do
  begin
    case Modules[i].Priority of
      1: Inc(P1);
      2: Inc(P2);
      3: Inc(P3);
    end;
    
    if (Modules[i].Category = 'Core') or (Modules[i].Category = 'Crypto-Core') then
      Inc(CoreCount)
    else if (Modules[i].Category = 'Hash') or (Modules[i].Category = 'Cipher') or 
            (Modules[i].Category = 'MAC') or (Modules[i].Category = 'KDF') or
            (Modules[i].Category = 'AEAD') or (Modules[i].Category = 'Modes') then
      Inc(CryptoCount)
    else if (Modules[i].Category = 'PKI') or (Modules[i].Category = 'PKCS') or
            (Modules[i].Category = 'Certificate') or (Modules[i].Category = 'Encoding') then
      Inc(PKICount)
    else
      Inc(UtilCount);
  end;
  
  WriteLn('========================================');
  WriteLn('  Module Statistics');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Total Modules:     ', Total);
  WriteLn;
  WriteLn('By Priority:');
  WriteLn('  Priority 1 (High):   ', P1, ' (', FormatFloat('0.0', (P1/Total)*100), '%)');
  WriteLn('  Priority 2 (Medium): ', P2, ' (', FormatFloat('0.0', (P2/Total)*100), '%)');
  WriteLn('  Priority 3 (Low):    ', P3, ' (', FormatFloat('0.0', (P3/Total)*100), '%)');
  WriteLn;
  WriteLn('By Functional Area:');
  WriteLn('  Core Infrastructure: ', CoreCount);
  WriteLn('  Cryptography:        ', CryptoCount);
  WriteLn('  PKI & Certificates:  ', PKICount);
  WriteLn('  Utilities:           ', UtilCount);
  WriteLn;
end;

begin
  WriteLn('========================================');
  WriteLn('  OpenSSL Module Inventory');
  WriteLn('========================================');
  WriteLn;
  
  InitializeModules;
  PrintModuleList;
  PrintStatistics;
  
  WriteLn('========================================');
  WriteLn('Next Steps:');
  WriteLn('----------------------------------------');
  WriteLn('1. Validate compilation of all modules');
  WriteLn('2. Test priority 1 modules (high priority)');
  WriteLn('3. Test priority 2 modules (medium priority)');
  WriteLn('4. Test priority 3 modules (low priority)');
  WriteLn('5. Generate comprehensive test report');
  WriteLn('========================================');
end.
