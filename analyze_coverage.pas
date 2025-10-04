program analyze_coverage;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Classes, StrUtils;

type
  TModulePriority = (mpCore, mpHigh, mpMedium, mpLow, mpSpecial);
  
  TModuleInfo = record
    Name: string;
    FileName: string;
    Priority: TModulePriority;
    HasTest: Boolean;
  end;

var
  Modules: array of TModuleInfo;
  
procedure AddModule(const AName, AFileName: string; APriority: TModulePriority);
var
  Idx: Integer;
begin
  Idx := Length(Modules);
  SetLength(Modules, Idx + 1);
  Modules[Idx].Name := AName;
  Modules[Idx].FileName := AFileName;
  Modules[Idx].Priority := APriority;
  Modules[Idx].HasTest := False;
end;

procedure InitializeModules;
begin
  // 核心模块 (Priority 0)
  AddModule('Core', 'fafafa.ssl.openssl.core.pas', mpCore);
  AddModule('Crypto', 'fafafa.ssl.openssl.crypto.pas', mpCore);
  AddModule('EVP', 'fafafa.ssl.openssl.evp.pas', mpCore);
  AddModule('ERR', 'fafafa.ssl.openssl.err.pas', mpCore);
  AddModule('BIO', 'fafafa.ssl.openssl.bio.pas', mpCore);
  AddModule('BN', 'fafafa.ssl.openssl.bn.pas', mpCore);
  AddModule('RAND', 'fafafa.ssl.openssl.rand.pas', mpCore);
  AddModule('Buffer', 'fafafa.ssl.openssl.buffer.pas', mpCore);
  
  // 高优先级 (Priority 1)
  AddModule('RSA', 'fafafa.ssl.openssl.rsa.pas', mpHigh);
  AddModule('DSA', 'fafafa.ssl.openssl.dsa.pas', mpHigh);
  AddModule('ECDSA', 'fafafa.ssl.openssl.ecdsa.pas', mpHigh);
  AddModule('EC', 'fafafa.ssl.openssl.ec.pas', mpHigh);
  AddModule('DH', 'fafafa.ssl.openssl.dh.pas', mpHigh);
  AddModule('ECDH', 'fafafa.ssl.openssl.ecdh.pas', mpHigh);
  AddModule('X.509', 'fafafa.ssl.openssl.x509.pas', mpHigh);
  AddModule('PEM', 'fafafa.ssl.openssl.pem.pas', mpHigh);
  AddModule('ASN.1', 'fafafa.ssl.openssl.asn1.pas', mpHigh);
  
  // 现代算法
  AddModule('SHA-2', 'fafafa.ssl.openssl.sha.pas', mpHigh);
  AddModule('SHA-3', 'fafafa.ssl.openssl.sha3.pas', mpHigh);
  AddModule('SHA-3 EVP', 'fafafa.ssl.openssl.sha3.evp.pas', mpHigh);
  AddModule('BLAKE2', 'fafafa.ssl.openssl.blake2.pas', mpHigh);
  AddModule('ChaCha20', 'fafafa.ssl.openssl.chacha.pas', mpHigh);
  AddModule('ChaCha20-Poly1305', 'fafafa.ssl.openssl.chacha20.pas', mpHigh);
  AddModule('SM3', 'fafafa.ssl.openssl.sm.pas', mpHigh);
  AddModule('SM4', 'fafafa.ssl.openssl.sm.pas', mpHigh);
  
  // MAC & KDF
  AddModule('HMAC', 'fafafa.ssl.openssl.hmac.pas', mpHigh);
  AddModule('CMAC', 'fafafa.ssl.openssl.cmac.pas', mpHigh);
  AddModule('CMAC EVP', 'fafafa.ssl.openssl.cmac.evp.pas', mpHigh);
  AddModule('KDF', 'fafafa.ssl.openssl.kdf.pas', mpHigh);
  AddModule('HKDF', 'fafafa.ssl.openssl.kdf.pas', mpHigh);
  
  // AEAD & Modes
  AddModule('AEAD', 'fafafa.ssl.openssl.aead.pas', mpHigh);
  AddModule('Modes', 'fafafa.ssl.openssl.modes.pas', mpMedium);
  
  // 中优先级 (Priority 2)
  AddModule('AES', 'fafafa.ssl.openssl.aes.pas', mpMedium);
  AddModule('DES', 'fafafa.ssl.openssl.des.pas', mpMedium);
  AddModule('Camellia', 'fafafa.ssl.openssl.camellia.pas', mpMedium);
  AddModule('ARIA', 'fafafa.ssl.openssl.aria.pas', mpMedium);
  AddModule('SEED', 'fafafa.ssl.openssl.seed.pas', mpMedium);
  AddModule('MD', 'fafafa.ssl.openssl.md.pas', mpMedium);
  AddModule('RIPEMD', 'fafafa.ssl.openssl.ripemd.pas', mpMedium);
  AddModule('Whirlpool', 'fafafa.ssl.openssl.scrypt_whirlpool.pas', mpMedium);
  
  // PKI
  AddModule('PKCS#7', 'fafafa.ssl.openssl.pkcs7.pas', mpMedium);
  AddModule('PKCS#12', 'fafafa.ssl.openssl.pkcs12.pas', mpMedium);
  AddModule('PKCS', 'fafafa.ssl.openssl.pkcs.pas', mpMedium);
  AddModule('CMS', 'fafafa.ssl.openssl.cms.pas', mpMedium);
  AddModule('OCSP', 'fafafa.ssl.openssl.ocsp.pas', mpMedium);
  AddModule('X.509v3', 'fafafa.ssl.openssl.x509v3.pas', mpMedium);
  AddModule('CT', 'fafafa.ssl.openssl.ct.pas', mpMedium);
  AddModule('TS', 'fafafa.ssl.openssl.ts.pas', mpMedium);
  
  // SSL/TLS
  AddModule('SSL', 'fafafa.ssl.openssl.ssl.pas', mpHigh);
  
  // 低优先级与工具
  AddModule('Stack', 'fafafa.ssl.openssl.stack.pas', mpLow);
  AddModule('LHash', 'fafafa.ssl.openssl.lhash.pas', mpLow);
  AddModule('Store', 'fafafa.ssl.openssl.store.pas', mpLow);
  AddModule('Objects', 'fafafa.ssl.openssl.obj.pas', mpLow);
  AddModule('Param', 'fafafa.ssl.openssl.param.pas', mpLow);
  AddModule('Provider', 'fafafa.ssl.openssl.provider.pas', mpLow);
  AddModule('Engine', 'fafafa.ssl.openssl.engine.pas', mpLow);
  AddModule('Config', 'fafafa.ssl.openssl.conf.pas', mpLow);
  
  // 专用模块
  AddModule('Thread', 'fafafa.ssl.openssl.thread.pas', mpSpecial);
  AddModule('Async', 'fafafa.ssl.openssl.async.pas', mpSpecial);
  AddModule('Comp', 'fafafa.ssl.openssl.comp.pas', mpSpecial);
  AddModule('SRP', 'fafafa.ssl.openssl.srp.pas', mpSpecial);
  AddModule('DSO', 'fafafa.ssl.openssl.dso.pas', mpSpecial);
  AddModule('UI', 'fafafa.ssl.openssl.ui.pas', mpSpecial);
  AddModule('TXT_DB', 'fafafa.ssl.openssl.txt_db.pas', mpSpecial);
  AddModule('Legacy Ciphers', 'fafafa.ssl.openssl.legacy_ciphers.pas', mpSpecial);
  
  // 工具类
  AddModule('Types', 'fafafa.ssl.openssl.types.pas', mpSpecial);
  AddModule('Consts', 'fafafa.ssl.openssl.consts.pas', mpSpecial);
  AddModule('Utils', 'fafafa.ssl.openssl.utils.pas', mpSpecial);
  AddModule('API', 'fafafa.ssl.openssl.api.pas', mpSpecial);
end;

function FindTestsForModule(const ModuleName: string): Boolean;
var
  SearchResult: TSearchRec;
  TestPattern, NormalizedName: string;
begin
  Result := False;
  NormalizedName := LowerCase(StringReplace(ModuleName, '.', '_', [rfReplaceAll]));
  NormalizedName := StringReplace(NormalizedName, '#', '', [rfReplaceAll]);
  NormalizedName := StringReplace(NormalizedName, ' ', '_', [rfReplaceAll]);
  NormalizedName := StringReplace(NormalizedName, '-', '', [rfReplaceAll]);
  
  // 搜索 tests 目录
  if FindFirst('tests' + PathDelim + 'test_' + NormalizedName + '*.pas', faAnyFile, SearchResult) = 0 then
  begin
    Result := True;
    FindClose(SearchResult);
    Exit;
  end;
  FindClose(SearchResult);
  
  // 搜索 tests/integration 目录
  if FindFirst('tests' + PathDelim + 'integration' + PathDelim + 'test_' + NormalizedName + '*.pas', faAnyFile, SearchResult) = 0 then
  begin
    Result := True;
    FindClose(SearchResult);
    Exit;
  end;
  FindClose(SearchResult);
  
  // 搜索 tests/unit 目录
  if FindFirst('tests' + PathDelim + 'unit' + PathDelim + 'test_' + NormalizedName + '*.pas', faAnyFile, SearchResult) = 0 then
  begin
    Result := True;
    FindClose(SearchResult);
  end;
  FindClose(SearchResult);
end;

procedure ScanTestCoverage;
var
  I: Integer;
begin
  for I := 0 to High(Modules) do
    Modules[I].HasTest := FindTestsForModule(Modules[I].Name);
end;

procedure PrintReport;
var
  I: Integer;
  TotalCount, TestedCount: Integer;
  PriorityName: string;
  CurrentPriority: TModulePriority;
  PriorityStats: array[TModulePriority] of record Total, Tested: Integer; end;
begin
  // 统计
  TotalCount := Length(Modules);
  TestedCount := 0;
  
  for I := 0 to High(Modules) do
  begin
    if Modules[I].HasTest then
      Inc(TestedCount);
    Inc(PriorityStats[Modules[I].Priority].Total);
    if Modules[I].HasTest then
      Inc(PriorityStats[Modules[I].Priority].Tested);
  end;
  
  WriteLn;
  WriteLn('========================================');
  WriteLn('  OpenSSL Pascal Test Coverage Report');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Overall Coverage: ', TestedCount, '/', TotalCount, ' (', 
          FormatFloat('0.0', TestedCount * 100.0 / TotalCount), '%)');
  WriteLn;
  
  // Priority stats
  WriteLn('By Priority:');
  WriteLn('  Core:      ', PriorityStats[mpCore].Tested, '/', 
          PriorityStats[mpCore].Total, ' (', 
          FormatFloat('0.0', PriorityStats[mpCore].Tested * 100.0 / PriorityStats[mpCore].Total), '%)');
  WriteLn('  High:      ', PriorityStats[mpHigh].Tested, '/', 
          PriorityStats[mpHigh].Total, ' (', 
          FormatFloat('0.0', PriorityStats[mpHigh].Tested * 100.0 / PriorityStats[mpHigh].Total), '%)');
  WriteLn('  Medium:    ', PriorityStats[mpMedium].Tested, '/', 
          PriorityStats[mpMedium].Total, ' (', 
          FormatFloat('0.0', PriorityStats[mpMedium].Tested * 100.0 / PriorityStats[mpMedium].Total), '%)');
  WriteLn('  Low:       ', PriorityStats[mpLow].Tested, '/', 
          PriorityStats[mpLow].Total, ' (', 
          FormatFloat('0.0', PriorityStats[mpLow].Tested * 100.0 / PriorityStats[mpLow].Total), '%)');
  WriteLn('  Special:   ', PriorityStats[mpSpecial].Tested, '/', 
          PriorityStats[mpSpecial].Total, ' (', 
          FormatFloat('0.0', PriorityStats[mpSpecial].Tested * 100.0 / PriorityStats[mpSpecial].Total), '%)');
  WriteLn;
  
  // Untested modules list
  WriteLn('========================================');
  WriteLn('Untested Modules (', TotalCount - TestedCount, '):');
  WriteLn('========================================');
  WriteLn;
  
  for CurrentPriority := mpCore to mpSpecial do
  begin
    case CurrentPriority of
      mpCore: PriorityName := 'Core Modules (Priority 0)';
      mpHigh: PriorityName := 'High Priority (Priority 1)';
      mpMedium: PriorityName := 'Medium Priority (Priority 2)';
      mpLow: PriorityName := 'Low Priority (Priority 3)';
      mpSpecial: PriorityName := 'Special/Utility (Priority 4)';
    end;
    
    WriteLn('[', PriorityName, ']');
    for I := 0 to High(Modules) do
    begin
      if (Modules[I].Priority = CurrentPriority) and (not Modules[I].HasTest) then
        WriteLn('  - ', Modules[I].Name);
    end;
    WriteLn;
  end;
  
  // Recommendations
  WriteLn('========================================');
  WriteLn('Recommendations:');
  WriteLn('========================================');
  WriteLn;
  WriteLn('1. Prioritize Core and High Priority modules');
  WriteLn('2. Test Medium Priority as needed');
  WriteLn('3. Low Priority and Special can be deferred');
  WriteLn;
end;

begin
  InitializeModules;
  WriteLn('Scanning test coverage...');
  ScanTestCoverage;
  PrintReport;
end.
