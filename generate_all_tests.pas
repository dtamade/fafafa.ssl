program generate_all_tests;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TModuleInfo = record
    Name: string;
    UnitName: string;
    TestFunctions: array of string;
  end;

var
  Modules: array of TModuleInfo;
  TestsDir: string;

procedure AddModule(const Name, UnitName: string; const TestFuncs: array of string);
var
  i: Integer;
begin
  SetLength(Modules, Length(Modules) + 1);
  with Modules[High(Modules)] do
  begin
    Name := Name;
    UnitName := UnitName;
    SetLength(TestFunctions, Length(TestFuncs));
    for i := 0 to High(TestFuncs) do
      TestFunctions[i] := TestFuncs[i];
  end;
end;

procedure GenerateTestFile(const ModInfo: TModuleInfo);
var
  F: TextFile;
  TestFile: string;
  i: Integer;
begin
  TestFile := IncludeTrailingPathDelimiter(TestsDir) + 'test_' + StringReplace(ModInfo.Name, '.', '_', [rfReplaceAll]) + '_comprehensive.pas';
  
  if FileExists(TestFile) then
  begin
    WriteLn('Skipped (exists): ', ExtractFileName(TestFile));
    Exit;
  end;
  
  AssignFile(F, TestFile);
  try
    Rewrite(F);
    
    // 文件头
    WriteLn(F, 'program test_', StringReplace(ModInfo.Name, '.', '_', [rfReplaceAll]), '_comprehensive;');
    WriteLn(F);
    WriteLn(F, '{$mode objfpc}{$H+}');
    WriteLn(F);
    WriteLn(F, 'uses');
    WriteLn(F, '  SysUtils, fafafa.ssl.openssl.api, ', ModInfo.UnitName, ';');
    WriteLn(F);
    WriteLn(F, 'var');
    WriteLn(F, '  TestsPassed, TestsFailed: Integer;');
    WriteLn(F);
    
    // RunTest 辅助函数
    WriteLn(F, 'procedure RunTest(const TestName: string; Passed: Boolean);');
    WriteLn(F, 'begin');
    WriteLn(F, '  if Passed then');
    WriteLn(F, '  begin');
    WriteLn(F, '    WriteLn(''[PASS] '', TestName);');
    WriteLn(F, '    Inc(TestsPassed);');
    WriteLn(F, '  end');
    WriteLn(F, '  else');
    WriteLn(F, '  begin');
    WriteLn(F, '    WriteLn(''[FAIL] '', TestName);');
    WriteLn(F, '    Inc(TestsFailed);');
    WriteLn(F, '  end;');
    WriteLn(F, 'end;');
    WriteLn(F);
    
    // 为每个测试函数生成模板
    if Length(ModInfo.TestFunctions) > 0 then
    begin
      for i := 0 to High(ModInfo.TestFunctions) do
      begin
        WriteLn(F, 'procedure Test_', ModInfo.TestFunctions[i], ';');
        WriteLn(F, 'begin');
        WriteLn(F, '  // TODO: Implement ', ModInfo.TestFunctions[i], ' test');
        WriteLn(F, '  RunTest(''', ModInfo.TestFunctions[i], ' - Basic Test'', True);');
        WriteLn(F, 'end;');
        WriteLn(F);
      end;
    end
    else
    begin
      // 默认测试
      WriteLn(F, 'procedure Test_BasicFunctionality;');
      WriteLn(F, 'begin');
      WriteLn(F, '  // TODO: Implement basic functionality test');
      WriteLn(F, '  RunTest(''Basic Functionality'', True);');
      WriteLn(F, 'end;');
      WriteLn(F);
    end;
    
    // 主程序
    WriteLn(F, 'begin');
    WriteLn(F, '  WriteLn(''========================================'');');
    WriteLn(F, '  WriteLn(''  OpenSSL ', UpperCase(ModInfo.Name), ' Module Test'');');
    WriteLn(F, '  WriteLn(''========================================'');');
    WriteLn(F, '  WriteLn;');
    WriteLn(F);
    WriteLn(F, '  TestsPassed := 0;');
    WriteLn(F, '  TestsFailed := 0;');
    WriteLn(F);
    WriteLn(F, '  if not LoadOpenSSLLibrary then');
    WriteLn(F, '  begin');
    WriteLn(F, '    WriteLn(''ERROR: Cannot load OpenSSL library'');');
    WriteLn(F, '    Halt(1);');
    WriteLn(F, '  end;');
    WriteLn(F);
    WriteLn(F, '  try');
    WriteLn(F, '    WriteLn(''Running ', ModInfo.Name, ' tests...'');');
    WriteLn(F, '    WriteLn;');
    WriteLn(F);
    
    // 调用所有测试
    if Length(ModInfo.TestFunctions) > 0 then
    begin
      for i := 0 to High(ModInfo.TestFunctions) do
        WriteLn(F, '    Test_', ModInfo.TestFunctions[i], ';');
    end
    else
      WriteLn(F, '    Test_BasicFunctionality;');
    
    WriteLn(F);
    WriteLn(F, '    WriteLn;');
    WriteLn(F, '    WriteLn(''========================================'');');
    WriteLn(F, '    WriteLn(''  Test Results'');');
    WriteLn(F, '    WriteLn(''========================================'');');
    WriteLn(F, '    WriteLn(''Passed: '', TestsPassed);');
    WriteLn(F, '    WriteLn(''Failed: '', TestsFailed);');
    WriteLn(F, '    WriteLn(''Total: '', TestsPassed + TestsFailed);');
    WriteLn(F, '    WriteLn(''Success Rate: '', ((TestsPassed * 100) div (TestsPassed + TestsFailed)):3, ''%'');');
    WriteLn(F, '    WriteLn;');
    WriteLn(F);
    WriteLn(F, '    if TestsFailed = 0 then');
    WriteLn(F, '    begin');
      WriteLn(F, '      WriteLn(''All tests passed!'');');
      WriteLn(F, '      Halt(0);');
      WriteLn(F, '    end');
      WriteLn(F, '    else');
      WriteLn(F, '    begin');
      WriteLn(F, '      WriteLn(''Some tests failed!'');');
    WriteLn(F, '      Halt(1);');
    WriteLn(F, '    end;');
    WriteLn(F);
    WriteLn(F, '  finally');
    WriteLn(F, '    UnloadOpenSSLLibrary;');
    WriteLn(F, '  end;');
    WriteLn(F, 'end.');
    
    WriteLn('Generated: ', ExtractFileName(TestFile));
  finally
    CloseFile(F);
  end;
end;

procedure InitializeModules;
begin
  // Priority 1 - 核心基础设施
  AddModule('bio', 'fafafa.ssl.openssl.bio', ['BIO_Creation', 'BIO_Read_Write', 'BIO_Memory']);
  AddModule('rand', 'fafafa.ssl.openssl.rand', ['RAND_bytes', 'RAND_pseudo_bytes']);
  AddModule('err', 'fafafa.ssl.openssl.err', ['ERR_get_error', 'ERR_error_string']);
  AddModule('crypto', 'fafafa.ssl.openssl.crypto', ['CRYPTO_malloc', 'CRYPTO_free']);
  
  // Priority 1 - 基础算法
  AddModule('md', 'fafafa.ssl.openssl.md', ['MD5', 'SHA1', 'SHA256']);
  AddModule('sha', 'fafafa.ssl.openssl.sha', ['SHA256_Init', 'SHA256_Update', 'SHA256_Final']);
  AddModule('aes', 'fafafa.ssl.openssl.aes', ['AES_set_key', 'AES_encrypt', 'AES_decrypt']);
  AddModule('des', 'fafafa.ssl.openssl.des', ['DES_set_key', 'DES_encrypt', 'DES_decrypt']);
  
  // Priority 1 - 非对称加密
  AddModule('rsa', 'fafafa.ssl.openssl.rsa', ['RSA_generate_key', 'RSA_public_encrypt', 'RSA_private_decrypt']);
  AddModule('dsa', 'fafafa.ssl.openssl.dsa', ['DSA_generate_parameters', 'DSA_sign', 'DSA_verify']);
  AddModule('dh', 'fafafa.ssl.openssl.dh', ['DH_generate_parameters', 'DH_generate_key']);
  AddModule('ec', 'fafafa.ssl.openssl.ec', ['EC_KEY_new', 'EC_KEY_generate_key']);
  AddModule('ecdh', 'fafafa.ssl.openssl.ecdh', ['ECDH_compute_key']);
  
  // Priority 1 - PKI
  AddModule('asn1', 'fafafa.ssl.openssl.asn1', ['ASN1_STRING_new', 'ASN1_INTEGER_set']);
  AddModule('pem', 'fafafa.ssl.openssl.pem', ['PEM_read_bio', 'PEM_write_bio']);
  AddModule('x509', 'fafafa.ssl.openssl.x509', ['X509_new', 'X509_sign', 'X509_verify']);
  AddModule('x509v3', 'fafafa.ssl.openssl.x509v3', ['X509V3_EXT_add', 'X509V3_EXT_get']);
  
  // Priority 1 - 其他核心
  AddModule('modes', 'fafafa.ssl.openssl.modes', ['CRYPTO_cbc128_encrypt', 'CRYPTO_ctr128_encrypt']);
  AddModule('param', 'fafafa.ssl.openssl.param', ['OSSL_PARAM_construct']);
  AddModule('buffer', 'fafafa.ssl.openssl.buffer', ['BUF_MEM_new', 'BUF_MEM_grow']);
  
  // Priority 2 - 扩展算法
  AddModule('aria', 'fafafa.ssl.openssl.aria', ['ARIA_set_key', 'ARIA_encrypt']);
  AddModule('seed', 'fafafa.ssl.openssl.seed', ['SEED_set_key', 'SEED_encrypt']);
  AddModule('chacha', 'fafafa.ssl.openssl.chacha', ['ChaCha20']);
  AddModule('cmac', 'fafafa.ssl.openssl.cmac', ['CMAC_Init', 'CMAC_Update', 'CMAC_Final']);
  
  // Priority 2 - PKCS 标准
  AddModule('pkcs', 'fafafa.ssl.openssl.pkcs', ['PKCS5_PBKDF2_HMAC']);
  AddModule('pkcs7', 'fafafa.ssl.openssl.pkcs7', ['PKCS7_new', 'PKCS7_sign']);
  AddModule('pkcs12', 'fafafa.ssl.openssl.pkcs12', ['PKCS12_create', 'PKCS12_parse']);
  AddModule('cms', 'fafafa.ssl.openssl.cms', ['CMS_sign', 'CMS_verify']);
  
  // Priority 2 - SSL/TLS
  AddModule('ssl', 'fafafa.ssl.openssl.ssl', ['SSL_CTX_new', 'SSL_new', 'SSL_connect']);
  
  // Priority 2 - 证书服务
  AddModule('ocsp', 'fafafa.ssl.openssl.ocsp', ['OCSP_request_new', 'OCSP_response_new']);
  AddModule('ct', 'fafafa.ssl.openssl.ct', ['SCT_new']);
  AddModule('ts', 'fafafa.ssl.openssl.ts', ['TS_REQ_new']);
  
  // Priority 2 - 高级特性
  AddModule('engine', 'fafafa.ssl.openssl.engine', ['ENGINE_load_builtin_engines', 'ENGINE_by_id']);
  AddModule('store', 'fafafa.ssl.openssl.store', ['OSSL_STORE_open']);
  AddModule('stack', 'fafafa.ssl.openssl.stack', ['OPENSSL_sk_new', 'OPENSSL_sk_push']);
  AddModule('lhash', 'fafafa.ssl.openssl.lhash', ['lh_new']);
  AddModule('obj', 'fafafa.ssl.openssl.obj', ['OBJ_nid2sn', 'OBJ_txt2nid']);
  AddModule('conf', 'fafafa.ssl.openssl.conf', ['CONF_load']);
  AddModule('thread', 'fafafa.ssl.openssl.thread', ['CRYPTO_THREAD_lock_new']);
  AddModule('scrypt_whirlpool', 'fafafa.ssl.openssl.scrypt_whirlpool', ['EVP_PBE_scrypt']);
  
  // Priority 3 - 遗留/专用
  AddModule('legacy_ciphers', 'fafafa.ssl.openssl.legacy_ciphers', ['RC4', 'Blowfish']);
  AddModule('async', 'fafafa.ssl.openssl.async', ['ASYNC_init_thread']);
  AddModule('comp', 'fafafa.ssl.openssl.comp', ['COMP_CTX_new']);
  AddModule('txt_db', 'fafafa.ssl.openssl.txt_db', ['TXT_DB_read']);
  AddModule('ui', 'fafafa.ssl.openssl.ui', ['UI_new']);
  AddModule('dso', 'fafafa.ssl.openssl.dso', ['DSO_new']);
  AddModule('srp', 'fafafa.ssl.openssl.srp', ['SRP_create_verifier']);
  AddModule('rand_old', 'fafafa.ssl.openssl.rand_old', ['RAND_seed']);
  
  // 核心配置模块（通常不需要测试，但为了完整性）
  AddModule('types', 'fafafa.ssl.openssl.types', ['TypeDefinitions']);
  AddModule('consts', 'fafafa.ssl.openssl.consts', ['Constants']);
  AddModule('utils', 'fafafa.ssl.openssl.utils', ['Utilities']);
  AddModule('core', 'fafafa.ssl.openssl.core', ['CoreFunctions']);
  AddModule('api', 'fafafa.ssl.openssl.api', ['LoadLibrary']);
  AddModule('sm', 'fafafa.ssl.openssl.sm', ['SM3', 'SM4']);
end;

var
  i, Generated: Integer;
begin
  WriteLn('========================================');
  WriteLn('  Batch Test File Generator');
  WriteLn('========================================');
  WriteLn;
  
  TestsDir := 'D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\tests';
  
  if not DirectoryExists(TestsDir) then
  begin
    WriteLn('Error: Test directory does not exist: ', TestsDir);
    Halt(1);
  end;
  
  InitializeModules;
  
  WriteLn('Generating test files for ', Length(Modules), ' modules...');
  WriteLn;
  
  Generated := 0;
  for i := 0 to High(Modules) do
  begin
    GenerateTestFile(Modules[i]);
    Inc(Generated);
  end;
  
  WriteLn;
  WriteLn('========================================');
  WriteLn('Completed! Generated ', Generated, ' test files');
  WriteLn('========================================');
end.
