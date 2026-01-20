program test_p2_store_comprehensive;

{$mode ObjFPC}{$H+}

{
  STORE (存储) 模块综合测试

  测试范围：
  1. OSSL_STORE_INFO 结构和类型
  2. OSSL_STORE_CTX 操作
  3. OSSL_STORE_SEARCH 功能
  4. OSSL_STORE_LOADER 加载器

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.store (STORE API)
  - fafafa.ssl.openssl.loader (版本检测)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.store,
  fafafa.ssl.openssl.loader;

var
  TotalTests, PassedTests, FailedTests: Integer;
  IsOpenSSL3: Boolean;

procedure Test(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(TestName + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FailedTests);
  end;
end;

procedure TestSTORE_InfoStructures;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: STORE INFO 结构 ===');

  // 测试 STORE_INFO 基本函数
  LResult := Assigned(@OSSL_STORE_INFO_get_type) and (OSSL_STORE_INFO_get_type <> nil);
  Test('OSSL_STORE_INFO_get_type 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_type_string) and (OSSL_STORE_INFO_type_string <> nil);
  Test('OSSL_STORE_INFO_type_string 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_free) and (OSSL_STORE_INFO_free <> nil);
  Test('OSSL_STORE_INFO_free 函数加载', LResult);
end;

procedure TestSTORE_InfoTypes;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: STORE INFO 类型操作 ===');

  // 测试 NAME 类型
  LResult := Assigned(@OSSL_STORE_INFO_new_NAME) and (OSSL_STORE_INFO_new_NAME <> nil);
  Test('OSSL_STORE_INFO_new_NAME 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_NAME) and (OSSL_STORE_INFO_get0_NAME <> nil);
  Test('OSSL_STORE_INFO_get0_NAME 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_NAME_description) and (OSSL_STORE_INFO_get0_NAME_description <> nil);
  Test('OSSL_STORE_INFO_get0_NAME_description 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_set0_NAME_description) and (OSSL_STORE_INFO_set0_NAME_description <> nil);
  Test('OSSL_STORE_INFO_set0_NAME_description 函数加载', LResult);

  // 测试 CERT 类型
  LResult := Assigned(@OSSL_STORE_INFO_new_CERT) and (OSSL_STORE_INFO_new_CERT <> nil);
  Test('OSSL_STORE_INFO_new_CERT 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_CERT) and (OSSL_STORE_INFO_get0_CERT <> nil);
  Test('OSSL_STORE_INFO_get0_CERT 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get1_CERT) and (OSSL_STORE_INFO_get1_CERT <> nil);
  Test('OSSL_STORE_INFO_get1_CERT 函数加载', LResult);

  // 测试 PKEY 类型
  LResult := Assigned(@OSSL_STORE_INFO_new_PKEY) and (OSSL_STORE_INFO_new_PKEY <> nil);
  Test('OSSL_STORE_INFO_new_PKEY 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_PKEY) and (OSSL_STORE_INFO_get0_PKEY <> nil);
  Test('OSSL_STORE_INFO_get0_PKEY 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get1_PKEY) and (OSSL_STORE_INFO_get1_PKEY <> nil);
  Test('OSSL_STORE_INFO_get1_PKEY 函数加载', LResult);

  // 测试 PUBKEY 类型
  LResult := Assigned(@OSSL_STORE_INFO_new_PUBKEY) and (OSSL_STORE_INFO_new_PUBKEY <> nil);
  Test('OSSL_STORE_INFO_new_PUBKEY 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_PUBKEY) and (OSSL_STORE_INFO_get0_PUBKEY <> nil);
  Test('OSSL_STORE_INFO_get0_PUBKEY 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get1_PUBKEY) and (OSSL_STORE_INFO_get1_PUBKEY <> nil);
  Test('OSSL_STORE_INFO_get1_PUBKEY 函数加载', LResult);

  // 测试 CRL 类型
  LResult := Assigned(@OSSL_STORE_INFO_new_CRL) and (OSSL_STORE_INFO_new_CRL <> nil);
  Test('OSSL_STORE_INFO_new_CRL 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get0_CRL) and (OSSL_STORE_INFO_get0_CRL <> nil);
  Test('OSSL_STORE_INFO_get0_CRL 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_INFO_get1_CRL) and (OSSL_STORE_INFO_get1_CRL <> nil);
  Test('OSSL_STORE_INFO_get1_CRL 函数加载', LResult);
end;

procedure TestSTORE_CTXOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: STORE CTX 操作 ===');

  // 测试 CTX 打开和关闭
  LResult := Assigned(@OSSL_STORE_open) and (OSSL_STORE_open <> nil);
  Test('OSSL_STORE_open 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_open_ex) and (OSSL_STORE_open_ex <> nil);
  Test('OSSL_STORE_open_ex 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_close) and (OSSL_STORE_close <> nil);
  Test('OSSL_STORE_close 函数加载', LResult);

  // 测试 CTX 加载和状态
  LResult := Assigned(@OSSL_STORE_load) and (OSSL_STORE_load <> nil);
  Test('OSSL_STORE_load 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_eof) and (OSSL_STORE_eof <> nil);
  Test('OSSL_STORE_eof 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_error) and (OSSL_STORE_error <> nil);
  Test('OSSL_STORE_error 函数加载', LResult);

  // 测试 CTX 控制
  LResult := Assigned(@OSSL_STORE_ctrl) and (OSSL_STORE_ctrl <> nil);
  Test('OSSL_STORE_ctrl 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_vctrl) and (OSSL_STORE_vctrl <> nil);
  Test('OSSL_STORE_vctrl 函数加载', LResult);
end;

procedure TestSTORE_SearchOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: STORE SEARCH 操作 ===');

  // 测试搜索创建函数
  LResult := Assigned(@OSSL_STORE_SEARCH_by_name_func) and (OSSL_STORE_SEARCH_by_name_func <> nil);
  Test('OSSL_STORE_SEARCH_by_name 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_SEARCH_by_issuer_serial_func) and (OSSL_STORE_SEARCH_by_issuer_serial_func <> nil);
  Test('OSSL_STORE_SEARCH_by_issuer_serial 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_SEARCH_by_key_fingerprint_func) and (OSSL_STORE_SEARCH_by_key_fingerprint_func <> nil);
  Test('OSSL_STORE_SEARCH_by_key_fingerprint 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_SEARCH_by_alias_func) and (OSSL_STORE_SEARCH_by_alias_func <> nil);
  Test('OSSL_STORE_SEARCH_by_alias 函数加载', LResult);

  // 测试搜索释放
  LResult := Assigned(@OSSL_STORE_SEARCH_free) and (OSSL_STORE_SEARCH_free <> nil);
  Test('OSSL_STORE_SEARCH_free 函数加载', LResult);

  // 测试搜索支持和执行
  LResult := Assigned(@OSSL_STORE_supports_search) and (OSSL_STORE_supports_search <> nil);
  Test('OSSL_STORE_supports_search 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_find) and (OSSL_STORE_find <> nil);
  Test('OSSL_STORE_find 函数加载', LResult);
end;

procedure TestSTORE_LoaderOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: STORE LOADER 操作 ===');

  // 测试 LOADER 创建和释放
  LResult := Assigned(@OSSL_STORE_LOADER_new) and (OSSL_STORE_LOADER_new <> nil);
  Test('OSSL_STORE_LOADER_new 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_LOADER_free) and (OSSL_STORE_LOADER_free <> nil);
  Test('OSSL_STORE_LOADER_free 函数加载', LResult);

  // 测试 LOADER 设置函数
  LResult := Assigned(@OSSL_STORE_LOADER_set_open) and (OSSL_STORE_LOADER_set_open <> nil);
  Test('OSSL_STORE_LOADER_set_open 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_LOADER_set_open_ex) and (OSSL_STORE_LOADER_set_open_ex <> nil);
  Test('OSSL_STORE_LOADER_set_open_ex 函数加载', LResult);

  // OpenSSL 1.x only functions - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@OSSL_STORE_LOADER_set_attach) and (OSSL_STORE_LOADER_set_attach <> nil);
    Test('OSSL_STORE_LOADER_set_attach 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_ctrl) and (OSSL_STORE_LOADER_set_ctrl <> nil);
    Test('OSSL_STORE_LOADER_set_ctrl 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_expect) and (OSSL_STORE_LOADER_set_expect <> nil);
    Test('OSSL_STORE_LOADER_set_expect 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_find) and (OSSL_STORE_LOADER_set_find <> nil);
    Test('OSSL_STORE_LOADER_set_find 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_load) and (OSSL_STORE_LOADER_set_load <> nil);
    Test('OSSL_STORE_LOADER_set_load 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_eof) and (OSSL_STORE_LOADER_set_eof <> nil);
    Test('OSSL_STORE_LOADER_set_eof 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_error) and (OSSL_STORE_LOADER_set_error <> nil);
    Test('OSSL_STORE_LOADER_set_error 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OSSL_STORE_LOADER_set_close) and (OSSL_STORE_LOADER_set_close <> nil);
    Test('OSSL_STORE_LOADER_set_close 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试 LOADER 注册
  LResult := Assigned(@OSSL_STORE_register_loader) and (OSSL_STORE_register_loader <> nil);
  Test('OSSL_STORE_register_loader 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_unregister_loader) and (OSSL_STORE_unregister_loader <> nil);
  Test('OSSL_STORE_unregister_loader 函数加载', LResult);
end;

procedure TestSTORE_ExpectOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: STORE EXPECT 操作 ===');

  // 测试 expect 函数
  LResult := Assigned(@OSSL_STORE_expect) and (OSSL_STORE_expect <> nil);
  Test('OSSL_STORE_expect 函数加载', LResult);

  LResult := Assigned(@OSSL_STORE_supports_search) and (OSSL_STORE_supports_search <> nil);
  Test('OSSL_STORE_supports_search 函数加载', LResult);
end;

procedure TestSTORE_Constants;
begin
  WriteLn;
  WriteLn('=== 测试 7: STORE 常量定义 ===');

  // 测试 INFO 类型常量
  Test('OSSL_STORE_INFO_NAME (1)', OSSL_STORE_INFO_NAME = 1);
  Test('OSSL_STORE_INFO_PARAMS (2)', OSSL_STORE_INFO_PARAMS = 2);
  Test('OSSL_STORE_INFO_PUBKEY (3)', OSSL_STORE_INFO_PUBKEY = 3);
  Test('OSSL_STORE_INFO_PKEY (4)', OSSL_STORE_INFO_PKEY = 4);
  Test('OSSL_STORE_INFO_CERT (5)', OSSL_STORE_INFO_CERT = 5);
  Test('OSSL_STORE_INFO_CRL (6)', OSSL_STORE_INFO_CRL = 6);

  // 测试 SEARCH 类型常量
  Test('OSSL_STORE_SEARCH_BY_NAME (1)', OSSL_STORE_SEARCH_BY_NAME = 1);
  Test('OSSL_STORE_SEARCH_BY_ISSUER_SERIAL (2)', OSSL_STORE_SEARCH_BY_ISSUER_SERIAL = 2);
  Test('OSSL_STORE_SEARCH_BY_KEY_FINGERPRINT (3)', OSSL_STORE_SEARCH_BY_KEY_FINGERPRINT = 3);
  Test('OSSL_STORE_SEARCH_BY_ALIAS (4)', OSSL_STORE_SEARCH_BY_ALIAS = 4);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('STORE (存储) 模块综合测试');
  WriteLn('=' + StringOfChar('=', 60));

  // 初始化 OpenSSL
  WriteLn;
  WriteLn('初始化 OpenSSL 库...');
  try
    LoadOpenSSLCore;
    WriteLn('✅ OpenSSL 库加载成功');
    WriteLn('版本: ', GetOpenSSLVersionString);

    // 检测 OpenSSL 版本
    IsOpenSSL3 := TOpenSSLLoader.IsOpenSSL3;
    if IsOpenSSL3 then
      WriteLn('检测到 OpenSSL 3.x - 将跳过不兼容的函数测试')
    else
      WriteLn('检测到 OpenSSL 1.x - 将测试所有函数');
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误：无法加载 OpenSSL 库: ', E.Message);
      Halt(1);
    end;
  end;

  // 加载 STORE 模块
  WriteLn;
  WriteLn('加载 STORE 模块...');
  try
    LoadSTOREFunctions;
    WriteLn('✅ STORE 模块加载成功');
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误：无法加载 STORE 模块: ', E.Message);
      Halt(1);
    end;
  end;

  // 运行测试
  TestSTORE_InfoStructures;
  TestSTORE_InfoTypes;
  TestSTORE_CTXOperations;
  TestSTORE_SearchOperations;
  TestSTORE_LoaderOperations;
  TestSTORE_ExpectOperations;
  TestSTORE_Constants;

  // 输出测试结果
  WriteLn;
  WriteLn('=============================================================');
  WriteLn('测试结果总结');
  WriteLn('=============================================================');
  WriteLn('总测试数: ', TotalTests);
  WriteLn('通过: ', PassedTests);
  WriteLn('失败: ', FailedTests);
  WriteLn('通过率: ', Format('%.1f', [PassedTests * 100.0 / TotalTests]), '%');
  WriteLn;

  if FailedTests = 0 then
  begin
    WriteLn('🎉 所有测试通过！STORE 模块工作正常');
    Halt(0);
  end
  else
  begin
    WriteLn('❌ 有 ', FailedTests, ' 个测试失败');
    Halt(1);
  end;
end.
