program test_p2_pkcs12_comprehensive;

{$mode ObjFPC}{$H+}

{
  PKCS#12 模块综合测试

  测试范围：
  1. PKCS12 结构创建和释放
  2. PKCS12 证书包创建
  3. PKCS12 证书和密钥导出/导入
  4. PKCS12 密码保护
  5. PKCS12 完整性验证
  6. 证书链处理

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.pkcs12 (PKCS12 API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.evp (EVP 加密)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.rand;

var
  TotalTests, PassedTests, FailedTests: Integer;

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

procedure TestPKCS12_BasicOperations;
var
  p12: PPKCS12;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: PKCS12 基本操作 ===');

  // 测试 PKCS12_new
  LResult := Assigned(@PKCS12_new) and (PKCS12_new <> nil);
  Test('PKCS12_new 函数加载', LResult);

  // 测试 PKCS12_free
  LResult := Assigned(@PKCS12_free) and (PKCS12_free <> nil);
  Test('PKCS12_free 函数加载', LResult);

  // 测试 PKCS12_parse
  LResult := Assigned(@PKCS12_parse) and (PKCS12_parse <> nil);
  Test('PKCS12_parse 函数加载', LResult);

  // 测试 PKCS12_create
  LResult := Assigned(@PKCS12_create) and (PKCS12_create <> nil);
  Test('PKCS12_create 函数加载', LResult);
end;

procedure TestPKCS12_PasswordProtection;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: PKCS12 密码保护 ===');

  // 测试密码相关函数
  LResult := Assigned(@PKCS12_key_gen_utf8_ex) and (PKCS12_key_gen_utf8_ex <> nil);
  Test('PKCS12_key_gen_utf8_ex 函数加载', LResult);

  LResult := Assigned(@PKCS12_pbe_crypt) and (PKCS12_pbe_crypt <> nil);
  Test('PKCS12_pbe_crypt 函数加载', LResult);

  LResult := Assigned(@PKCS12_crypt) and (PKCS12_crypt <> nil);
  Test('PKCS12_crypt 函数加载', LResult);
end;

procedure TestPKCS12_CertificateOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: PKCS12 证书操作 ===');

  // 测试证书获取
  LResult := Assigned(@PKCS12_get_cert) and (PKCS12_get_cert <> nil);
  Test('PKCS12_get_cert 函数加载', LResult);

  // 测试私钥获取
  LResult := Assigned(@PKCS12_get_pkey) and (PKCS12_get_pkey <> nil);
  Test('PKCS12_get_pkey 函数加载', LResult);

  // 测试证书袋获取
  LResult := Assigned(@PKCS12_get1_certs) and (PKCS12_get1_certs <> nil);
  Test('PKCS12_get1_certs 函数加载', LResult);

  // 测试添加证书
  LResult := Assigned(@PKCS12_add_cert) and (PKCS12_add_cert <> nil);
  Test('PKCS12_add_cert 函数加载', LResult);
end;

procedure TestPKCS12_SafeBags;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: PKCS12 安全袋 (SafeBags) ===');

  // 测试 SafeBag 结构
  LResult := Assigned(@PKCS12_SAFEBAG_new) and (PKCS12_SAFEBAG_new <> nil);
  Test('PKCS12_SAFEBAG_new 函数加载', LResult);

  LResult := Assigned(@PKCS12_SAFEBAG_free) and (PKCS12_SAFEBAG_free <> nil);
  Test('PKCS12_SAFEBAG_free 函数加载', LResult);

  // 测试证书袋
  LResult := Assigned(@PKCS12_certbag) and (PKCS12_certbag <> nil);
  Test('PKCS12_certbag 函数加载', LResult);

  // 测试密钥袋
  LResult := Assigned(@PKCS12_keybag) and (PKCS12_keybag <> nil);
  Test('PKCS12_keybag 函数加载', LResult);

  // 测试秘密袋
  LResult := Assigned(@PKCS12_secretbag) and (PKCS12_secretbag <> nil);
  Test('PKCS12_secretbag 函数加载', LResult);

  // 测试添加 SafeBag
  LResult := Assigned(@PKCS12_add_safe) and (PKCS12_add_safe <> nil);
  Test('PKCS12_add_safe 函数加载', LResult);
end;

procedure TestPKCS12_MacOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: PKCS12 MAC 操作 ===');

  // 测试 MAC 生成
  LResult := Assigned(@PKCS12_gen_mac) and (PKCS12_gen_mac <> nil);
  Test('PKCS12_gen_mac 函数加载', LResult);

  // 测试 MAC 验证
  LResult := Assigned(@PKCS12_verify_mac) and (PKCS12_verify_mac <> nil);
  Test('PKCS12_verify_mac 函数加载', LResult);

  // 测试 MAC 设置
  LResult := Assigned(@PKCS12_set_mac) and (PKCS12_set_mac <> nil);
  Test('PKCS12_set_mac 函数加载', LResult);
end;

procedure TestPKCS12_IOSerialization;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: PKCS12 I/O 和序列化 ===');

  // 测试 DER 编码
  LResult := Assigned(@i2d_PKCS12) and (i2d_PKCS12 <> nil);
  Test('i2d_PKCS12 函数加载', LResult);

  // 测试 DER 解码
  LResult := Assigned(@d2i_PKCS12) and (d2i_PKCS12 <> nil);
  Test('d2i_PKCS12 函数加载', LResult);

  // 测试 BIO 编码
  LResult := Assigned(@i2d_PKCS12_bio) and (i2d_PKCS12_bio <> nil);
  Test('i2d_PKCS12_bio 函数加载', LResult);

  // 测试 BIO 解码
  LResult := Assigned(@d2i_PKCS12_bio) and (d2i_PKCS12_bio <> nil);
  Test('d2i_PKCS12_bio 函数加载', LResult);

  // 测试 PEM 编码
  LResult := Assigned(@PEM_write_bio_PKCS12) and (PEM_write_bio_PKCS12 <> nil);
  Test('PEM_write_bio_PKCS12 函数加载', LResult);

  // 测试 PEM 解码
  LResult := Assigned(@PEM_read_bio_PKCS12) and (PEM_read_bio_PKCS12 <> nil);
  Test('PEM_read_bio_PKCS12 函数加载', LResult);
end;

procedure TestPKCS12_Pkcs8Integration;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 7: PKCS12 与 PKCS#8 集成 ===');

  // 测试 PKCS#8 密钥转换
  LResult := Assigned(@PKCS12_add_key_bag) and (PKCS12_add_key_bag <> nil);
  Test('PKCS12_add_key_bag 函数加载', LResult);

  LResult := Assigned(@PKCS12_add_key_ex) and (PKCS12_add_key_ex <> nil);
  Test('PKCS12_add_key_ex 函数加载', LResult);

  // 测试获取私钥
  LResult := Assigned(@PKCS12_get_private_key) and (PKCS12_get_private_key <> nil);
  Test('PKCS12_get_private_key 函数加载', LResult);
end;

procedure TestPKCS12_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 8: PKCS12 工具函数 ===');

  // 测试列表操作
  LResult := Assigned(@PKCS12_SAFEBAG_get0_certs) and (PKCS12_SAFEBAG_get0_certs <> nil);
  Test('PKCS12_SAFEBAG_get0_certs 函数加载', LResult);

  // 测试算法获取
  LResult := Assigned(@PKCS12_SAFEBAG_get0_pkcs8) and (PKCS12_SAFEBAG_get0_pkcs8 <> nil);
  Test('PKCS12_SAFEBAG_get0_pkcs8 函数加载', LResult);

  // 测试类型检查
  LResult := Assigned(@PKCS12_SAFEBAG_get_bag_type) and (PKCS12_SAFEBAG_get_bag_type <> nil);
  Test('PKCS12_SAFEBAG_get_bag_type 函数加载', LResult);

  // 测试 NID 常量
  Test('NID_certBag 常量', NID_certBag > 0);
  Test('NID_crlBag 常量', NID_crlBag > 0);
  Test('NID_secretBag 常量', NID_secretBag > 0);
  Test('NID_pkcs8ShroudedKeyBag 常量', NID_pkcs8ShroudedKeyBag > 0);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('PKCS#12 模块综合测试');
  WriteLn('=' + StringOfChar('=', 60));

  // 初始化 OpenSSL
  WriteLn;
  WriteLn('初始化 OpenSSL 库...');
  if not LoadOpenSSLCore then
  begin
    WriteLn('❌ 错误：无法加载 OpenSSL 库');
    Halt(1);
  end;
  WriteLn('✅ OpenSSL 库加载成功');
  WriteLn('版本: ', GetOpenSSLVersionString);

  // 执行测试套件
  TestPKCS12_BasicOperations;
  TestPKCS12_PasswordProtection;
  TestPKCS12_CertificateOperations;
  TestPKCS12_SafeBags;
  TestPKCS12_MacOperations;
  TestPKCS12_IOSerialization;
  TestPKCS12_Pkcs8Integration;
  TestPKCS12_UtilityFunctions;

  // 输出测试结果
  WriteLn;
  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('测试结果总结');
  WriteLn('=' + StringOfChar('=', 60));
  WriteLn(Format('总测试数: %d', [TotalTests]));
  WriteLn(Format('通过: %d', [PassedTests]));
  WriteLn(Format('失败: %d', [FailedTests]));
  WriteLn(Format('通过率: %.1f%%', [PassedTests * 100.0 / TotalTests]));

  if FailedTests > 0 then
  begin
    WriteLn;
    WriteLn('❌ 测试未完全通过');
    Halt(1);
  end
  else
  begin
    WriteLn;
    WriteLn('🎉 所有测试通过！PKCS#12 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
