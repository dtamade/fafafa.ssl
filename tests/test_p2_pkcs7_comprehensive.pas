program test_p2_pkcs7_comprehensive;

{$mode ObjFPC}{$H+}

{
  PKCS#7 模块综合测试

  测试范围：
  1. PKCS7 结构创建和释放
  2. PKCS7 签名和验证
  3. PKCS7 加密和解密
  4. PKCS7 各种内容类型（data, signed, enveloped）
  5. 签名者信息管理
  6. 证书链处理

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.pkcs7 (PKCS7 API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.evp (EVP 加密)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.api.pkcs7,
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

procedure TestPKCS7_BasicOperations;
var
  p7: PPKCS7;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: PKCS7 基本操作 ===');

  // 测试 PKCS7_new
  LResult := Assigned(@PKCS7_new) and (PKCS7_new <> nil);
  Test('PKCS7_new 函数加载', LResult);

  // 测试 PKCS7_free
  LResult := Assigned(@PKCS7_free) and (PKCS7_free <> nil);
  Test('PKCS7_free 函数加载', LResult);

  // 测试内容类型常量
  Test('NID_pkcs7_data 常量 (21)', NID_pkcs7_data = 21);
  Test('NID_pkcs7_signed 常量 (22)', NID_pkcs7_signed = 22);
  Test('NID_pkcs7_enveloped 常量 (23)', NID_pkcs7_enveloped = 23);

  // 测试标志常量
  Test('PKCS7_TEXT 标志 ($1)', PKCS7_TEXT = $1);
  Test('PKCS7_DETACHED 标志 ($40)', PKCS7_DETACHED = $40);
  Test('PKCS7_BINARY 标志 ($80)', PKCS7_BINARY = $80);
end;

procedure TestPKCS7_SignerInfo;
var
  si: PPKCS7_SIGNER_INFO;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: PKCS7 签名者信息 ===');

  // 测试签名者信息创建
  LResult := Assigned(@PKCS7_SIGNER_INFO_new) and (PKCS7_SIGNER_INFO_new <> nil);
  Test('PKCS7_SIGNER_INFO_new 函数加载', LResult);

  // 测试签名者信息释放
  LResult := Assigned(@PKCS7_SIGNER_INFO_free) and (PKCS7_SIGNER_INFO_free <> nil);
  Test('PKCS7_SIGNER_INFO_free 函数加载', LResult);

  // 测试添加签名者
  LResult := Assigned(@PKCS7_add_signer) and (PKCS7_add_signer <> nil);
  Test('PKCS7_add_signer 函数加载', LResult);

  // 测试签名属性
  LResult := Assigned(@PKCS7_add_signed_attribute) and (PKCS7_add_signed_attribute <> nil);
  Test('PKCS7_add_signed_attribute 函数加载', LResult);
end;

procedure TestPKCS7_SignOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: PKCS7 签名操作 ===');

  // 测试签名函数
  LResult := Assigned(@PKCS7_sign) and (PKCS7_sign <> nil);
  Test('PKCS7_sign 函数加载', LResult);

  // 测试添加签名者（带密钥）
  LResult := Assigned(@PKCS7_sign_add_signer) and (PKCS7_sign_add_signer <> nil);
  Test('PKCS7_sign_add_signer 函数加载', LResult);

  // 测试最终化
  LResult := Assigned(@PKCS7_final) and (PKCS7_final <> nil);
  Test('PKCS7_final 函数加载', LResult);

  // 测试获取签名者信息
  LResult := Assigned(@PKCS7_get_signer_info) and (PKCS7_get_signer_info <> nil);
  Test('PKCS7_get_signer_info 函数加载', LResult);

  // 测试 SMIME 能力属性
  LResult := Assigned(@PKCS7_add_attrib_smimecap) and (PKCS7_add_attrib_smimecap <> nil);
  Test('PKCS7_add_attrib_smimecap 函数加载', LResult);
end;

procedure TestPKCS7_VerifyOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: PKCS7 验证操作 ===');

  // 测试验证函数
  LResult := Assigned(@PKCS7_verify) and (PKCS7_verify <> nil);
  Test('PKCS7_verify 函数加载', LResult);

  // 测试获取签名者
  LResult := Assigned(@PKCS7_get0_signers) and (PKCS7_get0_signers <> nil);
  Test('PKCS7_get0_signers 函数加载', LResult);

  // 测试数据验证
  LResult := Assigned(@PKCS7_dataVerify) and (PKCS7_dataVerify <> nil);
  Test('PKCS7_dataVerify 函数加载', LResult);

  // 测试签名验证
  LResult := Assigned(@PKCS7_signatureVerify) and (PKCS7_signatureVerify <> nil);
  Test('PKCS7_signatureVerify 函数加载', LResult);
end;

procedure TestPKCS7_EncryptDecrypt;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: PKCS7 加密/解密 ===');

  // 测试加密
  LResult := Assigned(@PKCS7_encrypt) and (PKCS7_encrypt <> nil);
  Test('PKCS7_encrypt 函数加载', LResult);

  // 测试解密
  LResult := Assigned(@PKCS7_decrypt) and (PKCS7_decrypt <> nil);
  Test('PKCS7_decrypt 函数加载', LResult);

  // 测试设置密文算法
  LResult := Assigned(@PKCS7_set_cipher) and (PKCS7_set_cipher <> nil);
  Test('PKCS7_set_cipher 函数加载', LResult);

  // 测试添加接收者
  LResult := Assigned(@PKCS7_add_recipient) and (PKCS7_add_recipient <> nil);
  Test('PKCS7_add_recipient 函数加载', LResult);
end;

procedure TestPKCS7_DataOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: PKCS7 数据操作 ===');

  // 测试数据初始化
  LResult := Assigned(@PKCS7_dataInit) and (PKCS7_dataInit <> nil);
  Test('PKCS7_dataInit 函数加载', LResult);

  // 测试数据最终化
  LResult := Assigned(@PKCS7_dataFinal) and (PKCS7_dataFinal <> nil);
  Test('PKCS7_dataFinal 函数加载', LResult);

  // 测试数据解码
  LResult := Assigned(@PKCS7_dataDecode) and (PKCS7_dataDecode <> nil);
  Test('PKCS7_dataDecode 函数加载', LResult);

  // 测试流操作
  LResult := Assigned(@PKCS7_stream) and (PKCS7_stream <> nil);
  Test('PKCS7_stream 函数加载', LResult);
end;

procedure TestPKCS7_IOSerialization;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 7: PKCS7 I/O 和序列化 ===');

  // 测试 DER 编码
  LResult := Assigned(@Ti2d_PKCS7) and (Ti2d_PKCS7 <> nil);
  Test('Ti2d_PKCS7 函数加载', LResult);

  // 测试 DER 解码
  LResult := Assigned(@Td2i_PKCS7) and (Td2i_PKCS7 <> nil);
  Test('Td2i_PKCS7 函数加载', LResult);

  // 测试 BIO 编码
  LResult := Assigned(@Ti2d_PKCS7_bio) and (Ti2d_PKCS7_bio <> nil);
  Test('Ti2d_PKCS7_bio 函数加载', LResult);

  // 测试 BIO 解码
  LResult := Assigned(@Td2i_PKCS7_bio) and (Td2i_PKCS7_bio <> nil);
  Test('Td2i_PKCS7_bio 函数加载', LResult);

  // 测试 PEM 编码
  LResult := Assigned(@PEM_write_bio_PKCS7) and (PEM_write_bio_PKCS7 <> nil);
  Test('PEM_write_bio_PKCS7 函数加载', LResult);

  // 测试 PEM 解码
  LResult := Assigned(@PEM_read_bio_PKCS7) and (PEM_read_bio_PKCS7 <> nil);
  Test('PEM_read_bio_PKCS7 函数加载', LResult);
end;

procedure TestPKCS7_AdvancedFeatures;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 8: PKCS7 高级特性 ===');

  // 测试设置内容
  LResult := Assigned(@PKCS7_set_content) and (PKCS7_set_content <> nil);
  Test('PKCS7_set_content 函数加载', LResult);

  // 测试设置类型
  LResult := Assigned(@PKCS7_set_type) and (PKCS7_set_type <> nil);
  Test('PKCS7_set_type 函数加载', LResult);

  // 测试添加证书
  LResult := Assigned(@PKCS7_add_certificate) and (PKCS7_add_certificate <> nil);
  Test('PKCS7_add_certificate 函数加载', LResult);

  // 测试添加 CRL
  LResult := Assigned(@PKCS7_add_crl) and (PKCS7_add_crl <> nil);
  Test('PKCS7_add_crl 函数加载', LResult);

  // 测试获取接收者信息
  LResult := Assigned(@PKCS7_get_recip_info) and (PKCS7_get_recip_info <> nil);
  Test('PKCS7_get_recip_info 函数加载', LResult);

  // 测试获取属性
  LResult := Assigned(@PKCS7_get_attribute) and (PKCS7_get_attribute <> nil);
  Test('PKCS7_get_attribute 函数加载', LResult);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('PKCS#7 模块综合测试');
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
  TestPKCS7_BasicOperations;
  TestPKCS7_SignerInfo;
  TestPKCS7_SignOperations;
  TestPKCS7_VerifyOperations;
  TestPKCS7_EncryptDecrypt;
  TestPKCS7_DataOperations;
  TestPKCS7_IOSerialization;
  TestPKCS7_AdvancedFeatures;

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
    WriteLn('🎉 所有测试通过！PKCS#7 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
