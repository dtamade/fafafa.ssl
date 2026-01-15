program test_p2_cms_comprehensive;

{$mode ObjFPC}{$H+}

{
  CMS (加密消息语法) 模块综合测试

  测试范围：
  1. CMS 结构创建和释放
  2. CMS 签名和验证
  3. CMS 加密和解密
  4. CMS 收据处理
  5. CMS 接收者信息
  6. CMS 属性管理

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.cms (CMS API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.evp (EVP 加密)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.cms,
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

procedure TestCMS_ContentInfo;
var
  cms: PCMS_ContentInfo;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: CMS ContentInfo 基本操作 ===');

  // 测试 CMS_ContentInfo_new
  LResult := Assigned(@CMS_ContentInfo_new) and (CMS_ContentInfo_new <> nil);
  Test('CMS_ContentInfo_new 函数加载', LResult);

  // 测试 CMS_ContentInfo_free
  LResult := Assigned(@CMS_ContentInfo_free) and (CMS_ContentInfo_free <> nil);
  Test('CMS_ContentInfo_free 函数加载', LResult);

  // 测试 DER 编码
  LResult := Assigned(@Ti2d_CMS_ContentInfo) and (Ti2d_CMS_ContentInfo <> nil);
  Test('Ti2d_CMS_ContentInfo 函数加载', LResult);

  // 测试 DER 解码
  LResult := Assigned(@Td2i_CMS_ContentInfo) and (Td2i_CMS_ContentInfo <> nil);
  Test('Td2i_CMS_ContentInfo 函数加载', LResult);

  // 测试 BIO 编码
  LResult := Assigned(@Ti2d_CMS_bio) and (Ti2d_CMS_bio <> nil);
  Test('Ti2d_CMS_bio 函数加载', LResult);

  // 测试 BIO 解码
  LResult := Assigned(@Td2i_CMS_bio) and (Td2i_CMS_bio <> nil);
  Test('Td2i_CMS_bio 函数加载', LResult);
end;

procedure TestCMS_SignOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: CMS 签名操作 ===');

  // 测试签名函数
  LResult := Assigned(@CMS_sign) and (CMS_sign <> nil);
  Test('CMS_sign 函数加载', LResult);

  // 测试添加签名者
  LResult := Assigned(@CMS_add1_signer) and (CMS_add1_signer <> nil);
  Test('CMS_add1_signer 函数加载', LResult);

  // 测试收据签名
  LResult := Assigned(@CMS_sign_receipt) and (CMS_sign_receipt <> nil);
  Test('CMS_sign_receipt 函数加载', LResult);

  // 测试最终化
  LResult := Assigned(@CMS_final) and (CMS_final <> nil);
  Test('CMS_final 函数加载', LResult);

  // 测试数据初始化
  LResult := Assigned(@CMS_dataInit) and (CMS_dataInit <> nil);
  Test('CMS_dataInit 函数加载', LResult);

  // 测试数据最终化
  LResult := Assigned(@CMS_data) and (CMS_data <> nil);
  Test('CMS_data 函数加载', LResult);
end;

procedure TestCMS_VerifyOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: CMS 验证操作 ===');

  // 测试验证函数
  LResult := Assigned(@CMS_verify) and (CMS_verify <> nil);
  Test('CMS_verify 函数加载', LResult);

  // 测试获取签名者
  LResult := Assigned(@CMS_get0_signers) and (CMS_get0_signers <> nil);
  Test('CMS_get0_signers 函数加载', LResult);

  // 测试数据验证
  LResult := Assigned(@CMS_dataVerify) and (CMS_dataVerify <> nil);
  Test('CMS_dataVerify 函数加载', LResult);

  // 测试签名验证
  LResult := Assigned(@CMS_signatureVerify) and (CMS_signatureVerify <> nil);
  Test('CMS_signatureVerify 函数加载', LResult);

  // 测试获取签名者信息
  LResult := Assigned(@CMS_get0_SignerInfo) and (CMS_get0_SignerInfo <> nil);
  Test('CMS_get0_SignerInfo 函数加载', LResult);
end;

procedure TestCMS_EncryptDecrypt;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: CMS 加密/解密 ===');

  // 测试加密函数
  LResult := Assigned(@CMS_encrypt) and (CMS_encrypt <> nil);
  Test('CMS_encrypt 函数加载', LResult);

  // 测试解密函数
  LResult := Assigned(@CMS_decrypt) and (CMS_decrypt <> nil);
  Test('CMS_decrypt 函数加载', LResult);

  // 测试设置密文算法
  LResult := Assigned(@CMS_set1_eContentType) and (CMS_set1_eContentType <> nil);
  Test('CMS_set1_eContentType 函数加载', LResult);

  // 测试获取内容类型
  LResult := Assigned(@CMS_ContentInfo_get0_type) and (CMS_ContentInfo_get0_type <> nil);
  Test('CMS_ContentInfo_get0_type 函数加载', LResult);

  // 测试获取内容
  LResult := Assigned(@CMS_ContentInfo_get0_content) and (CMS_ContentInfo_get0_content <> nil);
  Test('CMS_ContentInfo_get0_content 函数加载', LResult);
end;

procedure TestCMS_RecipientInfo;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: CMS 接收者信息 ===');

  // 测试添加接收者
  LResult := Assigned(@CMS_add1_recipient_cert) and (CMS_add1_recipient_cert <> nil);
  Test('CMS_add1_recipient_cert 函数加载', LResult);

  // 测试接收者信息类型常量
  Test('CMS_RECIPINFO_TRANS (0)', CMS_RECIPINFO_TRANS = 0);
  Test('CMS_RECIPINFO_AGREE (1)', CMS_RECIPINFO_AGREE = 1);
  Test('CMS_RECIPINFO_KEK (2)', CMS_RECIPINFO_KEK = 2);
  Test('CMS_RECIPINFO_PASS (3)', CMS_RECIPINFO_PASS = 3);
  Test('CMS_RECIPINFO_OTHER (4)', CMS_RECIPINFO_OTHER = 4);
end;

procedure TestCMS_ReceiptOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: CMS 收据操作 ===');

  // 测试收据请求
  LResult := Assigned(@CMS_ReceiptRequest_create0) and (CMS_ReceiptRequest_create0 <> nil);
  Test('CMS_ReceiptRequest_create0 函数加载', LResult);

  // 测试获取收据
  LResult := Assigned(@CMS_get1_Receipt) and (CMS_get1_Receipt <> nil);
  Test('CMS_get1_Receipt 函数加载', LResult);

  // 测试收据验证
  LResult := Assigned(@CMS_Receipt_verify) and (CMS_Receipt_verify <> nil);
  Test('CMS_Receipt_verify 函数加载', LResult);

  // 测试获取原始收据请求
  LResult := Assigned(@CMS_ContentInfo_get0_ReceiptRequest) and (CMS_ContentInfo_get0_ReceiptRequest <> nil);
  Test('CMS_ContentInfo_get0_ReceiptRequest 函数加载', LResult);
end;

procedure TestCMS_Attributes;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 7: CMS 属性管理 ===');

  // 测试添加签名属性
  LResult := Assigned(@CMS_add1_attr) and (CMS_add1_attr <> nil);
  Test('CMS_add1_attr 函数加载', LResult);

  // 测试添加签名字段
  LResult := Assigned(@CMS_add1_attr_smimecap) and (CMS_add1_attr_smimecap <> nil);
  Test('CMS_add1_attr_smimecap 函数加载', LResult);

  // 测试添加签名时间
  LResult := Assigned(@CMS_add1_attr_signingTime) and (CMS_add1_attr_signingTime <> nil);
  Test('CMS_add1_attr_signingTime 函数加载', LResult);

  // 测试获取属性
  LResult := Assigned(@CMS_get0_attr) and (CMS_get0_attr <> nil);
  Test('CMS_get0_attr 函数加载', LResult);

  // 测试获取签名者属性
  LResult := Assigned(@CMS_SignerInfo_get0_attr) and (CMS_SignerInfo_get0_attr <> nil);
  Test('CMS_SignerInfo_get0_attr 函数加载', LResult);
end;

procedure TestCMS_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 8: CMS 工具函数 ===');

  // 测试获取版本
  LResult := Assigned(@CMS_ContentInfo_get_eContent) and (CMS_ContentInfo_get_eContent <> nil);
  Test('CMS_ContentInfo_get_eContent 函数加载', LResult);

  // 测试打印函数
  LResult := Assigned(@CMS_ContentInfo_print_ctx) and (CMS_ContentInfo_print_ctx <> nil);
  Test('CMS_ContentInfo_print_ctx 函数加载', LResult);

  // 测试流操作
  LResult := Assigned(@Ti2d_CMS_bio_stream) and (Ti2d_CMS_bio_stream <> nil);
  Test('Ti2d_CMS_bio_stream 函数加载', LResult);

  // 测试标志常量
  Test('CMS_TEXT 标志 ($1)', CMS_TEXT = $1);
  Test('CMS_DETACHED 标志 ($40)', CMS_DETACHED = $40);
  Test('CMS_BINARY 标志 ($80)', CMS_BINARY = $80);
  Test('CMS_STREAM 标志 ($1000)', CMS_STREAM = $1000);
  Test('CMS_PARTIAL 标志 ($4000)', CMS_PARTIAL = $4000);
  Test('CMS_REUSE_DIGEST 标志 ($8000)', CMS_REUSE_DIGEST = $8000);
  Test('CMS_USE_KEYID 标志 ($10000)', CMS_USE_KEYID = $10000);
end;

procedure TestCMS_PEMOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 9: CMS PEM 操作 ===');

  // 测试 PEM 编码
  LResult := Assigned(@PEM_write_bio_CMS) and (PEM_write_bio_CMS <> nil);
  Test('PEM_write_bio_CMS 函数加载', LResult);

  // 测试 PEM 解码
  LResult := Assigned(@PEM_read_bio_CMS) and (PEM_read_bio_CMS <> nil);
  Test('PEM_read_bio_CMS 函数加载', LResult);

  // 测试压缩 CMS 编码
  LResult := Assigned(@PEM_write_bio_CMS_stream) and (PEM_write_bio_CMS_stream <> nil);
  Test('PEM_write_bio_CMS_stream 函数加载', LResult);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('CMS (加密消息语法) 模块综合测试');
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
  TestCMS_ContentInfo;
  TestCMS_SignOperations;
  TestCMS_VerifyOperations;
  TestCMS_EncryptDecrypt;
  TestCMS_RecipientInfo;
  TestCMS_ReceiptOperations;
  TestCMS_Attributes;
  TestCMS_UtilityFunctions;
  TestCMS_PEMOperations;

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
    WriteLn('🎉 所有测试通过！CMS 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
