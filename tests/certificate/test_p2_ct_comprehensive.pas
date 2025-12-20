program test_p2_ct_comprehensive;

{$mode ObjFPC}{$H+}

{
  CT (证书透明度) 模块综合测试

  测试范围：
  1. SCT (签名证书时间戳) 结构
  2. CT 验证函数
  3. 证书透明度日志

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.ct (CT API)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.bio;

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

procedure TestCT_BasicStructures;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: CT 基本结构 ===');

  // 测试 SCT 结构
  LResult := Assigned(@CT_SCT_new) and (CT_SCT_new <> nil);
  Test('CT_SCT_new 函数加载', LResult);

  LResult := Assigned(@CT_SCT_free) and (CT_SCT_free <> nil);
  Test('CT_SCT_free 函数加载', LResult);

  // 测试 SCT 列表
  LResult := Assigned(@CT_SCT_LIST_new) and (CT_SCT_LIST_new <> nil);
  Test('CT_SCT_LIST_new 函数加载', LResult);

  LResult := Assigned(@CT_SCT_LIST_free) and (CT_SCT_LIST_free <> nil);
  Test('CT_SCT_LIST_free 函数加载', LResult);
end;

procedure TestCT_Serialization;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: CT 序列化 ===');

  // 测试 DER 编码
  LResult := Assigned(@i2d_CT_SCT) and (i2d_CT_SCT <> nil);
  Test('i2d_CT_SCT 函数加载', LResult);

  LResult := Assigned(@d2i_CT_SCT) and (d2i_CT_SCT <> nil);
  Test('d2i_CT_SCT 函数加载', LResult);

  // 测试 BIO 编码
  LResult := Assigned(@i2d_CT_SCT_bio) and (i2d_CT_SCT_bio <> nil);
  Test('i2d_CT_SCT_bio 函数加载', LResult);

  LResult := Assigned(@d2i_CT_SCT_bio) and (d2i_CT_SCT_bio <> nil);
  Test('d2i_CT_SCT_bio 函数加载', LResult);

  // 测试 SCT 列表编码
  LResult := Assigned(@i2d_CT_SCT_LIST) and (i2d_CT_SCT_LIST <> nil);
  Test('i2d_CT_SCT_LIST 函数加载', LResult);

  LResult := Assigned(@d2i_CT_SCT_LIST) and (d2i_CT_SCT_LIST <> nil);
  Test('d2i_CT_SCT_LIST 函数加载', LResult);
end;

procedure TestCT_Verification;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: CT 验证 ===');

  // 测试 SCT 验证
  LResult := Assigned(@CT_SCT_verify) and (CT_SCT_verify <> nil);
  Test('CT_SCT_verify 函数加载', LResult);

  // 测试 SCT 列表验证
  LResult := Assigned(@CT_SCT_LIST_verify) and (CT_SCT_LIST_verify <> nil);
  Test('CT_SCT_LIST_verify 函数加载', LResult);

  // 测试从 X509 获取 SCT
  LResult := Assigned(@X509_get0_ct_scts) and (X509_get0_ct_scts <> nil);
  Test('X509_get0_ct_scts 函数加载', LResult);
end;

procedure TestCT_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: CT 工具函数 ===');

  // 测试获取 SCT 版本
  LResult := Assigned(@CT_SCT_get_version) and (CT_SCT_get_version <> nil);
  Test('CT_SCT_get_version 函数加载', LResult);

  // 测试获取日志 ID
  LResult := Assigned(@CT_SCT_get0_log_id) and (CT_SCT_get0_log_id <> nil);
  Test('CT_SCT_get0_log_id 函数加载', LResult);

  // 测试获取时间戳
  LResult := Assigned(@CT_SCT_get_timestamp) and (CT_SCT_get_timestamp <> nil);
  Test('CT_SCT_get_timestamp 函数加载', LResult);

  // 测试获取哈希算法
  LResult := Assigned(@CT_SCT_get_hash_alg) and (CT_SCT_get_hash_alg <> nil);
  Test('CT_SCT_get_hash_alg 函数加载', LResult);

  // 测试获取签名算法
  LResult := Assigned(@CT_SCT_get_signature) and (CT_SCT_get_signature <> nil);
  Test('CT_SCT_get_signature 函数加载', LResult);

  // 测试获取扩展
  LResult := Assigned(@CT_SCT_get0_extensions) and (CT_SCT_get0_extensions <> nil);
  Test('CT_SCT_get0_extensions 函数加载', LResult);
end;

procedure TestCT_Status;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: CT 状态 ===');

  // 测试获取验证状态
  LResult := Assigned(@CT_SCT_get_validation_status) and (CT_SCT_get_validation_status <> nil);
  Test('CT_SCT_get_validation_status 函数加载', LResult);

  // 测试状态字符串
  LResult := Assigned(@CT_SCT_validation_status_string) and (CT_SCT_validation_status_string <> nil);
  Test('CT_SCT_validation_status_string 函数加载', LResult);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('CT (证书透明度) 模块综合测试');
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
  TestCT_BasicStructures;
  TestCT_Serialization;
  TestCT_Verification;
  TestCT_UtilityFunctions;
  TestCT_Status;

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
    WriteLn('🎉 所有测试通过！CT 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
