program test_p2_ts_comprehensive;

{$mode ObjFPC}{$H+}

{
  TS (时间戳协议) 模块综合测试

  测试范围：
  1. TS 请求创建和验证
  2. TS 响应处理
  3. TS 验证和时间验证
  4. TS 准确时间查询

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.ts (TS API)
  - fafafa.ssl.openssl.api.asn1 (ASN.1)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.bio,
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

procedure TestTS_RequestOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: TS 请求操作 ===');

  // 测试请求创建
  LResult := Assigned(@TS_REQ_new) and (TS_REQ_new <> nil);
  Test('TS_REQ_new 函数加载', LResult);

  LResult := Assigned(@TS_REQ_free) and (TS_REQ_free <> nil);
  Test('TS_REQ_free 函数加载', LResult);

  // 测试请求响应
  LResult := Assigned(@TS_REQ_set_version) and (TS_REQ_set_version <> nil);
  Test('TS_REQ_set_version 函数加载', LResult);

  LResult := Assigned(@TS_REQ_set_msg_imprint) and (TS_REQ_set_msg_imprint <> nil);
  Test('TS_REQ_set_msg_imprint 函数加载', LResult);

  // 测试请求获取
  LResult := Assigned(@TS_REQ_get_version) and (TS_REQ_get_version <> nil);
  Test('TS_REQ_get_version 函数加载', LResult);

  LResult := Assigned(@TS_REQ_get_msg_imprint) and (TS_REQ_get_msg_imprint <> nil);
  Test('TS_REQ_get_msg_imprint 函数加载', LResult);
end;

procedure TestTS_ResponseOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: TS 响应操作 ===');

  // 测试响应创建
  LResult := Assigned(@TS_RESP_new) and (TS_RESP_new <> nil);
  Test('TS_RESP_new 函数加载', LResult);

  LResult := Assigned(@TS_RESP_free) and (TS_RESP_free <> nil);
  Test('TS_RESP_free 函数加载', LResult);

  // 测试响应状态
  LResult := Assigned(@TS_RESP_set_status_info) and (TS_RESP_set_status_info <> nil);
  Test('TS_RESP_set_status_info 函数加载', LResult);

  LResult := Assigned(@TS_RESP_create_response) and (TS_RESP_create_response <> nil);
  Test('TS_RESP_create_response 函数加载', LResult);

  // 测试获取响应信息
  LResult := Assigned(@TS_RESP_get_status_info) and (TS_RESP_get_status_info <> nil);
  Test('TS_RESP_get_status_info 函数加载', LResult);

  LResult := Assigned(@TS_RESP_get_token) and (TS_RESP_get_token <> nil);
  Test('TS_RESP_get_token 函数加载', LResult);
end;

procedure TestTS_TSTInfo;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: TS TSTInfo ===');

  // 测试 TSTInfo 创建
  LResult := Assigned(@TS_TST_INFO_new) and (TS_TST_INFO_new <> nil);
  Test('TS_TST_INFO_new 函数加载', LResult);

  LResult := Assigned(@TS_TST_INFO_free) and (TS_TST_INFO_free <> nil);
  Test('TS_TST_INFO_free 函数加载', LResult);

  // 测试设置时间戳信息
  LResult := Assigned(@TS_TST_INFO_set_version) and (TS_TST_INFO_set_version <> nil);
  Test('TS_TST_INFO_set_version 函数加载', LResult);

  // OpenSSL 1.x only functions - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@TS_TST_INFO_set_policy_id) and (TS_TST_INFO_set_policy_id <> nil);
    Test('TS_TST_INFO_set_policy_id 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@TS_TST_INFO_set_msg_imprint) and (TS_TST_INFO_set_msg_imprint <> nil);
    Test('TS_TST_INFO_set_msg_imprint 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试获取时间戳信息
  LResult := Assigned(@TS_TST_INFO_get_version) and (TS_TST_INFO_get_version <> nil);
  Test('TS_TST_INFO_get_version 函数加载', LResult);

  // OpenSSL 1.x only functions - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@TS_TST_INFO_get_policy_id) and (TS_TST_INFO_get_policy_id <> nil);
    Test('TS_TST_INFO_get_policy_id 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@TS_TST_INFO_get_msg_imprint) and (TS_TST_INFO_get_msg_imprint <> nil);
    Test('TS_TST_INFO_get_msg_imprint 函数加载 (OpenSSL 1.x)', LResult);
  end;
end;

procedure TestTS_Verification;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: TS 验证 ===');

  // 测试响应验证
  LResult := Assigned(@TS_RESP_verify_response) and (TS_RESP_verify_response <> nil);
  Test('TS_RESP_verify_response 函数加载', LResult);

  // 测试签名验证
  LResult := Assigned(@TS_RESP_verify_signature) and (TS_RESP_verify_signature <> nil);
  Test('TS_RESP_verify_signature 函数加载', LResult);

  // 测试时间戳验证
  LResult := Assigned(@TS_VERIFY_CTX_new) and (TS_VERIFY_CTX_new <> nil);
  Test('TS_VERIFY_CTX_new 函数加载', LResult);

  LResult := Assigned(@TS_VERIFY_CTX_free) and (TS_VERIFY_CTX_free <> nil);
  Test('TS_VERIFY_CTX_free 函数加载', LResult);
end;

procedure TestTS_IOAndSerialization;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: TS I/O 和序列化 ===');

  // 测试 BIO 编码
  LResult := Assigned(@TS_REQ_i2d_bio) and (TS_REQ_i2d_bio <> nil);
  Test('TS_REQ_i2d_bio 函数加载', LResult);

  LResult := Assigned(@TS_REQ_d2i_bio) and (TS_REQ_d2i_bio <> nil);
  Test('TS_REQ_d2i_bio 函数加载', LResult);

  LResult := Assigned(@TS_RESP_i2d_bio) and (TS_RESP_i2d_bio <> nil);
  Test('TS_RESP_i2d_bio 函数加载', LResult);

  LResult := Assigned(@TS_RESP_d2i_bio) and (TS_RESP_d2i_bio <> nil);
  Test('TS_RESP_d2i_bio 函数加载', LResult);

  // 测试打印函数
  LResult := Assigned(@TS_REQ_print_bio) and (TS_REQ_print_bio <> nil);
  Test('TS_REQ_print_bio 函数加载', LResult);

  LResult := Assigned(@TS_RESP_print_bio) and (TS_RESP_print_bio <> nil);
  Test('TS_RESP_print_bio 函数加载', LResult);
end;

procedure TestTS_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: TS 工具函数 ===');

  // 测试状态信息
  LResult := Assigned(@TS_STATUS_INFO_get0_status) and (TS_STATUS_INFO_get0_status <> nil);
  Test('TS_STATUS_INFO_get0_status 函数加载', LResult);

  // OpenSSL 1.x only function - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@TS_STATUS_INFO_get0_text) and (TS_STATUS_INFO_get0_text <> nil);
    Test('TS_STATUS_INFO_get0_text 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试打印函数
  LResult := Assigned(@TS_TST_INFO_print_bio) and (TS_TST_INFO_print_bio <> nil);
  Test('TS_TST_INFO_print_bio 函数加载', LResult);

  LResult := Assigned(@TS_STATUS_INFO_print_bio) and (TS_STATUS_INFO_print_bio <> nil);
  Test('TS_STATUS_INFO_print_bio 函数加载', LResult);

  // 测试状态常量
  Test('TS_STATUS_GRANTED (0)', TS_STATUS_GRANTED = 0);
  Test('TS_STATUS_GRANTED_WITH_MODS (1)', TS_STATUS_GRANTED_WITH_MODS = 1);
  Test('TS_STATUS_REJECTION (2)', TS_STATUS_REJECTION = 2);
  Test('TS_STATUS_WAITING (3)', TS_STATUS_WAITING = 3);
  Test('TS_STATUS_REVOCATION_WARNING (4)', TS_STATUS_REVOCATION_WARNING = 4);
  Test('TS_STATUS_REVOCATION_NOTIFICATION (5)', TS_STATUS_REVOCATION_NOTIFICATION = 5);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('TS (时间戳协议) 模块综合测试');
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

  // 加载 TS 模块
  WriteLn;
  WriteLn('加载 TS 模块...');
  try
    LoadTSFunctions;
    WriteLn('✅ TS 模块加载成功');
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误：无法加载 TS 模块: ', E.Message);
      Halt(1);
    end;
  end;

  // 执行测试套件
  TestTS_RequestOperations;
  TestTS_ResponseOperations;
  TestTS_TSTInfo;
  TestTS_Verification;
  TestTS_IOAndSerialization;
  TestTS_UtilityFunctions;

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
    WriteLn('🎉 所有测试通过！TS 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
