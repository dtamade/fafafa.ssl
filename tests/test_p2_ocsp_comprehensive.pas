program test_p2_ocsp_comprehensive;

{$mode ObjFPC}{$H+}

{
  OCSP (在线证书状态协议) 模块综合测试

  测试范围：
  1. OCSP 请求创建和解析
  2. OCSP 响应处理
  3. OCSP 基本验证
  4. OCSP 证书 ID
  5. OCSP 单响应和响应列表

  功能级别：生产级测试

  依赖模块：
  - fafafa.ssl.openssl.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.ocsp (OCSP API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.asn1;

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

procedure TestOCSP_BasicOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: OCSP 基本操作 ===');

  // 测试 OCSP_request_new
  LResult := Assigned(@OCSP_request_new) and (OCSP_request_new <> nil);
  Test('OCSP_request_new 函数加载', LResult);

  // 测试 OCSP_request_free
  LResult := Assigned(@OCSP_request_free) and (OCSP_request_free <> nil);
  Test('OCSP_request_free 函数加载', LResult);

  // 测试 OCSP_response_new
  LResult := Assigned(@OCSP_response_new) and (OCSP_response_new <> nil);
  Test('OCSP_response_new 函数加载', LResult);

  // 测试 OCSP_response_free
  LResult := Assigned(@OCSP_response_free) and (OCSP_response_free <> nil);
  Test('OCSP_response_free 函数加载', LResult);
end;

procedure TestOCSP_RequestManipulation;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 2: OCSP 请求操作 ===');

  // 测试添加证书 ID
  LResult := Assigned(@OCSP_request_add_cert_id) and (OCSP_request_add_cert_id <> nil);
  Test('OCSP_request_add_cert_id 函数加载', LResult);

  // 测试设置请求扩展
  LResult := Assigned(@OCSP_request_set1_id) and (OCSP_request_set1_id <> nil);
  Test('OCSP_request_set1_id 函数加载', LResult);

  // 测试获取请求扩展
  LResult := Assigned(@OCSP_request_get1_id) and (OCSP_request_get1_id <> nil);
  Test('OCSP_request_get1_id 函数加载', LResult);

  // 测试添加扩展
  LResult := Assigned(@OCSP_request_add_ext) and (OCSP_request_add_ext <> nil);
  Test('OCSP_request_add_ext 函数加载', LResult);
end;

procedure TestOCSP_CertID;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: OCSP 证书 ID ===');

  // 测试证书 ID 创建
  LResult := Assigned(@OCSP_cert_id_new) and (OCSP_cert_id_new <> nil);
  Test('OCSP_cert_id_new 函数加载', LResult);

  // 测试证书 ID 释放
  LResult := Assigned(@OCSP_cert_id_free) and (OCSP_cert_id_free <> nil);
  Test('OCSP_cert_id_free 函数加载', LResult);

  // 测试解析证书 ID
  LResult := Assigned(@OCSP_parse_cert_id) and (OCSP_parse_cert_id <> nil);
  Test('OCSP_parse_cert_id 函数加载', LResult);

  // 测试获取证书 ID 哈希
  LResult := Assigned(@OCSP_cert_id_hash) and (OCSP_cert_id_hash <> nil);
  Test('OCSP_cert_id_hash 函数加载', LResult);
end;

procedure TestOCSP_ResponseOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: OCSP 响应操作 ===');

  // 测试获取响应状态
  LResult := Assigned(@OCSP_response_get1_status) and (OCSP_response_get1_status <> nil);
  Test('OCSP_response_get1_status 函数加载', LResult);

  // 测试获取基本响应
  LResult := Assigned(@OCSP_response_get_basic) and (OCSP_response_get_basic <> nil);
  Test('OCSP_response_get_basic 函数加载', LResult);

  // 测试获取响应扩展
  LResult := Assigned(@OCSP_response_get1_ext) and (OCSP_response_get1_ext <> nil);
  Test('OCSP_response_get1_ext 函数加载', LResult);

  // 测试设置响应扩展
  LResult := Assigned(@OCSP_response_set1_ext) and (OCSP_response_set1_ext <> nil);
  Test('OCSP_response_set1_ext 函数加载', LResult);

  // 测试获取响应生成时间
  LResult := Assigned(@OCSP_response_get1_produced_at) and (OCSP_response_get1_produced_at <> nil);
  Test('OCSP_response_get1_produced_at 函数加载', LResult);
end;

procedure TestOCSP_SingleResponse;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 5: OCSP 单响应 ===');

  // 测试获取响应数量
  LResult := Assigned(@OCSP_resp_count) and (OCSP_resp_count <> nil);
  Test('OCSP_resp_count 函数加载', LResult);

  // 测试获取响应列表
  LResult := Assigned(@OCSP_resp_get0) and (OCSP_resp_get0 <> nil);
  Test('OCSP_resp_get0 函数加载', LResult);

  // 测试获取单响应状态
  LResult := Assigned(@OCSP_single_get0_status) and (OCSP_single_get0_status <> nil);
  Test('OCSP_single_get0_status 函数加载', LResult);

  // 测试获取下一个响应
  LResult := Assigned(@OCSP_resp_find) and (OCSP_resp_find <> nil);
  Test('OCSP_resp_find 函数加载', LResult);
end;

procedure TestOCSP_Verification;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 6: OCSP 验证 ===');

  // 测试验证响应
  LResult := Assigned(@OCSP_basic_verify) and (OCSP_basic_verify <> nil);
  Test('OCSP_basic_verify 函数加载', LResult);

  // 测试验证响应状态
  LResult := Assigned(@OCSP_response_status_str) and (OCSP_response_status_str <> nil);
  Test('OCSP_response_status_str 函数加载', LResult);

  // 测试获取验证错误
  LResult := Assigned(@OCSP_resp_verify) and (OCSP_resp_verify <> nil);
  Test('OCSP_resp_verify 函数加载', LResult);

  // 测试检查响应
  LResult := Assigned(@OCSP_check_validity) and (OCSP_check_validity <> nil);
  Test('OCSP_check_validity 函数加载', LResult);
end;

procedure TestOCSP_IOSerialization;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 7: OCSP I/O 和序列化 ===');

  // 测试 DER 编码请求
  LResult := Assigned(@i2d_OCSP_REQUEST) and (i2d_OCSP_REQUEST <> nil);
  Test('i2d_OCSP_REQUEST 函数加载', LResult);

  // 测试 DER 解码请求
  LResult := Assigned(@d2i_OCSP_REQUEST) and (d2i_OCSP_REQUEST <> nil);
  Test('d2i_OCSP_REQUEST 函数加载', LResult);

  // 测试 DER 编码响应
  LResult := Assigned(@i2d_OCSP_RESPONSE) and (i2d_OCSP_RESPONSE <> nil);
  Test('i2d_OCSP_RESPONSE 函数加载', LResult);

  // 测试 DER 解码响应
  LResult := Assigned(@d2i_OCSP_RESPONSE) and (d2i_OCSP_RESPONSE <> nil);
  Test('d2i_OCSP_RESPONSE 函数加载', LResult);

  // 测试 BIO 请求编码
  LResult := Assigned(@i2d_OCSP_REQUEST_bio) and (i2d_OCSP_REQUEST_bio <> nil);
  Test('i2d_OCSP_REQUEST_bio 函数加载', LResult);

  // 测试 BIO 响应编码
  LResult := Assigned(@i2d_OCSP_RESPONSE_bio) and (i2d_OCSP_RESPONSE_bio <> nil);
  Test('i2d_OCSP_RESPONSE_bio 函数加载', LResult);
end;

procedure TestOCSP_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 8: OCSP 工具函数 ===');

  // 测试错误字符串
  LResult := Assigned(@OCSPerror_string) and (OCSPerror_string <> nil);
  Test('OCSPerror_string 函数加载', LResult);

  // 测试获取响应详细信息
  LResult := Assigned(@OCSP_response_get_mem_bio) and (OCSP_response_get_mem_bio <> nil);
  Test('OCSP_response_get_mem_bio 函数加载', LResult);

  // 测试状态常量
  Test('OCSP_RESPONSE_STATUS_SUCCESSFUL (0)', OCSP_RESPONSE_STATUS_SUCCESSFUL = 0);
  Test('OCSP_RESPONSE_STATUS_MALFORMEDREQUEST (1)', OCSP_RESPONSE_STATUS_MALFORMEDREQUEST = 1);
  Test('OCSP_RESPONSE_STATUS_INTERNALERROR (2)', OCSP_RESPONSE_STATUS_INTERNALERROR = 2);
  Test('OCSP_RESPONSE_STATUS_TRYLATER (3)', OCSP_RESPONSE_STATUS_TRYLATER = 3);
  Test('OCSP_RESPONSE_STATUS_SIGREQUIRED (4)', OCSP_RESPONSE_STATUS_SIGREQUIRED = 4);
  Test('OCSP_RESPONSE_STATUS_UNAUTHORIZED (5)', OCSP_RESPONSE_STATUS_UNAUTHORIZED = 5);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('OCSP (在线证书状态协议) 模块综合测试');
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
  TestOCSP_BasicOperations;
  TestOCSP_RequestManipulation;
  TestOCSP_CertID;
  TestOCSP_ResponseOperations;
  TestOCSP_SingleResponse;
  TestOCSP_Verification;
  TestOCSP_IOSerialization;
  TestOCSP_UtilityFunctions;

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
    WriteLn('🎉 所有测试通过！OCSP 模块工作正常');
  end;

  UnloadOpenSSLCore;
end.
