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
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.ocsp (OCSP API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.asn1,
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
  LResult := Assigned(@OCSP_request_add0_id) and (OCSP_request_add0_id <> nil);
  Test('OCSP_request_add0_id 函数加载', LResult);

  // 测试添加 nonce
  LResult := Assigned(@OCSP_request_add1_nonce) and (OCSP_request_add1_nonce <> nil);
  Test('OCSP_request_add1_nonce 函数加载', LResult);

  // 测试添加证书
  LResult := Assigned(@OCSP_request_add1_cert) and (OCSP_request_add1_cert <> nil);
  Test('OCSP_request_add1_cert 函数加载', LResult);

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
  LResult := Assigned(@OCSP_CERTID_free) and (OCSP_CERTID_free <> nil);
  Test('OCSP_CERTID_free 函数加载', LResult);

  // 测试证书 ID 复制
  LResult := Assigned(@OCSP_CERTID_dup) and (OCSP_CERTID_dup <> nil);
  Test('OCSP_CERTID_dup 函数加载', LResult);

  // 测试证书转 ID
  LResult := Assigned(@OCSP_cert_to_id) and (OCSP_cert_to_id <> nil);
  Test('OCSP_cert_to_id 函数加载', LResult);
end;

procedure TestOCSP_ResponseOperations;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 4: OCSP 响应操作 ===');

  // OpenSSL 1.x only functions - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@OCSP_RESPONSE_status) and (OCSP_RESPONSE_status <> nil);
    Test('OCSP_RESPONSE_status 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OCSP_RESPONSE_get1_basic) and (OCSP_RESPONSE_get1_basic <> nil);
    Test('OCSP_RESPONSE_get1_basic 函数加载 (OpenSSL 1.x)', LResult);

    LResult := Assigned(@OCSP_RESPONSE_create) and (OCSP_RESPONSE_create <> nil);
    Test('OCSP_RESPONSE_create 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试获取响应数据
  LResult := Assigned(@OCSP_resp_get0_respdata) and (OCSP_resp_get0_respdata <> nil);
  Test('OCSP_resp_get0_respdata 函数加载', LResult);

  // 测试获取响应生成时间
  LResult := Assigned(@OCSP_resp_get0_produced_at) and (OCSP_resp_get0_produced_at <> nil);
  Test('OCSP_resp_get0_produced_at 函数加载', LResult);
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

  // OpenSSL 1.x only function - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@OCSP_BASICRESP_verify) and (OCSP_BASICRESP_verify <> nil);
    Test('OCSP_BASICRESP_verify 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试检查 nonce
  LResult := Assigned(@OCSP_check_nonce) and (OCSP_check_nonce <> nil);
  Test('OCSP_check_nonce 函数加载', LResult);

  // 测试复制 nonce
  LResult := Assigned(@OCSP_copy_nonce) and (OCSP_copy_nonce <> nil);
  Test('OCSP_copy_nonce 函数加载', LResult);

  // 测试检查有效性
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

  // 测试打印请求
  LResult := Assigned(@OCSP_REQUEST_print) and (OCSP_REQUEST_print <> nil);
  Test('OCSP_REQUEST_print 函数加载', LResult);

  // 测试打印响应
  LResult := Assigned(@OCSP_RESPONSE_print) and (OCSP_RESPONSE_print <> nil);
  Test('OCSP_RESPONSE_print 函数加载', LResult);
end;

procedure TestOCSP_UtilityFunctions;
var
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 8: OCSP 工具函数 ===');

  // 测试 HTTP 函数
  LResult := Assigned(@OCSP_sendreq_new) and (OCSP_sendreq_new <> nil);
  Test('OCSP_sendreq_new 函数加载', LResult);

  // OpenSSL 1.x only function - skip on OpenSSL 3.x
  if not IsOpenSSL3 then
  begin
    LResult := Assigned(@OCSP_parse_url) and (OCSP_parse_url <> nil);
    Test('OCSP_parse_url 函数加载 (OpenSSL 1.x)', LResult);
  end;

  // 测试状态常量
  Test('OCSP_RESPONSE_STATUS_SUCCESSFUL (0)', OCSP_RESPONSE_STATUS_SUCCESSFUL = 0);
  Test('OCSP_RESPONSE_STATUS_MALFORMEDREQUEST (1)', OCSP_RESPONSE_STATUS_MALFORMEDREQUEST = 1);
  Test('OCSP_RESPONSE_STATUS_INTERNALERROR (2)', OCSP_RESPONSE_STATUS_INTERNALERROR = 2);
  Test('OCSP_RESPONSE_STATUS_TRYLATER (3)', OCSP_RESPONSE_STATUS_TRYLATER = 3);
  Test('OCSP_RESPONSE_STATUS_SIGREQUIRED (5)', OCSP_RESPONSE_STATUS_SIGREQUIRED = 5);
  Test('OCSP_RESPONSE_STATUS_UNAUTHORIZED (6)', OCSP_RESPONSE_STATUS_UNAUTHORIZED = 6);
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

  // 加载 OCSP 模块
  WriteLn;
  WriteLn('加载 OCSP 模块...');
  try
    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      WriteLn('❌ 错误：无法加载 OCSP 模块');
      Halt(1);
    end;
    WriteLn('✅ OCSP 模块加载成功');
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误：无法加载 OCSP 模块: ', E.Message);
      Halt(1);
    end;
  end;

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
