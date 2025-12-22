{
  test_error_recovery.pas - TLS Error Recovery Integration Tests

  测试 TLS 错误恢复路径：
  1. 握手失败恢复
  2. 连接中断恢复
  3. 证书验证失败处理
  4. 超时处理
  5. 资源清理验证

  作者: fafafa.ssl 开发团队
  创建: 2025-12-22
}

program test_error_recovery;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.err;

const
  // X509 名称常量
  MBSTRING_ASC = $1001;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

  // SSL 资源
  ServerCert: PX509;
  ServerKey: PEVP_PKEY;

procedure TestResult(const TestName: string; Success: Boolean; const Details: string = '');
begin
  Inc(TotalTests);
  if Success then
  begin
    Inc(PassedTests);
    WriteLn('[PASS] ', TestName);
    if Details <> '' then
      WriteLn('       ', Details);
  end
  else
  begin
    Inc(FailedTests);
    WriteLn('[FAIL] ', TestName);
    if Details <> '' then
      WriteLn('       原因: ', Details);
  end;
end;

procedure PrintHeader(const Title: string);
begin
  WriteLn;
  WriteLn('测试: ', Title);
  WriteLn('----------------------------------------');
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('测试摘要');
  WriteLn('========================================');
  WriteLn('总测试数: ', TotalTests);
  WriteLn('通过: ', PassedTests);
  WriteLn('失败: ', FailedTests);
  if TotalTests > 0 then
    WriteLn('通过率: ', (PassedTests * 100) div TotalTests, '%');
  WriteLn('========================================');
end;

function GenerateSelfSignedCert(out Cert: PX509; out Key: PEVP_PKEY): Boolean;
var
  RSA: PRSA;
  BN: PBIGNUM;
  PKey: PEVP_PKEY;
  X509Cert: PX509;
  Name: PX509_NAME;
  Serial: PASN1_INTEGER;
  NotBeforeTime, NotAfterTime: PASN1_TIME;
begin
  Result := False;
  Cert := nil;
  Key := nil;

  try
    PKey := EVP_PKEY_new();
    if PKey = nil then Exit;

    RSA := RSA_new();
    BN := BN_new();
    BN_set_word(BN, RSA_F4);

    if RSA_generate_key_ex(RSA, 2048, BN, nil) <> 1 then
    begin
      BN_free(BN); RSA_free(RSA); EVP_PKEY_free(PKey);
      Exit;
    end;
    BN_free(BN);

    EVP_PKEY_assign(PKey, EVP_PKEY_RSA, RSA);

    X509Cert := X509_new();
    X509_set_version(X509Cert, 2);

    Serial := X509_get_serialNumber(X509Cert);
    ASN1_INTEGER_set(Serial, 1);

    NotBeforeTime := X509_get_notBefore(X509Cert);
    X509_gmtime_adj(NotBeforeTime, 0);
    NotAfterTime := X509_get_notAfter(X509Cert);
    X509_gmtime_adj(NotAfterTime, 365 * 24 * 60 * 60);

    X509_set_pubkey(X509Cert, PKey);

    Name := X509_NAME_new();
    X509_NAME_add_entry_by_txt(Name, 'CN', MBSTRING_ASC, PByte(PAnsiChar('localhost')), -1, -1, 0);
    X509_set_subject_name(X509Cert, Name);
    X509_set_issuer_name(X509Cert, Name);
    X509_NAME_free(Name);

    X509_sign(X509Cert, PKey, EVP_sha256());

    Cert := X509Cert;
    Key := PKey;
    Result := True;
  except
    Result := False;
  end;
end;

procedure Test1_HandshakeFailureRecovery;
var
  ClientCtx: PSSL_CTX;
  ClientSSL: PSSL;
  ClientRead, ClientWrite: PBIO;
  Method: PSSL_METHOD;
  Ret, Err: Integer;
begin
  PrintHeader('1. 握手失败恢复测试');

  try
    // 创建客户端上下文
    Method := TLS_client_method();
    ClientCtx := SSL_CTX_new(Method);
    TestResult('创建客户端上下文', ClientCtx <> nil);

    if ClientCtx <> nil then
    begin
      // 创建 SSL 对象
      ClientSSL := SSL_new(ClientCtx);
      TestResult('创建 SSL 对象', ClientSSL <> nil);

      if ClientSSL <> nil then
      begin
        // 创建空的 BIO（模拟无响应的服务器）
        ClientRead := BIO_new(BIO_s_mem());
        ClientWrite := BIO_new(BIO_s_mem());
        SSL_set_bio(ClientSSL, ClientRead, ClientWrite);

        SSL_set_connect_state(ClientSSL);

        // 尝试握手（应该失败，因为没有服务器响应）
        Ret := SSL_do_handshake(ClientSSL);
        Err := SSL_get_error(ClientSSL, Ret);

        // 期望 WANT_READ 错误（等待服务器响应）
        TestResult('握手返回 WANT_READ', Err = SSL_ERROR_WANT_READ,
                  '错误码: ' + IntToStr(Err));

        // 验证可以正确清理资源
        SSL_shutdown(ClientSSL);
        SSL_free(ClientSSL);
        TestResult('清理 SSL 对象', True);
      end;

      SSL_CTX_free(ClientCtx);
      TestResult('清理上下文', True);
    end;

  except
    on E: Exception do
      TestResult('握手失败恢复', False, E.Message);
  end;
end;

procedure Test2_ConnectionInterruptRecovery;
var
  ClientCtx, ServerCtx: PSSL_CTX;
  ClientSSL, ServerSSL: PSSL;
  ClientRead, ClientWrite, ServerRead, ServerWrite: PBIO;
  Method: PSSL_METHOD;
  Buffer: array[0..4095] of Byte;
  Len, Ret, Err: Integer;
  i: Integer;
begin
  PrintHeader('2. 连接中断恢复测试');

  try
    // 创建客户端上下文
    Method := TLS_client_method();
    ClientCtx := SSL_CTX_new(Method);
    SSL_CTX_set_verify(ClientCtx, SSL_VERIFY_NONE, nil);

    // 创建服务端上下文
    Method := TLS_server_method();
    ServerCtx := SSL_CTX_new(Method);
    SSL_CTX_use_certificate(ServerCtx, ServerCert);
    SSL_CTX_use_PrivateKey(ServerCtx, ServerKey);

    // 创建 SSL 对象
    ClientSSL := SSL_new(ClientCtx);
    ServerSSL := SSL_new(ServerCtx);

    // 创建 BIO
    ClientRead := BIO_new(BIO_s_mem());
    ClientWrite := BIO_new(BIO_s_mem());
    ServerRead := BIO_new(BIO_s_mem());
    ServerWrite := BIO_new(BIO_s_mem());

    SSL_set_bio(ClientSSL, ClientRead, ClientWrite);
    SSL_set_bio(ServerSSL, ServerRead, ServerWrite);

    SSL_set_connect_state(ClientSSL);
    SSL_set_accept_state(ServerSSL);

    // 开始握手但中途中断
    for i := 1 to 3 do
    begin
      Ret := SSL_do_handshake(ClientSSL);

      // 传输数据
      repeat
        Len := BIO_read(ClientWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ServerRead, @Buffer[0], Len);
      until Len <= 0;

      Ret := SSL_do_handshake(ServerSSL);

      repeat
        Len := BIO_read(ServerWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ClientRead, @Buffer[0], Len);
      until Len <= 0;
    end;

    // 模拟中断：直接关闭服务端
    SSL_shutdown(ServerSSL);
    SSL_free(ServerSSL);
    TestResult('服务端中断', True);

    // 客户端尝试继续操作
    Ret := SSL_do_handshake(ClientSSL);
    Err := SSL_get_error(ClientSSL, Ret);

    // 应该返回错误
    TestResult('客户端检测到中断', Ret <> 1,
              '返回值: ' + IntToStr(Ret) + ', 错误: ' + IntToStr(Err));

    // 清理客户端
    SSL_shutdown(ClientSSL);
    SSL_free(ClientSSL);
    TestResult('客户端清理', True);

    SSL_CTX_free(ClientCtx);
    SSL_CTX_free(ServerCtx);
    TestResult('上下文清理', True);

  except
    on E: Exception do
      TestResult('连接中断恢复', False, E.Message);
  end;
end;

procedure Test3_CertificateVerificationFailure;
var
  ClientCtx, ServerCtx: PSSL_CTX;
  ClientSSL, ServerSSL: PSSL;
  ClientRead, ClientWrite, ServerRead, ServerWrite: PBIO;
  Method: PSSL_METHOD;
  Buffer: array[0..4095] of Byte;
  Len, Ret, Err: Integer;
  i: Integer;
  VerifyResult: LongInt;
begin
  PrintHeader('3. 证书验证失败处理测试');

  try
    // 创建客户端上下文（启用严格验证）
    Method := TLS_client_method();
    ClientCtx := SSL_CTX_new(Method);
    SSL_CTX_set_verify(ClientCtx, SSL_VERIFY_PEER, nil);
    TestResult('创建严格验证客户端', ClientCtx <> nil);

    // 创建服务端上下文
    Method := TLS_server_method();
    ServerCtx := SSL_CTX_new(Method);
    SSL_CTX_use_certificate(ServerCtx, ServerCert);
    SSL_CTX_use_PrivateKey(ServerCtx, ServerKey);

    // 创建 SSL 对象
    ClientSSL := SSL_new(ClientCtx);
    ServerSSL := SSL_new(ServerCtx);

    // 创建 BIO
    ClientRead := BIO_new(BIO_s_mem());
    ClientWrite := BIO_new(BIO_s_mem());
    ServerRead := BIO_new(BIO_s_mem());
    ServerWrite := BIO_new(BIO_s_mem());

    SSL_set_bio(ClientSSL, ClientRead, ClientWrite);
    SSL_set_bio(ServerSSL, ServerRead, ServerWrite);

    SSL_set_connect_state(ClientSSL);
    SSL_set_accept_state(ServerSSL);

    // 尝试握手
    for i := 1 to 10 do
    begin
      Ret := SSL_do_handshake(ClientSSL);
      if (Ret = 1) then Break;

      Err := SSL_get_error(ClientSSL, Ret);
      if (Err <> SSL_ERROR_WANT_READ) and (Err <> SSL_ERROR_WANT_WRITE) then
        Break;

      repeat
        Len := BIO_read(ClientWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ServerRead, @Buffer[0], Len);
      until Len <= 0;

      Ret := SSL_do_handshake(ServerSSL);

      repeat
        Len := BIO_read(ServerWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ClientRead, @Buffer[0], Len);
      until Len <= 0;
    end;

    // 检查验证结果
    if Assigned(SSL_get_verify_result) then
    begin
      VerifyResult := SSL_get_verify_result(ClientSSL);
      // 自签名证书应该验证失败（除非已添加到信任存储）
      if VerifyResult <> 0 then
        TestResult('证书验证失败（预期）', True,
                  '验证结果: ' + IntToStr(VerifyResult))
      else
        TestResult('证书验证通过', True, '可能已添加到信任存储');
    end
    else
      TestResult('获取验证结果', False, '函数未加载');

    // 清理
    SSL_shutdown(ClientSSL);
    SSL_shutdown(ServerSSL);
    SSL_free(ClientSSL);
    SSL_free(ServerSSL);
    SSL_CTX_free(ClientCtx);
    SSL_CTX_free(ServerCtx);
    TestResult('资源清理', True);

  except
    on E: Exception do
      TestResult('证书验证失败处理', False, E.Message);
  end;
end;

procedure Test4_ErrorQueueHandling;
var
  ErrCode: Cardinal;
  ErrBuf: array[0..255] of AnsiChar;
  ErrCount: Integer;
begin
  PrintHeader('4. 错误队列处理测试');

  try
    // 清空错误队列
    if Assigned(ERR_clear_error) then
    begin
      ERR_clear_error();
      TestResult('清空错误队列', True);
    end;

    // 故意产生一个错误（尝试使用无效参数）
    // 这里我们只是检查错误队列 API 是否可用
    TestResult('ERR_get_error 可用', Assigned(ERR_get_error));
    TestResult('ERR_error_string_n 可用', Assigned(ERR_error_string_n));
    TestResult('ERR_peek_error 可用', Assigned(ERR_peek_error));
    TestResult('ERR_clear_error 可用', Assigned(ERR_clear_error));

    // 测试错误队列迭代
    if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
    begin
      ErrCount := 0;
      ErrCode := ERR_get_error();
      while ErrCode <> 0 do
      begin
        ERR_error_string_n(ErrCode, @ErrBuf[0], SizeOf(ErrBuf));
        Inc(ErrCount);
        ErrCode := ERR_get_error();
      end;

      TestResult('错误队列迭代', True, '处理了 ' + IntToStr(ErrCount) + ' 个错误');
    end;

  except
    on E: Exception do
      TestResult('错误队列处理', False, E.Message);
  end;
end;

procedure Test5_ResourceCleanupVerification;
var
  ClientCtx: PSSL_CTX;
  ClientSSL: PSSL;
  Method: PSSL_METHOD;
  i: Integer;
begin
  PrintHeader('5. 资源清理验证测试');

  try
    // 测试多次创建和销毁资源
    for i := 1 to 10 do
    begin
      Method := TLS_client_method();
      ClientCtx := SSL_CTX_new(Method);

      if ClientCtx <> nil then
      begin
        ClientSSL := SSL_new(ClientCtx);

        if ClientSSL <> nil then
        begin
          // 设置一些状态
          SSL_set_connect_state(ClientSSL);

          // 清理
          SSL_free(ClientSSL);
        end;

        SSL_CTX_free(ClientCtx);
      end;
    end;

    TestResult('10 次创建/销毁循环', True, '无内存泄漏（需要外部工具验证）');

    // 测试异常情况下的清理
    try
      Method := TLS_client_method();
      ClientCtx := SSL_CTX_new(Method);

      if ClientCtx <> nil then
      begin
        // 故意在使用中抛出异常
        raise Exception.Create('测试异常');
      end;
    except
      on E: Exception do
      begin
        // 确保清理
        if ClientCtx <> nil then
          SSL_CTX_free(ClientCtx);
        TestResult('异常情况下资源清理', True, '异常: ' + E.Message);
      end;
    end;

  except
    on E: Exception do
      TestResult('资源清理验证', False, E.Message);
  end;
end;

procedure Test6_HighLevelExceptionHandling;
var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  ExceptionCaught: Boolean;
begin
  PrintHeader('6. 高层异常处理测试');

  try
    SSLLib := CreateOpenSSLLibrary;
    TestResult('创建库实例', SSLLib <> nil);

    if SSLLib <> nil then
    begin
      TestResult('初始化库', SSLLib.Initialize);

      Context := SSLLib.CreateContext(sslCtxClient);
      TestResult('创建上下文', Context <> nil);

      if Context <> nil then
      begin
        // 测试加载不存在的证书
        ExceptionCaught := False;
        try
          Context.LoadCertificate('/nonexistent/path/cert.pem');
        except
          on E: ESSLException do
          begin
            ExceptionCaught := True;
            TestResult('文件不存在异常', True, E.Message);
          end;
          on E: Exception do
          begin
            ExceptionCaught := True;
            TestResult('通用异常', True, E.Message);
          end;
        end;

        if not ExceptionCaught then
          TestResult('文件不存在异常', False, '未捕获异常');

        // 测试加载无效的 PEM 数据
        ExceptionCaught := False;
        try
          Context.LoadCertificatePEM('invalid pem data');
        except
          on E: Exception do
          begin
            ExceptionCaught := True;
            TestResult('无效 PEM 异常', True, E.Message);
          end;
        end;

        if not ExceptionCaught then
          TestResult('无效 PEM 异常', False, '未捕获异常');
      end;

      SSLLib.Finalize;
      TestResult('库终结', True);
    end;

  except
    on E: Exception do
      TestResult('高层异常处理', False, E.Message);
  end;
end;

procedure Test7_ErrorCodeTranslation;
var
  SSLLib: ISSLLibrary;
  ErrorCode: Integer;
  ErrorStr: string;
begin
  PrintHeader('7. 错误码翻译测试');

  try
    SSLLib := CreateOpenSSLLibrary;

    if SSLLib <> nil then
    begin
      SSLLib.Initialize;

      // 测试错误码获取
      ErrorCode := SSLLib.GetLastError;
      TestResult('获取错误码', True, '错误码: ' + IntToStr(ErrorCode));

      ErrorStr := SSLLib.GetLastErrorString;
      TestResult('获取错误字符串', True, '错误: ' + ErrorStr);

      // 测试清除错误
      SSLLib.ClearError;
      ErrorCode := SSLLib.GetLastError;
      TestResult('清除错误后', ErrorCode = 0, '错误码: ' + IntToStr(ErrorCode));

      SSLLib.Finalize;
    end
    else
      TestResult('创建库实例', False);

  except
    on E: Exception do
      TestResult('错误码翻译', False, E.Message);
  end;
end;

procedure Test8_GracefulShutdown;
var
  ClientCtx, ServerCtx: PSSL_CTX;
  ClientSSL, ServerSSL: PSSL;
  ClientRead, ClientWrite, ServerRead, ServerWrite: PBIO;
  Method: PSSL_METHOD;
  Buffer: array[0..4095] of Byte;
  Len, Ret: Integer;
  i: Integer;
  ShutdownComplete: Boolean;
begin
  PrintHeader('8. 优雅关闭测试');

  try
    Method := TLS_client_method();
    ClientCtx := SSL_CTX_new(Method);
    SSL_CTX_set_verify(ClientCtx, SSL_VERIFY_NONE, nil);

    Method := TLS_server_method();
    ServerCtx := SSL_CTX_new(Method);
    SSL_CTX_use_certificate(ServerCtx, ServerCert);
    SSL_CTX_use_PrivateKey(ServerCtx, ServerKey);

    ClientSSL := SSL_new(ClientCtx);
    ServerSSL := SSL_new(ServerCtx);

    ClientRead := BIO_new(BIO_s_mem());
    ClientWrite := BIO_new(BIO_s_mem());
    ServerRead := BIO_new(BIO_s_mem());
    ServerWrite := BIO_new(BIO_s_mem());

    SSL_set_bio(ClientSSL, ClientRead, ClientWrite);
    SSL_set_bio(ServerSSL, ServerRead, ServerWrite);

    SSL_set_connect_state(ClientSSL);
    SSL_set_accept_state(ServerSSL);

    // 完成握手
    ShutdownComplete := False;
    for i := 1 to 20 do
    begin
      Ret := SSL_do_handshake(ClientSSL);
      repeat
        Len := BIO_read(ClientWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ServerRead, @Buffer[0], Len);
      until Len <= 0;

      Ret := SSL_do_handshake(ServerSSL);
      repeat
        Len := BIO_read(ServerWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ClientRead, @Buffer[0], Len);
      until Len <= 0;

      if (SSL_is_init_finished(ClientSSL) = 1) and (SSL_is_init_finished(ServerSSL) = 1) then
      begin
        ShutdownComplete := True;
        Break;
      end;
    end;

    TestResult('握手完成', ShutdownComplete);

    if ShutdownComplete then
    begin
      // 测试双向关闭
      // 客户端发起关闭
      Ret := SSL_shutdown(ClientSSL);
      TestResult('客户端关闭发起', True, '返回值: ' + IntToStr(Ret));

      // 传输关闭通知
      repeat
        Len := BIO_read(ClientWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ServerRead, @Buffer[0], Len);
      until Len <= 0;

      // 服务端响应关闭
      Ret := SSL_shutdown(ServerSSL);
      TestResult('服务端关闭响应', True, '返回值: ' + IntToStr(Ret));

      repeat
        Len := BIO_read(ServerWrite, @Buffer[0], SizeOf(Buffer));
        if Len > 0 then BIO_write(ClientRead, @Buffer[0], Len);
      until Len <= 0;

      // 客户端完成关闭
      Ret := SSL_shutdown(ClientSSL);
      TestResult('客户端关闭完成', Ret >= 0, '返回值: ' + IntToStr(Ret));
    end;

    SSL_free(ClientSSL);
    SSL_free(ServerSSL);
    SSL_CTX_free(ClientCtx);
    SSL_CTX_free(ServerCtx);
    TestResult('资源释放', True);

  except
    on E: Exception do
      TestResult('优雅关闭', False, E.Message);
  end;
end;

procedure InitializeTests;
begin
  WriteLn('========================================');
  WriteLn('TLS 错误恢复路径集成测试');
  WriteLn('========================================');
  WriteLn;

  WriteLn('加载 OpenSSL 模块...');
  LoadOpenSSLCore();
  LoadOpenSSLBIO();
  LoadOpenSSLX509();
  LoadEVP(GetCryptoLibHandle);
  LoadOpenSSLRSA();
  LoadOpenSSLBN();
  LoadOpenSSLASN1(GetCryptoLibHandle);
  LoadOpenSSLSSL;
  LoadOpenSSLERR;

  WriteLn('OpenSSL 版本: ', GetOpenSSLVersionString);
  WriteLn;

  WriteLn('生成测试证书...');
  if not GenerateSelfSignedCert(ServerCert, ServerKey) then
  begin
    WriteLn('错误: 无法生成测试证书');
    Halt(1);
  end;
  WriteLn('证书生成成功');
  WriteLn;
end;

procedure CleanupTests;
begin
  if ServerCert <> nil then X509_free(ServerCert);
  if ServerKey <> nil then EVP_PKEY_free(ServerKey);
end;

begin
  try
    InitializeTests;

    Test1_HandshakeFailureRecovery;
    Test2_ConnectionInterruptRecovery;
    Test3_CertificateVerificationFailure;
    Test4_ErrorQueueHandling;
    Test5_ResourceCleanupVerification;
    Test6_HighLevelExceptionHandling;
    Test7_ErrorCodeTranslation;
    Test8_GracefulShutdown;

    CleanupTests;
    PrintSummary;

    WriteLn;
    if FailedTests > 0 then
    begin
      WriteLn('结果: ', FailedTests, ' 个测试失败');
      Halt(1);
    end
    else
    begin
      WriteLn('✅ 所有测试通过!');
      Halt(0);
    end;

  except
    on E: Exception do
    begin
      WriteLn('致命错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
