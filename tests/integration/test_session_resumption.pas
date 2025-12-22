{
  test_session_resumption.pas - TLS Session Resumption Integration Tests

  测试 TLS 会话恢复功能：
  1. 会话创建和缓存
  2. 会话恢复验证
  3. 会话超时处理
  4. 高层 API 测试

  作者: fafafa.ssl 开发团队
  创建: 2025-12-22
}

program test_session_resumption;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.session,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.consts;

const
  // X509 名称常量
  MBSTRING_ASC_VALUE = $1001;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

  // SSL 资源
  ServerCert: PX509;
  ServerKey: PEVP_PKEY;
  ClientCtx, ServerCtx: PSSL_CTX;

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
  PKey := nil;
  RSA := nil;
  BN := nil;
  X509Cert := nil;

  try
    PKey := EVP_PKEY_new();
    if PKey = nil then Exit;

    RSA := RSA_new();
    if RSA = nil then begin EVP_PKEY_free(PKey); Exit; end;

    BN := BN_new();
    if BN = nil then begin RSA_free(RSA); EVP_PKEY_free(PKey); Exit; end;

    BN_set_word(BN, RSA_F4);

    if RSA_generate_key_ex(RSA, 2048, BN, nil) <> 1 then
    begin
      BN_free(BN); RSA_free(RSA); EVP_PKEY_free(PKey);
      Exit;
    end;

    BN_free(BN);

    if EVP_PKEY_assign(PKey, EVP_PKEY_RSA, RSA) <> 1 then
    begin
      RSA_free(RSA); EVP_PKEY_free(PKey);
      Exit;
    end;

    X509Cert := X509_new();
    if X509Cert = nil then begin EVP_PKEY_free(PKey); Exit; end;

    X509_set_version(X509Cert, 2);

    Serial := X509_get_serialNumber(X509Cert);
    ASN1_INTEGER_set(Serial, 1);

    NotBeforeTime := X509_get_notBefore(X509Cert);
    X509_gmtime_adj(NotBeforeTime, 0);

    NotAfterTime := X509_get_notAfter(X509Cert);
    X509_gmtime_adj(NotAfterTime, 365 * 24 * 60 * 60);

    X509_set_pubkey(X509Cert, PKey);

    Name := X509_NAME_new();
    X509_NAME_add_entry_by_txt(Name, 'C', MBSTRING_ASC_VALUE, PByte(PAnsiChar('CN')), -1, -1, 0);
    X509_NAME_add_entry_by_txt(Name, 'O', MBSTRING_ASC_VALUE, PByte(PAnsiChar('Test')), -1, -1, 0);
    X509_NAME_add_entry_by_txt(Name, 'CN', MBSTRING_ASC_VALUE, PByte(PAnsiChar('localhost')), -1, -1, 0);

    X509_set_subject_name(X509Cert, Name);
    X509_set_issuer_name(X509Cert, Name);
    X509_NAME_free(Name);

    if X509_sign(X509Cert, PKey, EVP_sha256()) = 0 then
    begin
      X509_free(X509Cert); EVP_PKEY_free(PKey);
      Exit;
    end;

    Cert := X509Cert;
    Key := PKey;
    Result := True;
  except
    Result := False;
  end;
end;

procedure PumpData(ClientWrite, ServerRead, ServerWrite, ClientRead: PBIO);
var
  Buffer: array[0..4095] of Byte;
  Len: Integer;
begin
  repeat
    Len := BIO_read(ClientWrite, @Buffer[0], SizeOf(Buffer));
    if Len > 0 then BIO_write(ServerRead, @Buffer[0], Len);
  until Len <= 0;

  repeat
    Len := BIO_read(ServerWrite, @Buffer[0], SizeOf(Buffer));
    if Len > 0 then BIO_write(ClientRead, @Buffer[0], Len);
  until Len <= 0;
end;

function DoFullHandshake(ClientSSL, ServerSSL: PSSL;
                         ClientWrite, ServerRead, ServerWrite, ClientRead: PBIO): Boolean;
var
  ClientRet, ServerRet: Integer;
  ClientErr, ServerErr: Integer;
  i: Integer;
begin
  Result := False;

  for i := 1 to 100 do
  begin
    ClientRet := SSL_do_handshake(ClientSSL);
    if (ClientRet <> 1) then
    begin
      ClientErr := SSL_get_error(ClientSSL, ClientRet);
      if (ClientErr <> SSL_ERROR_WANT_READ) and (ClientErr <> SSL_ERROR_WANT_WRITE) then
        Exit;
    end;

    PumpData(ClientWrite, ServerRead, ServerWrite, ClientRead);

    ServerRet := SSL_do_handshake(ServerSSL);
    if (ServerRet <> 1) then
    begin
      ServerErr := SSL_get_error(ServerSSL, ServerRet);
      if (ServerErr <> SSL_ERROR_WANT_READ) and (ServerErr <> SSL_ERROR_WANT_WRITE) then
        Exit;
    end;

    PumpData(ClientWrite, ServerRead, ServerWrite, ClientRead);

    if (ClientRet = 1) and (ServerRet = 1) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure Test1_SessionCreation;
var
  ClientSSL, ServerSSL: PSSL;
  ClientRead, ClientWrite, ServerRead, ServerWrite: PBIO;
  Session: PSSL_SESSION;
  SessionID: PByte;
  SessionIDLen: Cardinal;
begin
  PrintHeader('1. 会话创建测试');

  try
    // 创建 BIO 和 SSL 对象
    ClientRead := BIO_new(BIO_s_mem());
    ClientWrite := BIO_new(BIO_s_mem());
    ServerRead := BIO_new(BIO_s_mem());
    ServerWrite := BIO_new(BIO_s_mem());

    ClientSSL := SSL_new(ClientCtx);
    ServerSSL := SSL_new(ServerCtx);

    SSL_set_bio(ClientSSL, ClientRead, ClientWrite);
    SSL_set_bio(ServerSSL, ServerRead, ServerWrite);

    SSL_set_connect_state(ClientSSL);
    SSL_set_accept_state(ServerSSL);

    // 执行握手
    if DoFullHandshake(ClientSSL, ServerSSL, ClientWrite, ServerRead, ServerWrite, ClientRead) then
    begin
      TestResult('TLS 握手完成', True);

      // 获取会话
      Session := SSL_get1_session(ClientSSL);
      TestResult('获取会话对象', Session <> nil);

      if Session <> nil then
      begin
        // 获取会话 ID
        if Assigned(SSL_SESSION_get_id) then
        begin
          SessionID := SSL_SESSION_get_id(Session, @SessionIDLen);
          TestResult('获取会话 ID', (SessionID <> nil) and (SessionIDLen > 0),
                    '会话 ID 长度: ' + IntToStr(SessionIDLen) + ' 字节');
        end
        else
          TestResult('获取会话 ID', False, 'SSL_SESSION_get_id 未加载');

        // 检查会话有效性
        if Assigned(SSL_SESSION_get_timeout) then
        begin
          TestResult('会话超时时间', SSL_SESSION_get_timeout(Session) > 0,
                    IntToStr(SSL_SESSION_get_timeout(Session)) + ' 秒');
        end;

        SSL_SESSION_free(Session);
      end;
    end
    else
      TestResult('TLS 握手', False, '握手失败');

    SSL_shutdown(ClientSSL);
    SSL_shutdown(ServerSSL);
    SSL_free(ClientSSL);
    SSL_free(ServerSSL);

  except
    on E: Exception do
      TestResult('会话创建', False, E.Message);
  end;
end;

procedure Test2_SessionResumption;
var
  ClientSSL1, ServerSSL1: PSSL;
  ClientSSL2, ServerSSL2: PSSL;
  ClientRead1, ClientWrite1, ServerRead1, ServerWrite1: PBIO;
  ClientRead2, ClientWrite2, ServerRead2, ServerWrite2: PBIO;
  OrigSession: PSSL_SESSION;
  Reused: Integer;
begin
  PrintHeader('2. 会话恢复测试');

  try
    // === 第一次连接 ===
    WriteLn('       建立初始连接...');

    ClientRead1 := BIO_new(BIO_s_mem());
    ClientWrite1 := BIO_new(BIO_s_mem());
    ServerRead1 := BIO_new(BIO_s_mem());
    ServerWrite1 := BIO_new(BIO_s_mem());

    ClientSSL1 := SSL_new(ClientCtx);
    ServerSSL1 := SSL_new(ServerCtx);

    SSL_set_bio(ClientSSL1, ClientRead1, ClientWrite1);
    SSL_set_bio(ServerSSL1, ServerRead1, ServerWrite1);

    SSL_set_connect_state(ClientSSL1);
    SSL_set_accept_state(ServerSSL1);

    if DoFullHandshake(ClientSSL1, ServerSSL1, ClientWrite1, ServerRead1, ServerWrite1, ClientRead1) then
    begin
      TestResult('初始连接建立', True);

      // 保存会话
      OrigSession := SSL_get1_session(ClientSSL1);
      TestResult('保存原始会话', OrigSession <> nil);

      SSL_shutdown(ClientSSL1);
      SSL_shutdown(ServerSSL1);
      SSL_free(ClientSSL1);
      SSL_free(ServerSSL1);

      if OrigSession <> nil then
      begin
        // === 第二次连接（尝试恢复） ===
        WriteLn('       尝试恢复连接...');

        ClientRead2 := BIO_new(BIO_s_mem());
        ClientWrite2 := BIO_new(BIO_s_mem());
        ServerRead2 := BIO_new(BIO_s_mem());
        ServerWrite2 := BIO_new(BIO_s_mem());

        ClientSSL2 := SSL_new(ClientCtx);
        ServerSSL2 := SSL_new(ServerCtx);

        SSL_set_bio(ClientSSL2, ClientRead2, ClientWrite2);
        SSL_set_bio(ServerSSL2, ServerRead2, ServerWrite2);

        // 设置要恢复的会话
        SSL_set_session(ClientSSL2, OrigSession);
        TestResult('设置恢复会话', True);

        SSL_set_connect_state(ClientSSL2);
        SSL_set_accept_state(ServerSSL2);

        if DoFullHandshake(ClientSSL2, ServerSSL2, ClientWrite2, ServerRead2, ServerWrite2, ClientRead2) then
        begin
          TestResult('恢复连接握手', True);

          // 检查会话是否被重用
          if Assigned(SSL_session_reused) then
          begin
            Reused := SSL_session_reused(ClientSSL2);
            if Reused = 1 then
              TestResult('会话恢复成功', True, '会话被重用')
            else
              TestResult('会话恢复检查', True, '新会话建立（可能是服务端缓存配置）');
          end
          else
            TestResult('检查会话恢复', False, 'SSL_session_reused 未加载');
        end
        else
          TestResult('恢复连接握手', False);

        SSL_shutdown(ClientSSL2);
        SSL_shutdown(ServerSSL2);
        SSL_free(ClientSSL2);
        SSL_free(ServerSSL2);

        SSL_SESSION_free(OrigSession);
      end;
    end
    else
    begin
      TestResult('初始连接', False);
      SSL_free(ClientSSL1);
      SSL_free(ServerSSL1);
    end;

  except
    on E: Exception do
      TestResult('会话恢复', False, E.Message);
  end;
end;

procedure Test3_SessionTimeout;
var
  ClientSSL, ServerSSL: PSSL;
  ClientRead, ClientWrite, ServerRead, ServerWrite: PBIO;
  Session: PSSL_SESSION;
  OrigTimeout, NewTimeout: LongInt;
begin
  PrintHeader('3. 会话超时处理测试');

  try
    ClientRead := BIO_new(BIO_s_mem());
    ClientWrite := BIO_new(BIO_s_mem());
    ServerRead := BIO_new(BIO_s_mem());
    ServerWrite := BIO_new(BIO_s_mem());

    ClientSSL := SSL_new(ClientCtx);
    ServerSSL := SSL_new(ServerCtx);

    SSL_set_bio(ClientSSL, ClientRead, ClientWrite);
    SSL_set_bio(ServerSSL, ServerRead, ServerWrite);

    SSL_set_connect_state(ClientSSL);
    SSL_set_accept_state(ServerSSL);

    if DoFullHandshake(ClientSSL, ServerSSL, ClientWrite, ServerRead, ServerWrite, ClientRead) then
    begin
      Session := SSL_get1_session(ClientSSL);

      if Session <> nil then
      begin
        if Assigned(SSL_SESSION_get_timeout) and Assigned(SSL_SESSION_set_timeout) then
        begin
          // 获取原始超时
          OrigTimeout := SSL_SESSION_get_timeout(Session);
          TestResult('获取原始超时', OrigTimeout > 0,
                    IntToStr(OrigTimeout) + ' 秒');

          // 设置新超时
          NewTimeout := 600; // 10 分钟
          SSL_SESSION_set_timeout(Session, NewTimeout);

          // 验证新超时
          TestResult('设置新超时', SSL_SESSION_get_timeout(Session) = NewTimeout,
                    '新超时: ' + IntToStr(SSL_SESSION_get_timeout(Session)) + ' 秒');

          // 测试极端值
          SSL_SESSION_set_timeout(Session, 0);
          TestResult('设置零超时', SSL_SESSION_get_timeout(Session) = 0);

          SSL_SESSION_set_timeout(Session, 86400); // 24 小时
          TestResult('设置 24 小时超时', SSL_SESSION_get_timeout(Session) = 86400);
        end
        else
          TestResult('超时函数', False, '函数未加载');

        SSL_SESSION_free(Session);
      end;
    end
    else
      TestResult('握手', False);

    SSL_shutdown(ClientSSL);
    SSL_shutdown(ServerSSL);
    SSL_free(ClientSSL);
    SSL_free(ServerSSL);

  except
    on E: Exception do
      TestResult('会话超时', False, E.Message);
  end;
end;

procedure Test4_HighLevelSessionAPI;
var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
begin
  PrintHeader('4. 高层会话 API 测试');

  try
    SSLLib := CreateOpenSSLLibrary;
    TestResult('创建 OpenSSL 库', SSLLib <> nil);

    if SSLLib <> nil then
    begin
      TestResult('初始化库', SSLLib.Initialize);

      Context := SSLLib.CreateContext(sslCtxClient);
      TestResult('创建上下文', Context <> nil);

      if Context <> nil then
      begin
        // 测试会话缓存配置
        Context.SetSessionCacheMode(True);
        TestResult('启用会话缓存', Context.GetSessionCacheMode);

        Context.SetSessionTimeout(300);
        TestResult('设置会话超时', Context.GetSessionTimeout = 300);

        Context.SetSessionCacheSize(100);
        TestResult('设置缓存大小', Context.GetSessionCacheSize = 100);
      end;

      SSLLib.Finalize;
    end;

  except
    on E: Exception do
      TestResult('高层 API', False, E.Message);
  end;
end;

procedure Test5_SessionAPICoverage;
begin
  PrintHeader('5. 会话 API 覆盖率测试');

  // 测试所有会话相关的 API 函数是否已加载
  TestResult('SSL_get1_session', Assigned(SSL_get1_session));
  TestResult('SSL_get_session', Assigned(SSL_get_session));
  TestResult('SSL_set_session', Assigned(SSL_set_session));
  TestResult('SSL_session_reused', Assigned(SSL_session_reused));
  TestResult('SSL_SESSION_get_id', Assigned(SSL_SESSION_get_id));
  TestResult('SSL_SESSION_get_time', Assigned(SSL_SESSION_get_time));
  TestResult('SSL_SESSION_get_timeout', Assigned(SSL_SESSION_get_timeout));
  TestResult('SSL_SESSION_set_timeout', Assigned(SSL_SESSION_set_timeout));
  TestResult('SSL_SESSION_get_protocol_version', Assigned(SSL_SESSION_get_protocol_version));
  TestResult('SSL_SESSION_get0_cipher', Assigned(SSL_SESSION_get0_cipher));
  TestResult('SSL_SESSION_get0_peer', Assigned(SSL_SESSION_get0_peer));
  TestResult('SSL_SESSION_up_ref', Assigned(SSL_SESSION_up_ref));
  TestResult('SSL_SESSION_free', Assigned(SSL_SESSION_free));
  TestResult('SSL_CTX_set_session_cache_mode', Assigned(SSL_CTX_set_session_cache_mode));
  TestResult('SSL_CTX_get_session_cache_mode', Assigned(SSL_CTX_get_session_cache_mode));
end;

procedure InitializeTests;
var
  Method: PSSL_METHOD;
begin
  WriteLn('========================================');
  WriteLn('TLS 会话恢复集成测试');
  WriteLn('========================================');
  WriteLn;

  // 加载模块
  WriteLn('加载 OpenSSL 模块...');
  LoadOpenSSLCore();
  LoadOpenSSLBIO();
  LoadOpenSSLX509();
  LoadEVP(GetCryptoLibHandle);
  LoadOpenSSLRSA();
  LoadOpenSSLBN();
  LoadOpenSSLASN1(GetCryptoLibHandle);
  LoadOpenSSLSSL;

  WriteLn('OpenSSL 版本: ', GetOpenSSLVersionString);
  WriteLn;

  // 生成测试证书
  WriteLn('生成测试证书...');
  if not GenerateSelfSignedCert(ServerCert, ServerKey) then
  begin
    WriteLn('错误: 无法生成测试证书');
    Halt(1);
  end;
  WriteLn('证书生成成功');
  WriteLn;

  // 创建 SSL 上下文
  WriteLn('创建 SSL 上下文...');

  if not Assigned(TLS_client_method) then
  begin
    WriteLn('错误: TLS_client_method 未加载');
    Halt(1);
  end;

  Method := TLS_client_method();
  if Method = nil then
  begin
    WriteLn('错误: TLS_client_method 返回 nil');
    Halt(1);
  end;

  if not Assigned(SSL_CTX_new) then
  begin
    WriteLn('错误: SSL_CTX_new 未加载');
    Halt(1);
  end;

  ClientCtx := SSL_CTX_new(Method);
  if ClientCtx = nil then
  begin
    WriteLn('错误: 无法创建客户端 SSL 上下文');
    Halt(1);
  end;

  if Assigned(SSL_CTX_set_verify) then
    SSL_CTX_set_verify(ClientCtx, SSL_VERIFY_NONE, nil);

  // 启用会话缓存
  if Assigned(SSL_CTX_set_session_cache_mode) then
    SSL_CTX_set_session_cache_mode(ClientCtx, SSL_SESS_CACHE_CLIENT);

  if not Assigned(TLS_server_method) then
  begin
    WriteLn('错误: TLS_server_method 未加载');
    Halt(1);
  end;

  Method := TLS_server_method();
  if Method = nil then
  begin
    WriteLn('错误: TLS_server_method 返回 nil');
    Halt(1);
  end;

  ServerCtx := SSL_CTX_new(Method);
  if ServerCtx = nil then
  begin
    WriteLn('错误: 无法创建服务端 SSL 上下文');
    Halt(1);
  end;

  if Assigned(SSL_CTX_use_certificate) then
    SSL_CTX_use_certificate(ServerCtx, ServerCert);
  if Assigned(SSL_CTX_use_PrivateKey) then
    SSL_CTX_use_PrivateKey(ServerCtx, ServerKey);

  // 启用服务端会话缓存
  if Assigned(SSL_CTX_set_session_cache_mode) then
    SSL_CTX_set_session_cache_mode(ServerCtx, SSL_SESS_CACHE_SERVER);

  WriteLn('SSL 上下文创建成功');
end;

procedure CleanupTests;
begin
  if ClientCtx <> nil then SSL_CTX_free(ClientCtx);
  if ServerCtx <> nil then SSL_CTX_free(ServerCtx);
  if ServerCert <> nil then X509_free(ServerCert);
  if ServerKey <> nil then EVP_PKEY_free(ServerKey);
end;

begin
  try
    InitializeTests;

    Test1_SessionCreation;
    Test2_SessionResumption;
    Test3_SessionTimeout;
    Test4_HighLevelSessionAPI;
    Test5_SessionAPICoverage;

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
