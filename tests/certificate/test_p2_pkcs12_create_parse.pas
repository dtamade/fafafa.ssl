program test_p2_pkcs12_create_parse;

{$mode ObjFPC}{$H+}

{
  PKCS#12 创建和解析功能测试

  测试范围：
  1. PKCS12 文件创建（证书+私钥）
  2. PKCS12 文件解析
  3. 密码保护
  4. MAC 完整性验证
  5. 证书链处理

  功能级别：生产级功能测试

  依赖模块：
  - fafafa.ssl.openssl.api.core (OpenSSL 加载)
  - fafafa.ssl.openssl.api.pkcs12 (PKCS12 API)
  - fafafa.ssl.openssl.api.x509 (X.509 证书)
  - fafafa.ssl.openssl.api.evp (EVP 加密)
  - fafafa.ssl.openssl.api.bio (BIO I/O)
  - fafafa.ssl.openssl.api.pem (PEM 编码)
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.stack,
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

function LoadCertificate(const FileName: AnsiString): PX509;
var
  bio: PBIO;
begin
  Result := nil;
  bio := BIO_new_file(PAnsiChar(FileName), 'r');
  if bio <> nil then
  begin
    Result := PEM_read_bio_X509(bio, nil, nil, nil);
    BIO_free(bio);
  end;
end;

function LoadPrivateKey(const FileName: AnsiString): PEVP_PKEY;
var
  bio: PBIO;
begin
  Result := nil;
  bio := BIO_new_file(PAnsiChar(FileName), 'r');
  if bio <> nil then
  begin
    Result := PEM_read_bio_PrivateKey(bio, nil, nil, nil);
    BIO_free(bio);
  end;
end;

procedure TestPKCS12_BasicCreateParse;
var
  cert: PX509;
  pkey: PEVP_PKEY;
  p12: PPKCS12;
  out_bio: PBIO;
  parsed_cert: PX509;
  parsed_pkey: PEVP_PKEY;
  parsed_ca: PSTACK_OF_X509;
  password: PAnsiChar;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 1: PKCS12 基本创建和解析 ===');

  // 加载测试证书和私钥
  cert := LoadCertificate('./tests/certificate/test_certs/signer_cert.pem');
  Test('加载测试证书', cert <> nil);

  pkey := LoadPrivateKey('./tests/certificate/test_certs/signer_key.pem');
  Test('加载测试私钥', pkey <> nil);

  if (cert = nil) or (pkey = nil) then
  begin
    WriteLn('错误：无法加载测试证书或私钥');
    Exit;
  end;

  // 创建 PKCS12 结构
  password := 'test123';
  p12 := PKCS12_create(
    password,           // 密码
    'Test Certificate', // 友好名称
    pkey,              // 私钥
    cert,              // 证书
    nil,               // CA 证书链
    0,                 // nid_key (0 = 默认)
    0,                 // nid_cert (0 = 默认)
    0,                 // iter (0 = 默认)
    0,                 // mac_iter (0 = 默认)
    0                  // keytype (0 = 默认)
  );
  Test('创建 PKCS12 结构', p12 <> nil);

  if p12 = nil then
  begin
    WriteLn('错误：无法创建 PKCS12 结构');
    Exit;
  end;

  // 验证 MAC
  LResult := PKCS12_verify_mac(p12, password, -1) = 1;
  Test('验证 PKCS12 MAC', LResult);

  // 将 PKCS12 写入内存 BIO
  out_bio := BIO_new(BIO_s_mem());
  Test('创建内存 BIO', out_bio <> nil);

  if out_bio <> nil then
  begin
    LResult := i2d_PKCS12_bio(out_bio, p12) = 1;
    Test('序列化 PKCS12 到 BIO', LResult);

    // 从 BIO 读取并解析
    parsed_cert := nil;
    parsed_pkey := nil;
    parsed_ca := nil;

    LResult := PKCS12_parse(p12, password, parsed_pkey, parsed_cert, parsed_ca) = 1;
    Test('解析 PKCS12 结构', LResult);

    if LResult then
    begin
      Test('解析得到证书', parsed_cert <> nil);
      Test('解析得到私钥', parsed_pkey <> nil);
    end;

    BIO_free(out_bio);
  end;

  WriteLn;
  WriteLn('基本创建和解析测试完成！');
end;

procedure TestPKCS12_WithCAChain;
var
  cert, ca_cert: PX509;
  pkey: PEVP_PKEY;
  ca_stack: PSTACK_OF_X509;
  p12: PPKCS12;
  parsed_cert: PX509;
  parsed_pkey: PEVP_PKEY;
  parsed_ca: PSTACK_OF_X509;
  password: PAnsiChar;
  LResult: Boolean;
  ca_count: Integer;
begin
  WriteLn;
  WriteLn('=== 测试 2: PKCS12 证书链处理 ===');

  // 加载证书、私钥和 CA 证书
  cert := LoadCertificate('./tests/certificate/test_certs/signer_cert.pem');
  Test('加载签名者证书', cert <> nil);

  pkey := LoadPrivateKey('./tests/certificate/test_certs/signer_key.pem');
  Test('加载签名者私钥', pkey <> nil);

  ca_cert := LoadCertificate('./tests/certificate/test_certs/ca_cert.pem');
  Test('加载 CA 证书', ca_cert <> nil);

  if (cert = nil) or (pkey = nil) or (ca_cert = nil) then
  begin
    WriteLn('错误：无法加载必需的证书或私钥');
    Exit;
  end;

  // 创建 CA 证书栈
  ca_stack := OPENSSL_sk_new_null();
  Test('创建 CA 证书栈', ca_stack <> nil);

  if ca_stack <> nil then
  begin
    OPENSSL_sk_push(ca_stack, ca_cert);
    Test('添加 CA 证书到栈', True);

    // 创建包含 CA 链的 PKCS12
    password := 'test456';
    p12 := PKCS12_create(
      password,
      'Certificate with CA Chain',
      pkey,
      cert,
      ca_stack,  // CA 证书链
      0, 0, 0, 0, 0
    );
    Test('创建包含 CA 链的 PKCS12', p12 <> nil);

    if p12 <> nil then
    begin
      // 验证 MAC
      LResult := PKCS12_verify_mac(p12, password, -1) = 1;
      Test('验证 PKCS12 MAC', LResult);

      // 解析 PKCS12
      parsed_cert := nil;
      parsed_pkey := nil;
      parsed_ca := nil;

      LResult := PKCS12_parse(p12, password, parsed_pkey, parsed_cert, parsed_ca) = 1;
      Test('解析包含 CA 链的 PKCS12', LResult);

      if LResult then
      begin
        Test('解析得到证书', parsed_cert <> nil);
        Test('解析得到私钥', parsed_pkey <> nil);
        Test('解析得到 CA 链', parsed_ca <> nil);

        if parsed_ca <> nil then
        begin
          ca_count := OPENSSL_sk_num(parsed_ca);
          Test('CA 链包含证书', ca_count > 0);
          WriteLn('  CA 链证书数量: ', ca_count);
        end;
      end;
    end;

    OPENSSL_sk_free(ca_stack);
  end;

  WriteLn;
  WriteLn('证书链处理测试完成！');
end;

procedure TestPKCS12_PasswordProtection;
var
  cert: PX509;
  pkey: PEVP_PKEY;
  p12: PPKCS12;
  parsed_cert: PX509;
  parsed_pkey: PEVP_PKEY;
  parsed_ca: PSTACK_OF_X509;
  correct_password, wrong_password: PAnsiChar;
  LResult: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试 3: PKCS12 密码保护 ===');

  // 加载测试证书和私钥
  cert := LoadCertificate('./tests/certificate/test_certs/recipient_cert.pem');
  Test('加载测试证书', cert <> nil);

  pkey := LoadPrivateKey('./tests/certificate/test_certs/recipient_key.pem');
  Test('加载测试私钥', pkey <> nil);

  if (cert = nil) or (pkey = nil) then
  begin
    WriteLn('错误：无法加载测试证书或私钥');
    Exit;
  end;

  // 创建带密码的 PKCS12
  correct_password := 'SecurePassword123!';
  p12 := PKCS12_create(
    correct_password,
    'Password Protected',
    pkey,
    cert,
    nil,
    0, 0, 0, 0, 0
  );
  Test('创建密码保护的 PKCS12', p12 <> nil);

  if p12 <> nil then
  begin
    // 测试正确密码
    LResult := PKCS12_verify_mac(p12, correct_password, -1) = 1;
    Test('使用正确密码验证 MAC', LResult);

    // 测试错误密码
    wrong_password := 'WrongPassword';
    LResult := PKCS12_verify_mac(p12, wrong_password, -1) = 1;
    Test('使用错误密码验证 MAC（应该失败）', not LResult);

    // 使用正确密码解析
    parsed_cert := nil;
    parsed_pkey := nil;
    parsed_ca := nil;

    LResult := PKCS12_parse(p12, correct_password, parsed_pkey, parsed_cert, parsed_ca) = 1;
    Test('使用正确密码解析 PKCS12', LResult);

    if LResult then
    begin
      Test('成功提取证书', parsed_cert <> nil);
      Test('成功提取私钥', parsed_pkey <> nil);
    end;

    // 尝试使用错误密码解析（应该失败）
    parsed_cert := nil;
    parsed_pkey := nil;
    parsed_ca := nil;

    LResult := PKCS12_parse(p12, wrong_password, parsed_pkey, parsed_cert, parsed_ca) = 1;
    Test('使用错误密码解析 PKCS12（应该失败）', not LResult);
  end;

  WriteLn;
  WriteLn('密码保护测试完成！');
end;

procedure TestPKCS12_FileIO;
var
  cert: PX509;
  pkey: PEVP_PKEY;
  p12, loaded_p12: PPKCS12;
  out_bio, in_bio: PBIO;
  password: PAnsiChar;
  temp_file: AnsiString;
  LResult: Boolean;
  parsed_cert: PX509;
  parsed_pkey: PEVP_PKEY;
  parsed_ca: PSTACK_OF_X509;
begin
  WriteLn;
  WriteLn('=== 测试 4: PKCS12 文件 I/O ===');

  // 加载测试证书和私钥
  cert := LoadCertificate('./tests/certificate/test_certs/signer_cert.pem');
  Test('加载测试证书', cert <> nil);

  pkey := LoadPrivateKey('./tests/certificate/test_certs/signer_key.pem');
  Test('加载测试私钥', pkey <> nil);

  if (cert = nil) or (pkey = nil) then
  begin
    WriteLn('错误：无法加载测试证书或私钥');
    Exit;
  end;

  // 创建 PKCS12
  password := 'filetest';
  p12 := PKCS12_create(
    password,
    'File I/O Test',
    pkey,
    cert,
    nil,
    0, 0, 0, 0, 0
  );
  Test('创建 PKCS12 结构', p12 <> nil);

  if p12 <> nil then
  begin
    // 保存到文件
    temp_file := '/tmp/test_pkcs12.p12';
    out_bio := BIO_new_file(PAnsiChar(temp_file), 'wb');
    Test('创建输出文件', out_bio <> nil);

    if out_bio <> nil then
    begin
      LResult := i2d_PKCS12_bio(out_bio, p12) = 1;
      Test('写入 PKCS12 到文件', LResult);
      BIO_free(out_bio);

      // 从文件加载
      in_bio := BIO_new_file(PAnsiChar(temp_file), 'rb');
      Test('打开输入文件', in_bio <> nil);

      if in_bio <> nil then
      begin
        loaded_p12 := nil;
        loaded_p12 := d2i_PKCS12_bio(in_bio, loaded_p12);
        Test('从文件读取 PKCS12', loaded_p12 <> nil);
        BIO_free(in_bio);

        if loaded_p12 <> nil then
        begin
          // 验证加载的 PKCS12
          LResult := PKCS12_verify_mac(loaded_p12, password, -1) = 1;
          Test('验证加载的 PKCS12 MAC', LResult);

          // 解析加载的 PKCS12
          parsed_cert := nil;
          parsed_pkey := nil;
          parsed_ca := nil;

          LResult := PKCS12_parse(loaded_p12, password, parsed_pkey, parsed_cert, parsed_ca) = 1;
          Test('解析加载的 PKCS12', LResult);

          if LResult then
          begin
            Test('从文件提取证书', parsed_cert <> nil);
            Test('从文件提取私钥', parsed_pkey <> nil);
          end;
        end;
      end;

      // 清理临时文件
      DeleteFile(temp_file);
    end;
  end;

  WriteLn;
  WriteLn('文件 I/O 测试完成！');
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('=' + StringOfChar('=', 60));
  WriteLn('PKCS#12 创建和解析功能测试');
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
      WriteLn('检测到 OpenSSL 3.x');
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误：无法加载 OpenSSL 库: ', E.Message);
      Halt(1);
    end;
  end;

  // 加载必需的 OpenSSL 模块
  WriteLn;
  WriteLn('加载 OpenSSL 模块...');

  LoadOpenSSLBIO;
  WriteLn('✅ BIO 模块加载成功');

  LoadOpenSSLX509;
  WriteLn('✅ X509 模块加载成功');

  if LoadOpenSSLPEM(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    WriteLn('✅ PEM 模块加载成功')
  else
  begin
    WriteLn('❌ PEM 模块加载失败');
    Halt(1);
  end;

  try
    LoadPKCS12Module(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto));
    WriteLn('✅ PKCS12 模块加载成功');
  except
    on E: Exception do
    begin
      WriteLn('❌ PKCS12 模块加载失败: ', E.Message);
      Halt(1);
    end;
  end;

  if LoadStackFunctions then
    WriteLn('✅ Stack 模块加载成功')
  else
  begin
    WriteLn('❌ Stack 模块加载失败');
    Halt(1);
  end;

  if LoadEVP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    WriteLn('✅ EVP 模块加载成功')
  else
  begin
    WriteLn('❌ EVP 模块加载失败');
    Halt(1);
  end;

  // 执行测试套件
  TestPKCS12_BasicCreateParse;
  TestPKCS12_WithCAChain;
  TestPKCS12_PasswordProtection;
  TestPKCS12_FileIO;

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
    WriteLn('🎉 所有功能测试通过！PKCS#12 创建和解析功能正常');
  end;

  UnloadOpenSSLCore;
end.
