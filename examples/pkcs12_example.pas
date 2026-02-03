program pkcs12_example;

{$mode ObjFPC}{$H+}

{
  PKCS#12 证书和密钥打包示例

  本示例演示如何使用 PKCS#12 格式打包和提取证书与私钥。

  功能：
  1. 创建 PKCS#12 文件（证书+私钥+密码保护）
  2. 解析 PKCS#12 文件提取证书和私钥
  3. 支持证书链（CA 证书）
  4. MAC 完整性验证

  使用场景：
  - 证书和私钥的安全存储
  - 证书导入导出
  - 跨平台证书传输
  - 浏览器和服务器证书管理

  工作流程：
  创建：证书+私钥 -> PKCS#12 -> 加密文件
  解析：加密文件 -> PKCS#12 -> 证书+私钥
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

procedure CreatePKCS12(const CertFile, KeyFile, CAFile, Password, OutputFile, FriendlyName: AnsiString);
var
  cert, ca_cert: PX509;
  pkey: PEVP_PKEY;
  ca_stack: PSTACK_OF_X509;
  p12: PPKCS12;
  out_bio: PBIO;
begin
  WriteLn('=== PKCS#12 创建示例 ===');
  WriteLn;
  WriteLn('工作流程: 证书+私钥 -> PKCS#12 -> 加密文件');
  WriteLn;

  // 步骤 1: 加载证书和私钥
  WriteLn('步骤 1: 加载证书和私钥');
  cert := LoadCertificate(CertFile);
  if cert = nil then
  begin
    WriteLn('错误：无法加载证书文件: ', CertFile);
    Exit;
  end;
  WriteLn('✓ 证书加载成功');

  pkey := LoadPrivateKey(KeyFile);
  if pkey = nil then
  begin
    WriteLn('错误：无法加载私钥文件: ', KeyFile);
    Exit;
  end;
  WriteLn('✓ 私钥加载成功');
  WriteLn;

  // 步骤 2: 加载 CA 证书（可选）
  ca_stack := nil;
  if CAFile <> '' then
  begin
    WriteLn('步骤 2: 加载 CA 证书链');
    ca_cert := LoadCertificate(CAFile);
    if ca_cert <> nil then
    begin
      ca_stack := OPENSSL_sk_new_null();
      if ca_stack <> nil then
      begin
        OPENSSL_sk_push(ca_stack, ca_cert);
        WriteLn('✓ CA 证书加载成功');
      end;
    end;
    WriteLn;
  end;

  // 步骤 3: 创建 PKCS#12 结构
  WriteLn('步骤 3: 创建 PKCS#12 结构');
  p12 := PKCS12_create(
    PAnsiChar(Password),      // 密码
    PAnsiChar(FriendlyName),  // 友好名称
    pkey,                     // 私钥
    cert,                     // 证书
    ca_stack,                 // CA 证书链
    0,                        // nid_key (0 = 默认)
    0,                        // nid_cert (0 = 默认)
    0,                        // iter (0 = 默认 2048)
    0,                        // mac_iter (0 = 默认)
    0                         // keytype (0 = 默认)
  );

  if p12 = nil then
  begin
    WriteLn('错误：无法创建 PKCS#12 结构');
    if ca_stack <> nil then
      OPENSSL_sk_free(ca_stack);
    Exit;
  end;
  WriteLn('✓ PKCS#12 结构创建成功');
  WriteLn;

  // 步骤 4: 验证 MAC
  WriteLn('步骤 4: 验证 PKCS#12 完整性');
  if PKCS12_verify_mac(p12, PAnsiChar(Password), -1) = 1 then
    WriteLn('✓ MAC 验证成功')
  else
    WriteLn('✗ MAC 验证失败');
  WriteLn;

  // 步骤 5: 保存到文件
  WriteLn('步骤 5: 保存 PKCS#12 到文件');
  out_bio := BIO_new_file(PAnsiChar(OutputFile), 'wb');
  if out_bio = nil then
  begin
    WriteLn('错误：无法创建输出文件: ', OutputFile);
    if ca_stack <> nil then
      OPENSSL_sk_free(ca_stack);
    Exit;
  end;

  if i2d_PKCS12_bio(out_bio, p12) <> 1 then
  begin
    WriteLn('错误：无法写入 PKCS#12 数据');
    BIO_free(out_bio);
    if ca_stack <> nil then
      OPENSSL_sk_free(ca_stack);
    Exit;
  end;
  WriteLn('✓ PKCS#12 已保存到: ', OutputFile);
  BIO_free(out_bio);

  if ca_stack <> nil then
    OPENSSL_sk_free(ca_stack);

  WriteLn;
  WriteLn('PKCS#12 创建完成！');
  WriteLn('文件已加密保护，只有知道密码的人才能提取证书和私钥。');
end;

procedure ParsePKCS12(const PKCS12File, Password, CertOutput, KeyOutput: AnsiString);
var
  in_bio, cert_bio, key_bio: PBIO;
  p12: PPKCS12;
  cert: PX509;
  pkey: PEVP_PKEY;
  ca_stack: PSTACK_OF_X509;
  ca_count: Integer;
begin
  WriteLn('=== PKCS#12 解析示例 ===');
  WriteLn;
  WriteLn('工作流程: 加密文件 -> PKCS#12 -> 证书+私钥');
  WriteLn;

  // 步骤 1: 加载 PKCS#12 文件
  WriteLn('步骤 1: 加载 PKCS#12 文件');
  in_bio := BIO_new_file(PAnsiChar(PKCS12File), 'rb');
  if in_bio = nil then
  begin
    WriteLn('错误：无法加载 PKCS#12 文件: ', PKCS12File);
    Exit;
  end;

  p12 := nil;
  p12 := d2i_PKCS12_bio(in_bio, p12);
  BIO_free(in_bio);

  if p12 = nil then
  begin
    WriteLn('错误：无法解析 PKCS#12 数据');
    Exit;
  end;
  WriteLn('✓ PKCS#12 文件加载成功');
  WriteLn;

  // 步骤 2: 验证 MAC
  WriteLn('步骤 2: 验证 PKCS#12 完整性');
  if PKCS12_verify_mac(p12, PAnsiChar(Password), -1) = 1 then
    WriteLn('✓ MAC 验证成功')
  else
  begin
    WriteLn('✗ MAC 验证失败！密码可能不正确。');
    Exit;
  end;
  WriteLn;

  // 步骤 3: 解析 PKCS#12 提取证书和私钥
  WriteLn('步骤 3: 提取证书和私钥');
  cert := nil;
  pkey := nil;
  ca_stack := nil;

  if PKCS12_parse(p12, PAnsiChar(Password), pkey, cert, ca_stack) <> 1 then
  begin
    WriteLn('错误：无法解析 PKCS#12 数据');
    Exit;
  end;

  if cert = nil then
  begin
    WriteLn('错误：未找到证书');
    Exit;
  end;
  WriteLn('✓ 证书提取成功');

  if pkey = nil then
  begin
    WriteLn('错误：未找到私钥');
    Exit;
  end;
  WriteLn('✓ 私钥提取成功');

  if ca_stack <> nil then
  begin
    ca_count := OPENSSL_sk_num(ca_stack);
    WriteLn('✓ CA 证书链提取成功 (', ca_count, ' 个证书)');
  end;
  WriteLn;

  // 步骤 4: 保存证书到文件
  WriteLn('步骤 4: 保存证书到文件');
  cert_bio := BIO_new_file(PAnsiChar(CertOutput), 'w');
  if cert_bio = nil then
  begin
    WriteLn('错误：无法创建证书输出文件: ', CertOutput);
    Exit;
  end;

  if PEM_write_bio_X509(cert_bio, cert) <> 1 then
  begin
    WriteLn('错误：无法写入证书');
    BIO_free(cert_bio);
    Exit;
  end;
  WriteLn('✓ 证书已保存到: ', CertOutput);
  BIO_free(cert_bio);
  WriteLn;

  // 步骤 5: 保存私钥到文件
  WriteLn('步骤 5: 保存私钥到文件');
  key_bio := BIO_new_file(PAnsiChar(KeyOutput), 'w');
  if key_bio = nil then
  begin
    WriteLn('错误：无法创建私钥输出文件: ', KeyOutput);
    Exit;
  end;

  if PEM_write_bio_PrivateKey(key_bio, pkey, nil, nil, 0, nil, nil) <> 1 then
  begin
    WriteLn('错误：无法写入私钥');
    BIO_free(key_bio);
    Exit;
  end;
  WriteLn('✓ 私钥已保存到: ', KeyOutput);
  BIO_free(key_bio);

  WriteLn;
  WriteLn('PKCS#12 解析完成！');
  WriteLn('证书和私钥已成功提取并保存。');
end;

var
  Command: string;
begin
  // 初始化 OpenSSL
  try
    LoadOpenSSLCore;
    LoadOpenSSLBIO;
    LoadOpenSSLX509;
    LoadOpenSSLPEM(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto));
    LoadPKCS12Module(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto));
    LoadStackFunctions;
    LoadEVP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto));
  except
    on E: Exception do
    begin
      WriteLn('错误：无法初始化 OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;

  WriteLn('PKCS#12 证书和密钥打包示例程序');
  WriteLn('==================================');
  WriteLn;
  WriteLn('本程序演示如何使用 PKCS#12 格式安全地打包和提取证书与私钥：');
  WriteLn('- 创建：将证书、私钥和 CA 链打包成加密的 PKCS#12 文件');
  WriteLn('- 解析：从 PKCS#12 文件中提取证书和私钥');
  WriteLn('- 密码保护：使用密码保护 PKCS#12 文件');
  WriteLn('- MAC 验证：确保文件完整性');
  WriteLn;

  if ParamCount < 1 then
  begin
    WriteLn('用法:');
    WriteLn('  创建: ', ParamStr(0), ' create <证书文件> <私钥文件> <CA证书文件> <密码> <输出文件> <友好名称>');
    WriteLn('  解析: ', ParamStr(0), ' parse <PKCS12文件> <密码> <证书输出> <私钥输出>');
    WriteLn;
    WriteLn('示例:');
    WriteLn('  ', ParamStr(0), ' create cert.pem key.pem ca.pem mypassword mycert.p12 "My Certificate"');
    WriteLn('  ', ParamStr(0), ' parse mycert.p12 mypassword extracted_cert.pem extracted_key.pem');
    WriteLn;
    WriteLn('说明:');
    WriteLn('  - CA证书文件可以为空字符串 "" 表示不包含 CA 链');
    WriteLn('  - 密码用于保护 PKCS#12 文件，请使用强密码');
    WriteLn('  - 友好名称是证书的可读标识，可选');
    WriteLn('  - PKCS#12 文件通常使用 .p12 或 .pfx 扩展名');
    Halt(1);
  end;

  Command := LowerCase(ParamStr(1));

  if Command = 'create' then
  begin
    if ParamCount < 7 then
    begin
      WriteLn('错误：create 命令需要 6 个参数');
      WriteLn('用法: ', ParamStr(0), ' create <证书文件> <私钥文件> <CA证书文件> <密码> <输出文件> <友好名称>');
      Halt(1);
    end;
    CreatePKCS12(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5), ParamStr(6), ParamStr(7));
  end
  else if Command = 'parse' then
  begin
    if ParamCount < 5 then
    begin
      WriteLn('错误：parse 命令需要 4 个参数');
      WriteLn('用法: ', ParamStr(0), ' parse <PKCS12文件> <密码> <证书输出> <私钥输出>');
      Halt(1);
    end;
    ParsePKCS12(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5));
  end
  else
  begin
    WriteLn('错误：未知命令: ', Command);
    WriteLn('支持的命令: create, parse');
    Halt(1);
  end;

  UnloadOpenSSLCore;
end.
