program PKCS7EncryptDecryptExample;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.loader;

var
  LRecipientCert: PX509;
  LRecipientKey: PEVP_PKEY;
  LData: TBytes;
  LDataBIO: PBIO;
  LP7Encrypted: PPKCS7;
  LEncryptedBIO: PBIO;
  LEncryptedData: TBytes;
  LDecryptedBIO: PBIO;
  LDecryptedData: TBytes;
  LCertFile, LKeyFile: string;
  LBytesRead: Integer;

procedure PrintUsage;
begin
  WriteLn('PKCS#7 加密和解密示例');
  WriteLn('');
  WriteLn('用法:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' <cert.pem> <key.pem>');
  WriteLn('');
  WriteLn('参数:');
  WriteLn('  cert.pem  - 接收者证书文件（PEM 格式）');
  WriteLn('  key.pem   - 接收者私钥文件（PEM 格式）');
  WriteLn('');
  WriteLn('示例:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' recipient.pem recipient-key.pem');
end;

begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 加密和解密示例');
  WriteLn('=============================================================');
  WriteLn('');

  // 检查命令行参数
  if ParamCount < 2 then
  begin
    PrintUsage;
    Halt(1);
  end;

  LCertFile := ParamStr(1);
  LKeyFile := ParamStr(2);

  // 检查文件是否存在
  if not FileExists(LCertFile) then
  begin
    WriteLn('错误: 证书文件不存在: ', LCertFile);
    Halt(1);
  end;

  if not FileExists(LKeyFile) then
  begin
    WriteLn('错误: 私钥文件不存在: ', LKeyFile);
    Halt(1);
  end;

  try
    // 初始化 OpenSSL
    WriteLn('1. 初始化 OpenSSL 库...');
    if not TOpenSSLLoader.Initialize then
    begin
      WriteLn('错误: 无法初始化 OpenSSL 库');
      Halt(1);
    end;
    WriteLn('   ✓ OpenSSL 库初始化成功');
    WriteLn('');

    // 加载接收者证书
    WriteLn('2. 加载接收者证书...');
    LRecipientCert := LoadCertificateFromFile(LCertFile);
    if LRecipientCert = nil then
    begin
      WriteLn('错误: 无法加载证书文件');
      Halt(1);
    end;
    WriteLn('   ✓ 证书加载成功');
    WriteLn('');

    // 准备要加密的数据
    WriteLn('3. 准备要加密的数据...');
    LData := TEncoding.UTF8.GetBytes('这是一段需要加密的机密数据。PKCS#7 加密可以确保只有持有私钥的接收者才能解密。');
    WriteLn('   原始数据: ', TEncoding.UTF8.GetString(LData));
    WriteLn('   数据长度: ', Length(LData), ' 字节');
    WriteLn('');

    // 创建 BIO 对象
    LDataBIO := BIO_new_mem_buf(@LData[0], Length(LData));
    if LDataBIO = nil then
    begin
      WriteLn('错误: 无法创建 BIO 对象');
      X509_free(LRecipientCert);
      Halt(1);
    end;

    // 创建证书栈（用于多个接收者）
    WriteLn('4. 对数据进行 PKCS#7 加密...');
    WriteLn('   加密算法: AES-256-CBC');
    WriteLn('   接收者: 1 个');
    
    // 注意：这里需要创建一个 STACK_OF(X509) 来存放接收者证书
    // 为了简化示例，我们直接使用 PKCS7_encrypt
    LP7Encrypted := PKCS7_encrypt(LRecipientCert, LDataBIO, EVP_aes_256_cbc, PKCS7_BINARY);
    if LP7Encrypted = nil then
    begin
      WriteLn('错误: PKCS#7 加密失败');
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;
    WriteLn('   ✓ 加密成功');
    WriteLn('');

    // 将加密数据写入内存 BIO
    WriteLn('5. 导出加密数据...');
    LEncryptedBIO := BIO_new(BIO_s_mem);
    if LEncryptedBIO = nil then
    begin
      WriteLn('错误: 无法创建输出 BIO');
      PKCS7_free(LP7Encrypted);
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;

    if i2d_PKCS7_bio(LEncryptedBIO, LP7Encrypted) = 0 then
    begin
      WriteLn('错误: 无法导出加密数据');
      BIO_free(LEncryptedBIO);
      PKCS7_free(LP7Encrypted);
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;

    // 读取加密数据
    SetLength(LEncryptedData, BIO_ctrl_pending(LEncryptedBIO));
    BIO_read(LEncryptedBIO, @LEncryptedData[0], Length(LEncryptedData));
    WriteLn('   ✓ 加密数据导出成功');
    WriteLn('   加密数据长度: ', Length(LEncryptedData), ' 字节');
    WriteLn('');

    // 加载接收者私钥
    WriteLn('6. 加载接收者私钥...');
    LRecipientKey := LoadPrivateKeyFromFile(LKeyFile, '');
    if LRecipientKey = nil then
    begin
      WriteLn('错误: 无法加载私钥文件');
      BIO_free(LEncryptedBIO);
      PKCS7_free(LP7Encrypted);
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;
    WriteLn('   ✓ 私钥加载成功');
    WriteLn('');

    // 解密数据
    WriteLn('7. 解密 PKCS#7 加密数据...');
    LDecryptedBIO := BIO_new(BIO_s_mem);
    if LDecryptedBIO = nil then
    begin
      WriteLn('错误: 无法创建解密输出 BIO');
      EVP_PKEY_free(LRecipientKey);
      BIO_free(LEncryptedBIO);
      PKCS7_free(LP7Encrypted);
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;

    if PKCS7_decrypt(LP7Encrypted, LRecipientKey, LRecipientCert, LDecryptedBIO, 0) = 0 then
    begin
      WriteLn('错误: PKCS#7 解密失败');
      BIO_free(LDecryptedBIO);
      EVP_PKEY_free(LRecipientKey);
      BIO_free(LEncryptedBIO);
      PKCS7_free(LP7Encrypted);
      BIO_free(LDataBIO);
      X509_free(LRecipientCert);
      Halt(1);
    end;

    // 读取解密数据
    SetLength(LDecryptedData, BIO_ctrl_pending(LDecryptedBIO));
    LBytesRead := BIO_read(LDecryptedBIO, @LDecryptedData[0], Length(LDecryptedData));
    SetLength(LDecryptedData, LBytesRead);
    
    WriteLn('   ✓ 解密成功');
    WriteLn('   解密数据: ', TEncoding.UTF8.GetString(LDecryptedData));
    WriteLn('   解密数据长度: ', Length(LDecryptedData), ' 字节');
    WriteLn('');

    // 验证数据完整性
    WriteLn('8. 验证数据完整性...');
    if CompareMem(@LData[0], @LDecryptedData[0], Length(LData)) then
    begin
      WriteLn('   ✓ 数据完整性验证成功！');
      WriteLn('   原始数据和解密数据完全一致');
    end
    else
    begin
      WriteLn('   ✗ 数据完整性验证失败');
      WriteLn('   原始数据和解密数据不一致');
    end;
    WriteLn('');

    WriteLn('=============================================================');
    WriteLn('示例完成');
    WriteLn('=============================================================');
    WriteLn('');
    WriteLn('说明:');
    WriteLn('- PKCS#7 加密使用接收者的公钥加密对称密钥');
    WriteLn('- 实际数据使用对称加密算法（AES-256-CBC）加密');
    WriteLn('- 只有持有对应私钥的接收者才能解密');
    WriteLn('- 支持多个接收者（每个接收者都可以解密）');
    WriteLn('- 本示例使用单个接收者进行演示');

    // 清理资源
    BIO_free(LDecryptedBIO);
    EVP_PKEY_free(LRecipientKey);
    BIO_free(LEncryptedBIO);
    PKCS7_free(LP7Encrypted);
    BIO_free(LDataBIO);
    X509_free(LRecipientCert);

  except
    on E: Exception do
    begin
      WriteLn('异常: ', E.Message);
      Halt(1);
    end;
  end;
end.
