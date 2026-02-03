program PKCS7SignVerifyExample;

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
  LCert: PX509;
  LKey: PEVP_PKEY;
  LData: TBytes;
  LDataBIO: PBIO;
  LP7: PPKCS7;
  LOutBIO: PBIO;
  LSignedData: TBytes;
  LVerifyResult: Integer;
  LCertFile, LKeyFile: string;

procedure PrintUsage;
begin
  WriteLn('PKCS#7 签名和验证示例');
  WriteLn('');
  WriteLn('用法:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' <cert.pem> <key.pem>');
  WriteLn('');
  WriteLn('参数:');
  WriteLn('  cert.pem  - 签名证书文件（PEM 格式）');
  WriteLn('  key.pem   - 私钥文件（PEM 格式）');
  WriteLn('');
  WriteLn('示例:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' mycert.pem mykey.pem');
end;

begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 签名和验证示例');
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

    // 加载证书
    WriteLn('2. 加载签名证书...');
    LCert := LoadCertificateFromFile(LCertFile);
    if LCert = nil then
    begin
      WriteLn('错误: 无法加载证书文件');
      Halt(1);
    end;
    WriteLn('   ✓ 证书加载成功');
    WriteLn('');

    // 加载私钥
    WriteLn('3. 加载私钥...');
    LKey := LoadPrivateKeyFromFile(LKeyFile, '');
    if LKey = nil then
    begin
      WriteLn('错误: 无法加载私钥文件');
      X509_free(LCert);
      Halt(1);
    end;
    WriteLn('   ✓ 私钥加载成功');
    WriteLn('');

    // 准备要签名的数据
    WriteLn('4. 准备要签名的数据...');
    LData := TEncoding.UTF8.GetBytes('这是一段需要签名的重要数据。PKCS#7 签名可以确保数据的完整性和来源的真实性。');
    WriteLn('   数据内容: ', TEncoding.UTF8.GetString(LData));
    WriteLn('   数据长度: ', Length(LData), ' 字节');
    WriteLn('');

    // 创建 BIO 对象
    LDataBIO := BIO_new_mem_buf(@LData[0], Length(LData));
    if LDataBIO = nil then
    begin
      WriteLn('错误: 无法创建 BIO 对象');
      EVP_PKEY_free(LKey);
      X509_free(LCert);
      Halt(1);
    end;

    // 对数据进行签名
    WriteLn('5. 对数据进行 PKCS#7 签名...');
    LP7 := PKCS7_sign(LCert, LKey, nil, LDataBIO, PKCS7_DETACHED or PKCS7_BINARY);
    if LP7 = nil then
    begin
      WriteLn('错误: PKCS#7 签名失败');
      BIO_free(LDataBIO);
      EVP_PKEY_free(LKey);
      X509_free(LCert);
      Halt(1);
    end;
    WriteLn('   ✓ 签名成功');
    WriteLn('');

    // 将签名数据写入内存 BIO
    WriteLn('6. 导出签名数据...');
    LOutBIO := BIO_new(BIO_s_mem);
    if LOutBIO = nil then
    begin
      WriteLn('错误: 无法创建输出 BIO');
      PKCS7_free(LP7);
      BIO_free(LDataBIO);
      EVP_PKEY_free(LKey);
      X509_free(LCert);
      Halt(1);
    end;

    if i2d_PKCS7_bio(LOutBIO, LP7) = 0 then
    begin
      WriteLn('错误: 无法导出签名数据');
      BIO_free(LOutBIO);
      PKCS7_free(LP7);
      BIO_free(LDataBIO);
      EVP_PKEY_free(LKey);
      X509_free(LCert);
      Halt(1);
    end;

    // 读取签名数据
    SetLength(LSignedData, BIO_ctrl_pending(LOutBIO));
    BIO_read(LOutBIO, @LSignedData[0], Length(LSignedData));
    WriteLn('   ✓ 签名数据导出成功');
    WriteLn('   签名数据长度: ', Length(LSignedData), ' 字节');
    WriteLn('');

    // 验证签名
    WriteLn('7. 验证 PKCS#7 签名...');
    
    // 重置数据 BIO
    BIO_free(LDataBIO);
    LDataBIO := BIO_new_mem_buf(@LData[0], Length(LData));
    
    // 创建证书存储
    LVerifyResult := PKCS7_verify(LP7, nil, nil, LDataBIO, nil, PKCS7_NOVERIFY);
    
    if LVerifyResult = 1 then
    begin
      WriteLn('   ✓ 签名验证成功！');
      WriteLn('   数据完整性: 完好');
      WriteLn('   签名有效性: 有效');
    end
    else
    begin
      WriteLn('   ✗ 签名验证失败');
    end;
    WriteLn('');

    WriteLn('=============================================================');
    WriteLn('示例完成');
    WriteLn('=============================================================');
    WriteLn('');
    WriteLn('说明:');
    WriteLn('- PKCS#7 签名使用分离式签名（DETACHED）');
    WriteLn('- 签名包含证书信息，可以验证签名者身份');
    WriteLn('- 签名可以确保数据在传输过程中未被篡改');
    WriteLn('- 本示例使用 PKCS7_NOVERIFY 跳过证书链验证');
    WriteLn('- 生产环境应使用完整的证书链验证');

    // 清理资源
    BIO_free(LOutBIO);
    PKCS7_free(LP7);
    BIO_free(LDataBIO);
    EVP_PKEY_free(LKey);
    X509_free(LCert);

  except
    on E: Exception do
    begin
      WriteLn('异常: ', E.Message);
      Halt(1);
    end;
  end;
end.
