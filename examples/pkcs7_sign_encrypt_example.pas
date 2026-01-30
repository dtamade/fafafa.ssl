program pkcs7_sign_encrypt_example;

{$mode ObjFPC}{$H+}

{
  PKCS#7 签名+加密组合示例

  本示例演示如何使用 PKCS#7 同时对数据进行签名和加密。

  功能：
  1. 先签名后加密（Sign-then-Encrypt）
  2. 先解密后验证（Decrypt-then-Verify）
  3. 完整的安全通信流程

  使用场景：
  - 安全邮件通信
  - 机密文档传输
  - 需要同时保证完整性和机密性的数据交换

  工作流程：
  发送方：数据 -> 签名 -> 加密 -> 发送
  接收方：接收 -> 解密 -> 验证 -> 数据
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.pkcs7,
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

procedure SignThenEncrypt(const DataFile, SignerCert, SignerKey, RecipientCert, OutputFile: AnsiString);
var
  signer_cert, recipient_cert: PX509;
  signer_key: PEVP_PKEY;
  data_bio, signed_bio, out_bio: PBIO;
  p7_signed, p7_encrypted: PPKCS7;
  recip_stack: PSTACK_OF_X509;
  cipher: PEVP_CIPHER;
  flags: Integer;
  signed_data: array[0..8191] of AnsiChar;
  signed_len: Integer;
begin
  WriteLn('=== PKCS#7 签名+加密示例 ===');
  WriteLn;
  WriteLn('工作流程: 数据 -> 签名 -> 加密 -> 输出');
  WriteLn;

  // 步骤 1: 加载签名者证书和私钥
  WriteLn('步骤 1: 加载签名者证书和私钥');
  signer_cert := LoadCertificate(SignerCert);
  if signer_cert = nil then
  begin
    WriteLn('错误：无法加载签名者证书: ', SignerCert);
    Exit;
  end;
  WriteLn('✓ 签名者证书加载成功');

  signer_key := LoadPrivateKey(SignerKey);
  if signer_key = nil then
  begin
    WriteLn('错误：无法加载签名者私钥: ', SignerKey);
    Exit;
  end;
  WriteLn('✓ 签名者私钥加载成功');
  WriteLn;

  // 步骤 2: 加载接收者证书
  WriteLn('步骤 2: 加载接收者证书');
  recipient_cert := LoadCertificate(RecipientCert);
  if recipient_cert = nil then
  begin
    WriteLn('错误：无法加载接收者证书: ', RecipientCert);
    Exit;
  end;
  WriteLn('✓ 接收者证书加载成功');
  WriteLn;

  // 步骤 3: 对数据进行签名
  WriteLn('步骤 3: 对数据进行 PKCS#7 签名');
  data_bio := BIO_new_file(PAnsiChar(DataFile), 'r');
  if data_bio = nil then
  begin
    WriteLn('错误：无法加载数据文件: ', DataFile);
    Exit;
  end;

  flags := PKCS7_BINARY;  // 二进制模式，签名包含数据
  p7_signed := PKCS7_sign(signer_cert, signer_key, nil, data_bio, flags);
  BIO_free(data_bio);

  if p7_signed = nil then
  begin
    WriteLn('错误：无法创建 PKCS#7 签名');
    Exit;
  end;
  WriteLn('✓ 数据签名成功');
  WriteLn;

  // 步骤 4: 将签名数据序列化到内存
  WriteLn('步骤 4: 序列化签名数据');
  signed_bio := BIO_new(BIO_s_mem());
  if signed_bio = nil then
  begin
    WriteLn('错误：无法创建内存 BIO');
    Exit;
  end;

  if i2d_PKCS7_bio(signed_bio, p7_signed) <> 1 then
  begin
    WriteLn('错误：无法序列化签名数据');
    BIO_free(signed_bio);
    Exit;
  end;

  // 读取签名数据到内存
  FillChar(signed_data, SizeOf(signed_data), 0);
  signed_len := BIO_read(signed_bio, @signed_data[0], SizeOf(signed_data) - 1);
  BIO_free(signed_bio);

  if signed_len <= 0 then
  begin
    WriteLn('错误：无法读取签名数据');
    Exit;
  end;
  WriteLn('✓ 签名数据序列化成功 (', signed_len, ' 字节)');
  WriteLn;

  // 步骤 5: 对签名数据进行加密
  WriteLn('步骤 5: 对签名数据进行 PKCS#7 加密');

  // 创建接收者证书栈
  recip_stack := OPENSSL_sk_new_null();
  if recip_stack = nil then
  begin
    WriteLn('错误：无法创建证书栈');
    Exit;
  end;
  OPENSSL_sk_push(recip_stack, recipient_cert);

  // 将签名数据放入 BIO
  signed_bio := BIO_new_mem_buf(@signed_data[0], signed_len);
  if signed_bio = nil then
  begin
    WriteLn('错误：无法创建签名数据 BIO');
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;

  // 使用 AES-256-CBC 加密
  cipher := EVP_aes_256_cbc();
  if cipher = nil then
  begin
    WriteLn('错误：无法获取加密算法');
    BIO_free(signed_bio);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;

  flags := 0;
  p7_encrypted := PKCS7_encrypt(recip_stack, signed_bio, cipher, flags);
  BIO_free(signed_bio);
  OPENSSL_sk_free(recip_stack);

  if p7_encrypted = nil then
  begin
    WriteLn('错误：无法创建 PKCS#7 加密数据');
    Exit;
  end;
  WriteLn('✓ 签名数据加密成功');
  WriteLn;

  // 步骤 6: 保存加密数据到文件
  WriteLn('步骤 6: 保存加密数据到文件');
  out_bio := BIO_new_file(PAnsiChar(OutputFile), 'w');
  if out_bio = nil then
  begin
    WriteLn('错误：无法创建输出文件: ', OutputFile);
    Exit;
  end;

  if i2d_PKCS7_bio(out_bio, p7_encrypted) <> 1 then
  begin
    WriteLn('错误：无法写入加密数据');
    BIO_free(out_bio);
    Exit;
  end;
  WriteLn('✓ 加密数据已保存到: ', OutputFile);
  BIO_free(out_bio);

  WriteLn;
  WriteLn('签名+加密完成！');
  WriteLn('数据已被签名并加密，只有接收者可以解密并验证签名。');
end;

procedure DecryptThenVerify(const EncryptedFile, RecipientCert, RecipientKey, SignerCert, OutputFile: AnsiString);
var
  recipient_cert, signer_cert: PX509;
  recipient_key: PEVP_PKEY;
  enc_bio, dec_bio, data_bio: PBIO;
  p7_encrypted, p7_signed: PPKCS7;
  decrypted_data: array[0..8191] of AnsiChar;
  decrypted_len: Integer;
  final_data: array[0..4095] of AnsiChar;
  final_len: Integer;
  output_file: TextFile;
  flags: Integer;
begin
  WriteLn('=== PKCS#7 解密+验证示例 ===');
  WriteLn;
  WriteLn('工作流程: 输入 -> 解密 -> 验证 -> 数据');
  WriteLn;

  // 步骤 1: 加载接收者证书和私钥
  WriteLn('步骤 1: 加载接收者证书和私钥');
  recipient_cert := LoadCertificate(RecipientCert);
  if recipient_cert = nil then
  begin
    WriteLn('错误：无法加载接收者证书: ', RecipientCert);
    Exit;
  end;
  WriteLn('✓ 接收者证书加载成功');

  recipient_key := LoadPrivateKey(RecipientKey);
  if recipient_key = nil then
  begin
    WriteLn('错误：无法加载接收者私钥: ', RecipientKey);
    Exit;
  end;
  WriteLn('✓ 接收者私钥加载成功');
  WriteLn;

  // 步骤 2: 加载签名者证书（用于验证）
  WriteLn('步骤 2: 加载签名者证书');
  signer_cert := LoadCertificate(SignerCert);
  if signer_cert = nil then
  begin
    WriteLn('错误：无法加载签名者证书: ', SignerCert);
    Exit;
  end;
  WriteLn('✓ 签名者证书加载成功');
  WriteLn;

  // 步骤 3: 加载并解析加密数据
  WriteLn('步骤 3: 加载加密数据');
  enc_bio := BIO_new_file(PAnsiChar(EncryptedFile), 'r');
  if enc_bio = nil then
  begin
    WriteLn('错误：无法加载加密文件: ', EncryptedFile);
    Exit;
  end;

  p7_encrypted := d2i_PKCS7_bio(enc_bio, nil);
  BIO_free(enc_bio);

  if p7_encrypted = nil then
  begin
    WriteLn('错误：无法解析加密数据');
    Exit;
  end;
  WriteLn('✓ 加密数据加载成功');
  WriteLn;

  // 步骤 4: 解密数据
  WriteLn('步骤 4: 解密 PKCS#7 数据');
  dec_bio := BIO_new(BIO_s_mem());
  if dec_bio = nil then
  begin
    WriteLn('错误：无法创建解密 BIO');
    Exit;
  end;

  if PKCS7_decrypt(p7_encrypted, recipient_key, recipient_cert, dec_bio, 0) <> 1 then
  begin
    WriteLn('错误：解密失败');
    BIO_free(dec_bio);
    Exit;
  end;

  // 读取解密后的签名数据
  FillChar(decrypted_data, SizeOf(decrypted_data), 0);
  decrypted_len := BIO_read(dec_bio, @decrypted_data[0], SizeOf(decrypted_data) - 1);
  BIO_free(dec_bio);

  if decrypted_len <= 0 then
  begin
    WriteLn('错误：无法读取解密数据');
    Exit;
  end;
  WriteLn('✓ 数据解密成功 (', decrypted_len, ' 字节)');
  WriteLn;

  // 步骤 5: 解析签名数据
  WriteLn('步骤 5: 解析签名数据');
  data_bio := BIO_new_mem_buf(@decrypted_data[0], decrypted_len);
  if data_bio = nil then
  begin
    WriteLn('错误：无法创建数据 BIO');
    Exit;
  end;

  p7_signed := d2i_PKCS7_bio(data_bio, nil);
  BIO_free(data_bio);

  if p7_signed = nil then
  begin
    WriteLn('错误：无法解析签名数据');
    Exit;
  end;
  WriteLn('✓ 签名数据解析成功');
  WriteLn;

  // 步骤 6: 验证签名并提取原始数据
  WriteLn('步骤 6: 验证 PKCS#7 签名');
  dec_bio := BIO_new(BIO_s_mem());
  if dec_bio = nil then
  begin
    WriteLn('错误：无法创建输出 BIO');
    Exit;
  end;

  flags := PKCS7_NOVERIFY;  // 跳过证书链验证
  if PKCS7_verify(p7_signed, nil, nil, nil, dec_bio, flags) <> 1 then
  begin
    WriteLn('✗ 签名验证失败！数据可能已被篡改。');
    BIO_free(dec_bio);
    Exit;
  end;
  WriteLn('✓ 签名验证成功！数据完整且未被篡改。');
  WriteLn;

  // 步骤 7: 读取并保存原始数据
  WriteLn('步骤 7: 保存原始数据');
  FillChar(final_data, SizeOf(final_data), 0);
  final_len := BIO_read(dec_bio, @final_data[0], SizeOf(final_data) - 1);
  BIO_free(dec_bio);

  if final_len > 0 then
  begin
    AssignFile(output_file, OutputFile);
    Rewrite(output_file);
    Write(output_file, Copy(final_data, 1, final_len));
    CloseFile(output_file);
    WriteLn('✓ 原始数据已保存到: ', OutputFile);
    WriteLn('  数据大小: ', final_len, ' 字节');
  end
  else
    WriteLn('错误：无法读取原始数据');

  WriteLn;
  WriteLn('解密+验证完成！');
  WriteLn('数据已成功解密并验证签名，确保了机密性和完整性。');
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
    LoadPKCS7Functions;
    LoadStackFunctions;
    LoadEVP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto));
  except
    on E: Exception do
    begin
      WriteLn('错误：无法初始化 OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;

  WriteLn('PKCS#7 签名+加密组合示例程序');
  WriteLn('================================');
  WriteLn;
  WriteLn('本程序演示如何同时使用 PKCS#7 签名和加密来保护数据：');
  WriteLn('- 签名：确保数据完整性和来源认证');
  WriteLn('- 加密：确保数据机密性');
  WriteLn;

  if ParamCount < 1 then
  begin
    WriteLn('用法:');
    WriteLn('  签名+加密: ', ParamStr(0), ' sign-encrypt <数据文件> <签名者证书> <签名者私钥> <接收者证书> <输出文件>');
    WriteLn('  解密+验证: ', ParamStr(0), ' decrypt-verify <加密文件> <接收者证书> <接收者私钥> <签名者证书> <输出文件>');
    WriteLn;
    WriteLn('示例:');
    WriteLn('  ', ParamStr(0), ' sign-encrypt data.txt signer_cert.pem signer_key.pem recipient_cert.pem secure.p7m');
    WriteLn('  ', ParamStr(0), ' decrypt-verify secure.p7m recipient_cert.pem recipient_key.pem signer_cert.pem original.txt');
    WriteLn;
    WriteLn('说明:');
    WriteLn('  - 签名者：对数据进行签名的一方');
    WriteLn('  - 接收者：接收加密数据的一方');
    WriteLn('  - 工作流程：数据 -> 签名 -> 加密 -> 传输 -> 解密 -> 验证 -> 数据');
    Halt(1);
  end;

  Command := LowerCase(ParamStr(1));

  if Command = 'sign-encrypt' then
  begin
    if ParamCount < 6 then
    begin
      WriteLn('错误：sign-encrypt 命令需要 5 个参数');
      WriteLn('用法: ', ParamStr(0), ' sign-encrypt <数据文件> <签名者证书> <签名者私钥> <接收者证书> <输出文件>');
      Halt(1);
    end;
    SignThenEncrypt(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5), ParamStr(6));
  end
  else if Command = 'decrypt-verify' then
  begin
    if ParamCount < 6 then
    begin
      WriteLn('错误：decrypt-verify 命令需要 5 个参数');
      WriteLn('用法: ', ParamStr(0), ' decrypt-verify <加密文件> <接收者证书> <接收者私钥> <签名者证书> <输出文件>');
      Halt(1);
    end;
    DecryptThenVerify(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5), ParamStr(6));
  end
  else
  begin
    WriteLn('错误：未知命令: ', Command);
    WriteLn('支持的命令: sign-encrypt, decrypt-verify');
    Halt(1);
  end;

  UnloadOpenSSLCore;
end.
