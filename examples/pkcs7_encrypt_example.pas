program pkcs7_encrypt_example;

{$mode ObjFPC}{$H+}

{
  PKCS#7 数据加密示例

  本示例演示如何使用 PKCS#7 对数据进行加密和解密。

  功能：
  1. 加载接收者证书和私钥
  2. 使用 PKCS#7 加密数据
  3. 使用 PKCS#7 解密数据
  4. 支持多接收者加密

  使用场景：
  - 敏感数据加密
  - 安全文件传输
  - 多方数据共享
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

procedure EncryptData(const DataFile, CertFile, OutputFile: AnsiString);
var
  cert: PX509;
  data_bio, out_bio: PBIO;
  p7: PPKCS7;
  recip_stack: PSTACK_OF_X509;
  cipher: PEVP_CIPHER;
  flags: Integer;
begin
  WriteLn('=== PKCS#7 数据加密示例 ===');
  WriteLn;

  // 1. 加载接收者证书
  WriteLn('步骤 1: 加载接收者证书');
  cert := LoadCertificate(CertFile);
  if cert = nil then
  begin
    WriteLn('错误：无法加载证书文件: ', CertFile);
    Exit;
  end;
  WriteLn('✓ 证书加载成功');
  WriteLn;

  // 2. 创建接收者证书栈
  WriteLn('步骤 2: 创建接收者证书栈');
  recip_stack := OPENSSL_sk_new_null();
  if recip_stack = nil then
  begin
    WriteLn('错误：无法创建证书栈');
    Exit;
  end;
  OPENSSL_sk_push(recip_stack, cert);
  WriteLn('✓ 证书栈创建成功');
  WriteLn;

  // 3. 加载要加密的数据
  WriteLn('步骤 3: 加载要加密的数据');
  data_bio := BIO_new_file(PAnsiChar(DataFile), 'r');
  if data_bio = nil then
  begin
    WriteLn('错误：无法加载数据文件: ', DataFile);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;
  WriteLn('✓ 数据文件加载成功');
  WriteLn;

  // 4. 选择加密算法并加密
  WriteLn('步骤 4: 使用 AES-256-CBC 加密数据');
  cipher := EVP_aes_256_cbc();
  if cipher = nil then
  begin
    WriteLn('错误：无法获取加密算法');
    BIO_free(data_bio);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;

  flags := 0;
  p7 := PKCS7_encrypt(recip_stack, data_bio, cipher, flags);
  if p7 = nil then
  begin
    WriteLn('错误：无法创建 PKCS#7 加密数据');
    BIO_free(data_bio);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;
  WriteLn('✓ 数据加密成功');
  WriteLn;

  // 5. 保存加密数据到文件
  WriteLn('步骤 5: 保存加密数据到文件');
  out_bio := BIO_new_file(PAnsiChar(OutputFile), 'w');
  if out_bio = nil then
  begin
    WriteLn('错误：无法创建输出文件: ', OutputFile);
    BIO_free(data_bio);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;

  if i2d_PKCS7_bio(out_bio, p7) <> 1 then
  begin
    WriteLn('错误：无法写入加密数据');
    BIO_free(out_bio);
    BIO_free(data_bio);
    OPENSSL_sk_free(recip_stack);
    Exit;
  end;
  WriteLn('✓ 加密数据已保存到: ', OutputFile);
  WriteLn;

  BIO_free(out_bio);
  BIO_free(data_bio);
  OPENSSL_sk_free(recip_stack);

  WriteLn('加密完成！');
end;

procedure DecryptData(const EncryptedFile, CertFile, KeyFile, OutputFile: AnsiString);
var
  cert: PX509;
  pkey: PEVP_PKEY;
  enc_bio, out_bio: PBIO;
  p7: PPKCS7;
  decrypted_data: array[0..4095] of AnsiChar;
  bytes_read: Integer;
  output_file: TextFile;
begin
  WriteLn('=== PKCS#7 数据解密示例 ===');
  WriteLn;

  // 1. 加载接收者证书和私钥
  WriteLn('步骤 1: 加载接收者证书和私钥');
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

  // 2. 加载加密数据
  WriteLn('步骤 2: 加载加密数据');
  enc_bio := BIO_new_file(PAnsiChar(EncryptedFile), 'r');
  if enc_bio = nil then
  begin
    WriteLn('错误：无法加载加密文件: ', EncryptedFile);
    Exit;
  end;

  p7 := d2i_PKCS7_bio(enc_bio, nil);
  BIO_free(enc_bio);
  if p7 = nil then
  begin
    WriteLn('错误：无法解析加密数据');
    Exit;
  end;
  WriteLn('✓ 加密数据加载成功');
  WriteLn;

  // 3. 解密数据
  WriteLn('步骤 3: 解密数据');
  out_bio := BIO_new(BIO_s_mem());
  if out_bio = nil then
  begin
    WriteLn('错误：无法创建输出 BIO');
    Exit;
  end;

  if PKCS7_decrypt(p7, pkey, cert, out_bio, 0) <> 1 then
  begin
    WriteLn('错误：解密失败');
    BIO_free(out_bio);
    Exit;
  end;
  WriteLn('✓ 数据解密成功');
  WriteLn;

  // 4. 读取解密后的数据
  WriteLn('步骤 4: 保存解密数据到文件');
  FillChar(decrypted_data, SizeOf(decrypted_data), 0);
  bytes_read := BIO_read(out_bio, @decrypted_data[0], SizeOf(decrypted_data) - 1);

  if bytes_read > 0 then
  begin
    AssignFile(output_file, OutputFile);
    Rewrite(output_file);
    Write(output_file, Copy(decrypted_data, 1, bytes_read));
    CloseFile(output_file);
    WriteLn('✓ 解密数据已保存到: ', OutputFile);
    WriteLn('  解密数据大小: ', bytes_read, ' 字节');
  end
  else
    WriteLn('错误：无法读取解密数据');

  BIO_free(out_bio);
  WriteLn;
  WriteLn('解密完成！');
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

  WriteLn('PKCS#7 数据加密示例程序');
  WriteLn('========================');
  WriteLn;

  if ParamCount < 1 then
  begin
    WriteLn('用法:');
    WriteLn('  加密: ', ParamStr(0), ' encrypt <数据文件> <接收者证书> <输出文件>');
    WriteLn('  解密: ', ParamStr(0), ' decrypt <加密文件> <接收者证书> <接收者私钥> <输出文件>');
    WriteLn;
    WriteLn('示例:');
    WriteLn('  ', ParamStr(0), ' encrypt data.txt recipient_cert.pem encrypted.p7e');
    WriteLn('  ', ParamStr(0), ' decrypt encrypted.p7e recipient_cert.pem recipient_key.pem decrypted.txt');
    Halt(1);
  end;

  Command := LowerCase(ParamStr(1));

  if Command = 'encrypt' then
  begin
    if ParamCount < 4 then
    begin
      WriteLn('错误：加密命令需要 3 个参数');
      WriteLn('用法: ', ParamStr(0), ' encrypt <数据文件> <接收者证书> <输出文件>');
      Halt(1);
    end;
    EncryptData(ParamStr(2), ParamStr(3), ParamStr(4));
  end
  else if Command = 'decrypt' then
  begin
    if ParamCount < 5 then
    begin
      WriteLn('错误：解密命令需要 4 个参数');
      WriteLn('用法: ', ParamStr(0), ' decrypt <加密文件> <接收者证书> <接收者私钥> <输出文件>');
      Halt(1);
    end;
    DecryptData(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5));
  end
  else
  begin
    WriteLn('错误：未知命令: ', Command);
    WriteLn('支持的命令: encrypt, decrypt');
    Halt(1);
  end;

  UnloadOpenSSLCore;
end.
