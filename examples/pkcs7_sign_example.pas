program pkcs7_sign_example;

{$mode ObjFPC}{$H+}

{
  PKCS#7 数字签名示例

  本示例演示如何使用 PKCS#7 对数据进行数字签名和验证。

  功能：
  1. 加载签名者证书和私钥
  2. 对数据进行 PKCS#7 签名
  3. 验证 PKCS#7 签名
  4. 保存和加载签名数据

  使用场景：
  - 文档签名
  - 代码签名
  - 数据完整性验证
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pem,
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

procedure SignData(const DataFile, CertFile, KeyFile, OutputFile: AnsiString);
var
  cert: PX509;
  pkey: PEVP_PKEY;
  data_bio, out_bio: PBIO;
  p7: PPKCS7;
  flags: Integer;
begin
  WriteLn('=== PKCS#7 数字签名示例 ===');
  WriteLn;

  // 1. 加载签名者证书和私钥
  WriteLn('步骤 1: 加载签名者证书和私钥');
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

  // 2. 加载要签名的数据
  WriteLn('步骤 2: 加载要签名的数据');
  data_bio := BIO_new_file(PAnsiChar(DataFile), 'r');
  if data_bio = nil then
  begin
    WriteLn('错误：无法加载数据文件: ', DataFile);
    Exit;
  end;
  WriteLn('✓ 数据文件加载成功');
  WriteLn;

  // 3. 创建 PKCS#7 签名
  WriteLn('步骤 3: 创建 PKCS#7 签名');
  flags := PKCS7_DETACHED or PKCS7_BINARY;  // 分离签名，二进制模式
  p7 := PKCS7_sign(cert, pkey, nil, data_bio, flags);
  if p7 = nil then
  begin
    WriteLn('错误：无法创建 PKCS#7 签名');
    BIO_free(data_bio);
    Exit;
  end;
  WriteLn('✓ PKCS#7 签名创建成功');
  WriteLn;

  // 4. 保存签名到文件
  WriteLn('步骤 4: 保存签名到文件');
  out_bio := BIO_new_file(PAnsiChar(OutputFile), 'w');
  if out_bio = nil then
  begin
    WriteLn('错误：无法创建输出文件: ', OutputFile);
    BIO_free(data_bio);
    Exit;
  end;

  if i2d_PKCS7_bio(out_bio, p7) <> 1 then
  begin
    WriteLn('错误：无法写入签名数据');
    BIO_free(out_bio);
    BIO_free(data_bio);
    Exit;
  end;
  WriteLn('✓ 签名已保存到: ', OutputFile);
  WriteLn;

  BIO_free(out_bio);
  BIO_free(data_bio);

  WriteLn('签名完成！');
end;

procedure VerifySignature(const DataFile, SignatureFile: AnsiString);
var
  data_bio, sig_bio: PBIO;
  p7: PPKCS7;
  flags: Integer;
  result: Integer;
begin
  WriteLn('=== PKCS#7 签名验证示例 ===');
  WriteLn;

  // 1. 加载签名文件
  WriteLn('步骤 1: 加载签名文件');
  sig_bio := BIO_new_file(PAnsiChar(SignatureFile), 'r');
  if sig_bio = nil then
  begin
    WriteLn('错误：无法加载签名文件: ', SignatureFile);
    Exit;
  end;

  p7 := d2i_PKCS7_bio(sig_bio, nil);
  BIO_free(sig_bio);
  if p7 = nil then
  begin
    WriteLn('错误：无法解析签名数据');
    Exit;
  end;
  WriteLn('✓ 签名文件加载成功');
  WriteLn;

  // 2. 加载原始数据
  WriteLn('步骤 2: 加载原始数据');
  data_bio := BIO_new_file(PAnsiChar(DataFile), 'r');
  if data_bio = nil then
  begin
    WriteLn('错误：无法加载数据文件: ', DataFile);
    Exit;
  end;
  WriteLn('✓ 数据文件加载成功');
  WriteLn;

  // 3. 验证签名
  WriteLn('步骤 3: 验证签名');
  flags := PKCS7_DETACHED or PKCS7_NOVERIFY;  // 分离签名，跳过证书验证
  result := PKCS7_verify(p7, nil, nil, data_bio, nil, flags);

  BIO_free(data_bio);

  WriteLn;
  if result = 1 then
    WriteLn('✓ 签名验证成功！数据完整且未被篡改。')
  else
    WriteLn('✗ 签名验证失败！数据可能已被篡改。');
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
  except
    on E: Exception do
    begin
      WriteLn('错误：无法初始化 OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;

  WriteLn('PKCS#7 数字签名示例程序');
  WriteLn('========================');
  WriteLn;

  if ParamCount < 1 then
  begin
    WriteLn('用法:');
    WriteLn('  签名: ', ParamStr(0), ' sign <数据文件> <证书文件> <私钥文件> <输出文件>');
    WriteLn('  验证: ', ParamStr(0), ' verify <数据文件> <签名文件>');
    WriteLn;
    WriteLn('示例:');
    WriteLn('  ', ParamStr(0), ' sign data.txt cert.pem key.pem signature.p7s');
    WriteLn('  ', ParamStr(0), ' verify data.txt signature.p7s');
    Halt(1);
  end;

  Command := LowerCase(ParamStr(1));

  if Command = 'sign' then
  begin
    if ParamCount < 5 then
    begin
      WriteLn('错误：签名命令需要 4 个参数');
      WriteLn('用法: ', ParamStr(0), ' sign <数据文件> <证书文件> <私钥文件> <输出文件>');
      Halt(1);
    end;
    SignData(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5));
  end
  else if Command = 'verify' then
  begin
    if ParamCount < 3 then
    begin
      WriteLn('错误：验证命令需要 2 个参数');
      WriteLn('用法: ', ParamStr(0), ' verify <数据文件> <签名文件>');
      Halt(1);
    end;
    VerifySignature(ParamStr(2), ParamStr(3));
  end
  else
  begin
    WriteLn('错误：未知命令: ', Command);
    WriteLn('支持的命令: sign, verify');
    Halt(1);
  end;

  UnloadOpenSSLCore;
end.
