program example_crypto_working;

{$mode objfpc}{$H+}

{
  可工作的加密示例
  直接复制 test_evp_cipher 的模式
}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

function BytesToHex(const Data: array of Byte; Len: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Len - 1 do
    Result := Result + IntToHex(Data[i], 2);
end;

procedure Example_AES_GCM;
const
  TestKey: array[0..31] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
    $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f
  );
  TestIV: array[0..11] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b
  );
  TestPlaintext: AnsiString = 'Hello, fafafa.ssl!';
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  ciphertext: array of Byte;
  plaintext: array of Byte;
  tag: array[0..15] of Byte;
  outlen, tmplen: Integer;
begin
  WriteLn('=== AES-256-GCM 加密/解密示例 ===');
  WriteLn;
  
  // 获取密码算法
  cipher := EVP_aes_256_gcm();
  if not Assigned(cipher) then
  begin
    WriteLn('✗ 无法获取 AES-256-GCM 算法');
    Exit;
  end;
  
  // === 加密 ===
  WriteLn('[1] 加密...');
  ctx := EVP_CIPHER_CTX_new();
  if not Assigned(ctx) then
  begin
    WriteLn('✗ 无法创建加密上下文');
    Exit;
  end;
  
  try
    // 初始化加密
    if EVP_EncryptInit_ex(ctx, cipher, nil, @TestKey[0], @TestIV[0]) <> 1 then
    begin
      WriteLn('✗ 加密初始化失败');
      Exit;
    end;
    
    // 准备输出缓冲区
    SetLength(ciphertext, Length(TestPlaintext) + 32);
    
    // 加密数据
    outlen := 0;
    if EVP_EncryptUpdate(ctx, @ciphertext[0], outlen, 
       PByte(TestPlaintext), Length(TestPlaintext)) <> 1 then
    begin
      WriteLn('✗ 加密数据失败');
      Exit;
    end;
    
    // 完成加密
    tmplen := 0;
    if EVP_EncryptFinal_ex(ctx, @ciphertext[outlen], tmplen) <> 1 then
    begin
      WriteLn('✗ 完成加密失败');
      Exit;
    end;
    
    outlen := outlen + tmplen;
    SetLength(ciphertext, outlen);
    
    // 获取认证标签
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16, @tag[0]) <> 1 then
    begin
      WriteLn('✗ 获取认证标签失败');
      Exit;
    end;
    
    WriteLn('  明文: ', TestPlaintext);
    WriteLn('  密文长度: ', outlen, ' 字节');
    WriteLn('  密文: ', BytesToHex(ciphertext, outlen));
    WriteLn('  标签: ', BytesToHex(tag, 16));
    WriteLn('  ✓ 加密成功');
    WriteLn;
    
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
  
  // === 解密 ===
  WriteLn('[2] 解密...');
  ctx := EVP_CIPHER_CTX_new();
  if not Assigned(ctx) then
  begin
    WriteLn('✗ 无法创建解密上下文');
    Exit;
  end;
  
  try
    // 初始化解密
    if EVP_DecryptInit_ex(ctx, cipher, nil, @TestKey[0], @TestIV[0]) <> 1 then
    begin
      WriteLn('✗ 解密初始化失败');
      Exit;
    end;
    
    // 准备输出缓冲区
    SetLength(plaintext, Length(ciphertext) + 16);
    
    // 解密数据
    outlen := 0;
    if EVP_DecryptUpdate(ctx, @plaintext[0], outlen,
       @ciphertext[0], Length(ciphertext)) <> 1 then
    begin
      WriteLn('✗ 解密数据失败');
      Exit;
    end;
    
    // 设置认证标签
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, 16, @tag[0]) <> 1 then
    begin
      WriteLn('✗ 设置认证标签失败');
      Exit;
    end;
    
    // 完成解密并验证
    tmplen := 0;
    if EVP_DecryptFinal_ex(ctx, @plaintext[outlen], tmplen) <> 1 then
    begin
      WriteLn('✗ 认证失败或解密错误');
      Exit;
    end;
    
    outlen := outlen + tmplen;
    SetLength(plaintext, outlen);
    
    // 验证结果
    if CompareMem(@plaintext[0], PByte(TestPlaintext), Length(TestPlaintext)) then
    begin
      WriteLn('  解密结果: ', PAnsiChar(@plaintext[0]));
      WriteLn('  ✓ 解密成功，认证通过');
    end
    else
      WriteLn('✗ 解密结果不匹配');
    
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
  WriteLn;
end;

procedure Example_SHA256;
const
  TestData: AnsiString = 'Hello, fafafa.ssl!';
var
  ctx: PEVP_MD_CTX;
  hash: array[0..31] of Byte;
  len: Cardinal;
begin
  WriteLn('=== SHA-256 哈希示例 ===');
  WriteLn;
  
  ctx := EVP_MD_CTX_new();
  if not Assigned(ctx) then
  begin
    WriteLn('✗ 无法创建哈希上下文');
    Exit;
  end;
  
  try
    if EVP_DigestInit_ex(ctx, EVP_sha256(), nil) <> 1 then
    begin
      WriteLn('✗ 哈希初始化失败');
      Exit;
    end;
    
    if EVP_DigestUpdate(ctx, PByte(TestData), Length(TestData)) <> 1 then
    begin
      WriteLn('✗ 哈希更新失败');
      Exit;
    end;
    
    if EVP_DigestFinal_ex(ctx, @hash[0], len) <> 1 then
    begin
      WriteLn('✗ 完成哈希失败');
      Exit;
    end;
    
    WriteLn('  原文: ', TestData);
    WriteLn('  SHA-256: ', BytesToHex(hash, 32));
    WriteLn('  ✓ 哈希计算成功');
    
  finally
    EVP_MD_CTX_free(ctx);
  end;
  WriteLn;
end;

begin
  WriteLn('==========================================');
  WriteLn('  fafafa.ssl 加密功能示例 (可工作版本)');
  WriteLn('==========================================');
  WriteLn;
  
  try
    // 加载OpenSSL (使用test_evp_cipher的方式)
    LoadOpenSSLCore();
    if not IsOpenSSLCoreLoaded then
    begin
      WriteLn('✗ OpenSSL加载失败');
      Halt(1);
    end;
    WriteLn('✓ OpenSSL加载成功: ', GetOpenSSLVersionString);
    
    // 加载EVP模块
    LoadEVP(GetCryptoLibHandle);
    WriteLn('✓ EVP模块加载成功');
    WriteLn;
    
    // 运行示例
    Example_AES_GCM;
    Example_SHA256;
    
    WriteLn('==========================================');
    WriteLn('✓ 所有示例完成！');
    WriteLn('==========================================');
    
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('按Enter退出...');
  ReadLn;
end.
