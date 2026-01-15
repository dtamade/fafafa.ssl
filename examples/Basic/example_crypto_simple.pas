program example_crypto_simple;

{$mode objfpc}{$H+}

{
  简单加密示例
  
  演示如何使用 fafafa.ssl 进行基本的加密操作
}

uses
  SysUtils,
  fafafa.ssl.init,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand;

procedure Example_AES_GCM;
var
  LPlaintext: string;
  LKey, LIV: array[0..31] of Byte;
  LData: array[0..255] of Byte;
  LCiphertext: array[0..512] of Byte;
  LDecrypted: array[0..255] of Byte;
  LTag: array[0..15] of Byte;
  LCtx: PEVP_CIPHER_CTX;
  LOutLen, LTotalLen: Integer;
  I: Integer;
begin
  WriteLn('=== AES-256-GCM 加密示例 ===');
  WriteLn;
  
  // 准备数据
  LPlaintext := 'Hello, fafafa.ssl! 这是一条机密消息。';
  Move(LPlaintext[1], LData[0], Length(LPlaintext));
  
  // 生成随机密钥和IV
  FillChar(LKey, SizeOf(LKey), $AA);  // 实际应用中使用 RAND_bytes
  FillChar(LIV, SizeOf(LIV), $BB);
  RAND_bytes(@LKey[0], 32);
  RAND_bytes(@LIV[0], 12);
  
  WriteLn('[1] 加密数据...');
  LCtx := EVP_CIPHER_CTX_new();
  try
    // 初始化加密
    EVP_EncryptInit_ex(LCtx, EVP_aes_256_gcm(), nil, @LKey[0], @LIV[0]);
    
    // 加密
    EVP_EncryptUpdate(LCtx, @LCiphertext[0], LOutLen, @LData[0], Length(LPlaintext));
    LTotalLen := LOutLen;
    
    // 完成
    EVP_EncryptFinal_ex(LCtx, @LCiphertext[LTotalLen], LOutLen);
    LTotalLen := LTotalLen + LOutLen;
    
    // 获取认证标签
    EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_GET_TAG, 16, @LTag[0]);
    
    WriteLn('  密文长度: ', LTotalLen, ' 字节');
    Write('  密文 (hex): ');
    for I := 0 to LTotalLen - 1 do
      Write(IntToHex(LCiphertext[I], 2));
    WriteLn;
    WriteLn('  ✓ 加密成功');
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
  WriteLn;
  
  WriteLn('[2] 解密数据...');
  LCtx := EVP_CIPHER_CTX_new();
  try
    // 初始化解密
    EVP_DecryptInit_ex(LCtx, EVP_aes_256_gcm(), nil, @LKey[0], @LIV[0]);
    
    // 解密
    EVP_DecryptUpdate(LCtx, @LDecrypted[0], LOutLen, @LCiphertext[0], LTotalLen);
    
    // 设置标签
    EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_TAG, 16, @LTag[0]);
    
    // 验证并完成
    if EVP_DecryptFinal_ex(LCtx, nil, LOutLen) = 1 then
    begin
      SetString(LPlaintext, PAnsiChar(@LDecrypted[0]), LOutLen);
      WriteLn('  明文: ', LPlaintext);
      WriteLn('  ✓ 解密成功，认证通过');
    end
    else
      WriteLn('  ✗ 认证失败');
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
  WriteLn;
end;

procedure Example_SHA256;
var
  LData: string;
  LHash: array[0..31] of Byte;
  LCtx: PEVP_MD_CTX;
  LLen: Cardinal;
  I: Integer;
begin
  WriteLn('=== SHA-256 哈希示例 ===');
  WriteLn;
  
  LData := 'Hello, World!';
  WriteLn('原文: ', LData);
  WriteLn;
  
  LCtx := EVP_MD_CTX_new();
  try
    EVP_DigestInit_ex(LCtx, EVP_sha256(), nil);
    EVP_DigestUpdate(LCtx, @LData[1], Length(LData));
    EVP_DigestFinal_ex(LCtx, @LHash[0], LLen);
    
    Write('SHA-256: ');
    for I := 0 to 31 do
      Write(IntToHex(LHash[I], 2));
    WriteLn;
    WriteLn('✓ 哈希计算成功');
  finally
    EVP_MD_CTX_free(LCtx);
  end;
  WriteLn;
end;

procedure Example_Random;
var
  LRandom: array[0..15] of Byte;
  I: Integer;
begin
  WriteLn('=== 安全随机数示例 ===');
  WriteLn;
  
  if RAND_bytes(@LRandom[0], 16) = 1 then
  begin
    Write('随机数 (16字节): ');
    for I := 0 to 15 do
      Write(IntToHex(LRandom[I], 2));
    WriteLn;
    WriteLn('✓ 随机数生成成功');
  end
  else
    WriteLn('✗ 随机数生成失败');
  WriteLn;
end;

begin
  WriteLn('==========================================');
  WriteLn('  fafafa.ssl 加密功能示例');
  WriteLn('==========================================');
  WriteLn;
  
  try
    // 初始化OpenSSL
    WriteLn('初始化 OpenSSL...');
    InitializeOpenSSL;
    WriteLn('✓ OpenSSL版本: ', GetOpenSSLVersion);
    WriteLn;
    
    // 运行示例
    Example_AES_GCM;
    Example_SHA256;
    Example_Random;
    
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
