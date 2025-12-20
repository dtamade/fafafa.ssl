program ExampleAESGCMAEAD;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

const
  // 测试数据
  PLAINTEXT = 'Hello, this is a secret message!';
  AAD_DATA = 'Additional authenticated data';

var
  // 密钥和IV（实际使用中应该随机生成）
  Key: array[0..31] of Byte;    // 256-bit key for AES-256
  IV: array[0..11] of Byte;      // 96-bit IV (recommended for GCM)
  
  // 数据缓冲区
  Plaintext: AnsiString;
  AAD: AnsiString;
  Ciphertext: array[0..1023] of Byte;
  Tag: array[0..15] of Byte;     // GCM authentication tag
  Decrypted: array[0..1023] of Byte;
  
  // EVP上下文
  Ctx: PEVP_CIPHER_CTX;
  Cipher: PEVP_CIPHER;
  
  // 长度变量
  OutLen, DecryptLen: Integer;
  I: Integer;

procedure InitializeKeyAndIV;
var
  i: Integer;
begin
  // 在实际应用中，应该使用 RAND_bytes 生成随机密钥和IV
  // 这里为了演示使用固定值
  for i := 0 to High(Key) do
    Key[i] := Byte(i);
    
  for i := 0 to High(IV) do
    IV[i] := Byte(i);
    
  WriteLn('密钥和IV已初始化');
end;

function BytesToHex(const Bytes: array of Byte; Len: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Len - 1 do
    Result := Result + IntToHex(Bytes[i], 2);
end;

procedure EncryptWithAESGCM;
begin
  WriteLn;
  WriteLn('=== AES-256-GCM 加密 ===');
  
  // 1. 获取AES-256-GCM密码算法
  Cipher := EVP_CIPHER_fetch(nil, 'AES-256-GCM', nil);
  if Cipher = nil then
  begin
    WriteLn('错误：无法获取 AES-256-GCM 算法');
    Exit;
  end;
  WriteLn('✓ 成功获取 AES-256-GCM 算法');
  
  // 2. 创建并初始化加密上下文
  Ctx := EVP_CIPHER_CTX_new;
  if Ctx = nil then
  begin
    WriteLn('错误：无法创建加密上下文');
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 成功创建加密上下文');
  
  // 3. 初始化加密操作
  if EVP_EncryptInit_ex(Ctx, Cipher, nil, @Key[0], @IV[0]) <> 1 then
  begin
    WriteLn('错误：加密初始化失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 加密操作已初始化');
  
  // 4. 添加额外认证数据（AAD）- 这些数据会被认证但不会被加密
  OutLen := 0;
  if EVP_EncryptUpdate(Ctx, nil, @OutLen, PByte(AAD), Length(AAD)) <> 1 then
  begin
    WriteLn('错误：添加AAD失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ AAD已添加: ', AAD);
  
  // 5. 加密明文
  if EVP_EncryptUpdate(Ctx, @Ciphertext[0], @OutLen, PByte(Plaintext), Length(Plaintext)) <> 1 then
  begin
    WriteLn('错误：加密数据失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 数据已加密，密文长度: ', OutLen, ' 字节');
  
  // 6. 完成加密操作
  if EVP_EncryptFinal_ex(Ctx, @Ciphertext[OutLen], @DecryptLen) <> 1 then
  begin
    WriteLn('错误：完成加密失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  OutLen := OutLen + DecryptLen;
  WriteLn('✓ 加密完成，总密文长度: ', OutLen, ' 字节');
  
  // 7. 获取认证标签（GCM Tag）
  if EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_GET_TAG, 16, @Tag[0]) <> 1 then
  begin
    WriteLn('错误：获取认证标签失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 认证标签: ', BytesToHex(Tag, 16));
  
  // 显示密文
  WriteLn('✓ 密文 (hex): ', BytesToHex(Ciphertext, OutLen));
  
  // 清理
  EVP_CIPHER_CTX_free(Ctx);
  EVP_CIPHER_free(Cipher);
end;

procedure DecryptWithAESGCM;
var
  TempLen: Integer;
begin
  WriteLn;
  WriteLn('=== AES-256-GCM 解密 ===');
  
  // 1. 获取AES-256-GCM密码算法
  Cipher := EVP_CIPHER_fetch(nil, 'AES-256-GCM', nil);
  if Cipher = nil then
  begin
    WriteLn('错误：无法获取 AES-256-GCM 算法');
    Exit;
  end;
  WriteLn('✓ 成功获取 AES-256-GCM 算法');
  
  // 2. 创建并初始化解密上下文
  Ctx := EVP_CIPHER_CTX_new;
  if Ctx = nil then
  begin
    WriteLn('错误：无法创建解密上下文');
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 成功创建解密上下文');
  
  // 3. 初始化解密操作
  if EVP_DecryptInit_ex(Ctx, Cipher, nil, @Key[0], @IV[0]) <> 1 then
  begin
    WriteLn('错误：解密初始化失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 解密操作已初始化');
  
  // 4. 添加额外认证数据（AAD）- 必须与加密时相同
  TempLen := 0;
  if EVP_DecryptUpdate(Ctx, nil, @TempLen, PByte(AAD), Length(AAD)) <> 1 then
  begin
    WriteLn('错误：添加AAD失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ AAD已添加');
  
  // 5. 解密密文
  if EVP_DecryptUpdate(Ctx, @Decrypted[0], @DecryptLen, @Ciphertext[0], OutLen) <> 1 then
  begin
    WriteLn('错误：解密数据失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 数据已解密，明文长度: ', DecryptLen, ' 字节');
  
  // 6. 设置期望的认证标签
  if EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_TAG, 16, @Tag[0]) <> 1 then
  begin
    WriteLn('错误：设置认证标签失败');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  WriteLn('✓ 认证标签已设置');
  
  // 7. 完成解密并验证标签
  if EVP_DecryptFinal_ex(Ctx, @Decrypted[DecryptLen], @TempLen) <> 1 then
  begin
    WriteLn('❌ 错误：认证失败！数据可能被篡改！');
    EVP_CIPHER_CTX_free(Ctx);
    EVP_CIPHER_free(Cipher);
    Exit;
  end;
  DecryptLen := DecryptLen + TempLen;
  WriteLn('✓ 解密完成并通过认证验证');
  
  // 显示解密后的明文
  SetLength(Plaintext, DecryptLen);
  Move(Decrypted[0], Plaintext[1], DecryptLen);
  WriteLn('✓ 解密后的明文: ', Plaintext);
  
  // 清理
  EVP_CIPHER_CTX_free(Ctx);
  EVP_CIPHER_free(Cipher);
end;

begin
  WriteLn('========================================');
  WriteLn('  AES-256-GCM AEAD 加密示例');
  WriteLn('========================================');
  WriteLn;
  
  // 初始化OpenSSL
  LoadOpenSSLCore;
  
  if not IsOpenSSLCoreLoaded then
  begin
    WriteLn('错误：无法加载 OpenSSL 库');
    Halt(1);
  end;
  
  WriteLn('OpenSSL 版本: ', GetOpenSSLVersionString);
  
  // 加载EVP模块
  LoadEVP(GetCryptoLibHandle);
  WriteLn('EVP 模块已加载');
  WriteLn;
  
  // 准备数据
  Plaintext := PLAINTEXT;
  AAD := AAD_DATA;
  InitializeKeyAndIV;
  
  WriteLn('明文: ', Plaintext);
  WriteLn('明文长度: ', Length(Plaintext), ' 字节');
  
  // 执行加密
  EncryptWithAESGCM;
  
  // 执行解密
  DecryptWithAESGCM;
  
  WriteLn;
  WriteLn('========================================');
  WriteLn('  演示完成！');
  WriteLn('========================================');
  WriteLn;
  WriteLn('关键要点：');
  WriteLn('1. GCM 提供认证加密（AEAD），同时保证机密性和完整性');
  WriteLn('2. AAD 数据被认证但不加密，适合存储元数据');
  WriteLn('3. GCM Tag 用于验证数据完整性，防止篡改');
  WriteLn('4. IV 不应重复使用，推荐96-bit长度');
  WriteLn('5. 解密时必须验证Tag，验证失败说明数据被篡改');
  
  ReadLn;
end.
