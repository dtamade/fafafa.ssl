program file_encrypt_tool;

{$mode objfpc}{$H+}

{
  简单文件加密工具
  
  用法:
    file_encrypt_tool encrypt <输入文件> <输出文件> <密码>
    file_encrypt_tool decrypt <输入文件> <输出文件> <密码>
}

uses
  SysUtils, Classes, Math,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand;

const
  BUFFER_SIZE = 8192;
  KEY_SIZE = 32;
  IV_SIZE = 16;

procedure DeriveKey(const APassword: string; var AKey: array of Byte);
var
  I: Integer;
begin
  // 简化的密钥派生（实际应使用PBKDF2）
  FillChar(AKey, KEY_SIZE, 0);
  for I := 0 to Min(Length(APassword) - 1, KEY_SIZE - 1) do
    AKey[I] := Byte(APassword[I + 1]);
end;

procedure EncryptFile(const AInputFile, AOutputFile, APassword: string);
var
  LInput, LOutput: TFileStream;
  LKey: array[0..KEY_SIZE-1] of Byte;
  LIV: array[0..IV_SIZE-1] of Byte;
  LInBuf, LOutBuf: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead, LOutLen: Integer;
  LCtx: PEVP_CIPHER_CTX;
begin
  WriteLn('加密文件: ', AInputFile);
  
  // 派生密钥
  DeriveKey(APassword, LKey);
  
  // 生成随机IV
  if RAND_bytes(@LIV[0], IV_SIZE) <> 1 then
    raise Exception.Create('生成随机IV失败');
  
  LInput := TFileStream.Create(AInputFile, fmOpenRead or fmShareDenyWrite);
  try
    LOutput := TFileStream.Create(AOutputFile, fmCreate);
    try
      // 写入IV到文件开头
      LOutput.WriteBuffer(LIV[0], IV_SIZE);
      
      // 初始化加密
      LCtx := EVP_CIPHER_CTX_new();
      if LCtx = nil then
        raise Exception.Create('无法创建加密上下文');
        
      try
        if EVP_EncryptInit_ex(LCtx, EVP_aes_256_cbc(), nil, @LKey[0], @LIV[0]) <> 1 then
          raise Exception.Create('加密初始化失败');
        
        // 加密数据
        repeat
          LBytesRead := LInput.Read(LInBuf, BUFFER_SIZE);
          if LBytesRead > 0 then
          begin
            if EVP_EncryptUpdate(LCtx, @LOutBuf[0], LOutLen, @LInBuf[0], LBytesRead) <> 1 then
              raise Exception.Create('加密数据失败');
            LOutput.WriteBuffer(LOutBuf[0], LOutLen);
          end;
        until LBytesRead = 0;
        
        // 完成加密
        if EVP_EncryptFinal_ex(LCtx, @LOutBuf[0], LOutLen) <> 1 then
          raise Exception.Create('完成加密失败');
        if LOutLen > 0 then
          LOutput.WriteBuffer(LOutBuf[0], LOutLen);
          
      finally
        EVP_CIPHER_CTX_free(LCtx);
      end;
      
      WriteLn('✓ 加密完成');
      WriteLn('输出: ', AOutputFile);
      WriteLn('大小: ', LOutput.Size, ' 字节');
      
    finally
      LOutput.Free;
    end;
  finally
    LInput.Free;
  end;
end;

procedure DecryptFile(const AInputFile, AOutputFile, APassword: string);
var
  LInput, LOutput: TFileStream;
  LKey: array[0..KEY_SIZE-1] of Byte;
  LIV: array[0..IV_SIZE-1] of Byte;
  LInBuf, LOutBuf: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead, LOutLen: Integer;
  LCtx: PEVP_CIPHER_CTX;
begin
  WriteLn('解密文件: ', AInputFile);
  
  // 派生密钥
  DeriveKey(APassword, LKey);
  
  LInput := TFileStream.Create(AInputFile, fmOpenRead or fmShareDenyWrite);
  try
    // 读取IV
    if LInput.Size < IV_SIZE then
      raise Exception.Create('文件太小，不是有效的加密文件');
    LInput.ReadBuffer(LIV[0], IV_SIZE);
    
    LOutput := TFileStream.Create(AOutputFile, fmCreate);
    try
      // 初始化解密
      LCtx := EVP_CIPHER_CTX_new();
      if LCtx = nil then
        raise Exception.Create('无法创建解密上下文');
        
      try
        if EVP_DecryptInit_ex(LCtx, EVP_aes_256_cbc(), nil, @LKey[0], @LIV[0]) <> 1 then
          raise Exception.Create('解密初始化失败');
        
        // 解密数据
        repeat
          LBytesRead := LInput.Read(LInBuf, BUFFER_SIZE);
          if LBytesRead > 0 then
          begin
            if EVP_DecryptUpdate(LCtx, @LOutBuf[0], LOutLen, @LInBuf[0], LBytesRead) <> 1 then
              raise Exception.Create('解密数据失败');
            if LOutLen > 0 then
              LOutput.WriteBuffer(LOutBuf[0], LOutLen);
          end;
        until LBytesRead = 0;
        
        // 完成解密
        if EVP_DecryptFinal_ex(LCtx, @LOutBuf[0], LOutLen) <> 1 then
          raise Exception.Create('完成解密失败或密码错误');
        if LOutLen > 0 then
          LOutput.WriteBuffer(LOutBuf[0], LOutLen);
          
      finally
        EVP_CIPHER_CTX_free(LCtx);
      end;
      
      WriteLn('✓ 解密完成');
      WriteLn('输出: ', AOutputFile);
      WriteLn('大小: ', LOutput.Size, ' 字节');
      
    finally
      LOutput.Free;
    end;
  finally
    LInput.Free;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('文件加密工具 - fafafa.ssl');
  WriteLn;
  WriteLn('用法:');
  WriteLn('  file_encrypt_tool encrypt <输入文件> <输出文件> <密码>');
  WriteLn('  file_encrypt_tool decrypt <输入文件> <输出文件> <密码>');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  file_encrypt_tool encrypt secret.txt secret.enc mypassword');
  WriteLn('  file_encrypt_tool decrypt secret.enc secret.txt mypassword');
  WriteLn;
  WriteLn('注意: 使用AES-256-CBC加密');
end;

var
  LMode, LInputFile, LOutputFile, LPassword: string;

begin
  WriteLn('==========================================');
  WriteLn('  文件加密工具');
  WriteLn('==========================================');
  WriteLn;
  
  // 检查参数
  if ParamCount < 4 then
  begin
    ShowUsage;
    Halt(1);
  end;
  
  LMode := LowerCase(ParamStr(1));
  LInputFile := ParamStr(2);
  LOutputFile := ParamStr(3);
  LPassword := ParamStr(4);
  
  // 验证输入文件
  if not FileExists(LInputFile) then
  begin
    WriteLn('错误: 输入文件不存在: ', LInputFile);
    Halt(1);
  end;
  
  try
    // 初始化OpenSSL
    LoadOpenSSLCore();
    if not IsOpenSSLCoreLoaded then
    begin
      WriteLn('错误: 无法加载OpenSSL');
      Halt(1);
    end;
    LoadEVP(GetCryptoLibHandle);
    
    // 执行操作
    if LMode = 'encrypt' then
      EncryptFile(LInputFile, LOutputFile, LPassword)
    else if LMode = 'decrypt' then
      DecryptFile(LInputFile, LOutputFile, LPassword)
    else
    begin
      WriteLn('错误: 无效的模式: ', LMode);
      WriteLn('必须是 "encrypt" 或 "decrypt"');
      Halt(1);
    end;
    
    WriteLn;
    WriteLn('==========================================');
    WriteLn('✓ 操作成功完成');
    WriteLn('==========================================');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('==========================================');
      WriteLn('✗ 错误: ', E.Message);
      WriteLn('==========================================');
      Halt(1);
    end;
  end;
end.
