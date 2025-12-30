# fafafa.ssl 快速开始指南

欢迎使用 **fafafa.ssl** - 强大而易用的 Free Pascal SSL/TLS 库！

本指南将帮助您在5分钟内开始使用 fafafa.ssl 进行加密操作和HTTPS通信。

---

## 📋 目录

1. [安装要求](#安装要求)
2. [第一个程序](#第一个程序)
3. [常见用例](#常见用例)
4. [示例程序](#示例程序)
5. [故障排除](#故障排除)

---

## 安装要求

### 系统要求
- **Free Pascal Compiler** 3.2.0 或更高
- **OpenSSL** 1.1.1+ 或 3.0+ (libcrypto.so/libssl.so)
- Linux/Windows/macOS

### 检查OpenSSL安装
```bash
# Linux
ldconfig -p | grep libcrypto

# 或直接检查
ls /usr/lib/x86_64-linux-gnu/libcrypto.so*
```

### 编译项目
```bash
cd ~/projects/fafafa.ssl
fpc -Mobjfpc -Sh -Fu./src -Fi./src your_program.pas
```

---

## 第一个程序

### Hello, fafafa.ssl!

创建文件 `hello_ssl.pas`:

```pascal
program hello_ssl;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

var
  LHash: array[0..31] of Byte;
  LCtx: PEVP_MD_CTX;
  LLen: Cardinal;
  I: Integer;

begin
  // 1. 初始化OpenSSL
  LoadOpenSSLCore();
  LoadEVP(GetCryptoLibHandle);
  
  WriteLn('✓ OpenSSL版本: ', GetOpenSSLVersionString);
  
  // 2. 计算SHA-256哈希
  LCtx := EVP_MD_CTX_new();
  try
    EVP_DigestInit_ex(LCtx, EVP_sha256(), nil);
    EVP_DigestUpdate(LCtx, PAnsiChar('Hello, fafafa.ssl!'), 19);
    EVP_DigestFinal_ex(LCtx, @LHash[0], LLen);
    
    Write('SHA-256: ');
    for I := 0 to 31 do
      Write(IntToHex(LHash[I], 2));
    WriteLn;
  finally
    EVP_MD_CTX_free(LCtx);
  end;
  
  WriteLn('✓ 成功！');
end.
```

编译并运行：
```bash
fpc -Mobjfpc -Sh -Fu./src -Fi./src hello_ssl.pas
./hello_ssl
```

输出：
```
✓ OpenSSL版本: 3.x (libcrypto.so.3)
SHA-256: 22052DC71024F61595A40918D6D2986CE11210B8DC95569B2AC6038BD36C9611
✓ 成功！
```

---

## 常见用例

### 1. 数据加密/解密 (AES-256-GCM)

```pascal
program encrypt_data;

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand;

const
  KEY_SIZE = 32;  // 256 bits
  IV_SIZE = 12;   // 96 bits (GCM推荐)

var
  LKey, LIV: array of Byte;
  LPlaintext: AnsiString;
  LCiphertext: array[0..1023] of Byte;
  LTag: array[0..15] of Byte;
  LCtx: PEVP_CIPHER_CTX;
  LOutLen: Integer;

begin
  // 初始化
  LoadOpenSSLCore();
  LoadEVP(GetCryptoLibHandle);
  
  // 生成随机密钥和IV
  SetLength(LKey, KEY_SIZE);
  SetLength(LIV, IV_SIZE);
  RAND_bytes(@LKey[0], KEY_SIZE);
  RAND_bytes(@LIV[0], IV_SIZE);
  
  LPlaintext := '机密数据：账户余额 $1,000,000';
  
  // 加密
  LCtx := EVP_CIPHER_CTX_new();
  try
    EVP_EncryptInit_ex(LCtx, EVP_aes_256_gcm(), nil, @LKey[0], @LIV[0]);
    EVP_EncryptUpdate(LCtx, @LCiphertext[0], LOutLen, 
                      PByte(LPlaintext), Length(LPlaintext));
    EVP_EncryptFinal_ex(LCtx, nil, LOutLen);
    EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_GET_TAG, 16, @LTag[0]);
    
    WriteLn('✓ 加密成功');
    WriteLn('密文长度: ', LOutLen, ' 字节');
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
  
  // 解密（使用相同密钥和IV）...
end.
```

### 2. HTTPS GET 请求

```pascal
program https_get;
uses
  SysUtils,
  fafafa.ssl.http.simple;

var
  LResponse: string;

begin
  try
    LResponse := TSimpleHTTPSClient.Get('https://api.github.com');
    WriteLn('响应长度: ', Length(LResponse));
    WriteLn('前100字符: ', Copy(LResponse, 1, 100));
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end.
```

### 3. 快速证书生成 (NEW)

使用 `TSSLQuick` 可以在一秒内生成测试证书：

```pascal
program quick_cert;

uses
  SysUtils, fafafa.ssl.quick;

begin
  // 1. 生成自签名服务器证书 (包含 SAN)
  if TSSLQuick.GenerateSelfSigned('server.crt', 'server.key', 'localhost') then
    WriteLn('✓ 证书生成成功！');

  // 2. 生成 CA 证书
  if TSSLQuick.GenerateCACert('ca.crt', 'ca.key', 'My Root CA') then
    WriteLn('✓ CA 生成成功！');
    
  // 3. 签发服务器证书
  TSSLQuick.GenerateSignedCert('ca.crt', 'ca.key', 'web.crt', 'web.key', 
    'www.example.com', 'DNS:www.example.com');
end.
```

### 4. 文件哈希计算

```pascal
program file_hash;

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

function CalculateFileHash(const AFileName: string): string;
var
  LFile: TFileStream;
  LBuffer: array[0..8191] of Byte;
  LBytesRead: Integer;
  LCtx: PEVP_MD_CTX;
  LHash: array[0..31] of Byte;
  LLen: Cardinal;
  I: Integer;
begin
  Result := '';
  
  LoadOpenSSLCore();
  LoadEVP(GetCryptoLibHandle);
  
  LFile := TFileStream.Create(AFileName, fmOpenRead);
  try
    LCtx := EVP_MD_CTX_new();
    try
      EVP_DigestInit_ex(LCtx, EVP_sha256(), nil);
      
      repeat
        LBytesRead := LFile.Read(LBuffer, SizeOf(LBuffer));
        if LBytesRead > 0 then
          EVP_DigestUpdate(LCtx, @LBuffer[0], LBytesRead);
      until LBytesRead = 0;
      
      EVP_DigestFinal_ex(LCtx, @LHash[0], LLen);
      
      for I := 0 to 31 do
        Result := Result + LowerCase(IntToHex(LHash[I], 2));
        
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    LFile.Free;
  end;
end;

begin
  if ParamCount = 0 then
  begin
    WriteLn('用法: file_hash <文件名>');
    Halt(1);
  end;
  
  WriteLn('文件: ', ParamStr(1));
  WriteLn('SHA-256: ', CalculateFileHash(ParamStr(1)));
end.
```

---

## 示例程序

项目包含多个完整示例程序：

### 加密示例
- **`example_crypto_working.pas`** ⭐ - AES-GCM加密和SHA-256哈希
- **`example_crypto_simple.pas`** - 简化的加密演示

### 网络示例
- **`example_https_client.pas`** - HTTPS客户端
- **`example_http_download.pas`** - 文件下载
- **`example_api_call.pas`** - REST API调用

### 运行示例
```bash
cd ~/projects/fafafa.ssl

# 编译
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/example_crypto_working.pas -o./bin/example

# 运行
./bin/example
```

---

## 重要概念

### 正确的初始化顺序

⚠️ **关键**: 必须按正确顺序初始化OpenSSL模块

```pascal
// ✅ 正确
LoadOpenSSLCore();           // 1. 加载核心库
LoadEVP(GetCryptoLibHandle); // 2. 加载EVP模块（加密/哈希）
// 现在可以使用EVP_*函数

// ✗ 错误 - 会导致访问违规
LoadOpenSSLCore();
LCipher := EVP_aes_256_gcm();  // 崩溃！EVP未加载
```

### 资源管理

始终释放OpenSSL资源：
```pascal
LCtx := EVP_CIPHER_CTX_new();
try
  // 使用ctx...
finally
  EVP_CIPHER_CTX_free(LCtx);  // 必须释放
end;
```

### 错误检查

检查OpenSSL函数返回值：
```pascal
if EVP_EncryptInit_ex(LCtx, cipher, nil, @key[0], @iv[0]) <> 1 then
begin
  WriteLn('加密初始化失败');
  Exit;
end;
```

---

## 故障排除

### 问题1: "Can't find OpenSSL library"

**原因**: OpenSSL未安装或不在库路径中

**解决**:
```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev

# CentOS/RHEL
sudo yum install openssl-devel

# 或设置LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

### 问题2: "Access violation" 访问违规

**原因**: 未正确加载EVP模块

**解决**: 确保调用顺序正确
```pascal
LoadOpenSSLCore();
LoadEVP(GetCryptoLibHandle);  // 不要遗漏这行！
```

### 问题3: 编译错误 "Can't find unit fafafa.ssl.*"

**原因**: 编译时未指定源代码路径

**解决**:
```bash
fpc -Fu./src -Fi./src your_program.pas
#    ^^^^^^^^ ^^^^^^^^ 添加这些参数
```

### 问题4: Random number generation failed

**原因**: RAND模块未初始化

**解决**: RAND_bytes通常通过核心库可用，无需额外加载
```pascal
LoadOpenSSLCore();
// RAND_bytes现在应该可用
if RAND_bytes(@buffer[0], size) <> 1 then
  WriteLn('随机数生成失败');
```

---

## 下一步

### 深入学习
- 📖 查看 `examples/` 目录中的完整示例
- 📖 阅读 API 文档（即将推出）
- 📖 查看测试程序了解高级用法

### 常见任务
- [文件加密工具](examples/file_encrypt.pas)
- [HTTPS服务器](examples/https_server.pas)
- [证书生成](examples/cert_generate.pas)

### 获取帮助
- 查看项目 README.md
- 检查 `tests/` 目录中的测试用例
- 参考 OpenSSL官方文档

---

## 完整示例：加密文件工具

这是一个完整的命令行文件加密工具示例：

```pascal
program simple_encrypt;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand;

procedure EncryptFile(const AInputFile, AOutputFile, APassword: string);
var
  LInput, LOutput: TFileStream;
  LKey: array[0..31] of Byte;
  LIV: array[0..15] of Byte;
  LBuffer, LOutBuf: array[0..8191] of Byte;
  LBytesRead, LOutLen: Integer;
  LCtx: PEVP_CIPHER_CTX;
  I: Integer;
begin
  // 从密码派生密钥（简化版，实际应使用PBKDF2）
  FillChar(LKey, SizeOf(LKey), 0);
  Move(APassword[1], LKey[0], Min(Length(APassword), 32));
  
  // 生成随机IV
  RAND_bytes(@LIV[0], 16);
  
  LInput := TFileStream.Create(AInputFile, fmOpenRead);
  try
    LOutput := TFileStream.Create(AOutputFile, fmCreate);
    try
      // 将IV写入输出文件开头
      LOutput.Write(LIV[0], 16);
      
      LCtx := EVP_CIPHER_CTX_new();
      try
        EVP_EncryptInit_ex(LCtx, EVP_aes_256_cbc(), nil, @LKey[0], @LIV[0]);
        
        // 加密数据
        repeat
          LBytesRead := LInput.Read(LBuffer, SizeOf(LBuffer));
          if LBytesRead > 0 then
          begin
            EVP_EncryptUpdate(LCtx, @LOutBuf[0], LOutLen, @LBuffer[0], LBytesRead);
            LOutput.Write(LOutBuf[0], LOutLen);
          end;
        until LBytesRead = 0;
        
        // 完成加密（处理填充）
        EVP_EncryptFinal_ex(LCtx, @LOutBuf[0], LOutLen);
        LOutput.Write(LOutBuf[0], LOutLen);
        
      finally
        EVP_CIPHER_CTX_free(LCtx);
      end;
    finally
      LOutput.Free;
    end;
  finally
    LInput.Free;
  end;
  
  WriteLn('✓ 文件加密完成: ', AOutputFile);
end;

begin
  LoadOpenSSLCore();
  LoadEVP(GetCryptoLibHandle);
  
  if ParamCount < 3 then
  begin
    WriteLn('用法: simple_encrypt <输入文件> <输出文件> <密码>');
    Halt(1);
  end;
  
  try
    EncryptFile(ParamStr(1), ParamStr(2), ParamStr(3));
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

---

## 总结

您现在已经掌握了 fafafa.ssl 的基础知识！

✅ 您学会了：
- 如何初始化OpenSSL
- 如何进行加密/解密
- 如何计算哈希
- 如何进行HTTPS通信

🚀 下一步：探索 `examples/` 目录中的更多示例，构建您自己的安全应用！

---

**需要帮助？** 查看示例程序或测试用例以获取更多灵感。

Happy coding! 🎉
