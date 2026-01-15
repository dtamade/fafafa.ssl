# AEAD 快速参考

## 快速开始

### 1. 验证 AEAD 可用性

```bash
# 运行诊断工具
cd tests
diagnose_aead.exe
```

### 2. 基本 AES-GCM 加密示例

```pascal
program aes_gcm_example;

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.consts;

var
  Ctx: PEVP_CIPHER_CTX;
  Cipher: PEVP_CIPHER;
  Key: array[0..31] of Byte;
  IV: array[0..11] of Byte;
  Plain: array[0..15] of Byte;
  Cipher: array[0..31] of Byte;
  Tag: array[0..15] of Byte;
  OutLen: Integer;
  KeyPtr, IVPtr, PlainPtr, CipherPtr, TagPtr: PByte;
  i: Integer;

begin
  // 初始化
  LoadOpenSSLCore;
  LoadEVP(GetCryptoLibHandle);
  
  // 准备测试数据
  for i := 0 to 31 do Key[i] := Byte(i);
  for i := 0 to 11 do IV[i] := Byte(i);
  for i := 0 to 15 do Plain[i] := Byte($AA);
  
  // 设置指针
  KeyPtr := @Key[0];
  IVPtr := @IV[0];
  PlainPtr := @Plain[0];
  CipherPtr := @Cipher[0];
  TagPtr := @Tag[0];
  
  // 获取 AES-256-GCM
  Cipher := EVP_aes_256_gcm();
  
  // 加密
  Ctx := EVP_CIPHER_CTX_new();
  try
    EVP_EncryptInit_ex(Ctx, Cipher, nil, nil, nil);
    EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_IVLEN, 12, nil);
    EVP_EncryptInit_ex(Ctx, nil, nil, KeyPtr, IVPtr);
    EVP_EncryptUpdate(Ctx, CipherPtr, @OutLen, PlainPtr, 16);
    EVP_EncryptFinal_ex(Ctx, CipherPtr + OutLen, @OutLen);
    EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_GET_TAG, 16, TagPtr);
    
    WriteLn('Success!');
  finally
    EVP_CIPHER_CTX_free(Ctx);
  end;
  
  // 清理
  UnloadEVP;
  UnloadOpenSSLCore;
end.
```

## 常用 AEAD 模式

### AES-GCM (最常用)

| 参数 | 值 |
|------|---|
| 密钥大小 | 16/24/32 bytes |
| IV 大小 | 12 bytes (推荐) |
| 标签大小 | 16 bytes |
| EVP 函数 | `EVP_aes_128/192/256_gcm()` |
| 控制命令 | `EVP_CTRL_GCM_SET_IVLEN`, `EVP_CTRL_GCM_GET_TAG`, `EVP_CTRL_GCM_SET_TAG` |

### ChaCha20-Poly1305 (高性能)

| 参数 | 值 |
|------|---|
| 密钥大小 | 32 bytes |
| Nonce 大小 | 12 bytes |
| 标签大小 | 16 bytes |
| EVP 函数 | `EVP_chacha20_poly1305()` |
| 控制命令 | `EVP_CTRL_AEAD_GET_TAG`, `EVP_CTRL_AEAD_SET_TAG` |

### AES-CCM (IoT)

| 参数 | 值 |
|------|---|
| 密钥大小 | 16/24/32 bytes |
| Nonce 大小 | 7-13 bytes |
| 标签大小 | 4-16 bytes (可配置) |
| EVP 函数 | `EVP_aes_128/192/256_ccm()` |
| 控制命令 | `EVP_CTRL_CCM_SET_IVLEN`, `EVP_CTRL_CCM_SET_TAG`, `EVP_CTRL_CCM_GET_TAG` |

### AES-XTS (磁盘加密)

| 参数 | 值 |
|------|---|
| 密钥大小 | 32/64 bytes (双密钥) |
| 调整值 | 16 bytes |
| 标签大小 | 无 (不提供认证) |
| EVP 函数 | `EVP_aes_128/256_xts()` |

## 常用代码片段

### 加密数据

```pascal
EVP_EncryptInit_ex(Ctx, Cipher, nil, nil, nil);
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_IVLEN, IVLen, nil);
EVP_EncryptInit_ex(Ctx, nil, nil, KeyPtr, IVPtr);
// 可选: 设置 AAD
EVP_EncryptUpdate(Ctx, nil, @OutLen, AADPtr, AADLen);
// 加密数据
EVP_EncryptUpdate(Ctx, OutPtr, @OutLen, InPtr, InLen);
EVP_EncryptFinal_ex(Ctx, OutPtr + OutLen, @OutLen);
// 获取标签
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_GET_TAG, TagLen, TagPtr);
```

### 解密数据

```pascal
EVP_DecryptInit_ex(Ctx, Cipher, nil, nil, nil);
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_IVLEN, IVLen, nil);
EVP_DecryptInit_ex(Ctx, nil, nil, KeyPtr, IVPtr);
// 可选: 设置 AAD
EVP_DecryptUpdate(Ctx, nil, @OutLen, AADPtr, AADLen);
// 解密数据
EVP_DecryptUpdate(Ctx, OutPtr, @OutLen, InPtr, InLen);
// 设置标签
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_TAG, TagLen, TagPtr);
// 验证标签并完成
Result := EVP_DecryptFinal_ex(Ctx, OutPtr + OutLen, @OutLen);
if Result <> 1 then
  WriteLn('Authentication failed!');
```

## 重要常量

```pascal
// GCM 控制
EVP_CTRL_GCM_SET_IVLEN = 9;
EVP_CTRL_GCM_GET_TAG = 16;
EVP_CTRL_GCM_SET_TAG = 17;

// CCM 控制
EVP_CTRL_CCM_SET_IVLEN = 9;
EVP_CTRL_CCM_SET_TAG = 16;
EVP_CTRL_CCM_GET_TAG = 17;

// 通用 AEAD
EVP_CTRL_AEAD_GET_TAG = 16;
EVP_CTRL_AEAD_SET_TAG = 17;
EVP_CTRL_AEAD_SET_IVLEN = 9;
```

## 类型系统技巧

### ✅ 正确的方式

```pascal
var
  Data: array[0..15] of Byte;
  DataPtr: PByte;
  DataLen: Integer;
begin
  DataPtr := @Data[0];
  DataLen := Length(Data);
  EVP_EncryptUpdate(Ctx, OutPtr, @OutLen, DataPtr, DataLen);
end;
```

### ❌ 错误的方式

```pascal
var
  Data: TBytes;
begin
  SetLength(Data, 16);
  // 这会导致类型错误！
  EVP_EncryptUpdate(Ctx, OutPtr, @OutLen, @Data[0], Length(Data));
end;
```

## 常见错误及解决

### 错误 1: "Incompatible types: got Pointer expected LongInt"

**原因**: FPC 类型检查严格  
**解决**: 使用指针变量

```pascal
// 错误
EVP_EncryptUpdate(Ctx, @Out[0], @Len, @In[0], Length(In));

// 正确
var
  InPtr, OutPtr: PByte;
begin
  InPtr := @In[0];
  OutPtr := @Out[0];
  EVP_EncryptUpdate(Ctx, OutPtr, @Len, InPtr, Integer(Length(In)));
end;
```

### 错误 2: Authentication failed

**原因**: 标签设置顺序错误  
**解决**: 在 DecryptFinal 之前设置标签

```pascal
// 正确的顺序
EVP_DecryptUpdate(Ctx, OutPtr, @Len, InPtr, InLen);
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_TAG, 16, TagPtr);  // 先设置标签
EVP_DecryptFinal_ex(Ctx, OutPtr + Len, @Len);  // 然后验证
```

### 错误 3: IV length错误

**原因**: 未设置或设置错误的 IV 长度  
**解决**: 在设置密钥前设置 IV 长度

```pascal
EVP_EncryptInit_ex(Ctx, Cipher, nil, nil, nil);
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_IVLEN, 12, nil);  // 必须在此设置
EVP_EncryptInit_ex(Ctx, nil, nil, KeyPtr, IVPtr);
```

## 性能提示

1. **重用上下文**: 使用 `EVP_CIPHER_CTX_reset` 重置而不是创建新的
2. **批量处理**: 使用多次 `EVP_EncryptUpdate` 处理大数据
3. **硬件加速**: 确保 OpenSSL 启用了 AES-NI
4. **选择合适算法**: 
   - 硬件 AES 支持 → AES-GCM
   - 纯软件 → ChaCha20-Poly1305
   - 受限设备 → AES-CCM

## 安全提示

⚠️ **永远不要重用 IV/Nonce！**  
⚠️ **始终验证认证标签！**  
⚠️ **使用加密随机数生成 IV**  
⚠️ **不要篡改密文或AAD**  

## 相关资源

- 完整文档: `docs/AEAD_SUPPORT.md`
- 实现总结: `docs/AEAD_IMPLEMENTATION_SUMMARY.md`
- 诊断工具: `tests/diagnose_aead.exe`
- MODES 模块: `src/fafafa.ssl.openssl.modes.pas`

---

**快速检查清单**:
- [ ] OpenSSL 3.x 或 1.1.x 已安装
- [ ] 运行 `diagnose_aead.exe` 确认可用性
- [ ] 使用固定数组或指针变量
- [ ] IV/Nonce 每次加密都不同
- [ ] 解密时验证认证标签
