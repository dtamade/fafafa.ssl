# 第二轮修复计划

## 当前状态
- 编译成功: 22/77 (28%)
- 剩余失败: 55个测试
  - WinSSL平台限制: ~15个（跳过）
  - 可修复的: ~40个

## 错误分类

### 类型1: 接口方法缺失
**影响测试**: test_openssl_features, test_integration_tls_end_to_end
**缺失方法**:
- `ISSLContext.GetVersion`
- `ISSLLibrary.GetLibrary` (或类似的获取库实例方法)

### 类型2: 辅助函数缺失
**影响测试**: test_error_handling_comprehensive
**缺失函数**:
- `GetFriendlyErrorMessage`
- `ClassifyOpenSSLError`
- `GetOpenSSLErrorCategory`

### 类型3: 常量/枚举缺失
**影响测试**: test_integration_tls_end_to_end
**缺失项**:
- `sslContextServer`
- `sslContextClient`

### 类型4: API函数缺失（高频）
**影响测试**: diagnose_all_modules 及多个 enterprise 测试
**缺失API**:
- CMAC_CTX_new/update/final
- PKCS5_PBKDF2_HMAC
- EVP_PBE_scrypt
- X509_new/verify
- BN_new/add/mul
- ASN1_STRING_new/INTEGER_new
- PEM_read_bio_X509/write_bio_X509
- PKCS12_create/parse

## 修复策略

### 优先级1: 快速修复（预计+5~8个测试）
1. 添加接口方法 (GetVersion, GetLibrary)
2. 添加缺失常量 (sslContextServer/Client)
3. 修复类型引用错误

### 优先级2: 中等修复（预计+5~10个测试）
1. 添加错误处理辅助函数
2. 添加常用的 X509 API
3. 添加常用的 BN API

### 优先级3: 长期修复（预计+10~15个测试）
1. 完善 CMAC 模块
2. 完善 PKCS5/PBKDF2 模块
3. 完善 PEM 读写函数
4. 完善 PKCS12 剩余函数

## 执行计划
