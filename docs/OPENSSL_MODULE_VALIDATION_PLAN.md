# OpenSSL 模块验证计划

**日期**: 2025-10-01  
**目标**: 系统化验证所有 76 个 OpenSSL 模块的功能完整性

---

## 📊 当前测试覆盖概况

### 现有测试 (11 个)

| 测试文件 | 覆盖模块 | 状态 |
|---------|---------|------|
| `diagnose_openssl.pas` | 核心加载 | ✅ |
| `test_openssl_load.pas` | 动态加载 | ✅ |
| `test_crypto_basics.pas` | 基础加密 | ✅ |
| `diagnose_aead.pas` | AEAD 全模式 | ✅ |
| `test_aead_gcm.pas` | AES-GCM | ✅ |
| `test_aead_modes.pas` | 多 AEAD 模式 | ✅ |
| `test_aead_simple.pas` | AEAD 简化 | ✅ |
| `test_gcm_simple.pas` | GCM 简化 | ✅ |
| `test_cmac_evp.pas` | CMAC EVP | ✅ |
| `test_sha3_evp.pas` | SHA3 EVP | ✅ |
| `test_sha3_names.pas` | SHA3 命名 | ✅ |

### 已验证模块 (估计 15/64)

- ✅ **core** - SSL/TLS 核心
- ✅ **evp** - EVP 加密抽象 (包含 AEAD)
- ✅ **aead** - AEAD 专用
- ✅ **crypto** - 基础加密
- ✅ **aes** - AES 算法
- ✅ **chacha** - ChaCha20
- ✅ **cmac** - CMAC
- ✅ **sha** - SHA 系列
- ✅ **sha3** - SHA3 系列
- ✅ **modes** - 加密模式
- ⚠️ **err** - 错误处理 (部分)
- ⚠️ **rand** - 随机数 (部分)
- ⚠️ **bio** - I/O 抽象 (部分)
- ⚠️ **types** - 类型定义 (间接)
- ⚠️ **consts** - 常量定义 (间接)

**测试覆盖率**: 约 23% (15/64 个功能模块)

---

## 🎯 验证策略

### 优先级分类

#### P0 - 关键核心 (必须 100% 验证)

这些模块是库的基础，必须完全可靠：

1. **ssl** - SSL/TLS 核心功能
2. **bio** - I/O 抽象层
3. **err** - 错误处理
4. **crypto** - 加密基础 ✅
5. **rand** - 随机数生成
6. **evp** - 加密抽象 ✅

#### P1 - 高频使用 (优先验证)

用户最常使用的功能模块：

**对称加密**:
- **aes** ✅
- **des**
- **chacha** ✅

**非对称加密**:
- **rsa**
- **ec**
- **dh**
- **ecdsa**
- **ecdh**

**哈希算法**:
- **sha** ✅
- **sha3** ✅
- **md**

**证书管理**:
- **x509**
- **x509v3**
- **pem**
- **pkcs12**
- **pkcs7**

**消息认证码**:
- **hmac**
- **cmac** ✅

#### P2 - 常用功能 (重要验证)

**密钥派生**:
- **kdf**
- **scrypt_whirlpool**

**高级加密**:
- **cms**
- **pkcs**

**证书服务**:
- **ocsp**
- **ct**
- **ts**

**其他算法**:
- **aria**
- **blake2**
- **sm** (SM2/SM3/SM4)
- **seed**

#### P3 - 辅助功能 (基础验证)

**数据结构**:
- **asn1**
- **bn**
- **stack**
- **lhash**
- **buffer**

**系统功能**:
- **engine**
- **provider**
- **async**
- **thread**
- **dso**

**辅助工具**:
- **obj**
- **conf**
- **param**
- **store**
- **ui**
- **txt_db**
- **utils**

#### P4 - 遗留功能 (可选验证)

- **legacy_ciphers** - 旧式算法
- **rand_old** - 旧 RAND API
- **srp** - SRP 协议
- **comp** - 压缩 (已弃用)

---

## 📋 验证计划

### 阶段 1: P0 核心模块验证 (本阶段重点)

#### 1.1 SSL/TLS 核心 (ssl.pas)

**目标**: 验证 SSL/TLS 连接建立和数据传输

**测试内容**:
- [ ] SSL 上下文创建和配置
- [ ] TLS 1.2 客户端连接
- [ ] TLS 1.3 客户端连接
- [ ] TLS 服务器功能
- [ ] 证书验证
- [ ] SNI 支持
- [ ] ALPN 协商
- [ ] 会话恢复
- [ ] 错误处理

**预计工作量**: 2-3 天

#### 1.2 BIO 抽象层 (bio.pas)

**目标**: 验证各种 BIO 类型和操作

**测试内容**:
- [ ] 内存 BIO
- [ ] 文件 BIO
- [ ] Socket BIO
- [ ] SSL BIO
- [ ] BIO 链操作
- [ ] BIO 读写操作
- [ ] BIO 控制操作

**预计工作量**: 1 天

#### 1.3 错误处理 (err.pas)

**目标**: 验证错误报告和处理机制

**测试内容**:
- [ ] 错误码获取
- [ ] 错误字符串转换
- [ ] 错误队列操作
- [ ] 错误回调设置
- [ ] 多线程错误处理

**预计工作量**: 0.5 天

#### 1.4 随机数生成 (rand.pas)

**目标**: 验证随机数生成的正确性和安全性

**测试内容**:
- [ ] RAND_bytes 生成
- [ ] RAND_priv_bytes 生成
- [ ] 熵池状态
- [ ] 种子设置
- [ ] RAND 状态查询
- [ ] 统计随机性检查

**预计工作量**: 0.5 天

#### 1.5 EVP 高级 API (evp.pas) - 部分完成

**目标**: 全面验证 EVP 接口

**测试内容**:
- [x] EVP AEAD 模式 ✅
- [ ] EVP 对称加密 (各种算法)
- [ ] EVP 非对称加密
- [ ] EVP 签名验证
- [ ] EVP 密钥派生
- [ ] EVP MAC
- [ ] EVP 摘要

**预计工作量**: 2 天

---

### 阶段 2: P1 高频模块验证

#### 2.1 RSA 加密 (rsa.pas)

**测试内容**:
- [ ] RSA 密钥生成
- [ ] RSA 公钥加密/私钥解密
- [ ] RSA 签名/验证
- [ ] RSA PSS 填充
- [ ] RSA OAEP 填充
- [ ] 密钥导入/导出 (PEM/DER)

**预计工作量**: 1 天

#### 2.2 椭圆曲线 (ec.pas, ecdsa.pas, ecdh.pas)

**测试内容**:
- [ ] EC 密钥生成 (P-256, P-384, P-521)
- [ ] ECDSA 签名/验证
- [ ] ECDH 密钥交换
- [ ] 曲线参数查询
- [ ] EC 密钥导入/导出

**预计工作量**: 1.5 天

#### 2.3 X.509 证书 (x509.pas, x509v3.pas)

**测试内容**:
- [ ] 证书解析
- [ ] 证书创建
- [ ] 证书验证
- [ ] 证书链构建
- [ ] 扩展处理
- [ ] 证书吊销列表 (CRL)
- [ ] 证书请求 (CSR)

**预计工作量**: 2 天

#### 2.4 PEM/PKCS 格式 (pem.pas, pkcs7.pas, pkcs12.pas)

**测试内容**:
- [ ] PEM 读写 (密钥、证书)
- [ ] PKCS#7 签名验证
- [ ] PKCS#12 导入导出
- [ ] 密码保护

**预计工作量**: 1 天

#### 2.5 HMAC (hmac.pas)

**测试内容**:
- [ ] HMAC-SHA256
- [ ] HMAC-SHA512
- [ ] HMAC-MD5
- [ ] 密钥长度测试
- [ ] 已知向量验证

**预计工作量**: 0.5 天

#### 2.6 摘要算法 (md.pas)

**测试内容**:
- [ ] MD5 摘要
- [ ] SHA-1 摘要
- [ ] SHA-224/256/384/512
- [ ] 增量更新
- [ ] 已知向量验证

**预计工作量**: 0.5 天

---

### 阶段 3: P2 常用模块验证

#### 3.1 密钥派生 (kdf.pas)

**测试内容**:
- [ ] HKDF
- [ ] PBKDF2
- [ ] Scrypt
- [ ] 参数配置

**预计工作量**: 1 天

#### 3.2 CMS (cms.pas)

**测试内容**:
- [ ] CMS 签名
- [ ] CMS 加密
- [ ] CMS 签名验证
- [ ] CMS 解密

**预计工作量**: 1 天

#### 3.3 OCSP (ocsp.pas)

**测试内容**:
- [ ] OCSP 请求构建
- [ ] OCSP 响应解析
- [ ] OCSP 验证

**预计工作量**: 1 天

#### 3.4 国密算法 (sm.pas)

**测试内容**:
- [ ] SM2 签名验证
- [ ] SM3 摘要
- [ ] SM4 加密

**预计工作量**: 1 天

---

### 阶段 4: P3 辅助模块验证

#### 4.1 ASN.1 (asn1.pas)

**测试内容**:
- [ ] ASN.1 编码/解码
- [ ] 时间处理
- [ ] 字符串转换

**预计工作量**: 1 天

#### 4.2 大数运算 (bn.pas)

**测试内容**:
- [ ] 大数创建
- [ ] 算术运算
- [ ] 模运算
- [ ] 素数生成

**预计工作量**: 1 天

#### 4.3 引擎和提供者 (engine.pas, provider.pas)

**测试内容**:
- [ ] 引擎列举
- [ ] 引擎加载
- [ ] 提供者查询
- [ ] 算法查询

**预计工作量**: 1 天

---

## 🛠️ 验证工具和方法

### 验证工具开发

#### 1. 综合诊断工具 `diagnose_modules.pas`

**功能**:
- 自动检测所有 OpenSSL 模块的可用性
- 测试每个模块的关键函数
- 生成详细的兼容性报告

**输出示例**:
```
========================================
OpenSSL Module Diagnostic Report
========================================
OpenSSL Version: 3.4.1

Core Modules:
✅ SSL/TLS Core           - Available
✅ EVP                    - Available
✅ BIO                    - Available
✅ Crypto                 - Available
✅ Error Handling         - Available
✅ Random                 - Available

Symmetric Ciphers:
✅ AES                    - Available
✅ DES                    - Available
✅ ChaCha20               - Available
...

Asymmetric Ciphers:
✅ RSA                    - Available
✅ EC                     - Available
❌ DSA                    - Not Available (Deprecated)
...

Hash Functions:
✅ SHA-256                - Available
✅ SHA-3                  - Available
...

Total: 60/64 modules available (94%)
```

#### 2. 单元测试模板

每个模块的测试应包含：

```pascal
program test_module_name;

uses
  fafafa.ssl.openssl.module_name;

procedure TestBasicFunctionality;
begin
  // 基础功能测试
end;

procedure TestEdgeCases;
begin
  // 边界情况测试
end;

procedure TestErrorHandling;
begin
  // 错误处理测试
end;

procedure TestKnownVectors;
begin
  // 已知测试向量验证
end;

begin
  WriteLn('Testing: fafafa.ssl.openssl.module_name');
  WriteLn('=========================================');
  
  TestBasicFunctionality;
  TestEdgeCases;
  TestErrorHandling;
  TestKnownVectors;
  
  WriteLn('All tests passed!');
end.
```

#### 3. 自动化测试运行器 `run_all_tests.ps1`

**功能**:
- 编译所有测试程序
- 运行所有测试
- 收集结果
- 生成 HTML/Markdown 报告

---

## 📈 成功标准

### 模块验证通过标准

一个模块要标记为"已验证"，必须满足：

1. ✅ **编译通过** - 模块可以正常编译
2. ✅ **加载成功** - 所有函数可以从 OpenSSL 加载
3. ✅ **基础功能** - 核心功能正常工作
4. ✅ **错误处理** - 错误情况处理正确
5. ✅ **已知向量** - 通过标准测试向量 (如适用)
6. ✅ **文档完整** - 有使用说明和示例

### 整体验证目标

| 指标 | 当前 | 短期目标 | 长期目标 |
|------|------|---------|---------|
| P0 模块覆盖率 | 50% | 100% | 100% |
| P1 模块覆盖率 | 20% | 80% | 100% |
| P2 模块覆盖率 | 5% | 50% | 90% |
| P3 模块覆盖率 | 0% | 30% | 70% |
| **总体覆盖率** | **23%** | **65%** | **90%** |

---

## 📅 时间线

### 第一周: P0 核心模块 (重点)

- **Day 1-2**: SSL/TLS 核心验证
- **Day 3**: BIO 和错误处理
- **Day 4**: 随机数和 EVP 补充
- **Day 5**: 综合诊断工具开发

**预期输出**:
- 5 个核心模块 100% 验证
- 1 个综合诊断工具
- 验证报告

### 第二周: P1 高频模块

- **Day 1**: RSA 验证
- **Day 2**: EC/ECDSA/ECDH 验证
- **Day 3-4**: X.509 证书验证
- **Day 5**: PEM/PKCS 和 HMAC/MD 验证

**预期输出**:
- 10+ 模块验证完成
- 覆盖率提升至 50%

### 第三周: P2 常用模块

- **Day 1**: KDF 验证
- **Day 2**: CMS 验证
- **Day 3**: OCSP 验证
- **Day 4**: 国密算法验证
- **Day 5**: 测试报告整理

**预期输出**:
- 覆盖率提升至 65%
- 完整测试套件

### 第四周: P3 辅助模块和总结

- **Day 1-2**: ASN.1, BN 验证
- **Day 3**: Engine/Provider 验证
- **Day 4**: 自动化测试脚本
- **Day 5**: 最终报告和文档

**预期输出**:
- 覆盖率提升至 75%+
- 自动化测试流程
- 完整文档

---

## 🚀 立即开始

### 推荐第一步: 创建综合诊断工具

**原因**:
1. 快速了解整体状况
2. 识别不可用的模块
3. 为后续验证提供基准
4. 可重复使用

**文件**: `tests/diagnose_all_modules.pas`

**预期时间**: 2-3 小时

**输出**: 
- 完整的模块可用性报告
- 版本兼容性信息
- 问题模块列表

---

## 📝 报告和文档

### 验证报告结构

每完成一个模块验证后，更新：

1. **MODULE_VALIDATION_STATUS.md** - 验证状态跟踪
2. **TEST_RESULTS.md** - 测试结果汇总
3. **KNOWN_ISSUES.md** - 已知问题列表

### 示例验证记录

```markdown
## fafafa.ssl.openssl.rsa

**验证日期**: 2025-10-02  
**验证人员**: [Your Name]  
**OpenSSL 版本**: 3.4.1  
**状态**: ✅ 已验证

### 测试项目

- ✅ RSA 密钥生成 (1024/2048/4096 位)
- ✅ RSA 公钥加密
- ✅ RSA 私钥解密
- ✅ RSA 签名 (PKCS#1 v1.5)
- ✅ RSA 签名 (PSS)
- ✅ PEM 导入导出
- ✅ 错误处理

### 已知问题

- ⚠️ 512 位密钥在 OpenSSL 3.x 中不支持 (预期行为)

### 测试代码

见 `tests/test_rsa.pas`
```

---

## 🎯 下一步行动

1. **创建 `diagnose_all_modules.pas`** - 全模块诊断工具
2. **创建 `MODULE_VALIDATION_STATUS.md`** - 验证状态跟踪表
3. **开始 P0 模块验证** - SSL/TLS 核心

---

**创建时间**: 2025-10-01 01:07 UTC+8  
**最后更新**: 2025-10-01 01:07 UTC+8
