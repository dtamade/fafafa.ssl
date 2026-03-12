# OpenSSL Pascal 绑定模块清单

## 项目概述
fafafa.ssl 是一个完整的 OpenSSL Pascal 绑定库，提供了对 OpenSSL 加密库的全面封装。

## 模块组织结构
所有模块统一存放在 `src` 目录下，采用扁平化目录结构。规范 OpenSSL API 单元名为 `fafafa.ssl.openssl.api.*.pas`；历史 `fafafa.ssl.openssl.*` 名称在活跃代码中仍通过兼容 shim 保持可编译。

## 已完成模块列表

### 核心模块
- [x] **fafafa.ssl.openssl.api.types.pas** - 基础类型定义
- [x] **fafafa.ssl.openssl.api.consts.pas** - 常量定义
- [x] **fafafa.ssl.openssl.api.core.pas** - 核心功能
- [x] **fafafa.ssl.openssl.api.pas** - API 加载管理
- [x] **fafafa.ssl.openssl.api.crypto.pas** - 加密原语

### 错误处理与随机数
- [x] **fafafa.ssl.openssl.api.err.pas** - 错误处理
- [x] **fafafa.ssl.openssl.api.rand.pas** - 随机数生成

### 大数运算
- [x] **fafafa.ssl.openssl.api.bn.pas** - BIGNUM 大数运算

### 哈希算法
- [x] **fafafa.ssl.openssl.api.sha.pas** - SHA 系列哈希 (SHA-1/224/256/384/512)
- [x] **fafafa.ssl.openssl.api.md.pas** - MD 系列哈希 (MD4/MD5/MDC2/RIPEMD160)
- [x] **fafafa.ssl.openssl.api.sm.pas** - 国密 SM3 哈希

### 对称加密
- [x] **fafafa.ssl.openssl.api.evp.pas** - EVP 高级加密接口（支持所有对称算法）
  - AES (128/192/256)
  - DES/3DES
  - RC4/RC2
  - Blowfish
  - Camellia
  - ChaCha20/Poly1305
  - SM4

### 非对称加密
- [x] **fafafa.ssl.openssl.api.rsa.pas** - RSA 算法
- [x] **fafafa.ssl.openssl.api.dsa.pas** - DSA 数字签名算法
- [x] **fafafa.ssl.openssl.api.dh.pas** - Diffie-Hellman 密钥交换
- [x] **fafafa.ssl.openssl.api.ec.pas** - 椭圆曲线基础
- [x] **fafafa.ssl.openssl.api.ecdsa.pas** - 椭圆曲线数字签名
- [x] **fafafa.ssl.openssl.api.ecdh.pas** - 椭圆曲线 Diffie-Hellman

### 消息认证码
- [x] **fafafa.ssl.openssl.api.hmac.pas** - HMAC 消息认证码

### 密钥派生
- [x] **fafafa.ssl.openssl.api.kdf.pas** - 密钥派生函数 (PBKDF2/HKDF/scrypt)

### 编码与格式
- [x] **fafafa.ssl.openssl.api.asn1.pas** - ASN.1 编码/解码
- [x] **fafafa.ssl.openssl.api.pem.pas** - PEM 格式处理
- [x] **fafafa.ssl.openssl.api.pkcs.pas** - PKCS 标准支持

### 证书管理
- [x] **fafafa.ssl.openssl.api.x509.pas** - X.509 证书
- [x] **fafafa.ssl.openssl.api.ocsp.pas** - OCSP 在线证书状态
- [x] **fafafa.ssl.openssl.api.ct.pas** - 证书透明度
- [x] **fafafa.ssl.openssl.api.store.pas** - 证书存储管理

### SSL/TLS
- [x] **fafafa.ssl.openssl.api.ssl.pas** - SSL/TLS 协议支持

### 高级功能
- [x] **fafafa.ssl.openssl.api.cms.pas** - 加密消息语法
- [x] **fafafa.ssl.openssl.api.ts.pas** - 时间戳服务
- [x] **fafafa.ssl.openssl.api.engine.pas** - 硬件加速引擎

### 辅助功能
- [x] **fafafa.ssl.openssl.api.bio.pas** - I/O 抽象层
- [x] **fafafa.ssl.openssl.api.comp.pas** - 压缩支持
- [x] **fafafa.ssl.openssl.api.conf.pas** - 配置文件管理
- [x] **fafafa.ssl.openssl.api.ui.pas** - 用户界面交互

### 主实现
- [x] **fafafa.ssl.openssl.pas** - OpenSSL 后端主实现

## 功能覆盖率

### 加密算法支持
| 类型 | 算法 | 状态 |
|------|------|------|
| 哈希 | MD4, MD5, SHA-1, SHA-224/256/384/512, SHA3, BLAKE2, SM3, RIPEMD160 | ✅ |
| 对称加密 | AES, DES, 3DES, RC4, RC2, Blowfish, Camellia, ChaCha20, SM4 | ✅ |
| 非对称加密 | RSA, DSA, DH, ECDSA, ECDH, EdDSA (Ed25519/Ed448) | ✅ |
| MAC | HMAC, CMAC, Poly1305 | ✅ |
| KDF | PBKDF2, HKDF, scrypt, TLS1-PRF | ✅ |

### 协议支持
| 协议 | 版本 | 状态 |
|------|------|------|
| SSL | 2.0, 3.0 | ✅ |
| TLS | 1.0, 1.1, 1.2, 1.3 | ✅ |
| DTLS | 1.0, 1.2 | ✅ |

### 证书格式
| 格式 | 支持 |
|------|------|
| X.509 | ✅ |
| PKCS#1 | ✅ |
| PKCS#7 | ✅ |
| PKCS#8 | ✅ |
| PKCS#12 | ✅ |
| PEM | ✅ |
| DER | ✅ |

## 使用示例

### 基础哈希计算
```pascal
uses
  fafafa.ssl.openssl.api.sha;

var
  Data: TBytes;
  Hash: TBytes;
  HashStr: string;
begin
  // 计算 SHA-256
  Data := TEncoding.UTF8.GetBytes('Hello, World!');
  Hash := SHA256Hash(Data);
  
  // 或直接获取十六进制字符串
  HashStr := SHA256HashString('Hello, World!');
end;
```

### RSA 加密
```pascal
uses
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.evp;

var
  Key: PRSA;
  EVPKey: PEVP_PKEY;
begin
  // 生成 RSA 密钥对
  Key := RSA_new();
  RSA_generate_key_ex(Key, 2048, nil, nil);
  
  // 转换为 EVP_PKEY
  EVPKey := EVP_PKEY_new();
  EVP_PKEY_set1_RSA(EVPKey, Key);
  
  // 使用密钥进行加密/解密操作
  // ...
  
  RSA_free(Key);
  EVP_PKEY_free(EVPKey);
end;
```

### 椭圆曲线签名
```pascal
uses
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.ecdsa;

var
  Key: PEC_KEY;
begin
  // 创建 secp256k1 密钥
  Key := EC_KEY_new_secp256k1();
  EC_KEY_generate_key(Key);
  
  // 使用密钥进行签名
  // ...
  
  EC_KEY_free(Key);
end;
```

## 编译要求
- Free Pascal Compiler (FPC) 3.2.0+
- Lazarus 2.0+
- OpenSSL 1.1.1+ 或 3.0+

## 许可证
Copyright (c) 2024 fafafa

## 贡献者
- fafafa - 主要开发者

## 更新日志
- 2024-12-29: 完成所有核心模块的移植
- 2024-12-28: 项目初始化，基础架构搭建

## 扩展建议（非阻断）
- 完善单元测试覆盖与回归矩阵
- 添加性能基准测试基线
- 补齐关键 API 的调用示例
- 评估 OpenSSL 3.x 新特性适配优先级
- 优化高频路径的内存分配与回收策略
