# Implementation Plan: SSH Key Support

## Overview

本实现计划将 SSH 密钥支持功能分解为可执行的编码任务。实现使用 FreePascal，遵循 fafafa.ssl 现有的架构模式和编码规范。

## Tasks

- [x] 1. 创建 SSH 密钥模块基础结构
  - [x] 1.1 创建 `src/fafafa.ssl.ssh.pas` 单元文件
    - 定义 TSSHKeyType、TSSHKeyFormat 枚举
    - 定义 TSSHKeyError 错误枚举
    - 定义 Result 类型（TSSHKeyResult、TSSHPublicKeyResult）
    - _Requirements: 1.4, 1.5, 1.6_

  - [x] 1.2 定义 SSH 密钥接口
    - 定义 ISSHPublicKey 接口
    - 定义 ISSHPrivateKey 接口
    - 定义 ISSHKeyPair 接口
    - _Requirements: 1.1, 2.1_

- [x] 2. 实现 SSH 公钥解析
  - [x] 2.1 实现 OpenSSH 公钥格式解析器
    - 解析 "ssh-rsa", "ssh-ed25519", "ecdsa-sha2-*" 格式
    - 提取密钥类型、Base64 数据、注释
    - 实现 TryParsePublicKey 方法
    - _Requirements: 1.1, 1.2, 1.3, 1.4, 1.5, 1.6_

  - [x]* 2.2 编写公钥解析属性测试
    - **Property 1: Public Key Parse-Export Round Trip**
    - **Validates: Requirements 1.1, 4.1**

  - [x] 2.3 实现公钥 Blob 解码
    - 解码 RSA 公钥 Blob（e, n）
    - 解码 Ed25519 公钥 Blob（32 字节）
    - 解码 ECDSA 公钥 Blob（曲线 + 点）
    - _Requirements: 1.4, 1.5, 1.6_

- [x] 3. 实现 SSH 私钥解析
  - [x] 3.1 实现 OpenSSH 私钥格式解析器
    - 解析 openssh-key-v1 格式头部
    - 解析加密参数（cipher、kdf）
    - 解析公钥和私钥部分
    - _Requirements: 2.1, 2.6_

  - [x] 3.2 实现私钥解密
    - 实现 bcrypt KDF 密钥派生 (BcryptPBKDF, BcryptHash)
    - 实现 Blowfish 加密 (BlowfishInit, BlowfishEncrypt, BlowfishExpandKey, BlowfishExpandKeyWithSalt)
    - 实现 AES-256-CTR 解密 (使用现有 AESDecryptCTR)
    - 实现 AES-256-CBC 解密 (使用现有 AESDecryptCBC)
    - 处理未加密私钥
    - _Requirements: 2.3, 2.4, 2.5_

  - [x] 3.3 实现 PEM 格式私钥解析
    - 支持 PKCS#1 RSA 私钥
    - 支持 PKCS#8 私钥
    - 集成现有 fafafa.ssl.pem 模块
    - _Requirements: 2.2, 2.7_

  - [x]* 3.4 编写私钥解析属性测试
    - **Property 2: Private Key Parse-Export Round Trip**
    - **Validates: Requirements 2.1, 2.2, 2.3, 2.5, 4.2, 4.3**

  - [x]* 3.5 编写错误处理属性测试
    - **Property 3: Invalid Input Error Handling**
    - **Validates: Requirements 1.3, 2.4**

- [x] 4. Checkpoint - 确保解析测试通过
  - 确保所有测试通过，如有问题请询问用户
  - 创建了 tests/test_ssh.pas 测试文件
  - 添加了纯 Pascal Base64 编码/解码函数以避免 OpenSSL 依赖
  - 修复了常量和函数前向声明顺序问题
  - 所有 46 个测试通过

- [x] 5. 实现 SSH 密钥生成
  - [x] 5.1 实现 RSA 密钥对生成
    - 使用 OpenSSL RSA_generate_key_ex
    - 支持 2048、3072、4096 位
    - 生成公钥和私钥对象
    - _Requirements: 3.1, 3.6_

  - [x] 5.2 实现 Ed25519 密钥对生成
    - 使用 OpenSSL EVP_PKEY_keygen
    - 生成 32 字节私钥和公钥
    - _Requirements: 3.2, 3.6_

  - [x] 5.3 实现 ECDSA 密钥对生成
    - 支持 P-256、P-384、P-521 曲线
    - 使用 OpenSSL EC_KEY_generate_key
    - _Requirements: 3.3, 3.6_

  - [x] 5.4 实现密钥注释和加密
    - 添加注释到公钥
    - 使用 bcrypt + AES-256-CTR 加密私钥
    - _Requirements: 3.4, 3.5_

  - [x]* 5.5 编写密钥生成属性测试
    - **Property 4: Key Generation Validity**
    - **Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5, 7.1, 7.2**

- [x] 6. 实现 SSH 密钥导出
  - [x] 6.1 实现公钥 OpenSSH 格式导出
    - 编码密钥 Blob 为 Base64
    - 格式化为单行格式
    - _Requirements: 4.1_

  - [x] 6.2 实现私钥 OpenSSH 格式导出
    - 构建 openssh-key-v1 格式
    - 支持加密和未加密导出
    - _Requirements: 4.2, 4.4_

  - [x] 6.3 实现 PEM 格式导出
    - 导出公钥为 SubjectPublicKeyInfo
    - 导出私钥为 PKCS#8
    - _Requirements: 4.3_

- [x] 7. 实现格式转换
  - [x] 7.1 实现 OpenSSH ↔ PEM 公钥转换
    - OpenSSH 公钥 → PEM (SubjectPublicKeyInfo)
    - PEM 公钥 → OpenSSH 格式
    - _Requirements: 6.1, 6.2_

  - [x] 7.2 实现 OpenSSH ↔ PEM 私钥转换
    - OpenSSH 私钥 → PEM (PKCS#8)
    - PEM 私钥 → OpenSSH 格式
    - 处理加密密钥转换
    - _Requirements: 6.3, 6.4, 6.5_

  - [x]* 7.3 编写格式转换属性测试
    - **Property 5: Format Conversion Round Trip**
    - **Validates: Requirements 6.1, 6.2, 6.3, 6.4, 6.5**

- [x] 8. Checkpoint - 确保导出和转换测试通过
  - 确保所有测试通过，如有问题请询问用户

- [x] 9. 实现指纹计算
  - [x] 9.1 实现 SHA-256 指纹计算
    - 计算公钥 Blob 的 SHA-256 哈希
    - 格式化为 "SHA256:base64" 格式
    - _Requirements: 5.1, 5.3_

  - [x] 9.2 实现 MD5 指纹计算
    - 计算公钥 Blob 的 MD5 哈希
    - 格式化为 "MD5:xx:xx:xx:..." 格式
    - _Requirements: 5.2, 5.3_

  - [x]* 9.3 编写指纹计算属性测试
    - **Property 6: Fingerprint Consistency**
    - **Validates: Requirements 5.1, 5.2, 5.3, 5.4**

- [x] 10. 实现密钥验证
  - [x] 10.1 实现密钥对匹配验证
    - 使用私钥签名测试数据
    - 使用公钥验证签名
    - _Requirements: 7.1_

  - [x] 10.2 实现密钥参数验证
    - 验证 RSA 模数大小
    - 验证 ECDSA 曲线参数
    - 检测弱密钥配置
    - _Requirements: 7.2, 7.3, 7.4_

  - [x]* 10.3 编写密钥验证属性测试
    - **Property 7: Key Pair Validation Correctness**
    - **Validates: Requirements 7.1, 7.3**

- [x] 11. 实现 authorized_keys 处理
  - [x] 11.1 实现 authorized_keys 解析
    - 解析多行文件
    - 提取选项（command=, from=, no-pty）
    - 跳过无效行并记录警告
    - _Requirements: 8.1, 8.2, 8.4_

  - [x] 11.2 实现 authorized_keys 写入
    - 格式化条目为标准格式
    - 保留选项
    - _Requirements: 8.3_

  - [x]* 11.3 编写 authorized_keys 属性测试
    - **Property 8: authorized_keys Round Trip**
    - **Validates: Requirements 8.1, 8.2, 8.3, 8.4**

- [x] 12. 实现文件 I/O 和权限
  - [x] 12.1 实现密钥文件读写
    - 从文件加载公钥和私钥
    - 保存密钥到文件
    - _Requirements: 1.2, 4.5_

  - [x] 12.2 实现文件权限设置（Unix）
    - 私钥文件设置 600 权限
    - 公钥文件设置 644 权限
    - _Requirements: 4.5_

- [x] 13. Final Checkpoint - 确保所有测试通过
  - 确保所有测试通过，如有问题请询问用户

## Notes

- 标记为 `*` 的任务是可选的属性测试任务，可以跳过以加快 MVP 开发
- 每个任务都引用了具体的需求条款以确保可追溯性
- 检查点任务用于验证增量进度
- 属性测试验证通用正确性属性，每个测试至少运行 100 次迭代
