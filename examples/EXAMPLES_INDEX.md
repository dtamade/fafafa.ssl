# fafafa.ssl 示例程序索引

**最后更新**: 2026-01-20

本索引提供了 fafafa.ssl 所有示例程序的完整导航，按功能分类并标注难度级别和依赖要求。

## 📚 快速导航

- [入门示例](#入门示例) - 从这里开始
- [TLS 连接示例](#tls-连接示例) - 客户端和服务器
- [证书管理示例](#证书管理示例) - 生成、验证、链管理
- [加密工具示例](#加密工具示例) - 哈希、加密、签名
- [HTTPS 客户端示例](#https-客户端示例) - 完整的 HTTP 客户端
- [HTTPS 服务器示例](#https-服务器示例) - 完整的 HTTP 服务器
- [PKCS 示例](#pkcs-示例) - PKCS#7 和 PKCS#12
- [WinSSL 示例](#winssl-示例) - Windows 原生 SSL
- [生产级示例](#生产级示例) - 生产环境代码
- [工具和辅助示例](#工具和辅助示例) - 实用工具

## 🎯 难度级别说明

- 🟢 **初级**: 适合初学者，代码简单，依赖少
- 🟡 **中级**: 需要一定的 SSL/TLS 知识，可能需要额外配置
- 🔴 **高级**: 复杂场景，需要深入理解 SSL/TLS 和相关协议

## 📋 依赖说明

- 🌐 **需要网络**: 示例需要连接到互联网
- 📜 **需要证书**: 示例需要证书文件（可能需要先生成）
- 🪟 **仅 Windows**: 示例仅在 Windows 平台运行
- 🐧 **仅 Linux/macOS**: 示例仅在 Linux/macOS 平台运行

---

## 入门示例

从这些示例开始学习 fafafa.ssl 的基本用法。

### hello_ssl.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 验证 OpenSSL 环境和版本信息
- **用途**: 确认 fafafa.ssl 安装正确
- **编译**: `fpc -Fu./src -Fu./src/openssl hello_ssl.pas`
- **运行**: `./hello_ssl`

### 01_tls_client.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: TLS 客户端连接示例
- **用途**: 学习如何建立 TLS 连接并发送 HTTPS 请求
- **编译**: `fpc -Mobjfpc -Fu./src -Fu./src/openssl 01_tls_client.pas`
- **运行**: `./01_tls_client https://www.example.com/`

### example_crypto_simple.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 简单加密示例（SHA-256, 随机数）
- **用途**: 学习基本的加密工具使用
- **编译**: `fpc -Fu./src example_crypto_simple.pas`

### hash_calculator.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 哈希计算工具（支持多种算法）
- **用途**: 计算文件或字符串的哈希值
- **编译**: `fpc -Fu./src hash_calculator.pas`

---

## TLS 连接示例

学习如何建立和管理 TLS 连接。

### 01_tls_client.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: 基本 TLS 客户端
- **特性**: SNI、证书验证、协议版本检测
- **位置**: `examples/01_tls_client.pas`

### example_tls_client.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: TLS 客户端示例（使用工厂模式）
- **特性**: 使用 TSSLFactory 创建连接
- **位置**: `examples/example_tls_client.pas`

### 05_https_server.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 简单 HTTPS 服务器
- **特性**: 服务器端 TLS、证书加载
- **位置**: `examples/05_https_server.pas`

### 08_mutual_tls.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 双向 TLS（mTLS）示例
- **特性**: 客户端证书认证、双向验证
- **位置**: `examples/08_mutual_tls.pas`

### simple_ssl_connection.pas
- **难度**: 🟢 初级
- **依赖**: 🌐 需要网络
- **功能**: 最简单的 SSL 连接示例
- **用途**: 快速验证 SSL 连接功能
- **位置**: `examples/simple_ssl_connection.pas`

### session_reuse_example.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: TLS 会话复用示例
- **特性**: 演示会话缓存和复用，提升性能
- **位置**: `examples/session_reuse_example.pas`

---

## 证书管理示例

学习证书的生成、验证和管理。

### 02_generate_certificate.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 生成自签名证书
- **特性**: RSA 密钥对生成、X.509 证书创建
- **位置**: `examples/02_generate_certificate.pas`

### 07_certificate_chain.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 证书链验证
- **特性**: 证书链构建、信任验证
- **位置**: `examples/07_certificate_chain.pas`

### 10_cert_renewal.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 证书自动更新服务
- **特性**: 证书过期监控、自动续期
- **位置**: `examples/10_cert_renewal.pas`

### certificate_verification_example.pas
- **难度**: 🟡 中级
- **依赖**: 📜 需要证书
- **功能**: 证书验证示例
- **特性**: 证书有效性检查、链验证
- **位置**: `examples/certificate_verification_example.pas`

### cert_info_viewer.pas
- **难度**: 🟢 初级
- **依赖**: 📜 需要证书
- **功能**: 证书信息查看器
- **用途**: 查看证书详细信息（主题、颁发者、有效期等）
- **位置**: `examples/cert_info_viewer.pas`

### pem_der_converter.pas
- **难度**: 🟢 初级
- **依赖**: 📜 需要证书
- **功能**: PEM/DER 格式转换工具
- **用途**: 在 PEM 和 DER 格式之间转换证书
- **位置**: `examples/pem_der_converter.pas`

---

## 加密工具示例

学习各种加密算法和工具的使用。

### 03_file_encryption.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 文件加密与解密
- **特性**: AES-256-GCM 加密、密钥派生
- **位置**: `examples/03_file_encryption.pas`

### 06_digital_signature.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 数字签名与验证
- **特性**: RSA-SHA256 签名、签名验证
- **位置**: `examples/06_digital_signature.pas`

### example_aes_gcm_aead.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: AES-GCM AEAD 加密示例
- **特性**: 认证加密、附加数据保护
- **位置**: `examples/example_aes_gcm_aead.pas`

### example_crypto_working.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 加密工具综合示例
- **特性**: 多种加密算法演示
- **位置**: `examples/example_crypto_working.pas`

### hash_calculator.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 哈希计算工具
- **特性**: 支持 MD5、SHA-1、SHA-256、SHA-512
- **位置**: `examples/hash_calculator.pas`

### file_encrypt_tool.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 文件加密工具（命令行）
- **用途**: 实用的文件加密/解密工具
- **位置**: `examples/file_encrypt_tool.pas`

### password_hash.pas / password_hash_v2.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 密码哈希工具
- **特性**: PBKDF2 密钥派生、盐值生成
- **位置**: `examples/password_hash.pas`, `examples/password_hash_v2.pas`

---

## HTTPS 客户端示例

完整的 HTTPS 客户端实现，包含各种场景。

### https_client/ 目录

#### https_client/https_client_simple.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: 简单 HTTPS GET 请求
- **特性**: 基本的 HTTPS 客户端实现

#### https_client/https_client_post.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: HTTPS POST 请求
- **特性**: POST 数据发送、表单提交

#### https_client/https_client_auth.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络, 📜 需要证书
- **功能**: 客户端证书认证
- **特性**: mTLS、客户端证书加载

#### https_client/https_client_session.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: 会话复用示例
- **特性**: TLS 会话缓存、性能优化

### 其他 HTTPS 客户端示例

#### 04_https_rest_client.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: REST API 客户端
- **特性**: JSON 处理、REST 接口调用

#### https_simple_get.pas
- **难度**: 🟢 初级
- **依赖**: 🌐 需要网络
- **功能**: 超简单 HTTPS GET 请求
- **用途**: 快速入门 HTTPS 客户端

#### https_client_production.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络
- **功能**: 生产级 HTTPS 客户端
- **特性**: 完整的错误处理、重试机制、超时控制

#### simple_https_demo.pas
- **难度**: 🟢 初级
- **依赖**: 🌐 需要网络
- **功能**: HTTPS 演示程序
- **用途**: 快速演示 HTTPS 功能

---

## HTTPS 服务器示例

完整的 HTTPS 服务器实现，包含各种场景。

### https_server/ 目录

#### https_server/https_server_simple.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 简单 HTTPS 服务器
- **特性**: 基本的 HTTPS 服务器实现

#### https_server/https_server_mtls.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: mTLS 服务器
- **特性**: 客户端证书验证、双向认证

#### https_server/https_server_alpn.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: ALPN 协议协商
- **特性**: HTTP/2、HTTP/1.1 协议选择

#### https_server/https_server_common.pas
- **难度**: 🔴 高级
- **依赖**: 无
- **功能**: HTTPS 服务器公共模块
- **用途**: 其他服务器示例的共享代码

### 其他服务器示例

#### 05_https_server.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 简单 HTTPS 服务器
- **特性**: 基本的服务器端 TLS 实现

---

## PKCS 示例

PKCS#7 和 PKCS#12 标准的使用示例。

### pkcs7_sign_example.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: PKCS#7 数字签名
- **特性**: 数据签名、签名验证

### pkcs7_encrypt_example.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: PKCS#7 加密
- **特性**: 数据加密、解密

### pkcs7_sign_encrypt_example.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: PKCS#7 签名+加密
- **特性**: 组合使用签名和加密

### pkcs12_example.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: PKCS#12 证书包
- **特性**: 证书和私钥打包、导入导出

---

## WinSSL 示例

Windows 原生 SSL/TLS 实现（Schannel）。

### 09_winssl_fips.pas
- **难度**: 🔴 高级
- **依赖**: 🪟 仅 Windows
- **功能**: WinSSL FIPS 模式
- **特性**: FIPS 140-2 合规性检测

### winssl_https_downloader.pas
- **难度**: 🟡 中级
- **依赖**: 🪟 仅 Windows, 🌐 需要网络
- **功能**: WinSSL HTTPS 下载器
- **特性**: 使用 WinSSL 下载文件

### winssl_rest_client.pas
- **难度**: 🟡 中级
- **依赖**: 🪟 仅 Windows, 🌐 需要网络
- **功能**: WinSSL REST 客户端
- **特性**: 使用 WinSSL 调用 REST API

### winssl_health_checker.pas
- **难度**: 🟡 中级
- **依赖**: 🪟 仅 Windows, 🌐 需要网络
- **功能**: WinSSL 健康检查工具
- **特性**: 检查 HTTPS 服务可用性

---

## 生产级示例

适合生产环境使用的完整实现。

### production/ 目录

所有 production/ 目录下的示例都是生产级代码，包含完整的错误处理、日志记录和性能优化。

#### production/https_client_simple.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络
- **功能**: 生产级 HTTPS 客户端（GET）
- **特性**: 完整错误处理、超时控制、重试机制

#### production/https_client_post.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络
- **功能**: 生产级 HTTPS 客户端（POST）
- **特性**: POST 请求、表单提交、错误处理

#### production/https_client_auth.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络, 📜 需要证书
- **功能**: 生产级客户端证书认证
- **特性**: mTLS、证书管理、错误处理

#### production/https_client_session.pas
- **难度**: 🔴 高级
- **依赖**: 🌐 需要网络
- **功能**: 生产级会话复用
- **特性**: 会话缓存、性能优化、连接池

#### production/https_server_simple.pas
- **难度**: 🔴 高级
- **依赖**: 📜 需要证书
- **功能**: 生产级 HTTPS 服务器
- **特性**: 完整的服务器实现、错误处理、日志记录

---

## 工具和辅助示例

实用工具和辅助模块。

### fafafa.examples.tcp.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: TCP 连接辅助模块
- **用途**: 提供 TCP socket 创建和连接功能

### fafafa.examples.sockets.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: Socket 操作辅助模块
- **用途**: 跨平台 socket 操作封装

### example_factory_usage.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 工厂模式使用示例
- **特性**: TSSLFactory 使用演示

### example_error_handling.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 错误处理示例
- **特性**: 异常处理、错误码使用

### example_result_type.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: Result 类型使用示例
- **特性**: Rust 风格错误处理

### example_streaming_operations.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 流式操作示例
- **特性**: TSSLStream 使用演示

### example_json_api.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: JSON API 示例
- **特性**: JSON 解析、API 调用

### example_https_api.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: HTTPS API 示例
- **特性**: HTTPS + JSON API

### demo_facade_api.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: Facade API 演示
- **特性**: 简化的 API 接口

### demo_fluent_api.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: Fluent API 演示
- **特性**: 链式调用、Builder 模式

### practical_examples.pas
- **难度**: 🟡 中级
- **依赖**: 无
- **功能**: 实用示例集合
- **特性**: 多个实用功能演示

### probe_sockets.pas
- **难度**: 🟡 中级
- **依赖**: 🌐 需要网络
- **功能**: Socket 探测工具
- **用途**: 测试 socket 连接

### simple_test.pas / ultra_simple_test.pas
- **难度**: 🟢 初级
- **依赖**: 无
- **功能**: 简单测试程序
- **用途**: 快速验证基本功能

---

## 📝 编译说明

### 基本编译命令

```bash
# 编译单个示例
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl -Fu./examples <example_file.pas>

# 编译到指定目录
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl -Fu./examples -FE./examples/bin <example_file.pas>
```

### 平台特定编译

#### Linux/macOS
```bash
# 标准编译
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl -Fu./examples <example_file.pas>

# macOS 需要指定 OpenSSL 路径
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl -Fu./examples \
    -Fl$(brew --prefix openssl@3)/lib \
    -Fi$(brew --prefix openssl@3)/include \
    <example_file.pas>
```

#### Windows
```powershell
# 使用 OpenSSL
fpc -Mobjfpc -Sh -Fu.\src -Fu.\src\openssl -Fu.\examples <example_file.pas>

# 使用 WinSSL（无需 OpenSSL）
fpc -Mobjfpc -Sh -Fu.\src -Fu.\src\winssl -Fu.\examples <example_file.pas>
```

### 批量编译

```bash
# 使用项目提供的编译脚本
chmod +x examples/compile_test.sh
./examples/compile_test.sh
```

---

## 🔧 运行说明

### 需要网络的示例

这些示例需要连接到互联网：
- 所有 HTTPS 客户端示例
- TLS 客户端示例
- REST API 示例

### 需要证书的示例

这些示例需要证书文件，可以使用 `02_generate_certificate.pas` 生成测试证书：

```bash
# 生成测试证书
./02_generate_certificate

# 运行需要证书的示例
./08_mutual_tls
```

### Windows 专用示例

这些示例仅在 Windows 平台运行：
- 所有 WinSSL 示例（09_winssl_fips.pas, winssl_*.pas）

---

## 📚 相关文档

- [README.md](README.md) - 示例程序说明
- [GETTING_STARTED.md](../docs/GETTING_STARTED.md) - 入门指南
- [API_REFERENCE.md](../docs/API_REFERENCE.md) - API 参考
- [TROUBLESHOOTING.md](../docs/TROUBLESHOOTING.md) - 问题排查

---

## 🤝 贡献示例

如果你想贡献新的示例程序，请遵循以下规范：

1. **命名规范**：
   - 编号示例：`NN_description.pas`（如 `11_new_feature.pas`）
   - 功能示例：`feature_name.pas`（如 `jwt_token_example.pas`）
   - 避免使用 `test_` 前缀（测试文件应放在 `tests/` 目录）

2. **代码规范**：
   - 包含清晰的注释说明功能和用途
   - 提供编译和运行说明
   - 包含错误处理
   - 遵循项目代码风格

3. **文档要求**：
   - 在文件头部添加功能说明
   - 更新本索引文档
   - 如果是新类别，考虑创建子目录

---

**最后更新**: 2026-01-20
