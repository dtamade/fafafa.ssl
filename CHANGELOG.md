# 变更日志

本文档记录 fafafa.ssl 项目的所有重要变更。

格式基于 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.0.0/)，
版本号遵循 [语义化版本](https://semver.org/lang/zh-CN/)。

---

## [1.1.0] - 待发布

**架构改进版本** - 为纯 FreePascal TLS 后端铺平道路

### 变更

#### 架构改进

- **GetNativeHandle 接口重构**
  - 从 6 个核心接口移除 `GetNativeHandle` 方法
  - 新增可选接口 `ISSLNativeHandleAccess`
  - C 库后端（OpenSSL/WinSSL/MbedTLS/WolfSSL）实现新接口
  - 纯 Pascal 后端无需实现（为未来准备）

- **类型安全提升**
  - 使用 `Supports()` 接口查询机制
  - 运行时类型检查防止误用
  - 统一的错误上下文信息

- **辅助函数单元**
  - `fafafa.ssl.openssl.native_handle`
  - `fafafa.ssl.winssl.native_handle`
  - `fafafa.ssl.mbedtls.native_handle`
  - `fafafa.ssl.wolfssl.native_handle`
  - 提供 `GetNativeHandleSafe()` 和 `TryGetNativeHandle()` 函数

### 新增

- **文档**
  - `docs/ARCHITECTURE.md` - 架构设计文档
  - `docs/MIGRATION_GUIDE_V1.1.md` - v1.1 迁移指南
  - `.claude/plans/refactoring-completion-report.md` - 重构完成报告
  - `.claude/plans/refactoring-test-verification.md` - 测试验证报告

### 修复

- 更新测试文件以使用新的接口模式
  - `test_mbedtls_framework.pas`
  - `test_wolfssl_framework.pas`
  - `openssl/test_openssl_v2.pas`
  - `openssl/test_openssl_basic_validation.pas`

### 影响

- **向后兼容**: ✅ 对于标准用户代码完全兼容
- **高级用户**: 需要迁移直接使用 `GetNativeHandle` 的代码（见迁移指南）
- **性能**: 无性能回归（Supports 查询开销可忽略）

### 测试验证

- 191 个测试通过，通过率 ~99%
- 所有后端编译成功
- 无功能回归

---

## [1.0.0] - 2026-02-05

**fafafa.ssl v1.0.0 正式发布** - 企业级 SSL/TLS 库

### 亮点

- **160 个源文件，95,143 行代码**
- **415 个测试文件，100% 通过率**
- **57 个示例程序**
- **0 个 TODO 残留**

### 新增

#### PKCS#11 硬件安全模块支持
- **TPKCS11Engine** - HSM 集成引擎
  - 动态加载 PKCS#11 库
  - 支持 SoftHSM2、YubiKey 等硬件
- **PIN 回调机制** - 安全的 PIN 输入
  - 交互式 PIN 输入回调
  - PIN 缓存和自动重试
- **私钥加载** - 从 HSM 加载私钥
  - PKCS#11 URI 解析
  - 密钥查找和使用

#### DANE/DNSSEC 支持
- **TDANEValidator** - DNS-Based Authentication
  - TLSA 记录查询和验证
  - 证书关联验证
- **ldns 集成** - 可选 ldns 库支持
  - DNSSEC 签名验证
  - 动态库加载，优雅降级

#### 无锁并发优化 (Phase B)
- **TLockFreeRingBuffer** - 高性能 SPSC 无锁环形缓冲区
  - 单生产者单消费者模型，无需锁即可线程安全
  - x86/x86_64 内存屏障实现（lfence/sfence/mfence）
  - 零拷贝读写支持（GetWritePtr/GetReadPtr）
  - 缓存行对齐避免伪共享
  - 性能: 16M+ ops/s, 195+ MB/s 吞吐量
  - 30 个测试全部通过（含并发测试）

- **TBufferPool** - 三级内存池
  - 小缓冲区 (4KB): 高频小数据
  - 中缓冲区 (16KB): 常规数据块
  - 大缓冲区 (64KB): 大文件传输
  - 引用计数和自动归还
  - 100% 命中率（重复分配场景）
  - 21 个测试全部通过

- **TShardedSessionCache** - 分片会话缓存
  - 16 个独立分片，每个分片独立锁
  - FNV-1a 哈希均匀分布
  - 并发吞吐量提升 8-16 倍
  - 18 个测试全部通过

#### 测试覆盖增强 (Phase C Week 1)
- **完整的 .lpi 覆盖** - 为所有 366 个测试程序创建 Lazarus 项目文件
  - tests/ (根目录): 61 个
  - certificate/: 39 个
  - crypto/: 61 个
  - examples/: 39 个
  - winssl/: 37 个
  - integration/: 26 个
  - connection/: 17 个
  - unit/: 17 个
  - benchmarks/: 15 个
  - openssl/: 13 个
  - diagnostic/: 11 个
  - config/: 10 个
  - security/: 8 个

#### WinSSL 后端 100% 完成
- Phase 1: 证书验证（自动模式）- 证书链验证、主机名验证
- Phase 2: 证书文件加载 - LoadCertificate/LoadPrivateKey/LoadCAFile
- Phase 3: 客户端证书（双向 TLS）- 客户端证书配置和握手
- Phase 4: ALPN 协议协商 - HTTP/2 协议协商支持
- Phase 5: 服务器 TLS 握手 - 完整的服务器端实现
- Phase 6: 会话复用优化 - 线程安全的会话管理器

### 改进

#### TBaseSSLConnection 抽象基类
- **架构重构** - 所有连接模块现在继承自 `TBaseSSLConnection`
  - 21 个抽象 `Do*` 方法供后端实现
  - 基类提供 ~50 个公共方法的统一实现
  - 统一的性能指标跟踪、错误历史管理、状态管理
- **代码精简**
  - MbedTLS Connection: 705 → 566 行 (-20%)
  - OpenSSL Connection: 1480 → 1388 行 (-6%)
  - WinSSL Connection: 2741 → 2169 行 (-21%)
  - 新建 WolfSSL Connection: 641 行（独立模块）
  - 新建 Base Class: 676 行

#### 测试基础设施
- 模糊测试框架 `tests/fuzz/fuzz_framework.pas`
  - TFuzzer 类支持随机输入生成和变异
  - 7 个模糊测试目标（Base64、Hex、PEM、DER、ASN.1、DN、URL）
- 性能基线框架 `tests/benchmarks/benchmark_framework.pas`
  - 统计分析（mean、stddev、P50/P95/P99）
  - JSON 基线导出
  - 回归检测（15% 阈值）

### 修复

- **编译器警告清理**
  - fafafa.ssl.logging.pas - 修复 FreeInstance 方法名冲突
  - fafafa.ssl.crypto.hash.pas - 抑制 SHA-512 常量范围检查警告
  - fafafa.ssl.cert.utils.pas - 抑制 TBytes 未初始化误报
  - fafafa.ssl.factory.pas - 正确处理弃用 API 调用
- 移除 `crypto.utils.pas` 中 6 处不可达代码
- 初始化 3 个函数的 Result 变量
- 修复 OpenSSL 库初始化死锁和无限递归
- 改进 test_security_attacks 以优雅处理 OpenSSL 不可用

### 文档

- 完整的 API 参考文档
- PKCS#11 架构文档
- 用户指南和快速入门
- 部署指南和安全最佳实践

---

## [0.8.0] - 2025-10-24
### 新增

#### WinSSL 企业功能
- 企业配置管理类 `TSSLEnterpriseConfig`
- FIPS 模式检测 `IsFipsModeEnabled`
- 企业受信任根证书获取 `GetEnterpriseTrustedRoots`
- 组策略读取 `GetGroupPolicies`

#### 增强证书验证
- 新增 `VerifyEx` 方法支持高级验证选项
- 证书吊销检查（CRL/OCSP）
- 详细验证结果 `TSSLCertVerifyResult`
- 证书验证标志 `TSSLCertVerifyFlags`

#### 错误处理
- 友好错误消息（中英文）`GetFriendlyErrorMessageCN/EN`
- 错误分类 `ClassifyOpenSSLError`
- 错误类别获取 `GetOpenSSLErrorCategory`
- 结构化日志支持

#### 文档
- 完整文档中心 `docs/README.md`
- API 参考文档 `docs/API_REFERENCE.md`
- 用户指南 `docs/USER_GUIDE.md`
- 故障排除指南 `docs/TROUBLESHOOTING.md`
- 部署指南 `docs/DEPLOYMENT_GUIDE.md`
- 安全指南 `docs/SECURITY_GUIDE.md`
- 迁移指南 `docs/MIGRATION_GUIDE.md`
- 快速入门更新 `QUICK_START.md`

#### 示例
- `examples/hello_ssl.pas` - 快速入门示例

### 改进 🚀

#### 代码质量
- 统一编译模式为 `{$mode objfpc}{$H+}`
- 遵循 WARP.md 命名规范
- 参数命名统一（`a` 前缀）
- 本地变量命名统一（`L` 前缀）

#### OpenSSL API
- 补充 CMS 模块缺失的 80+ 函数
- 添加 X.509 验证相关函数
  - `X509_STORE_set_flags`
  - `X509_STORE_CTX_get_error`
  - `X509_STORE_CTX_get0_param`
  - `X509_VERIFY_PARAM_set_flags`

#### WinSSL API
- 添加证书链验证标志常量
  - `CERT_CHAIN_REVOCATION_CHECK_END_CERT`
  - `CERT_CHAIN_REVOCATION_CHECK_CHAIN`
- 添加证书错误代码常量
  - `CERT_E_REVOCATION_FAILURE`
  - `CERT_E_CN_NO_MATCH`
  - `CERT_E_INVALID_NAME`

### 修复 🐛

- 修复 `fafafa.ssl.openssl.api.pkcs7.pas` 参数命名错误
- 修复 `fafafa.ssl.openssl.api.cms.pas` 编译模式不兼容
- 修复 CMS 模块函数指针类型转换
- 修复 `CMS_stream` 关键字冲突（重命名为 `CMS_stream_func`）
- 修复 OpenSSL `VerifyEx` 方法缺失实现

### 测试 🧪

- PKCS#7: 90.9% (10/11 测试通过)
- PKCS#12: 100% (15/15 测试通过)
- CMS: 95% (19/20 测试通过)
- 证书服务: 92.3% 平均通过率
- 新增 WinSSL 企业功能测试
- 新增错误处理测试
- 新增证书验证增强测试

### 性能 ⚡

- CMS 测试通过率从 50% 提升到 95%
- 减少编译警告数量

### 文档 📖

- 新增 7 个核心文档（共 ~10,000 行）
- 更新快速入门指南
- 添加完整 API 参考
- 提供部署和安全最佳实践

---

## [0.7.0] - 2025-10-01

### 新增

#### 核心架构
- 抽象接口层 (`fafafa.ssl.abstract.intf`)
- 统一类型定义 (`fafafa.ssl.abstract.types`)
- 工厂模式支持 (`fafafa.ssl.factory`)

#### OpenSSL 支持
- OpenSSL 1.1.1 兼容性
- OpenSSL 3.x 支持
- 50+ 核心模块绑定
- Priority 1 模块 97.9% 测试通过率

#### WinSSL 支持
- Windows Schannel 集成
- 系统证书存储访问
- 原生 Windows API 调用

### 改进

- 模块化架构设计
- 跨平台抽象
- 内存管理优化

### 测试

- 150+ 自动化测试
- PowerShell 测试运行器
- 分优先级测试覆盖

---

## [0.6.0] - 2025-09-15

### 新增

- 基础 SSL/TLS 连接支持
- 证书加载与验证
- 基本错误处理

### 已知问题

- 部分模块测试覆盖不足
- 性能未优化

---

## [0.5.0] - 2025-09-01

### 新增

- 项目初始化
- OpenSSL 基础绑定
- 简单示例程序

---

## 版本说明

### 版本号格式

`主版本号.次版本号.修订号`

- **主版本号**: 不兼容的 API 变更
- **次版本号**: 向后兼容的功能新增
- **修订号**: 向后兼容的问题修复

### 发布周期

- **主版本**: 每年 1 次
- **次版本**: 每季度 1 次
- **修订版**: 按需发布

---

## 如何升级

### 从 v0.7 升级到 v0.8

1. **无需修改代码** - v0.8 完全向后兼容
2. **可选使用新功能**:
   ```pascal
   // 使用增强验证
   var LResult: TSSLCertVerifyResult;
   LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult);
   
   // 使用 WinSSL 企业功能
   var LConfig := TSSLEnterpriseConfig.Create;
   if LConfig.IsFipsModeEnabled then
     WriteLn('FIPS mode enabled');
   ```
3. **查看** [MIGRATION_GUIDE.md](MIGRATION_GUIDE.md)

### 从 v0.6 升级到 v0.7

1. **更新接口引用**:
   ```pascal
   // 旧代码
   var LContext: TSSLContext;
   
   // 新代码
   var LContext: ISSLContext;
   ```
2. **更新类型名称**:
   ```pascal
   // 旧: TSSLProtocol
   // 新: TSSLProtocolVersion
   ```
3. **详细步骤** 参见 [MIGRATION_GUIDE.md](MIGRATION_GUIDE.md)

---

## 贡献者

感谢所有为 fafafa.ssl 做出贡献的开发者！

- 核心开发团队
- 测试贡献者
- 文档贡献者
- Issue 报告者

---

## 支持

- **问题报告**: [GitHub Issues](https://github.com/dtamade/fafafa.ssl/issues)
- **功能请求**: [GitHub Discussions](https://github.com/dtamade/fafafa.ssl/discussions)
- **安全漏洞**: security@example.com

---

[未发布]: https://github.com/dtamade/fafafa.ssl/compare/v0.8.0...HEAD
[0.8.0]: https://github.com/dtamade/fafafa.ssl/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/dtamade/fafafa.ssl/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/dtamade/fafafa.ssl/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/dtamade/fafafa.ssl/releases/tag/v0.5.0

