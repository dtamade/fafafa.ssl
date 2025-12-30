# 变更日志

本文档记录 fafafa.ssl 项目的所有重要变更。

格式基于 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.0.0/)，
版本号遵循 [语义化版本](https://semver.org/lang/zh-CN/)。

---

## [未发布]

### 新增 🎉

#### 测试基础设施
- 模糊测试框架 `tests/fuzz/fuzz_framework.pas`
  - TFuzzer 类支持随机输入生成和变异
  - 7 个模糊测试目标（Base64、Hex、PEM、DER、ASN.1、DN、URL）
- 性能基线框架 `tests/benchmarks/benchmark_framework.pas`
  - 统计分析（mean、stddev、P50/P95/P99）
  - JSON 基线导出
  - 回归检测（15% 阈值）

#### CI 工具
- `scripts/ci_benchmark.sh` - 性能回归检测
- `scripts/coverage_report.sh` - 代码覆盖率报告

### 修复 🐛
- 移除 `crypto.utils.pas` 中 6 处不可达代码
- 初始化 3 个函数的 Result 变量
- 修复 `QuickServer` 未使用参数提示

### 文档 📚
- `docs/RUST_ALIGNMENT_ROADMAP.md` - Rust 架构对齐评估
  - 诚实分析：不盲目追求 Rust 模式
  - 记录已完成的高价值改进

---

## [0.8.0] - 2025-10-24

### 新增 🎉

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

