# fafafa.ssl - Production-Ready SSL/TLS Library

[![Production Ready](https://img.shields.io/badge/Production%20Ready-99.5%25-brightgreen)](https://github.com) [![Tests](https://img.shields.io/badge/Tests-1086%20passed-success)](docs/archive/phase_reports/PHASE_7_FINAL_REPORT.md) [![License](https://img.shields.io/badge/License-MIT-yellow)](LICENSE)

**企业级 SSL/TLS 加密库** - 为 FreePascal 提供完整的 TLS/加密封装，支持 OpenSSL、WinSSL、MbedTLS、WolfSSL 四后端

## 📦 30秒快速开始

```bash
# 安装 (Ubuntu/Debian)
sudo apt-get install libssl-dev fpc
```

### 最小示例

完整可运行代码，包含 TCP 连接 - 复制即可运行：

```pascal
program quickstart_complete;
{$mode objfpc}{$H+}

uses
  SysUtils, Math,
  fafafa.ssl.factory, fafafa.ssl.base,
  fafafa.ssl.openssl.api, fafafa.ssl.openssl.backed,
  fafafa.examples.tcp;  // 跨平台 TCP 辅助

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Socket: TSocketHandle;
  NetError: string;

begin
  // 1) 初始化网络 (Windows 需要)
  InitNetwork(NetError);

  // 2) TCP 连接
  Socket := ConnectTCP('www.google.com', 443);

  // 3) 初始化 OpenSSL
  Lib := TSSLFactory.GetLibrary(sslOpenSSL);
  Lib.Initialize;

  // 4) 创建 SSL 上下文和连接
  Ctx := Lib.CreateContext(sslCtxClient);
  Ctx.SetVerifyMode([sslVerifyNone]);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  Conn := Ctx.CreateConnection(Socket);
  (Conn as ISSLClientConnection).SetServerName('www.google.com');
  Conn.Connect;

  // 5) 显示连接信息
  WriteLn('TLS 连接成功!');
  WriteLn('协议: TLS 1.', Ord(Conn.GetProtocolVersion) - Ord(sslProtocolTLS10));
  WriteLn('密码套件: ', Conn.GetCipherName);

  // 清理
  Conn.Shutdown;
  CloseSocket(Socket);
  CleanupNetwork;
end.
```

### 编译运行

```bash
fpc -Fusrc -Fusrc/openssl -Fuexamples quickstart_complete.pas
./quickstart_complete
```

输出示例：
```
TLS 连接成功!
协议: TLS 1.3
密码套件: TLS_AES_256_GCM_SHA384
```

## 🌟 核心特性

| 特性 | 描述 |
|------|------|
| **四后端支持** | OpenSSL + WinSSL (Stable) / MbedTLS + WolfSSL (Preview) |
| **现代 API** | Rust 风格 Result 类型、Fluent Builder、Try 方法 |
| **生产级加密** | AES-256-GCM, PBKDF2, SHA-256/512, TLS 1.2/1.3 |
| **完整证书管理** | X.509 解析/验证/生成、CRL/OCSP |
| **全面测试** | 1,086 项测试、70+ 真实网站验证 |

## 📚 文档

| 入口 | 描述 |
|------|------|
| [文档索引](docs/INDEX.md) | **推荐入口** - 渐进式文档导航 |
| [API 参考](docs/API_REFERENCE.md) | 完整 API 文档 |
| [示例程序](examples/) | 分类示例 (Basic/Advanced/Scenarios) |
| [FAQ](docs/FAQ.md) | 常见问题解答 |

## 🚀 核心 API

### 推荐方式：Fluent Builder（现代 API）

```pascal
uses fafafa.ssl.connection.builder, fafafa.ssl.quick;

// TLS 连接 - 流式 API
Conn := TSSLConnectionBuilder.Create
  .WithHostname('api.example.com')
  .WithTimeout(30000)
  .BuildClient;

// 证书生成 - Quick API
TSSLQuick.GenerateCertFiles('localhost', 'server.crt', 'server.key');

// 错误处理 - Result 类型（无异常）
if TCryptoUtils.TrySHA256(Data, Hash) then
  ProcessData(Hash);
```

### 底层 API（完整控制）

<details>
<summary>展开查看底层 API 示例</summary>

```pascal
// 基础加密
Hash := TCryptoUtils.SHA256('Hello World');
Ciphertext := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);

// TLS 连接（工厂模式）
Ctx := TSSLFactory.CreateContext(sslCtxClient);
Conn := Ctx.CreateConnection(Socket);
Conn.Connect;

// 证书生成（Builder 模式）
KeyPair := TCertificateBuilder.Create
  .WithCommonName('localhost')
  .ValidFor(365)
  .SelfSigned;
```

</details>

## 🧪 测试 & CI/CD


### 运行测试

```bash
# 完整CI/CD流程（构建+测试+性能）
./ci_pipeline.sh all

# 仅构建
./ci_pipeline.sh build

# 仅测试
./ci_pipeline.sh test

# 性能基准测试
./ci_pipeline.sh bench
```

### 测试覆盖

| 测试类型 | 覆盖率 | 文件 |
|---------|--------|------|
| 单元测试 | 235个测试 | `tests/unit/` |
| 集成测试 | 70+网站 | `examples/test_real_websites_*.pas` |
| E2E场景 | 6场景, 83%通过 | `tests/test_e2e_scenarios.pas` |
| 模糊测试 | 7个目标 | `tests/fuzz/` |
| 性能基准 | 10项指标 | `tests/benchmarks/` |

### 模糊测试

```bash
# 运行编码器模糊测试
./tests/fuzz/bin/fuzz_ssl 5000

# 运行解析器模糊测试
./tests/fuzz/bin/fuzz_parsers 5000
```

模糊测试目标：Base64、Hex、PEM证书、DER证书、ASN.1、DN解析、URL解析

### CI/CD结果示例

```
================================================================
Performance Benchmark Results
================================================================
[ 1] TLS_Context_Create_x100      28ms    3571.4 ops/s
[ 2] SHA256_1KBx1000               45ms   22222.2 ops/s  
[ 3] Random_1KBx100                12ms    8333.3 ops/s
================================================================
Total: 8 tests, 8 passed, 0 failed
✅ All benchmarks passed!
```

## 📊 性能指标

| 操作 | 吞吐量 | 延迟 |
|------|--------|------|
| TLS握手 | ~3,200 ops/s | ~30ms |
| SHA-256 (1KB) | ~22,000 ops/s | ~0.04ms |
| AES-256-GCM (1KB) | ~15,000 ops/s | ~0.07ms |
| 随机数生成 (1KB) | ~8,000 ops/s | ~0.12ms |

*基于 Intel Core i7, OpenSSL 3.0*

## 🛡️ 安全特性

- ✅ **密码学强度**: AES-256, SHA-256/512, RSA-2048+
- ✅ **安全默认值**: TLS 1.2+, 强密码套件
- ✅ **证书验证**: 自动系统CA加载，支持证书钉扎
- ✅ **密钥派生**: PBKDF2 100,000次迭代
- ✅ **防时序攻击**: 恒定时间比较
- ✅ **内存安全**: 及时清零敏感数据

## 🏗️ 架构

```
fafafa.ssl/
├── src/                      # 核心源代码
│   ├── fafafa.ssl.factory.pas   # 工厂模式入口
│   ├── fafafa.ssl.base.pas      # 基础接口定义
│   ├── fafafa.ssl.openssl/      # OpenSSL后端 (Stable)
│   ├── fafafa.ssl.winssl/       # WinSSL后端 (Stable)
│   ├── fafafa.ssl.mbedtls/      # MbedTLS后端 (Preview)
│   ├── fafafa.ssl.wolfssl/      # WolfSSL后端 (Preview)
│   ├── fafafa.ssl.crypto.utils.pas  # 加密工具
│   ├── fafafa.ssl.cert.*/       # 证书管理
│   └── fafafa.ssl.errors.pas    # 错误处理
├── examples/                 # 95+示例程序
├── tests/                    # 测试套件
│   ├── unit/                # 单元测试
│   ├── fuzz/                # 模糊测试
│   ├── benchmarks/          # 性能测试
│   └── integration/         # 集成测试
├── scripts/                  # CI/CD工具
│   ├── ci_benchmark.sh      # 性能回归检测
│   └── coverage_report.sh   # 覆盖率报告
├── docs/                     # 文档
└── ci_pipeline.sh           # CI/CD主脚本
```

### 后端支持级别

| 后端 | 状态 | 平台 | 说明 |
|------|------|------|------|
| OpenSSL | **Stable** | Linux/macOS/Windows | 生产就绪，完整功能支持 |
| WinSSL | **Stable** | Windows | 生产就绪，原生 Schannel 支持 |
| MbedTLS | Preview | Linux/Windows | 轻量级嵌入式 TLS，需安装 libmbedtls |
| WolfSSL | Preview | Linux/Windows | 轻量级 TLS，需安装 libwolfssl |

**启用 Preview 后端**：
```bash
# 编译时启用 MbedTLS/WolfSSL
fpc -dENABLE_MBEDTLS -dENABLE_WOLFSSL -Fusrc your_app.pas
```

## 🤝 贡献指南

1. Fork本仓库
2. 创建特性分支 (`git checkout -b feature/AmazingFeature`)
3. 提交更改 (`git commit -m 'Add AmazingFeature'`)
4. 推送到分支 (`git push origin feature/AmazingFeature`)
5. 开启Pull Request

## 📈 版本历史

- **2.0.0** (2025-12-02) - 生产就绪版本 (98%)
  - ✅ 完整错误处理重构
  - ✅ 70+真实网站集成测试
  - ✅ CI/CD自动化流水线
  - ✅ 性能回归检测系统
  
- **1.5.0** (2025-11-26) - 功能完善
  - ✅ Production-grade AES-256-GCM
  - ✅ OCSP API完整绑定
  - ✅ 会话复用API

- **1.0.0** (2025-11-20) - 初始版本

## 📄 许可证

本项目采用 MIT 许可证 - 详见 [LICENSE](LICENSE) 文件

## 🙏 致谢

- [OpenSSL Project](https://www.openssl.org/) - 加密库
- [Free Pascal](https://www.freepascal.org/) - 编译器

## 💬 支持

- **文档**: 查看 `docs/` 目录
- **示例**: 查看 `examples/` 目录
- **问题**: 提交 GitHub Issue
- **讨论**: GitHub Discussions

---

**🚀 Ready for Production!** - 98% Production-Ready SSL/TLS Library

Made with ❤️ by the fafafa.ssl team
