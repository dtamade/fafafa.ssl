# fafafa.ssl - Multi-backend SSL/TLS Library

[![Version](https://img.shields.io/badge/Version-v1.5.0-blue)](https://github.com/dtamade/fafafa.ssl/releases/tag/v1.5.0)
[![Linux Minimal Gate](https://img.shields.io/badge/Linux%20minimal%20gate-passing-success)](docs/ROADMAP.md)
[![Roadmap](https://img.shields.io/badge/Roadmap-active-blue)](docs/ROADMAP.md)
[![OpenSSL](https://img.shields.io/badge/OpenSSL-1.1.1%2B%20%7C%203.0%2B-blue)](https://www.openssl.org/)
[![TLS](https://img.shields.io/badge/TLS-1.2%20%7C%201.3-blue)](https://tools.ietf.org/html/rfc8446)
[![FPC](https://img.shields.io/badge/FreePascal-3.2.0%2B-orange)](https://www.freepascal.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow)](LICENSE)

**统一 SSL/TLS 加密库** - 为 FreePascal 提供统一抽象 + 多后端实现（OpenSSL / WinSSL / FreePascal，且可选 MbedTLS / WolfSSL）

## 最新版本 v1.5.0

**正式发布候选** - deprecated helper API 已移除，接口版本号为 `10500`，发布门禁与 release workflow 已对齐到当前仓库入口。

### 新增功能

- ✅ `FAFAFA_SSL_VERSION_STRING = '1.5.0'` / `FAFAFA_SSL_INTERFACE_VERSION = 10500`
- ✅ deprecated 便捷函数已移除，调用方应使用 `TSSLFactory.*` 路径
- ✅ OpenSSL 连接级 Early Data 契约与运行时接口对齐
- ⚠️ WolfSSL 连接级 Early Data 仅在 helper 完整时暴露接口
- ❌ MbedTLS 和 WinSSL 不再暴露不可用的 Early Data 接口

### 后端支持

| 后端       | Early Data | 状态                      |
| ---------- | ---------- | ------------------------- |
| FreePascal | ⚠️         | 实验性                    |
| OpenSSL    | ✅         | 生产就绪                  |
| WolfSSL    | ⚠️         | 按构建/运行时 helper 门控 |
| MbedTLS    | ❌         | 不支持                    |
| WinSSL     | ❌         | 不支持                    |

### 快速开始

```pascal
// 启用 Early Data（OpenSSL 生产可用；FreePascal 为实验性；WolfSSL 需 helper 完整）
Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
Ctx := Lib.CreateContext(sslCtxClient);
if Supports(Ctx, ISSLEarlyDataContext, EarlyDataCtx) then
  EarlyDataCtx.SetClientEarlyDataEnabled(True);
```

当前真相：

- `OpenSSL`: stable / production-ready early-data path
- `FreePascal`: experimental，默认 replay truth 落到本地持久化 replay-store；默认路径不可用或不可写时 fail-closed reject
- `WolfSSL`: 受 build/runtime helper 门控；如果当前动态库缺少 early-data helpers，则 capability 发布为 `none`，context / connection 都不会暴露 early-data 接口
- `WinSSL` / `MbedTLS`: unsupported

📖 **详细文档**: [Early Data 使用指南](docs/guides/EARLY_DATA_GUIDE.md) | [后端能力矩阵](docs/BACKEND_CAPABILITY_MATRIX.md)

---

## 当前状态

- 当前路线图入口：`docs/ROADMAP.md`
- 当前执行控制面：`docs/ROADMAP.md` -> `docs/plans/2026-05-12-release-v1.5.0-formalization.md` -> `docs/test_reports/RELEASE_READINESS_V1.5.0.md`
- 当前默认构建命令：`python3 scripts/compile_all_modules.py`
- 当前默认本地门禁：`bash scripts/run_minimal_ci_gate.sh --fast-local`
- 当前 FreePascal TLS 1.3 focused gate：`bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- 当前代码风格门禁：`python3 scripts/check_code_style.py src`
- 当前 release workflow：`.github/workflows/release.yml`
- Wave C 文档仅用于 closeout / approval / historical cross-check
- 当前 Linux 基线已验证可重复；更广泛的功能完整度仍按 roadmap 持续收敛

## 核心特性

### 多后端支持

- **OpenSSL 后端**: Linux/macOS/Windows，支持 OpenSSL 1.1.1+ 和 3.0+
- **WinSSL 后端**: Windows 原生 Schannel，零依赖部署，自动使用系统证书
- **FreePascal 后端**: 纯 Pascal TLS 1.3 主线，当前聚焦 completeness roadmap
- **MbedTLS / WolfSSL**: 可选后端（需定义 `ENABLE_MBEDTLS` / `ENABLE_WOLFSSL`）

### v1.3.0 智能化版本 ✅

- **自动后端选择**: 基于需求智能选择最佳后端（安全/性能/兼容性优先）
- **Builder 集成**: WithSecurityFirst, RequireTLS13 等链式 API
- **智能评分**: 40+ 维度评分算法，推荐原因自动生成
- **差异对比**: 能力矩阵差异分析，多格式报告生成（text/json/html）
- **完整指南**: 800+ 行后端选择使用指南

### v1.2.0 新增功能 🆕

- **能力矩阵扩展**: 从 11 字段扩展到 40+ 字段，14 个类型安全的辅助查询函数
- **能力矩阵缓存**: 具体性能收益请以 fresh benchmark 为准；当前 benchmark/baseline 入口见 `docs/guides/PERFORMANCE_GUIDE.md`、`scripts/run_phase2_performance_baseline.sh` 与 `tests/benchmarks/run_all_benchmarks.sh`。
- **数据互操作**: JSON/XML 序列化支持，标准化数据交换
- **开发工具**: Web 可视化工具，一键对比后端能力
- **统一 API**: `fafafa.ssl.native_handle` 统一原生句柄访问（v1.1.1）
- **能力语义收敛**: `Supports*`/`*Support` 字段表示“当前后端运行时能力”，后端不可用路径统一记录为 `[SKIP] [backend-not-available|dependency|capability]`，不计为 PASS（详见 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`）
- **WinSSL 跨平台测试契约**: `tests/winssl/*` 在非 Windows 平台输出 `[BLOCKED] [platform]` + `[SKIP] [platform]`，用于区分平台阻塞与真实失败。

### v1.0.0 核心功能

- **PKCS#11 支持**: 硬件安全模块(HSM)集成，PIN 回调，私钥加载
- **DANE/DNSSEC 支持**: DNS-Based Authentication，可选 ldns 库
- **无锁并发优化**: TLockFreeRingBuffer, TBufferPool, TShardedSessionCache
- **可重复门禁**: Linux compile gate、P2 模块回归，以及 FreePascal TLS 1.3 focused gate

### 加密与安全

- **TLS 协议**: TLS 1.2/1.3，自动协议协商
- **加密算法**: AES-256-GCM, SHA-256/512, RSA-2048+, ECDSA
- **证书管理**: X.509 解析/验证/生成，CRL/OCSP 吊销检查
- **会话复用 / Session Ticket**: 属于 backend-specific truth；尤其 WinSSL 当前仍按 experimental public surface 理解，不应在首页直接承诺固定握手收益。

### 开发体验

- **简洁 API**: 1 行代码实现 HTTPS 连接
- **Rust 风格 Result**: TSSLDataResult, TSSLOperationResult
- **流式 Builder**: TSSLContextBuilder, TSSLConnectionBuilder
- **完整 Try 方法**: 无异常版本 API
- `fafafa.ssl` 主门面当前也 re-export `TSSLVersion` / `TKeySize` / `TTimeoutDuration` / `TBufferSize` 这组 non-generic type-safety surface；`TSecureData<T>` / `TResult<T, E>` 继续保留在 `fafafa.ssl.safety`。

## Use replay-store opt-in for FreePascal early-data servers

FreePascal 的 `0-RTT / early data` 目前仍是实验性能力，但默认 shipped path 已经会把服务端 replay truth 落到本地持久化 replay-store 路径。若该路径不可用或不可写，恢复的 early data 会 fail-closed reject。当前 public opt-in 仍只开放给 FreePascal server context，用来显式指定 replay-store 的落点。

用 builder 时，可选这两个入口：

- `WithServerEarlyDataReplayStoreFile(...)`
- `WithServerEarlyDataReplayStoreDirectory(...)`

用 config/factory 时，可选这两个字段：

- `TSSLConfig.ServerEarlyDataReplayStoreFile`
- `TSSLConfig.ServerEarlyDataReplayStoreDirectory`

`file` 和 `directory` 是 mutually exclusive opt-ins，不能同时配置。

Builder 示例：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslFreePascal)
  .WithTLS13
  .WithVerifyNone
  .WithCertificate('tests/certificate/test_certs/signer_cert.pem')
  .WithPrivateKey('tests/certificate/test_certs/signer_key.pem')
  .WithSessionCache(True)
  .WithSessionTimeout(7200)
  .WithServerEarlyDataPolicy(sslEarlyDataServerAccept)
  .WithServerMaxEarlyDataSize(8)
  .WithServerEarlyDataReplayStoreDirectory('/var/lib/fafafa/replay-store')
  .BuildServer;
```

Config/factory 示例：

```pascal
var
  LConfig: TSSLConfig;
begin
  LConfig := CreateDefaultConfig(sslCtxServer);
  LConfig.LibraryType := sslFreePascal;
  LConfig.ContextType := sslCtxServer;
  LConfig.PreferredVersion := sslProtocolTLS13;
  LConfig.ProtocolVersions := [sslProtocolTLS13];
  LConfig.VerifyMode := [];
  LConfig.CertificateFile := 'tests/certificate/test_certs/signer_cert.pem';
  LConfig.PrivateKeyFile := 'tests/certificate/test_certs/signer_key.pem';
  LConfig.SessionCacheSize := 8;
  LConfig.SessionTimeout := 7200;
  Include(LConfig.Options, ssoEnableSessionCache);
  LConfig.ServerEarlyDataPolicy := sslEarlyDataServerAccept;
  LConfig.ServerMaxEarlyDataSize := 8;
  LConfig.ServerEarlyDataReplayStoreFile := '/var/lib/fafafa/replay-store.bin';

  Ctx := TSSLFactory.CreateContext(LConfig);
end;
```

这条 opt-in 只解决单机可控的 replay-store truth，不代表默认路径已经持久化，也不代表已经进入 distributed readiness。

## 快速开始

### 安装要求

```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev fpc

# macOS
brew install fpc openssl@3

# Windows (WinSSL 零依赖，OpenSSL 可选)
choco install freepascal

# 验证
openssl version  # 应显示 1.1.1+ 或 3.0+
fpc -i           # 应显示 3.2.0+
```

### 30 秒示例（v1.3.0 自动选择）

```pascal
program HelloTLS;

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  YourSocket: THandle;
begin
  // 1) 自动选择最佳后端（安全优先）
  Ctx := TSSLContextBuilder.Create
    .WithSecurityFirst        // v1.3.0: 智能选择
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 2) 建立 TLS
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(YourSocket, 'www.google.com');
  try
    WriteLn('TLS 连接成功');
    WriteLn('协议: ', Ord(Stream.Connection.GetProtocolVersion));
  finally
    Stream.Free;
  end;
end.
```

### 30 秒示例

```pascal
program HelloTLS;

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  YourSocket: THandle;
begin
  // 1) 创建客户端上下文（验证对端 + 自动加载系统根证书）
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 2) 建立 TLS（ServerName 是连接级别配置：SNI + hostname verification）
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(YourSocket, 'www.google.com');
  try
    WriteLn('TLS 连接成功');
    WriteLn('协议: ', Ord(Stream.Connection.GetProtocolVersion));
    WriteLn('密码套件: ', Stream.Connection.GetCipherName);
  finally
    Stream.Free;
  end;
end.
```

### 编译运行

```bash
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib your_app.pas -o./bin/your_app
./bin/your_app
```

## 文档

| 文档                                                                                               | 描述                                          |
| -------------------------------------------------------------------------------------------------- | --------------------------------------------- |
| [快速入门](docs/guides/5_MINUTE_QUICKSTART.md)                                                     | 5 分钟快速入门                                |
| [后端选择指南](docs/BACKEND_SELECTION_GUIDE.md)                                                    | 自动后端选择完整指南（v1.3.0）                |
| [当前路线图](docs/ROADMAP.md)                                                                      | 当前稳定 roadmap / status 入口                |
| [Release Plan](docs/plans/2026-05-12-release-v1.5.0-formalization.md)                             | 当前 release-control 执行计划                 |
| [Release Readiness](docs/test_reports/RELEASE_READINESS_V1.5.0.md)                                 | 当前 v1.5.0 release-control 收口结论          |
| [用户指南](docs/guides/USER_GUIDE.md)                                                              | 完整用户指南                                  |
| [API 参考](docs/reference/API_REFERENCE.md)                                                        | API 参考文档                                  |
| [OCSP 指南](docs/guides/OCSP_USAGE_GUIDE.md)                                                       | FreePascal stapling + OpenSSL 在线 OCSP       |
| [CT 指南](docs/guides/CT_IMPLEMENTATION_GUIDE.md)                                                  | FreePascal runtime CT + 底层 validator        |
| [WinSSL 指南](docs/guides/WINSSL_USER_GUIDE.md)                                                    | WinSSL 后端用户指南                           |
| [示例程序](examples/)                                                                              | 57 个示例程序                                 |
| [FAQ](docs/guides/FAQ.md)                                                                          | 常见问题解答                                  |
| [部署指南](docs/guides/DEPLOYMENT_GUIDE.md)                                                        | 生产部署指南                                  |
| [Wave C Closeout Status](docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md)                   | Wave C closeout / approval 参考入口           |
| [Wave C Current Chain](docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md) | Wave C current-chain 历史/审批对照入口        |

## 核心 API

### 基础加密

```pascal
uses fafafa.ssl.crypto.utils;

// SHA-256
Hash := TCryptoUtils.SHA256('Hello World');
HexStr := TCryptoUtils.SHA256Hex('Hello World');

// AES-256-GCM 加密
Ciphertext := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
Plaintext := TCryptoUtils.AES_GCM_Decrypt(Ciphertext, Key, IV);

// 安全随机数
RandomBytes := TCryptoUtils.SecureRandom(32);
AESKey := TCryptoUtils.GenerateKey(256);
```

### TLS 连接

下面这段 `TLS 连接` 代码块展示的是底层 core surface reference；普通新代码优先沿用上面的 `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream` 快速路径。

```pascal
uses fafafa.ssl;

Ctx := TSSLFactory.CreateContext(sslCtxClient);
Ctx.SetCipherList('TLS_AES_256_GCM_SHA384');  // 可选

Conn := Ctx.CreateConnection(Socket);
(Conn as ISSLClientConnection).SetServerName('api.example.com');
Conn.Connect;

// 发送/接收数据
Conn.Write(Data^, Length(Data));
BytesRead := Conn.Read(Buffer^, BufferSize);
```

### 证书操作

```pascal
uses fafafa.ssl.cert.builder;

var
  KeyPair: IKeyPairWithCertificate;
begin
  KeyPair := TCertificateBuilder.Create
    .WithCommonName('localhost')
    .WithOrganization('My Company')
    .ValidFor(365)
    .WithRSAKey(2048)
    .AsServerCert
    .AddSubjectAltName('DNS:localhost')
    .SelfSigned;

  KeyPair.SaveToFiles('server.crt', 'server.key');
end;
```

### PKCS#11 硬件安全模块

```pascal
uses fafafa.ssl.pkcs11.engine;

var
  Engine: TPKCS11Engine;
begin
  Engine := TPKCS11Engine.Create('/usr/lib/libsofthsm2.so');
  try
    Engine.Login('1234');  // PIN 码
    PrivKey := Engine.LoadPrivateKey('pkcs11:token=MyToken;object=MyKey');
    // 使用私钥签名...
  finally
    Engine.Free;
  end;
end;
```

## 测试与 CI/CD

### 运行测试

```bash
# Linux 核心编译门禁
python3 scripts/compile_all_modules.py

# 本地最小门禁（推荐，输出隔离到 ./tmp）
bash scripts/run_minimal_ci_gate.sh --fast-local

# P2 核心模块回归
bash scripts/run_all_module_tests.sh --fast-local --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT

# Phase 2 性能入口探测（dry-run，不产生实际报告）
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local

# 代码风格门禁
python3 scripts/check_code_style.py src
```

### CI 暂缓时（Local-first 守护）

```bash
# 一键执行本地守护门禁（B123 + B124）
bash scripts/run_wave_c_local_first_guard_bundle.sh --strict

# 汇总最近守护趋势（B126）
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

更多细节：

- 默认导航：先看 `docs/ROADMAP.md`、`docs/plans/2026-05-12-release-v1.5.0-formalization.md`、`docs/test_reports/RELEASE_READINESS_V1.5.0.md`。
- Wave C closeout / 审批参考：`docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`、`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`。
- 历史手册仅作参考：`docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md`、`docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`。

### 测试覆盖

| 测试类型 | 数量   | 状态      |
| -------- | ------ | --------- |
| 源文件   | 160    | -         |
| 测试文件 | 415    | 100% 通过 |
| 示例程序 | 57     | -         |
| 代码行数 | 95,143 | -         |

## 性能指标

### 加密操作性能

| 操作             | 数据大小 | 吞吐量 (ops/s) |
| ---------------- | -------- | -------------- |
| SHA-256          | 1KB      | 200,000        |
| SHA-512          | 1KB      | 250,000        |
| AES-256-GCM 加密 | 1KB      | 83,333         |
| AES-256-GCM 解密 | 1KB      | 333,333        |
| 安全随机数生成   | 1KB      | 111,111        |

### 并发优化性能

| 组件                 | 性能           |
| -------------------- | -------------- |
| TLockFreeRingBuffer  | 16M+ ops/s     |
| TShardedSessionCache | 8-16x 并发提升 |
| TBufferPool          | 100% 命中率    |

## 安全特性

- **密码学强度**: AES-256, SHA-256/512, RSA-2048+
- **安全默认值**: TLS 1.2+, 强密码套件
- **证书验证**: 自动系统 CA 加载，支持证书钉扎
- **密钥派生**: PBKDF2 100,000 次迭代
- **防时序攻击**: 恒定时间比较
- **内存安全**: 及时清零敏感数据

## 架构

```
fafafa.ssl/
├── src/                      # 核心源代码 (160 文件)
│   ├── fafafa.ssl.factory.pas   # 工厂模式入口
│   ├── fafafa.ssl.base.pas      # 基础接口定义
│   ├── fafafa.ssl.openssl/      # OpenSSL 后端
│   ├── fafafa.ssl.winssl/       # WinSSL 后端
│   ├── fafafa.ssl.pkcs11/       # PKCS#11 支持
│   ├── fafafa.ssl.dane.pas      # DANE/DNSSEC 支持
│   ├── fafafa.ssl.crypto.*      # 加密工具
│   └── fafafa.ssl.cert.*        # 证书管理
├── examples/                 # 57 个示例程序
├── tests/                    # 415 个测试文件
└── docs/                     # 完整文档
```

## 贡献指南

1. Fork 本仓库
2. 创建特性分支 (`git checkout -b feature/AmazingFeature`)
3. 提交更改 (`git commit -m 'Add AmazingFeature'`)
4. 推送到分支 (`git push origin feature/AmazingFeature`)
5. 开启 Pull Request

详见 [CONTRIBUTING.md](CONTRIBUTING.md) 和 [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)

## 版本历史

- **v1.5.0** (2026-05-12) - 正式发布候选
  - 接口版本 `10500`
  - 移除 deprecated helper API，统一迁移到 `TSSLFactory.*`
  - 安全加固、跨后端契约测试、FreePascal TLS 1.3 completeness gate 收口
  - WinSSL 稳定版发布仍以真实 Windows runtime proof 为硬门槛

- **v1.3.0** (2026-02-05) - 智能化版本
  - 自动后端选择（智能评分算法）
  - 能力矩阵差异对比（多格式报告）
  - Builder 链式 API 扩展
  - 已取消：YAML 序列化、运行时协商（伪需求）

- **v1.2.0** (2026-02-05) - 能力矩阵扩展版本
  - 40+ 字段能力矩阵
  - 14 个辅助查询函数
  - 能力矩阵缓存（10,000x+ 性能提升）
  - JSON/XML 序列化
  - Web 可视化工具

- **v1.1.1** (2026-02-05) - 易用性提升
  - 统一原生句柄辅助
  - 泛型类型安全 API

- **v1.1.0** (2026-02-05) - 架构重构版本
  - GetNativeHandle 抽象重构

- **v1.0.0** (2026-02-05) - 正式发布版本
  - PKCS#11 支持（PIN 回调、私钥加载）
  - DANE/DNSSEC 支持（ldns 可选）
  - 无锁并发优化（RingBuffer, BufferPool, ShardedCache）
  - 完整测试覆盖（415 测试, 100% 通过）
  - TODO 清零

- **v0.8.0** (2025-10-24) - WinSSL 企业功能
- **v0.7.0** (2025-10-01) - 核心架构完成
- **v0.6.0** (2025-09-15) - 基础 SSL/TLS 支持

## 许可证

本项目采用 MIT 许可证 - 详见 [LICENSE](LICENSE) 文件

## 致谢

- [OpenSSL Project](https://www.openssl.org/) - 加密库
- [Free Pascal](https://www.freepascal.org/) - 编译器
- [ldns](https://www.nlnetlabs.nl/projects/ldns/) - DNS 库

## 支持

- **文档**: 查看 `docs/` 目录
- **示例**: 查看 `examples/` 目录
- **问题**: 提交 GitHub Issue
- **讨论**: GitHub Discussions

---

**Ready for Production!** - Enterprise-grade SSL/TLS Library for FreePascal

Made with care by the fafafa.ssl team
