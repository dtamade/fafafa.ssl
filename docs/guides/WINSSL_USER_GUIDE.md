# WinSSL 用户指南

**WinSSL** 是 fafafa.ssl 的 Windows 原生 SSL/TLS 后端实现，基于 Windows Schannel API，为 Windows 应用提供**零依赖**的 HTTPS 客户端功能。

---

## 📑 文档导航

本用户指南作为 WinSSL 文档的入口点，帮助你快速找到所需信息：

### 🚀 快速开始

- **[WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md)** - 详细的快速入门指南
  - 5 分钟快速开始
  - 完整 HTTPS 示例代码
  - 常见使用场景
  - 故障排除
  - FAQ 常见问题

### 📦 部署

- **[ZERO_DEPENDENCY_DEPLOYMENT.md](../ZERO_DEPENDENCY_DEPLOYMENT.md)** - 零依赖部署指南
  - Windows 零依赖优势
  - 部署步骤和最佳实践
  - 企业场景集成
  - 与 OpenSSL 部署对比

### 🧪 测试和验证

- **[WINSSL_BACKEND_STATUS_REPORT.md](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md)** - 当前后端状态报告
  - TLS 握手测试
  - HTTPS 客户端测试
  - 性能基准
  - 稳定性验证

### 🏗️ 架构和设计

- **[WINSSL_DESIGN.md](../reference/WINSSL_DESIGN.md)** - 架构设计文档
  - Schannel API 绑定
  - 接口实现细节
  - 内部工作原理
  - 技术决策

### 📋 状态与能力报告

- **[WINSSL_BACKEND_STATUS_REPORT.md](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md)** - 当前 WinSSL 后端状态
- **[WINSSL_BACKEND_CAPABILITY_MATRIX.md](../reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md)** - 当前 WinSSL 能力矩阵

---

## 🎯 WinSSL 是什么？

WinSSL 是 **fafafa.ssl** 框架的一个后端实现，使用 Windows 系统内置的 **Schannel (Security Support Provider Interface)** 提供 SSL/TLS 功能。

### 核心优势

#### 1. ✅ 零依赖部署

```
传统 OpenSSL 应用:
MyApp.exe (200 KB)
├── libcrypto-3-x64.dll (5 MB)    ← 需要分发
├── libssl-3-x64.dll (800 KB)     ← 需要分发
└── ca-bundle.crt (200 KB)        ← 需要分发
总计: ~6 MB

WinSSL 应用:
MyApp.exe (210 KB)                 ← 仅需这一个文件
(使用系统 Schannel，内置于 Windows)
总计: 210 KB
```

**优势**:

- 简化部署流程
- 减少应用体积
- 无 DLL 版本冲突
- 无需管理 OpenSSL 更新

#### 2. ✅ 系统集成

**自动使用 Windows 证书存储**:

- 企业根 CA 证书（通过 GPO 分发）
- 用户个人证书（智能卡、USB Token）
- Windows Update 自动更新的根证书

**自动遵守企业安全策略**:

- 密码套件优先级（GPO 配置）
- 禁用的协议版本（GPO 配置）
- FIPS 140-2 合规模式

#### 3. ✅ 自动维护

- **自动安全更新**: Windows Update 自动修补 Schannel 漏洞
- **无需手动升级**: 不需要应用开发者管理 SSL 库版本
- **零停机时间**: 系统更新后无需重新部署应用

#### 4. ✅ 统一 API

WinSSL 与 OpenSSL/WolfSSL/MbedTLS 共享统一的核心 public interface，但具体 published capability 仍以后端的 `ISSLLibrary.GetCapabilities` 为准。
像 password callback、DER/PKCS8 私钥导入、PKCS#12 helper 范围这类能力，仍然属于 backend-specific published truth。

```pascal
// 核心创建路径保持一致，只需改变库类型
{$IFDEF WINDOWS}
Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);   // Windows: 零依赖
{$ELSE}
Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);  // Linux/macOS: OpenSSL
{$ENDIF}

// 或者让工厂自动选择
Lib := TSSLFactory.GetLibraryInstance(sslAutoDetect);

// 后续核心连接/握手路径一致；可选能力仍需按 capability 判断
Ctx := Lib.CreateContext(sslCtxClient);
Conn := Ctx.CreateConnection(Socket);
(Conn as ISSLClientConnection).SetServerName('www.example.com');
Conn.Connect;
```

---

## 🔍 适用场景

### ✅ 推荐使用 WinSSL

**Windows 专有应用**:

- 企业内部管理工具
- Windows 桌面客户端
- Windows 服务程序
- 系统管理脚本

**简单 HTTPS 客户端**:

- REST API 调用
- 文件下载
- 健康检查
- Webhook 通知

**企业环境**:

- 需要集成企业 CA
- 需要遵守安全策略
- 需要 FIPS 140-2 合规
- 集中式证书管理

**零依赖部署需求**:

- 绿色软件（单文件 EXE）
- 便携应用
- 简化安装程序
- 减少应用体积

### ⚠️ 不推荐使用 WinSSL

**跨平台应用**:

- 需要在 Linux/macOS 上运行
- 使用 OpenSSL 后端更合适

**需要完整协议控制**:

- 自定义密码套件
- 传统算法支持（Blowfish 等）
- 精确的协议版本控制

**需要完整跨平台 server/runtime 保证的场景**:

- 需要在 Linux/macOS 上复用同一后端
- 需要 caller-provided server OCSP stapling 等 OpenSSL 优先能力
- 需要把 session resumption / tickets 当成已稳定 runtime-proven 能力

---

## 🚀 5 秒开始

### 最简示例

```pascal
uses
  fafafa.ssl;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);  // 1. 创建 WinSSL 库
  Lib.Initialize;                          // 2. 初始化

  Ctx := Lib.CreateContext(sslCtxClient);  // 3. 创建客户端上下文
  // 4. 创建连接后，在连接级设置 SNI（需要 Socket）
  // Conn := Ctx.CreateConnection(Socket);
  // (Conn as ISSLClientConnection).SetServerName('www.example.com');

  // 5. 创建连接并握手（需要 Socket）
  // Conn.Connect;
end;
```

### 完整 HTTPS GET 示例

完整可运行的代码示例，请参阅：

- **[WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md)** - 详细示例
- **[examples/winssl_https_downloader.pas](../examples/winssl_https_downloader.pas)** - 文件下载器
- **[examples/winssl_rest_client.pas](../examples/winssl_rest_client.pas)** - REST API 客户端
- **[examples/winssl_health_checker.pas](../examples/winssl_health_checker.pas)** - 健康检查工具

---

## 📊 功能状态

### 当前 public surface 与验证边界

| 功能 | 当前状态 | 当前口径 |
| --- | --- | --- |
| **TLS 1.0/1.1/1.2** | ✅ 支持 | 基础 TLS capability 已接通 |
| **TLS 1.3** | ⚠️ 条件支持 | 受 Windows 版本限制 |
| **客户端 TLS 握手** | ✅ 已有当前 Windows runtime baseline | 见状态报告 |
| **服务器 TLS 握手** | ✅ 已实现 | 更细 server runtime 场景继续按状态报告区分 |
| **SNI (服务器名称指示)** | ✅ 支持 | 客户端 / 服务器 surface 已接通 |
| **证书验证（自动模式）** | ✅ 支持 | 基础证书链 / 主机名校验已接通 |
| **证书链验证** | ✅ 支持 | 使用 Windows 证书存储 |
| **主机名验证** | ✅ 支持 | 支持通配符和 SAN |
| **证书文件加载** | ✅ 支持 | PFX / DER / PEM |
| **客户端证书（双向 TLS）** | ✅ 支持 | 见状态报告中的当前证据边界 |
| **ALPN 协议协商** | ✅ 支持 | 受 Windows 版本影响 |
| **会话复用 / Session Ticket** | ⚠️ 实验性 public surface | 当前 dedicated Windows runtime truth: `observed_reuse=false` / `session_configured=true` |
| **数据加密/解密** | ✅ 支持 | 连接读写 surface 已接通 |
| **连接管理** | ✅ 支持 | 客户端 / 服务器 connection surface 已接通 |
| **错误处理** | ✅ 支持 | WinSSL 错误映射已接到公共接口 |
| **Windows 证书存储访问** | ✅ 支持 | 原生集成 |

### 📋 功能详情

**Phase 1: 证书验证（自动模式）**

- ✅ 证书链验证（`CertGetCertificateChain` + `CertVerifyCertificateChainPolicy`）
- ✅ 主机名验证（支持通配符和 SAN）
- ✅ 吊销检查（CRL/OCSP）

**Phase 2: 证书文件加载**

- ✅ LoadCertificate（支持 PFX、DER、PEM 格式）
- ✅ LoadPrivateKey（支持密码保护的 PFX/P12）
- ❌ bare DER / PKCS#8 private key loading（当前 capability 不发布）
- ✅ LoadCAFile（CA 证书内存存储）

**Phase 3: 客户端证书（双向 TLS）**

- ✅ 客户端证书配置
- ✅ 双向 TLS 握手（`ASC_REQ_MUTUAL_AUTH`）

**Phase 4: ALPN 协议协商**

- ✅ ALPN 配置（SetALPNProtocols/GetALPNProtocols）
- ✅ ALPN 缓冲区构建和协商
- ✅ 协商结果获取（优先通过 `ISSLConnectionInfo.GetSelectedALPNProtocol`）

**Phase 5: 服务器 TLS 握手**

- ✅ 服务器上下文配置
- ✅ 服务器握手 public surface（`AcceptSecurityContext`）
- ⚠️ 更细 server runtime 场景继续按状态报告区分

**Phase 6: 会话复用优化**

- ✅ 线程安全的会话对象 / metadata surface
- ✅ FIFO 缓存与会话材料接线
- ⚠️ 当前 dedicated Windows runtime truth 仍是 `observed_reuse=false` / `session_configured=true`
- ⚠️ 因此 session resumption / tickets 继续按实验性 public surface 理解

### ⏳ 验证状态

| 验证类型 | 状态 | 说明 |
| --- | --- | --- |
| **源码 / compile proof** | ✅ 通过 | Linux 侧 source contract 与 compile gate 已闭环 |
| **GitHub Windows runtime baseline** | ✅ 通过 | 当前 broader suite / runtime evidence 已落地 |
| **Session resumption semantics** | ⚠️ 实验性 | 当前 dedicated Windows runtime truth: `observed_reuse=false` / `session_configured=true` |
| **更深 native resumed-handshake 行为** | ⚠️ 继续按状态报告收敛 | 不再写成“100% 完成” |

---

## 🆚 WinSSL vs OpenSSL

### 快速对比

| 特性             | WinSSL         | OpenSSL            |
| ---------------- | -------------- | ------------------ |
| **部署依赖**     | ✅ 零依赖      | ❌ 需要 6+ MB DLL  |
| **Windows 集成** | ✅ 原生支持    | ⚠️ 第三方库        |
| **跨平台**       | ❌ 仅 Windows  | ✅ Win/Linux/macOS |
| **协议支持**     | TLS 1.0-1.3    | SSL 2.0-TLS 1.3    |
| **算法控制**     | ⚠️ 系统决定    | ✅ 完全控制        |
| **证书存储**     | ✅ 系统存储    | 📁 文件/内存       |
| **维护**         | Windows Update | 手动更新           |
| **性能**         | ✅ 硬件加速    | ✅ 优化良好        |
| **企业策略**     | ✅ 自动遵守    | ❌ 手动配置        |
| **FIPS 合规**    | ✅ 内置        | ⚠️ 需要特殊构建    |

### 选择建议

**使用 WinSSL**:

```
✅ Windows 专有应用
✅ 需要零依赖部署
✅ 企业环境（Windows 管理）
✅ 简单 HTTPS 客户端
✅ 需要 FIPS 合规
```

**使用 OpenSSL**:

```
✅ 跨平台应用
✅ 需要完整协议控制
✅ 服务器应用（当前）
✅ 传统算法支持
✅ 自定义密码套件
```

---

## 🔧 配置选项

### 协议版本

```pascal
// 仅 TLS 1.2（最安全，兼容性好）
Ctx.SetProtocolVersions([sslProtocolTLS12]);

// TLS 1.2 和 1.3（推荐）
Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### SNI 主机名

```pascal
// 必须在握手前设置 SNI，且要设置在连接上
Conn := Ctx.CreateConnection(Socket);
(Conn as ISSLClientConnection).SetServerName('www.example.com');
```

### 证书验证

```pascal
// 生产环境：验证证书（推荐）
Ctx.SetVerifyMode([sslVerifyPeer]);

// 测试环境：不验证证书
Ctx.SetVerifyMode([]);

// 双向 TLS：要求客户端证书
Ctx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
```

更多配置选项请参阅 **[WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md#配置选项)**

---

## 🐛 常见问题

### Q: "SSL library initialization failed"

**原因**: Windows 版本太旧或 Schannel 不可用

**解决**:

- 检查 Windows 版本（需要 Vista+）
- 或使用 OpenSSL 后端

### Q: "TLS handshake failed"

**可能原因**:

- 未设置 SNI 主机名
- 协议版本不匹配
- 网络连接问题

**解决**:

- 确保在 `Conn.Connect` 前调用 `(Conn as ISSLClientConnection).SetServerName('...')`
- 检查服务器支持的 TLS 版本
- 验证网络连接正常

### Q: Windows 7 上不支持 TLS 1.3？

**回答**: 是的，TLS 1.3 需要：

- Windows 10 Build 20348+ 或
- Windows 11

Windows 7/8/10 早期版本仅支持 TLS 1.0/1.1/1.2

### Q: 如何从 OpenSSL 迁移？

**回答**: 非常简单，只需修改一行代码：

```pascal
// Before
Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);

// After
Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);

// 其他代码保持不变
```

更多问题请参阅 **[WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md#常见问题-faq)**

---

## 📈 性能

### 测试结果（Phase 2.4）

| 指标         | 测量值      | 评估                 |
| ------------ | ----------- | -------------------- |
| TLS 握手延迟 | 436.94 ms   | 可接受（含网络延迟） |
| 数据传输延迟 | 204.52 ms   | 良好                 |
| 连接建立速率 | 2.41 conn/s | 可接受               |
| 连接稳定性   | 100%        | 优秀 (30/30 成功)    |

**测试环境**: Windows 11 x64, 网络连接到互联网服务器

详细性能说明：**[WINSSL_PERFORMANCE_TUNING.md](../reference/WINSSL_PERFORMANCE_TUNING.md)**

---

## 🎓 学习路径

### 新手路径

1. **阅读快速入门** - [WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md)
2. **运行示例程序** - `examples/winssl_*.pas`
3. **查看测试代码** - `tests/test_winssl_*.pas`
4. **尝试自己的项目**

### 进阶路径

1. **理解架构设计** - [WINSSL_DESIGN.md](../reference/WINSSL_DESIGN.md)
2. **阅读状态报告** - [WINSSL_BACKEND_STATUS_REPORT.md](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md)
3. **查看能力矩阵** - [WINSSL_BACKEND_CAPABILITY_MATRIX.md](../reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md)
4. **探索源代码** - `src/fafafa.ssl.winssl.*.pas`

### 企业用户路径

1. **零依赖部署指南** - [ZERO_DEPENDENCY_DEPLOYMENT.md](../ZERO_DEPENDENCY_DEPLOYMENT.md)
2. **企业场景示例** - `examples/winssl_*.pas`
3. **安全策略集成** - 查看 Windows GPO 配置
4. **FIPS 合规配置** - Windows 安全设置

---

## 🔗 相关链接

### 文档

- [快速入门指南](WINSSL_QUICKSTART.md)
- [零依赖部署指南](../ZERO_DEPENDENCY_DEPLOYMENT.md)
- [后端状态报告](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md)
- [架构设计](../reference/WINSSL_DESIGN.md)

### 示例代码

- [HTTPS 下载器](../examples/winssl_https_downloader.pas)
- [REST API 客户端](../examples/winssl_rest_client.pas)
- [健康检查工具](../examples/winssl_health_checker.pas)

### 测试代码

- [单元测试](../tests/test_winssl_unit_comprehensive.pas)
- [集成测试](../tests/test_winssl_integration_multi.pas)
- [性能测试](../tests/test_winssl_performance.pas)

### 外部资源

- [Microsoft Schannel 文档](https://docs.microsoft.com/en-us/windows/win32/secauthn/secure-channel)
- [TLS 1.2 规范 (RFC 5246)](https://tools.ietf.org/html/rfc5246)
- [TLS 1.3 规范 (RFC 8446)](https://tools.ietf.org/html/rfc8446)

---

## 📞 获取帮助

### 文档

- 阅读 **[WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md)** 的故障排除章节
- 查看 **[WINSSL_BACKEND_STATUS_REPORT.md](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md)** 中的当前已知问题

### 报告问题

提供以下信息：

- Windows 版本（`winver` 命令查看）
- Free Pascal 版本
- 完整错误信息
- 最小可复现示例代码

---

**文档版本**: 2.1
**最后更新**: 2026-05-19
**项目状态**: ✅ WinSSL 零依赖客户端基线已验证；会话复用 / Session Ticket 仍为实验性 public surface
**当前权威入口**: [WinSSL 后端状态报告](../test_reports/WINSSL_BACKEND_STATUS_REPORT.md) · [WinSSL 后端能力矩阵](../reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md)

---

_享受 Windows 零依赖的 HTTPS 开发体验！_ 🚀
