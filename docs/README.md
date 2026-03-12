# fafafa.ssl 文档中心

> **版本**: v1.0.0
> **更新**: 2026-03-09

fafafa.ssl 是 Free Pascal 的高性能 SSL/TLS 库，支持 OpenSSL、WinSSL、MbedTLS、WolfSSL 多后端。

---

## 快速开始

```pascal
uses fafafa.ssl.factory, fafafa.ssl.base;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // 创建客户端上下文
  Ctx := TSSLFactory.CreateContext(sslCtxClient);

  // 创建连接（包装你的 socket）
  Conn := Ctx.CreateConnection(YourSocket);

  // SSL 握手
  if Conn.Connect then
  begin
    Conn.Write(Data, Length(Data));
    BytesRead := Conn.Read(Buffer, SizeOf(Buffer));
  end;

  Conn.Shutdown;
end;
```

---


## 当前语义真相（2026-03-09）

- `TSSLContextBuilder` 在 `BuildClient` / `BuildServer` 路径上会先 resolve 一次 concrete backend，再把同一个 backend 复用于 context 与 system-roots store；builder 不再让 context/store 各自重新 autodetect。
- `ISSLLibrary.SetDefaultConfig(...)` 是 library-scope 入口，只应用稳定默认值；证书、私钥、CA 文件/路径属于 request/context scope，请走 `TSSLFactory.CreateContext(const AConfig)` 或 builder。
- `LibraryType` / `ContextType` 在 library defaults 上会 normalize 回 backend-owned baseline，因此 `GetDefaultConfig(...)` 反映的是 backend 当前真相，而不是调用方临时噪音。
- `LogLevel` / `LogCallback` 属于 library scope；`BufferSize` / `HandshakeTimeout` 这类当前未接到 context 创建路径的 runtime-only 字段，不应被当作默认配置入口。
- `UsePKCS11(...)` 只替代私钥来源；server 证书仍需通过 `WithCertificate` 或 `WithCertificatePEM` 提供。若 `pkcs11_uri` 与本地私钥材料并存，当前合同是 `PKCS#11` 优先。
- 当前主线汇总见 `plans/2026-03-current-summary.md`，设计细节见 `reference/ARCHITECTURE.md`。

## 文档结构

```
docs/
├── README.md              # 本文件
├── INTEGRATION_GUIDE.md   # 框架集成指南 ⭐
├── PLATFORM_SUPPORT.md    # 平台支持
├── DEPENDENCIES.md        # 依赖说明
├── RELEASE_NOTES.md       # 发布说明
│
├── guides/                # 用户指南
│   ├── GETTING_STARTED.md
│   ├── USER_GUIDE.md
│   ├── QUICKSTART.md
│   ├── TROUBLESHOOTING.md
│   ├── FAQ.md
│   └── ...
│
├── reference/             # API 参考
│   ├── API_REFERENCE.md
│   ├── INTERFACE_DESIGN_V2.md
│   ├── ARCHITECTURE.md
│   └── ...
│
├── PLANS_CURRENT_INDEX.md # 当前执行/治理入口
├── plans/                 # 历史执行计划（记录用途）
│   └── README.md          # 历史目录边界说明
├── test_reports/          # 历史验证报告（记录用途）
│   └── README.md          # 历史目录边界说明
└── archive/               # 历史归档文档（记录用途）
```

---

## 核心文档

| 文档 | 说明 |
|------|------|
| [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md) | **框架集成指南** - 如何集成到其他网络框架 |
| [guides/QUICKSTART.md](guides/QUICKSTART.md) | 5 分钟快速上手 |
| [guides/USER_GUIDE.md](guides/USER_GUIDE.md) | 完整用户指南 |
| [reference/API_REFERENCE.md](reference/API_REFERENCE.md) | API 参考手册 |
| [reference/API_CONTRACT_CURRENT_INDEX.md](reference/API_CONTRACT_CURRENT_INDEX.md) | 当前 API contract 入口 |
| [reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md](reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md) | pure Pascal client M1 清单 |
| [reference/API_ENTRYPOINT_GOVERNANCE.md](reference/API_ENTRYPOINT_GOVERNANCE.md) | 入口治理文档 |
| [reference/API_ERROR_MODEL.md](reference/API_ERROR_MODEL.md) | 当前错误模型 |
| [reference/API_CAPABILITY_STRATEGY.md](reference/API_CAPABILITY_STRATEGY.md) | capability / fallback 策略 |
| [testing/CURRENT_HEALTH.md](testing/CURRENT_HEALTH.md) | 当前仓库健康状态与最短验证路径 |
| [PLANS_CURRENT_INDEX.md](PLANS_CURRENT_INDEX.md) | 当前执行/治理入口与最近高信号计划索引 |
| [testing/RUNTIME_CONTRACT_CLEANUP_PR_BODY_2026-03-07.md](testing/RUNTIME_CONTRACT_CLEANUP_PR_BODY_2026-03-07.md) | Runtime 合同清理的 PR 正文模板 |
| [test_reports/REPO_HYGIENE_REMEDIATION_SUMMARY_2026-03-06.md](test_reports/REPO_HYGIENE_REMEDIATION_SUMMARY_2026-03-06.md) | 最近一轮仓库卫生整改汇总 |
| [test_reports/REPO_HYGIENE_HANDOFF_SUMMARY_2026-03-06.md](test_reports/REPO_HYGIENE_HANDOFF_SUMMARY_2026-03-06.md) | 最近一轮仓库卫生整改交接清单 |
| [reference/INTERFACE_DESIGN_V2.md](reference/INTERFACE_DESIGN_V2.md) | 接口设计文档 |
| [test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md](test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md) | Wave C local-first 一页执行手册 |
| [test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md](test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md) | Wave C local-first 故障速查 |

---

## 后端支持

| 后端 | 平台 | 文档 |
|------|------|------|
| OpenSSL | 全平台 | [reference/OPENSSL_MODULES.md](reference/OPENSSL_MODULES.md) |
| WinSSL | Windows | [guides/WINSSL_USER_GUIDE.md](guides/WINSSL_USER_GUIDE.md) |
| MbedTLS | 全平台 | [guides/MBEDTLS_USER_GUIDE.md](guides/MBEDTLS_USER_GUIDE.md) |
| WolfSSL | 全平台 | 基础支持 |

---

## 高级功能

| 功能 | 说明 | 文档 |
|------|------|------|
| PKCS#11 | 硬件安全模块 (HSM) 支持 | [guides/PKCS11_USER_GUIDE.md](guides/PKCS11_USER_GUIDE.md) |
| DANE/DNSSEC | DNS-Based 证书验证 | [guides/DANE_USER_GUIDE.md](guides/DANE_USER_GUIDE.md) |
| CT | 证书透明度验证 | [guides/CT_IMPLEMENTATION_GUIDE.md](guides/CT_IMPLEMENTATION_GUIDE.md) |
| PKCS#7/12 | 证书打包和签名 | [guides/PKCS7_USER_GUIDE.md](guides/PKCS7_USER_GUIDE.md), [guides/PKCS12_USER_GUIDE.md](guides/PKCS12_USER_GUIDE.md) |

---

## ISSLConnection 核心接口

框架集成只需关注这些方法：

```pascal
ISSLConnection = interface
  // 连接控制
  function Connect: Boolean;
  function Accept: Boolean;
  function Shutdown: Boolean;

  // 数据传输
  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;

  // 非阻塞支持
  function WantRead: Boolean;   // SSL 需要读取？
  function WantWrite: Boolean;  // SSL 需要写入？
  function GetError(ARet: Integer): TSSLErrorCode;

  // 状态查询
  function IsConnected: Boolean;
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
end;
```

详见 [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md)

---

## 获取帮助

- **Issues**: https://github.com/dtamade/fafafa.ssl/issues
- **Discussions**: https://github.com/dtamade/fafafa.ssl/discussions

---

## 文档治理

- 文档噪声治理规则：`docs/DOCS_NOISE_GOVERNANCE.md`
- active 范围扫描默认排除：`docs/archive/**`、`docs/plans/**`、`docs/test_reports/**`

---

**许可证**: MIT
