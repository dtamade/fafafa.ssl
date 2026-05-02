# fafafa.ssl 文档中心

> **版本**: rolling
> **更新**: 2026-03-27

fafafa.ssl 是 Free Pascal 的高性能 SSL/TLS 库，支持 OpenSSL、WinSSL、FreePascal，以及可选的 MbedTLS / WolfSSL 后端。

---

## 当前工程入口（Wave C canonical chain）

如果你是在继续当前工程的验证/收口，而不是单纯查 API，默认先走这条链路：

- 当前路线图：[ROADMAP.md](ROADMAP.md)
- 默认入口：`test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`
- 当前链路：`test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- 当前构建命令：`python3 scripts/compile_all_modules.py`
- 当前最小门禁：`bash scripts/run_minimal_ci_gate.sh --fast-local`
- 当前 FreePascal TLS 1.3 focused gate：`bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`（TLS 1.3 + validation runtime focused lanes）
- Phase 2 入口探测：`bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
- 历史参考：`test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md`、`test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`

---

## 快速开始

```pascal
uses fafafa.ssl.factory, fafafa.ssl.base;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  // 创建客户端上下文
  Ctx := TSSLFactory.CreateContext(sslClient);

  // 创建连接（包装你的 socket）
  Conn := Ctx.CreateConnection(YourSocket);
  ClientConn := Conn as ISSLClientConnection;
  ClientConn.SetServerName('example.com');  // SNI + hostname verification 是连接级配置

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
└── archive/               # 历史文档
    └── (项目报告、阶段总结等)
```

---

## 核心文档

| 文档                                                                                                                                           | 说明                                                 |
| ---------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| [README.md](README.md)                                                                                                                         | 文档中心与当前工程入口                               |
| [ROADMAP.md](ROADMAP.md)                                                                                                                       | 当前稳定 roadmap / status 入口                       |
| [DOCUMENTATION_INDEX.md](DOCUMENTATION_INDEX.md)                                                                                               | 文档全索引与 current-chain / historical 分区         |
| [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md)                                                                                                   | **框架集成指南** - 如何集成到其他网络框架            |
| [guides/QUICKSTART.md](guides/QUICKSTART.md)                                                                                                   | 5 分钟快速上手                                       |
| [guides/USER_GUIDE.md](guides/USER_GUIDE.md)                                                                                                   | 完整用户指南                                         |
| [reference/API_REFERENCE.md](reference/API_REFERENCE.md)                                                                                       | API 参考手册                                         |
| [reference/INTERFACE_DESIGN_V2.md](reference/INTERFACE_DESIGN_V2.md)                                                                           | 接口设计文档                                         |
| [test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md](test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md)                                         | Wave C 当前 canonical 收口状态                       |
| [test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md](test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md) | Wave C 当前 local-first / pre-CI / submission 总入口 |

---

## 后端支持

| 后端       | 平台    | 文档                                                                             |
| ---------- | ------- | -------------------------------------------------------------------------------- |
| OpenSSL    | 全平台  | [reference/OPENSSL_MODULES.md](reference/OPENSSL_MODULES.md)                     |
| WinSSL     | Windows | [guides/WINSSL_USER_GUIDE.md](guides/WINSSL_USER_GUIDE.md)                       |
| FreePascal | 全平台  | [ROADMAP.md](ROADMAP.md), [reference/ARCHITECTURE.md](reference/ARCHITECTURE.md) |
| MbedTLS    | 全平台  | [guides/MBEDTLS_USER_GUIDE.md](guides/MBEDTLS_USER_GUIDE.md)                     |
| WolfSSL    | 全平台  | 基础支持                                                                         |

---

## 高级功能

| 功能        | 说明                                | 文档                                                                                                                 |
| ----------- | ----------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| PKCS#11     | 硬件安全模块 (HSM) 支持             | [guides/PKCS11_USER_GUIDE.md](guides/PKCS11_USER_GUIDE.md)                                                           |
| DANE/DNSSEC | DNS-Based 证书验证                  | [guides/DANE_USER_GUIDE.md](guides/DANE_USER_GUIDE.md)                                                               |
| OCSP        | Stapled response cryptographic verification + 在线 OCSP 工作流 | [guides/OCSP_USAGE_GUIDE.md](guides/OCSP_USAGE_GUIDE.md)                                            |
| CT          | 证书透明度验证（TLS / embedded / OCSP-delivered SCT） | [guides/CT_IMPLEMENTATION_GUIDE.md](guides/CT_IMPLEMENTATION_GUIDE.md)                        |
| PKCS#7/12   | 证书打包和签名                      | [guides/PKCS7_USER_GUIDE.md](guides/PKCS7_USER_GUIDE.md), [guides/PKCS12_USER_GUIDE.md](guides/PKCS12_USER_GUIDE.md) |

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

**许可证**: MIT
