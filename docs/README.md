# fafafa.ssl 文档中心

> **版本**: v1.0.0
> **更新**: 2026-02-05

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
  Ctx := TSSLFactory.CreateContext(sslClient);

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

| 文档 | 说明 |
|------|------|
| [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md) | **框架集成指南** - 如何集成到其他网络框架 |
| [guides/QUICKSTART.md](guides/QUICKSTART.md) | 5 分钟快速上手 |
| [guides/USER_GUIDE.md](guides/USER_GUIDE.md) | 完整用户指南 |
| [reference/API_REFERENCE.md](reference/API_REFERENCE.md) | API 参考手册 |
| [reference/INTERFACE_DESIGN_V2.md](reference/INTERFACE_DESIGN_V2.md) | 接口设计文档 |

---

## 后端支持

| 后端 | 平台 | 文档 |
|------|------|------|
| OpenSSL | 全平台 | [reference/OPENSSL_MODULES.md](reference/OPENSSL_MODULES.md) |
| WinSSL | Windows | [guides/WINSSL_USER_GUIDE.md](guides/WINSSL_USER_GUIDE.md) |
| MbedTLS | 全平台 | [guides/MBEDTLS_USER_GUIDE.md](guides/MBEDTLS_USER_GUIDE.md) |
| WolfSSL | 全平台 | 基础支持 |

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
