# QUIC 协议支持评估报告

> **Batch**: B74
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本报告评估在 fafafa.ssl 中添加 QUIC 协议支持的可行性、技术方案和实施路径。

## QUIC 协议简介

QUIC (Quick UDP Internet Connections) 是由 Google 开发、IETF 标准化的传输层协议，基于 UDP 实现，提供：

- **0-RTT 连接建立**: 比 TLS 1.3 更快的握手
- **多路复用**: 无队头阻塞
- **连接迁移**: 网络切换时保持连接
- **内置加密**: TLS 1.3 集成

## 技术评估

### 方案 1: OpenSSL QUIC API

**OpenSSL 3.2+ 原生 QUIC 支持**

```pascal
// 伪代码示例
Ctx := SSL_CTX_new(OSSL_QUIC_client_method());
SSL_set_alpn_protos(SSL, 'h3', 2);
```

**优点**:
- 与现有 OpenSSL 后端集成
- 官方支持，持续维护
- API 风格一致

**缺点**:
- 需要 OpenSSL 3.2+
- API 仍在演进中
- 文档不完善

**评估**: ⭐⭐⭐⭐ (推荐)

### 方案 2: MsQuic 集成

**Microsoft QUIC 实现**

```pascal
// 伪代码示例
MsQuicOpen2(@MsQuic);
MsQuic.ConfigurationOpen(Registration, @Settings, ...);
```

**优点**:
- 高性能，生产级
- 跨平台（Windows/Linux/macOS）
- 活跃开发

**缺点**:
- 需要额外依赖
- API 与 OpenSSL 不同
- 需要新的绑定层

**评估**: ⭐⭐⭐ (备选)

### 方案 3: quiche (Cloudflare)

**Rust 实现的 QUIC 库**

**优点**:
- 高质量实现
- 良好的 C API

**缺点**:
- 需要 Rust 工具链
- FFI 复杂度高

**评估**: ⭐⭐ (不推荐)

### 方案 4: 纯 Pascal 实现

**从头实现 QUIC 协议**

**优点**:
- 无外部依赖
- 完全控制

**缺点**:
- 工作量巨大
- 安全风险高
- 维护成本高

**评估**: ⭐ (不推荐)

## 推荐方案

### 短期 (2026 Q2-Q3)

**OpenSSL QUIC API 集成**

1. 添加 OpenSSL 3.2+ QUIC API 绑定
2. 扩展 `ISSLContext` 支持 QUIC 模式
3. 实现基本的 QUIC 客户端连接

### 中期 (2026 Q4)

**HTTP/3 支持**

1. 实现 HTTP/3 帧解析
2. 添加 QPACK 头部压缩
3. 集成到 HTTPS 客户端

### 长期 (2027+)

**完整 QUIC 生态**

1. QUIC 服务器支持
2. 连接迁移
3. 0-RTT 数据

## API 设计草案

### QUIC Context

```pascal
type
  IQUICContext = interface(ISSLContext)
    procedure SetQUICTransportParams(const AParams: TQUICTransportParams);
    function GetQUICTransportParams: TQUICTransportParams;
    procedure EnableEarlyData(AEnable: Boolean);
  end;
```

### QUIC Connection

```pascal
type
  IQUICConnection = interface(ISSLConnection)
    function OpenStream(AType: TQUICStreamType): IQUICStream;
    function AcceptStream: IQUICStream;
    procedure Migrate(ANewSocket: TSocket);
    function GetConnectionId: TBytes;
  end;
```

### QUIC Stream

```pascal
type
  IQUICStream = interface
    function GetStreamId: UInt64;
    function Read(var ABuffer; ACount: Integer): Integer;
    function Write(const ABuffer; ACount: Integer): Integer;
    procedure Close;
    function IsClosed: Boolean;
  end;
```

## 依赖要求

| 组件 | 最低版本 | 说明 |
|------|----------|------|
| OpenSSL | 3.2.0 | QUIC API |
| FPC | 3.2.2 | 64位整数支持 |
| UDP Socket | - | 系统原生 |

## 风险评估

| 风险 | 级别 | 缓解措施 |
|------|------|----------|
| OpenSSL QUIC API 变更 | 中 | 版本检测 + 条件编译 |
| 性能问题 | 低 | 基准测试 + 优化 |
| 兼容性问题 | 中 | 广泛测试 |

## 时间线估算

| 阶段 | 工作量 | 时间 |
|------|--------|------|
| API 绑定 | 2 周 | 2026 Q2 |
| 基本客户端 | 3 周 | 2026 Q2 |
| HTTP/3 | 4 周 | 2026 Q3 |
| 服务器支持 | 4 周 | 2026 Q4 |

## 结论

**推荐**: 采用 OpenSSL QUIC API 方案，从 2026 Q2 开始实施。

**优先级**: P3 (低优先级) - 当前 TLS 1.3 已满足大多数需求。

## 相关资源

- [OpenSSL QUIC Design](https://www.openssl.org/docs/manmaster/man7/ossl-guide-quic-introduction.html)
- [RFC 9000 - QUIC](https://datatracker.ietf.org/doc/html/rfc9000)
- [RFC 9001 - QUIC-TLS](https://datatracker.ietf.org/doc/html/rfc9001)
- [MsQuic GitHub](https://github.com/microsoft/msquic)
