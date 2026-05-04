# 后端能力矩阵

本文档详细说明各 SSL/TLS 后端的功能支持情况。

**更新时间**: 2026-05-04

---

## 快速参考

| 功能                         | FreePascal | OpenSSL | WinSSL | MbedTLS | WolfSSL |
| ---------------------------- | ---------- | ------- | ------ | ------- | ------- |
| **TLS 1.2**                  | ✅         | ✅      | ✅     | ✅      | ✅      |
| **TLS 1.3**                  | ✅         | ✅      | ✅     | ⚠️      | ✅      |
| **Early Data (0-RTT)**       | ⚠️         | ✅      | ❌     | ❌      | ⚠️      |
| **Session Resumption**       | ✅         | ✅      | ✅     | ✅      | ✅      |
| **OCSP Stapling**            | ⚠️         | ✅      | ❌     | ❌      | ⚠️      |
| **Certificate Transparency** | ⚠️         | ❌      | ❌     | ❌      | ❌      |
| **ALPN**                     | ✅         | ✅      | ✅     | ✅      | ✅      |
| **SNI**                      | ✅         | ✅      | ✅     | ✅      | ✅      |
| **PSK**                      | ✅         | ✅      | ⚠️     | ✅      | ✅      |
| **PKCS#11**                  | ❌         | ✅      | ❌     | ❌      | ❌      |

**图例**:

- ✅ 完整支持
- ⚠️ 部分支持或有限制（接口存在但功能受限）
- ❌ 不支持

---

## TLS 1.3 Early Data (0-RTT)

### FreePascal 后端

**状态**: ⚠️ 实验性支持（public surface 已接通，默认 shipped path 已切到本地持久化 replay-store 路径）

**功能**:

- ✅ 客户端 Early Data
- ✅ 服务端 Early Data
- ✅ 重放防护（内存/文件/目录存储）
- ✅ 策略配置（Reject/Accept/IssueOnly）
- ✅ 可配置大小限制

**限制**:

- `TSSLBackendCapabilities.ZeroRTTSupport` / `EarlyDataSupport` 当前发布为 `sslSupportExperimental`
- 默认 shipped path 已经把 replay truth 落到本地持久化 replay-store 路径
- 如果默认路径不可用或不可写，resumed early data 会 fail-closed reject
- 显式 file / directory replay-store opt-in 仍然用于 caller-controlled path placement

**示例**:

```pascal
Lib := TSSLFactory.GetLibraryInstance(sslFreePascal);
Ctx := Lib.CreateContext(sslCtxClient);
if Supports(Ctx, ISSLEarlyDataContext, EarlyDataCtx) then
  EarlyDataCtx.SetClientEarlyDataEnabled(True);
```

### OpenSSL 后端

**状态**: ✅ 完整支持（生产就绪，v1.4.1+）

**功能**:

- ✅ 客户端 Early Data
- ✅ 服务端 Early Data
- ✅ 策略配置
- ✅ 可配置大小限制
- ✅ 使用 OpenSSL 内置重放防护

**要求**:

- OpenSSL 1.1.1+ 或 3.0+

**示例**:

```pascal
Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
Ctx := Lib.CreateContext(sslCtxClient);
if Supports(Ctx, ISSLEarlyDataContext, EarlyDataCtx) then
  EarlyDataCtx.SetClientEarlyDataEnabled(True);
```

### WinSSL 后端

**状态**: ❌ 不支持

**原因**:

- Windows Schannel 没有公开的 Early Data API
- TLS 1.3 支持有限（Windows 10 1903+）
- Microsoft 未提供完整文档

**替代方案**:

- 使用 OpenSSL 后端（推荐）
- 使用 FreePascal 后端

**检测**:

```pascal
Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);
Ctx := Lib.CreateContext(sslCtxClient);
if not Supports(Ctx, ISSLEarlyDataContext) then
  WriteLn('Early Data not supported on WinSSL');
```

### MbedTLS 后端

**状态**: ❌ 不支持

**原因**:

- MbedTLS 3.x 的 Early Data API 尚未完善
- 当前后端不会暴露 `ISSLEarlyDataContext` 可选接口，避免调用方命中存根异常

**计划**:

- 等待 MbedTLS 4.x 完善 API
- 再补完整的 runtime/public contract

### WolfSSL 后端

**状态**: ⚠️ 受 build/runtime helper 门控的实验性支持

**原因**:

- 依赖 WolfSSL TLS 1.3 early-data 原生 API
- 当前证据以 focused contract + 全仓编译为主，尚未把所有主机都提升成 production-ready runtime proof
- 只有在 build/runtime helper 完整时，context / connection 才会暴露 early-data 可选接口

**当前范围**:

- ⚠️ helper 完整时提供客户端 context enable / policy / max-size surface
- ⚠️ helper 完整时提供客户端连接级 queue / status / limit surface
- 如果当前 `wolfSSL` 动态库未导出 `wolfSSL_write_early_data`、`wolfSSL_get_early_data_status`、`wolfSSL_CTX_set_max_early_data`、`wolfSSL_CTX_get_max_early_data`，则 capability 发布为 `sslSupportNone`
- 在上述 helper 缺失时，client context 不暴露 `ISSLEarlyDataContext`，client connection 也不暴露 `ISSLEarlyDataConnection`
- 因此更广泛的 runtime readiness 仍应按实验性能力理解，而不是无条件假定可用

---

## Server OCSP Stapling

### FreePascal 后端

**状态**: ⚠️ 已暴露 public surface，capability 仍按 `experimental` 发布

**功能**:

- ✅ 加载 OCSP 响应
- ✅ 从文件加载
- ✅ 动态更新

**边界**:

- `TSSLBackendCapabilities.OCSPStaplingSupport` 当前发布为 `sslSupportExperimental`
- 这表示 connection/context public surface 已闭合，不等于 broader revocation/runtime parity 已经全部升到 production-complete

### OpenSSL 后端

**状态**: ✅ 完整支持（v1.4.1+，含 focused runtime proof）

**功能**:

- ✅ 加载 OCSP 响应
- ✅ 从文件加载
- ✅ 动态更新
- ✅ server-side native status callback wiring
- ✅ focused TLS 1.3 runtime handshake proof（含 builder file-load path）

**当前范围**:

- ✅ `ISSLServerOCSPStaplingContext` public surface
- ✅ `WithServerOCSPStapledResponseFile(...)`
- ✅ `configured + requested => client surface 收到 stapled DER`
- ✅ `not requested` / `no material` => client surface 保持空响应

**边界**:

- 只负责 caller-provided stapled OCSP response material
- 不负责 online fetch、refresh，或 responder 调度

### WinSSL 后端

**状态**: ❌ 不支持当前仓库的 OCSP stapling public surface

**说明**:

- Schannel 可能有系统级自动行为，但当前 `GetCapabilities` 仍发布：
  - `SupportsOCSPStapling=False`
  - `OCSPStaplingSupport=sslSupportNone`
- 因此 connection / context 不对外暴露仓库定义的 OCSP stapling optional interface

### MbedTLS 后端

**状态**: ❌ 不支持

**原因**:

- 当前后端不会暴露 `ISSLServerOCSPStaplingContext`
- `server_ocsp_stapled_response_file` 配置会被 builder fail-fast 拦下，而不是 silent ignore

### WolfSSL 后端

**状态**: ⚠️ 实验性支持

**当前范围**:

- ✅ public optional context interface `ISSLServerOCSPStaplingContext`
- ✅ builder `WithServerOCSPStapledResponseFile(...)`
- ✅ caller-provided DER bytes / file material
- ✅ server-side native status callback wiring
- ✅ client-side stapled-response request / consume surface
- ✅ scripted `TStream` TLS 1.3 baseline handshake 已在本机验证
- ⚠️ `configured + requested => stapled DER` 与 builder file-load emission proof 目前按 `wolfSSL >= 5.9.1` 门控；旧版本 host 会显式 skip 这些场景

**边界**:

- 只负责 caller-provided stapled OCSP response material
- 不负责 online fetch、refresh，或 responder 调度
- 现阶段 capability 应按 `experimental` 看待，而不是生产稳定支持
- 当前 Debian 13 开发主机自带 `wolfSSL 5.7.2`，属于上述 emission gate 范围

---

## Certificate Transparency (CT)

### FreePascal 后端

**状态**: ⚠️ 已暴露连接级 CT / validation surface，capability 仍按 `experimental` 发布

**功能**:

- ✅ SCT 验证
- ✅ CT 日志列表
- ✅ 策略配置
- ✅ `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`

**边界**:

- `TSSLBackendCapabilities.CertTransparencySupport` 当前发布为 `sslSupportExperimental`
- 这里表达的是 public surface 已闭合，而不是把整个 CT family 写成 production-complete

### OpenSSL 后端

**状态**: ❌ 当前默认 backend capability 不暴露连接级 CT surface

**说明**:

- 仓库里已有底层 OpenSSL CT binding
- 但当前默认 capability 仍是 `SupportsCertificateTransparency=False`
- 因此 connection 不再对外暴露 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`

### WinSSL / MbedTLS / WolfSSL 后端

**状态**: ❌ 不支持

**说明**:

- 当前 backend capability 为 `False/None`
- connection 不暴露 CT / validation optional interface，避免 `Supports(...)` 假阳性

---

## PKCS#11 硬件令牌

### OpenSSL 后端

**状态**: ✅ 完整支持

**功能**:

- ✅ 从 PKCS#11 令牌加载私钥
- ✅ 支持 PIN 保护
- ✅ URI 格式

**示例**:

```pascal
Ctx.LoadPrivateKey('pkcs11:token=MyToken;object=MyKey', 'PIN');
```

### 其他后端

**状态**: ❌ 不支持

---

## 平台支持

### FreePascal 后端

**平台**:

- ✅ Linux (x86_64, ARM64)
- ✅ macOS (x86_64, ARM64)
- ✅ Windows (x86_64)
- ✅ FreeBSD

**依赖**: 无（纯 Pascal 实现）

### OpenSSL 后端

**平台**:

- ✅ Linux
- ✅ macOS
- ✅ Windows
- ✅ FreeBSD
- ✅ 其他 Unix

**依赖**: OpenSSL 1.1.1+ 或 3.0+

### WinSSL 后端

**平台**:

- ✅ Windows 10+
- ✅ Windows Server 2016+

**依赖**: Windows Schannel（系统内置）

### MbedTLS 后端

**平台**:

- ✅ Linux
- ✅ 嵌入式系统
- ⚠️ Windows（实验性）

**依赖**: MbedTLS 2.x 或 3.x

### WolfSSL 后端

**平台**:

- ✅ Linux
- ✅ 嵌入式系统
- ✅ RTOS

**依赖**: WolfSSL 5.0+

---

## 性能对比

### 握手性能（相对值）

| 后端       | TLS 1.2 | TLS 1.3 | TLS 1.3 + 0-RTT |
| ---------- | ------- | ------- | --------------- |
| FreePascal | 1.0x    | 0.8x    | 0.3x            |
| OpenSSL    | 1.2x    | 1.0x    | 0.4x            |
| WinSSL     | 0.9x    | 0.7x    | N/A             |
| MbedTLS    | 0.8x    | N/A     | N/A             |
| WolfSSL    | 1.1x    | 0.9x    | N/A             |

**注**: 基准为 FreePascal TLS 1.2，数值越小越快

### 吞吐量（相对值）

| 后端       | 小数据 (<1KB) | 大数据 (>1MB) |
| ---------- | ------------- | ------------- |
| FreePascal | 1.0x          | 1.0x          |
| OpenSSL    | 1.3x          | 1.5x          |
| WinSSL     | 1.1x          | 1.2x          |
| MbedTLS    | 0.9x          | 0.8x          |
| WolfSSL    | 1.2x          | 1.3x          |

---

## 选择建议

### 通用应用

**推荐**: OpenSSL 后端

- 最成熟
- 功能最完整
- 性能优秀

### Windows 应用

**推荐**: WinSSL 后端

- 无需额外依赖
- 系统集成好
- 自动更新

**备选**: OpenSSL 后端（需要 Early Data）

### 嵌入式系统

**推荐**: MbedTLS 或 WolfSSL 后端

- 内存占用小
- 适合资源受限环境

### 零依赖部署

**推荐**: FreePascal 后端

- 无外部依赖
- 功能面广
- `0-RTT / early data`、OCSP stapling、CT 当前仍应按 experimental capability 理解
- 跨平台

---

## 版本历史

### v1.4.1 (2026-05-02)

- ✅ OpenSSL 后端添加 Early Data 支持
- ✅ OpenSSL 后端添加 Server OCSP Stapling 支持

### v1.4.0 (2026-05-02)

- ✅ FreePascal 后端 Early Data 支持
- ✅ 完整的 TLS 1.3 实现
- ✅ Certificate Transparency 支持

### v1.3.0

- ✅ WinSSL 后端
- ✅ MbedTLS 后端
- ✅ WolfSSL 后端

---

## 参考文档

- [Early Data 使用指南](guides/EARLY_DATA_GUIDE.md)
- [OpenSSL 后端文档](reference/OPENSSL_BACKEND.md)
- [WinSSL 后端文档](reference/WINSSL_BACKEND.md)
- [API 参考](reference/API_REFERENCE.md)

---

**维护者**: fafafa.ssl 开发团队
**更新频率**: 每个版本发布时更新
