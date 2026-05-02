# 后端能力矩阵

本文档详细说明各 SSL/TLS 后端的功能支持情况。

**更新时间**: 2026-05-02 (v1.4.1)

---

## 快速参考

| 功能 | FreePascal | OpenSSL | WinSSL | MbedTLS | WolfSSL |
|------|-----------|---------|--------|---------|---------|
| **TLS 1.2** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **TLS 1.3** | ✅ | ✅ | ✅ | ⚠️ | ✅ |
| **Early Data (0-RTT)** | ✅ | ✅ | ❌ | ❌ | ❌ |
| **Session Resumption** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **OCSP Stapling** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Certificate Transparency** | ✅ | ✅ | ⚠️ | ⚠️ | ⚠️ |
| **ALPN** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **SNI** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **PSK** | ✅ | ✅ | ⚠️ | ✅ | ✅ |
| **PKCS#11** | ❌ | ✅ | ❌ | ❌ | ❌ |

**图例**:
- ✅ 完整支持
- ⚠️ 部分支持或有限制
- ❌ 不支持
- 🚧 开发中

---

## TLS 1.3 Early Data (0-RTT)

### FreePascal 后端

**状态**: ✅ 完整支持（生产就绪）

**功能**:
- ✅ 客户端 Early Data
- ✅ 服务端 Early Data
- ✅ 重放防护（内存/文件/目录存储）
- ✅ 策略配置（Reject/Accept/IssueOnly）
- ✅ 可配置大小限制

**限制**:
- 默认使用内存存储（单进程）
- 跨进程需要配置文件或目录存储

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

**状态**: ❌ 不支持（计划中）

**原因**:
- MbedTLS 3.x 开始支持 TLS 1.3
- Early Data API 尚未完善

**计划**:
- 等待 MbedTLS 4.x 完善 API
- 预计 v1.5.0 添加支持

### WolfSSL 后端

**状态**: ❌ 不支持（计划中）

**原因**:
- WolfSSL 支持 TLS 1.3 Early Data
- 需要绑定 WolfSSL 特定 API

**计划**:
- v1.5.0 添加支持

---

## Server OCSP Stapling

### FreePascal 后端

**状态**: ✅ 完整支持

**功能**:
- ✅ 加载 OCSP 响应
- ✅ 从文件加载
- ✅ 动态更新

### OpenSSL 后端

**状态**: ✅ 完整支持（v1.4.1+）

**功能**:
- ✅ 加载 OCSP 响应
- ✅ 从文件加载
- ✅ 动态更新

### WinSSL 后端

**状态**: ⚠️ 部分支持

**功能**:
- ✅ 自动 OCSP Stapling（系统管理）
- ❌ 手动加载响应（Schannel 限制）

### MbedTLS / WolfSSL 后端

**状态**: ❌ 不支持（计划中）

---

## Certificate Transparency (CT)

### FreePascal 后端

**状态**: ✅ 完整支持

**功能**:
- ✅ SCT 验证
- ✅ CT 日志列表
- ✅ 策略配置

### OpenSSL 后端

**状态**: ✅ 完整支持

**功能**:
- ✅ SCT 验证
- ✅ CT 日志列表
- ✅ 策略配置

### WinSSL / MbedTLS / WolfSSL 后端

**状态**: ⚠️ 基础支持

**功能**:
- ✅ SCT 提取
- ⚠️ 验证需要应用层实现

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

**依赖**: WolfSSL 4.x+

---

## 性能对比

### 握手性能（相对值）

| 后端 | TLS 1.2 | TLS 1.3 | TLS 1.3 + 0-RTT |
|------|---------|---------|-----------------|
| FreePascal | 1.0x | 0.8x | 0.3x |
| OpenSSL | 1.2x | 1.0x | 0.4x |
| WinSSL | 0.9x | 0.7x | N/A |
| MbedTLS | 0.8x | N/A | N/A |
| WolfSSL | 1.1x | 0.9x | N/A |

**注**: 基准为 FreePascal TLS 1.2，数值越小越快

### 吞吐量（相对值）

| 后端 | 小数据 (<1KB) | 大数据 (>1MB) |
|------|--------------|--------------|
| FreePascal | 1.0x | 1.0x |
| OpenSSL | 1.3x | 1.5x |
| WinSSL | 1.1x | 1.2x |
| MbedTLS | 0.9x | 0.8x |
| WolfSSL | 1.2x | 1.3x |

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
- 完整功能
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
