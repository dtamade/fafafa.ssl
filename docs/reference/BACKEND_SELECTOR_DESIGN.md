# 后端选择器设计文档

> **Batch**: B68
> **Status**: draft
> **Created**: 2026-02-07

## 概述

后端选择器（Backend Selector）是 fafafa.ssl 的核心组件，负责根据运行环境、用户需求和功能要求自动选择最合适的 TLS 后端。

## 设计目标

1. **智能选择**: 根据平台和需求自动选择最佳后端
2. **透明回退**: 首选后端不可用时自动回退
3. **可解释性**: 能够说明选择原因
4. **可配置性**: 允许用户覆盖默认选择

## 支持的后端

| 后端 | 标识符 | 平台 | 优先级 |
|------|--------|------|--------|
| OpenSSL | `sslOpenSSL` | 全平台 | 默认 |
| WinSSL | `sslWinSSL` | Windows | Windows 默认 |
| MbedTLS | `sslMbedTLS` | 全平台 | 备选 |
| WolfSSL | `sslWolfSSL` | 全平台 | 备选 |

## 选择算法

### 默认选择流程

```
┌─────────────────────────────────────────┐
│           开始选择后端                   │
└─────────────────┬───────────────────────┘
                  │
                  v
┌─────────────────────────────────────────┐
│         用户是否指定后端？               │
└─────────────────┬───────────────────────┘
                  │
        ┌─────────┴─────────┐
        │ 是                │ 否
        v                   v
┌───────────────┐   ┌───────────────────────┐
│ 使用指定后端  │   │ 检测运行平台          │
└───────────────┘   └───────────┬───────────┘
                                │
                    ┌───────────┴───────────┐
                    │ Windows               │ 其他
                    v                       v
            ┌───────────────┐       ┌───────────────┐
            │ 尝试 WinSSL   │       │ 尝试 OpenSSL  │
            └───────┬───────┘       └───────┬───────┘
                    │                       │
                    v                       v
            ┌───────────────┐       ┌───────────────┐
            │ 可用？        │       │ 可用？        │
            └───────┬───────┘       └───────┬───────┘
                    │                       │
            ┌───────┴───────┐       ┌───────┴───────┐
            │ 是    │ 否    │       │ 是    │ 否    │
            v       v       v       v       v       v
        ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────────────┐
        │使用 │ │回退 │ │使用 │ │回退到│ │尝试 MbedTLS │
        │WinSSL│ │OpenSSL│ │OpenSSL│ │MbedTLS│ │或报错      │
        └─────┘ └─────┘ └─────┘ └─────┘ └─────────────┘
```

### 功能需求匹配

```pascal
type
  TBackendRequirement = (
    brTLS13,           // 需要 TLS 1.3
    brOCSPStapling,    // 需要 OCSP Stapling
    brSessionTicket,   // 需要 Session Ticket
    brClientCert,      // 需要客户端证书
    brPSK,             // 需要预共享密钥
    brEd25519,         // 需要 Ed25519
    brFIPS,            // 需要 FIPS 模式
    brSystemCertStore  // 需要系统证书存储
  );
  TBackendRequirements = set of TBackendRequirement;
```

### 能力矩阵

| 需求 | OpenSSL | WinSSL | MbedTLS |
|------|---------|--------|---------|
| TLS 1.3 | ✅ | ✅* | ✅ |
| OCSP Stapling | ✅ | ❌ | ❌ |
| Session Ticket | ✅ | ⚠️ | ✅ |
| 客户端证书 | ✅ | ✅ | ✅ |
| PSK | ✅ | ❌ | ✅ |
| Ed25519 | ✅ | ❌ | ⚠️ |
| FIPS | ✅ | ✅ | ❌ |
| 系统证书存储 | ⚠️ | ✅ | ⚠️ |

*需要 Windows 10 1903+

## API 设计

### 自动选择

```pascal
// 自动选择最佳后端
Ctx := TSSLContextBuilder.Create
  .WithAutoBackend  // 自动选择
  .BuildClient;
```

### 显式指定

```pascal
// 显式指定后端
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslOpenSSL)  // 或 sslWinSSL, sslMbedTLS
  .BuildClient;
```

### 带需求的选择

```pascal
// 根据需求选择
Ctx := TSSLContextBuilder.Create
  .WithRequirements([brTLS13, brOCSPStapling])
  .BuildClient;
```

### 带回退的选择

```pascal
// 指定首选和回退
Ctx := TSSLContextBuilder.Create
  .WithPreferredBackend(sslOpenSSL)
  .WithFallbackBackend(sslMbedTLS)
  .BuildClient;
```

### 查询选择原因

```pascal
var
  Selector: TBackendSelector;
  Result: TBackendSelectionResult;
begin
  Selector := TBackendSelector.Create;
  Result := Selector.Select([brTLS13]);

  WriteLn('Selected: ', Result.Backend);
  WriteLn('Reason: ', Result.Reason);
  WriteLn('Alternatives: ', Result.Alternatives);
end;
```

## 选择结果结构

```pascal
type
  TBackendSelectionResult = record
    Backend: TSSLBackend;           // 选中的后端
    Reason: string;                 // 选择原因
    Alternatives: array of TSSLBackend;  // 可用的替代方案
    UnsupportedFeatures: TBackendRequirements;  // 不支持的功能
    Warnings: TStringList;          // 警告信息
  end;
```

## 选择原因示例

| 场景 | 选择 | 原因 |
|------|------|------|
| Windows + 无特殊需求 | WinSSL | "Windows 平台默认使用 WinSSL，无需额外依赖" |
| Linux + 无特殊需求 | OpenSSL | "Linux 平台默认使用 OpenSSL" |
| 需要 PSK | OpenSSL/MbedTLS | "WinSSL 不支持 PSK，回退到 OpenSSL" |
| 需要 Ed25519 | OpenSSL | "仅 OpenSSL 完整支持 Ed25519" |
| 嵌入式环境 | MbedTLS | "MbedTLS 内存占用最小" |

## 配置文件支持

```ini
; fafafa.ssl.ini
[Backend]
; 首选后端: auto, openssl, winssl, mbedtls
Preferred=auto

; 回退后端
Fallback=mbedtls

; 禁用的后端（逗号分隔）
Disabled=wolfssl

; 强制使用（忽略可用性检查）
Force=false
```

## 环境变量支持

```bash
# 强制使用特定后端
export FAFAFA_SSL_BACKEND=openssl

# 禁用特定后端
export FAFAFA_SSL_DISABLE_BACKEND=winssl

# 启用调试日志
export FAFAFA_SSL_SELECTOR_DEBUG=1
```

## 错误处理

### 无可用后端

```pascal
try
  Ctx := TSSLContextBuilder.Create
    .WithRequirements([brPSK, brEd25519])  // 无后端同时支持
    .BuildClient;
except
  on E: ESSLNoSuitableBackend do
  begin
    WriteLn('无合适后端: ', E.Message);
    WriteLn('需求: ', E.Requirements);
    WriteLn('已尝试: ', E.TriedBackends);
  end;
end;
```

### 部分功能不支持

```pascal
Ctx := TSSLContextBuilder.Create
  .WithRequirements([brTLS13, brOCSPStapling])
  .WithAllowPartialMatch  // 允许部分匹配
  .BuildClient;

// 检查实际支持的功能
if not Ctx.SupportsFeature(brOCSPStapling) then
  WriteLn('警告: OCSP Stapling 不可用');
```

## 实现注意事项

1. **延迟加载**: 仅在需要时加载后端库
2. **缓存结果**: 缓存可用性检测结果
3. **线程安全**: 选择器必须线程安全
4. **版本检测**: 检测后端库版本以确定功能支持

## 相关文档

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `src/fafafa.ssl.factory.pas` - 后端工厂实现
