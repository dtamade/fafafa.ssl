# 后端选择器设计文档

> **Batch**: B68
> **Status**: draft
> **Created**: 2026-02-07

## 概述

后端选择器（Backend Selector）负责根据运行环境、调用方需求与当前
`GetCapabilities` 真值，在多个 backend 中选择最合适的 TLS 实现。

> 当前公开 selector API 以 `SelectBestBackend(...)` / `SelectBestBackends(...)` 与 `TSSLContextBuilder.WithAutoBackendSelection(...)` 为准。
>
> 当前 source 尚未发布 `TBackendSelector` / `TBackendSelectionResult` / `WithPreferredBackend(...)` / `WithFallbackBackend(...)` / `WithAllowPartialMatch` 这组草案 surface，也没有 dedicated `FAFAFA_SSL_BACKEND` / `FAFAFA_SSL_DISABLE_BACKEND` / `FAFAFA_SSL_SELECTOR_DEBUG` selector entrypoint。

## 设计目标

1. 根据 capability truth 自动筛选 backend
2. 让 builder 入口和函数式入口共享同一套选择逻辑
3. 允许调用方表达协议、功能、性能、兼容性与平台偏好
4. 把“平台潜力”和“当前已发布 capability”明确区分开

## 支持的后端

| 后端 | 标识符 | 平台 | 当前角色 |
|------|--------|------|----------|
| OpenSSL | `sslOpenSSL` | 全平台 | 通用 C-library backend |
| WolfSSL | `sslWolfSSL` | 全平台 | helper/runtime-gated backend |
| MbedTLS | `sslMbedTLS` | 全平台 | 轻量 C-library backend |
| WinSSL | `sslWinSSL` | Windows | OS-native backend |
| FreePascal | `sslFreePascal` | 全平台 | native Pascal backend |

## 选择算法

### 高层流程

1. 枚举当前可用 backend
2. 读取每个 backend 的 `TSSLBackendCapabilities`
3. 用 `TSSLRequirements` 做 minimum-requirement 过滤
4. 按安全、性能、兼容性、平台偏好打分
5. 返回单个最佳候选或排序后的候选列表

### Requirements 模型

当前 source 使用 `TSSLRequirements`，不是旧草案里的 `TBackendRequirement`：

```pascal
var
  Requirements: TSSLRequirements;
begin
  Requirements := CreateDefaultRequirements(optBalanced);
  Requirements.RequiredProtocols := [sslProtocolTLS13];
  Requirements.RequiredFeatures := [sslFeatOCSPStapling];
  Requirements.PlatformPreferences.PreferOSNative := True;
end;
```

### capability 字段映射

- `sslProtocolTLS13` 通过 `SupportsTLS13` 判定
- `sslFeatSNI` / `sslFeatALPN` / `sslFeatSessionCache` /
  `sslFeatSessionTickets` / `sslFeatOCSPStapling` /
  `sslFeatCertificateTransparency`
  通过相应 `*Support <> sslSupportNone` 判定
- `PreferOSNative` 等平台偏好会参与评分，而不是直接绕过 capability 过滤
- WinSSL 当前 `OCSPStaplingSupport=sslSupportNone`、`EarlyDataSupport=sslSupportNone`，因此 requirement 不能把它算成支持 server OCSP / early-data 的 backend。

更完整的 backend truth 统一以 `docs/BACKEND_CAPABILITY_MATRIX.md`
及 dedicated backend matrices 为准。

## API 设计

### Builder 入口

```pascal
var
  Requirements: TSSLRequirements;
begin
  Requirements := CreateSecurityFirstRequirements;

  Ctx := TSSLContextBuilder.Create
    .WithAutoBackendSelection(Requirements)
    .WithVerifyPeer
    .BuildClient;
end;
```

### Builder 快捷方法

当前公开的 builder convenience API 包括：

- `WithSecurityFirst`
- `WithPerformanceFirst`
- `WithCompatibilityFirst`
- `RequireTLS13`
- `RequireCipher(...)`
- `RequirePKCS11Support`
- `PreferOSNative`
- `WithBackend(...)`

这些方法最终仍然回到 `TSSLRequirements` 或显式 backend 选择。

### 函数式入口

```pascal
var
  Requirements: TSSLRequirements;
  SelectedType: TSSLLibraryType;
  MatchScore: Integer;
begin
  Requirements := CreateDefaultRequirements(optBalanced);
  Requirements.RequiredProtocols := [sslProtocolTLS13];
  Requirements.RequiredFeatures := [sslFeatOCSPStapling];

  if SelectBestBackend(Requirements, SelectedType, MatchScore) then
    WriteLn('Selected: ', Ord(SelectedType), ' score=', MatchScore);
end;
```

### 查询候选和原因

```pascal
var
  Requirements: TSSLRequirements;
  Results: TSSLBackendMatchArray;
  I: Integer;
begin
  Requirements := CreateDefaultRequirements(optBalanced);
  Requirements.RequiredProtocols := [sslProtocolTLS13];
  Requirements.RequiredFeatures := [sslFeatOCSPStapling];
  Results := SelectBestBackends(Requirements, 3);

  for I := 0 to High(Results) do
  begin
    WriteLn(Results[I].BackendName);
    WriteLn(Results[I].MatchScore);
    WriteLn(Results[I].RecommendationReason);
  end;
end;
```

## 返回结构

当前排序结果使用 `TSSLBackendMatchArray` / `TSSLBackendMatch`：

```pascal
type
  TSSLBackendMatch = record
    BackendType: TSSLLibraryType;
    BackendName: string;
    MatchScore: Integer;
    Capabilities: TSSLBackendCapabilities;
    MatchDetails: TSSLBackendMatchDetails;
    RecommendationReason: string;
  end;
```

## 当前未发布的草案 surface

- 没有 `TBackendSelector` 对象式公开 API
- 没有 `TBackendSelectionResult` 这套旧 record
- 没有 `WithPreferredBackend(...)` / `WithFallbackBackend(...)`
- 没有 `WithAllowPartialMatch`
- 没有 selector 专用 env/config 入口

如果调用方需要更细粒度控制，当前做法是：

1. 直接构造 `TSSLRequirements`
2. 先调用 `SelectBestBackends(...)` 看排序结果
3. 再决定使用 `WithAutoBackendSelection(...)` 或 `WithBackend(...)`

## 错误处理

当前自动选择失败时，应按一般 `ESSLException` 路径处理，而不是依赖未发布的专用异常类型：

```pascal
try
  Ctx := TSSLContextBuilder.Create
    .WithSecurityFirst
    .WithVerifyPeer
    .BuildClient;
except
  on E: ESSLException do
    WriteLn('自动选择失败: ', E.Message);
end;
```

当前也没有 dedicated partial-match builder API；如果需求过严，应通过放宽
`TSSLRequirements` 或先看 `SelectBestBackends(...)` 的候选结果来调整。

## 实现注意事项

1. capability 判断以 source/runtime truth 为准，不以历史布尔字段心智模型为准
2. 平台偏好只影响排序，不应绕过“不支持就是不支持”的 capability 过滤
3. 设计文档不要再维护与 canonical matrix 分叉的 backend 支持表
4. 如果 selector 语义发生变化，必须同步更新 focused contract

## 相关文档

- `src/fafafa.ssl.backend.selector.pas`
- `src/fafafa.ssl.context.builder.pas`
- `docs/BACKEND_SELECTION_GUIDE.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
