# `ISSLOCSPStapling` Active Guidance De-emphasis

## Goal

把普通 API 文档里仍把 OCSP stapling 能力当作 `ISSLConnection` 核心入口的示例切到 `ISSLOCSPStapling` owner path，让 OCSP 这组能力面从“普通文档仍直连 core”收缩到“仅 compatibility-core mirrors / backend-specific runtime 残留”。

## Scope

本批只处理 active guidance、focused contract 与台账：

- `docs/reference/API_DOCUMENTATION.md`
- `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不碰 backend-specific OCSP runtime / contract proof
- 不重跑重型 Pascal/repo gate

## Why This Batch

当前 `ISSLOCSPStapling` 的 owner truth 已经先有：

- `tests/contract/test_backend_contract.pas` 已锁住 capability 与 owner interface 曝露边界
- `docs/reference/API_DOCUMENTATION.md` 的 “检查 OCSP 状态” 与排错条目也已经有一套 `Supports(Connection, ISSLOCSPStapling, OCSP)` 示例

但同一个文档里仍保留四段 ordinary direct-core 示例：

- `Connection.GetOCSPStaplingEnabled`
- `Connection.GetOCSPResponse`
- `Connection.IsOCSPResponseVerified`
- `Connection.GetOCSPResponseStatus`

这会把普通读者重新带回 core mirrors，而不是 owner path。

## Planned Changes

1. 把 `API_DOCUMENTATION` 里 4 段 direct-core OCSP 示例切到 `ISSLOCSPStapling` owner path。
2. 补明确的 owner-first guidance，说明新代码优先通过 `ISSLOCSPStapling` 读取 stapling 状态 / response / verify status。
3. 新增 focused contract，防止普通文档重新把 OCSP 教回 direct core。

## Verification

```bash
bash -n tests/scripts/test_isslocspstapling_active_guidance_contract.sh
bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh
git diff --check
```

## Expected Outcome

- ordinary OCSP documentation stops teaching direct core `GetOCSP*` access as the preferred path
- new code guidance clearly points to `ISSLOCSPStapling`
- remaining direct core OCSP mirrors stay intentionally confined to compatibility-core truth and backend-specific runtime proof

## Result

- `docs/reference/API_DOCUMENTATION.md` 的 4 段 direct-core OCSP 示例现在统一改成：
  - `Supports(Connection, ISSLOCSPStapling, OCSP)`
  - `OCSP.GetOCSPStaplingEnabled`
  - `OCSP.GetOCSPResponse`
  - `OCSP.IsOCSPResponseVerified`
  - `OCSP.GetOCSPResponseStatus`
- 同一文档现在新增明确的 owner-first guidance：
  - 新代码优先通过 `ISSLOCSPStapling` 读取 stapling 状态 / response / verify status / status string
  - `Connection.GetOCSP*` 只作为 compatibility-core mirrors 继续存在
- 新增 focused contract：
  - `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`

## Route Impact

- `ISSLOCSPStapling` 的 owner truth 原本就已存在；这批之后 ordinary OCSP docs 也不再继续把 `GetOCSP*` 当推荐主路径
- 当前高价值 optional-owner ordinary-guidance sweep 已完成：
  - `ISSLDiagnostics`
  - `ISSLCertificateVerification`
  - `ISSLSessionResumption`
  - `ISSLOCSPStapling`
- 默认下一步不该再重复做这 4 组 optional-owner surface 的普通文档清扫
- 若继续推进主线，默认应切回更大的 interface-design completeness / implementation-completeness 审查
