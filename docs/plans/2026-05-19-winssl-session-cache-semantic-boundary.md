# WinSSL Session Cache Semantic Boundary

## Goal

把 `SessionCacheSupport=sslSupportStable` 在 WinSSL 上的真实语义写死：

- 它表示 context-level session cache/control surface 已发布且已接线
- 它不等于当前已经 runtime-proven 的 resumed handshake

避免调用方把 capability 级别的 `stable` 误读成 dedicated Windows proof 已经命中复用。

## Scope

- 不改 WinSSL 握手实现或 runtime 路径
- 不重开已经收口的 `SetSession(...)` compatibility metadata 语义线
- 只修 source comment / API reference / WinSSL 活跃文档中的语义缺口

## Files

- Add: `docs/plans/2026-05-19-winssl-session-cache-semantic-boundary.md`
- Add: `tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前实现层面已经有两条事实同时成立：

1. `SessionCacheSupport=sslSupportStable` 可以成立，因为 WinSSL 的 context-level session cache/control surface 已发布，而且 `TWinSSLContext` 的相关配置已经接线到 credential acquisition。
2. 当前 dedicated Windows runtime truth 仍是 `observed_reuse=false` / `session_configured=true`，所以 resumed-handshake 结果还不能被写成已稳定命中。

真正的 drift 在于“stable 的是哪一层”没有被活跃接口文档写透，`API_REFERENCE.md` 甚至连 `SessionCacheSupport` 字段都没有完整列出来。

## Steps

1. 新增 focused contract，先固定应有语义边界。
2. 最小修正 source comment 与活跃文档：
   - 补齐 `API_REFERENCE.md` 的 `SessionCacheSupport` 字段
   - 统一说明 `SessionCacheSupport` 只代表 cache/control surface，不代表 resumed-handshake 结果
3. 跑 focused contract 与相关回归。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh
bash tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh
bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh
git diff --check
```

## Expected Outcome

- source / docs 都会明确：
  - `SessionCacheSupport=sslSupportStable` 说的是 session cache/control surface
  - `observed_reuse=false` / `session_configured=true` 仍是当前 dedicated Windows runtime truth
- `API_REFERENCE.md` 不再遗漏 `SessionCacheSupport` 字段
- 后续再看 WinSSL session 线时，不会把 capability stable 和 resumed-handshake proof 混成同一个问题
