# 2026-05-19 Backend Capability Truth Tightening

## Goal

在不重开大范围重构的前提下，收掉两类会持续误导后续开发路线的真相漂移：

1. `MbedTLS / WolfSSL` 已在 `IsFeatureSupported(sslFeatSessionCache)` 宣称支持 session cache，但 `GetCapabilities` 没有发布 `SessionCacheSupport`
2. WinSSL 的活跃文档仍在多处把 `session resumption / session ticket / OCSP stapling` 说得比当前 runtime/capability truth 更满

## Scope

- 只修 capability 发布面和活跃文档/设计文档
- 不重开新的后端实现线
- 不在本批继续追 WinSSL native probe 实现修复

## Files

- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/QUICKSTART.md`
- `docs/reference/WINSSL_DESIGN.md`
- `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- `docs/reference/BACKEND_SELECTOR_DESIGN.md`
- `tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
- `tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- backend selector 对 `sslFeatSessionCache` 的判定看的是 `SessionCacheSupport`
- 当前 `MbedTLS / WolfSSL` source 里 `IsFeatureSupported(sslFeatSessionCache)=True`，但 `GetCapabilities` 没有对应 support-level truth，会造成 capability surface 自相矛盾
- WinSSL 当前 public session truth 仍是：
  - `observed_reuse=false`
  - `session_configured=true`
- WinSSL native `SECPKG_ATTR_SESSION_INFO` probe 仍只允许停留在 opt-in isolated worker / experimental evidence lane

## Steps

1. 扩 WinSSL docs truth contract，把顶层矩阵、Quickstart、设计文档、selector/abstraction 设计一并纳入 RED 检查
2. 新增 optional-backend session-cache capability contract，锁住 `MbedTLS / WolfSSL` 的 source truth
3. 最小修改 `mbedtls/wolfssl` capability source 和 WinSSL 相关文档
4. 跑 focused contracts、全仓编译、`git diff --check`

## Commands

```bash
bash -n tests/scripts/test_optional_backends_session_cache_capability_contract.sh
bash tests/scripts/test_optional_backends_session_cache_capability_contract.sh
bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
python3 scripts/compile_all_modules.py
git diff --check
```

## Expected Result

- `MbedTLS / WolfSSL` 的 `SessionCacheSupport` 与 `IsFeatureSupported(sslFeatSessionCache)` 一致
- WinSSL 活跃文档不再宣称已稳定 runtime-proven 的 session resumption/performance truth
- selector / abstraction 设计文档不再把 WinSSL OCSP stapling / session ticket 写成无条件完整支持
