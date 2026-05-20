# 2026-05-21 MbedTLS Session Resumption Doc Truth

## Goal

把 `MbedTLS` 当前 session resumption 的 active docs / reference truth 收紧到和源码一致：

- 已发布的 truth：
  - `GetSession / SetSession`
  - session `Serialize / Deserialize`
  - cache / ticket candidate path
- 当前不能高估的 truth：
  - 不能把 `SetSession(...)` 自动解释成
    `observed resumed handshake`
  - 不能把 generic session-resumption 示例写成
    `MbedTLS`
    已有通用 runtime proof

## Scope

- 只做：
  - `MbedTLS` session resumption 的 source/doc truth alignment
  - focused docs contract
  - 台账同步
- 不做：
  - 新增 `MbedTLS` native observed-reuse runtime probe
  - 重开 `WinSSL` / `OpenSSL` / `WolfSSL` 的 session lane
  - 生产实现改动

## Files

- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/API_DOCUMENTATION.md`
- `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- `tests/scripts/test_mbedtls_session_resumption_doc_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `MbedTLS`
  当前 local source/header truth：
  - 有 `mbedtls_ssl_set_session`
  - 有 `mbedtls_ssl_get_session`
  - 有 `mbedtls_ssl_session_load/save`
  - 但没有像
    `SSL_session_reused`
    /
    `wolfSSL_session_reused`
    那样的 public reused getter
- 所以当前最稳妥的 active truth 是：
  - `SetSession(...)`
    = 为下一次握手配置候选 session
  - 不是：
    `observed resumed handshake`
- 当前 shared source / contract truth 继续只稳定证明：
  - configured session
    不会被误报成
    observed reuse

## Steps

1. 复核 `src/fafafa.ssl.mbedtls.connection.pas` 与当前 local-header truth。
2. 收紧 MbedTLS 专属文档与高入口通用参考文档的措辞。
3. 新增 focused docs contract，冻结这条 source/doc truth。
4. 运行：
   - `bash -n tests/scripts/test_mbedtls_session_resumption_doc_truth_contract.sh`
   - `bash tests/scripts/test_mbedtls_session_resumption_doc_truth_contract.sh`
   - `bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
   - `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
   - `git diff --check`
5. 更新 `task_plan.md` / `findings.md` / `progress.md`

