# Backend Capability Matrix Quick Reference Truth

## Goal

收紧 `docs/BACKEND_CAPABILITY_MATRIX.md` 顶部 quick reference 里几处
summary-level capability 漂移，消除“顶层表格比 source truth 和 backend-specific
文档更激进”的自相矛盾。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结顶层能力矩阵 quick reference 的当前真相
- 只修改 `docs/BACKEND_CAPABILITY_MATRIX.md`
- 不改生产实现
- 不扩大到 archive / 历史版本说明

## Files

- Add: `docs/plans/2026-05-19-backend-capability-matrix-quick-reference-truth.md`
- Add: `tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前顶层 quick reference 至少有 3 处已经比 source/backend truth 更激进：

- `WinSSL TLS 1.3` 被写成无条件 `✅`
  - 但 source `SupportsTLS13` 明确受 Windows / Schannel 版本门控
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 也写的是
    `Windows 10 1903+`
- `WinSSL PSK` 被写成 `⚠️`
  - 但 WinSSL backend 专项矩阵已经明确：
    `PSK | ❌ 不支持 | Schannel 限制`
- `FreePascal ALPN / SNI` 被写成 `✅`
  - 但顶层矩阵自己已经声明这些 paired feature 要按 `*Support` 字段解读
  - source 当前仍发布：
    - `ALPNSupport=sslSupportExperimental`
    - `SNISupport=sslSupportExperimental`

这类问题的风险在于：

- 读者会先看 quick reference，不一定继续下钻 backend-specific 文档
- 顶层摘要一旦过头，就会把“条件支持 / experimental / unsupported”
  误读成稳定可依赖 truth

## Verification

```bash
bash -n tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh
bash tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh
npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md
git diff --check
```

## Expected Outcome

- 顶层 quick reference 不再把 `WinSSL TLS 1.3` 写成无条件 `✅`
- 顶层 quick reference 不再把 `WinSSL PSK` 写成 `⚠️`
- 顶层 quick reference 不再把 `FreePascal ALPN / SNI` 写成稳定 `✅`
- 读者从根入口进入时，就能先拿到和 source/backend truth 一致的保守口径
