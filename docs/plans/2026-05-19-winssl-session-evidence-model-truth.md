# WinSSL Session Evidence Model Truth

## Goal

把 WinSSL session-resumption 的证据模型写死，避免把两类信号混成同一个结论：

- `observed_reuse` 是当前 shared/canonical path 的 conservative public truth
- `native_observed_reuse` / `native_probe_succeeded` 才是 isolated native probe lane 的额外证据

## Scope

- 不改 WinSSL 握手实现
- 不重开 `SetSession(...)` compatibility metadata 语义线
- 不重新争论 `SessionCacheSupport=sslSupportStable`
- 只修 proof marker、Windows validation docs、WinSSL 高入口说明里的 evidence-model drift

## Files

- Add: `docs/plans/2026-05-19-winssl-session-evidence-model-truth.md`
- Add: `tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `tests/windows/VALIDATION_BUNDLE.md`
- Modify: `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前源码已经有一个很重要但容易误读的事实：

1. `UpdateSessionReuseTruthFromContext(...)` 在 shared canonical path 上继续撤下 live `SECPKG_ATTR_SESSION_INFO` probe，避免 Windows AV。
2. 因而 broader/shared lane 的 `observed_reuse=false` 目前是 conservative public truth，不是“已经安全直接观测到 Schannel 没复用”。
3. 更深 native 证据当前只能通过 opt-in isolated worker / native probe lane 查看：
   - `native_probe_enabled`
   - `native_observed_reuse`
   - `native_probe_succeeded`

但当前 checklist / bundle / 状态报告和部分高入口说明，还容易把 `[WINSSL-RUNTIME] session_resumption summary ... observed_reuse=...` 读成“是否真的观测到 resumed handshake”的唯一结论。

## Steps

1. 新增 focused contract，先固定应有 evidence-model wording。
2. 让 dedicated proof program 额外输出一条稳定 `evidence_model` marker。
3. 同步修正 Windows validation docs、状态报告和 WinSSL 高入口说明。
4. 跑 focused contract 与现有 runtime-truth contract。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_evidence_model_truth_contract.sh
bash tests/scripts/test_winssl_session_evidence_model_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
git diff --check
```

## Expected Outcome

- artifact 中会稳定出现：
  - `[WINSSL-RUNTIME] session_resumption evidence_model ...`
  - richer `summary ... observed_reuse=... native_observed_reuse=...`
- 活跃文档会明确：
  - `observed_reuse` 是 conservative public truth
  - `native_observed_reuse` / `native_probe_succeeded` 才是 isolated native probe 的额外 evidence
