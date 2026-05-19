# 2026-05-20 WinSSL None-Capability Surface Doc Alignment

## Goal

把 WinSSL backend 专页中

- `OCSP Stapling`
- `0-RTT`

这两行从“Schannel 可能有平台潜力”的叙事，收回到当前 fafafa.ssl
真正发布的 capability / public surface truth：

- `OCSPStaplingSupport=sslSupportNone`
- `EarlyDataSupport=sslSupportNone`
- 不暴露 `ISSLServerOCSPStaplingContext`
- 不暴露 `ISSLEarlyDataContext`

## Scope

- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 WinSSL 实现
- 不改 top-level backend matrix（它当前已经是 `❌`）
- 不重开 session/ticket/runtime-proof 线

## Why This Batch

当前 drift 很集中：

- WinSSL source 已明确发布：
  - `Result.OCSPStaplingSupport := sslSupportNone;`
  - `Result.EarlyDataSupport := sslSupportNone;`
- top-level `docs/BACKEND_CAPABILITY_MATRIX.md`
  也已经把 WinSSL 的
  `Early Data` / `OCSP Stapling`
  汇总成 `❌`
- 但 dedicated `WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  仍写：
  - `OCSP Stapling | ⚠️ 部分`
  - `0-RTT | ⚠️ 部分`

这会把“底层平台潜力”误读成“fafafa.ssl 当前 shipped public capability”。

## Steps

1. 新增 focused shell contract，先做 RED
2. 最小更新 WinSSL backend matrix 两行 wording
3. 跑 focused contract + `git diff --check`

## Verification

```bash
bash -n tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh
bash tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- WinSSL 专页不再把 none-published capability 写成 `⚠️ 部分`
- 平台潜力与当前 fafafa.ssl public surface 真相彻底分层
