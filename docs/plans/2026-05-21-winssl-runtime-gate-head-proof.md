# WinSSL Runtime Gate Head Proof

## Goal

为当前远端 head `80b3500` 补一份 fresh Windows runtime proof，避免 `WinSSL` 继续停留在“历史 run 绿过，但不覆盖今天代码”的状态。

这批不改 `src/` 生产实现，也不重开更重的 `wave-b-b2-manual.yml`。只做三件事：

- 把本地已完成的 13 个提交 push 到 `origin/master`
- 对当前 head 手动派发最窄的 `winssl-tests.yml`
- 下载 artifact，确认它不只是绿勾，而是真的包含 quick smoke / Wave B / broader runtime suite evidence

## Why This Batch

当前本地已经把：

- `TSSLConfig.ServerName`
- `LogLevel / LogCallback`
- `HandshakeTimeout / BufferSize`

这些高价值 mixed-scope 线补到了 backend parity proof。

但远端 `WinSSL Runtime Gate` 最近的成功 run 仍停在更早的 head：

- `26185903650` on `8a4e9a0...`

这意味着：

- workflow 自身是活的
- WinSSL Windows lane 最近也确实能绿
- 但它还没有对今天的 `80b3500` 给出 fresh runtime proof

## Scope

### Add

- `docs/plans/2026-05-21-winssl-runtime-gate-head-proof.md`
- `docs/test_reports/WINSSL_RUNTIME_GATE_HEAD_PROOF_2026-05-21.md`

### Update

- `.github/README.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands

```bash
git push origin master
gh api repos/dtamade/fafafa.ssl/actions/workflows/winssl-tests.yml/dispatches -X POST -f ref=master
gh api repos/dtamade/fafafa.ssl/actions/workflows/winssl-tests.yml/runs -q '.workflow_runs[] | select(.head_sha=="80b3500bc00eb3778dff2c97168e61c236e6506b") | [.id, .head_sha, .status, (.conclusion // ""), .event, .display_title, .created_at] | @tsv'
gh run watch 26193849105 --exit-status --interval 20
gh run view 26193849105 --json status,conclusion,displayTitle,event,headSha,jobs,workflowName,createdAt,updatedAt,url
gh api repos/dtamade/fafafa.ssl/actions/runs/26193849105/artifacts -q '.artifacts[] | [.name, .expired, .size_in_bytes] | @tsv'
gh run download 26193849105 -D tmp/gh-run-26193849105
```

## Expected Outputs

- `WinSSL Runtime Gate` 在 head `80b3500` 上 fresh `SUCCESS`
- artifact `winssl-windows-evidence-gh_26193849105_1` 可下载
- `wave_b_windows_gate_summary_gh_26193849105_1.md` 显示 `overall: PASS`
- `winssl_runtime_suite_gh_26193849105_1.log` 包含：
  - `[WINSSL-RUNTIME] suite_start`
  - `[WINSSL-RUNTIME] suite_summary`
  - `[WINSSL-RUNTIME] suite_end status=PASS`
- WinSSL session runtime truth 继续保持当前保守口径：
  - `observed_reuse=false`
  - `session_configured=true`
  - `native_probe_enabled=false`

## Route Impact

- `WinSSL` 不再只是“历史上某次在 Windows 绿过”
- 当前 head 的 Windows runtime evidence 已重新补齐
- 后续默认不应再把“今天这批代码有没有 fresh WinSSL proof”当成 open question
- 下一刀应优先回到：
  - pure Pascal backend completeness
  - 或 WinSSL 更深的行为 gap
  - 而不是重复派发同一条 auto lane
