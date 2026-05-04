# WinSSL Documentation Truth Alignment Plan

**Goal:** 把 WinSSL 设计文档和状态报告收口到当前可验证真相，不再继续发布“100% 完成”或“运行时已证实”的过宽表述；明确区分 Linux 上已拿到的 source/compile 证据与仍需 Windows 主机的 runtime proof。

**Architecture:** 这批不改任何 WinSSL 生产代码，也不重开 capability 设计。只做文档 truth alignment：以 `docs/BACKEND_CAPABILITY_MATRIX.md`、当前 `src/fafafa.ssl.winssl.*` 实现、以及今天已经跑过的 source contract / Win64 交叉编译 / compile gate / minimal CI gate 为真相源，更新 `docs/reference/WINSSL_DESIGN.md` 和 `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`。文档要强调三层边界：public surface、cross-target compile surface、Windows runtime proof。

**Files:**

- Modify: `docs/reference/WINSSL_DESIGN.md`
- Modify: `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Audit the doc drift against current truth

Check:

```bash
sed -n '1,260p' docs/reference/WINSSL_DESIGN.md
sed -n '1,260p' docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md
sed -n '1,260p' docs/BACKEND_CAPABILITY_MATRIX.md
sed -n '472,610p' src/fafafa.ssl.winssl.lib.pas
```

Expected findings:

- `WINSSL_DESIGN.md` 仍把 WinSSL 描述成“100% 完成”，且缺少当前 compile/runtime 证据分层
- `WINSSL_BACKEND_STATUS_REPORT.md` 仍把 `DTLS`、`OCSP Stapling`、`Session Ticket` 等写成已证实支持
- 当前真实 capability 以 `docs/BACKEND_CAPABILITY_MATRIX.md` 和 `src/fafafa.ssl.winssl.lib.pas` 为准：Early Data / caller-provided server OCSP stapling 不暴露 public surface，DTLS 为不支持

## Task 2: Update the docs only

Change:

- `docs/reference/WINSSL_DESIGN.md`
  - 顶部改成 evidence-based 状态说明
  - 明确 Linux 侧当前可做 source contract + Win64 cross-target compile，不能代替 runtime
  - 把核心类型示例对齐到当前 `ISSLNativeHandleAccess` / internal access seam
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - 改写成当前证据报告，而不是 feature wishlist
  - 只保留已证实的 compile/public-surface truth
  - 单独列出仍待 Windows 主机证明的 runtime 区域

Constraints:

- 不修改生产代码或 capability matrix
- 不把 Linux 交叉编译成功写成 Windows runtime 已完成
- 不保留和当前 `GetCapabilities` 冲突的功能表述

## Task 3: Verification

Run:

```bash
git diff --check
git diff --stat
```

Formatting:

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/WINSSL_DESIGN.md /home/dtamade/projects/fafafa.ssl/docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md /home/dtamade/projects/fafafa.ssl/task_plan.md /home/dtamade/projects/fafafa.ssl/findings.md /home/dtamade/projects/fafafa.ssl/progress.md /home/dtamade/projects/fafafa.ssl/docs/plans/2026-05-04-winssl-documentation-truth-alignment.md
```

## Definition Of Done

- `WINSSL_DESIGN.md` 不再宣称 WinSSL runtime 已被当前环境完整证实
- `WINSSL_BACKEND_STATUS_REPORT.md` 的功能表述与当前 capability / compile evidence 一致
- 文档明确写清 source contract、Win64 compile、Windows runtime proof 三层边界
- 台账同步到新的 WinSSL 文档真相
