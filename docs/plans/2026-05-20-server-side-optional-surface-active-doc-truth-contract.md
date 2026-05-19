# 2026-05-20 Server-Side Optional Surface Active-Docs Truth Contract

## Goal

为 server-side optional surface 增补一条 active-docs truth contract，
把当前 cross-backend 真值稳定冻结到：

- `docs/reference/API_REFERENCE.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/EARLY_DATA_GUIDE.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`

避免以后再出现：

- source / runtime capability 已经对齐
- 但 active docs 在不同入口上重新漂开

## Scope

- `tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh`
- `docs/plans/2026-05-20-server-side-optional-surface-active-doc-truth-contract.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 backend 实现
- 不重开 runtime handshake proof
- 不回写历史 archive / 老计划文档

## Why This Batch

刚刚两批 closeout 已经说明：

- WinSSL dedicated matrix 可能独自漂离 source / top-level matrix
- FreePascal durable-default replay truth 可能在 active docs / old contract 中残留旧表述

这说明当前缺的不是又一轮大实现，而是一条专门冻结
server-side optional surface active-doc truth 的 focused contract。

## Current Truth To Freeze

- `API_REFERENCE`
  - 当前 public Pascal source 尚未声明 `ISSLServerConnection`
  - `ISSLServerOCSPStaplingContext` / `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
    都是 live public surface
- top-level `BACKEND_CAPABILITY_MATRIX`
  - FreePascal early-data = experimental + durable default replay-store
  - OpenSSL early-data = stable
  - WinSSL early-data = none-published capability
  - MbedTLS early-data = none-published capability
  - WolfSSL early-data = helper-gated experimental
  - FreePascal / OpenSSL / WolfSSL server OCSP public surface 已发布
  - WinSSL / MbedTLS server OCSP public surface 未发布
- dedicated backend pages
  - WinSSL:
    - `OCSPStaplingSupport=sslSupportNone`
    - `EarlyDataSupport=sslSupportNone`
  - MbedTLS:
    - `0-RTT` 当前 capability none
- active guides
  - `EARLY_DATA_GUIDE`
    记录 backend-by-backend early-data truth
  - `OCSP_USAGE_GUIDE`
    记录 server stapling 只负责 caller-provided material，
    backend 不支持时 builder fail-fast

## Verification

```bash
bash -n tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh
bash tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh
git diff --check
```

## Expected Outcome

- 以后再改 early-data / server stapling active docs 时，
  如果 source truth 与 guide / matrix / reference 漂开，
  focused contract 会第一时间报红
