# Backend Optional Interface Alignment Plan

**Goal:** 收紧后端可选接口暴露，让 `ISSLEarlyDataContext` / `ISSLServerOCSPStaplingContext` 只在真实支持的后端上可见，并同步能力文档。

**Architecture:** 不新增新功能，不改核心 `ISSLContext` 契约；只修“可选接口暴露 truth source”。以 `tests/contract/test_backend_contract.pas` 为跨后端 RED/GREEN 入口，生产改动只落在 `src/fafafa.ssl.mbedtls.context.pas`、`src/fafafa.ssl.winssl.context.pas`，文档 truth alignment 只落在 `docs/BACKEND_CAPABILITY_MATRIX.md`。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.mbedtls.context.pas`
- Modify: `src/fafafa.ssl.winssl.context.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove unsupported backends still expose optional interfaces

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- If `Caps.EarlyDataSupport = sslSupportNone`, context must not support `ISSLEarlyDataContext`.
- If `Caps.OCSPStaplingSupport = sslSupportNone`, context must not support `ISSLServerOCSPStaplingContext`.
- If support level is usable, the interface should still be present.

## Task 2: GREEN - remove false-positive interface exposure

Change:
- `TMbedTLSContext`: stop implementing `ISSLEarlyDataContext` and `ISSLServerOCSPStaplingContext`.
- `TWinSSLContext`: stop implementing `ISSLEarlyDataContext` and `ISSLServerOCSPStaplingContext`.

Constraint:
- Keep implementation scoped; do not reopen unrelated TLS behavior.

## Task 3: Docs truth alignment

Fix `docs/BACKEND_CAPABILITY_MATRIX.md` so quick table and per-backend sections match the current implementation truth.

## Task 4: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
```

If the touched area justifies it, also run:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Execution Result

- RED:
  - `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
  - 初次在 `sslMbedTLS` 上失败，暴露出 unsupported optional interface 仍被 `Supports(...)` 识别为真。
- GREEN:
  - 移除 `TMbedTLSContext` / `TWinSSLContext` 的 `ISSLEarlyDataContext`、`ISSLServerOCSPStaplingContext` 接口声明。
  - focused contract 复跑转绿。
- Verification:
  - `python3 scripts/compile_all_modules.py` => PASS (`185/185`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS（compile gate + 17/17 module tests + phase2 baseline dry-run）
