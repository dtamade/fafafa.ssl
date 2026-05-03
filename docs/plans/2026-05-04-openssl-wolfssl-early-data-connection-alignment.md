# OpenSSL / WolfSSL Early-Data Connection Alignment Plan

**Goal:** 收口 `OpenSSL` / `WolfSSL` 在连接级 `early data` 上的 public/runtime 漂移，让支持 early-data 的后端真正暴露 `ISSLEarlyDataConnection`，并把能力与文档重新对齐。

**Architecture:** 这批不重开 FreePascal 0-RTT 主线，不扩新接口。只补三层一致性：
- `tests/*` focused contract 锁定 `OpenSSL` / `WolfSSL` 的 capability / optional-interface / 基本 precondition 语义。
- `src/fafafa.ssl.openssl.*` / `src/fafafa.ssl.wolfssl.*` 补齐缺失的 session/early-data native 绑定和连接级 `ISSLEarlyDataConnection`。
- `README.md` / `docs/BACKEND_CAPABILITY_MATRIX.md` / `docs/guides/EARLY_DATA_GUIDE.md` 收紧或更新到当前实现 truth。

**Files:**
- Add: `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
- Modify: `src/fafafa.ssl.openssl.api.core.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.api.pas`
- Modify: `src/fafafa.ssl.wolfssl.base.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `README.md`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/EARLY_DATA_GUIDE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - lock capability and optional-interface drift

Run:

```bash
fpc -Fu./src tests/test_openssl_wolfssl_early_data_connection_contract.pas -otmp/test_openssl_wolfssl_early_data_connection_contract
./tmp/test_openssl_wolfssl_early_data_connection_contract
```

Add checks:
- `OpenSSL` / `WolfSSL` capability matrix must no longer say `EarlyDataSupport = sslSupportNone` when the backend exposes runtime early-data APIs.
- client context must expose `ISSLEarlyDataContext`
- client connection must expose `ISSLEarlyDataConnection`
- helper surface must agree with `Supports(...)`
- `SetEarlyData(...)` must fail coherently before enable/session prerequisites are met

## Task 2: GREEN - implement missing connection-level early-data surface

Change:
- `TOpenSSLConnection`: implement `ISSLEarlyDataConnection`, store configured session/queued payload, derive early-data limit from native session helpers, and update status from native early-data status helpers.
- `TWolfSSLConnection`: implement `ISSLEarlyDataConnection`, store configured session/queued payload, use WolfSSL TLS 1.3 early-data helpers for limit/status, and align capability reporting.
- add missing native bindings required by those paths.

Constraint:
- keep scope bounded to client connection early-data queue/status semantics
- do not reopen unrelated TLS transport or CT / OCSP lines

## Task 3: Docs truth alignment

Update:
- `README.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/EARLY_DATA_GUIDE.md`

Requirements:
- stop claiming removed `MbedTLS` / `WinSSL` early-data interfaces still exist
- stop claiming `WolfSSL` is “not supported” once the connection interface exists
- keep wording conservative where only focused contract evidence exists

## Task 4: Verification

Run:

```bash
fpc -Fu./src tests/test_openssl_wolfssl_early_data_connection_contract.pas -otmp/test_openssl_wolfssl_early_data_connection_contract && ./tmp/test_openssl_wolfssl_early_data_connection_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `OpenSSL` / `WolfSSL` client connections expose `ISSLEarlyDataConnection`
- focused contract passes
- repo compile gate passes
- minimal CI gate passes
- docs stop overclaiming or lagging behind current code truth

## Execution Result

- RED:
  - `fpc -Fu./src tests/test_openssl_wolfssl_early_data_connection_contract.pas -otmp/test_openssl_wolfssl_early_data_connection_contract && ./tmp/test_openssl_wolfssl_early_data_connection_contract`
  - 初次结果：`OpenSSL helper detects ISSLEarlyDataConnection` / `OpenSSL connection exposes ISSLEarlyDataConnection` 失败，证明 context-level 与 connection-level early-data surface 脱节。
- GREEN:
  - `TOpenSSLConnection` / `TWolfSSLConnection` 均已实现 `ISSLEarlyDataConnection`。
  - `OpenSSL` / `WolfSSL` 缺失的 session/connection early-data native binding 已补齐。
  - `TWolfSSLLibrary.GetCapabilities` 已把 `EarlyDataSupport` / `ZeroRTTSupport` 收敛为 `sslSupportExperimental`。
  - README / capability matrix / early-data guide 已重新对齐当前代码 truth。
- Verification:
  - focused contract 复跑：`Passed: 12 / Failed: 0 / Skipped: 1`（`WolfSSL` 因 backend 不可用而本机跳过）
  - `python3 scripts/compile_all_modules.py` => PASS (`185/185`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS（compile gate + `17/17` module tests + phase2 baseline dry-run）
