# OpenSSL SSL Runtime Wave 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收紧 OpenSSL `SSL` 模块剩余 helper family 的 load contract 与 capability matrix，让公开能力声明和真实已绑定 helper surface 保持一致。

**Architecture:** 延续 wave1 的 TDD contract-hardening 路线，但本轮不再“看到一个点修一个点”，而是按公开承诺优先级分批推进：先处理 capability matrix 已宣称稳定的 `early-data / 0-RTT`，再处理 declared-but-unbound 的 `keylog / record-padding`，最后处理当前 host 明确导出的 `async / QUIC` helper surface。每一批都先写 focused RED contract，再做最小 loader/capability 修复，最后执行回归链和 ledger 回填。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-06)
- wave1 已经收口：
  - compile gate fail-open
  - minimal gate `eval`
  - factory publish-before-initialize
  - loader required-symbol enforcement
  - `SSL` unload drift
  - `SSL` info/state + session-ticket / PSK load drift
- 当前剩余 `SSL` helper family 漂移集中在：
  - `early-data / 0-RTT`
  - `keylog`
  - `record-padding`
  - `async / SSL_poll`
  - `QUIC`
- 当前源码证据：
  - `src/fafafa.ssl.openssl.api.ssl.pas`
    - 上述 family 都已声明、也会在 unload 时清空
    - 但 `LoadOpenSSLSSL` 仍未绑定这些 family
  - `src/fafafa.ssl.openssl.backed.pas`
    - `ZeroRTTSupport` / `EarlyDataSupport` 当前只按 TLS 1.3 版本号宣称 `sslSupportStable`
    - 这和 helper readiness 没有运行时对齐
  - `tests/openssl/test_openssl_features.pas`
    - 当前只锁住了 `SNI` / `ALPN` / `Renegotiation` 的 runtime drift contract
    - 还没有覆盖 `early-data / 0-RTT`
- 当前 host `libssl.so.3` probe 结果：
  - `early-data`: 5/5 exported
  - `keylog`: 2/2 exported
  - `record-padding`: setters/args/block-padding exported；`SSL_CTX_get_record_padding_callback` 与 `SSL_get_record_padding_callback` not exported
  - `async`: 4/4 exported
  - `QUIC`: 13/13 exported

## Delivery Order
1. `early-data / 0-RTT` contract + capability alignment
2. `keylog / record-padding` load contract
3. `async / QUIC` load contract
4. regression chain + working-memory closeout

---

### Task 1: Early-Data / 0-RTT Contract And Capability Alignment

**Files:**
- Create: `tests/test_openssl_ssl_early_data_contract.pas`
- Modify: `tests/openssl/test_openssl_features.pas`
- Modify: `src/fafafa.ssl.openssl.api.ssl.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Write the failing load contract**
- In `tests/test_openssl_ssl_early_data_contract.pas`:
  - load `libssl`
  - call `LoadOpenSSLSSL`
  - assert exported helpers are bound when exported:
    - `SSL_CTX_set_max_early_data`
    - `SSL_CTX_get_max_early_data`
    - `SSL_set_max_early_data`
    - `SSL_get_max_early_data`
    - `SSL_get_early_data_status`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_ssl_early_data_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_early_data_contract -FEtmp/openssl_ssl_early_data_contract -otmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract tests/test_openssl_ssl_early_data_contract.pas && ./tmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract`
- Expected:
  - FAIL because `LoadOpenSSLSSL` currently leaves these helpers as `nil`

**Step 3: Add the failing runtime-drift contract**
- In `tests/openssl/test_openssl_features.pas`:
  - extend the runtime drift section so it temporarily clears a representative early-data helper such as `SSL_CTX_set_max_early_data`
  - call `GetCapabilities`
  - assert:
    - `ZeroRTTSupport <> sslSupportStable`
    - `EarlyDataSupport <> sslSupportStable`

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because capability matrix still claims stable support from version alone

**Step 5: Write minimal implementation**
- In `src/fafafa.ssl.openssl.api.ssl.pas`:
  - bind the 5 early-data helpers in `LoadOpenSSLSSL`
- In `src/fafafa.ssl.openssl.backed.pas`:
  - add a local readiness helper for early-data
  - only publish `ZeroRTTSupport` / `EarlyDataSupport` as `sslSupportStable` when:
    - TLS 1.3 is supported
    - representative early-data helper surface is actually assigned at runtime

**Step 6: Run GREEN**
- Re-run the two commands from Step 2 and Step 4
- Expected:
  - both focused contracts PASS

---

### Task 2: Keylog / Record-Padding Load Contract

**Files:**
- Create: `tests/test_openssl_ssl_padding_contract.pas`
- Modify: `src/fafafa.ssl.openssl.api.ssl.pas`

**Step 1: Write the failing contract**
- In `tests/test_openssl_ssl_padding_contract.pas`:
  - load `libssl`
  - call `LoadOpenSSLSSL`
  - assert exported helpers are bound when exported:
    - `SSL_CTX_set_keylog_callback`
    - `SSL_CTX_get_keylog_callback`
    - `SSL_CTX_set_record_padding_callback`
    - `SSL_CTX_set_record_padding_callback_arg`
    - `SSL_CTX_get_record_padding_callback_arg`
    - `SSL_CTX_set_block_padding`
    - `SSL_set_record_padding_callback`
    - `SSL_set_record_padding_callback_arg`
    - `SSL_get_record_padding_callback_arg`
    - `SSL_set_block_padding`
  - explicitly probe and skip host-unexported getters:
    - `SSL_CTX_get_record_padding_callback`
    - `SSL_get_record_padding_callback`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_ssl_padding_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_padding_contract -FEtmp/openssl_ssl_padding_contract -otmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract tests/test_openssl_ssl_padding_contract.pas && ./tmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract`
- Expected:
  - FAIL on exported setters/args/block-padding helpers still left as `nil`

**Step 3: Write minimal implementation**
- In `src/fafafa.ssl.openssl.api.ssl.pas`:
  - bind keylog helpers
  - bind record-padding setters/args/block-padding helpers
  - keep host-unexported getters optional

**Step 4: Run GREEN**
- Re-run the focused contract
- Expected:
  - PASS with skip only on the two non-exported getters

---

### Task 3: Async / QUIC Load Contract

**Files:**
- Create: `tests/test_openssl_ssl_async_quic_contract.pas`
- Modify: `src/fafafa.ssl.openssl.api.ssl.pas`

**Step 1: Write the failing contract**
- In `tests/test_openssl_ssl_async_quic_contract.pas`:
  - load `libssl`
  - call `LoadOpenSSLSSL`
  - assert exported helpers are bound when exported:
    - async:
      - `SSL_poll`
      - `SSL_set_async_callback`
      - `SSL_set_async_callback_arg`
      - `SSL_get_async_status`
    - QUIC:
      - `SSL_get_stream_id`
      - `SSL_get_stream_type`
      - `SSL_is_stream_local`
      - `SSL_new_stream`
      - `SSL_accept_stream`
      - `SSL_get_accept_stream_queue_len`
      - `SSL_set_default_stream_mode`
      - `SSL_set_incoming_stream_policy`
      - `SSL_get0_connection`
      - `SSL_is_connection`
      - `SSL_get_stream_read_error_code`
      - `SSL_get_stream_write_error_code`
      - `SSL_get_conn_close_info`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_ssl_async_quic_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_async_quic_contract -FEtmp/openssl_ssl_async_quic_contract -otmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract tests/test_openssl_ssl_async_quic_contract.pas && ./tmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract`
- Expected:
  - FAIL because `LoadOpenSSLSSL` currently does not bind this family

**Step 3: Write minimal implementation**
- In `src/fafafa.ssl.openssl.api.ssl.pas`:
  - bind async helpers
  - bind QUIC helpers
  - do not change broader `osmSSL` readiness semantics in this batch

**Step 4: Run GREEN**
- Re-run the focused contract
- Expected:
  - PASS on current host `libssl.so.3`

---

### Task 4: Regression Chain And Ledger

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `docs/plans/2026-04-06-openssl-ssl-runtime-wave2.md`

**Step 1: Run focused regression chain**
- Run:
  - `mkdir -p tmp/openssl_ssl_load_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_load_contract -FEtmp/openssl_ssl_load_contract -otmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract tests/test_openssl_ssl_load_contract.pas && ./tmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract`
  - `mkdir -p tmp/openssl_ssl_unload_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_unload_contract -FEtmp/openssl_ssl_unload_contract -otmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract tests/test_openssl_ssl_unload_contract.pas && ./tmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract`
  - `mkdir -p tmp/openssl_ssl_early_data_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_early_data_contract -FEtmp/openssl_ssl_early_data_contract -otmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract tests/test_openssl_ssl_early_data_contract.pas && ./tmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract`
  - `mkdir -p tmp/openssl_ssl_padding_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_padding_contract -FEtmp/openssl_ssl_padding_contract -otmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract tests/test_openssl_ssl_padding_contract.pas && ./tmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract`
  - `mkdir -p tmp/openssl_ssl_async_quic_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_async_quic_contract -FEtmp/openssl_ssl_async_quic_contract -otmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract tests/test_openssl_ssl_async_quic_contract.pas && ./tmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.api.ssl.pas src/fafafa.ssl.openssl.backed.pas tests/test_openssl_ssl_early_data_contract.pas tests/test_openssl_ssl_padding_contract.pas tests/test_openssl_ssl_async_quic_contract.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-06-openssl-ssl-runtime-wave2.md`

**Step 3: Update ledgers**
- In `task_plan.md`:
  - mark each wave2 batch complete
- In `findings.md`:
  - record which public promises were tightened
  - record any host-specific optional surfaces
- In `progress.md`:
  - record RED/GREEN evidence and repo baseline outputs

**Expected Outcome:**
- `LoadOpenSSLSSL` 不再漏绑宿主已导出的 `early-data` / `keylog` / `record-padding` / `async` / `QUIC` helper families
- `ZeroRTTSupport` / `EarlyDataSupport` 不再只按版本号宣称稳定，而是和实际 helper readiness 对齐
- focused SSL contracts 从“只锁住部分 helper family”扩展为“对剩余高价值 family 有明确边界”
- repo baseline 继续保持绿
