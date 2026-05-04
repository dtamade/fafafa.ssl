# MbedTLS Feature Capability Runtime Consistency Plan

**Goal:** 收口 MbedTLS library 的 capability truth，让 `SNI` / `ALPN` / `SessionTickets` 的 capability 发布不再依赖硬编码，而是和真实 helper surface 对齐。

**Architecture:** 这批不新增新功能，不实现新的 TLS 行为，只修 library 层 capability 检测。先在 `tests/test_mbedtls_framework.pas` 里构造一个 deterministic helper-loss 场景：手动加载 MbedTLS 动态库、临时清空相关函数指针、再让 `TMbedTLSLibrary.Initialize` 基于当前 helper state 做检测。这个 RED 应该直接暴露当前硬编码的 `HasSNI` / `HasALPN` / `HasSessionTickets`。然后在 `src/fafafa.ssl.mbedtls.lib.pas` 做最小修复，让 `DetectCapabilities` 和 `GetCapabilities` 都基于 helper surface 发布 truth。最后跑 focused framework test、compile gate 和 minimal CI gate，并写回台账。

**Files:**
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove helper-loss still publishes false-positive capability

Run:

```bash
fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas
./tmp/mbedtls_framework_units/test_mbedtls_framework
```

Expected RED:
- 在 helper-loss 场景下，`SupportsSNI` / `SNISupport` 仍错误地保持可用
- 或 `SupportsALPN` / `ALPNSupport` 仍错误地保持可用
- 或 `SupportsSessionTickets` / `SessionTicketsSupport` 仍错误地保持可用

## Task 2: GREEN - make capability detection follow helper truth

Change:
- `src/fafafa.ssl.mbedtls.lib.pas`
  - `HasSNI := Assigned(mbedtls_ssl_set_hostname)`
  - `HasALPN := Assigned(mbedtls_ssl_conf_alpn_protocols) and Assigned(mbedtls_ssl_get_alpn_protocol)`
  - `HasSessionTickets := Assigned(mbedtls_ssl_get_session) and Assigned(mbedtls_ssl_set_session)`
  - `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 基于上述布尔值发布 `stable` 或 `none`

Constraints:
- 不新增新的 library API binding
- 不改 connection/context 的 SNI/ALPN/session 行为
- 不扩大成完整 runtime resumption 审计

## Task 3: Verification

Run:

```bash
fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas
./tmp/mbedtls_framework_units/test_mbedtls_framework
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- helper-loss 场景下 capability 不再错误发布 `SNI` / `ALPN` / `SessionTickets`
- `IsFeatureSupported` 与 `GetCapabilities` 的 support-level truth 对齐
- focused framework test、compile gate、minimal CI gate 全绿
- 台账同步到新的 MbedTLS capability truth

## Execution Result

- RED 复现成功：
  - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
  - `./tmp/mbedtls_framework_units/test_mbedtls_framework`
  - 初次结果：`Total: 96 / Passed: 87 / Failed: 9 / Rate: 90.6%`
  - 失败点正好集中在 helper-loss 后仍错误发布的 `SNI` / `ALPN` / `SessionTickets`
- GREEN 最小修复：
  - `src/fafafa.ssl.mbedtls.lib.pas`
    - `DetectCapabilities` 改为按 helper assignment 检测 `HasSNI` / `HasALPN` / `HasSessionTickets`
    - `GetCapabilities` 的 `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 改为按同一组布尔值发布 `stable` 或 `none`
- GREEN 验证结果：
  - focused framework test：`Total: 96 / Passed: 96 / Failed: 0 / Rate: 100.0%`
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：compile gate `185/185`、模块回归 `17/17`、phase2 dry-run 通过，最终 `[PASS] minimal CI gate finished`
