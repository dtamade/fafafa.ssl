# MbedTLS Renegotiate Explicit Unsupported Semantics Plan

**Goal:** 收口 MbedTLS connection 的 `Renegotiate` public contract，让它不再只是静默 `False`，而会给出显式 `sslErrUnsupported` 错误分类和稳定诊断文案。

**Architecture:** 这批不实现真正的 renegotiation，也不扩大到 capability 矩阵、builder、或其它握手路径。最终只做两件事：先在 `tests/test_mbedtls_framework.pas` 补一个 focused RED，锁住 `Renegotiate` 的返回值、错误分类和文案；然后在 `src/fafafa.ssl.mbedtls.connection.pas` 做最小修复，用 `RecordError(...)` 发布 unsupported 语义，并让 `DoGetError` / `DoGetVerifyResultString` 在没有 native error 时仍能把这条语义返回给调用方。最后跑 focused framework test、compile gate 和 minimal CI gate，并写回台账。

**Files:**
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove renegotiate is still a silent failure

Run:

```bash
fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas
./tmp/mbedtls_framework_units/test_mbedtls_framework
```

Expected RED:
- `Renegotiate returns false before handshake` 仍然通过
- 但错误分类不是 `sslErrUnsupported`
- 或诊断文案为空 / 不包含 `renegotiation`

## Task 2: GREEN - make unsupported semantics explicit

Change:
- `src/fafafa.ssl.mbedtls.connection.pas`
  - `DoRenegotiate` 调用 `RecordError(sslErrUnsupported, ...)`
  - `DoGetError` 在无 native error 但已有语义错误时优先返回 `FLastErrorCode`
  - `DoGetVerifyResultString` 在已有语义错误文案时优先返回 `FLastErrorString`

Constraints:
- 不实现真实 renegotiation
- 不新增 capability 字段
- 不改 verify 成功路径或正常证书校验文案

## Task 3: Verification

Run:

```bash
fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas
./tmp/mbedtls_framework_units/test_mbedtls_framework
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `Renegotiate` 不再是静默 `False`
- MbedTLS connection 在该路径上发布 `sslErrUnsupported`
- `GetVerifyResultString` 暴露稳定的 renegotiation 诊断文案
- focused framework test、compile gate、minimal CI gate 全绿
- 台账同步到新的 MbedTLS renegotiate contract truth
