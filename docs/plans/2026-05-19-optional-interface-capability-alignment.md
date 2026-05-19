# 2026-05-19 Optional Interface Capability Alignment

## Goal

收掉 backend capability 发布与 public optional interface 暴露之间的结构性漂移，避免出现：

- capability 说 `none`
- 但 `Supports(..., optional-interface, ...)` 仍返回 `True`

这种会直接误导 builder / factory / caller contract 的情况。

## Scope

- 只处理当前已确认存在结构性风险的 optional interface 暴露面：
  - OpenSSL `ISSLEarlyDataContext`
  - OpenSSL `ISSLEarlyDataConnection`
  - OpenSSL `ISSLServerOCSPStaplingContext`
  - WolfSSL `ISSLServerOCSPStaplingContext`
- 用 capability-aware `GetInterface(...)` 做最小收口
- 新增 focused source contract
- 不重构 runtime capability 实现本身
- 不改动 MbedTLS / FreePascal / WinSSL 的实现路径

## Files

- `src/fafafa.ssl.openssl.context.pas`
- `src/fafafa.ssl.openssl.connection.pas`
- `src/fafafa.ssl.wolfssl.context.pas`
- `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
- `docs/plans/2026-05-19-optional-interface-capability-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `tests/contract/test_backend_contract.pas` 已明确表达当前公共心智：
  - `EarlyDataSupport = none` 时，不应暴露 `ISSLEarlyDataContext / ISSLEarlyDataConnection`
  - `OCSPStaplingSupport = none` 时，不应暴露 `ISSLServerOCSPStaplingContext`
- WolfSSL 的 early-data 已经走了 capability-gated subclass 路线
- 但 OpenSSL / WolfSSL 仍有部分类直接实现 optional interface，存在 capability 为 `none` 时 interface 仍被 `Supports(...)` 看见的风险

## Steps

1. 新增 focused source contract，对 OpenSSL / WolfSSL optional interface gate 先做 RED
2. 最小修改相关类，在 `GetInterface(...)` 上按 capability 收口
3. 跑 focused contract 与 `git diff --check`

## Commands

```bash
bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh
bash tests/scripts/test_optional_interface_capability_alignment_contract.sh
git diff --check
```

## Expected Result

- OpenSSL context/connection 的 early-data optional interface 与 capability 对齐
- OpenSSL/WolfSSL server OCSP stapling context 只在 capability 非 `none` 时对外暴露
- builder / factory / source contract 对 optional interface 的假设重新一致
