# Factory/Library Logging Scope Clarification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不新增核心接口的前提下，明确 `TSSLConfig.LogLevel` / `LogCallback` 的 owner boundary：它们只属于 library-default scope，不属于 `TSSLFactory.CreateContext(const AConfig)` 的 one-shot request scope。

**Architecture:** 保持现有 logging runtime 设计不变，不把 logging 配置错误地下沉到 context/request path。`ISSLLibrary.SetDefaultConfig(...)` 与 `ISSLLibrary.SetLogCallback(...)` 共同维护 library-default logging state；`CreateDefaultConfig(...)` 继续作为 request-safe config constructor，对 library-scoped logging 字段做安全清理；`TSSLFactory.CreateContext(const AConfig)` 对 scope-mismatched logging 字段 fail-fast。

**Tech Stack:** FreePascal (ObjFPC), `TSSLFactory`, `CreateDefaultConfig(...)`, backend library contracts, FreePascal backend focused tests, compile-all verification.

---

## Summary

- 当前 logging 真值已经明确是 library-default scope：
  - backend `InternalLog(...)` 走 `FLogLevel` / `FLogCallback`
  - `SetDefaultConfig(...)` 会更新 runtime logging state
- 但还有两个 drift：
  - `CreateContext(const AConfig)` 还会静默接受 request-path logging 字段
  - 多个 backend 的 `SetLogCallback(...)` 只改 runtime callback，不同步 `GetDefaultConfig.LogCallback`
- 本批只做 scope clarification，不引入 per-context logging：
  - one-shot request config 遇到 logging 字段直接拒绝
  - library-default path 保持 round-trip 和 runtime dispatch 一致
  - `CreateDefaultConfig(...)` 继续 request-safe，不回漏 library-scoped logging defaults

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 factory/library logging scope clarification。
2. 先在 `CreateDefaultConfig` 与 factory/library logging contract 上加 RED。
3. 最小实现只改 `src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.pas` 和 backend library units。
4. 跑 focused tests、相邻回归、`compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Logging Scope Clarification

**Files:**
- Modify: `tests/config/test_default_config.pas`
- Add: `tests/test_factory_logging_scope_clarification.pas`

**Step 1: Strengthen `CreateDefaultConfig(...)` request-safe contract**
- 在 `tests/config/test_default_config.pas`：
  - 显式把 `sslFreePascal` library default logging 改成自定义 `LogLevel` / `LogCallback`
  - 临时将默认库切到 `sslFreePascal`
  - 断言 `CreateDefaultConfig(...)` 返回：
    - `LogLevel = sslLogError`
    - `LogCallback = nil`

**Step 2: Add factory/library logging scope contract**
- 在 `tests/test_factory_logging_scope_clarification.pas`：
  - 使用 `sslFreePascal`，避免外部库依赖
  - 覆盖 request path：
    - `TSSLFactory.CreateContext(AConfig)` 遇到非默认 `LogLevel` 时抛 `ESSLConfigurationException`
    - `TSSLFactory.CreateContext(AConfig)` 遇到非空 `LogCallback` 时抛 `ESSLConfigurationException`
  - 覆盖 library-default path：
    - `SetDefaultConfig(...)` 后 `GetDefaultConfig.LogLevel` round-trip 可见
    - `SetLogCallback(...)` 后 `GetDefaultConfig.LogCallback` round-trip 可见
    - `Log(...)` 仅在 `ALevel <= configured LogLevel` 时调用 callback

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logging_scope && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logging_scope -FEtmp/factory_logging_scope -otmp/factory_logging_scope/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/factory_logging_scope/test_factory_logging_scope_clarification`
- Expected:
  - RED 聚焦暴露：
    - `CreateDefaultConfig(...)` 泄漏 library-scoped logging defaults
    - request path 仍接受 logging 字段
    - `SetLogCallback(...)` 没有同步 `GetDefaultConfig.LogCallback`

### Task 2: Implement Minimal Logging Scope Clarification

**Files:**
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `src/fafafa.ssl.mbedtls.lib.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`

**Step 1: Fail fast on request-path logging fields**
- 在 `src/fafafa.ssl.factory.pas`：
  - 为 `CreateContext(const AConfig)` 增加 logging-scope 校验
  - 非默认 `LogLevel` 或非空 `LogCallback` 时抛 `ESSLConfigurationException.CreateWithContext(...)`
  - 错误码保持 `sslErrConfiguration`

**Step 2: Keep `CreateDefaultConfig(...)` request-safe**
- 在 `src/fafafa.ssl.pas`：
  - 无论 backend default config 如何，返回前都清理 request-path-illegal logging 字段：
    - `LogLevel := sslLogError`
    - `LogCallback := nil`

**Step 3: Keep library-default snapshot/runtime aligned**
- 在 backend library units：
  - `SetLogCallback(...)` 同步 `FDefaultConfig.LogCallback`
  - 保持 `Log(...)` 仍走现有 `InternalLog(...)` gating

**Step 4: Run GREEN**
- Re-run Task 1 commands
- Expected:
  - PASS

### Task 3: Verify Broadly And Close Out

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused regressions + adjacent coverage**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic`
  - `mkdir -p tmp/factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_server_name_isolation -FEtmp/factory_config_server_name_isolation -otmp/factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - `mkdir -p tmp/factory_logging_scope && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logging_scope -FEtmp/factory_logging_scope -otmp/factory_logging_scope/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/factory_logging_scope/test_factory_logging_scope_clarification`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-factory-library-logging-scope-clarification.md src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.wolfssl.lib.pas tests/config/test_default_config.pas tests/test_factory_logging_scope_clarification.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- `CreateDefaultConfig(...)` 对 request path 保持 logging-safe
- `TSSLFactory.CreateContext(const AConfig)` 不再静默接受 library-scoped logging 字段
- `ISSLLibrary.SetLogCallback(...)` 与 `GetDefaultConfig` snapshot 一致
- backend logging dispatch 继续遵守 `ALevel <= LogLevel`
- focused regressions、adjacent regressions、`compile_all_modules.py` 通过
- working-memory files 记录 RED/GREEN evidence 和 closeout notes
