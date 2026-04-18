# Factory Config Early-Data Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不新增核心接口的前提下，把现有 TLS 1.3 early-data 可选 surface 从 builder/context 扩到 `TSSLConfig` 与 `TSSLFactory.CreateContext(...)` 路径，消除 config-driven/default-config 入口上的 early-data 配置漂移。

**Architecture:** 继续复用已经存在的 `ISSLEarlyDataContext` 可选接口，不改 backend 行为真值。`TSSLConfig` 只承载 client/server early-data 默认值；`TSSLFactory` 成为统一的配置应用层，在 context 支持 `ISSLEarlyDataContext` 时把 config 值打入 context。默认值保持保守：client early-data 关闭、server policy = `Reject`、server max size = `0`。

**Tech Stack:** FreePascal (ObjFPC), `TSSLConfig` record, `TSSLFactory`, FreePascal backend contract tests, default-config tests, compile-all verification.

---

## Summary

- 当前 early-data 真值链已经在 builder/context 上完整：
  - `ISSLEarlyDataContext`
  - `TSSLContextBuilder.WithClientEarlyData(...)`
  - `TSSLContextBuilder.WithServerEarlyDataPolicy(...)`
  - `TSSLContextBuilder.WithServerMaxEarlyDataSize(...)`
- 但 `TSSLConfig` / factory path 仍然缺席：
  - `TSSLConfig` 没有对应字段
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 不会应用 early-data defaults
  - `TSSLFactory.CreateContext(const AConfig)` 也不会应用 one-shot early-data config
- 因此本批只做 config/factory parity，不重复改 backend early-data 行为：
  - `TSSLConfig` 增加 `ClientEarlyDataEnabled`
  - `TSSLConfig` 增加 `ServerEarlyDataPolicy`
  - `TSSLConfig` 增加 `ServerMaxEarlyDataSize`
  - `TSSLFactory` 在 context 支持 `ISSLEarlyDataContext` 时应用这些字段
  - one-shot config 仍不得污染 shared default config

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 `TSSLConfig` / factory parity。
2. 先在 default-config / factory isolation tests 上加 RED。
3. 最小实现只改 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.pas`、`src/fafafa.ssl.debug.utils.pas`。
4. 跑 focused tests、`compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Config/Factory Early-Data Parity

**Files:**
- Modify: `tests/config/test_default_config.pas`
- Modify: `tests/test_factory_logic.pas`
- Add: `tests/test_factory_config_early_data_isolation.pas`

**Step 1: Extend public default-config contract**
- 在 `tests/config/test_default_config.pas`：
  - 断言 `CreateDefaultConfig(...)` 默认 `ClientEarlyDataEnabled = False`
  - 断言 `CreateDefaultConfig(...)` 默认 `ServerEarlyDataPolicy = sslEarlyDataServerReject`
  - 断言 `CreateDefaultConfig(...)` 默认 `ServerMaxEarlyDataSize = 0`

**Step 2: Extend raw config record contract**
- 在 `tests/test_factory_logic.pas`：
  - 断言 `TSSLConfig` 的 early-data 字段可读写
  - 断言 `NormalizeConfig(...)` 不会把默认 `Reject/0/False` 漂移成其它值

**Step 3: Add factory parity / isolation contract**
- 在 `tests/test_factory_config_early_data_isolation.pas`：
  - 使用 `sslFreePascal`，避免外部库依赖
  - 覆盖 default-config path：
    - `ISSLLibrary.SetDefaultConfig(...)`
    - `TSSLFactory.CreateContext(sslCtxClient, sslFreePascal)` 观察 client early-data default
    - `TSSLFactory.CreateContext(sslCtxServer, sslFreePascal)` 观察 server policy/max-size default
  - 覆盖 one-shot path：
    - `TSSLFactory.CreateContext(AConfig)` 观察 one-shot early-data config 生效
    - 后续 default-path context 不能继承 one-shot 值

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic`
  - `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation`
- Expected:
  - RED 聚焦暴露：
    - `TSSLConfig` 缺少 early-data 字段
    - factory path 没有把 config 打到 `ISSLEarlyDataContext`

### Task 2: Implement Minimal Config/Factory Parity

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.debug.utils.pas`

**Step 1: Extend public config record**
- 在 `src/fafafa.ssl.base.pas`：
  - `TSSLConfig` 增加：
    - `ClientEarlyDataEnabled: Boolean`
    - `ServerEarlyDataPolicy: TSSLEarlyDataServerPolicy`
    - `ServerMaxEarlyDataSize: Cardinal`

**Step 2: Apply early-data config in factory**
- 在 `src/fafafa.ssl.factory.pas`：
  - 增加内部 helper，把 `TSSLConfig` early-data 字段应用到 `ISSLEarlyDataContext`
  - 在两个 `CreateContext` overload 中都调用该 helper
  - 保持语义：
    - 不支持 `ISSLEarlyDataContext` 的 backend 直接跳过
    - one-shot `CreateContext(const AConfig)` 仍只影响返回的 context，不改 shared default config

**Step 3: Keep defaults and diagnostics aligned**
- 在 `src/fafafa.ssl.pas`：
  - `CreateDefaultConfig(...)` fallback/default path 保持 early-data 默认值真实可读
- 在 `src/fafafa.ssl.debug.utils.pas`：
  - `DumpSSLConfig(...)` 输出新的 early-data 配置字段

**Step 4: Run GREEN**
- Re-run Task 1 commands
- Expected:
  - PASS

### Task 3: Verify Broadly And Close Out

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused regressions + core compile**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic`
  - `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-factory-config-early-data-parity.md src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.debug.utils.pas tests/config/test_default_config.pas tests/test_factory_logic.pas tests/test_factory_config_early_data_isolation.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- `TSSLConfig` 可表达 client/server early-data defaults
- `CreateDefaultConfig(...)` 默认值与 builder/context 默认值一致
- `TSSLFactory.CreateContext(...)` 能把 early-data config 打到支持 `ISSLEarlyDataContext` 的 context
- one-shot config 不会污染 shared library default config
- focused tests 与 `compile_all_modules.py` 通过
- working-memory files 记录 RED/GREEN evidence 和 closeout notes
