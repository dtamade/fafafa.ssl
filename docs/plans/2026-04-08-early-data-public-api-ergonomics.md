# Early-Data Public API Ergonomics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不重开 early-data 核心行为与后端实现的前提下，把 TLS 1.3 early-data 的关键枚举/接口补进 `fafafa.ssl` 主入口，并提供最小 helper 层，减少用户侧对 `fafafa.ssl.base` 与 `Supports(...)` 样板的依赖。

**Architecture:** 保持现有 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` 作为行为真值，不新增新的核心接口。`fafafa.ssl` 只负责 re-export 现有 public optional interfaces 和相关类型；`TSSLHelper` 追加最小静态 helper，用于 capability probe、optional-interface 获取、以及上下文级 early-data 配置包装。FreePascal backend 继续作为 Linux-safe focused contract 探针。

**Tech Stack:** FreePascal (ObjFPC), public-unit re-export, static helper methods, FreePascal backend contract tests, compile-all verification.

---

## Summary

- 当前 early-data 的真实行为链已经存在：
  - `ISSLEarlyDataContext`
  - `ISSLEarlyDataConnection`
  - `TSSLContextBuilder.WithClientEarlyData(...)`
  - `TSSLContextBuilder.WithServerEarlyDataPolicy(...)`
  - `TSSLContextBuilder.WithServerMaxEarlyDataSize(...)`
  - FreePascal backend transport / policy / anti-replay contract
- 但 public ergonomics 仍有两个真实缺口：
  - `fafafa.ssl` 主入口没有 re-export：
    - `TSSLEarlyDataStatus`
    - `TSSLEarlyDataServerPolicy`
    - `ISSLEarlyDataContext`
    - `ISSLEarlyDataConnection`
  - 用户侧如果只 `uses fafafa.ssl;`，仍要下钻到 `fafafa.ssl.base`，并重复写 `Supports(...)`
- 因此本批继续克制：
  - 不改 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` contract
  - 不改 backend early-data accept/reject / anti-replay 逻辑
  - 只补主入口 re-export 与最小 helper ergonomic layer

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 public-unit re-export + helper ergonomics。
2. 先在 public API contract 上加 RED，证明主入口当前缺 early-data re-export / helper。
3. 最小实现只改 `src/fafafa.ssl.pas`、`src/fafafa.ssl.factory.pas`。
4. 跑 focused tests、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Early-Data Public API Ergonomics

**Files:**
- Add: `tests/test_early_data_public_api_contract.pas`

**Step 1: Write the failing public-API contract**
- 在 `tests/test_early_data_public_api_contract.pas`：
  - 只 `uses fafafa.ssl`, `fafafa.ssl.context.builder`, `fafafa.ssl.freepascal.lib`
  - 不直接引用 `fafafa.ssl.base`
  - 覆盖：
    - 主入口能直接引用：
      - `TSSLEarlyDataStatus`
      - `TSSLEarlyDataServerPolicy`
      - `ISSLEarlyDataContext`
      - `ISSLEarlyDataConnection`
    - `TSSLHelper` 暴露最小 helper：
      - `SupportsEarlyDataContext(...)`
      - `SupportsEarlyDataConnection(...)`
      - `TryGetEarlyDataContext(...)`
      - `TryGetEarlyDataConnection(...)`
      - `ConfigureClientEarlyData(...)`
      - `ConfigureServerEarlyData(...)`
      - `GetEarlyDataStatus(...)`
      - `GetEarlyDataLimit(...)`
  - 用 `sslFreePascal` 做 runtime probe：
    - client context 应支持 early-data context helper
    - client connection 应支持 early-data connection helper
    - helper 改动 client flag / server policy / server max-size 后可观察
    - default connection status / limit 可读

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/early_data_public_api_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/early_data_public_api_contract -FEtmp/early_data_public_api_contract -otmp/early_data_public_api_contract/test_early_data_public_api_contract tests/test_early_data_public_api_contract.pas && ./tmp/early_data_public_api_contract/test_early_data_public_api_contract`
- Expected:
  - RED 聚焦暴露：
    - `fafafa.ssl` 未 re-export early-data 类型/接口
    - `TSSLHelper` 缺少 early-data ergonomic helpers

### Task 2: Implement Minimal Early-Data Public Ergonomics

**Files:**
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.factory.pas`

**Step 1: Re-export early-data public types**
- 在 `src/fafafa.ssl.pas`：
  - re-export：
    - `TSSLEarlyDataStatus`
    - `TSSLEarlyDataServerPolicy`
    - `ISSLEarlyDataContext`
    - `ISSLEarlyDataConnection`

**Step 2: Add helper ergonomic layer**
- 在 `src/fafafa.ssl.factory.pas` 的 `TSSLHelper`：
  - 增加最小 helper：
    - `SupportsEarlyDataContext(...)`
    - `SupportsEarlyDataConnection(...)`
    - `TryGetEarlyDataContext(...)`
    - `TryGetEarlyDataConnection(...)`
    - `ConfigureClientEarlyData(...)`
    - `ConfigureServerEarlyData(...)`
    - `GetEarlyDataStatus(...)`
    - `GetEarlyDataLimit(...)`
  - 保持语义：
    - 不支持对应 optional interface 时返回 `False`
    - `GetEarlyDataStatus(...)` 回退 `sslEarlyDataNone`
    - `GetEarlyDataLimit(...)` 回退 `0`
  - 不新增 queue/send helper，避免把 session/resumption 语义一起扩进这批

**Step 3: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify Broadly And Close Out

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused regressions + core compile**
- Run:
  - `mkdir -p tmp/early_data_public_api_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/early_data_public_api_contract -FEtmp/early_data_public_api_contract -otmp/early_data_public_api_contract/test_early_data_public_api_contract tests/test_early_data_public_api_contract.pas && ./tmp/early_data_public_api_contract/test_early_data_public_api_contract`
  - `mkdir -p tmp/context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_early_data_contract -FEtmp/context_builder_early_data_contract -otmp/context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-early-data-public-api-ergonomics.md src/fafafa.ssl.pas src/fafafa.ssl.factory.pas tests/test_early_data_public_api_contract.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- `fafafa.ssl` 主入口可直接使用 early-data 关键枚举/接口
- 用户代码不需要下钻 `fafafa.ssl.base` 才能声明 early-data optional interfaces
- `TSSLHelper` 提供最小 early-data probe/config ergonomic layer
- focused contracts 与 `compile_all_modules.py` 通过
- working-memory files 记录 RED/GREEN evidence 和 closeout notes
