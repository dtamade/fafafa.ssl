# Milestone 1 Builder/Factory/Backend Semantics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 `builder` / `factory` / backend 三层之间关于默认后端、默认配置、请求级配置与上下文级配置的语义彻底收口。

**Architecture:** 先从最容易出错的 backend-resolution contract 入手，消掉 client/server 分叉和 implicit-default 漂移；再把 default-config 与 validation 的边界统一成少数共享 helper；最后将测试夹具平台化、把文档中的“当前真相”同步出来。整个里程碑坚持 TDD：每个子波次先 RED，最小 GREEN，再跑 compile-all 和相邻回归。

**Tech Stack:** FreePascal/FPC, Pascal core units in `src/`, standalone Pascal tests in `tests/`, shell/python verification scripts in `scripts/`.

---

### Task 1: 统一 backend resolution contract

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `tests/test_context_builder_backend_store_consistency.pas`
- Plan: `docs/plans/2026-03-09-builder-server-default-backend-store-consistency.md`
- Plan: `docs/plans/2026-03-09-builder-implicit-default-backend-resolution-consistency.md`

**Steps:**
1. 写 RED：implicit-default `WithSystemRoots` server 崩溃合同。
2. 跑 RED，确认崩在 `CreateCertificateStore(SelectedBackend)`。
3. 最小修复 `BuildServer` 初始化。
4. 写 RED：drifting default backend 导致 client/server context-store 漂移。
5. 跑 RED，确认 implicit-default 双次 autodetect 漂移。
6. 最小修复 implicit-default 先 resolve concrete backend 再复用。
7. 跑 `tests/test_context_builder_backend_store_consistency.pas` + config focused suites + `python3 -u scripts/compile_all_modules.py`。

### Task 2: 抽共享 builder backend helper

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `tests/test_context_builder_backend_store_consistency.pas`
- Plan: `docs/plans/2026-03-09-builder-backend-resolution-helper.md`

**Steps:**
1. 写 RED：client/server helper 前后行为不变合同。
2. 抽出统一 helper，覆盖 auto / explicit / default 三分支。
3. 清掉重复分支代码，保持 public behavior 不变。
4. 跑 focused + compile-all。

### Task 3: 收口 default-config / request-scope / context-scope 边界

**Files:**
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: backend libs under `src/fafafa.ssl.*.pas`
- Add/Modify: factory-focused tests under `tests/`
- Plan: `docs/plans/2026-03-09-factory-default-config-boundary-followup.md`

**Steps:**
1. 盘点仍可能跨 scope 泄漏的字段与 helper。
2. 写 RED：factory request/default/context boundary focused contracts。
3. 最小修复共享 helper 或边界检查。
4. 跑 factory + compile-all 回归。

### Task 4: 测试夹具平台化

**Files:**
- Create/Modify: `tests/framework/*` or `tests/helpers/*`（以现有结构为准）
- Modify: `tests/config/*.pas`
- Modify: `tests/test_context_builder_backend_store_consistency.pas`
- Plan: `docs/plans/2026-03-09-test-fixture-platformization.md`

**Steps:**
1. 盘点 fake backend / fixture 重复面。
2. 抽出公共 helper/unit，替代 include + 局部重复类型。
3. 保持 standalone test compile path 可用。
4. 跑受影响 suites + compile-all。

### Task 5: 文档与当前真相同步

**Files:**
- Modify: `README.md`
- Modify: `ARCHITECTURE.md`（如存在相关段落）
- Modify: `docs/README.md`
- Modify: `docs/plans/2026-03-current-summary.md`

**Steps:**
1. 将 backend-selection、implicit-default、default-config 边界写成明确 contract。
2. 补一页“当前真相/优先级/下一主线”。
3. 确认文档不与实现漂移。

### Milestone Gate

**Required verification:**
```bash
fpc -gl -Fu./src -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency
fpc -Fu./src -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone
fpc -Fu./src -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export
fpc -Fu./src -otmp/test_config_backend_selection_snapshot_semantics tests/config/test_config_backend_selection_snapshot_semantics.pas && ./tmp/test_config_backend_selection_snapshot_semantics
fpc -Fu./src -otmp/test_config_backend_selection_mode_normalization tests/config/test_config_backend_selection_mode_normalization.pas && ./tmp/test_config_backend_selection_mode_normalization
python3 -u scripts/compile_all_modules.py
```

**Exit criteria:**
- backend resolution contract 不再在 client/server 间分叉
- implicit-default 不再存在 context/store 双次 autodetect 漂移
- default-config 与 request/context scope 边界可描述、可测试
- fake backend 夹具可复用，新增测试无需再临时拼装
- 月度索引与核心文档能说明“当前真相”
