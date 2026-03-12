# Test Fixture Platformization Plan

**Goal**
- 把当前散落在单测内部和 `tests/config/` 局部 include 里的 fake backend fixture 收口到 `tests/helpers/`，降低后续继续补 contract 时的重复复制成本。

**Architecture**
- 先把 `tests/config/test_fake_default_backend_fixture.inc` 提升为 `tests/helpers/` 共享 include，并让旧路径保留薄包装以避免一次性改太多测试。
- 再把 `tests/test_context_builder_backend_store_consistency.pas` 内联的 default/drifting fake backend fixture 抽成 `tests/helpers/test_backend_store_fake_fixture.inc`。
- 这是纯 refactor wave，不改行为，直接用现有 focused suites 做 safety net。

**Files**
- Add: `tests/helpers/test_fake_default_backend_fixture.inc`
- Add: `tests/helpers/test_backend_store_fake_fixture.inc`
- Modify: `tests/config/test_fake_default_backend_fixture.inc`
- Modify: `tests/test_context_builder_backend_store_consistency.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/plans/2026-03-current-summary.md`

**Steps**
1. 提升 config fake default backend include 到 `tests/helpers/`。
2. 抽 builder backend store fake fixture include。
3. 用旧 focused suites 做 safety net。
4. 跑 compile-all。
5. 回写 working memory。

**Expected Outputs**
- fake backend fixture 不再被单个测试程序“私有化”。
- config/build-path 与 builder/store consistency 两条线都能复用 helper include。
- 后续继续补 contract 时，不必再从单测里复制大段 fake backend 类型定义。
