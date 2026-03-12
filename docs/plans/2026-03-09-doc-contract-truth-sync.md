# Contract Truth Sync Plan

**Goal**
- 把 Milestone 1 已落地的 builder / factory / backend 合同写进公共入口文档，避免代码已收口而 README 仍停留在旧语义。

**Architecture**
- 在 `README.md` 给出最短可执行 contract：builder 单次解析 backend、library default 与 request/context scope 分离、owner fields normalize。
- 在 `docs/README.md` 给文档入口页补“当前语义真相”，让读者先看到稳定规则，再跳深入文档。
- 在 `docs/reference/ARCHITECTURE.md` 记录更细的作用域边界：single-resolve backend、request-only material、library-scoped logging、owner fields、runtime-only dead fields。
- 这是文档同步 wave，不改行为；验证以 `git diff --check` 为主，并复用前一波已通过的 focused suites 与 `compile_all_modules` 结果。

**Files**
- Add: `docs/plans/2026-03-09-doc-contract-truth-sync.md`
- Modify: `README.md`
- Modify: `docs/README.md`
- Modify: `docs/reference/ARCHITECTURE.md`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 从源码与 focused contracts 提炼当前 builder/default-config 规则。
2. 更新根 README 的短入口说明。
3. 更新 docs 首页与架构参考文档。
4. 跑 diff 健康检查。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- 外部读者能看见 `TSSLContextBuilder` 会先解析一次 concrete backend，再复用给 context/store。
- `ISSLLibrary.SetDefaultConfig(...)` 与 `TSSLFactory.CreateContext(const AConfig)` 的职责边界明确可见。
- `GetDefaultConfig(...)` / owner fields 的 normalize 语义有文档锚点，不再只能靠读测试和源码理解。
