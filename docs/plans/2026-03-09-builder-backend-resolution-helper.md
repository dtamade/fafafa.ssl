# Builder Backend Resolution Helper Plan

**Goal**
- 抽出 `TSSLContextBuilderImpl` 的共享 backend-resolution helper，统一 `BuildClient` / `BuildServer` 对 auto / explicit / default 三条路径的 concrete backend 解析语义。
- 顺手修复 `WithBackend(sslAutoDetect)` 在 `WithSystemRoots` 路径下仍把 `sslAutoDetect` 传给 store 创建、导致 context/store backend 可漂移的问题。

**Architecture**
- 先用 drifting fake backend 写 RED，钉住 `explicit sslAutoDetect` 场景下 context/store 必须共用同一 concrete backend。
- 再把 `BuildClient` / `BuildServer` 里重复的 backend 解析分支收口到私有 helper，只返回一次性解析后的 concrete backend 与已创建 context。
- 保持 build-specific 差异（server 证书校验、client SNI 等）不动，避免语义扩散。

**Files**
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `tests/test_context_builder_backend_store_consistency.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/plans/2026-03-current-summary.md`

**Steps**
1. 写 RED：`WithBackend(sslAutoDetect)` + drifting default fake backend 下，client/server 的 store backend 不得漂移。
2. 跑 focused test，确认 explicit branch 仍把 `sslAutoDetect` 作为未解析 backend 继续向下传递。
3. 在 builder 中抽共享 helper，统一返回 resolved concrete backend。
4. 让 `BuildClient` / `BuildServer` 只消费 helper 输出，移除重复分支。
5. 跑 focused suites + `python3 -u scripts/compile_all_modules.py`。
6. 回写 plan / findings / progress / current summary。

**Expected Outputs**
- 新增 focused contract 覆盖 explicit-`sslAutoDetect` 漂移场景。
- `BuildClient` / `BuildServer` 不再各自维护 backend 解析分支。
- compile-all 与 config/builder focused suites 继续通过。
