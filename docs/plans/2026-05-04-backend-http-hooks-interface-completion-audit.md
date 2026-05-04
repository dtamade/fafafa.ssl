# Backend HTTP Hooks Interface Completion Audit Plan

**Goal:** 把 context-level `ISSLHttpHooksAccess` public contract 补成 cross-backend completion audit：当前真正依赖 context HTTP hooks 的后端必须显式暴露该接口，其余后端继续保持 absent。

**Architecture:** 这批优先做 completion audit，不预设一定有生产代码改动。先在 `tests/contract/test_backend_contract.pas` 增加跨后端契约，把 context-level `ISSLHttpHooksAccess` 的 presence/absence 和最小 callback round-trip 锁进统一约束；只有 focused RED 证明还存在真实漂移时，才改后端 context 类声明或 setter/getter。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove whether HTTP hooks interface drift still exists

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `OpenSSL` / `FreePascal` context 必须支持 `ISSLHttpHooksAccess`
- `SetHTTPGetCallback` / `SetHTTPPostCallback` 后，`GetHTTPGetCallback` / `GetHTTPPostCallback` 必须能 round-trip
- `WolfSSL` / `MbedTLS` / `WinSSL` context 不应暴露该接口

Expected possibilities:
- 如果出现 RED，优先判断是类声明漏挂接口，还是 callback storage round-trip 漂移
- 如果直接全绿，这批就作为 completion audit 证据提交

## Task 2: GREEN - only if the new contract proves real drift

Possible change directions:
- 类声明漂移：补/移除 `ISSLHttpHooksAccess`
- setter/getter 漂移：修正 hook storage / round-trip 语义

Constraints:
- 不重开 online OCSP / CT / network transport 逻辑
- 不把 `ISSLHttpHooksAccess` 扩成所有后端都必须支持
- 不修改 builder，仅验证 direct context truth

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- context-level `ISSLHttpHooksAccess` 已被 cross-backend contract 锁定
- 若无 RED，形成 completion audit 证据
- 若有 RED，完成最小修复并让 focused contract、compile gate、minimal CI gate 全绿
