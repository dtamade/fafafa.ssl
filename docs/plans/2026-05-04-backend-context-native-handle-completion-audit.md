# Backend Context Native-Handle Completion Audit Plan

**Goal:** 把 context-level `ISSLNativeHandleAccess` public contract 补成 cross-backend completion audit：基于 C 库的 backend context 必须显式暴露该接口，纯 Pascal backend 继续保持 absent。

**Architecture:** 这批优先做 completion audit，不预设一定有生产代码改动。先在 `tests/contract/test_backend_contract.pas` 增加跨后端契约，把 context-level native-handle 暴露、backend type、以及最小句柄有效性锁进统一约束；只有 focused RED 证明还存在真实漂移时，才改后端 context 类声明或 getter。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove whether context native-handle drift still exists

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 这类 C-library / OS-native backend 的 context 应支持 `ISSLNativeHandleAccess`
- `GetBackendType` 必须与 backend 枚举一致
- `GetNativeHandle` 不应返回 `nil`
- `FreePascal` backend context 不应暴露 `ISSLNativeHandleAccess`

Expected possibilities:
- 如果出现 RED，优先判断是类声明漏挂接口，还是 getter/validity 语义不一致
- 如果直接全绿，这批就作为 completion audit 证据提交

## Task 2: GREEN - only if the new contract proves real drift

Possible change directions:
- 类声明漂移：补/移除 `ISSLNativeHandleAccess`
- getter/validity 漂移：修正 `GetBackendType` / `GetNativeHandle` / `IsNativeHandleValid`

Constraints:
- 不扩大到 connection-level native-handle（已收口）
- 不改 certificate/store/session 的 native-handle 设计
- 不让 `FreePascal` backend context 暴露伪 native handle

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- context-level `ISSLNativeHandleAccess` 已被 cross-backend contract 锁定
- 若无 RED，形成 completion audit 证据
- 若有 RED，完成最小修复并让 focused contract、compile gate、minimal CI gate 全绿
