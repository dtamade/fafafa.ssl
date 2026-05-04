# Backend Session Native-Handle Completion Audit Plan

**Goal:** 把 session-level `ISSLNativeHandleAccess` public contract 补成 focused completion audit：C-library backend 的 session wrapper 应稳定暴露 native-handle surface，纯 Pascal backend 继续保持 absent。

**Architecture:** 这批优先做 completion audit，不预设一定有生产代码改动。先在 `tests/contract/test_backend_contract.pas` 增加跨后端 session contract，锁住 session-level native-handle 暴露、backend type、以及 helper round-trip；只有 focused RED 证明还存在真实漂移时，才改 session 类声明或 native-handle helper 接线。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove whether session native-handle drift still exists

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `OpenSSL` / `WolfSSL` / `MbedTLS` 的 session probe 应支持 `ISSLNativeHandleAccess`
- `GetBackendType` 必须与 backend 枚举一致
- `GetNativeHandle` 不应返回 `nil`
- `TryGetNativeHandle` 必须能 round-trip 同一 handle
- `FreePascal` session 不应暴露 `ISSLNativeHandleAccess`
- `WinSSL` 在本批明确 skip，不把 Linux 结果外推成 Windows session truth

Probe strategy:
- `OpenSSL`: `SSL_SESSION_new`
- `WolfSSL` / `MbedTLS`: 最小 wrapped opaque-handle probe
- `FreePascal`: `TFreePascalSession.Create`

Expected possibilities:
- 如果出现 RED，优先判断是类声明漏挂接口，还是 helper/handle 语义不一致
- 如果直接全绿，这批就作为 completion audit 证据提交

## Task 2: GREEN - only if the new contract proves real drift

Possible change directions:
- 类声明漂移：补/移除 `ISSLNativeHandleAccess`
- helper 漂移：修正 `GetBackendType` / `GetNativeHandle` / `TryGetNativeHandle` 路径

Constraints:
- 不重开完整 session resumption runtime proof
- 不把 `WinSSL` duplicate session truth source 混到 Linux 收口批次
- 不让 `FreePascal` backend session 暴露伪 native handle

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- session-level `ISSLNativeHandleAccess` 已被 cross-backend contract 锁定
- `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的当前 public truth 有测试证据
- `WinSSL` 平台边界与风险已明确记录，不被误写成已证实
- 若无 RED，形成 completion audit 证据
- 若有 RED，完成最小修复并让 focused contract、compile gate、minimal CI gate 全绿
