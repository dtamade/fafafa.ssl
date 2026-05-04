# Backend Context Optional Interface Completion Audit Plan

**Goal:** 把 context-level `ISSLEarlyDataContext` / `ISSLServerOCSPStaplingContext` 的 public contract 完整锁住：不只是 unsupported backend 必须 absent，capability 为 usable 的 backend 也必须真的暴露对应接口。

**Architecture:** 这批优先做 completion audit，不预设一定有实现改动。先在 `tests/contract/test_backend_contract.pas` 增加跨后端契约，把 capability 与 context optional interface 暴露做成双向约束；只有当 focused RED 证明还有漂移时，才改生产代码。范围只锁定 context-level optional surface，不扩到 runtime fetch、builder 新功能、或额外 capability 字段。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove remaining context-level capability drift if any

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `EarlyDataSupport <> sslSupportNone` 的 backend，client context 必须暴露 `ISSLEarlyDataContext`
- `EarlyDataSupport = sslSupportNone` 的 backend，client context 不应暴露 `ISSLEarlyDataContext`
- `OCSPStaplingSupport <> sslSupportNone` 的 backend，server context 必须暴露 `ISSLServerOCSPStaplingContext`
- `OCSPStaplingSupport = sslSupportNone` 的 backend，server context 不应暴露 `ISSLServerOCSPStaplingContext`

Expected possibilities:
- 如果出现 RED，优先定位是 capability 过宽，还是 context class 没挂接口
- 如果直接全绿，这批就作为 completion audit 证据提交

## Task 2: GREEN - only if the new contract proves real drift

Possible change directions:
- capability truth 过宽：收紧 `GetCapabilities`
- interface 漂移：补/移除对应 context optional interface

Constraints:
- 不新增新的 early-data / OCSP 功能
- 不重写 builder
- 不扩大到 connection-level 已收口批次

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- context-level early-data / server-OCSP optional interface 已被 capability 双向锁定
- 若无 RED，形成 completion audit 证据
- 若有 RED，完成最小修复并让 focused contract、compile gate、minimal CI gate 全绿
