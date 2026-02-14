# FreePascal Unsupported Error Semantics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal backend 在“明确不支持路径”上的错误语义，使其不再误导为 “not implemented yet”，并建立可执行回归测试。

**Architecture:** 采用最小改动策略：先在 `test_freepascal_backend_basic` 增加红测，覆盖 client/server 在 `PreferredVersion <> TLS13` 下的失败语义；再只修改 `MarkUnsupported` 的错误文案，保持错误码 `sslErrUnsupported` 不变；最后跑一组关键回归确保不引入副作用。

**Tech Stack:** FreePascal (ObjFPC), 程序级测试 (`tests/test_freepascal_backend_basic.pas`, `tests/test_freepascal_server_accept_skeleton.pas`), fafafa.ssl FreePascal backend。

---

## Scan Summary (2026-02-11)

### High-signal gaps
1. `src/fafafa.ssl.freepascal.connection.pas:1346`
   - `MarkUnsupported` 当前文案为 `is not implemented in FreePascal backend yet`，与 `sslErrUnsupported` 语义不一致。
2. `src/fafafa.ssl.freepascal.connection.pas:1440,1526,2193`
   - `DoConnect`/`DoAccept`/`DoRenegotiate` 的非 TLS1.3 路径使用 `MarkUnsupported`，因此会传播误导性文案。
3. `tests/test_freepascal_backend_basic.pas`
   - 已覆盖握手前读写 precondition，但尚未覆盖 unsupported 文案契约。

### Priority
- **P0:** 锁定 unsupported 文案语义（红测 + 最小修复）
- **P1:** 关键回归验证（server accept skeleton + unit runner）

---

### Task 1 (P0): Add failing tests for unsupported wording contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Write failing test assertions**
- 在 `test_freepascal_backend_basic` 增加两个场景：
  - client context 设置 `PreferredVersion=sslProtocolTLS12`，`Connect` 失败。
  - server context 设置 `PreferredVersion=sslProtocolTLS12`，`Accept` 失败。
- 两个场景都断言：
  - `GetError(-1) = sslErrUnsupported`
  - `GetVerifyResultString` 包含 `unsupported`
  - `GetVerifyResultString` 不包含 `not implemented`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前文案含 `not implemented`）。

---

### Task 2 (P0): Minimal implementation for unsupported wording

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Write minimal implementation**
- 仅调整 `MarkUnsupported` 文案：
  - 从 `is not implemented in FreePascal backend yet`
  - 改为 `is unsupported by FreePascal backend`
- 不改错误码与调用路径。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Run focused regression batch

**Files:**
- Verify only (no planned code changes)

**Step 1: Run FreePascal server skeleton regression**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: PASS。

**Step 2: Run unit regression subset**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: PASS。

**Step 3: Update planning files**
- 更新：`task_plan.md`、`findings.md`、`progress.md`（记录 RED->GREEN 与验证输出）。

---

## Execution Notes
- 严格遵循：不写脚本、不改 CI/DI。
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令执行后都记录输出摘要。

## Suggested Immediate Start
- 立即执行 **Task 1**（红测），确认 `not implemented` 文案缺口仍可复现。
