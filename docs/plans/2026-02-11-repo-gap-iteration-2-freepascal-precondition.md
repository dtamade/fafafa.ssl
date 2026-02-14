# FreePascal Precondition Error Semantics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 FreePascal 连接在“握手前读写”场景下错误语义不准确的问题，并收紧相关测试契约。

**Architecture:** 通过 TDD 先在现有 FreePascal backend 测试里暴露错误码语义缺口（当前返回 `sslErrUnsupported` + `not implemented`），再以最小改动引入 precondition 错误标记路径，最后回归关键 TLS13/FreePascal 测试。

**Tech Stack:** FreePascal (ObjFPC), fafafa.ssl.freepascal.connection, 程序级测试。

---

### Task 1 (P0): Add failing test for read/write-before-handshake error code

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Write failing test**
- 在 `test_freepascal_backend_basic` 增加：
  - 创建 `CreateConnection(TMemoryStream)` 后不执行握手，直接 `Read/Write`。
  - 断言错误码为 `sslErrProtocol`（预条件未满足），不是 `sslErrUnsupported`。

**Step 2: Run test to verify failure**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前实现返回 unsupported）。

---

### Task 2 (P0): Implement precondition error marker in FreePascal connection

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Minimal implementation**
- 新增 precondition 错误标记方法（`sslErrProtocol` + 明确 handshake 前置条件文案）。
- `DoRead` / `DoWrite` 在握手未完成时使用该路径。

**Step 2: Re-run failing test**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Tighten skeleton accept test contract (remove placeholder allowance)

**Files:**
- Modify: `tests/test_freepascal_server_accept_skeleton.pas`

**Step 1: Tighten assertion**
- 从失败原因允许项中去掉 `placeholder_certverify`。

**Step 2: Run targeted test**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: PASS（若 FAIL，再补最小修复）。

---

## Execution Notes
- 严格 TDD：先红后绿。
- 每步命令输出必须回报。
