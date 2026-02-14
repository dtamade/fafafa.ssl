# FreePascal Renegotiate Precondition Error Classification Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 连接在握手前调用 `Renegotiate` 的错误分类，使其与 `Read/Write` 前置条件语义一致（`sslErrProtocol`）。

**Architecture:** 在 `test_freepascal_backend_basic` 增加红测断言；最小修改 `DoRenegotiate` 在握手未完成时走 `MarkPrecondition`；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.connection`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.connection.pas:2187`
   - `DoRenegotiate` 在未握手时使用 `sslErrHandshake`。
2. 同文件 `DoRead/DoWrite` 在未握手时已使用 `MarkPrecondition`（`sslErrProtocol`）。
3. 前置条件错误分类不一致，影响调用侧统一处理。

### Priority
- **P0:** 统一握手前置条件错误分类（test-first）。

---

### Task 1 (P0): Add failing test for renegotiate precondition classification

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Write failing assertion**
- 在握手前连接断言中新增：
  - `not LConn.Renegotiate`
  - `LConn.GetError(-1) = sslErrProtocol`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前为 `sslErrHandshake`）。

---

### Task 2 (P0): Implement protocol precondition classification for renegotiate

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Minimal implementation**
- `DoRenegotiate` 在 `not FHandshakeComplete` 分支调用 `MarkPrecondition('TLS renegotiate/key update')` 并返回 `False`。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Step 1:**
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
- `./tmp/test_fp_accept`

**Step 2:**
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
- `./tmp/run_unit_tests_simple --format=plain --all`

---

## Execution Notes
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令输出必须回报。

---

## Execution Record (2026-02-11 10:15 +0800)

### Task 1 (P0): Add failing test for renegotiate precondition classification
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - `not LConn.Renegotiate`
  - `LConn.GetError(-1) = sslErrProtocol`

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ Renegotiate before handshake should report protocol precondition error`

### Task 2 (P0): Implement protocol precondition classification for renegotiate
- Modified: `src/fafafa.ssl.freepascal.connection.pas`
- Implementation:
  - `DoRenegotiate` handshake-precondition branch now calls
    `MarkPrecondition('TLS renegotiate/key update')` and returns `False`.

**GREEN command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `✅ FreePascal backend basic checks passed`

### Task 3 (P1): Focused regression

**Regression command 1**
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
```
**Output (key):**
- `✅ FreePascal server accept skeleton checks passed`

**Regression command 2**
```bash
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
**Output (key):**
- `Number of run tests: 10`
- `Number of failures: 0`
- `Number of errors: 0`
- `Number of ignored tests: 2`

### Iteration Status
- Iteration 16 P0 task: **complete**
- Contract gap (`Renegotiate` precondition error classification drift) closed.
