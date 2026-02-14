# Repository Gap Iteration (Async Wait + Backend Skeleton) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 基于全仓扫描先关闭一个可复现的运行正确性缺口（Unix ASYNC wait 假阳性），并建立后续高优先级缺口的执行队列。

**Architecture:** 采用 `P0 -> P1 -> P2` 的迭代收敛。P0 先修复会导致“测试假通过/运行假成功”的路径（`WaitForAsyncJob` Unix 分支）；P1 再推进 FreePascal server skeleton 的 `placeholder_certverify` 缺口；P2 收敛剩余能力声明与错误语义一致性问题。每个任务按 TDD 小步执行：先写失败测试，再最小实现，再回归验证。

**Tech Stack:** FreePascal (ObjFPC), fpcunit, fafafa.ssl OpenSSL/FreePascal 后端。

---

## Scan Summary (2026-02-11)

### High-signal gaps
1. `src/fafafa.ssl.openssl.api.async.pas:331`
   - Unix `WaitForAsyncJob` 分支是 `Result := True` 占位实现，会导致等待逻辑假阳性。
2. `tests/test_freepascal_server_accept_skeleton.pas:117`
   - 测试仍允许 `placeholder_certverify`，说明 server CertificateVerify 签名路径仍有 skeleton 缺口。
3. `src/fafafa.ssl.freepascal.connection.pas:1345`
   - `MarkUnsupported` 统一返回“not implemented yet”，与协议不支持/状态不满足场景语义混杂。

### Priority
- **P0:** Async wait 假阳性（可快速修复 + 高风险）
- **P1:** FreePascal server CertificateVerify skeleton
- **P2:** FreePascal unsupported/not-implemented 错误语义分层

---

### Task 1 (P0): Add failing unit test for Unix async wait false-positive

**Files:**
- Create: `tests/unit/test_openssl_async_unit.pas`
- Modify: `tests/unit/run_unit_tests_simple.lpr`
- Target: `src/fafafa.ssl.openssl.api.async.pas`

**Step 1: Write failing test**
- 新增用例：`TestWaitForAsyncJob_UnixPendingFD_ShouldReturnFalse`
- 通过替换 `ASYNC_get_wait_ctx` 与 `ASYNC_WAIT_CTX_get_all_fds` 全局函数指针，构造“有 fd 待等待”的场景。

**Step 2: Run test to verify failure**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: FAIL（Unix 当前占位分支返回 `True`）。

**Step 3: Commit (only if task isolated and complete)**
- 本轮先不提交，和 Task 2 一并回归后再决定。

**Acceptance Criteria:**
- 新测试稳定暴露 Unix 占位逻辑导致的假阳性。

---

### Task 2 (P0): Implement minimal Unix-safe behavior for `WaitForAsyncJob`

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.async.pas`
- Test: `tests/unit/test_openssl_async_unit.pas`

**Step 1: Write minimal implementation**
- 将 Unix 分支从“无条件 `True`”改为保守返回 `False`（在未实现 poll/select 前避免假成功）。

**Step 2: Run targeted tests**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: PASS（Task 1 新增用例通过）。

**Step 3: Quick regression**
- Run: `fpc -Fu./src tests/openssl/test_openssl_basic_validation.pas -otmp/test_openssl_basic_validation && ./tmp/test_openssl_basic_validation`
- Expected: PASS。

**Acceptance Criteria:**
- Unix 分支不再产生 wait 假阳性。

---

### Task 3 (P0): Add no-FD contract regression for async wait

**Files:**
- Modify: `tests/unit/test_openssl_async_unit.pas`

**Step 1: Write test**
- 新增用例：`TestWaitForAsyncJob_NoFds_ShouldReturnTrue`。
- 覆盖现有约定：`NumFds = 0` 时可立即返回 `True`。

**Step 2: Run full unit runner**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: PASS。

**Step 3: Document results**
- 更新：`task_plan.md`、`findings.md`、`progress.md`。

**Acceptance Criteria:**
- async wait 在“有 fd”和“无 fd”两条分支都有防回归测试。

---

### Task 4 (P1): Replace `placeholder_certverify` with real server signer path

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `tests/test_freepascal_server_accept_skeleton.pas`

**Step 1: Write failing test**
- 将 `placeholder_certverify` 从允许项移除，要求错误信息体现真实 signer/finished 路径。

**Step 2: Run and confirm failure**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`

**Step 3: Implement minimal signer path**
- 接入已存在的 signer 能力，替换 placeholder 路径。

**Step 4: Re-run tests**
- Expected: PASS。

---

### Task 5 (P2): Split unsupported vs not-implemented error semantics in FreePascal backend

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Add/Modify tests around error codes/messages

**Step 1: Add failing tests for error classification**
- 不支持算法 -> `sslErrUnsupported`
- 功能未实现 -> 明确 `not implemented` 文案
- 状态不满足（如握手未完成）-> 非 `not implemented` 文案

**Step 2: Minimal implementation and verification**
- 仅调整分类与文案，不引入额外行为变更。

---

## Execution Notes
- 严格执行：不写脚本、不改 CI/DI。
- 每个任务遵循 TDD：先失败、后实现、再回归。
- 每步命令必须回报实际输出摘要。
