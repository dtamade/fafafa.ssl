# Wave B Linux Checklist Status Mapping Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 消除 `generate_wave_b_cross_platform_summary.sh` 对 Linux checklist 三项（compile/modules/examples）的硬编码 `PASS`，改为按 Linux summary 实际 step 状态回填。

**Architecture:** 先通过脚本合同测试制造 RED（Linux step 为 `SKIP/FAIL/PASS` 时，输出不能全是 `PASS`）；再最小扩展现有 step 解析逻辑到 Linux；最后跑双合同测试 + 关键回归。

**Tech Stack:** Bash, markdown table parsing, fixture-based script contract tests.

---

### Task 1: RED contract for Linux checklist mapping

**Files:**
- Create: `tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`

**Step 1: Write failing test**

```bash
# fixture linux summary with compile=SKIP, modules=FAIL, examples=PASS
# assert checklist row reflects SKIP/FAIL/PASS for linux column
```

**Step 2: Run test to verify RED**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: FAIL（当前脚本硬编码 Linux 为 PASS）。

---

### Task 2: GREEN minimal implementation

**Files:**
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`

**Step 1: Minimal code change**

```bash
# add linux_compile_check/linux_modules_check/linux_examples_check
# derive from read_platform_step_status(LINUX_SUMMARY, ...)
# update checklist table linux column from hardcoded PASS to derived values
```

**Step 2: Run test to verify GREEN**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: PASS。

---

### Task 3: Regression

**Files:**
- Verify only

**Step 1: Existing cross-platform TODO-closure contract**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
Expected: PASS。

**Step 2: Core regression**

Run: `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
Expected: PASS（0 fail）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record evidence timestamp**

Run: `date '+%Y-%m-%d %H:%M:%S %z'`
Expected: timestamp recorded.
