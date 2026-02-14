# Phase4 Gate Summary Consistency Path Resolution Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `check_cross_platform_gate_summary_consistency_draft.sh` 在任意工作目录下都可正确处理 `--summary` 相对路径，避免误报“文件不存在”。

**Architecture:** 先加脚本合同测试复现“在 /tmp 调用脚本 + 仓库相对路径参数”失败，再最小修复路径归一化逻辑，最后跑回归和严格模式检查。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract

**Files:**
- Create: `tests/scripts/test_phase4_gate_summary_consistency_path.sh`

**Step 1: Write failing test**
- 场景 A：在仓库根运行，`--summary docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md` 应成功。
- 场景 B：在 `/tmp` 运行同一命令也应成功（当前会失败）。

**Step 2: Run RED**

Run: `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
Expected: FAIL（场景 B 失败）。

---

### Task 2: GREEN minimal fix

**Files:**
- Modify: `scripts/check_cross_platform_gate_summary_consistency_draft.sh`

**Step 1: Implement path normalization**
- 若 `--summary` 不是绝对路径，优先检查当前路径；若不存在，则回退检查 `$PROJECT_ROOT/$SUMMARY_FILE`。
- 命中后统一使用可访问路径继续执行。

**Step 2: Run GREEN**

Run: `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
Expected: PASS。

---

### Task 3: Regression

**Step 1:** strict mode should still fail on inconsistent summary
- Run: `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh --strict-check`
- Expected: PASS（测试脚本内部断言 strict 行为）。

**Step 2:** existing wave-b contracts remain green
- Run:
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
- Expected: PASS。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
