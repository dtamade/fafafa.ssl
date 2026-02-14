# Cross-Platform Gate Summary Absolute-Input Contract Closure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `generate_cross_platform_gate_summary_draft.sh` 正确处理绝对输入文件路径，并在任意工作目录下稳定处理相对 `--output` 路径。

**Architecture:** 先新增脚本合同测试复现“绝对 `--input` 被错误拼接导致摘要字段变 unknown/missing”问题，再最小修复输入文件绝对路径分支与输出路径归一化，最后跑既有合同测试回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for absolute input path

**Files:**
- Create: `tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
- Use fixture: `docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md`

**Step 1: Write failing test**
- 场景 A：在仓库根目录运行，`--input` 传绝对路径，输出摘要应解析出 `platform=linux`、`profile=nightly`。
- 场景 B：在 `/tmp` 运行同命令且 `--output` 为相对路径，输出文件应落在仓库根下相对路径。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
Expected: FAIL（当前会出现 unknown/missing 或输出路径落错目录）。

---

### Task 2: GREEN minimal path handling fix

**Files:**
- Modify: `scripts/generate_cross_platform_gate_summary_draft.sh`

**Step 1: Minimal implementation**
- 在遍历 `REPORT_FILES` 时，区分 absolute/relative source：
  - absolute: 直接使用该路径读取
  - relative: 使用 `$PROJECT_ROOT/$file`
- 对 `--output`：若是相对路径，统一归一化到 `$PROJECT_ROOT/<relative>`。
- 不改现有汇总字段与 markdown 结构。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
Expected: PASS。

---

### Task 3: Focused regression

**Step 1:** syntax check
- Run: `bash -n scripts/generate_cross_platform_gate_summary_draft.sh`
- Expected: PASS。

**Step 2:** existing contracts stay green
- Run:
  - `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
  - `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
  - `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
- Expected: PASS。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。

**Step 2:** 标记完成并滚动下一优先任务。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `generate_cross_platform_gate_summary_draft.sh` 绝对输入路径合同缺口收口。
2. **P1**: `drill_archive_audit_sla_rollback_linkage_draft.sh` strict 合同测试补齐。
3. **P1**: `generate_archive_audit_status_dashboard_draft.sh` 合同测试补齐。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理。
