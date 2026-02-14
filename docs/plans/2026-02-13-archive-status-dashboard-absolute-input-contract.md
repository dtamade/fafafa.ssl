# Archive Status Dashboard Absolute-Input Contract Closure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `generate_archive_audit_status_dashboard_draft.sh` 正确处理 absolute 输入 glob/file，并在跨目录调用时稳定解析相对输出路径。

**Architecture:** 先新增合同测试复现“absolute 输入被错误拼接导致指标读零/unknown”与“/tmp 调用时相对输出落错目录”，再做最小路径修复（输入文件路径归一化 + relative output 归一化），最后执行既有合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for absolute input + cross-dir output

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_status_dashboard_abs_glob.sh`
- Use fixtures:
  - `docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md`
  - `docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md`
  - `docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md`
  - `docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md`

**Step 1: Write failing test**
- 场景 A（仓库根）：`--*-report-glob` 使用 absolute 路径，摘要必须正确解析样例指标（如 `hold_overdue_total=1`、`dashboard_status=fail`）。
- 场景 B（/tmp）：同一命令 + 相对 `--output`，输出文件应写入仓库根相对路径。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_status_dashboard_abs_glob.sh`
Expected: FAIL（当前 absolute 输入读取错误，且相对输出跨目录不稳定）。

---

### Task 2: GREEN minimal path handling fix

**Files:**
- Modify: `scripts/generate_archive_audit_status_dashboard_draft.sh`

**Step 1: Minimal implementation**
- 增加 `resolve_report_abs_path`：处理 `collect_files` 产出的 relative/absolute 条目。
- 在 hold/linkage/checklist/weekly 四个循环中统一用该函数得到可读路径。
- 对 `--output`：若为相对路径，统一归一化到 `$PROJECT_ROOT/<relative>`。
- 保持 dashboard 计算规则不变。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_status_dashboard_abs_glob.sh`
Expected: PASS。

---

### Task 3: Strict contract + focused regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_status_dashboard_abs_glob.sh --strict-check`
Expected: PASS（脚本在 non-pass dashboard 时 strict 返回非 0）。

**Step 2: Existing contracts regression**
Run:
- `bash tests/scripts/test_archive_audit_sla_rollback_drill_path.sh`
- `bash tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
- `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
- `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
- `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: PASS。

**Step 3: Syntax check**
Run: `bash -n scripts/generate_archive_audit_status_dashboard_draft.sh`
Expected: PASS（no output）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。

**Step 2:** 标记本轮完成并滚动下一个优先任务。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `generate_archive_audit_status_dashboard_draft.sh` absolute-input 合同缺口收口。
2. **P1**: `generate_archive_audit_execution_approval_chain_draft.sh` strict/path 合同测试补齐。
3. **P1**: `generate_archive_audit_full_chain_closure_report_draft.sh` 合同测试补齐。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理。
