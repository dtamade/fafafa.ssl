# Archive Full Chain Closure Path Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `generate_archive_audit_full_chain_closure_report_draft.sh` 增加首个 path+strict 合同测试，并修复跨目录调用时相对输入/输出路径不稳定。

**Architecture:** 先新增合同测试复现“在 `/tmp` 调用脚本 + 相对输入路径失败”，再最小修复路径归一化与输出目录创建，最后跑 strict 合同和既有脚本合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for path resolution

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_full_chain_closure_report_path.sh`
- Use fixtures:
  - `docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md`
  - `docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md`
  - `docs/test_reports/ARCHIVE_AUDIT_CLOSURE_REVALIDATE_SAMPLE_B58.md`
  - `docs/test_reports/ARCHIVE_AUDIT_CLOSURE_TREND_SAMPLE_B57.md`
  - `docs/test_reports/ARCHIVE_AUDIT_CLOSURE_RETRY_SAMPLE_B56.md`
  - `docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_VERIFY_SAMPLE_B55.md`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对输入+相对输出）应生成报告，且包含 `overall_status=fail`。
- 场景 B：`/tmp` 执行同命令（相对输入+相对输出）也应成功；当前预期失败（RED）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_full_chain_closure_report_path.sh`
Expected: FAIL（/tmp 下相对输入无法定位）。

---

### Task 2: GREEN minimal path normalization

**Files:**
- Modify: `scripts/generate_archive_audit_full_chain_closure_report_draft.sh`

**Step 1: Minimal implementation**
- 对 7 个输入文件参数做归一化：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`。
- 写文件前补 `mkdir -p "$(dirname "$OUTPUT")"`。
- 不改链路状态计算规则。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_full_chain_closure_report_path.sh`
Expected: PASS。

---

### Task 3: Strict contract + focused regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_full_chain_closure_report_path.sh --strict-check`
Expected: PASS（样例 overall_status=fail，strict 应返回非 0）。

**Step 2: Existing contracts regression**
Run:
- `bash tests/scripts/test_generate_archive_audit_execution_approval_chain_path.sh`
- `bash tests/scripts/test_generate_archive_audit_status_dashboard_abs_glob.sh`
- `bash tests/scripts/test_archive_audit_sla_rollback_drill_path.sh`
- `bash tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
- `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
- `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
- `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: PASS。

**Step 3: Syntax check**
Run: `bash -n scripts/generate_archive_audit_full_chain_closure_report_draft.sh`
Expected: PASS（no output）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。

**Step 2:** 标记本轮完成并滚动下一优先项。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `generate_archive_audit_full_chain_closure_report_draft.sh` path+strict 合同收口。
2. **P1**: `generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh` strict/path 合同测试补齐。
3. **P1**: `generate_archive_audit_consistency_remediation_draft.sh` 合同测试补齐。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理。
