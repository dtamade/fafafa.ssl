# Archive Hold Linkage Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `generate_archive_audit_hold_linkage_draft.sh` 增加首个 `path+strict` 合同测试，并修复跨目录调用时相对输入/输出路径不稳定问题。

**Architecture:** 先用合同测试复现“在 `/tmp` 调用脚本 + 传仓库相对路径参数”失败（RED），再最小修复输入/输出路径归一化（相对路径回退 `$PROJECT_ROOT`）与输出目录创建（GREEN），最后执行 strict 合同与既有脚本合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for hold linkage path resolution

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_hold_linkage_path.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对输入+相对输出）应生成联动报告，并包含 `status=warn`（fixture 设置 overdue 风险）。
- 场景 B：`/tmp` 执行同命令（相对输入+相对输出）也应成功；当前预期失败（RED：相对输入无法定位或相对输出落到错误目录）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_hold_linkage_path.sh`
Expected: FAIL。

---

### Task 2: GREEN minimal path normalization for hold linkage script

**Files:**
- Modify: `scripts/generate_archive_audit_hold_linkage_draft.sh`

**Step 1: Minimal implementation**
- 对 2 个输入文件参数做归一化：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`。
- 不修改 risk 计算逻辑与 strict 语义。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_hold_linkage_path.sh`
Expected: PASS。

---

### Task 3: Strict contract + regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_hold_linkage_path.sh --strict-check`
Expected: PASS（fixture status=warn，strict 应返回非 0 且仍写出报告）。

**Step 2: Syntax check**
Run: `bash -n scripts/generate_archive_audit_hold_linkage_draft.sh`
Expected: PASS（no output）。

**Step 3: Existing contracts regression**
Run:
- `bash tests/scripts/test_backtest_archive_audit_threshold_policy_path_and_abs_glob.sh`
- `bash tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh`
- `bash tests/scripts/test_generate_pre_release_archive_audit_checklist_path.sh`
- `bash tests/scripts/test_generate_archive_audit_risk_execution_receipt_path.sh`
- `bash tests/scripts/test_archive_audit_sla_breach_alert_path.sh`
- `bash tests/scripts/test_archive_audit_approval_evidence_consistency_path.sh`
- `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh`
- `bash tests/scripts/test_generate_archive_audit_consistency_remediation_path.sh`
- `bash tests/scripts/test_generate_archive_audit_full_chain_closure_report_path.sh`
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

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。
**Step 2:** 标记本轮完成并滚动下一优先项。

