# Archive SLA-Rollback Drill Path Resolution Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `drill_archive_audit_sla_rollback_linkage_draft.sh` 在任意工作目录下可稳定解析相对输入/输出路径，并补齐首个脚本合同测试。

**Architecture:** 先新增 path+strict 合同测试复现“/tmp 调用 + 相对路径参数失败”，再对脚本做最小路径归一化修复（输入报告与输出文件），最后执行既有脚本合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for path resolution

**Files:**
- Create: `tests/scripts/test_archive_audit_sla_rollback_drill_path.sh`
- Use fixtures:
  - `docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md`
  - `docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md`
  - `docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md`

**Step 1: Write failing test**
- 在仓库根执行：相对输入 + 相对输出，预期成功。
- 在 `/tmp` 执行同命令：同样应成功；当前预期失败（RED）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_drill_path.sh`
Expected: FAIL（/tmp 下输入相对路径无法定位）。

---

### Task 2: GREEN minimal fix

**Files:**
- Modify: `scripts/drill_archive_audit_sla_rollback_linkage_draft.sh`

**Step 1: Minimal implementation**
- 增加路径归一化：
  - 输入报告参数（3 个）：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
  - `--output`：相对路径统一归一到 `$PROJECT_ROOT/<relative>`。
- 保持联动计算与 strict 规则不变。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_drill_path.sh`
Expected: PASS。

---

### Task 3: Strict contract + focused regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_drill_path.sh --strict-check`
Expected: PASS（测试内部断言 strict 对非 pass 状态返回非 0）。

**Step 2: Existing script contracts regression**
Run:
- `bash tests/scripts/test_generate_cross_platform_gate_summary_abs_input.sh`
- `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
- `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
- `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: PASS。

**Step 3: Syntax check**
Run: `bash -n scripts/drill_archive_audit_sla_rollback_linkage_draft.sh`
Expected: PASS（无输出）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令与结果。

**Step 2:** 标记本轮完成，滚动下一优先项。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `drill_archive_audit_sla_rollback_linkage_draft.sh` 路径归一化 + 首个合同测试。
2. **P1**: `generate_archive_audit_status_dashboard_draft.sh` 合同测试补齐。
3. **P1**: `generate_archive_audit_execution_approval_chain_draft.sh` 合同测试补齐。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理。
