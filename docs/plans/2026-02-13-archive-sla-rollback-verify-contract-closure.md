# Archive SLA/Rollback Verify Contract Closure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `verify_archive_audit_sla_rollback_linkage_draft.sh` 建立首个脚本合同测试，并修复跨工作目录调用时相对路径解析不稳定问题。

**Architecture:** 先新增合同测试复现“在 `/tmp` 调用脚本 + 传仓库相对路径报告文件”失败，再最小修复输入/输出路径归一化逻辑，最后执行 strict 合同与既有脚本回归，确保无回退。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for path resolution

**Files:**
- Create: `tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
- Use fixtures: `docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md`
- Use fixtures: `docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md`
- Use fixtures: `docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md`

**Step 1: Write failing test**
- 场景 A：在仓库根运行，使用相对路径参数应成功。
- 场景 B：在 `/tmp` 运行同一命令（同样相对路径）也应成功；当前预期失败（RED）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
Expected: FAIL（/tmp 相对路径输入无法定位）。

---

### Task 2: GREEN minimal fix

**Files:**
- Modify: `scripts/verify_archive_audit_sla_rollback_linkage_draft.sh`

**Step 1: Minimal implementation**
- 增加 `SCRIPT_DIR/PROJECT_ROOT` 与路径归一化函数：
  - 输入报告参数：相对路径优先当前目录，找不到时回退 `$PROJECT_ROOT/<relative>`。
  - `--archive-root`：相对路径优先当前目录，目录不存在时回退 `$PROJECT_ROOT/<relative>`。
  - `--output`：相对路径统一写到 `$PROJECT_ROOT/<relative>`。
- 不改验真规则与 strict 语义。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh`
Expected: PASS。

---

### Task 3: Strict contract + focused regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_archive_audit_sla_rollback_verify_contract.sh --strict-check`
Expected: PASS（测试内部断言 strict 在失败检查时返回非 0）。

**Step 2: Existing contracts regression**
Run:
- `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
- `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
Expected: PASS。

**Step 3: Syntax validation**
Run: `bash -n scripts/verify_archive_audit_sla_rollback_linkage_draft.sh`
Expected: PASS（no output）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级决策、RED/GREEN/回归命令输出。

**Step 2:** 标记本轮完成，并生成下一优先任务。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `verify_archive_audit_sla_rollback_linkage_draft.sh` 首个合同测试 + 路径归一化。
2. **P1**: `generate_cross_platform_gate_summary_draft.sh` 合同测试补齐。
3. **P1**: `drill_archive_audit_sla_rollback_linkage_draft.sh` 合同测试补齐。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理。
