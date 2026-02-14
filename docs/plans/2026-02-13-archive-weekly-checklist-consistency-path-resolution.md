# Archive Weekly Checklist Consistency Path Resolution Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `check_archive_audit_weekly_checklist_consistency_draft.sh` 在任意工作目录下都能正确解析 `--weekly-report/--checklist-report/--output` 的仓库相对路径，并保持 strict 行为稳定。

**Architecture:** 先用脚本合同测试复现“在 `/tmp` 调用脚本 + 传仓库相对路径参数”失败，再最小化修复路径归一化逻辑（仅补齐入参路径解析，不改业务规则），最后跑既有回归脚本确保此前修复不回退。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for relative path execution

**Files:**
- Create: `tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
- Use fixtures: `docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md`
- Use fixtures: `docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md`

**Step 1: Write the failing test**
- 在仓库根执行：
  - `bash scripts/check_archive_audit_weekly_checklist_consistency_draft.sh --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md --checklist-report docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md --output tmp/test_archive_weekly_checklist/root_ok.md`
  - 预期成功。
- 在 `/tmp` 执行同一脚本 + 同样的仓库相对路径参数：
  - 预期也应成功（当前预期失败，作为 RED）。

**Step 2: Run test to verify it fails**
Run: `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
Expected: FAIL（/tmp 相对路径场景报 input file not found）。

---

### Task 2: GREEN minimal path normalization

**Files:**
- Modify: `scripts/check_archive_audit_weekly_checklist_consistency_draft.sh`

**Step 1: Write minimal implementation**
- 增加路径归一化函数：
  - 对 `--weekly-report` 与 `--checklist-report`：若为相对路径且当前目录不存在，则回退到 `$PROJECT_ROOT/<relative>`。
  - 对 `--output`：若为相对路径，统一写到 `$PROJECT_ROOT/<relative>`，避免从 `/tmp` 执行时写到错误目录。
- 保持现有一致性计算与 strict 规则不变。

**Step 2: Run test to verify it passes**
Run: `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`
Expected: PASS。

---

### Task 3: Strict-mode contract and focused regression

**Files:**
- Reuse: `tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh`

**Step 1: Verify strict contract**
Run: `bash tests/scripts/test_archive_audit_weekly_checklist_consistency_path.sh --strict-check`
Expected: PASS（测试脚本内部断言 strict 对不一致输入返回非 0）。

**Step 2: Verify existing script contracts remain green**
Run:
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
- `bash tests/scripts/test_phase4_gate_summary_consistency_path.sh`
Expected: PASS。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record execution evidence**
- 回写全仓扫描结果、优先级决策、RED/GREEN/回归命令与关键输出。

**Step 2: Mark completion state**
- 新增本轮条目并标记完成，明确下一优先项（B55/B56 之一）作为后续入口。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `check_archive_audit_weekly_checklist_consistency_draft.sh` 相对路径执行缺口 + 首个合同测试。
2. **P1**: `verify_archive_audit_sla_rollback_linkage_draft.sh` 增加 contracts（参数必填、strict 非 pass 退出码、路径归一化）。
3. **P1**: `generate_cross_platform_gate_summary_draft.sh` 增加脚本合同测试并与 `generate_wave_b_cross_platform_summary.sh` 对齐。
4. **P2**: 清理 `docs/archive/**` 的历史 TODO 噪声，避免误导当前 backlog 统计。
