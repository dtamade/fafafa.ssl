# Archive Weekly Report Path + Absolute Glob Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `generate_archive_audit_weekly_report_draft.sh` 增加首个合同测试，覆盖跨目录相对 `--output` 与 absolute glob 输入的稳定性，并修复相应路径归一化缺口。

**Architecture:** 先用 path 合同测试复现“在 `/tmp` 执行脚本 + 相对输出路径写到错误目录”，再用 absolute-glob 合同测试复现“absolute 输入被二次拼接导致指标误读”，然后最小修复输出路径与输入文件路径解析，最后跑 strict 合同与既有脚本合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for relative output path resolution

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对 glob + 相对输出）应生成报告，且包含 `weekly_status=fail`（由自建夹具保证）。  
- 场景 B：`/tmp` 执行同命令（相对 glob + 相对输出）也应将输出落在项目根目录下；当前预期失败（RED）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh`
Expected: FAIL（/tmp 下相对输出未归一到 `$PROJECT_ROOT`）。

---

### Task 2: GREEN minimal output path normalization

**Files:**
- Modify: `scripts/generate_archive_audit_weekly_report_draft.sh`

**Step 1: Minimal implementation**
- 若 `--output` 为相对路径，则统一归一到 `$PROJECT_ROOT/<relative>`（与其他 archive audit 脚本契约一致）。  
- 不修改业务统计逻辑。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh`
Expected: PASS（path contract）。

---

### Task 3: RED contract for absolute glob inputs

**Step 1: Run absolute-glob contract**
Run: `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh --abs-check`
Expected: FAIL（absolute 输入被 `$PROJECT_ROOT/$file` 二次拼接，导致 `weekly_status` 被误读为 pass）。

---

### Task 4: GREEN minimal absolute-input normalization

**Files:**
- Modify: `scripts/generate_archive_audit_weekly_report_draft.sh`

**Step 1: Minimal implementation**
- 增加 `resolve_report_abs_path()`：若条目为 absolute path 则直接使用，否则拼接 `$PROJECT_ROOT/$file`。  
- hold/linkage/checklist 三条读取链统一使用该函数读取 metrics。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh --abs-check`
Expected: PASS。

---

### Task 5: Strict contract + focused regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_weekly_report_path_and_abs_glob.sh --strict-check`
Expected: PASS（样例 `weekly_status=fail`，strict 应返回非 0）。

**Step 2: Existing contracts regression**
Run:
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

**Step 3: Syntax check**
Run: `bash -n scripts/generate_archive_audit_weekly_report_draft.sh`
Expected: PASS（no output）。

---

### Task 6: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。

**Step 2:** 标记本轮完成并滚动下一优先项。

---

## Priority Queue From This Scan

1. **P0（本轮执行）**: `generate_archive_audit_weekly_report_draft.sh` path+absolute-glob+strict 合同收口。
2. **P1**: `check_archive_audit_approval_evidence_consistency_draft.sh` 增加 strict/path 合同测试。
3. **P1**: `monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh` 增加 strict/path 合同测试。
4. **P2**: `docs/archive/**` 历史 TODO 噪声治理并与当前 backlog 分层。
