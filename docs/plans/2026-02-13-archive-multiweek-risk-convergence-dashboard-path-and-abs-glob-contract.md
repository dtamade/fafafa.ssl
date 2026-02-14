# Archive Multi-Week Risk Convergence Dashboard Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh` 增加首个 `path+abs-glob+strict` 合同测试，并修复跨目录调用时相对输出路径与 absolute glob 输入不稳定问题。

**Architecture:** 先用合同测试复现两个缺口：1) `/tmp` 下相对 `--output` 落盘到错误目录；2) absolute glob 输入导致读取路径二次拼接、状态误判。按严格 TDD（RED→GREEN）逐个闭环，然后跑 strict 合同与既有脚本合同回归，最后回写 planning 文件证据。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for multiweek convergence dashboard (path + abs-glob)

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对 glob + 相对输出）应生成看板，且包含 `risk_convergence_status=fail`（fixture 设置 latest backtest_status=fail）。
- 场景 B：`/tmp` 执行同命令（相对 glob + 相对输出）也应成功；当前预期失败（RED：相对输出落到 /tmp）。
- 场景 C：absolute glob 输入（`/abs/path/*.md`）也应保持 `risk_convergence_status=fail`；当前预期失败（RED：absolute path 被二次拼接，状态变为 unknown/warn）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh`
Expected: FAIL。

---

### Task 2: GREEN-1 minimal output path normalization

**Files:**
- Modify: `scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh`

**Step 1: Minimal implementation**
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`（与其他 draft 脚本一致）。

**Step 2: Run GREEN-1**
Run: `bash tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh`
Expected: PASS（path contract）。

---

### Task 3: GREEN-2 absolute glob support

**Files:**
- Modify: `scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh`

**Step 1: Minimal implementation**
- 处理 collect_files 输出的 absolute path 条目：读取时不得再拼接 `$PROJECT_ROOT/$file`。
- 建议：引入 `resolve_report_abs_path`（若 path 以 `/` 开头则原样返回，否则拼接 `$PROJECT_ROOT/$path`），用于三类 stream 的读取路径。

**Step 2: Run GREEN-2**
Run: `bash tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh --abs-check`
Expected: PASS（absolute glob contract）。

---

### Task 4: Strict contract + regression

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_multiweek_risk_convergence_dashboard_path_and_abs_glob.sh --strict-check`
Expected: PASS（fixture 为 fail，strict 应返回非 0 且仍写出报告）。

**Step 2: Syntax check**
Run: `bash -n scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh`
Expected: PASS（no output）。

**Step 3: Existing contracts regression**
Run:
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

### Task 5: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。
**Step 2:** 标记本轮完成并滚动下一优先项。

