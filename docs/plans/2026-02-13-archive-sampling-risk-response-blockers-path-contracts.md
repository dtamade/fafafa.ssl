# Archive Sampling + Risk Response + Blockers Path Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为三条关键链路脚本补齐首个合同测试并修复跨目录调用不稳定问题：
- `generate_archive_audit_risk_response_draft.sh`（path+strict）
- `extract_pre_release_audit_blockers_draft.sh`（path+strict）
- `generate_archive_audit_sampling_record_draft.sh`（path，含 artifact-root 目录回退）

**Architecture:** 对每个脚本先新增合同测试复现“在 `/tmp` 调用脚本 + 传仓库相对路径参数”失败（RED），再最小修复路径归一化（相对路径回退 `$PROJECT_ROOT`，相对输出统一落在 `$PROJECT_ROOT`）（GREEN），最后跑 strict/语法检查与既有脚本合同回归。

**Tech Stack:** Bash, fixture-based shell contract tests.

---

### Task 1: RED contract for risk response path resolution

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_risk_response_path.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对输入+相对输出）应生成风险矩阵，且 `overall_risk=high`。
- 场景 B：`/tmp` 执行同命令（相对输入+相对输出）也应成功；当前预期失败（RED：相对输入无法定位或相对输出落到错误目录）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_risk_response_path.sh`
Expected: FAIL（/tmp 下相对输入无法定位或相对输出落到错误目录）。

---

### Task 2: GREEN minimal path normalization for risk response script

**Files:**
- Modify: `scripts/generate_archive_audit_risk_response_draft.sh`

**Step 1: Minimal implementation**
- 对 4 个输入文件参数做归一化：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`。
- 不修改风险计算逻辑与 strict 语义。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_risk_response_path.sh`
Expected: PASS。

---

### Task 3: Risk response strict contract + syntax check

**Step 1: Strict contract**
Run: `bash tests/scripts/test_generate_archive_audit_risk_response_path.sh --strict-check`
Expected: PASS（fixture overall_risk=high，strict 应返回非 0 且仍写出报告）。

**Step 2: Syntax check**
Run: `bash -n scripts/generate_archive_audit_risk_response_draft.sh`
Expected: PASS（no output）。

---

### Task 4: RED contract for blockers extraction path resolution

**Files:**
- Create: `tests/scripts/test_extract_pre_release_audit_blockers_path.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对输入+相对输出）应生成 blockers 报告，且 `blockers_status=fail`。
- 场景 B：`/tmp` 执行同命令（相对输入+相对输出）也应成功；当前预期失败（RED）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_extract_pre_release_audit_blockers_path.sh`
Expected: FAIL。

---

### Task 5: GREEN minimal path normalization for blockers script

**Files:**
- Modify: `scripts/extract_pre_release_audit_blockers_draft.sh`

**Step 1: Minimal implementation**
- 对 4 个输入文件参数做归一化：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`。
- 不修改 blockers 计算逻辑与 strict 语义。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_extract_pre_release_audit_blockers_path.sh`
Expected: PASS。

---

### Task 6: Blockers strict contract + syntax check

**Step 1: Strict contract**
Run: `bash tests/scripts/test_extract_pre_release_audit_blockers_path.sh --strict-check`
Expected: PASS（fixture blockers_status=fail，strict 应返回非 0 且仍写出报告）。

**Step 2: Syntax check**
Run: `bash -n scripts/extract_pre_release_audit_blockers_draft.sh`
Expected: PASS（no output）。

---

### Task 7: RED contract for sampling record path resolution

**Files:**
- Create: `tests/scripts/test_generate_archive_audit_sampling_record_path.sh`

**Step 1: Write failing test**
- 场景 A：仓库根执行（相对 `--artifact-root` + 相对输出）应生成抽样记录，并包含 `run_a`/`run_b` 且 profile=pr、manifest=yes。
- 场景 B：`/tmp` 执行同命令也应成功；当前预期失败（RED：相对输出落到 /tmp，且 artifact-root 未回退）。

**Step 2: Run RED**
Run: `bash tests/scripts/test_generate_archive_audit_sampling_record_path.sh`
Expected: FAIL。

---

### Task 8: GREEN minimal path normalization for sampling record script

**Files:**
- Modify: `scripts/generate_archive_audit_sampling_record_draft.sh`

**Step 1: Minimal implementation**
- 对 `--artifact-root` 做目录路径归一化：相对路径优先当前目录，否则回退 `$PROJECT_ROOT/<relative>`。
- 对 `--output` 相对路径归一化到 `$PROJECT_ROOT/<relative>`。
- 不改变抽样逻辑（manual/oldest/newest）。

**Step 2: Run GREEN**
Run: `bash tests/scripts/test_generate_archive_audit_sampling_record_path.sh`
Expected: PASS。

---

### Task 9: Sampling syntax + full regression

**Step 1: Syntax check**
Run: `bash -n scripts/generate_archive_audit_sampling_record_draft.sh`
Expected: PASS（no output）。

**Step 2: Existing contracts regression**
Run:
- `bash tests/scripts/test_generate_archive_audit_hold_linkage_path.sh`
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

### Task 10: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1:** 回写扫描证据、优先级、RED/GREEN/回归命令输出。
**Step 2:** 标记本轮完成并滚动下一优先项。

