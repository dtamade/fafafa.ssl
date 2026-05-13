# Task Plan - Wave B/B2 Consistency Cross Summary Platform Evidence Metadata Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `cross_summary` 平台 evidence 行的盲信，避免 active macOS probe / Windows summary metadata 丢失时，strict consistency 仍错误显示为 `CONSISTENT`。

## Current Batch
1. 写 focused contract，证明 active macOS probe / Windows summary metadata 丢失时，consistency 仍会静默吞掉。
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`，把 `cross_summary` 平台 evidence metadata 校验接入 parse-issue 语义。
3. 复跑 consistency / prepare / handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified false-green trust of malformed active platform evidence metadata in cross summary
- [completed] wrote focused contract for cross summary platform evidence metadata truth
- [completed] minimal active platform evidence metadata validation in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- 当前 consistency checker 在修复前还有一类更细粒度的假绿灯：
  - `cross_summary` 会把 macOS probe / Windows summary 标成 active
  - 但 evidence path metadata 可以已经丢失
  - strict consistency 仍然会继续返回 `CONSISTENT`
- 这会把“坏掉的 active platform truth”伪装成一致性绿灯。
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`: FAIL before fix
  - failure shape:
    - `consistency should fail strict mode when cross summary marks macOS probe evidence active but loses the probe path metadata`
    - `consistency should fail strict mode when cross summary marks Windows evidence active but loses the summary path metadata`
- minimal implementation:
  - 更新 `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added cross-summary platform state/evidence parsers
    - active macOS probe / macOS summary / Windows summary metadata now get explicit validation
    - malformed active metadata now increments `runid_mismatch_or_parse_issue`
    - explicit `probe: <path> (missing file)` now also gets inherited and required
  - 新增 `tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh` 锁住显式缺失 probe path 继承
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Metadata Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `cross summary` 元数据的盲信，避免 `cross_summary` 缺失关键字段时，只要真实 evidence 还在，consistency 仍错误显示为 `CONSISTENT`。

## Current Batch
1. 写 focused contract，证明缺失 `linux_examples_json` 的 `cross_summary` 仍会被 consistency 静默吞掉。
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`，把 `cross summary` 关键 metadata 校验接入 parse-issue 语义。
3. 复跑 consistency / prepare / handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified false-green trust of malformed cross summary metadata in consistency checker
- [completed] wrote focused contract for cross summary metadata truth
- [completed] minimal cross-summary metadata validation in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- 当前 consistency checker 在修复前还有一类更底层的假绿灯：
  - `cross_summary` 文件存在且 `run_id` 正常
  - 真实 `linux_examples_json` 也仍然存在
  - 但 `cross_summary` 自己已经缺失 `linux_examples_json` metadata
  - strict consistency 仍然会返回 `CONSISTENT`
- 这会把“坏掉的 cross summary”伪装成“完整的一致性绿灯”。
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`: FAIL before fix
  - failure shape:
    - `consistency should fail strict mode when cross summary is missing required linux_examples_json metadata even if the actual linux examples artifact still exists`
- minimal implementation:
  - 更新 `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added dedicated `check_cross_summary_artifact(...)`
    - cross summary now validates required `linux_summary` / `linux_examples_json` metadata
    - missing metadata now increments `runid_mismatch_or_parse_issue`
    - cross summary row now exposes the parse issue instead of staying `ok`
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Closure Platform Matrix Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 对 `closure_report` 平台状态矩阵的盲信，避免 `linux/macos/windows` 任一平台状态行缺失或非法时，顶层 handoff 仍继续给出正常状态。

## Current Batch
1. 写 focused contract，证明 `closure_status=CLOSED` 但平台状态表不完整时，顶层仍会被误导。
2. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`，把 closure 平台状态矩阵完整性接入 report-chain 校验。
3. 复跑 handoff / consistency 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified blind trust of closure platform matrix completeness in handoff bundle
- [completed] wrote focused contract for closure platform matrix truth
- [completed] minimal platform-state matrix validation in handoff bundle
- [completed] focused verification and review closeout

## Current Evidence
- 当前顶层 handoff bundle 在修复前还有一类更隐蔽的坏链路：
  - `closure_status` 可以是 `CLOSED`
  - `consistency_status` 可以是 `CONSISTENT`
  - 但 `closure_report` 的平台状态表仍可能缺失 `linux/macos/windows` 某一行
- 这会让顶层继续输出 `CLOSED` 或普通 next actions，却看不到 closure matrix 已经不可信。
- focused RED:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`: FAIL before fix
  - failure shape:
    - `handoff bundle should reject a closure report whose platform status table is incomplete`
- minimal implementation:
  - 更新 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added closure platform-state allow-list validation
    - linux/macos/windows row missing or invalid now becomes `NEEDS_REPORT_REPAIR`
    - report-chain guidance now explicitly covers closure platform table completeness
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Report Chain Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 对下游报告元数据的盲信，避免 `closure_report` / `consistency_report` 缺失关键状态字段时，顶层 handoff bundle 仍产出正常状态，制造“坏报告链仍可继续”的假象。

## Current Batch
1. 写 focused contract，证明缺失 `consistency_status` 的 downstream report 会被顶层静默吞掉。
2. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`，新增 report-chain metadata 校验与 `NEEDS_REPORT_REPAIR`。
3. 复跑 handoff / consistency 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified silent trust of malformed downstream report metadata in handoff bundle
- [completed] wrote focused contract for handoff report-chain truth
- [completed] minimal NEEDS_REPORT_REPAIR state and report_chain_note surfacing in handoff bundle
- [completed] focused verification and review closeout

## Current Evidence
- 当前顶层 handoff bundle 在修复前还有一层更深的假状态：
  - `closure_report` 可以是 `CLOSED`
  - `consistency_report` 可以缺失 `consistency_status`
  - 顶层仍继续按普通链路消费，并落到正常 handoff state
- 这会把“下游报告已坏”伪装成“handoff 还能继续推进”。
- focused RED:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: FAIL before fix
  - failure shape:
    - `handoff bundle should surface malformed downstream report metadata as NEEDS_REPORT_REPAIR instead of pretending the handoff can continue`
- minimal implementation:
  - 更新 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added metadata parsers for `closure_status` / `consistency_status`
    - added status allow-list validation
    - malformed downstream report-chain metadata now falls to `NEEDS_REPORT_REPAIR`
    - top-level report now emits `report_chain_note`
    - normal `READY_FOR_RUNNER` / `NEEDS_EVIDENCE_SYNC` / `CLOSED` branches stay intact for valid chains
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Gate Repair State Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 `handoff_state` 语义漂移，避免已有平台 gate 已经失败时，顶层状态仍错误显示为 `READY_FOR_RUNNER`。

## Current Batch
1. 写 focused contract，证明 Linux `FAIL` 场景下 handoff_state 仍错误落在 `READY_FOR_RUNNER`。
2. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`，新增 gate-repair 状态分支。
3. 复跑 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified misleading READY_FOR_RUNNER semantics when an existing platform gate already failed
- [completed] wrote focused contract for handoff gate-repair state truth
- [completed] minimal NEEDS_GATE_REPAIR state split in handoff bundle
- [completed] focused verification and review closeout

## Current Evidence
- 当前顶层 handoff bundle 在修复前存在更深一层状态误导：
  - Linux summary 已存在且 `Overall Status: FAIL`
  - consistency 仍是 `CONSISTENT`
  - 结果 `handoff_state` 仍落到 `READY_FOR_RUNNER`
- 这会把“需要修 gate”伪装成“只差 runner 证据”。
- focused RED:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: FAIL before fix
  - failure shape:
    - handoff bundle should not stay READY_FOR_RUNNER when an existing platform summary already reports FAIL
- minimal implementation:
  - 更新 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `is_gate_repair_state(...)`
    - any `FAIL/READY/DRY_RUN` platform state now falls to `NEEDS_GATE_REPAIR`
    - `NEEDS_EVIDENCE_SYNC` still wins when consistency is inconsistent
    - `CLOSED` still wins when closure+consistency both green
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Linux Next Actions Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 顶层 `Next Actions` 的 Linux 缺口，避免 handoff bundle 在 Linux 成为真实阻塞项时仍不给出具体 Linux 修复动作。

## Current Batch
1. 写 focused contract，证明 Linux `FAIL`、macOS/Windows `PASS` 时 handoff bundle 不提 Linux。
2. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`，把 Linux platform state 也接入顶层 `Next Actions`。
3. 复跑 handoff 与 cross-summary 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified missing Linux next-action guidance on the top-level handoff bundle
- [completed] wrote focused contract for handoff Linux next-action truth
- [completed] minimal Linux-aware handoff next-actions guidance
- [completed] focused verification and review closeout

## Current Evidence
- 当前顶层 handoff bundle 在修复前会出现一类真实半截指导：
  - `closure_status=IN_PROGRESS`
  - `consistency_status=CONSISTENT`
  - Linux 是唯一阻塞平台
  - 但 `Next Actions` 既不提 Linux，也不提具体修复动作，只剩 replay command
- focused RED:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - handoff bundle next actions should explicitly mention Linux FAIL when Linux baseline is the blocking platform
- minimal implementation:
  - 更新 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - now reads `linux` platform state from `closure_report`
    - Linux 非 `PASS` 时显式提示修复或重跑 Linux baseline
    - kept `handoff_state` calculation and replay command unchanged
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B Cross Summary Next Actions Truth

## Goal
收口 `generate_wave_b_cross_platform_summary.sh` 的 `Next Actions` 漂移，避免它继续忽略 Linux mandatory baseline，并把操作者引向只刷新局部摘要的旧入口。

## Current Batch
1. 写 focused contract，锁住 Linux `FAIL` 与三平台 `PASS` 两种 next-action 真相。
2. 最小修改 `generate_wave_b_cross_platform_summary.sh`，把 `Next Actions` 收口到状态驱动 + prepare 单一入口。
3. 复跑 cross-summary 与 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified stale static cross-summary next-actions after handoff truth consolidation
- [completed] wrote focused contract for cross-summary next-action truth
- [completed] minimal state-driven cross-summary next-actions guidance
- [completed] focused verification and review closeout

## Current Evidence
- 当前 cross summary 在修复前仍保留固定模板：
  - 只提醒 macOS / Windows
  - 不提醒 Linux baseline
  - 仍提示“重新运行本脚本，形成最终三平台对齐摘要”
- 这与当前 repo truth 已经冲突：
  - Linux baseline 是 handoff 必需前提
  - 完整刷新入口已经是 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 三平台全 `PASS` 时也不该继续提示重跑平台 lane
- focused RED:
  - 新增 `tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary next actions should explicitly mention Linux FAIL when Linux baseline is the blocking platform
- minimal implementation:
  - 更新 `scripts/generate_wave_b_cross_platform_summary.sh`
    - added state-driven `NEXT_ACTIONS`
    - Linux 非 `PASS` 时显式提示修复或重跑 Linux baseline
    - macOS / Windows 非 `PASS` 时分别给出对应 runner 修复动作
    - 三平台全 `PASS` 时改为 aligned/optional refresh 提示
    - final guidance now points to `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Closure Linux Next Actions Truth

## Goal
收口 `check_wave_b_b2_closure_readiness.sh` 的 `Next Actions` 漂移，避免 Linux baseline 已成为必需前提后，closure 报告仍只提醒 macOS/Windows，而不提示 Linux 非 `PASS` 的修复动作。

## Current Batch
1. 写 focused contract，证明 Linux `READY` 场景下 closure 报告缺少 Linux next-action guidance。
2. 最小修改 `check_wave_b_b2_closure_readiness.sh`，把 `Next Actions` 收口到按平台状态生成。
3. 复跑 closure 与 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified stale static closure next-actions after Linux baseline became mandatory
- [completed] wrote focused contract for Linux closure next-action truth
- [completed] minimal state-driven closure next-actions guidance
- [completed] focused verification and review closeout

## Current Evidence
- 当前 closure 报告已经能把 Linux 无法解析 `Overall Status` 的 summary 识别成：
  - `| linux | READY | summary exists but overall unknown | ... |`
- 但修复前 `## Next Actions` 仍然只写：
  - macOS runner
  - Windows runner
  - prepare 入口
- 这会把当前最关键的 Linux baseline 修复动作直接藏掉。
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - closure next actions should explicitly mention Linux READY/FAIL states after Linux baseline became mandatory
- minimal implementation:
  - 更新 `scripts/check_wave_b_b2_closure_readiness.sh`
    - added state-driven `NEXT_ACTIONS`
    - Linux/macOS/Windows 非 `PASS` 时分别给出对应修复动作
    - `CLOSED` 时改为闭环完成/可选复核提示
    - final rerun guidance continues to point to `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Next Actions Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 报告面的操作指引漂移，避免它在 `closure_status_note=IN_PROGRESS` 时只显示 `CONSISTENT`，却不说明 handoff 尚未闭环，也不把操作者指回当前真实的 `prepare_wave_b_b2_handoff_bundle.sh` 刷新入口。

## Current Batch
1. 写 focused contract，证明 consistency 报告在 Linux-only / closure 未闭环场景下缺少 next-action guidance。
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`，基于 `consistency_status + closure_status_note` 生成 `Next Actions`。
3. 复跑 focused consistency 与 handoff/workflow 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified missing next-action guidance on the consistency report surface
- [completed] wrote focused contract for consistency next-action truth
- [completed] minimal state-driven next-actions guidance in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- 当前 Linux-only / closure 未闭环样本已经证明 consistency 报告表面仍有误导：
  - `consistency_status: **CONSISTENT**`
  - `closure_status_note: IN_PROGRESS`
  - 但正文没有 `## Next Actions`
  - 也没有说明“evidence consistency 绿色不等于 handoff 已闭环”
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - consistency report should include next-action guidance when closure is still in progress
- minimal implementation:
  - 更新 `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added state-driven `NEXT_ACTIONS`
    - `IN_PROGRESS` 时显式说明这只代表 evidence consistency，handoff 尚未闭环
    - report now points operators back to `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - kept required-missing / parse-issue gate logic unchanged
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Closure Status Parse Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 malformed `closure_report` 的假绿灯，避免 closure 报告缺少 `closure_status` 时仍被判为 `CONSISTENT`。

## Current Batch
1. 写 focused contract，证明缺少 `closure_status` 的 `closure_report` 仍会被 strict 误放行。
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`，把 `closure_status` 缺失/非法视为 parse issue。
3. 复跑 focused consistency 与 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified false-green consistency behavior for malformed closure report metadata
- [completed] wrote focused contract for missing closure_status truth
- [completed] minimal closure_report parse validation in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- 直接构造的样本已经证明旧逻辑存在 repo-side 假绿灯：
  - `closure_report` 文件存在、`run_id` 正常
  - 但缺少 `closure_status`
  - `check_wave_b_b2_evidence_consistency.sh` 仍输出：
    - `consistency_status: **CONSISTENT**`
    - `runid_mismatch_or_parse_issue: 0`
    - `closure_status_note:` 空白
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: FAIL before fix
  - failure shape:
    - strict consistency 错误接受了缺少 `closure_status` 的 `closure_report`
- minimal implementation:
  - 更新 `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `is_valid_closure_status(...)`
    - added `check_closure_report_artifact(...)`
    - `closure_report` 现在除 run_id 外，还要求 `closure_status` 存在且属于 `IN_PROGRESS` / `CLOSED`
    - 缺失或非法时计入 `runid_mismatch_or_parse_issue`，并把顶层 `closure_status_note` 与 artifact row note 写成显式 parse issue
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Closure Next Action Truth

## Goal
收口 `check_wave_b_b2_closure_readiness.sh` 报告中的 `Next Actions` 入口漂移，避免 closure readiness 仍引导调用者去复跑 `generate_wave_b_cross_platform_summary.sh`，而不是当前真实的 `prepare_wave_b_b2_handoff_bundle.sh` 上层交接入口。

## Current Batch
1. 写 focused contract，证明 closure report 仍引用旧的 cross-summary 重跑入口。
2. 最小修改 `check_wave_b_b2_closure_readiness.sh` 的 `Next Actions` 文案。
3. 跑 focused closure/handoff 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified stale closure-report rerun guidance after workflow handoff truth-source consolidation
- [completed] wrote focused contract for closure next-action truth
- [completed] minimal closure next-action wording sync
- [completed] focused verification and review closeout

## Current Evidence
- 当前 `check_wave_b_b2_closure_readiness.sh` 报告尾部仍写：
  - `三平台 summary 回填后，复跑 'scripts/generate_wave_b_cross_platform_summary.sh'。`
- 但 repo 里当前真实的上层收口入口已经变成：
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - workflow summary 也已经统一走 `prepare`
- 继续保留旧文案会制造一个新的 report-side 误导：
  - cross summary 单独复跑并不会刷新 consistency / handoff bundle
  - 调用者会被引向一个已经不再是主入口的脚本
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - closure readiness report still told operators to rerun `scripts/generate_wave_b_cross_platform_summary.sh`
    - report output had no mention of the current `prepare` handoff entrypoint
- minimal implementation:
  - 新增 `tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
  - 更新 `scripts/check_wave_b_b2_closure_readiness.sh`
    - the final rerun guidance now points to the Wave B/B2 handoff bundle prepare flow instead of the stale cross-summary-only entrypoint
    - kept closure readiness state evaluation unchanged
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Strict Input Description Truth

## Goal
收口 workflow 输入 `strict_closure` 的描述漂移，避免它仍宣称“只在 B2 not closed 时失败”，而当前真实语义已经是完整 `prepare --strict` handoff 失败路径。

## Current Batch
1. 写 focused workflow contract，证明 `strict_closure` 的输入描述仍停留在 closure-only 旧语义。
2. 最小修改 live 与 `.disabled` 模板的 `strict_closure` 描述文字。
3. 跑 focused workflow 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified stale strict_closure input wording after strict-boundary review
- [completed] wrote focused workflow contract for strict input description truth
- [completed] minimal live + disabled workflow input description sync
- [completed] focused verification and review closeout

## Current Evidence
- 当前 workflow 入口仍写：
  - `strict_closure: description: Fail workflow if B2 not closed`
- 但它实际映射到：
  - `prepare_wave_b_b2_handoff_bundle.sh --strict`
  - 而这条 strict 路径会同时因为：
    - closure 未闭环
    - evidence consistency 为 `INCONSISTENT`
    - Windows runtime artifacts 缺失
    - 其他 required evidence 缺失
    触发非 0
- 这会把一个已经变成“完整 handoff strict” 的输入，继续伪装成 “closure-only” 开关。
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`: FAIL before fix
  - failure shape:
    - live workflow 仍写 `description: Fail workflow if B2 not closed`
    - `.disabled` 模板也保留相同的 closure-only 旧文案
- minimal implementation:
  - 新增 `tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
  - 更新：
    - `.github/workflows/wave-b-b2-manual.yml`
    - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - live 与 disabled 双模板都把 `strict_closure` 描述改为：
    - `Fail workflow if Wave B/B2 handoff is not fully closed and consistent`
- focused GREEN:
  - `bash -n tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`: PASS
  - `git diff --check`: PASS
  - `diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`: PASS

# Task Plan - Wave B/B2 Prepare Strict Metadata Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh --strict` 的报告元数据漂移，避免严格模式下生成出来的 closure/consistency markdown 仍把 `strict_mode` 写成 `false`。

## Current Batch
1. 写 focused contract，证明 `prepare --strict` 虽然会在最后按严格模式失败，但它先生成的 closure/consistency 报告仍错误标记 `strict_mode: false`。
2. 仅在 `prepare_wave_b_b2_handoff_bundle.sh` 内最小修补已生成报告的 `strict_mode` 元数据。
3. 跑 focused 合同与 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified strict-mode metadata drift after workflow strict-boundary review
- [completed] wrote focused contract for prepare strict metadata truth
- [completed] minimal prepare strict metadata synchronization
- [completed] focused verification and review closeout

## Current Evidence
- 当前 `prepare_wave_b_b2_handoff_bundle.sh` 为了保证 strict 失败前仍能先生成报告，主流程调用：
  - `check_wave_b_b2_closure_readiness.sh` 时不带 `--strict`
  - `check_wave_b_b2_evidence_consistency.sh` 时也不带 `--strict`
- 真正的严格失败只发生在报告写完之后追加的：
  - `check_wave_b_b2_evidence_consistency.sh --strict --dry-run`
  - `check_wave_b_b2_closure_readiness.sh --strict --dry-run`
- 结果就是当 `prepare` 自己是 `--strict` 时：
  - handoff bundle 会写 `- strict_mode: true`
  - 但 closure/consistency 两份落盘 markdown 仍会写 `- strict_mode: false`
- 这会让同一批上传证据在严格模式元数据上自相矛盾。
- focused RED:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: FAIL before fix
  - failure shape:
    - `prepare --strict` 的 handoff bundle 已经写 `strict_mode: true`
    - 但 closure readiness 报告仍写 `strict_mode: false`
    - consistency 报告同样保持旧的 `false`
- minimal implementation:
  - 新增 `tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
  - 更新 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `sync_report_strict_mode(...)`
    - after generating closure/consistency reports, `prepare` now rewrites their `- strict_mode:` line to the effective top-level `STRICT` value
    - retained the existing execution order: generate all reports first, then run strict dry-run failure checks
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Optional Runner Artifact Download Tolerance

## Goal
收口 `summary` job 在下载 macOS/Windows artifact 时的提前失败风险，避免 workflow 明明已经有 `prepare_wave_b_b2_handoff_bundle.sh` 的缺证据判定链，却在更前面的 `download-artifact` 步骤就因为 runner 产物缺失直接中断。

## Current Batch
1. 写 focused workflow contract，要求 Linux artifact download 保持严格，但 macOS/Windows artifact download 对缺失容错。
2. 最小修改 live 与 `.disabled` 模板的 summary 下载步骤。
3. 跑 focused workflow 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the pre-prepare artifact-download failure risk after Linux baseline cleanup
- [completed] wrote focused workflow contracts for optional runner artifact download tolerance
- [completed] minimal live + disabled workflow download-step hardening
- [completed] focused verification and review closeout

## Current Evidence
- 当前 `summary` job 已经把缺失平台证据的语义收敛到：
  - `prepare_wave_b_b2_handoff_bundle.sh`
  - cross summary / closure / consistency / handoff bundle
- 但它在这之前仍有两步无容错下载：
  - `Download macOS evidence`
  - `Download Windows evidence`
- 一旦上游 runner 因失败、无文件、或 `upload-artifact` 仅产生 `warn` 而没有真正 artifact，`download-artifact` 就可能先把 summary job 终止，后面的 `prepare` 根本不会运行。
- 这和当前 repo truth 冲突：
  - macOS/Windows 缺证据本来就应该由 handoff 链生成 `PENDING` / `READY_FOR_RUNNER` / `NEEDS_EVIDENCE_SYNC`
  - Linux evidence 仍然是 required truth，不应被同样放宽
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`: FAIL before fix
  - failure shape:
    - live workflow 的 `Download macOS evidence` / `Download Windows evidence` 仍无 `continue-on-error: true`
    - summary job 仍可能在进入 `prepare` 之前就因 runner artifact 缺失而提前失败
- minimal implementation:
  - 新增 `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
  - 更新：
    - `.github/workflows/wave-b-b2-manual.yml`
    - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - live 与 disabled 双模板都：
    - `Download macOS evidence` 增加 `continue-on-error: true`
    - `Download Windows evidence` 增加 `continue-on-error: true`
    - 保持 `Download Linux evidence` 继续严格
- focused GREEN:
  - `bash -n tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Linux Baseline Required Workflow Truth

## Goal
收口 `wave-b-b2-manual` workflow 中 `run_linux_baseline` 的假可选分支，避免 handoff summary 已经强依赖 Linux summary，但 dispatch 输入仍宣称 Linux baseline 可以关闭，导致 workflow 走进必坏路径。

## Current Batch
1. 写 focused workflow contract，证明 Linux baseline 现在必须是 summary/handoff 的前提，而不是一个可安全关闭的开关。
2. 最小修改 live 与 `.disabled` 模板，删除 `run_linux_baseline` 输入和相关条件分支。
3. 跑 focused workflow 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the broken optional-linux branch after workflow handoff truth-source sync
- [completed] wrote focused contracts for required Linux baseline truth
- [completed] minimal live + disabled workflow cleanup for required Linux baseline
- [completed] focused verification and review closeout

## Current Evidence
- 当前 `summary` job 无条件构造：
  - `LINUX_SUMMARY="test-reports/wave_b_ci_gate_summary_${RUN_ID}.md"`
  - `LINUX_EXAMPLES="test-reports/examples_compile_ci_gate_${RUN_ID}.json"`
  - 并把它们直接传给 `prepare_wave_b_b2_handoff_bundle.sh`
- `prepare_wave_b_b2_handoff_bundle.sh` 又明确要求 Linux summary 必须存在，否则直接：
  - `[ERROR] linux summary not found: ...`
- 但 workflow 仍保留一条假可选分支：
  - dispatch input: `run_linux_baseline`
  - `linux-gate` job 有 `if: ${{ github.event.inputs.run_linux_baseline != 'false' }}`
  - `Download Linux evidence` step 也有同样的条件
- repo 内没有任何其他合同或文档要求支持 `run_linux_baseline=false` 的可工作路径；现状只会把用户引导进一个静态上必坏的 summary/handoff 路径。
- focused RED:
  - 新增 `tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
  - `bash -n tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: FAIL before fix
  - failure shape:
    - live workflow 仍暴露 `run_linux_baseline`
    - `linux-gate` 与 `Download Linux evidence` 仍有 `run_linux_baseline != 'false'` 条件分支
    - 这与 summary/handoff 必需 Linux truth 的现状冲突
- minimal implementation:
  - 新增 `tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
  - 更新：
    - `.github/workflows/wave-b-b2-manual.yml`
    - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - live 与 disabled 双模板都：
    - 删除 dispatch input `run_linux_baseline`
    - 删除 `linux-gate` job 的条件分支
    - 删除 `Download Linux evidence` step 的条件分支
- focused GREEN:
  - `bash -n tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `git diff --check`: PASS
  - `diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`: PASS

# Task Plan - Wave B/B2 Disabled Workflow Handoff Truth Sync

## Goal
收口 `.github/workflows/wave-b-b2-manual.yml.disabled` 与 live workflow 的 handoff summary 漂移，避免 disabled 模板恢复启用或被人工对照时重新带回旧的 `generate/closure/consistency` 平行实现。

## Current Batch
1. 扩大 workflow handoff contract，让它同时约束 live 与 `.disabled` 两个模板。
2. 用 RED 证明 `.github/workflows/wave-b-b2-manual.yml.disabled` 仍停在旧的重复 summary 逻辑。
3. 最小同步 disabled 模板到 `prepare_wave_b_b2_handoff_bundle.sh` 单一入口。
4. 跑 focused workflow 回归，更新 working-memory，并在 review 后提交。

## Status
- [completed] identified disabled workflow drift after live workflow truth-source closeout
- [completed] focused RED contract expansion for live + disabled templates
- [completed] minimal disabled-template handoff truth sync
- [completed] focused verification and review closeout

## Current Evidence
- `tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh` 初版只约束 live workflow，无法阻止 `.github/workflows/wave-b-b2-manual.yml.disabled` 继续漂移。
- fresh RED:
  - 将该合同扩大为同时检查：
    - `.github/workflows/wave-b-b2-manual.yml`
    - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - `bash -n tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: FAIL before fix
  - failure shape:
    - `.github/workflows/wave-b-b2-manual.yml.disabled` 仍缺少 `PREPARE_ARGS`
    - 仍保留 `MACOS_*ARGS` / `WINDOWS_EVIDENCE_ARGS`
    - 仍直接调用 `generate/closure/consistency`
    - 仍未上传 `wave_b_b2_handoff_bundle_<run_id>.md`
- minimal implementation:
  - 更新 `tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`，让它统一校验 live + disabled 双模板
  - 更新 `.github/workflows/wave-b-b2-manual.yml.disabled`
    - summary step 改成 `PREPARE_ARGS -> prepare_wave_b_b2_handoff_bundle.sh`
    - `strict_closure=true` 时映射到 `PREPARE_ARGS+=(--strict)`
    - final upload 增加 `wave_b_b2_handoff_bundle_<run_id>.md`
- focused GREEN:
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Workflow Handoff Truth Source

## Goal
收口 `.github/workflows/wave-b-b2-manual.yml` 的 summary job 逻辑漂移，避免 workflow 继续手工拼装 `cross summary / closure / consistency`，却绕开已经收敛好的 `prepare_wave_b_b2_handoff_bundle.sh` 单一交接入口。

## Current Batch
1. 写 focused RED workflow contracts，证明 summary job 仍在复制 `MACOS_*ARGS` / `WINDOWS_*ARGS` 与三个下游脚本调用，且没有上传 handoff bundle。
2. 仅在 workflow 和相关合同测试中改成统一走 `prepare_wave_b_b2_handoff_bundle.sh`。
3. 跑 workflow 合同与 handoff 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] resynced branch, planning files, and current Wave B/B2 handoff lane state
- [completed] wrote RED workflow contracts for single-source handoff summary
- [completed] minimal workflow truth-source switch to prepare script
- [completed] focused verification and review closeout

## Current Evidence
- 当前 `summary` job 仍在 workflow 内重复维护：
  - `MACOS_CROSS_ARGS`
  - `MACOS_SUMMARY_ARGS`
  - `MACOS_CONSISTENCY_ARGS`
  - `WINDOWS_SUMMARY_ARGS`
  - `WINDOWS_EVIDENCE_ARGS`
- 它还直接调用了：
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
- 但 repo 里更新更快、语义更完整的真相源已经是：
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 该脚本会统一生成 cross/closure/consistency/handoff bundle，并承载 replay command、state-driven next actions、Windows companion artifact list、explicit missing passthrough
- workflow 目前上传的最终 summary artifacts 仍只有：
  - `wave_b_cross_platform_summary_<run_id>.md`
  - `wave_b_b2_closure_readiness_<run_id>.md`
  - `wave_b_b2_evidence_consistency_<run_id>.md`
  - 缺少 `wave_b_b2_handoff_bundle_<run_id>.md`
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: FAIL before fix
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: FAIL before fix
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: FAIL before fix
- minimal implementation:
  - 新增 `tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
  - 更新：
    - `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
    - `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - `.github/workflows/wave-b-b2-manual.yml`
    - summary job 改为构造单个 `PREPARE_ARGS`
    - `strict_closure=true` 时只映射到 `prepare --strict`
    - 删除 workflow 内部重复的 `MACOS_*ARGS` / `WINDOWS_EVIDENCE_ARGS` 与三个 direct script calls
    - final upload 新增 `wave_b_b2_handoff_bundle_<run_id>.md`
- focused GREEN:
  - `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Bundle Next Actions

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 `Next Actions` 模板误导问题，避免 bundle 在平台已经 PASS 或整体已 CLOSED 时，仍固定提示去跑 macOS/Windows runner。

## Current Batch
1. 写 focused RED contract，覆盖“Windows 已 PASS 但 macOS 未完成”与“整体 CLOSED”两种 stale next-actions 场景。
2. 仅在 `prepare_wave_b_b2_handoff_bundle.sh` 内改成状态驱动的 `Next Actions` 生成。
3. 跑 focused 合同、replay command、artifact list、explicit-missing 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the stale next-actions gap after the replay-command batch
- [completed] focused RED contract for state-driven next actions
- [completed] minimal next-actions hardening
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - when Windows summary/logs are already green, bundle still says “在 Windows runner 执行 live gate”
  - when macOS and Windows are both PASS and the package is `CLOSED`, bundle still prints both runner steps
- target scope for this batch:
  - keep evidence selection and replay command semantics unchanged
  - only make `Next Actions` follow current closure/runtime truth instead of a fixed template
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: FAIL before fix
  - failure shape:
    - partial-green bundle still prompted a stale Windows live-gate action even when Windows was already PASS
    - fully closed bundle still kept stale macOS/Windows runner instructions
- minimal implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `parse_closure_platform_state(...)`
    - `Next Actions` now derives from macOS/Windows closure states plus Windows companion-runtime presence
    - `CLOSED` bundles now emit only an optional replay step
    - Windows summary PASS but missing companion runtime artifacts now maps to a Windows-runtime-artifact action instead of a stale summary action
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Bundle Replay Command

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 在 handoff bundle 里生成的 replay command 缺口，避免报告只剩 `--run-id --strict`，却丢掉本批次实际使用的自定义路径上下文。

## Current Batch
1. 写 focused RED contract，证明 custom linux/windows/output-dir 已参与当前批次，但 handoff bundle replay command 仍无法复现这条 truth。
2. 仅在 `prepare_wave_b_b2_handoff_bundle.sh` 内生成保留关键 top-level args 的 replay command。
3. 跑 focused 合同、handoff artifact-list、explicit-missing passthrough 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the replay-command gap after the handoff bundle artifact-list batch
- [completed] focused RED contract for replay command truth
- [completed] minimal replay-command hardening
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - handoff bundle artifact table already records custom linux summary/examples/windows summary and custom output dir
  - but `Next Actions` still tells the user to rerun only `scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id <id> --strict`
  - that command cannot reconstruct the same custom evidence chain
- target scope for this batch:
  - keep current evidence selection semantics unchanged
  - only make the replay command preserve the batch-defining top-level args
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: FAIL before fix
  - failure shape:
    - handoff bundle artifact table already recorded custom linux/windows/output-dir truth
    - but `Next Actions` still emitted only `--run-id ... --strict`
    - the replay command therefore could not reproduce the same custom evidence chain
- minimal implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `build_shell_command(...)` for shell-safe replay command rendering
    - replay command now preserves `run_id`, `linux_summary`, `linux_examples`, `output_dir`, and active/explicit top-level macOS/Windows evidence args
    - default no-evidence surfaces stay omitted, so replay guidance does not invent new explicit-missing semantics
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Bundle Windows Artifact List

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 artifact 清单缺口，避免 handoff bundle 已经引用 Windows summary 和 consistency truth，却仍漏掉 companion quick/runtime logs。

## Current Batch
1. 扩 focused RED contracts，证明确有 Windows companion path truth 时，handoff bundle 仍不列 quick/runtime artifacts。
2. 仅在 `prepare_wave_b_b2_handoff_bundle.sh` 内把 Windows companion artifacts 加进 bundle 清单。
3. 跑 focused 合同、显式缺失 passthrough、consistency 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the handoff bundle artifact-list gap after the explicit-missing passthrough batch
- [completed] focused RED contracts for Windows companion artifact listing
- [completed] minimal handoff bundle artifact-list hardening
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - handoff bundle already tracks `windows_summary`
  - consistency report already tracks `windows_quick_log` / `windows_runtime_transcript`
  - but bundle artifact list still omits those two companion runtime artifacts entirely
- target scope for this batch:
  - keep existing Windows companion derivation logic unchanged
  - only bring the bundle index surface up to the same artifact truth
- focused RED:
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: FAIL before fix
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: FAIL before fix
  - failure shape:
    - consistency already tracked `windows_quick_log` / `windows_runtime_transcript`
    - but handoff bundle artifact list still omitted both rows entirely
    - the omission happened both when companion logs existed and when explicit Windows summary made them required-but-missing
- minimal implementation:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - artifact list now builds a `BUNDLE_ARTIFACTS` array
    - when `WINDOWS_EVIDENCE_ARGS` is active, the bundle now lists both derived companion runtime artifacts
    - existing/missing status is rendered through the same artifact loop as the rest of the bundle
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Explicit Missing Evidence Passthrough

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 与 `generate_wave_b_cross_platform_summary.sh` 对显式缺失 evidence path 的吞参/静默降级问题，避免调用者明明传了具体路径，下游摘要却仍写成 `no evidence`，甚至让 consistency 假绿。

## Current Batch
1. 写 focused RED contracts，复现 direct `generate` 与 `prepare` 入口对显式缺失 macOS/Windows evidence 的吞参与静默 `no evidence`。
2. 仅在 `prepare` / `generate` 内补齐显式缺失 evidence 的透传与展示语义。
3. 跑 focused 合同、explicit-required、run_id、inactive-probe 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the explicit-missing passthrough gap after the explicit-artifact requiredness batch
- [completed] focused RED contracts for generate/prepare explicit-missing evidence truth
- [completed] minimal explicit-missing passthrough hardening in prepare/generate
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - `prepare` only forwards macOS/Windows args when the file already exists
  - explicit missing `--macos-summary` / `--windows-summary` therefore disappear before reaching downstream scripts
  - direct `generate` also renders explicit missing summary paths as plain `no evidence`
  - resulting `consistency` report can stay `CONSISTENT` even though the caller explicitly requested missing summary evidence to be checked
- target scope for this batch:
  - explicit missing evidence must stay visible through `prepare -> generate/closure/consistency`
  - direct `generate` should distinguish explicit missing file from default no-evidence state
- focused RED:
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`: FAIL before fix
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: FAIL before fix
  - failure shape:
    - direct `generate` collapsed explicit missing macOS/Windows evidence into generic `no evidence`
    - `prepare` only forwarded macOS/Windows args when files already existed
    - downstream `consistency` therefore stayed green and `handoff_state` stayed `READY_FOR_RUNNER`
- minimal implementation:
  - new focused contracts:
    - `tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`
    - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - added `MACOS_PROBE_EXPLICIT`
    - explicit missing `macos_summary`, explicit missing `macos_probe`, and explicit missing `windows_summary` now surface as `...(missing file)` instead of `no evidence`
    - explicit missing `macos_summary` now keeps priority over probe fallback
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added explicit flags for `macos_probe`, `macos_summary`, and `windows_summary`
    - explicit missing macOS/Windows evidence now continues to flow into downstream `generate` / `closure` / `consistency`
    - explicit `windows_summary` still derives companion runtime logs for downstream consistency
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Explicit Artifact Requiredness

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对显式非 Linux evidence 参数的 required 语义漂移，避免调用者明明传了 `--macos-summary` / `--windows-summary` / Windows runtime logs，strict 却仍给出假绿灯。

## Current Batch
1. 写 focused RED contracts，证明显式 macOS/Windows summary 与显式 Windows runtime logs 在 strict 下仍被静默降成 optional。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内统一显式非 Linux evidence 的 required 语义。
3. 跑 focused 合同、run_id 回归、active-path、Windows strict 与 inactive macOS probe 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the explicit-artifact requiredness gap after the report-chain run_id fallback batch
- [completed] focused RED contracts for explicit non-Linux evidence requiredness
- [completed] minimal explicit-artifact requiredness hardening in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- fresh repros already show the bug shape:
  - explicit `--macos-summary <missing>` still let strict return green
  - explicit `--windows-summary <missing>` still let strict return green
  - explicit `--windows-quick-log <missing>` and `--windows-runtime-transcript <missing>` also still stayed optional
- target scope for this batch:
  - explicit non-Linux evidence should become required because the caller asked to validate it
  - explicit `windows_summary` should also activate the existing sibling runtime strictness
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`: FAIL before fix
  - `bash -n tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`: FAIL before fix
  - failure shape:
    - explicit `--macos-summary`, explicit `--windows-summary`, and explicit Windows runtime logs all still showed `missing`
    - but `required_missing` stayed `0`
    - strict therefore returned false green
- minimal implementation:
  - new focused contracts:
    - `tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
    - `tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - explicit `macos_summary` now becomes required evidence
    - explicit `windows_summary` now becomes required evidence and activates sibling runtime strictness
    - explicit `windows_quick_log` / `windows_runtime_transcript` now each become required evidence even without a Windows summary
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Existing Report Run ID Fallback

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 在 active Linux summary 缺失时的 `run_id` 次生污染，避免它明明拿到了现有 `cross summary + closure report`，却仍回退到新时间戳并把这两份报告一起误记成 mismatch。

## Current Batch
1. 写 focused RED contract，证明 active Linux summary 缺失后，direct consistency 仍会把现有 `cross summary` / `closure report` 的真实 `run_id` 污染成 mismatch。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内增加从现有 markdown reports 自身回收 `run_id` 的 fallback。
3. 跑 focused 合同、上一批 run_id inference 合同，以及 Linux/macOS/Windows 邻近回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the next report-chain run_id fallback gap after the cross-summary-driven run_id batch
- [completed] focused RED contract for existing-report run_id fallback
- [completed] minimal report-chain run_id fallback hardening in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - active custom `linux_summary` had already been removed
  - existing `cross summary` and `closure report` still both carried the real batch run_id
  - direct consistency still minted a fresh timestamp run_id and added two fake mismatches on top of the real missing-summary error
- target scope for this batch:
  - keep strict failure on missing active Linux summary
  - only stop polluting existing reports with a fresh timestamp when their own run_id is already available
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: FAIL before fix
  - failure shape:
    - active custom `linux_summary` was missing
    - existing `cross summary` and `closure report` still both carried the real batch run_id
    - direct consistency minted a fresh timestamp and added two fake mismatches on top of the real missing-summary failure
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `infer_run_id_from_markdown_artifact(...)`
    - `RUN_ID` now falls back in this order: explicit value -> explicit/inherited Linux summary -> cross-summary-declared active Linux summary -> cross summary run_id -> closure report run_id -> timestamp
    - synced `--help` text to the broader report-chain fallback order
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Run ID Inference

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 `RUN_ID` 推导时序缺口，避免 direct consistency 在只拿到现有 `cross summary + closure report` 时，先生成新的时间戳 `run_id`，再把同一批 active Linux evidence 误判成 mismatch。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已声明 active custom `linux_summary`，但 direct consistency 在省略 `--run-id` / `--linux-summary` 时仍会错误生成新的 `RUN_ID`。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内调整 `RUN_ID` 推导时序，让它能先消费 cross-summary-declared active Linux truth。
3. 跑 focused 合同，以及 Linux/macOS/Windows active-path、Windows required、inactive macOS probe 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] identified the next false-red gap after the active Windows required batch
- [completed] focused RED contract for cross-summary-driven run_id inference
- [completed] minimal run_id inference hardening in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- fresh repro already shows the bug shape:
  - omitted `--run-id` and `--linux-summary`
  - provided existing `--cross-summary` and `--closure-report`
  - checker minted a timestamp `run_id`, then later inherited the active custom `linux_summary`
  - resulting report counted `linux_summary`, `cross_summary`, and `closure_report` as `run_id mismatch`
- target scope for this batch:
  - keep active path inheritance semantics unchanged
  - only move `RUN_ID` default truth closer to the inherited active Linux summary
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already declared an active custom `linux_summary`
    - direct consistency was invoked without `--run-id` / `--linux-summary`
    - checker minted a fresh timestamp first and then marked aligned `linux_summary` / `cross_summary` / `closure_report` as mismatched
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - moved `parse_cross_summary_linux_summary_path(...)` ahead of `RUN_ID` defaulting
    - added `infer_run_id_from_cross_summary_linux_summary(...)`
    - `RUN_ID` now falls back in this order: explicit value -> explicit/inherited Linux summary -> cross-summary-declared active Linux summary -> timestamp
    - synced `--help` text to the new default run_id truth
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Windows Summary Required

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 active custom `windows_summary` 的 required 语义缺口，避免在 `cross summary` 已承认 Windows summary 为 active evidence 时，summary 本体和 sibling runtime artifacts 仍被当成 optional missing。

## Current Batch
1. 写 focused RED contract，证明 `cross summary` 已声明 active custom `windows_summary`，但 direct consistency 仍把它和 companion runtime artifacts 记成 optional / green。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内把 cross-summary-declared Windows summary 提升为 required evidence，并让 runtime strictness 跟着 active summary truth 走。
3. 跑 focused 合同、active Windows summary path、Windows companion-path、linux/macOS active-path 与 inactive-probe 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the required-semantics gap after the active Windows summary path batch
- [completed] focused RED contract for active Windows required evidence
- [completed] minimal Windows required-semantics hardening in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: FAIL before fix
  - failure shape:
    - `cross summary` already declared an active custom `windows_summary`
    - that summary was then removed
    - direct consistency still kept `required_missing=0` and left the two runtime artifacts as optional missing
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - active cross-summary-declared `windows_summary` is now required evidence
    - when that active Windows truth exists, sibling `windows_quick_log` and `windows_runtime_transcript` also become required evidence even if the summary file is already missing
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Linux Summary Path

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 Linux active-path 漂移，让它在未显式传 `--linux-summary` 时，也能从 `cross summary` 继承实际使用的 custom `linux_summary` 路径，并在 active summary 缺失/漂移时不再给出假绿灯。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已经声明 active custom `linux_summary`，但 direct consistency 仍会忽略它并保持 green。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `linux_summary` 路径的解析与继承。
3. 跑 focused 合同、linux examples active-path、active macOS summary、active Windows summary、inactive macOS probe 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the next active Linux truth drift after the active macOS summary batch
- [completed] focused RED contract for cross-summary-declared custom linux summary
- [completed] minimal active linux summary inheritance in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already declared an active custom `linux_summary`
    - that active summary was then removed
    - direct consistency still stayed green because it tracked the default run-specific Linux summary path instead
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `LINUX_SUMMARY_EXPLICIT`
    - added `parse_cross_summary_linux_summary_path(...)`
    - when `--linux-summary` is omitted, now inherits active Linux summary from cross summary
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary macOS Summary Path

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 macOS active-path 漂移，让它在未显式传 `--macos-summary` 时，也能从 `cross summary` 继承实际使用的 custom `macOS summary` 路径，并在 active summary 缺失/漂移时不再给出假绿灯。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已经声明 active custom `macOS summary`，但 direct consistency 仍会忽略它并保持 green。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `macOS summary` 路径的解析与继承。
3. 跑 focused 合同、inactive macOS probe、probe-only consistency、Windows active-summary 与 linux examples active-path 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the next active macOS truth drift after the active Windows summary batch
- [completed] focused RED contract for cross-summary-declared custom macOS summary
- [completed] minimal active macOS summary inheritance in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already declared an active custom `macOS summary`
    - that active summary was then removed
    - direct consistency still stayed green because it only tracked the default macOS summary path
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `MACOS_SUMMARY_EXPLICIT`
    - added `parse_cross_summary_macos_summary_path(...)`
    - when `--macos-summary` is omitted, now inherits active macOS summary from cross summary
    - treats that inherited active summary as required evidence, while keeping probe-only logic separate
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Windows Summary Path

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 Windows active-path 漂移，让它在未显式传 `--windows-summary` 时，也能从 `cross summary` 继承实际使用的 custom `windows_summary` 路径，并因此正确触发 Windows runtime artifact 严格校验。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已经声明 active custom `windows_summary`，但 direct consistency 仍会忽略它并给出假绿灯。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `windows_summary` 路径的解析与继承。
3. 跑 focused 合同、Windows companion-path、linux examples active-path、macOS probe 与 Windows strict 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the next active Windows truth drift after the companion-path batch
- [completed] focused RED contract for cross-summary-declared custom windows summary
- [completed] minimal active windows summary inheritance in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already declared an active custom `windows_summary`
    - that active Windows evidence should have triggered runtime artifact strictness
    - but direct consistency still ignored it and returned green
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `WINDOWS_SUMMARY_EXPLICIT`
    - added `WINDOWS_QUICK_LOG_EXPLICIT`
    - added `WINDOWS_RUNTIME_TRANSCRIPT_EXPLICIT`
    - added `parse_cross_summary_windows_summary_path(...)`
    - when Windows args are omitted, now inherits active `windows_summary` from cross summary before deriving sibling runtime artifact paths
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Windows Companion Path

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 在 custom `windows_summary` 场景下的 companion-path 漂移，让它在未显式传 `--windows-quick-log` / `--windows-runtime-transcript` 时，也能默认跟随 `windows_summary` 同目录寻找 sibling artifacts。

## Current Batch
1. 写 focused RED contract，证明 custom `windows_summary` 与 sibling runtime artifacts 已存在，但 direct consistency 仍误报缺失。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内补一层 sibling-artifact 默认推导。
3. 跑 focused 合同、linux examples active-path、macOS probe、Windows strict 与 handoff companion-path 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the direct custom-windows companion-path drift after the linux examples active-path batch
- [completed] focused RED contract for custom windows summary sibling artifacts
- [completed] minimal windows companion-path default alignment in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: FAIL before fix
  - failure shape:
    - custom `windows_summary` existed
    - sibling `winssl_quick_smoke_<run_id>.log` and `winssl_runtime_suite_<run_id>.log` also existed
    - `check_wave_b_b2_evidence_consistency.sh --strict` still returned non-zero because it only checked `test-reports/...`
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `derive_sibling_artifact_path(...)`
    - when Windows runtime artifact args are omitted, now derives them from `WINDOWS_SUMMARY` rather than hardcoding `test-reports/...`
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Cross Summary Linux Examples Path

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 active-path 漂移，让它在未显式传 `--linux-examples` 时，能够从 `cross summary` 继承实际使用的 `linux_examples_json` 路径，而不是继续盯着默认 generic JSON。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已经声明 custom `linux_examples_json`，但 consistency 仍可能因为 generic JSON 存在而给出假绿灯。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `linux_examples_json` 路径的解析与继承。
3. 跑 focused 合同、generic fallback、run-specific、macOS probe、Windows strict 与 run_id infer 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the next active-path truth drift after the generic fallback batch
- [completed] focused RED contract for cross-summary-declared linux examples path
- [completed] minimal linux examples active-path inheritance in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already recorded a custom `linux_examples_json` path
    - after that active JSON was intentionally corrupted
    - `check_wave_b_b2_evidence_consistency.sh --strict` still stayed green because it tracked the default generic JSON instead
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `LINUX_EXAMPLES_EXPLICIT`
    - added `parse_cross_summary_linux_examples_path(...)`
    - when `--linux-examples` is not explicitly passed, now prefers the active path declared by cross summary before falling back to run-specific/generic defaults
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Consistency Generic Linux Examples Fallback

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的 direct 调用缺口，让它在未显式传 `--linux-examples` 且只有旧 generic `test-reports/examples_compile_ci_gate.json` 存在时，也能与 `generate_wave_b_cross_platform_summary.sh` / `prepare_wave_b_b2_handoff_bundle.sh` 保持一致，不再误判为缺失。

## Current Batch
1. 写 focused RED contract，证明 cross summary 已经消费 generic Linux examples JSON，但 direct consistency 仍只认 run-specific 默认路径。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内补齐 `run-specific 优先、generic fallback` 的默认解析。
3. 跑 focused 合同、run-specific contract、run-id handoff contract 与 probe / windows strict 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the direct consistency generic-fallback drift after the inactive probe batch
- [completed] focused RED contract for generic linux examples fallback
- [completed] minimal linux examples default-path alignment in consistency checker
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`: FAIL before fix
  - failure shape:
    - cross summary already emitted `- linux_examples_json: test-reports/examples_compile_ci_gate.json`
    - but `check_wave_b_b2_evidence_consistency.sh --strict` still required `test-reports/examples_compile_ci_gate_<run_id>.json`
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `default_linux_examples_json_path(...)`
    - default precedence is now explicit `--linux-examples` > run-specific JSON > generic JSON fallback
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Ignore Inactive macOS Probe Consistency

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 的过度跟踪问题，避免在 macOS summary 已经是权威证据时，仅因默认路径下存在一个无关或损坏的 `wave_b_macos_gate_probe_<run_id>.json` 就把 strict consistency 误判为 `INCONSISTENT`。

## Current Batch
1. 写 focused RED contract，证明 inactive stale macOS probe 当前仍会污染 strict consistency。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内收紧 `macos_probe` 跟踪条件。
3. 修正新 contract 自身的误报断言，避免把 run_id/path 里的 `macos_probe` 子串误判成 artifact row。
4. 跑 focused 合同、active probe consistency 合同、handoff/workflow 回归与 diff hygiene。
5. 更新 working-memory，并在 review 后提交。

## Status
- [completed] resumed current static review batch from the existing macOS probe consistency lane
- [completed] focused RED proof for inactive stale macOS probe over-tracking
- [completed] minimal macOS probe tracking-condition hardening
- [completed] contract false-positive hardening for exact `macos_probe` row matching
- [completed] focused verification and review closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: FAIL before fix
  - failure shape:
    - `check_wave_b_b2_evidence_consistency.sh --strict` still failed when only a malformed default-path macOS probe existed beside an authoritative macOS summary
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - removed the fallback branch that tracked `MACOS_PROBE` merely because the default file existed
    - now tracks `macos_probe` only when the caller explicitly passes `--macos-probe`, or when cross summary explicitly reports `PROBE_ONLY/PROBE_OK`
  - contract hardening:
    - tightened the negative assertion to `^\\| macos_probe \\|`, so `run_id/path` substrings no longer create false failures
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 macOS Probe Consistency Hardening

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 macOS probe-only 证据的盲区，确保当 cross summary 使用 `wave_b_macos_gate_probe_<run_id>.json` 时，consistency report 也会显式列出并校验这份 probe 证据。

## Current Batch
1. 写 focused RED contract，证明 handoff bundle 已经消费 macOS probe，但 consistency report 仍不列出它。
2. 给 `check_wave_b_b2_evidence_consistency.sh` 增加 `--macos-probe` 支持。
3. 让 `prepare_wave_b_b2_handoff_bundle.sh` 与 `wave-b-b2-manual.yml` 在 probe-only 场景下把 probe 传入 consistency，但不改 closure 的 summary-only 语义。
4. 跑 focused 合同、workflow 文本合同、handoff/workflow 回归与 diff hygiene。
5. 更新 working-memory，并在 review 后提交。

## Status
- [completed] found the consistency-report blind spot after the probe fallback batches
- [completed] focused RED contracts for macOS probe consistency coverage
- [completed] minimal macOS probe consistency propagation
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: FAIL before fix
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: FAIL before fix
  - failure shape:
    - consistency report omitted a `macos_probe` row even though probe-only evidence was active
    - workflow summary stage passed no probe argument into `check_wave_b_b2_evidence_consistency.sh`
- implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`
  - updated workflow contract: `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - added `--macos-probe`
    - now surfaces probe JSON in the artifact matrix
    - can infer the active probe path from the cross summary when probe-only evidence is in use
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - now uses `MACOS_CONSISTENCY_ARGS` to pass probe-only evidence into consistency without widening closure args
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
    - summary stage now mirrors the same `MACOS_CONSISTENCY_ARGS` split
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B Cross Summary macOS Probe Default Hardening

## Goal
收口 `generate_wave_b_cross_platform_summary.sh` 的 direct 调用缺口，让它在未显式传 `--macos-probe` 且没有 macOS summary 时，也能自动拾取 `test-reports/wave_b_macos_gate_probe_<run_id>.json`。

## Current Batch
1. 写 focused RED contract，证明 direct cross-summary 入口仍会忽略默认 macOS probe。
2. 仅在 `generate_wave_b_cross_platform_summary.sh` 内补 run-specific probe 默认检测。
3. 跑 focused 合同、现有 cross-summary 回归、handoff probe 回归与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] post-commit static sweep found the remaining direct cross-summary probe default gap
- [completed] focused RED contract for default macOS probe pickup
- [completed] minimal default probe detection in cross summary generator
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: FAIL before fix
  - failure shape:
    - direct `generate_wave_b_cross_platform_summary.sh` still emitted `macos = PENDING / no evidence`
    - even though `test-reports/wave_b_macos_gate_probe_<run_id>.json` already existed
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - now defaults `MACOS_PROBE` to `test-reports/wave_b_macos_gate_probe_<run_id>.json` after run_id is known
    - keeps macOS summary precedence unchanged
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 macOS Probe Fallback Hardening

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 与 `wave-b-b2-manual.yml` 在 macOS probe-only 场景下丢失证据的问题，确保没有 macOS summary 时，cross summary 仍能吸收 `wave_b_macos_gate_probe_<run_id>.json` 并显示 `PROBE_ONLY`。

## Current Batch
1. 写 focused RED contract，证明 handoff bundle 在只有 macOS probe 时仍会把 macOS 降成 `PENDING/no evidence`。
2. 给 `prepare` 增加 `--macos-probe` 与默认 fallback，并把 workflow 汇总阶段也补成 summary-first / probe-fallback。
3. 修掉参数转发里的二次缺口：`--macos-probe` 只能进 cross summary，不能再误传给 closure/evidence。
4. 跑 focused 合同、workflow 文本合同、handoff/cross-summary 回归与 diff hygiene。
5. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state and isolated the next macOS probe-only evidence gap
- [completed] focused RED proof for handoff/workflow probe loss
- [completed] minimal macOS probe fallback plus argument-surface split
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - manual old-script repro against `git show HEAD:scripts/prepare_wave_b_b2_handoff_bundle.sh`: reproduced `macos = PENDING / no evidence` even though `test-reports/wave_b_macos_gate_probe_<run_id>.json` existed
  - `git show HEAD:.github/workflows/wave-b-b2-manual.yml | rg ...macos-probe...`: no match
  - failure shape:
    - old prepare/workflow both dropped probe-only evidence because only `--macos-summary` was ever forwarded
- implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`
  - new workflow contract: `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `--macos-probe`
    - now defaults to `test-reports/wave_b_macos_gate_probe_<run_id>.json`
    - uses `MACOS_CROSS_ARGS` vs `MACOS_SUMMARY_ARGS` so probe-only evidence goes only to cross summary
    - handoff artifact index now includes the macOS probe
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
    - summary stage now mirrors the same summary-first / probe-fallback split
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B Cross Summary Run ID Help Sync

## Goal
收口 `generate_wave_b_cross_platform_summary.sh` 的帮助文本漂移，确保 `--run-id` 文案与当前“优先从 Linux summary 推导，否则回退时间戳”的实际行为一致。

## Current Batch
1. 静态检查 `Wave B/B2` 四个共享脚本的 `--run-id` usage 文案。
2. 仅修 `generate_wave_b_cross_platform_summary.sh` 里残留的旧描述。
3. 跑语法检查、`--help` 文案匹配和 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] post-commit static sweep found the remaining run-id help drift
- [completed] minimal help-text sync in cross summary generator
- [completed] lightweight verification
- [in_progress] review and commit closeout

## Current Evidence
- bug located:
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    already said `默认优先从 Linux summary 推导，否则时间戳`
  - only `scripts/generate_wave_b_cross_platform_summary.sh` still said `默认时间戳`
- minimal implementation:
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - synced the `--run-id` help text with the actual run-id inference behavior
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash scripts/generate_wave_b_cross_platform_summary.sh --help | rg "默认优先从 Linux summary 推导，否则时间戳"`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Infer Run ID From Linux Summary

## Goal
收口 `Wave B/B2` 脚本在未显式传 `--run-id` 时的默认 run_id 漂移，确保 `prepare` / `generate` / `closure` / `consistency` 都优先继承 Linux summary 中的 run_id，而不是各自产生新的时间戳批次。

## Current Batch
1. 写 focused RED contract，证明 handoff bundle 在只提供 Linux summary 时会把输出文件命名到新的时间戳 run_id，造成静态证据分叉。
2. 统一四个共享脚本的 run_id 解析顺序：显式 `--run-id` > Linux summary 推导 > 时间戳 fallback。
3. 跑 focused 合同、cross summary / handoff / evidence / run-id passthrough 旧合同与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state and identified the next shared Wave B/B2 run_id drift
- [completed] focused RED contract for Linux-summary-driven run_id inference
- [completed] minimal run_id inference unification across generate/closure/consistency/prepare
- [in_progress] focused verification, review, and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: FAIL before fix
  - failure shape:
    - handoff outputs were named under a fresh timestamp run_id instead of the Linux summary run_id
    - consistency report therefore drifted to `INCONSISTENT`
- minimal implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - all now prefer explicit `--run-id`, otherwise infer from the Linux summary, then fall back to a timestamp only when inference is impossible
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`: PASS
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Run-Specific Linux Examples Default Hardening

## Goal
收口 `generate_wave_b_cross_platform_summary.sh` 与 `prepare_wave_b_b2_handoff_bundle.sh` 的 Linux examples JSON 默认路径漂移，确保在存在 `test-reports/examples_compile_ci_gate_<run_id>.json` 时优先消费 run-specific 产物，避免 handoff bundle 静默回落到陈旧 generic JSON。

## Current Batch
1. 写 focused RED contract，证明 `prepare_wave_b_b2_handoff_bundle.sh` 在未显式传 `--linux-examples` 时仍会把 generic JSON 注入 cross summary / consistency report。
2. 让 `prepare` 与已部分修过的 `generate` 统一采用“run-specific 优先、generic fallback”的默认路径策略。
3. 跑 focused 合同、cross-summary 回归合同、handoff 相关旧合同与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state and resumed the same static Wave B/B2 script chain
- [completed] focused RED contract for run-specific linux examples preference
- [completed] minimal default-path unification across generate and prepare
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: FAIL before fix
  - failure shape:
    - generated cross summary still recorded `linux_examples_json: test-reports/examples_compile_ci_gate.json`
    - metrics therefore came from the generic JSON instead of the run-specific fixture
- minimal implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - now prefers `test-reports/examples_compile_ci_gate_<run_id>.json`, falls back to the generic path only when needed
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - now mirrors the same default-path resolution instead of hardcoding the generic JSON
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Handoff Bundle Windows Companion Path Hardening

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 Windows companion artifact 路径推导缺陷：当调用者传入自定义 `--windows-summary` 路径时，默认 quick smoke / runtime transcript 也应跟随该 summary 同目录，而不是继续硬编码到 `test-reports/`。

## Current Batch
1. 写 focused RED contract，证明 handoff bundle 在自定义 Windows summary 路径下会错误去 `test-reports/` 查找 companion logs，导致假性 `INCONSISTENT`。
2. 对 `prepare_wave_b_b2_handoff_bundle.sh` 做最小修法，只修 companion path 推导。
3. 跑新 contract、handoff 相关旧 contract 与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state for the next static handoff micro-batch
- [completed] focused RED companion-path contract
- [completed] minimal handoff companion-path hardening
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: FAIL before fix
  - failure shape:
    - consistency report was generated
    - but it did not stay `CONSISTENT` even though the custom Windows summary already had sibling quick log + runtime transcript
- minimal implementation:
  - new focused contract: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - added `derive_sibling_artifact_path(...)`
    - when `windows_summary` exists, default `--windows-quick-log` / `--windows-runtime-transcript` now follow the summary directory instead of staying hardcoded at `test-reports/`
- focused GREEN:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B Cross-Platform Summary Absolute Input Hardening

## Goal
收口 `scripts/generate_wave_b_cross_platform_summary.sh` 的 absolute 输入路径缺陷，确保 `--linux-summary`、`--linux-examples`、`--macos-summary`、`--macos-probe`、`--windows-summary` 在跨目录调用时都能正确解析，而不是被再次拼接到 `PROJECT_ROOT` 下。

## Current Batch
1. 写 focused RED contract，从 `/tmp` 调用 cross-platform summary 脚本，传 absolute 输入路径与 absolute 输出路径，证明当前入口检查和后续读取都会误判。
2. 在脚本内补最小输入路径归一化，只统一读路径，不顺手扩成别的重构。
3. 跑旧 contract 回归、新 absolute-input contract、上一批 absolute-output contract 与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state for the next static review micro-batch
- [completed] focused RED absolute-input contract
- [completed] minimal input-path hardening
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: FAIL before fix
  - failure shape:
    - script exited at the Linux summary existence guard
    - even though `--linux-summary` pointed to a real absolute file
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - moved `resolve_path(...)` up to the input-normalization stage
    - introduced:
      - `LINUX_SUMMARY_ABS`
      - `LINUX_EXAMPLES_JSON_ABS`
      - `MACOS_PROBE_ABS`
      - `MACOS_SUMMARY_ABS`
      - `WINDOWS_SUMMARY_ABS`
    - switched all metadata/step/json reads to the normalized absolute paths
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Absolute Output Path Hardening

## Goal
收口 `Wave B/B2` 报告脚本链上的绝对输出路径缺陷，确保 `--output FILE` 与 `--output-dir DIR` 在传入 absolute path 时，文件真正写到目标绝对路径，而不是被错误拼接到 `PROJECT_ROOT` 下面。

## Current Batch
1. 写 focused RED contract，从 `/tmp` 调用三份报告脚本和 handoff bundle，给相对输入与绝对输出，证明当前 absolute output path 会落错位置。
2. 对 `generate_wave_b_cross_platform_summary.sh`、`check_wave_b_b2_closure_readiness.sh`、`check_wave_b_b2_evidence_consistency.sh` 做最小修法，只修输出路径归一化。
3. 跑 focused GREEN、现有 workflow contract 与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo state for the next static script-only review batch
- [completed] focused RED absolute-output contract
- [completed] minimal output-path hardening
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: FAIL before fix
  - failure shape:
    - `generate_wave_b_cross_platform_summary.sh` reported success
    - but the expected absolute output file was missing
    - root cause is the shared write pattern `"$PROJECT_ROOT/$OUTPUT_FILE"` in the report scripts
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
    - added local `resolve_path(...)` for output path normalization
    - writes report to `OUTPUT_ABS` instead of `"$PROJECT_ROOT/$OUTPUT_FILE"`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
    - now normalizes `OUTPUT_FILE` through existing `resolve_path(...)`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - now normalizes `OUTPUT_FILE` through existing `resolve_path(...)`
- focused GREEN:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`: PASS
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`: PASS
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `git diff --check`: PASS

# Task Plan - Wave B/B2 Windows Runtime Evidence Consistency Hardening

## Goal
收口 Wave B/B2 最终证据链里的 Windows 运行时缺口：一旦 `windows_summary` 存在，`quick smoke` 日志与 `broader runtime suite transcript` 也必须同时存在并被一致性校验纳入 strict 失败条件。

## Current Batch
1. 写 focused RED contract，证明当前 `check_wave_b_b2_evidence_consistency.sh --strict` 在只有 `windows_summary`、缺失 `winssl_quick_smoke` / `winssl_runtime_suite` 时仍不会失败。
2. 对一致性脚本做最小修法，显式识别并校验 Windows quick smoke 与 runtime transcript。
3. 把 `.github/workflows/wave-b-b2-manual.yml` 与 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 的 Windows 参数链补齐为显式传递，不再只传 `--windows-summary`。
4. 跑 focused GREEN、现有 workflow/bundle contracts、diff hygiene，更新 working-memory，并在 review 后提交。

## Status
- [completed] refreshed current repo and memory context for the next Windows runtime evidence batch
- [completed] focused RED contract authoring
- [completed] minimal evidence-consistency / workflow wiring fix
- [completed] focused verification
- [in_progress] review and commit closeout

## Current Evidence
- 当前 `windows-gate` job 已明确产出：
  - `test-reports/winssl_quick_smoke_<run_id>.log`
  - `test-reports/wave_b_windows_gate_summary_<run_id>.md`
  - `test-reports/winssl_runtime_suite_<run_id>.log`
- 但 summary 阶段当前只把 `--windows-summary` 组装进 `WINDOWS_ARGS`，再传给：
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
- 当前 `scripts/check_wave_b_b2_evidence_consistency.sh` 只认识 `windows_summary` 这个 Windows 侧 markdown 证据，对 quick smoke / runtime transcript 没有任何存在性校验。
- focused RED:
  - `bash -n tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: FAIL before fix
  - failure shape: strict consistency still returned success even though the fixture omitted `winssl_quick_smoke` / `winssl_runtime_suite`
- minimal implementation:
  - new focused contract: `tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
    - now accepts `--windows-quick-log` / `--windows-runtime-transcript`
    - defaults both paths by `run_id`
    - treats them as required evidence whenever `windows_summary` exists
    - surfaces both artifacts in the report matrix
  - `.github/workflows/wave-b-b2-manual.yml`
    - summary stage now splits `WINDOWS_SUMMARY_ARGS` and `WINDOWS_EVIDENCE_ARGS`
    - evidence consistency call now receives the explicit Windows runtime artifact args
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
    - kept textually aligned with the live workflow
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - now forwards the same explicit Windows runtime artifact args into evidence consistency
- focused GREEN:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`: PASS
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`: PASS
  - `bash -n tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`: PASS
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`: PASS
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`: PASS
  - focused handoff smoke: `scripts/prepare_wave_b_b2_handoff_bundle.sh` now emits `consistency_status: INCONSISTENT` plus missing `windows_quick_log` / `windows_runtime_transcript` rows when those artifacts are absent
  - `.github/workflows/wave-b-b2-manual.yml.disabled` vs live workflow: no diff
  - `git diff --check`: PASS

# Task Plan - Internal Context ServerName Warning Quarantine

## Goal
收口内部兼容路径上的 `context-level ServerName` 弃用 warning，避免 focused 编译持续刷出已知兼容噪音。

## Current Batch
1. 写 focused compile contract，锁住 `factory` / `builder` / `OpenSSL` 兼容路径当前仍会发出 deprecated `ServerName` warning。
2. 在实际兼容调用点补局部 warning quarantine，不改变 runtime 语义。
3. 跑 focused GREEN、compile gate 与 diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] synced current repo state and reran high-value FreePascal runtime suites
- [completed] focused RED compile evidence captured from `tests/test_builder_integration.pas`
- [completed] local warning quarantine edits and focused verification
- [completed] compile gate and diff hygiene

## Current Evidence
- runtime resync before the warning batch:
  - `tests/test_freepascal_client_cert_verify_flags_runtime.pas`: PASS
  - `tests/test_freepascal_client_session_resumption.pas`: PASS
  - `tests/test_freepascal_client_certificateverify_runtime.pas`: PASS
- focused RED:
  - `mkdir -p tmp/builder_warning_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/builder_warning_contract -FEtmp/builder_warning_contract -otmp/builder_warning_contract/test_builder_integration tests/test_builder_integration.pas`
  - result before fix: emitted deprecated `ISSLContext.Get/SetServerName` warnings from:
    - `src/fafafa.ssl.factory.pas`
    - `src/fafafa.ssl.context.builder.pas`
    - `src/fafafa.ssl.openssl.connection.pas`
    - `src/fafafa.ssl.openssl.backed.pas`
- minimal implementation:
  - new focused compile contract: `tests/scripts/test_internal_context_servername_warning_contract.sh`
  - local warning quarantine added only around intentional compatibility calls in the four files above
- focused GREEN:
  - `bash -n tests/scripts/test_internal_context_servername_warning_contract.sh`: PASS
  - `bash tests/scripts/test_internal_context_servername_warning_contract.sh`: PASS
  - `./tmp/internal_context_servername_warning_contract/test_builder_integration`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - WinSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TWinSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在未握手前暴露误导性 verify-status 诊断的公共语义漂移。

## Current Batch
1. 写 focused RED source contract，锁住 WinSSL pre-handshake getter 需要显式返回 `-1 / Not verified`。
2. 在 `src/fafafa.ssl.winssl.connection.pas` 做最小 getter 修法，只收口 `sslHsNotStarted` / `sslHsInProgress`。
3. 跑 focused GREEN、Win64 compile proof 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the WinSSL pre-handshake verify-status batch
- [completed] focused RED source contract authoring
- [completed] minimal WinSSL pre-handshake verify-status fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `bash -n tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`
  - `bash tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`
  - result before fix: FAIL on missing explicit pre-handshake `-1` guard in `DoGetVerifyResult`
- minimal implementation:
  - `tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`
    - added focused source contract locking WinSSL pre-handshake getter semantics
  - `src/fafafa.ssl.winssl.connection.pas`
    - `DoGetVerifyResult` now short-circuits to `-1` for `sslHsNotStarted` / `sslHsInProgress`
    - `DoGetVerifyResultString` now returns `Not verified` for the same pre-handshake states
    - `sslHsFailed` / `sslHsCompleted` still reuse the existing role-resolved validation path
- focused GREEN:
  - `bash tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_preverify_host_win64 -FEtmp/winssl_preverify_host_win64 -otmp/winssl_preverify_host_win64/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_preverify_revocation_win64 -FEtmp/winssl_preverify_revocation_win64 -otmp/winssl_preverify_revocation_win64/test_winssl_revocation_online.exe tests/winssl/test_winssl_revocation_online.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - MbedTLS Pre-Handshake Verify Status Clarification

## Goal
修复 `TMbedTLSConnection.GetVerifyResult` / `GetVerifyResultString` 在未握手前误报 verify success 的公共语义漂移。

## Current Batch
1. 写 focused RED，证明 fresh MbedTLS connection 仍会在 pre-handshake 路径返回 `0/OK`。
2. 在 `src/fafafa.ssl.mbedtls.connection.pas` 做最小 getter 修法，明确 pre-handshake 为 `-1 / Not verified`。
3. 跑 focused GREEN 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the MbedTLS pre-handshake verify-status batch
- [completed] focused RED contract authoring
- [completed] minimal MbedTLS pre-handshake verify-status fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
  - `./tmp/mbedtls_framework_units/test_mbedtls_framework`
  - result before fix: FAIL on:
    - `Fresh MbedTLS connection does not report verify success before handshake`
    - `Fresh MbedTLS connection reports not-verified diagnostic before handshake`
- minimal implementation:
  - `tests/test_mbedtls_framework.pas`
    - added `TestMbedTLSVerifyStatusBeforeHandshakeContract`
    - added `TTestMbedTLSConnection.MarkHandshakeCompleteForTest` so the older helper-loss contract still exercises a completed-handshake helper-loss path after the new pre-handshake guard
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - `DoGetVerifyResult` now exits with `-1` before handshake completion
    - `DoGetVerifyResultString` now returns `Not verified` before handshake completion while preserving post-handshake unavailable fallback
- focused GREEN:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`: PASS
  - `./tmp/mbedtls_framework_units/test_mbedtls_framework`: PASS, `100 passed / 0 failed`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - WolfSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TWolfSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在未握手前误报 verify success 的公共语义漂移。

## Current Batch
1. 写 focused RED，证明 fresh WolfSSL connection 仍会在 pre-handshake 路径返回 `0/OK`。
2. 在 `src/fafafa.ssl.wolfssl.connection.pas` 做最小 getter 修法，明确 pre-handshake 为 `-1 / Not verified`。
3. 跑 focused GREEN 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the WolfSSL pre-handshake verify-status batch
- [completed] focused RED contract authoring
- [completed] minimal WolfSSL pre-handshake verify-status fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/wolfssl_framework_units -FEtmp/wolfssl_framework_units -otmp/wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
  - `./tmp/wolfssl_framework_units/test_wolfssl_framework`
  - result before fix: FAIL on:
    - `Fresh WolfSSL connection does not report verify success before handshake`
    - `Fresh WolfSSL connection reports not-verified diagnostic before handshake`
- minimal implementation:
  - `tests/test_wolfssl_framework.pas`
    - added `TestWolfSSLVerifyStatusBeforeHandshakeContract`
    - uses a real WolfSSL client context + stream connection
    - asserts pre-handshake `GetVerifyResult = -1` and `GetVerifyResultString` contains `not verified`
  - `src/fafafa.ssl.wolfssl.connection.pas`
    - `DoGetVerifyResult` now returns `-1` before handshake completion when no native verify error exists
    - `DoGetVerifyResultString` now returns `Not verified` before handshake completion
- focused GREEN:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/wolfssl_framework_units -FEtmp/wolfssl_framework_units -otmp/wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`: PASS
  - `./tmp/wolfssl_framework_units/test_wolfssl_framework`: PASS, `112 passed / 0 failed`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - OpenSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TOpenSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在未完成握手时的 verify-status 假阳性。

## Current Batch
1. 写 focused RED，证明 fresh OpenSSL stream connection 仍会在 pre-handshake 路径返回 `0/OK`。
2. 在 `src/fafafa.ssl.openssl.connection.pas` 做最小 getter 修法，明确 pre-handshake 为 `Not verified`。
3. 跑 focused GREEN 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the OpenSSL pre-handshake verify-status batch
- [completed] focused RED contract authoring
- [completed] minimal OpenSSL pre-handshake verify-status fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `fpc -B -Fu./src -Fu./tests -otmp/test_openssl_connection_verify_result_contract tests/test_openssl_connection_verify_result_contract.pas`
  - `./tmp/test_openssl_connection_verify_result_contract`
  - result before fix: FAIL on:
    - `Fresh OpenSSL connection should not report verify success before handshake`
    - `Fresh OpenSSL connection should surface not-verified diagnostic before handshake`
- minimal implementation:
  - `tests/test_openssl_connection_verify_result_contract.pas`
    - added fresh pre-handshake verify-result contract
    - preserved existing helper-loss guard contract in the same focused unit
  - `src/fafafa.ssl.openssl.connection.pas`
    - `DoGetVerifyResult` now short-circuits to `-1` before handshake completion
    - `DoGetVerifyResultString` now returns `Not verified` before handshake completion
- focused GREEN:
  - `fpc -B -Fu./src -Fu./tests -otmp/test_openssl_connection_verify_result_contract tests/test_openssl_connection_verify_result_contract.pas`: PASS
  - `./tmp/test_openssl_connection_verify_result_contract`: PASS, `4 passed / 0 failed`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - FreePascal Verify Result Status Clarification

## Goal
修复 `TFreePascalConnection.GetVerifyResult` / `GetVerifyResultString` 在 pre-handshake 与 successful-verified 路径上的公共语义漂移。

## Current Batch
1. 写 focused RED，证明 fresh connection 仍会误报 verify success，且成功握手后仍返回 `Not verified`。
2. 在 `src/fafafa.ssl.freepascal.connection.pas` 做最小 getter 修法，区分 `Not verified` 与 `OK`。
3. 跑 focused GREEN 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the FreePascal verify-result status batch
- [completed] focused RED contract authoring
- [completed] minimal FreePascal verify-result status fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `fpc -B -Fu./src -Fu./tests -otmp/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas`
  - `./tmp/test_freepascal_client_chain_trust_runtime`
  - result before fix: FAIL on `Fresh connection must not report verify success before handshake (expected=-1 actual=0)`
- minimal implementation:
  - `tests/test_freepascal_client_chain_trust_runtime.pas`
    - added fresh pre-handshake verify-result contract
    - added successful CA-trusted handshake verify-string contract
  - `src/fafafa.ssl.freepascal.connection.pas`
    - `DoGetVerifyResult` now returns `-1` when no handshake has completed and no verify error exists
    - `DoGetVerifyResultString` now returns `Not verified` before handshake and `OK` after a successful handshake with no verify error
- focused GREEN:
  - `fpc -B -Fu./src -Fu./tests -otmp/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas`: PASS
  - `./tmp/test_freepascal_client_chain_trust_runtime`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - MbedTLS Verify Result Helper Guard

## Goal
修复 `TMbedTLSConnection.GetVerifyResult` 在 helper 缺失时误报 `0/OK` 的 public contract drift。

## Current Batch
1. 写 focused RED，证明 MbedTLS verify-result helper 缺失时仍错误返回 success。
2. 在 `src/fafafa.ssl.mbedtls.connection.pas` 做最小 guard 修法，并给 string getter 补稳定 unavailable 诊断。
3. 跑 focused GREEN 与 compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the MbedTLS verify-result guard batch
- [completed] focused RED contract authoring
- [completed] minimal MbedTLS verify-result guard fix
- [completed] focused verification and compile review

## Current Evidence
- focused RED:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
  - `./tmp/mbedtls_framework_units/test_mbedtls_framework`
  - result before fix: runtime FAIL on:
    - `VerifyResult helper loss degrades to -1`
    - `VerifyResultString helper loss exposes unavailable diagnostic`
- minimal implementation:
  - `tests/test_mbedtls_framework.pas`
    - added `TestMbedTLSVerifyResultHelperLossContract`
    - temporarily clears `mbedtls_ssl_get_verify_result`
    - asserts `GetVerifyResult = -1` and unavailable-style string degradation
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - `DoGetVerifyResult` now defaults to `-1` and exits early on nil helper/context
    - `DoGetVerifyResultString` now defaults to `Verification status unavailable` and exits early on nil helper/context
- focused GREEN:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`: PASS
  - `./tmp/mbedtls_framework_units/test_mbedtls_framework`: PASS, `98 passed / 0 failed`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - WinSSL sslCtxBoth Verification Role Clarification

## Goal
修复 `sslCtxBoth` 在 `WinSSL` 显式 `Connect` / `Accept` 之后仍按 `ContextType` 猜证书校验角色的 public drift。

## Current Batch
1. 写 focused RED source contract，锁住 WinSSL dual-context verification path 仍按 `ContextType` 推导 role。
2. 在 `src/fafafa.ssl.winssl.connection.pas` 做最小修法，把 verify role 改成显式来源，并让 verify-result getter 复用同一真相源。
3. 跑 focused GREEN、Win64 compile proof、Linux compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the WinSSL verification-role batch
- [completed] focused RED contract added and observed
- [completed] minimal WinSSL verification-role fix implemented
- [completed] focused verification and compile proof

## Current Evidence
- focused RED:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`
  - result before fix: FAIL on missing explicit peer-validation role state, explicit role recorder/resolver, and role-parameterized `ValidatePeerCertificate(...)`
- minimal implementation:
  - `src/fafafa.ssl.winssl.connection.pas`
    - added connection-local peer-validation role state and resolver
    - `ValidatePeerCertificate(...)` now accepts explicit `AIsClient`
    - `DoConnect` / `DoAccept` / verify-result getter now use explicit verification role truth instead of `ContextType`
    - fixed a Pascal `if ... then ... else` semicolon slip caught by Win64 cross-compile while landing the change
- focused GREEN:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_client_win64 -FEtmp/winssl_role_client_win64 -otmp/winssl_role_client_win64/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_server_win64 -FEtmp/winssl_role_server_win64 -otmp/winssl_role_server_win64/test_winssl_mtls_e2e_local.exe tests/winssl/test_winssl_mtls_e2e_local.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - sslCtxBoth Roleless Handshake Clarification

## Goal
修复 `sslCtxBoth` 在 role-less 握手入口上的公共合同漂移：`DoHandshake` 和 OpenSSL 未连接 stream `Read/Write` 当前会静默猜 client/server，缺少明确边界。

## Current Batch
1. 写 focused RED，证明 `sslCtxBoth` 走 `DoHandshake` 和 OpenSSL 隐式 stream handshake 时仍会偷偷猜角色。
2. 在连接基类与 OpenSSL stream implicit handshake 入口做最小 fail-fast 修法，不扩到更大的 dual-role state 设计。
3. 跑 focused GREEN 与相邻回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the sslCtxBoth roleless-handshake batch
- [completed] focused RED regression added and observed
- [completed] minimal handshake-boundary fix implemented
- [completed] focused verification and neighbor regression review

## Notes
- 这批不改变显式 `Connect` / `Accept` 的语义。
- 这批也不引入新的 “connection role” 持久状态；先把当前没有角色来源的公共入口收口成清晰 precondition。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_sslctxboth_roleless_handshake_clarification.pas -otmp/test_sslctxboth_roleless_handshake_clarification && ./tmp/test_sslctxboth_roleless_handshake_clarification`
  - result before fix: `17 passed / 7 failed`
  - failure shape:
    - FreePascal / OpenSSL / MbedTLS `sslCtxBoth` `DoHandshake` 没有给出 configuration boundary
    - WolfSSL `sslCtxBoth` `DoHandshake` 甚至直接回到 `sslHsInProgress`
    - OpenSSL dual-context stream `Read/Write` 的隐式握手没有记录明确 configuration error
- minimal implementation:
  - `src/fafafa.ssl.connection.base.pas`
    - added shared dual-role handshake ambiguity helpers
    - `DoHandshake` now fail-fast on `sslCtxBoth` with `sslErrConfiguration`
  - `src/fafafa.ssl.openssl.connection.pas`
    - stream `Read/Write` now reject disconnected `sslCtxBoth` implicit handshake paths
    - `DoHandshake` log label now reports `Dual` instead of misleading `Server`
- focused GREEN:
  - `tests/test_sslctxboth_roleless_handshake_clarification.pas`: PASS, `24 passed / 0 failed`
  - `tests/test_openssl_connection_stream_handshake_contract.pas`: PASS
  - `tests/test_sslctxboth_client_capability_clarification.pas`: PASS, `28 passed / 0 failed / 1 skipped`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - sslCtxBoth Client Capability Clarification

## Goal
修复 `sslCtxBoth` 在连接级 client capability 上的公共合同漂移：当前多个后端的 connection 构造和 early-data role gate 仍把它当成“不是 client”，与 `sslCtxBoth` 的公开语义不一致。

## Current Batch
1. 写 focused RED，锁定 `sslCtxBoth` 作为客户端使用时的两类失真：
   - context fallback `ServerName` 没有继承到连接
   - supporting backends 的 `SetEarlyData(...)` 先被错误 role gate 拒绝
2. 在相关 connection units 做最小 capability-based 修法，不扩到更大的 dual-role handshake 设计。
3. 跑 focused GREEN 与相邻 server-name / early-data 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the sslCtxBoth client-capability batch
- [completed] focused RED regression added and observed
- [completed] minimal connection capability fix implemented
- [completed] focused verification and neighbor regression review

## Notes
- 这批只收“client-capable runtime truth”，不重新定义 `sslCtxBoth` 的完整 dual-role 握手状态机。
- 如果 fresh RED 证明 `DoHandshakeInternal` / implicit handshake 路径也存在真实 public drift，再单开下一批。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_sslctxboth_client_capability_clarification.pas -otmp/test_sslctxboth_client_capability_clarification && ./tmp/test_sslctxboth_client_capability_clarification`
  - result before fix: `21 passed / 7 failed / 1 skipped`
  - failure shape:
    - `sslCtxBoth` stream connections on FreePascal / OpenSSL / WolfSSL / MbedTLS lost context fallback `ServerName`
    - `sslCtxBoth` socket connections on FreePascal lost the same fallback
    - FreePascal / OpenSSL `SetEarlyData(...)` rejected `sslCtxBoth` with `Early data is only available on client connections`
- minimal implementation:
  - `src/fafafa.ssl.connection.base.pas`
    - added shared client/server capability helpers for connection units
  - `src/fafafa.ssl.freepascal.connection.pas`
    - dual-context connections now inherit client fallback `ServerName`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.openssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.wolfssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
    - client/server pre-handshake OCSP capability gates now accept `sslCtxBoth`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.winssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
- focused GREEN:
  - `tests/test_sslctxboth_client_capability_clarification.pas`: PASS, `28 passed / 0 failed / 1 skipped`
  - `tests/test_freepascal_context_server_name_inheritance.pas`: PASS
  - `tests/test_early_data_public_api_contract.pas`: PASS
  - `tests/test_factory_config_server_name_isolation.pas`: PASS
  - `tests/test_openssl_wolfssl_early_data_connection_contract.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - Early-Data Context Scope Clarification

## Goal
修复 early-data mixed-scope 下发漂移：`TSSLContextBuilder` / `TSSLConfig` 可以同时携带 client/server early-data 默认值，但 builder/factory/helper 当前会把 opposite-side 值也写进错误的 context。

## Current Batch
1. 写 focused RED，证明 builder / factory / public helper 会把 `ClientEarlyDataEnabled`、`ServerEarlyDataPolicy`、`ServerMaxEarlyDataSize` 泄漏到错误的 context type。
2. 在 `src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.factory.pas` 做最小 scope-aware application 修法，保持组合配置模型不变。
3. 跑 focused GREEN、相邻 early-data 回归，并修掉回归里暴露出来的默认持久化 replay-ledger 测试脆弱点。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the early-data context-scope batch
- [completed] RED regression added and observed
- [completed] minimal builder/factory/helper scope fix implemented
- [completed] adjacent replay-store test flake hardened
- [completed] focused verification and neighbor regression review

## Notes
- 这批不是把组合 `TSSLConfig` / builder 拆掉，而是把“组合配置”与“具体 context 下发”分开：
  - 组合对象仍可同时携带 client/server 默认值
  - 具体 `sslCtxClient` / `sslCtxServer` / `sslCtxBoth` 创建时只应用对应子集
- 这批不把 mixed-scope 改成 fail-fast，因为现有 builder / config round-trip / shared-default 用法本身就隐含“一个组合配置可供两侧复用”的设计。
- `TSSLHelper.ConfigureClientEarlyData(...)` / `ConfigureServerEarlyData(...)` 现在也跟随 context type 收口，wrong-scope context 返回 `False`。
- 相邻发现的测试脆弱点不是生产回归：
  - `tests/test_factory_config_early_data_isolation.pas` 对默认持久化 replay-ledger 复用了固定 session label
  - 多次重跑会被历史 residue 污染
  - 现已改成每次运行唯一 label

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_early_data_context_scope_clarification.pas -otmp/test_early_data_context_scope_clarification && ./tmp/test_early_data_context_scope_clarification`
  - result before fix: `30 passed / 14 failed`
  - failure shape:
    - `BuildClient` / factory client path 会观察到 server policy/max
    - `BuildServer` / factory server path 会观察到 client early-data flag
    - `TSSLHelper.ConfigureServerEarlyData(...)` 会错误接受 client context
    - `TSSLHelper.ConfigureClientEarlyData(...)` 会错误接受 server context
- minimal implementation:
  - `src/fafafa.ssl.context.builder.pas`
    - new scope-aware early-data application helper
    - `BuildClient` 仅下发 client early-data flag
    - `BuildServer` 仅下发 server policy/max
  - `src/fafafa.ssl.factory.pas`
    - `ApplyEarlyDataContextConfig(...)` now applies only the context-relevant subset
    - `TSSLHelper.ConfigureClientEarlyData(...)` / `ConfigureServerEarlyData(...)` now refuse wrong-scope contexts
  - `src/fafafa.ssl.debug.utils.pas`
    - config dump now labels client/server early-data scalar fields with their actual application scope
  - `tests/test_factory_config_early_data_isolation.pas`
    - one-shot server-context assertion updated to the new scope truth
    - default persistent replay-ledger probe labels now use per-run unique session ids
- focused GREEN:
  - `tests/test_early_data_context_scope_clarification.pas`: PASS, `44 passed / 0 failed`
  - `tests/test_factory_config_early_data_isolation.pas`: PASS, `60 passed / 0 failed`
  - `tests/config/test_context_builder_early_data_contract.pas`: PASS
  - `tests/test_early_data_public_api_contract.pas`: PASS
  - `tests/config/test_context_builder_try.pas`: PASS, `66 passed / 0 failed`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_early_data_context_scope_clarification.pas -otmp/test_early_data_context_scope_clarification && ./tmp/test_early_data_context_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_early_data_contract.pas -otmp/test_context_builder_early_data_contract && ./tmp/test_context_builder_early_data_contract`
4. `fpc -Fu./src -Fu./tests tests/test_early_data_public_api_contract.pas -otmp/test_early_data_public_api_contract && ./tmp/test_early_data_public_api_contract`
5. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
6. `git diff --check`
7. `git status --short`

# Task Plan - Client Replay-Store Scope Clarification

## Goal
修复 `server_early_data_replay_store_file` / `server_early_data_replay_store_directory` 在 client builder/factory 路径上的静默 no-op，让 server-only replay-store opt-in 变成明确的 scope contract。

## Current Batch
1. 写 focused RED，证明 `ValidateClient` / `TryBuildClient` / factory default-config client path / factory one-shot client path 都会静默接受 server replay-store 字段。
2. 在 `src/fafafa.ssl.context.builder.pas` 和 `src/fafafa.ssl.factory.pas` 做最小 fail-fast scope 修法，不改 server replay-store runtime 安装链。
3. 跑 focused GREEN、相邻 replay-store 回归、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the client replay-store scope batch
- [completed] RED regression added and observed
- [completed] minimal builder/factory scope fix implemented
- [completed] focused verification and adjacent regression review

## Notes
- 这批只收 `ServerEarlyDataReplayStoreFile` / `ServerEarlyDataReplayStoreDirectory` 这两个最明确的 server-only opt-in。
- 这批不扩到 `ClientEarlyDataEnabled` / `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` 的 broader scope truth；先收最小高价值误导点。
- shared default config 一旦携带 server replay-store opt-in，default-path client context 现在会 fail-fast；这比继续静默丢掉 replay-store 配置更符合 public truth。
- factory 的 replay-store 安装边界现在按 server-capable context 处理，`sslCtxServer` / `sslCtxBoth` 继续允许。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_early_data_replay_store_client_scope_clarification.pas -otmp/test_early_data_replay_store_client_scope_clarification && ./tmp/test_early_data_replay_store_client_scope_clarification`
  - result before fix: `0 passed / 14 failed`
  - failure shape: `ValidateClient` / `TryBuildClient` / factory default-path client / factory one-shot client 全都静默接受 server replay-store config
- minimal implementation:
  - `src/fafafa.ssl.context.builder.pas`
    - `ValidateClient` now reports server replay-store fields as invalid on client builders
    - `BuildClient` / `TryBuildClient` now fail fast on those fields
  - `src/fafafa.ssl.factory.pas`
    - client factory paths now reject server replay-store fields with `ESSLConfigurationException`
    - replay-store installer application now treats `sslCtxBoth` as server-capable
  - `src/fafafa.ssl.debug.utils.pas`
    - config dump now labels replay-store fields as server-scoped and notes client builder/factory contexts do not accept them
  - `tests/test_factory_config_early_data_isolation.pas`
    - old default-path client no-op expectation updated to the new fail-fast truth
- focused GREEN:
  - `tests/test_early_data_replay_store_client_scope_clarification.pas`: PASS, `14 passed / 0 failed`
  - `tests/test_factory_config_early_data_isolation.pas`: PASS, `60 passed / 0 failed`
  - `tests/config/test_context_builder_try.pas`: PASS, `66 passed / 0 failed`
  - `tests/config/test_config_validation.pas`: PASS, `53 passed / 0 failed`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_early_data_replay_store_client_scope_clarification.pas -otmp/test_early_data_replay_store_client_scope_clarification && ./tmp/test_early_data_replay_store_client_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
4. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
5. `git diff --check`
6. `git status --short`

# Task Plan - Factory ServerName Scope Clarification

## Goal
修复 `TSSLFactory.CreateContext(...)` 对 `TSSLConfig.ServerName` 的 server-side scope 漂移：当前 factory/config 路径会把 client-only `ServerName` 静默接受并写入 server context，但 server-side connections 明确忽略它。

## Current Batch
1. 写 focused RED 合同，证明 factory 的 one-shot config path 和 library-default path 都会接受 server-context `ServerName`。
2. 在 `src/fafafa.ssl.factory.pas` 做最小 scope 校验，让 factory 在没有 warning surface 的情况下 fail-fast。
3. 跑 focused verification、相邻回归、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the factory ServerName scope batch
- [completed] RED regression added and observed
- [completed] minimal factory scope fix implemented
- [completed] focused verification and review

## Notes
- 这批不改 builder：`WithSNI` 在 server path 上继续保留兼容 + warning 语义。
- 这批只收口 factory/config，因为它没有 builder 那样的 validation warning surface，继续静默接受会更误导。
- 优先修 `sslCtxServer` 这条已被现有验证文案明确定义为“连接会忽略”的路径。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_factory_server_name_scope_clarification.pas -otmp/test_factory_server_name_scope_clarification && ./tmp/test_factory_server_name_scope_clarification`
  - result before fix: `4 passed / 2 failed`
  - failure shape: client controls passed, while server default-config and one-shot config paths both silently accepted `ServerName` instead of rejecting it
- minimal implementation:
  - `src/fafafa.ssl.factory.pas`
    - `ValidateConnectionCreationScope(...)` now receives the effective context type
    - server-context `ServerName` now raises `ESSLConfigurationException` on factory/config creation paths
  - `src/fafafa.ssl.debug.utils.pas`
    - `DumpSSLConfig(...)` now labels `ServerName` as client-scoped and notes server factory contexts do not accept it
- focused GREEN:
  - `tests/test_factory_server_name_scope_clarification.pas`: PASS, `6 passed / 0 failed`
  - `tests/test_factory_config_server_name_isolation.pas`: PASS
  - `tests/config/test_config_validation.pas`: PASS, including the existing builder server-side warning contract

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_factory_server_name_scope_clarification.pas -otmp/test_factory_server_name_scope_clarification && ./tmp/test_factory_server_name_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_server_name_isolation.pas -otmp/test_factory_config_server_name_isolation && ./tmp/test_factory_config_server_name_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
4. `git diff --check`
5. `git status --short`

# Task Plan - Builder Server Smoke Truth

## Goal
修复 `tests/test_builder_integration.pas` 的 server-context smoke 误导：当前脚本把一个缺少证书的 `BuildServer` 调用当成集成 smoke，导致输出看起来像 runtime 失败；应改成带临时自签名证书的真实成功路径。

## Current Batch
1. 复核 docs / builder validation / existing config tests，确认 `BuildServer` 缺证书失败是既有正确语义，不是 runtime regression。
2. 最小修正 builder integration smoke，让 server-context case 带测试证书与私钥。
3. 跑 focused verification、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the builder server smoke batch
- [completed] server-context semantics revalidated against docs and existing builder contracts
- [completed] misleading integration smoke updated to a real success path
- [completed] focused verification and review

## Notes
- 这批只修测试真值，不改 `BuildServer` 运行时语义。
- `Server context requires a certificate` 仍然是正确的 runtime/validation 结论。
- 真实要修的是：集成 smoke 不该把一个注定失败的 server build 当成“成功路径”示例。

## Current Evidence
- semantics revalidated before the fix:
  - `src/fafafa.ssl.context.builder.pas` 明确在 `BuildServer` 和 `ValidateServer` 上要求证书
  - `docs/BACKEND_SELECTION_GUIDE.md` 的服务器示例也始终带 `WithCertificate(...)` / `WithPrivateKey(...)`
  - `tests/config/test_context_builder_try.pas` 已覆盖“无证书失败、有证书成功”的 builder contract
- minimal test-truth fix:
  - `tests/test_builder_integration.pas` server-context smoke now generates a temporary self-signed certificate and private key via `TCertificateUtils.TryGenerateSelfSignedSimple(...)`
  - the smoke then uses `WithCertificatePEM(...)` + `WithPrivateKeyPEM(...)` before `BuildServer`
- focused verification:
  - `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
  - result: all eight smoke cases now report success

# Task Plan - Security-First Selector Viability

## Goal
修复 `CreateSecurityFirstRequirements` 与当前能力矩阵/安全评分真值的脱节，避免在本机已有 OpenSSL 可用且硬性协议/算法条件满足时，security-first 需求模板仍因过高门槛而选不出任何后端。

## Current Batch
1. 写 focused RED 合同，证明当存在满足 security-first 硬性协议/算法要求的可用后端时，`CreateSecurityFirstRequirements` 的最低安全分门槛不能高于这些后端可达到的真实安全分。
2. 在 `src/fafafa.ssl.backend.selector.pas` 做最小修法，让 security-first 默认模板重新可用。
3. 跑 focused verification、相邻 smoke、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the security-first viability batch
- [completed] RED regression added and observed
- [completed] minimal requirement-template fix implemented
- [completed] focused verification and review

## Notes
- 这批优先修需求模板真值，不重算全局 `GetSecurityScore(...)` 权重。
- 修法必须和当前 capability truth 对齐，而不是靠放宽协议/算法硬要求掩盖问题。
- 如果 fresh evidence 证明现有最高可达安全分是 `80`，那 security-first 默认门槛就不能继续写成 `85`。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_backend_selector_security_first_viability.pas -otmp/test_backend_selector_security_first_viability && ./tmp/test_backend_selector_security_first_viability`
  - result before fix: `1 passed / 2 failed`
  - failure shape: at least one available backend satisfied the hard security-first protocol/algorithm requirements, but `CreateSecurityFirstRequirements.MinSecurityScore` still exceeded the best eligible backend security score, so selection returned none
- minimal implementation:
  - `src/fafafa.ssl.backend.selector.pas`: `CreateSecurityFirstRequirements.MinSecurityScore` tightened from `85` to `80`
  - `docs/BACKEND_SELECTION_GUIDE.md`: threshold examples updated to `80`
- focused GREEN:
  - `tests/test_backend_selector_security_first_viability.pas`: PASS, `3 passed / 0 failed`
  - `tests/test_backend_selector_basic.pas`: security-first smoke now succeeds and reports `最低安全评分要求: 80`
  - `tests/test_builder_integration.pas`: `WithSecurityFirst` path now creates a client context successfully
- residual neighbor signal:
  - `tests/test_builder_integration.pas` still prints `Server context requires a certificate` for the performance-first server-context smoke; this batch did not widen into server-certificate provisioning semantics

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_security_first_viability.pas -otmp/test_backend_selector_security_first_viability && ./tmp/test_backend_selector_security_first_viability`
2. `fpc -Fu./src -Fu./tests tests/test_backend_selector_basic.pas -otmp/test_backend_selector_basic && ./tmp/test_backend_selector_basic`
3. `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
4. `git diff --check`
5. `git status --short`

# Task Plan - Backend Selector Required-Feature Truth

## Goal
修复 backend selector 对 `RequiredFeatures` 的漏判/错判，让 `sslFeatSessionCache`、`sslFeatSessionTickets`、`sslFeatRenegotiation`、`sslFeatOCSPStapling`、`sslFeatCertificateTransparency` 等必需功能真正参与筛选，并以 capability support-level 作为真相源。

## Current Batch
1. 写 focused RED 合同，证明 `RequiredFeatures = [sslFeatRenegotiation]` 时 selector 仍会接受不满足的候选，且 `RequiredFeaturesTotal` 统计错误。
2. 在 `src/fafafa.ssl.backend.selector.pas` 增加最小 feature helper，统一从 support-level 判断功能是否存在。
3. 跑 focused verification、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the selector batch
- [completed] RED regression added, corrected to minimal requirement truth, and observed
- [completed] minimal selector fix implemented
- [completed] focused verification and review

## Notes
- 这批不改 capability producer，不改 serializer/diff 文档，只修 public selector 行为。
- `RequiredFeatures` 的 requirement 语义按“功能存在即可”处理：`stable / experimental / deprecated` 都算满足，只有 `none` 不满足。
- 旧布尔字段保留兼容，但 selector 这条链不再把它们当主真相。
- `TSSLBackendMatchDetails.RequiredFeaturesTotal/Matched` 当前实际统计的是所有 required 维度总数，不只是 `RequiredFeatures` 子集；focused 合同必须用最小 requirement 基线隔离 feature 参与度。

## Current Evidence
- first RED attempt exposed a test-model drift, not a production regression:
  - `CreateDefaultRequirements(optBalanced)` 自带 `TLS12/TLS13` 和最低评分门槛，掩盖了单 feature requirement 的真值
  - `RequiredFeaturesTotal/Matched` 命名偏窄，但实现会把协议/算法/平台 requirement 一起计入
- corrected focused GREEN:
  - `fpc -Fu./src -Fu./tests tests/test_backend_selector_required_feature_truth.pas -otmp/test_backend_selector_required_feature_truth && ./tmp/test_backend_selector_required_feature_truth`
  - result: PASS, `8 passed / 0 failed`
- neighbor regressions:
  - `tests/test_backend_selector_minimum_score_filtering.pas`: PASS
  - `tests/test_backend_selector_basic.pas`: compile+smoke PASS; historical informational output still says security-first selection failed on this host, but the harness has no failing assertion and exits `0`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_required_feature_truth.pas -otmp/test_backend_selector_required_feature_truth && ./tmp/test_backend_selector_required_feature_truth`
2. `git diff --check`
3. `git status --short`

# Task Plan - Factory Connection-Scope Clarification

## Goal
修复 `TSSLConfig.BufferSize` / `TSSLConfig.HandshakeTimeout` 在 factory/context 创建路径上的静默吞配置问题，让 `TSSLFactory.CreateContext(...)` 不再接受看起来可配、实际不生效的 connection-scoped 字段。

## Current Batch
1. 写 focused RED 合同，证明 one-shot request path 和 library-default path 都会静默接受自定义 `BufferSize` / `HandshakeTimeout`。
2. 在 `src/fafafa.ssl.factory.pas` 做最小 fail-fast 作用域校验，不扩 runtime surface。
3. 跑 focused test + diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the config-scope batch
- [completed] RED regression added and observed
- [completed] minimal factory scope validation implemented
- [completed] focused verification and review

## Notes
- 这批不去“补实现” `BufferSize` / `HandshakeTimeout`，因为当前 context/runtime surface 没有一致的消费路径。
- 修法以 fail-fast 为主，保持现有连接/后端行为不变。
- `HandshakeTimeout` 的真实替代路径是 `TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)` / `ISSLConnection.SetTimeout(...)`。
- `BufferSize` 不是 context factory 选项，应由 transport/IO 层自行管理。

## Current Evidence
- fresh RED:
  - `fpc -Fu./src -Fu./tests tests/test_factory_connection_scope_clarification.pas -otmp/test_factory_connection_scope_clarification && ./tmp/test_factory_connection_scope_clarification`
  - result before fix: 4 FAIL, all on "should raise ESSLConfigurationException"
- minimal implementation:
  - `src/fafafa.ssl.factory.pas` now rejects custom `HandshakeTimeout` / `BufferSize` on both one-shot request path and library-default path
  - `src/fafafa.ssl.debug.utils.pas` now labels both fields as non-context runtime settings in config dumps
- fresh GREEN:
  - `tests/test_factory_connection_scope_clarification.pas`: PASS, `12 passed / 0 failed`
  - `tests/test_factory_logging_scope_clarification.pas`: PASS
  - `tests/config/test_default_config.pas`: PASS
  - `git diff --check`: PASS

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_factory_connection_scope_clarification.pas -otmp/test_factory_connection_scope_clarification && ./tmp/test_factory_connection_scope_clarification`
2. `git diff --check`
3. `git status --short`

# Task Plan - Interface Design Audit

## Goal
对 `fafafa.ssl` 的公开 Pascal 接口做一次完整静态审查，找出设计上有问题、边界不清或与文档不一致的地方，并形成可追溯的审查结论。

## Current Batch
1. 静态核对 `src/fafafa.ssl.base.pas`、连接基类、各后端连接类、factory、context builder 和 facade。
2. 对比 `docs/ARCHITECTURE.md`、`docs/reference/INTERFACE_DESIGN_V2.md` 与源码真实 public surface。
3. 生成 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`。
4. 保留既有工作记忆历史，只追加本次审查摘要。

## Status
- [completed] interface discovery and evidence collection
- [completed] audit report written
- [completed] diff hygiene checked with `git diff --check`
- [pending] final delivery to user

## Notes
- 本批次是静态设计审查，不改实现、不跑编译门禁。
- 主要结论：core `ISSLConnection` 过胖，context-level SNI 已弃用但仍被高层入口写入，`TSSLConfig` 跨层混用且含 inert 字段，能力矩阵存在双真相，文档承诺了源码里不存在的 `ISSLServerConnection`。

## Verification Plan
1. `git diff --check`
2. `git status --short`
3. 手工复核报告引用的关键源码/文档行号

---

# Task Plan - v1.5.0 Linux Static Audit Closeout

## Goal
把 `v1.5.0` 的正式发布收口到 Linux-only 可合并状态：本地 gate 全绿、Pascal 公共接口与实现完整、release 文档与 readiness 对齐，并在 review 后完整合回 `main`。

## Current Batch
1. 复跑 Linux release gates，确认当前仓库仍然全绿。
2. 做 Pascal 静态审查，锁住 public facade、factory API、placeholder scan 和 WinSSL 骨架测试的边界。
3. 更新 release notes、readiness report 和静态审查报告，使文档真实反映 Linux-only closeout。
4. 复核 diff hygiene，提交后将已验证分支合回 `main`。

## Status
- [completed] Linux gates green on the current branch
- [completed] static Pascal audit and docs alignment
- [completed] review, commit, and merge back to `main`

## Notes
- 这批不再把 GitHub Actions 额度不足当成阻塞项；Windows runtime proof 明确转为后续独立批次。
- `TSSLHelper` 仍然是公开辅助类；移除的是旧全局 helper 别名/函数，不是 helper 类本身。
- `src/fafafa.ssl*.pas` 里不应再有 `TODO` / `FIXME` / `skeleton` / `placeholder` 这类未完成信号。

## Verification Plan
1. `python3 scripts/compile_all_modules.py`
2. `bash scripts/run_minimal_ci_gate.sh --fast-local`
3. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
4. `python3 scripts/check_code_style.py src`
5. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
6. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
7. `bash tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh`
8. `git diff --check`
9. merge the verified branch back into `main`

## Definition Of Done
- Linux gates are green
- static Pascal audit is green
- release notes and readiness report match the Linux-only closeout policy
- branch is merged back into `main`

# Task Plan - v1.5.0 Release Formalization

## Goal
把 `v1.5.0` 的正式发布准备收口到可审查、可复跑、可签字的状态：先确认版本真相和本地发布门禁，再把 release workflow / release notes / README / changelog 对齐，补一份 release workflow 契约和最终 readiness 报告，最后只在用户明确批准后再打 tag。

## Current Batch
1. 复核 baseline 真相与本地 release 门禁。
2. 落地 release workflow、release notes、版本文档对齐。
3. 触发并收集 Wave B/B2 Windows runtime 证据。
4. 生成 release readiness 报告，review 后提交。

## Status
- [completed] baseline truth / local release gates
- [completed] release workflow + notes + docs alignment
- [completed] readiness report drafted and updated with remote blocker evidence
- [completed] local release-prep batch committed as `8491b91`
- [completed] pushed `glm51` and dispatched Wave B/B2
- [blocked] Windows runtime proof refresh is blocked by GitHub Actions billing/spending-limit settings
- [pending] rerun Wave B/B2 after billing access is restored, then collect artifacts

## Notes
- 版本真相已经在 `src/fafafa.ssl.base.pas`：`FAFAFA_SSL_VERSION_STRING = '1.5.0'`，`FAFAFA_SSL_INTERFACE_VERSION = 10500`
- `CHANGELOG.md`、`README.md`、`fafafa_ssl.lpk`、`RELEASE_NOTES_V1.5.0.md` 已对齐到 `v1.5.0`
- `.github/workflows/release.yml` 已启用，`.github/workflows/release.yml.disabled` 已同步成同一份当前模板
- `python3 scripts/check_code_style.py src` 首轮打出 369 个缩进错误；已按 checker 实际报错做 44 个文件 / 369 行机械缩进修复，复跑通过
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已生成，并已记录 GitHub run `25698425400` 的外部 billing blocker
- `glm51` 已推送；刷新 Windows 证据链需要先恢复 GitHub Actions billing/spending-limit access，或使用等价可信 Windows 主机执行同一验证链

## Verification Plan
1. `git status --short`
2. `git clean -nd`
3. `git clean -ndX`
4. `python3 scripts/compile_all_modules.py`
5. `bash scripts/run_minimal_ci_gate.sh --fast-local`
6. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
7. `python3 scripts/check_code_style.py src`
8. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
9. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
10. `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
11. `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
12. `bash tests/scripts/test_active_roadmap_references_contract.sh`
13. `gh workflow run .github/workflows/wave-b-b2-manual.yml --ref glm51 ...` and collect artifacts if Windows proof is still missing

## Definition Of Done
- release workflow contract passes
- docs and package version align to `v1.5.0`
- local gates are green
- Wave B/B2 Windows evidence is captured or a concrete external blocker is recorded
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` exists and says whether the batch is ready for tag approval
- batch is committed after review

---

# Task Plan - Repo Hygiene And Ignore Consolidation

## Goal
收口仓库里的 build/output 噪音，补齐 nested `tests/**/test_*` 可执行文件的 ignore 规则，并安全清理已知输出目录，让工作树保持可复现、可审查。

## Current Batch
1. 收紧根目录专属 ignore 规则，避免把归档文档里的同名文件误判成工作记忆。
2. 移除示例目录里仓库不该自带的生成型 PEM 成品。
3. 修复数字签名示例契约对 `tmp/` 已存在的隐式假设。
4. 复核状态和 diff hygiene。
5. 提交仓库整理批次。

## Status
- [completed] Inventory ignored/untracked noise and size the safe cleanup scope
- [completed] Expand ignore coverage and clean safe generated outputs
- [completed] Update working-memory records for the new hygiene batch
- [completed] Root-anchor repo-local ignore entries and drop sample key artifacts
- [completed] Make digital-signature contract create its ignored tmp parent
- [completed] Verify diff hygiene and commit

## Current Evidence
- `git clean -ndX` showed the repository had a lot of ignored build output, including:
  - `bin/` around `728M`
  - `tests/bin/` around `131M`
  - `tests/lib/` around `5.2M`
  - `examples/bin/` around `107M`
  - `artifacts/` around `1.7M`
  - `tmp/` around `6.0G`
  - `tools/test_audit/bin/` around `2.0M`
- `tests/**/test_*` was not covered by the existing top-level test-binary ignore rule, so nested generated executables could still surface as untracked files.
- The cleanup sweep removed generated output directories. The first broad pass also swept local ignored agent/config folders and `archive/`, so this batch now makes local agent/cache ignores explicit.
- After the `.gitignore` update, `git check-ignore -v` confirms nested `tests/**/test_*` executables are ignored, test sources remain visible, and benchmark report markdown stays ignored.
- `git clean -nd` only reports this new plan doc; `git clean -ndX` only reports `.agents/` and `.codex/` as ignored local caches.
- The follow-up sweep found two more repo-hygiene nits:
  - rootless `task_plan.md` / `findings.md` / `progress.md` / `WARP.md` ignore patterns can accidentally match archive docs such as `docs/archive/old_reports/PROGRESS.md`
  - `examples/digital_signature/private.pem` and `public.pem` are generated outputs that the README already instructs users to create locally
- `git check-ignore -v --no-index` now confirms the root-local working-memory files are matched only at the repo root, while `docs/archive/old_reports/PROGRESS.md` is no longer caught by those patterns.
- `tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh` now creates `tmp/` itself, so it still works after a clean artifact sweep removes the ignored directory.
- After the tmp parent fix and follow-up cleanup, `git clean -nd` and `git clean -ndX` are back to empty.

## Risks
- Do not delete local agent/config folders or archived notes.
- Do not broaden the cleanup into tracked source trees.
- Preserve the test source files and docs under `tests/**`.
- Keep the ignore rules root-scoped for repo-local files only.

## Follow-up Queue
1. Organization batch committed.

# Task Plan - Working-Memory, Artifact Hygiene, And WinSSL Workflow Closeout

## Goal
把当前工作树从“历史批次 + 本地产物残留”收口回 `HEAD` `e80100a` 的真实状态，清掉 3 个测试二进制残留，并把 `wave-b-b2-manual.yml` 的 Windows lane 对齐到 WinSSL runtime checklist。

## Current Batch
1. 清理 `tests/contract/` 与 `tests/wolfssl/` 下的无扩展名 ELF 测试产物。
2. 新增本批 plan 文档，作为可恢复的工作记忆。
3. 把 `task_plan.md` / `findings.md` / `progress.md` 顶部对齐到当前真相。
4. 修复 `test_wave_b_b2_windows_runtime_workflow_contract.sh` 打出的 workflow RED。
5. 复跑 focused contracts、diff hygiene，并提交。

## Status
- [completed] Freeze current state and remove generated test binaries
- [completed] Resync working-memory files to current HEAD and next queue
- [completed] Align Wave B/B2 Windows workflow to the runtime checklist
- [completed] Verify diff hygiene and record results
- [pending] Commit the batch

## Current Evidence
- `git log --oneline -1` shows current `HEAD` as `e80100a fix: batch 6 - compiler warning reduction and capabilities contract test`
- `git status --short` initially listed only three untracked ELF test binaries:
  - `tests/contract/test_capabilities_contract`
  - `tests/wolfssl/test_wolfssl_connection_contract`
  - `tests/wolfssl/test_wolfssl_context_contract`
- `file` confirmed those artifacts are Linux ELF executables, not source files
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` produced a real RED: the workflow did not install or verify Lazarus / `lazbuild`
- current product-side blocker remains real Windows runtime evidence, but the GitHub Actions lane can now be made capable of collecting it

## Risks
- Do not reopen `src/fafafa.ssl.winssl.*` in this batch.
- Do not treat workflow readiness as a substitute for Windows runtime proof.
- Keep the batch narrow so the commit stays reviewable.

## Follow-up Queue
1. Commit the closeout batch.
2. Trigger the updated `wave-b-b2-manual.yml` on GitHub Actions when a real Windows runtime proof run is needed.

# Task Plan - Wave B/B2 WinSSL Runtime Workflow Alignment

## Goal
把当前手动 Windows CI workflow 对齐到最新 `WinSSL` runtime checklist，让仓库在没有本地 Windows 主机时，仍能通过 `wave-b-b2-manual.yml` 去推进 quick smoke、Wave B gate、broader suite 这条真实运行时证据链。

## Current Batch
1. 用 focused contract 证明当前 `wave-b-b2-manual.yml` 的 Windows job 仍低于最新 runtime checklist。
2. 最小修改 workflow / docs，把 quick smoke、Wave B gate、broader suite transcript 接进现有 Windows lane。
3. 复跑 focused contracts，并更新计划/台账。

## Status
- [pending] RED workflow contract for Windows runtime checklist alignment
- [pending] Align wave-b-b2-manual Windows lane
- [pending] Re-run workflow + bundle contracts
- [pending] Record evidence and commit the batch

## Current Evidence
- fresh broad completion audit 已证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source contract / bundle contract：全部 PASS
- fresh Win64 cross-target compile 已补齐：
  - `tests/winssl/test_winssl_session_management.pas` 可成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 可成功交叉编译到 Win64
- 当前 Linux 主机环境边界已确认：
  - `command -v pwsh`：空 / exit `1`
  - `wine --version`：exit `159`
- 结论：
  - Linux 侧 public surface、capability truth、repo gates、source contract、Win64 compile proof 都已闭合
  - 唯一未闭合 requirement 是真实 Windows 主机上的 `WinSSL` runtime proof
- 当前仓库已有的 Windows CI 入口是 `.github/workflows/wave-b-b2-manual.yml`，但从源码可见：
  - 只跑 `scripts/run_wave_b_windows_gate.ps1`
  - 还没显式安装/验证 `lazbuild`
  - 还没把 quick smoke 和 broader suite transcript 纳入 artifact
- 因此“有 Windows workflow”还不等于“这条 workflow 已覆盖当前 runtime checklist”。

## Risks
- 这批只能把 CI lane 对齐到 checklist，不能替代真实 Windows runtime 结果本身。
- 如果 workflow 只补命令不补 artifact，后续仍然没法做可审查闭环。
- 如果只改 `.yml` 不同步 `.disabled` 模板和文档，后续容易再次漂移。

## Follow-up Queue
1. 触发对齐后的 `wave-b-b2-manual.yml` Windows lane。
2. 审查 quick smoke / Wave B / broader suite artifacts。
3. 只有当这些 Windows runtime 结果真实返回后，才继续判断 broad objective 是否可标记完成。
