# Wave B/B2 Consistency Cross Summary Windows Summary Required Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 对 active custom `windows_summary` 的 required 语义缺口，避免在 `cross summary` 已承认 Windows summary 为 active evidence 时，summary 本体和 sibling runtime artifacts 仍被当成 optional missing。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 `cross summary` 已声明 active custom `windows_summary`，但 direct consistency 仍把它和 companion runtime artifacts 记成 optional / green；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内把 cross-summary-declared Windows summary 提升为 required evidence，并让 runtime strictness 跟着 active summary truth 走；
- 最后用 focused 合同、active Windows summary path、Windows companion-path、linux/macOS active-path 与 inactive-probe 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-cross-summary-windows-summary-required.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for active Windows required-evidence drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - cross summary already records an active custom `windows_summary`
  - that summary is removed afterward
  - direct consistency still keeps `required_missing=0` and marks the two runtime artifacts as optional missing

## Task 2: Minimal active Windows required-semantics hardening

Patch:

- when cross summary declares an active custom `windows_summary`:
  - `windows_summary` becomes required evidence
  - `windows_quick_log` and `windows_runtime_transcript` also become required evidence, even if the summary file is now missing
- keep sibling-path derivation and default `test-reports/...` behavior unchanged
- do not expand this batch into workflow or producer-side changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
git diff --check
```

Expected after fix:

- active custom `windows_summary` no longer stays optional when `cross summary` already says it is active
- missing sibling runtime artifacts are marked as required missing under the same active Windows truth
- active Windows/Linux/macOS path inheritance and inactive macOS probe boundaries remain GREEN
