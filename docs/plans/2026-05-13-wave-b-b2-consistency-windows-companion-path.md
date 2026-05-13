# Wave B/B2 Consistency Windows Companion Path Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 在 custom `windows_summary` 场景下的 companion-path 漂移，让它在未显式传 `--windows-quick-log` / `--windows-runtime-transcript` 时，也能默认跟随 `windows_summary` 同目录寻找 sibling artifacts。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 custom `windows_summary` 已存在且 sibling runtime artifacts 真实存在，但 direct consistency 仍误报缺失；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内补一层 sibling-artifact 默认推导；
- 最后用 focused 合同、linux examples active-path、macOS probe、Windows strict 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-windows-companion-path.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for custom windows sibling artifacts

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - custom `windows_summary` exists
  - sibling `winssl_quick_smoke_<run_id>.log` and `winssl_runtime_suite_<run_id>.log` also exist beside it
  - but consistency still only checks `test-reports/...`

## Task 2: Minimal sibling-path inference hardening

Patch:

- add a small helper to derive sibling artifact paths from `WINDOWS_SUMMARY`
- when `--windows-quick-log` / `--windows-runtime-transcript` are omitted:
  - default to the sibling paths beside `WINDOWS_SUMMARY`
  - keep existing `test-reports/...` behavior unchanged for the default summary path
- do not expand this batch into workflow or producer-side changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

Expected after fix:

- direct consistency no longer misreports sibling Windows runtime artifacts as missing when a custom `windows_summary` path is provided
- linux examples active-path inheritance, macOS probe strict boundary, and Windows strict missing-artifact rules remain GREEN
