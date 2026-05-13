# Wave B/B2 Ignore Inactive macOS Probe Consistency Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的过度跟踪问题，避免在 macOS summary 已经是权威证据时，仅因默认路径下存在一个无关/损坏的 probe JSON 就把 consistency 误判为 `INCONSISTENT`。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 inactive macOS probe 当前仍会污染 strict consistency；
- 再只收紧 `check_wave_b_b2_evidence_consistency.sh` 的 macOS probe 跟踪条件；
- 最后用 focused 合同、probe consistency 合同与 handoff/workflow 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-ignore-inactive-macos-probe-consistency.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for inactive probe over-tracking

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because `check_wave_b_b2_evidence_consistency.sh` still tracks `test-reports/wave_b_macos_gate_probe_<run_id>.json` merely because the file exists
- even though cross summary uses macOS summary and the probe is inactive, a malformed probe JSON still flips strict consistency to failure

## Task 2: Minimal tracking-condition hardening

Patch:

- keep tracking `macos_probe` when:
  - the caller explicitly passes `--macos-probe`, or
  - cross summary explicitly indicates `PROBE_ONLY/PROBE_OK`
- stop tracking default macOS probe files merely because they happen to exist beside an authoritative macOS summary
- do not expand this batch into closure/workflow behavior changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
git diff --check
```

Expected after fix:

- inactive stale macOS probe files no longer poison strict consistency
- active probe-only scenarios still surface `macos_probe`
- handoff/workflow contracts remain GREEN
