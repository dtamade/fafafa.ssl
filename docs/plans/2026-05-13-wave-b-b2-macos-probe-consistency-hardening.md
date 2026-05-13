# Wave B/B2 macOS Probe Consistency Hardening Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 对 macOS probe-only 证据的盲区，确保当 cross summary 使用 `wave_b_macos_gate_probe_<run_id>.json` 时，consistency report 也会显式列出并校验这份 probe 证据。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 handoff bundle 已经会消费 macOS probe，但 consistency report 仍不列出它；
- 再给 `check_wave_b_b2_evidence_consistency.sh` 增加 `--macos-probe` 支持；
- 同步让 `prepare_wave_b_b2_handoff_bundle.sh` 和 `wave-b-b2-manual.yml` 在 probe-only 场景下把 probe 传进 consistency；
- 最后跑 focused 合同、workflow 文本合同与 handoff/cross-summary 回归。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-macos-probe-consistency-hardening.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh`
- Update: `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `.github/workflows/wave-b-b2-manual.yml`
- Update: `.github/workflows/wave-b-b2-manual.yml.disabled`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for macOS probe consistency coverage

Run:

```bash
bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because consistency report does not include a `macos_probe` row
- workflow text contract fails because the summary job does not pass any `MACOS_CONSISTENCY_ARGS` into `check_wave_b_b2_evidence_consistency.sh`

## Task 2: Minimal macOS probe consistency propagation

Patch:

- add `--macos-probe FILE` support to `check_wave_b_b2_evidence_consistency.sh`
- treat the probe as a JSON artifact and surface it in the artifact matrix when probe-only evidence is active
- pass that probe into consistency from:
  - `prepare_wave_b_b2_handoff_bundle.sh`
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
- keep closure readiness summary-only; do not widen probe handling there

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled
git diff --check
```

Expected after fix:

- consistency report explicitly lists the macOS probe when it is the live macOS evidence
- workflow summary stage also passes macOS probe evidence into consistency checks
- existing probe fallback and Windows runtime contracts remain GREEN
