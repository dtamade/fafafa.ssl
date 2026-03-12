# Wave B / B2 Handoff Bundle

- run_id: 20260208_041500
- generated_at: 2026-02-08 04:25:19 +0800
- handoff_state: **NEEDS_EVIDENCE_SYNC**
- closure_status: IN_PROGRESS
- consistency_status: INCONSISTENT
- strict_mode: true

## Artifacts

| artifact | path | exists |
|----------|------|--------|
| wave_b_ci_gate_summary_20260208_034029.md | docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md | YES |
| examples_compile_ci_gate.json | docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json | YES |
| wave_b_macos_gate_summary_20260208_041500.md | docs/archive/reports/wave-b-history/wave_b_macos_gate_summary_20260208_041500.md | YES |
| wave_b_windows_gate_summary_20260208_041500.md | test-reports/wave_b_windows_gate_summary_20260208_041500.md | NO |
| wave_b_cross_platform_summary_20260208_041500.md | docs/archive/reports/wave-b-history/wave_b_cross_platform_summary_20260208_041500.md | YES |
| wave_b_b2_closure_readiness_20260208_041500.md | docs/archive/reports/wave-b-history/wave_b_b2_closure_readiness_20260208_041500.md | YES |
| wave_b_b2_evidence_consistency_20260208_041500.md | docs/archive/reports/wave-b-history/wave_b_b2_evidence_consistency_20260208_041500.md | YES |

## Next Actions

1. 在 macOS runner 执行 live gate 并回填 macOS summary。
2. 在 Windows runner 执行 live gate 并回填 Windows summary。
3. 回填后重新执行 'scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id 20260208_041500 --strict'。
