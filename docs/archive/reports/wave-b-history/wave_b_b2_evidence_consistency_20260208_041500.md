# Wave B / B2 Evidence Consistency

- run_id: 20260208_041500
- generated_at: 2026-02-08 04:25:19 +0800
- consistency_status: **INCONSISTENT**
- strict_mode: false
- required_missing: 0
- runid_mismatch_or_parse_issue: 1
- closure_status_note: IN_PROGRESS

## Artifact Matrix

| artifact | path | exists | parsed_run_id | run_id_match | note |
|----------|------|--------|---------------|--------------|------|
| linux_summary | docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md | YES | 20260208_034029 | NO | run_id mismatch |
| linux_examples_json | docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json | YES | n/a | n/a | json_valid=YES |
| macos_summary | docs/archive/reports/wave-b-history/wave_b_macos_gate_summary_20260208_041500.md | YES | 20260208_041500 | YES | ok |
| windows_summary | test-reports/wave_b_windows_gate_summary_20260208_041500.md | NO | n/a | NO | missing |
| cross_summary | docs/archive/reports/wave-b-history/wave_b_cross_platform_summary_20260208_041500.md | YES | 20260208_041500 | YES | ok |
| closure_report | docs/archive/reports/wave-b-history/wave_b_b2_closure_readiness_20260208_041500.md | YES | 20260208_041500 | YES | ok |

## Gate Rule

- CONSISTENT 条件：required_missing=0 且 runid_mismatch_or_parse_issue=0
- strict 模式：若非 CONSISTENT，脚本返回非 0
