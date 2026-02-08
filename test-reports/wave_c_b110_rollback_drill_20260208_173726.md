# Wave C B110 Rollback Drill Report

- run_id: 20260208_173726
- generated_at: 2026-02-08 17:39:22 +0800
- rollout_report: test-reports/wave_c_b109_canary_rollout_20260208_173726.md
- readiness_report: test-reports/wave_c_b108_default_on_readiness_20260208_173726.md
- threshold_report: test-reports/wave_c_b107_threshold_eval_20260208_173726.md
- validation_report: test-reports/wave_c_b101_validation_20260208_173726.md
- simulate_failure: true
- drill_status: **PASS**

## Input Snapshot

| key | value |
|-----|-------|
| rollout_state | CANARY_READY |
| readiness | READY |
| threshold_overall | PASS |
| validation_overall | PASS |
| validation_hit_rate_percent | 99.9 |
| validation_speedup_factor_x | 6.5 |

## Drill Steps

| step | result | note |
|------|--------|------|
| precheck | PASS | rollout/readiness/threshold/validation baseline checks |
| inject_failure | PASS | simulated canary anomaly trigger |
| detect_and_gate | PASS | verify rollback condition can be raised |
| rollback_execute | PASS | rollback to previous safe stage/default-off |
| recovery_recheck | PASS | readiness recheck report: test-reports/wave_c_b110_recheck_20260208_173726.md |
