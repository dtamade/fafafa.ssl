# Wave C B140 Local Guard Consistency

- run_id: 20260209_051129
- generated_at: 2026-02-09 05:11:29 +0800
- consistency_state: **CONSISTENT**

## Script Checks

| script | result |
|--------|--------|
| scripts/check_wave_c_local_first_continuity.sh | PASS |
| scripts/check_wave_c_local_drift_watch.sh | PASS |
| scripts/run_wave_c_local_first_guard_bundle.sh | PASS |
| scripts/summarize_wave_c_local_guard_history.sh | PASS |
| scripts/run_wave_c_local_guard_oncall_check.sh | PASS |
| scripts/generate_wave_c_local_first_status_snapshot.sh | PASS |
| scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh | PASS |
| scripts/run_wave_c_pre_ci_reenable_full_gate.sh | PASS |

## Document Checks

| document | result |
|----------|--------|
| docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md | PASS |
| docs/test_reports/WAVE_C_B130_ONCALL_RHYTHM_TEMPLATE_2026-02-09.md | PASS |
| docs/test_reports/WAVE_C_B136_DELIVERABLES_OVERVIEW_2026-02-09.md | PASS |
| docs/test_reports/WAVE_C_B137_PRE_CI_REENABLE_PACKET_RESULT_2026-02-09.md | PASS |
| docs/test_reports/WAVE_C_B138_PRE_CI_REENABLE_FULL_GATE_RESULT_2026-02-09.md | PASS |

## Global Checks

| check | value | result |
|------|-------|--------|
| documentation_index_tokens | B137/B138/script | PASS |
| workflow_state | DISABLED | PASS |
