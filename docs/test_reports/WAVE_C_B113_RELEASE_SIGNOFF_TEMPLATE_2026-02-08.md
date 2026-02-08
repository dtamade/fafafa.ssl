# Wave C B113 Release Signoff Template（2026-02-08）

## Signoff Metadata

- signoff_id: <RUN_ID>
- generated_at: <YYYY-MM-DD HH:MM:SS +0800>
- release_scope: Wave C cert verify cache rollout governance
- default_policy: DEFAULT_OFF
- signoff_state: PENDING_APPROVAL

## Required Evidence

| gate | required | evidence | status |
|------|----------|----------|--------|
| B107 threshold | PASS | `test-reports/wave_c_b107_threshold_eval_<run_id>.md` | <PASS/FAIL> |
| B108 readiness | READY | `test-reports/wave_c_b108_default_on_readiness_<run_id>.md` | <READY/HOLD> |
| B109 canary plan | CANARY_READY | `test-reports/wave_c_b109_canary_rollout_<run_id>.md` | <CANARY_READY/BLOCKED> |
| B110 rollback drill | PASS | `test-reports/wave_c_b110_rollback_drill_<run_id>.md` | <PASS/FAIL> |
| Quick sprint bundle | PASS | `test-reports/wave_c_quick_sprint_bundle_<run_id>.md` | <PASS/FAIL> |

## Risk Decision

- allow_canary_execution: <YES/NO>
- allow_default_on_switch: NO (must stay default-off until separate approval)
- rollback_owner: <name>
- incident_contact: <name>

## Approval

- approver_name: <name>
- approver_role: <role>
- approval_time: <timestamp>
- comments: <free text>
