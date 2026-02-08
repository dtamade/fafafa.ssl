# Archive Audit Writeback Coverage Autofix Report

## Metadata

| Field | Value |
|-------|-------|
| autofix_id | {autofix_id} |
| generated_at | {timestamp} |
| mode | {dry-run|apply} |
| closure_gate_report | {path} |
| tracker_report | {path|n/a} |
| versioning_report | {path|n/a} |
| sla_rollback_report | {path|n/a} |
| max_actions | {N} |
| owner_filter | {owner|all} |
| priority_filter | {levels} |

## Summary

| Metric | Value |
|--------|-------|
| total_actions | {N} |
| executed_actions | {N} |
| simulated_actions | {N} |
| failed_actions | {N} |
| autofix_status | {pass|pending} |

## Autofix Actions

| blocker_id | priority | owner | original_status | result |
|------------|----------|-------|-----------------|--------|
| BLK-001 | critical | release-manager | pending | simulated |
| BLK-002 | high | qa-lead | pending | simulated |
| ... | ... | ... | ... | ... |

## Execution Log

~~~
Mode: {mode}
Timestamp: {timestamp}
Actions processed: {N}
~~~

## Next Steps

- Review simulated/failed actions above.
- Re-run with --apply to execute pending fixes.
- Escalate failed items to responsible owners.

## Release Advice

| Condition | Advice |
|-----------|--------|
| autofix_status=pass | proceed-to-release-gate |
| autofix_status=pending (dry-run) | re-run-with-apply |
| autofix_status=pending (failed>0) | escalate-and-retry |
