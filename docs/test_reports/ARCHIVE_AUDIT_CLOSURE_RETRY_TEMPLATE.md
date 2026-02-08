# Archive Audit Closure Acceptance Retry Report

## Metadata

| Field | Value |
|-------|-------|
| retry_id | {retry_id} |
| generated_at | {timestamp} |
| mode | {dry-run|apply} |
| closure_gate_report | {path|n/a} |
| autofix_report | {path|n/a} |
| verify_report | {path|n/a} |
| max_retries | {N} |
| retry_delay | {N}s |
| escalate_threshold | {N} |

## Summary

| Metric | Value |
|--------|-------|
| total_items | {N} |
| retry_items | {N} |
| escalate_items | {N} |
| skip_items | {N} |
| pending_items | {N} |
| retry_status | {pass|pending|escalate} |

## Retry Actions

| item_id | priority | owner | status | retry_count | action |
|---------|----------|-------|--------|-------------|--------|
| BLK-001 | critical | release-manager | retry-pending | 1 | retry |
| BLK-002 | high | qa-lead | escalated | 3 | escalate |
| ... | ... | ... | ... | ... | ... |

## Escalation Queue

| item_id | priority | owner | reason |
|---------|----------|-------|--------|
| BLK-002 | high | qa-lead | max-retries-exceeded |
| ... | ... | ... | ... |

## Next Steps

- Review pending items and re-run with --apply.
- Contact owners for escalated items.

## Release Advice

| Condition | Advice |
|-----------|--------|
| retry_status=pass | proceed-to-release |
| retry_status=pending | re-run-with-apply |
| retry_status=escalate | manual-intervention-required |
