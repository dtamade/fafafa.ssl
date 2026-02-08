# Archive Audit SLA/Rollback Linkage Verification Report

## Metadata

| Field | Value |
|-------|-------|
| verify_id | {verify_id} |
| generated_at | {timestamp} |
| sla_alert_report | {path|n/a} |
| rollback_drill_report | {path|n/a} |
| linkage_drill_report | {path|n/a} |
| archive_root | {path} |
| dry_run | {true|false} |

## Summary

| Metric | Value |
|--------|-------|
| total_checks | {N} |
| pass_checks | {N} |
| fail_checks | {N} |
| warn_checks | {N} |
| skip_checks | {N} |
| verify_status | {pass|warn|fail} |

## Verification Checks

| status | check_label | target | detail |
|--------|-------------|--------|--------|
| pass | sla_alert_report | {path} | exists |
| pass | sla_alert_content | {path} | non-empty |
| fail | archive_root | {path} | missing |
| ... | ... | ... | ... |

## Archive Integrity

| Check | Status |
|-------|--------|
| sla_alert_archived | {present|missing|n/a} |
| rollback_drill_archived | {present|missing|n/a} |
| linkage_drill_archived | {present|missing|n/a} |

## Next Steps

- Review warnings and address failed checks.
- Re-run verification after fixes.

## Release Advice

| Condition | Advice |
|-----------|--------|
| verify_status=pass | proceed-to-release |
| verify_status=warn | review-and-proceed |
| verify_status=fail | block-until-fixed |
