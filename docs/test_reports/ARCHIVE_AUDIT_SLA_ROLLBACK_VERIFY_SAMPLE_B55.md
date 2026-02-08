# Archive Audit SLA/Rollback Linkage Verification Report

## Metadata

| Field | Value |
|-------|-------|
| verify_id | b55_sample_20260207_2054 |
| generated_at | 2026-02-07 20:54:31 +0800 |
| sla_alert_report | docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md |
| rollback_drill_report | docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md |
| linkage_drill_report | docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md |
| archive_root | artifacts/audit |
| dry_run | false |

## Summary

| Metric | Value |
|--------|-------|
| total_checks | 9 |
| pass_checks | 8 |
| fail_checks | 0 |
| warn_checks | 1 |
| skip_checks | 0 |
| verify_status | warn |

## Verification Checks

| status | check_label | target | detail |
|--------|-------------|--------|--------|
| pass | sla_alert_report | docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md | exists |
| pass | sla_alert_content | docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md | non-empty |
| pass | rollback_drill_report | docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md | exists |
| pass | rollback_drill_content | docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md | non-empty |
| pass | linkage_drill_report | docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md | exists |
| pass | linkage_drill_content | docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md | non-empty |
| warn | archive_root | artifacts/audit | missing |
| pass | cross_report_linkage | sla=79,linkage=97 | populated |
| pass | timestamp_presence | sla=2026-02-07 08:49:27 +0800,rollback=2026-02-07 09:13:47 +0800 | both-present |

## Archive Integrity

| Check | Status |
|-------|--------|
| sla_alert_archived | present |
| rollback_drill_archived | present |
| linkage_drill_archived | present |

## Next Steps

- Some warnings detected, review recommended.
- Consider re-generating missing reports.

## Release Advice

| Condition | Advice |
|-----------|--------|
| verify_status=pass | proceed-to-release |
| verify_status=warn | review-and-proceed |
| verify_status=fail | block-until-fixed |
