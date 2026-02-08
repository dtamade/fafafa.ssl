# Archive Audit Closure Gate Revalidation Report

## Metadata

| Field | Value |
|-------|-------|
| revalidate_id | {revalidate_id} |
| generated_at | {timestamp} |
| mode | {dry-run|execute} |
| autofix_report | {path|n/a} |
| closure_gate_script | {path|n/a} |

## Autofix Summary

| Metric | Value |
|--------|-------|
| autofix_status | {status} |
| total_actions | {N} |
| applied_actions | {N} |
| failed_actions | {N} |

## Revalidation Results

| Metric | Value |
|--------|-------|
| revalidation_mode | {dry-run|execute|error} |
| revalidation_status | {pass|warn|fail|pending|error} |
| gate_total_checks | {N} |
| gate_failed_checks | {N} |
| gate_pass_rate | {N}% |

## Overall Assessment

| Check | Status | Detail |
|-------|--------|--------|
| autofix_applied | {yes|no} | {N} actions applied |
| autofix_failures | {none|detected} | {N} failures |
| gate_revalidation | {status} | pass_rate={N}% |
| overall_status | {pass|warn|fail|pending} | {detail} |

## Recommendations

- {recommendation_1}
- {recommendation_2}
- {recommendation_3}

## Release Advice

| Condition | Advice |
|-----------|--------|
| overall_status=pass | proceed-to-release |
| overall_status=warn | review-before-release |
| overall_status=fail | block-until-resolved |
| overall_status=pending | complete-revalidation-first |
