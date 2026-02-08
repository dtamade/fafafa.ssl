# Archive Audit Full Chain Closure Report

## Metadata

| Field | Value |
|-------|-------|
| report_id | {report_id} |
| generated_at | {timestamp} |
| total_stages | {N} |
| configured_stages | {N} |

## Summary

| Metric | Value |
|--------|-------|
| pass_stages | {N} |
| warn_stages | {N} |
| fail_stages | {N} |
| missing_stages | {N} |
| completion_rate | {N}% |
| overall_status | {pass|warn|fail|pending} |

## Chain Status

| Stage | Batch | Status | Report |
|-------|-------|--------|--------|
| closure_gate | B53 | {status} | {report_name} |
| autofix | B54 | {status} | {report_name} |
| verify | B55 | {status} | {report_name} |
| retry | B56 | {status} | {report_name} |
| trend | B57 | {status} | {report_name} |
| revalidate | B58 | {status} | {report_name} |
| sla_drill | B52 | {status} | {report_name} |

## Chain Flow

~~~
[B53: Closure Gate] --> [B54: Autofix] --> [B58: Revalidate]
         |                    |
         v                    v
    [B55: Verify]        [B56: Retry]
         |                    |
         v                    v
    [B52: SLA Drill]     [B57: Trend]
         |                    |
         +--------------------+
                  |
                  v
         [B59: Full Chain Report]
~~~

## Assessment

| Check | Status | Detail |
|-------|--------|--------|
| chain_coverage | {adequate|partial} | {N} of {N} stages configured |
| pass_rate | {high|medium|low} | {N}% |
| blocking_issues | {yes|no} | {N} failed stages |
| overall_readiness | {status} | {detail} |

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
| overall_status=pending | complete-chain-first |
