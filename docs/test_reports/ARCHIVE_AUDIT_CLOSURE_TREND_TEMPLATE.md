# Archive Audit Closure Gate Weekly Trend Review

## Metadata

| Field | Value |
|-------|-------|
| review_id | {review_id} |
| generated_at | {timestamp} |
| gate_report_glob | {pattern} |
| analysis_weeks | {N} |
| drift_threshold | {N}% |

## Summary

| Metric | Value |
|--------|-------|
| reports_analyzed | {N} |
| trend_direction | {improving|degrading|stable} |
| drift_percent | {N}% |
| first_pass_rate | {N}% |
| last_pass_rate | {N}% |
| drift_status | {detected|none} |
| review_status | {pass|warn|fail} |

## Weekly Trend Data

| week | report | total_checks | fail_checks | pass_rate |
|------|--------|--------------|-------------|-----------|
| 1 | {report_name} | {N} | {N} | {N}% |
| 2 | {report_name} | {N} | {N} | {N}% |
| ... | ... | ... | ... | ... |

## Drift Analysis

| Check | Status | Detail |
|-------|--------|--------|
| trend_direction | {direction} | {positive|negative|neutral} |
| drift_magnitude | {N}% | threshold={N}% |
| drift_detected | {detected|none} | {action-required|within-tolerance} |

## Recommendations

- {recommendation_1}
- {recommendation_2}
- {recommendation_3}

## Release Advice

| Condition | Advice |
|-----------|--------|
| review_status=pass | proceed-with-monitoring |
| review_status=warn | review-before-release |
| review_status=fail | block-until-trend-reverses |
