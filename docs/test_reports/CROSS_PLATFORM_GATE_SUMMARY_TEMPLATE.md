# Cross-Platform Gate Summary Template（Draft）

- generated_at: `<YYYY-MM-DD HH:MM:SS +TZ>`
- run_id: `<summary_run_id>`
- source_pattern: `<input_glob_or_file>`
- input_reports: `<count>`

## 1) Input Evidence Reports

| platform | profile | run_id | focus_layer | source |
|----------|---------|--------|-------------|--------|
| `<linux|macos|windows>` | `<pr|nightly|release>` | `<run_id>` | `<L0|L1|L2|L3>` | `<path>` |

## 2) Layer Signal Snapshot

| platform | profile | run_id | layer | status | source |
|----------|---------|--------|-------|--------|--------|
| `<platform>` | `<profile>` | `<run_id>` | `L0` | `<pass/fail/skip/unknown>` | `<path>` |
| `<platform>` | `<profile>` | `<run_id>` | `L1` | `<pass/fail/skip/unknown>` | `<path>` |
| `<platform>` | `<profile>` | `<run_id>` | `L2` | `<pass/fail/skip/unknown>` | `<path>` |
| `<platform>` | `<profile>` | `<run_id>` | `L3` | `<pass/fail/skip/unknown>` | `<path>` |

## 3) Platform Aggregate

| platform | report_count | profile_samples |
|----------|--------------|-----------------|
| `<platform>` | `<count>` | `<profiles>` |

## 4) Next Actions

- `<action_1>`
- `<action_2>`
- `<action_3>`
