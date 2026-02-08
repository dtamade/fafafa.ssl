# Gate & Archive Evidence Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| report_id | b16_sample_20260207_0456 |
| generated_at | 2026-02-07 04:42:04 +0800 |
| platform | linux |
| workflow_profile | nightly |
| repository | fafafa.ssl |
| run_id | b16_sample_20260207_0456 |
| focus_layer | L2 |

## 2) Gate Layer Results

| layer | scope | command entry | expected | actual | status |
|------|-------|---------------|----------|--------|--------|
| L0 | 环境预检 | <command> | 依赖可见 | <result> | <pass/fail/skip> |
| L1 | 快速阻断 | <command> | 编译/核心回归通过 | <result> | <pass/fail/skip> |
| L2 | 扩展验证 | <command> | 路径/兼容链路可执行 | <result> | <pass/fail/skip> |
| L3 | 深度验证 | <command> | 矩阵/性能/对照验证 | <result> | <pass/fail/skip> |

## 3) Command Evidence

| # | command | exit code | output report/log |
|---|---------|-----------|-------------------|
| 1 | <command> | <0/non-zero> | <path> |

## 4) Archive Mapping Evidence

| class | profile retention | artifact path | included | notes |
|------|-------------------|---------------|----------|-------|
| core-reports | <days> | <path> | <yes/no> | |
| perf-baseline | <days> | <path> | <yes/no> | |
| docs-evidence | <days> | <path> | <yes/no> | |
| debug-logs | <days> | <path> | <yes/no> | |
| binaries | <days> | <path> | <yes/no> | <optional> |

## 5) Decision

- merge_blocking: <true/false>
- release_blocking: <true/false>
- decision_reason: <one sentence>

## 6) Follow-ups

- <ticket_or_action_1>
- <ticket_or_action_2>

## 7) Attachments

- artifacts/ci/<run_id>/manifest.csv
- artifacts/ci/<run_id>/manifest.md
