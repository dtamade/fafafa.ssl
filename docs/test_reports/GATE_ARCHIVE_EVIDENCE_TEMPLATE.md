# Gate & Archive Evidence Template（Draft）

> 适用于 Linux / macOS / Windows 的门禁分层证据统一记录。

## 1) Metadata

| field | value |
|------|-------|
| report_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| platform | `<linux|macos|windows>` |
| workflow_profile | `<pr|nightly|release>` |
| repository | `fafafa.ssl` |
| run_id | `<ci_run_id_or_local_run_id>` |
| branch_or_ref | `<branch/tag>` |

## 2) Gate Layer Results

| layer | scope | command entry | expected | actual | status |
|------|-------|---------------|----------|--------|--------|
| L0 | 环境预检 | `<cmd>` | 依赖可见 | `<result>` | `<pass/fail/skip>` |
| L1 | 快速阻断 | `<cmd>` | 编译/核心回归通过 | `<result>` | `<pass/fail/skip>` |
| L2 | 扩展验证 | `<cmd>` | 路径/兼容链路可执行 | `<result>` | `<pass/fail/skip>` |
| L3 | 深度验证 | `<cmd>` | 矩阵/性能/对照验证 | `<result>` | `<pass/fail/skip>` |

## 3) Command Evidence

| # | command | exit code | output report/log |
|---|---------|-----------|-------------------|
| 1 | `<command>` | `<0/非0>` | `<path>` |
| 2 | `<command>` | `<0/非0>` | `<path>` |

## 4) Archive Mapping Evidence

| class | profile retention | artifact path | included | notes |
|------|-------------------|---------------|----------|-------|
| core-reports | `<days>` | `<path>` | `<yes/no>` | `<...>` |
| perf-baseline | `<days>` | `<path>` | `<yes/no>` | `<...>` |
| docs-evidence | `<days>` | `<path>` | `<yes/no>` | `<...>` |
| debug-logs | `<days>` | `<path>` | `<yes/no>` | `<...>` |
| binaries | `<days>` | `<path>` | `<yes/no>` | `<optional>` |

## 5) Decision

- **merge_blocking**: `<true/false>`
- **release_blocking**: `<true/false>`
- **decision_reason**: `<one sentence>`

## 6) Follow-ups

- `<ticket_or_action_1>`
- `<ticket_or_action_2>`

## 7) Attachments

- `artifacts/ci/<run_id>/manifest.csv`
- `artifacts/ci/<run_id>/manifest.md`
- `<other key logs/reports>`
