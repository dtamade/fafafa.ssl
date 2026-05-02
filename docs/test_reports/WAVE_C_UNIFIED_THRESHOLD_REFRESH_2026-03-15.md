# Wave C Unified Threshold Refresh（2026-03-15）

## Purpose

基于 2026-03-15 统一后的 fast-local 入口，重建 B106 风格的阈值观察表，避免继续依赖 2026-02-08 的历史旁路样本。

## Input Evidence

- `tmp/test-reports/wave_c_b101_validation_20260315_172046.md`
- `tmp/test-reports/wave_c_b101_validation_20260315_180632.md`
- `tmp/test-reports/wave_c_b101_validation_20260315_180735.md`

## Snapshot

| run_id | overall | hit_rate_percent | speedup_factor_x |
|--------|---------|------------------|------------------|
| 20260315_172046 | PASS | 99.9 | 5.2 |
| 20260315_180632 | PASS | 99.9 | 7.7 |
| 20260315_180735 | PASS | 99.9 | 5.9 |

## Observations

1. 功能门禁稳定：3 次 `B101 --fast-local --strict --full-gate` 均为 `PASS`。
2. 命中率稳定：3 次 `hit_rate_percent` 全部为 `99.9`。
3. 加速比稳定高于旧阈值：区间 `5.2x ~ 7.7x`，中位数约 `5.9x`。

## Threshold Check

| Gate | Rule | Result |
|------|------|--------|
| Gate 1 | `overall == PASS` | PASS |
| Gate 2 | `hit_rate_percent >= 99.0` | PASS |
| Gate 3 | `speedup_factor_x >= 3.0` | PASS |
| Consecutive | 连续 3 次满足 Gate 1-3 | PASS |

## Decision

- 基于新统一入口，旧 B106 阈值已经被重新证明。
- 当前状态可推进到新的 readiness/default-on 评估讨论。
- 但默认策略仍建议保持 `DEFAULT_OFF`，直到你明确要求推进默认开启或受控灰度。

## Recommended Next Step

1. 如需继续保守推进：
   - 生成新的 B107/B108 风格 readiness / canary 文档，输入仅使用 2026-03-15 之后的新入口证据。
2. 如需继续工程化：
   - 将 B101/B107/B108 的阈值判断脚本化，减少手工判读。
