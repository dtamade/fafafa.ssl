# Wave C B109 Controlled Canary Enablement Template（2026-02-08）

## 目标

在保持生产默认 `default-off` 的前提下，提供一套可执行、可回滚、可审计的受控灰度启用模板。

## 前置条件

1. B107 阈值评估 `overall=PASS`。
2. B108 readiness `READY`。
3. 最新 B101 全门禁 `overall=PASS`。

## 流量阶段

- S0: 0%（准备态）
- S1: 5%
- S2: 25%
- S3: 50%
- S4: 100%

每个阶段都必须执行：
- `evaluate_wave_c_b101_thresholds.sh --strict`
- `check_wave_c_default_on_readiness.sh --strict`

## 回滚准则

任一阶段满足以下条件即回滚：
- readiness 从 `READY` 变为 `HOLD`
- threshold 从 `PASS` 变为 `FAIL`
- 回归门禁出现新增失败

回滚动作：
1. 立即停止扩量。
2. 回退到上一稳定阶段或直接回到 default-off。
3. 复跑 B101/B107/B108 链路并产出新证据。

## 自动化入口

- `scripts/prepare_wave_c_b109_canary_rollout.sh`
  - 读取现有报告，输出可执行灰度计划：
    - `test-reports/wave_c_b109_canary_rollout_<run_id>.md`
