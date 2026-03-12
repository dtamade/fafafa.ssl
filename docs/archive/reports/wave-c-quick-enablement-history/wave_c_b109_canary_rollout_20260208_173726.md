# Wave C B109 Controlled Canary Rollout

- run_id: 20260208_173726
- generated_at: 2026-02-08 17:39:21 +0800
- readiness_report: docs/archive/reports/wave-c-quick-enablement-history/wave_c_b108_default_on_readiness_20260208_173726.md
- threshold_report: docs/archive/reports/wave-c-quick-enablement-history/wave_c_b107_threshold_eval_20260208_173726.md
- validation_report: docs/archive/reports/wave-c-quick-enablement-history/wave_c_b101_validation_20260208_173726.md
- rollout_state: **CANARY_READY**
- default_policy: **DEFAULT_OFF**

## Input Snapshot

| key | value |
|-----|-------|
| readiness | READY |
| threshold_overall | PASS |
| validation_overall | PASS |
| validation_hit_rate_percent | 99.9 |
| validation_speedup_factor_x | 6.5 |

## Canary Stages

| stage | traffic | entry gate | success criteria | rollback trigger |
|-------|---------|------------|------------------|------------------|
| S0 | 0% | rollout_state=CANARY_READY | readiness=READY | any gate != PASS |
| S1 | 5% | S0 passed | validation overall PASS, hit_rate>=99.0, speedup>=3.0 | readiness=HOLD or error burst |
| S2 | 25% | S1 stable 30m | same as S1 | same as S1 |
| S3 | 50% | S2 stable 60m | same as S1 | same as S1 |
| S4 | 100% | S3 stable 120m | same as S1 + no new regression failures | same as S1 |

## Operator Commands

1. 阈值评估
   bash scripts/evaluate_wave_c_b101_thresholds.sh --strict

2. readiness 复核
   bash scripts/check_wave_c_default_on_readiness.sh --strict

3. 失败时策略
   - 保持 default-off
   - 停止扩大流量，回退到上一 stage
   - 重新执行 B101/B107/B108 校验链路
