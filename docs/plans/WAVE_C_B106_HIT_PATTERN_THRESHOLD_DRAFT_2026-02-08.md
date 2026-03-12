# Wave C B106 Hit Pattern & Threshold Draft（2026-02-08）

## 输入证据

- `docs/archive/reports/wave-c-quick-enablement-history/wave_c_b101_validation_20260208_043500.md`
- `docs/archive/reports/wave-c-quick-enablement-history/wave_c_b101_validation_20260208_045835.md`
- `docs/archive/reports/wave-c-quick-enablement-history/wave_c_b101_validation_20260208_050421.md`
- `docs/archive/reports/wave-c-quick-enablement-history/wave_c_b101_validation_20260208_051419.md`
- `docs/test_reports/WAVE_C_B105_CACHE_OBSERVABILITY_RESULT_2026-02-08.md`

## 命中模式快照（B101 全门禁）

| run_id | overall | hit_rate_percent | speedup_factor_x |
|--------|---------|------------------|------------------|
| 20260208_043500 | PASS | 99.9 | 4.7 |
| 20260208_045835 | PASS | 99.9 | 7.3 |
| 20260208_050421 | PASS | 99.9 | 6.1 |
| 20260208_051419 | PASS | 99.9 | 5.2 |

## 观察结论

1. 功能门禁稳定：4 次全门禁均 PASS。
2. 命中率稳定：hit rate 全部为 99.9%。
3. 加速比存在波动但均显著 >1x：区间 4.7x ~ 7.3x。

## 阈值策略草案（B106）

### Gate 1（功能）
- `overall` 必须为 `PASS`。

### Gate 2（命中）
- `hit_rate_percent >= 99.0`。

### Gate 3（收益）
- `speedup_factor_x >= 3.0`。
- 连续 3 次运行满足 Gate 2 + Gate 3 才可进入下一批默认-on 评估。

### 异常告警（仅告警，不自动升级风险级别）
- speedup 相对最近 3 次中位数下降超过 30%。
- hit rate 低于 99.0%。

## 执行建议（B107 前）

1. 保持 `ssoEnableCertVerifyCache` 默认关闭。
2. 每次代码接入后执行：
   - `bash scripts/run_wave_c_b101_validation_playbook.sh --run-id <RUN_ID> --strict --full-gate`
3. 记录 run 表并更新该文档的阈值观察区。

## 结论

- 当前证据支持继续推进 Wave C，且不需要触发高风险升级。
- 建议 B107 聚焦“阈值自动评估脚本化”，避免人工误判。
