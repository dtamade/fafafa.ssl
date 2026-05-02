# Wave C B107 Threshold Evaluation Result（2026-03-15）

## Goal

基于 2026-03-15 之后统一的 fast-local B101 full-gate 入口，重新执行 B107 阈值评估。

## Inputs

- `tmp/test-reports/wave_c_b101_validation_20260315_172046.md`
- `tmp/test-reports/wave_c_b101_validation_20260315_180632.md`
- `tmp/test-reports/wave_c_b101_validation_20260315_180735.md`

## Command

```bash
bash scripts/evaluate_wave_c_b101_thresholds.sh \
  --reports-dir tmp/test-reports \
  --report-glob 'wave_c_b101_validation_20260315_*.md' \
  --require-full-gate \
  --run-id 20260315_unified \
  --strict \
  --output tmp/test-reports/wave_c_b107_threshold_eval_20260315_unified.md
```

## Result

- threshold report: `tmp/test-reports/wave_c_b107_threshold_eval_20260315_unified.md`
- overall: `PASS`
- passing_runs: `3`
- total_runs: `3`

## Conclusion

- B107 已在新的 fast-local 统一入口下重新证明。
- 当前阈值判断不再依赖 2026-02-08 的历史旁路样本。
