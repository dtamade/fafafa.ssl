# Cross-Platform Gate Summary（Draft）

- generated_at: 2026-02-07 04:50:58 +0800
- run_id: b20_sample_20260207_0522
- source_pattern: docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md
- input_reports: 1

## 1) Input Evidence Reports

| platform | profile | run_id | focus_layer | source |
|----------|---------|--------|-------------|--------|
| linux | nightly | b16_sample_20260207_0456 | L2 | docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md |

## 2) Layer Signal Snapshot

| platform | profile | run_id | layer | status | source |
|----------|---------|--------|-------|--------|--------|
| linux | nightly | b16_sample_20260207_0456 | L0 | unknown | docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md |
| linux | nightly | b16_sample_20260207_0456 | L1 | unknown | docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md |
| linux | nightly | b16_sample_20260207_0456 | L2 | unknown | docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md |
| linux | nightly | b16_sample_20260207_0456 | L3 | unknown | docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md |

## 3) Platform Aggregate

| platform | report_count | profile_samples |
|----------|--------------|-----------------|
| linux | 1 | nightly |

## 4) Next Actions

- 校验 `unknown/missing` 状态对应的原始报告是否缺字段。
- 如需发布级审阅，补齐 release profile 的 L3 实证记录。
- 将本摘要与 `artifacts/ci/<run_id>/manifest.*` 关联归档。
