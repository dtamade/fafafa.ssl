# TLS13 Signer Gate Snapshot

- run_id: local_bundle_smoke2
- generated_at: 2026-02-10 06:12:17 +0800
- snapshot_state: **GREEN**

## Gate Status

| item | value | expected | result |
|------|-------|----------|--------|
| summary_overall | PASS | PASS | PASS |
| purity_status | PASS | PASS | PASS |
| bench_status | PASS | PASS | PASS |

## Bench Metrics

- scheme: rsa_pkcs1_sha256
- iterations: 1
- warmup: 0
- CRT_avg_ms: 87.0000
- D_avg_ms: 333.0000
- Speedup_D_over_CRT: 3.83x

## Evidence

- summary: test-reports/wave_b_ci_gate_summary_tls13_signer_local_bundle_smoke2.md
- bench_json: test-reports/wave_b_tls13_signer_local_bundle_smoke2.json
- history: test-reports/tls13_signer_bench_history_local_bundle_smoke2.md
