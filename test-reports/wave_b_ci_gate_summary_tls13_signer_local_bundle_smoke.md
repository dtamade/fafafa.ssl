# Wave B Linux CI Gate Summary

- Run ID: `20260210_060849`
- Generated At: `2026-02-10 06:08:50 +0800`
- Project Root: `/home/dtamade/projects/fafafa.ssl`
- Overall Status: **PASS**

## Gate Steps

| Step | Exit Code | Status | Log |
|------|-----------|--------|-----|
| compile_all_modules | `0` | **SKIP** | `-` |
| run_all_module_tests | `0` | **SKIP** | `-` |
| verify_examples_compile | `0` | **SKIP** | `-` |
| tls13_signer_purity | `0` | **PASS** | `test-reports/wave_b_tls13_sign_purity_20260210_060849.log` |
| tls13_servercertverify_bench | `0` | **PASS** | `test-reports/wave_b_tls13_sign_bench_20260210_060849.log` |

## Examples Gate Metrics

- Report: `test-reports/examples_compile_ci_gate.json`
- Threshold: `80.0`
- Summary: `passed=n/a, failed=n/a, skipped=n/a, total=n/a, pass_rate=n/a`

## TLS13 Signer Bench Metrics

- Scheme: `rsa_pkcs1_sha256`
- Iterations: `1`
- Warmup: `0`
- Timeout: `180`
- Key: `tests/certificate/test_certs/signer_key.pem`
- CRT_avg_ms: `121.0000`
- D_avg_ms: `510.0000`
- Speedup_D_over_CRT: `4.21x`
- JSON: `test-reports/wave_b_tls13_signer_local_bundle_smoke.json`

## Commands

`cd '/home/dtamade/projects/fafafa.ssl' && python3 scripts/compile_all_modules.py`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/verify_examples_compile.sh -f json -o 'test-reports/examples_compile_ci_gate.json'`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/check_tls13_signer_pure_pascal.sh`

`cd '/home/dtamade/projects/fafafa.ssl' && FAFAFA_TLS13_SIGN_BENCH_ITERATIONS='1' FAFAFA_TLS13_SIGN_BENCH_WARMUP='0' FAFAFA_TLS13_SIGN_BENCH_SCHEME='rsa_pkcs1_sha256' FAFAFA_TLS13_SIGN_BENCH_KEY='tests/certificate/test_certs/signer_key.pem' FAFAFA_TLS13_SIGN_BENCH_TIMEOUT='180' FAFAFA_TLS13_SIGN_BENCH_JSON_OUT='/home/dtamade/projects/fafafa.ssl/test-reports/wave_b_tls13_signer_local_bundle_smoke.json' bash scripts/run_freepascal_tls13_servercertverify_bench.sh`
