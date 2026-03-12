# Wave B Linux CI Gate Summary

- Run ID: `20260208_025426`
- Generated At: `2026-02-08 02:57:07 +0800`
- Project Root: `/home/dtamade/projects/fafafa.ssl`
- Overall Status: **PASS**

## Gate Steps

| Step | Exit Code | Status | Log |
|------|-----------|--------|-----|
| compile_all_modules | `0` | **PASS** | `test-reports/wave_b_compile_20260208_025426.log` |
| run_all_module_tests | `0` | **PASS** | `test-reports/wave_b_modules_20260208_025426.log` |
| verify_examples_compile | `0` | **PASS** | `test-reports/wave_b_examples_20260208_025426.log` |

## Examples Gate Metrics

- Report: `docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json`
- Threshold: `80.0`
- Summary: `passed=71, failed=0, skipped=4, total=75, pass_rate=100.0`

## Commands

`cd '/home/dtamade/projects/fafafa.ssl' && python3 scripts/compile_all_modules.py`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/verify_examples_compile.sh -f json -o 'docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json'`
