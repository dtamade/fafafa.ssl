# Wave B Linux CI Gate Summary

- Run ID: `20260208_020808`
- Generated At: `2026-02-08 02:08:08 +0800`
- Project Root: `/home/dtamade/projects/fafafa.ssl`
- Overall Status: **FAIL**

## Gate Steps

| Step | Exit Code | Status | Log |
|------|-----------|--------|-----|
| compile_all_modules | `[WAVE-B] [compile] cd '/home/dtamade/projects/fafafa.ssl' && python3 scripts/compile_all_modules.py
[WAVE-B] [compile] exit=1 elapsed=0s log=/home/dtamade/projects/fafafa.ssl/test-reports/wave_b_compile_20260208_020808.log
1` | **FAIL** | `test-reports/wave_b_compile_20260208_020808.log` |
| run_all_module_tests | `[WAVE-B] [modules] cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
[WAVE-B] [modules] exit=1 elapsed=0s log=/home/dtamade/projects/fafafa.ssl/test-reports/wave_b_modules_20260208_020808.log
1` | **FAIL** | `test-reports/wave_b_modules_20260208_020808.log` |
| verify_examples_compile | `[WAVE-B] [examples] cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/verify_examples_compile.sh -f json -o 'test-reports/examples_compile_ci_gate.json'
[WAVE-B] [examples] exit=2 elapsed=0s log=/home/dtamade/projects/fafafa.ssl/test-reports/wave_b_examples_20260208_020808.log
2` | **FAIL** | `test-reports/wave_b_examples_20260208_020808.log` |

## Examples Gate Metrics

- Report: `test-reports/examples_compile_ci_gate.json`
- Threshold: `80.0`
- Summary: `passed=0, failed=0, skipped=0, total=0, pass_rate=0.0`

## Commands

`cd '/home/dtamade/projects/fafafa.ssl' && python3 scripts/compile_all_modules.py`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

`cd '/home/dtamade/projects/fafafa.ssl' && bash scripts/verify_examples_compile.sh -f json -o 'test-reports/examples_compile_ci_gate.json'`
