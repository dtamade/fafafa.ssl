# Wave B Cross-Platform Summary

- run_id: 20260208_041500
- generated_at: 2026-02-08 04:25:19 +0800
- linux_summary: docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md
- linux_examples_json: docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | **PASS** | docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md |
| macos | DRY_RUN | summary: docs/archive/reports/wave-b-history/wave_b_macos_gate_summary_20260208_041500.md (overall=DRY_RUN) |
| windows | PENDING | no evidence |

## 2) Linux Gate Metrics

| metric | value |
|--------|-------|
| total | 75 |
| passed | 71 |
| failed | 0 |
| skipped | 4 |
| pass_rate | 100.0 |

## 3) Cross-Platform Checklist

| check | linux | macos | windows |
|-------|-------|-------|---------|
| compile_all_modules | PASS | TODO | TODO |
| p2_modules_gate | PASS | TODO | TODO |
| examples_compile_gate | PASS | TODO | TODO |
| overall | **PASS** | DRY_RUN | TODO |

## 4) Next Actions

- 在 macOS runner 执行 B2 命令并回填 macos 证据文件。
- 在 Windows runner 执行 WinSSL/OpenSSL 对照回归并回填 windows 证据文件。
- 回填后重新运行本脚本，形成最终三平台对齐摘要。
