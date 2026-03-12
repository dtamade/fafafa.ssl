# Wave B Linux CI 门禁执行记录（2026-02-08）

## 执行目标

将 Wave B / B1 的三条门禁命令收敛为单一脚本入口，并输出可归档证据。

## 脚本入口

- `scripts/run_wave_b_ci_gate.sh`

## 本轮执行证据

- 命令：`bash scripts/run_wave_b_ci_gate.sh --examples-threshold 80.0`
- Summary：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_022636.md`
- Compile log：`test-reports/wave_b_compile_20260208_022636.log`
- Modules log：`test-reports/wave_b_modules_20260208_022636.log`
- Examples log：`test-reports/wave_b_examples_20260208_022636.log`

## 结果

- overall status: **PASS**
- compile_all_modules: exit `0`
- run_all_module_tests(P2): exit `0`
- verify_examples_compile: exit `1`（按阈值判定通过）
- examples summary: `62/75`，`87.3%`

## 判定口径

- 示例门禁采用阈值策略：`pass_rate >= 80.0` 视为通过。
- 当前 `87.3%`，满足 Wave B 最小门禁要求。

## 追加执行（2026-02-08 02:57 +0800）

- 命令：`bash scripts/run_wave_b_ci_gate.sh --examples-threshold 80.0`
- 产物：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_025426.md`
- 结果：overall **PASS**
  - compile: PASS
  - modules: PASS
  - examples: PASS（`71/75`, `failed=0`, `pass_rate=100.0%`）

结论：Wave B Linux 最小门禁进入稳定态；后续重点为跨平台 runner 实测回填。

## 追加执行（2026-02-08 03:43 +0800）

- 命令：`bash scripts/run_wave_b_ci_gate.sh --examples-threshold 80.0`
- 产物：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md`
- 结果：overall **PASS**
  - compile: PASS
  - modules: PASS
  - examples: PASS（`71/75`, `failed=0`, `pass_rate=100.0%`）

### 口径稳健性补充

- `scripts/verify_examples_compile.sh` 已修复 JSON 空失败列表输出。
- `docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json` 当前 `failed_files=[]`，可直接供跨平台汇总脚本消费。
