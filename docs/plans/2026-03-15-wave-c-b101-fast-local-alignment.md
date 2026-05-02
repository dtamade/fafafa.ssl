# Wave C B101 fast-local alignment（2026-03-15）

## Goal
- 让 `scripts/run_wave_c_b101_validation_playbook.sh` 与当前 Wave C baseline / fast-local 策略一致。
- 保证 B101 playbook 在本地执行时，report/log/benchmark bin/full-gate 临时产物都能落到 `./tmp`，且不污染工作区。

## Architecture / Approach
1. `run_wave_c_b101_validation_playbook.sh`
   - 增加 `--fast-local`、`--reports-dir`、`--bench-bin-dir`
   - `--dry-run` 输出解析后的绝对路径配置
   - `--fast-local --full-gate` 时：
     - `compile_all_modules.py` 使用 `--unit-output-dir tmp/wave_c_b101_compile_units_<run_id>`
     - `run_all_module_tests.sh` 使用独立的 module report/bin/unit 目录
     - benchmark bin 使用 `tmp/wave_c_b101_bench_bin_<run_id>`
2. `cleanup_fast_local_outputs.sh`
   - 纳入 `wave_c_b101_*` 相关目录
3. Contracts
   - dry-run clean-worktree
   - real execution clean-worktree

## Files
- `scripts/run_wave_c_b101_validation_playbook.sh`
- `scripts/cleanup_fast_local_outputs.sh`
- `tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_dry_run_contract.sh`
- `tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_contract.sh`
- `tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`

## Step-by-step Commands
1. Syntax:
   - `bash -n scripts/run_wave_c_b101_validation_playbook.sh scripts/cleanup_fast_local_outputs.sh`
2. Contracts:
   - `bash tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_dry_run_contract.sh`
   - `bash tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_contract.sh`
   - `bash tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`
3. Focused execution:
   - `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict`

## Expected Outputs
- dry-run 输出所有关键路径均位于 `./tmp`
- 实际执行在 `tmp/test-reports/` 生成 B101 report/log
- fast-local 真实执行前后 `git status --porcelain` 不变化
