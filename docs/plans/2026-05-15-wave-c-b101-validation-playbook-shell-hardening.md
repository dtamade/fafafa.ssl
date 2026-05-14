# 2026-05-15 Wave C B101 Validation Playbook Shell Hardening

## Goal
收口 `scripts/run_wave_c_b101_validation_playbook.sh` 的 `eval` / 字符串执行风险，避免 `--modules` 与 `--bench-bin-dir` 被当成 shell 语法执行。

## Architecture
- 当前 B101 validation playbook 仍通过：
  - `run_step() -> ( cd "$PROJECT_ROOT" && eval "$cmd" )`
  - `modules_cmd` 直接把 `MODULE_SET` 拼进字符串命令
  - `bench_compile` 直接把 `BENCH_BIN_DIR` 拼进 `mkdir -p && fpc ... -FE...`
  - `bench_run` 直接把 `BENCH_BIN_DIR` 拼进 benchmark 可执行路径字符串
- 这意味着即使 `resolve_under_project_root()` 已经收住“写到仓库外”的问题，动态输入仍会在真正执行前重新进入 shell 解释层。
- 这批最小修法继续保持一致：
  - display 命令保留
  - 真正执行切成 direct argv / `env "KEY=value"` 数组
  - `mkdir -p` 前置到 shell 外，避免再用 `&&` 串联

## Files
- `scripts/run_wave_c_b101_validation_playbook.sh`
- `tests/scripts/test_run_wave_c_b101_validation_playbook_modules_injection_contract.sh`
- `tests/scripts/test_run_wave_c_b101_validation_playbook_bench_bin_dir_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--modules` payload 当前可从 full-gate module step 的 `eval` 命令串逃逸。
2. 写 focused contract，证明 `--bench-bin-dir` payload 当前可从 bench compile / run 路径的 `eval` 命令串逃逸。
3. 最小修改 B101 validation playbook，把 compile/modules/bench compile/bench run 都切到 argv / env 数组执行。
4. 复跑两个 focused contract、`test_run_wave_c_b101_validation_playbook_fast_local_contract.sh`、`test_run_wave_c_b101_validation_playbook_fast_local_dry_run_contract.sh` 和脚本语法检查。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_b101_validation_playbook_modules_injection_contract.sh
bash tests/scripts/test_run_wave_c_b101_validation_playbook_modules_injection_contract.sh
bash -n tests/scripts/test_run_wave_c_b101_validation_playbook_bench_bin_dir_injection_contract.sh
bash tests/scripts/test_run_wave_c_b101_validation_playbook_bench_bin_dir_injection_contract.sh
bash tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_contract.sh
bash tests/scripts/test_run_wave_c_b101_validation_playbook_fast_local_dry_run_contract.sh
bash -n scripts/run_wave_c_b101_validation_playbook.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `--modules` payload 可作为 shell 语法执行
  - `--bench-bin-dir` payload 可从 bench compile / run 命令串逃逸
- 修复后：
  - `MODULE_SET` 只作为单个 argv 透传给 module runner
  - `BENCH_BIN_DIR` 只作为 argv / env 数据进入 mkdir、fpc 和 benchmark 可执行路径
  - 既有 `fast-local` 与 `dry-run` 契约保持 green
