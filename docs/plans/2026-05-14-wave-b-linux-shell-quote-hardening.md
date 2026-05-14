# 2026-05-14 Wave B Linux Shell Quote Hardening

## Goal
收口 `scripts/run_wave_b_ci_gate.sh` 的动态命令拼接风险，避免 `--modules` 与 TLS13 bench 相关输入被当成 `bash -lc` shell 语法执行。

## Architecture
- 当前 Linux gate 仍通过 `run_step() -> "$STEP_SHELL" -lc "$cmd"` 执行字符串命令。
- 两条已坐实的注入路径：
  - `--modules "PKCS7; touch '$MARKER'; #"` 会执行 payload，且 gate 仍可 `exit 0`
  - `--tls13-sign-bench-scheme "rsa_pkcs1_sha256'; touch '$MARKER'; echo '"` 会执行 payload，且 gate 仍可 `exit 0`
- 仓库里已有两套安全先例可直接复用：
  - `scripts/run_minimal_ci_gate.sh`：argv / `env` 数组执行
  - `scripts/run_wave_b_macos_gate.sh`：`printf '%q'` 生成仅用于展示的命令串

## Files
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_module_injection_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_tls13_sign_bench_scheme_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写两个 focused contracts，分别锁住 `--modules` 与 `--tls13-sign-bench-scheme` 的注入边界。
2. 最小修改 `run_wave_b_ci_gate.sh`，把 step 执行从字符串 shell 切到 argv / env 数组。
3. 复跑新合同与现有 Linux gate focused contracts，确认 dry-run、summary、run-id 与 examples 语义不回归。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_ci_gate_module_injection_contract.sh
bash tests/scripts/test_wave_b_ci_gate_module_injection_contract.sh
bash -n tests/scripts/test_wave_b_ci_gate_tls13_sign_bench_scheme_injection_contract.sh
bash tests/scripts/test_wave_b_ci_gate_tls13_sign_bench_scheme_injection_contract.sh
bash tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh
bash tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_invalid_examples_json_contract.sh
bash tests/scripts/test_wave_b_ci_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
bash tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh
bash -n scripts/run_wave_b_ci_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - payload 可真实执行
  - nested runner 只能收到被截断的参数，或根本拿不到 bench env
  - summary 仍可能伪装成一次正常 PASS
- 修复后：
  - payload 不再执行
  - nested runner 仍收到完整 `modules` 原始值与完整 bench scheme env
  - dry-run / summary / examples threshold 语义保持不变。
