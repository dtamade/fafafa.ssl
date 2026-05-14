# 2026-05-15 Wave B macOS Shell Execution Hardening

## Goal
收口 `scripts/run_wave_b_macos_gate.sh` 仍通过 login shell 执行 step 的底座风险，避免 shell 启动文件继续介入 gate 真正执行。

## Architecture
- 当前脚本虽然已经把动态输入做了 `printf '%q'` 级别的安全拼装，但执行底座仍是：
  - `STEP_SHELL="/bin/bash"` / `"/usr/bin/zsh"`
  - `run_step() -> "$STEP_SHELL" -lc "$cmd"`
- 这意味着当前问题已经不再只是“某个参数会不会注入”：
  - 只要 login shell 启动文件有副作用，gate step 就会被额外代码介入
  - display command 和 execution truth 仍然绑定在同一段 shell 字符串上
- 这条线与刚刚完成的 Linux gate / macOS path-check 收口方向一致：
  - 展示命令继续保留
  - 真正执行改成 `(cd "$PROJECT_ROOT" && "$@")`
  - 只有 probe 这一步需要把 stdout 单独落到 JSON 文件，stderr 留在 step log

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_shell_startup_hook_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明当前 macOS gate step 执行仍会受 `ZDOTDIR/.zshenv` 干扰。
2. 最小修改 `run_wave_b_macos_gate.sh`，把 `run_step` 切到 argv 执行，并给 probe step 单独保留 stdout->JSON 的落盘语义。
3. 复跑新合同和现有 macOS gate focused contracts。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_shell_startup_hook_contract.sh
bash tests/scripts/test_wave_b_macos_gate_shell_startup_hook_contract.sh
bash tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh
bash tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `ZDOTDIR/.zshenv` 这类 shell 启动文件会在 gate step 执行时被读取
  - focused contract 会观察到外部 marker 被创建
- 修复后：
  - step 执行不再经过 login shell
  - focused contract 不再看到 shell 启动文件副作用
  - 既有 path-check / modules / examples 邻近契约继续保持 green
