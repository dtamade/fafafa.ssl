# 2026-05-15 macOS OpenSSL Path Check Shell Hardening

## Goal
收口 `scripts/run_macos_openssl_path_check_draft.sh` 的 `eval` / 字符串执行风险，避免 `--modules` 与 `--openssl-root` 被当成 shell 语法执行。

## Architecture
- 当前脚本仍通过：
  - `ENV_PREFIX="..."`
  - `run_cmd() -> eval "$cmd"`
  - `module_cmd="cd '$PROJECT_ROOT' && $ENV_PREFIX bash ... --modules $MODULE_SET"`
- 这让两条动态输入都进入了 shell 解释层：
  - `MODULE_SET`
  - `OPENSSL_ROOT`（同时影响 env 前缀与后续文件检查命令）
- 现在这条风险已经不只是“脚本单体问题”：
  - 上一批已经让 `run_wave_b_macos_gate.sh --path-check-live` 开始把真实 `--openssl-root` / `--modules` 传进这个子脚本
  - 所以这个 `eval` 面已经重新回到 gate 链的真实热路径

## Files
- `scripts/run_macos_openssl_path_check_draft.sh`
- `tests/scripts/test_macos_openssl_path_check_module_injection_contract.sh`
- `tests/scripts/test_macos_openssl_path_check_openssl_root_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写两个 focused contracts，分别锁住 `--modules` 与 `--openssl-root` 的注入边界。
2. 最小修改脚本，把 `eval` / 字符串执行切到 argv / env 数组。
3. 复跑新合同和相关 macOS 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_macos_openssl_path_check_module_injection_contract.sh
bash tests/scripts/test_macos_openssl_path_check_module_injection_contract.sh
bash -n tests/scripts/test_macos_openssl_path_check_openssl_root_injection_contract.sh
bash tests/scripts/test_macos_openssl_path_check_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh
bash -n scripts/run_macos_openssl_path_check_draft.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - modules payload 可作为 shell 语法执行
  - openssl-root payload 可从 env 前缀或 test/openssl 命令串里逃逸
- 修复后：
  - 两条输入都只作为 argv / env 数据传递
  - fake nested runner / fake openssl 仍观察到完整原始值
  - `eval` 不再是执行真值来源。
