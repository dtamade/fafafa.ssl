# 2026-05-15 Linux OpenSSL Matrix Shell Hardening

## Goal
收口 `scripts/run_linux_openssl_matrix_draft.sh` 的 `eval` / 字符串执行风险，避免 `--modules` 与 `--openssl3-lib-dir` 被当成 shell 语法执行。

## Architecture
- 当前脚本仍通过：
  - `run_cmd() -> eval "$cmd"`
  - `prefix="LD_LIBRARY_PATH='...'"` 字符串前缀
  - `module_cmd="bash scripts/run_all_module_tests.sh --modules $MODULE_SET"`
- 这让两条动态输入直接进入 shell 解释层：
  - `MODULE_SET`
  - `OPENSSL3_LIB_DIR`（通过 `prefix` 进入 `LD_LIBRARY_PATH=... openssl version` / module / phase2 命令串）
- 一次性复现已坐实两条真实风险：
  - `--modules "PKCS7; touch '$FLAG'; #"` 会执行 payload，且脚本仍可 `exit 0`
  - `--openssl3-lib-dir "<payload with quote break>"` 会执行 payload，且脚本仍可 `exit 0`

## Files
- `scripts/run_linux_openssl_matrix_draft.sh`
- `tests/scripts/test_linux_openssl_matrix_module_injection_contract.sh`
- `tests/scripts/test_linux_openssl_matrix_openssl3_lib_dir_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写两个 focused contracts，分别锁住 `--modules` 与 `--openssl3-lib-dir` 的注入边界。
2. 最小修改脚本，把 `eval` / 字符串执行切到 argv / env 数组。
3. 复跑新合同和相关邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_linux_openssl_matrix_module_injection_contract.sh
bash tests/scripts/test_linux_openssl_matrix_module_injection_contract.sh
bash -n tests/scripts/test_linux_openssl_matrix_openssl3_lib_dir_injection_contract.sh
bash tests/scripts/test_linux_openssl_matrix_openssl3_lib_dir_injection_contract.sh
bash -n scripts/run_linux_openssl_matrix_draft.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - modules payload 可作为 shell 语法执行
  - openssl3 lib dir payload 可从 `LD_LIBRARY_PATH=...` 字符串前缀逃逸
- 修复后：
  - 两条输入都只作为 argv / env 数据传递
  - fake nested runner / fake openssl 仍观察到完整原始值
  - `eval` 不再参与执行。
