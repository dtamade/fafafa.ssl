# Wave B / B2: macOS 模块测试超时工具可移植性修复

## Goal
- 修复 Wave B macOS gate 在 `modules` 步骤 **全量失败**的问题，使 B2 闭环推进不再被脚本环境差异阻塞。

## Background / Evidence
- GitHub Actions（macOS runner）在 2026-03-13 的 live gate 里：
  - `overall: FAIL`，原因是 `modules` 步骤失败（其余步骤 PASS）。
  - 模块测试日志显示所有测试退出码均为 `127`（命令不存在）。
- 根因：`scripts/run_all_module_tests.sh` 运行测试时依赖 `timeout`（GNU coreutils），而 **macOS 默认没有 `timeout`**。

## Architecture / Fix
- 在 `scripts/run_all_module_tests.sh` 中引入可移植的超时执行器：
  1) 优先使用 `timeout`（Linux）或 `gtimeout`（macOS + coreutils）
  2) 若不可用/不可执行，则回退到 `python3 subprocess.run(..., timeout=...)`
- 同时将失败路径的 `grep -P` 解析替换为 `awk`，避免 BSD grep 不支持 PCRE 导致额外噪音。

## Files
- Modify: `scripts/run_all_module_tests.sh`
- Add: `tests/scripts/test_run_all_module_tests_timeout_portability_contract.sh`

## Step-by-step (Local)
1) 先跑 contract（模拟坏掉的 `timeout`）
   - `bash tests/scripts/test_run_all_module_tests_timeout_portability_contract.sh`
2) 回归模块测试（最小集合）
   - `bash scripts/run_all_module_tests.sh --modules PKCS7 --fast-local`
3) （可选）最小门禁回归
   - `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Step-by-step (CI / Runner)
1) 触发 `.github/workflows/wave-b-b2-manual.yml` 的 `workflow_dispatch`
2) 预期：macOS gate summary 中 `modules` 为 PASS，overall 为 PASS
3) 生成的 `wave_b_b2_closure_readiness_*.md` / `wave_b_b2_evidence_consistency_*.md` 应不再因 macOS modules 阻塞

## Expected Outputs / Acceptance
- `tests/scripts/test_run_all_module_tests_timeout_portability_contract.sh` => PASS
- `scripts/run_all_module_tests.sh --modules PKCS7 --fast-local` => PASS
- macOS Wave B gate：`wave_b_macos_gate_summary_<run_id>.md` 中 `modules`=PASS，整体 `overall`=PASS

## Next Queue
- 复跑 B2 manual workflow，若 Windows 仍未回填或失败，优先修 Windows gate（保持三平台闭环口径一致）。
