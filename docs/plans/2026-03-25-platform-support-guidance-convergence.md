# Platform support guidance convergence（2026-03-25）

## Goal

- 收口 `docs/PLATFORM_SUPPORT.md` 中仍然使用旧 Linux/macOS 构建脚本作为默认入口的 drift。
- 让这页平台总览与当前工程口径一致：
  - Linux 默认验证入口对齐 canonical chain
  - macOS 不再把历史脚本当默认入口，而是给出当前 focused smoke 起点
  - Windows 继续保留当前真实可用的 PowerShell 脚本路径

## Architecture

- 这是一个 docs-only family，范围只限 `docs/PLATFORM_SUPPORT.md`。
- 不改 `build_linux.sh` / `build_macos.sh` 实现；只改变这页文档如何引导读者。
- 保持平台语义清晰，不把 Linux baseline 误写成跨平台统一全量 gate。

## Files

- `docs/plans/2026-03-25-platform-support-guidance-convergence.md`
- `docs/PLATFORM_SUPPORT.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 复扫 `docs/PLATFORM_SUPPORT.md` 中的旧入口命中：
   - `build_linux.sh`
   - `run_core_tests.sh`
   - `build_macos.sh`
2. 在文档顶部补 `当前工程验证入口`：
   - current Wave C closeout/current-chain
   - Linux canonical command chain
   - multi-platform workflow 仍以 template/manual 为主
3. 更新 Linux section：
   - 把默认编译和测试改成 `compile_all_modules.py` / `run_minimal_ci_gate.sh --fast-local`
4. 更新 macOS section：
   - 移除 `build_macos.sh` 作为默认入口
   - 改成 focused compile smoke (`tests/openssl/test_openssl_simple.pas`)
   - 明确 full multi-platform workflow 仍是 template/manual 状态
5. 更新 `task_plan.md` / `findings.md` / `progress.md`，记录本批范围和验证。

## Commands

```bash
rg -n "build_linux\\.sh|run_core_tests\\.sh|build_macos\\.sh" docs/PLATFORM_SUPPORT.md
rg -n "python3 scripts/compile_all_modules\\.py|bash scripts/run_minimal_ci_gate\\.sh --fast-local|tests/openssl/test_openssl_simple\\.pas|WAVE_C_CLOSEOUT_STATUS_2026-03-18|WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16" docs/PLATFORM_SUPPORT.md
git diff --check -- docs/plans/2026-03-25-platform-support-guidance-convergence.md docs/PLATFORM_SUPPORT.md task_plan.md findings.md progress.md
```

## Expected Outputs

- `docs/PLATFORM_SUPPORT.md` 不再把 `build_linux.sh` / `run_core_tests.sh` / `build_macos.sh` 当默认入口。
- Linux 段落对齐当前 canonical chain。
- macOS 段落不再把历史脚本当成默认路径，而是给出当前 focused smoke 起点。
