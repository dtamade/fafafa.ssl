# 2026-03-30 Project Review

## Goal
- 对当前仓库做一次 fresh、evidence-backed 的 repo review。
- 不修改生产代码，只输出按风险排序的结论和建议，并把证据回填到 working memory。

## Architecture
- 默认验证入口：
  - `python3 scripts/check_code_style.py src`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
- 本轮重点审查路径：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.loader.pas`
  - `src/fafafa.ssl.openssl.api.aes.pas`
  - `src/fafafa.ssl.openssl.api.sha.pas`
  - `src/fafafa.ssl.openssl.api.modes.pas`
  - `scripts/compile_all_modules.py`
  - `scripts/run_minimal_ci_gate.sh`
- 契约/流程对照：
  - `docs/AGENTS.md`
  - `tests/scripts/test_check_code_style_baseline_contract.sh`
  - `tests/scripts/test_compile_all_modules_fpc_host_units_override_contract.sh`
  - `tests/scripts/test_minimal_ci_gate_fpc_host_passthrough_contract.sh`

## Files
- `docs/plans/2026-03-30-project-review.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 复核当前 dirty worktree，避免把 review 结论和用户未提交改动混淆。
2. 跑仓库当前推荐的 style gate 与 minimal gate，确认 fresh baseline。
3. 审查 factory 的并发初始化/缓存发布语义。
4. 审查 compile gate 与 minimal gate 的 fail-open / shell-eval 风险。
5. 审查 OpenSSL loader 的 loaded-state / required-symbol contract。
6. 输出按严重度排序的 findings，并给出后续收口顺序。

## Commands
- `git status --short`
- `python3 scripts/check_code_style.py src`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `nl -ba src/fafafa.ssl.factory.pas | sed -n '1,120p'`
- `nl -ba src/fafafa.ssl.factory.pas | sed -n '520,860p'`
- `nl -ba src/fafafa.ssl.factory.pas | sed -n '1150,1195p'`
- `nl -ba src/fafafa.ssl.openssl.loader.pas | sed -n '104,118p'`
- `nl -ba src/fafafa.ssl.openssl.loader.pas | sed -n '484,503p'`
- `nl -ba src/fafafa.ssl.openssl.api.aes.pas | sed -n '156,165p'`
- `nl -ba src/fafafa.ssl.openssl.api.sha.pas | sed -n '242,252p'`
- `nl -ba src/fafafa.ssl.openssl.api.modes.pas | sed -n '262,265p'`
- `nl -ba scripts/compile_all_modules.py | sed -n '263,273p'`
- `nl -ba scripts/run_minimal_ci_gate.sh | sed -n '164,209p'`

## Expected Outputs
- 当前 style gate 结果。
- 当前 minimal gate 结果。
- 至少 4 条带文件/行号的 review findings。

## Done
- `python3 scripts/check_code_style.py src` => FAIL (`257` style errors)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
- findings 已回填到 working memory。
