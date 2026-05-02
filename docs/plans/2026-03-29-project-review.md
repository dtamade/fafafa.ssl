# 2026-03-29 Project Review

## Goal
- 对当前仓库做一次 fresh, evidence-backed 的 repo review。
- 不修改生产代码，只输出按风险排序的结论和建议，并回填 working memory。

## Architecture
- 默认验证入口：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `python3 scripts/check_code_style.py src`
- 本轮重点审查路径：
  - `src/fafafa.ssl.context.builder.pas`
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.loader.pas`
  - `src/fafafa.ssl.openssl.api.aes.pas`
  - `src/fafafa.ssl.openssl.api.sha.pas`
  - `src/fafafa.ssl.openssl.api.modes.pas`
  - `src/fafafa.ssl.openssl.context.pas`
  - `src/fafafa.ssl.winssl.context.pas`
  - `src/fafafa.ssl.mbedtls.context.pas`
- 契约/文档对照：
  - `docs/guides/GETTING_STARTED.md`
  - `docs/reference/API_INVENTORY.md`
  - `tests/config/test_config_validation.pas`

## Files
- `docs/plans/2026-03-29-project-review.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 恢复上一轮 review 上下文，确认当前 worktree 和默认入口。
2. 跑仓库要求的最小门禁与 style gate，记录 fresh baseline。
3. 审查 context builder 的 system-roots / backend-selection 路径。
4. 审查 OpenSSL loader / module-loaded 契约，找出“宣称已加载但未验证核心符号”的路径。
5. 输出按严重程度排序的 findings，并给出后续修复顺序。

## Commands
- `git status --short`
- `python3 scripts/check_code_style.py src`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `rg -n "WithSystemRoots|BuildClient|BuildServer|ValidateServer|CreateCertificateStore\\(sslAutoDetect\\)" src/fafafa.ssl.context.builder.pas tests/config/test_config_validation.pas docs/guides/GETTING_STARTED.md docs/reference/API_INVENTORY.md`
- `rg -n "LoadFunctions\\(|SetModuleLoaded\\(|Required: True" src/fafafa.ssl.openssl.loader.pas src/fafafa.ssl.openssl.api.aes.pas src/fafafa.ssl.openssl.api.sha.pas src/fafafa.ssl.openssl.api.modes.pas`
- `nl -ba src/fafafa.ssl.openssl.context.pas`
- `nl -ba src/fafafa.ssl.winssl.context.pas`
- `nl -ba src/fafafa.ssl.mbedtls.context.pas`

## Expected Outputs
- 当前 minimal gate 结果。
- 当前 style gate 结果。
- 至少 3 条带文件/行号的审查结论。

## Done
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
- `python3 scripts/check_code_style.py src` => FAIL (`257` style errors)
- review findings 已回填到 working memory。
