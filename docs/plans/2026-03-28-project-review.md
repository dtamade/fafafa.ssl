# 2026-03-28 Project Review

## Goal
- 对当前仓库做一次 evidence-backed 的 repo scan / architecture review。
- 不改生产代码，只输出按风险排序的结论和建议。

## Architecture
- 文档与入口约定：
  - `README.md`
  - `docs/AGENTS.md`
- 默认门禁与本地验证：
  - `scripts/compile_all_modules.py`
  - `scripts/run_minimal_ci_gate.sh`
  - `scripts/check_code_style.py`
- 核心运行时入口：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.loader.pas`
  - `src/fafafa.ssl.tls.pas`
  - `src/fafafa.ssl.connection.builder.pas`

## Files
- `docs/plans/2026-03-28-project-review.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 扫描仓库结构与文档入口，确认默认 build/test/review 路径。
2. 运行默认最小门禁，确认当前树是否有 fresh baseline failure。
3. 运行仓库文档要求的风格检查，记录是否通过。
4. 审查默认门禁脚本、工厂初始化路径、OpenSSL loader 生命周期路径。
5. 输出 findings，按严重级别给出建议与后续修复顺序。

## Commands
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/check_code_style.py src`
- `rg --files -g 'AGENTS.md' -g 'docs/AGENTS.md' -g 'README*' -g '*.pas' -g '*.sh'`
- `rg -n "compile_all_modules.py|success_rate|target_rate|run_cmd|module_cmd=|phase2_cmd=|eval"`
- `nl -ba src/fafafa.ssl.factory.pas`
- `nl -ba src/fafafa.ssl.openssl.loader.pas`
- `nl -ba src/fafafa.ssl.tls.pas`

## Expected Outputs
- 当前最小门禁结果。
- 当前风格检查结果。
- 至少 3 条带文件/行号的审查结论。

## Done
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
- `python3 scripts/check_code_style.py src` => FAIL (`257` style errors)
- 审查结论已回填到 working memory。
