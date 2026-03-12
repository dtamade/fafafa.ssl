# Focused Style Batch: WinSSL Context + Connection (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中当前 `src/fafafa.ssl.winssl.context.pas` 与 `src/fafafa.ssl.winssl.connection.pas` 的 7 处高信号缩进问题收敛到 0。
- 把该 focused style contract 纳入 `tests/scripts/test_repo_hygiene_contract_batch.sh`，避免同类回归。

## Scope
- `src/fafafa.ssl.winssl.context.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增一个 focused style contract，只检查 style checker 输出中的两个 WinSSL 目标文件。
- 先用合同脚本制造 RED，再最小修改 7 处 continuation-line 奇数缩进，不改变运行时语义。
- 最后将新合同接入 repo hygiene batch，并执行 focused + compile + minimal gate 回归。

## RED
1. Add `tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
   - Expected: FAIL，提示 `src/fafafa.ssl.winssl.context.pas` 仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.winssl.context.pas`
   - `src/fafafa.ssl.winssl.connection.pas`
2. Add the new focused style contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added `tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`.
- RED run:
  - `bash tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
  - Result: FAIL
  - Key failure: `src/fafafa.ssl.winssl.context.pas:482 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.winssl.context.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added focused contract wiring:
  - `tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh`
- Reduced the targeted WinSSL style slice by 7 odd-indentation findings.

### Regression
- `bash tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but total errors reduced from `103` to `96` and target files are now clean
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
