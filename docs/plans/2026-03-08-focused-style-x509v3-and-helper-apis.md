# Focused Style Wave: X509V3 + Helper APIs (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中当前 `openssl.api.x509v3`、`capability.diff`、`openssl.api.sha3` 的 20 处高信号缩进问题收敛到 0。
- 把两个 focused style contracts 纳入 `tests/scripts/test_repo_hygiene_contract_batch.sh`，避免同类回归。

## Scope
- `src/fafafa.ssl.openssl.api.x509v3.pas`
- `src/fafafa.ssl.capability.diff.pas`
- `src/fafafa.ssl.openssl.api.sha3.pas`
- `tests/scripts/test_focused_style_contract_x509v3.sh`
- `tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增两个 focused style contracts：
  - `openssl.api.x509v3`
  - `capability.diff` + `openssl.api.sha3`
- 先用合同脚本制造 RED，再最小修改 20 处 continuation-line 奇数缩进，不改变运行时语义。
- 最后将新合同接入 repo hygiene batch，并执行 focused + compile + minimal gate 回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_x509v3.sh`
   - `tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_x509v3.sh`
   - `bash tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
   - Expected: FAIL，提示目标文件仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.openssl.api.x509v3.pas`
   - `src/fafafa.ssl.capability.diff.pas`
   - `src/fafafa.ssl.openssl.api.sha3.pas`
2. Add the new focused style contracts to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_x509v3.sh`
- `bash tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_x509v3.sh`
  - `tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_x509v3.sh` => FAIL
  - Key failure: `src/fafafa.ssl.openssl.api.x509v3.pas:301 缩进不是 2 空格倍数`
  - `bash tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh` => FAIL
  - Key failure: `src/fafafa.ssl.capability.diff.pas:316 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.openssl.api.x509v3.pas`
  - `src/fafafa.ssl.capability.diff.pas`
  - `src/fafafa.ssl.openssl.api.sha3.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added focused contract wiring:
  - `tests/scripts/test_focused_style_contract_x509v3.sh`
  - `tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh`
- Reduced the targeted style slice by 20 odd-indentation findings.

### Regression
- `bash tests/scripts/test_focused_style_contract_x509v3.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but total errors reduced from `85` to `65` and target files are now clean
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
