# Focused Style Wave: OpenSSL TS API (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中 `src/fafafa.ssl.openssl.api.ts.pas` 的 8 处缩进问题收敛到 0。
- 增加 focused style contract，锁定该文件不再回归。

## Scope
- `src/fafafa.ssl.openssl.api.ts.pas`
- `tests/scripts/test_focused_style_contract_openssl_api_ts.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 先加单文件 focused style contract，验证当前 RED。
- 仅修复 continuation-line 奇数缩进，不改 RFC3161/TS 逻辑。
- 然后把新合同接入 repo hygiene batch，并执行编译/门禁回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_openssl_api_ts.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_openssl_api_ts.sh`
   - Expected: FAIL，提示 `src/fafafa.ssl.openssl.api.ts.pas` 仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.openssl.api.ts.pas`
2. Add the new focused style contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_openssl_api_ts.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_openssl_api_ts.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_openssl_api_ts.sh` => FAIL
  - Key failure: `src/fafafa.ssl.openssl.api.ts.pas:612 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.openssl.api.ts.pas`
- Cleared 8 targeted odd-indentation findings in the OpenSSL TS API unit.
- Wired the new focused contract into:
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

### Regression
- `bash -n tests/scripts/test_focused_style_contract_openssl_api_ts.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_openssl_api_ts.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but target file is now clean and total errors dropped from `32` to `24`
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
