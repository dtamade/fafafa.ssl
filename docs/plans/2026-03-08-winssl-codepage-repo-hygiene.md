# WinSSL CODEPAGE + Repo Hygiene Batch (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中当前两个明确的 WinSSL `{$CODEPAGE UTF8}` 告警收敛到 0。
- 把该约束纳入 `tests/scripts/test_repo_hygiene_contract_batch.sh`，避免告警回归。

## Scope
- `src/fafafa.ssl.winssl.session.pas`
- `src/fafafa.ssl.winssl.native_handle.pas`
- `tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增一个 focused repo hygiene contract，只检查 style checker 输出中的两个 WinSSL `CODEPAGE` 告警。
- 先用合同脚本制造 RED，再最小修改两个 WinSSL 单元增加 `{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}`。
- 最后将新合同接入 repo hygiene batch，并运行 batch + minimal gate 回归。

## RED
1. Add `tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`
2. Run:
   - `bash tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`
   - Expected: FAIL，提示两个 WinSSL 单元仍有 `CODEPAGE` 告警。

## GREEN
1. Add `{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}` to:
   - `src/fafafa.ssl.winssl.session.pas`
   - `src/fafafa.ssl.winssl.native_handle.pas`
2. Add the new contract to repo hygiene batch + coverage contract.

## Regression
- `bash tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `python3 scripts/check_code_style.py src`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added `tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`.
- RED run:
  - `bash tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh`
  - Result: FAIL
  - Key failure: `src/fafafa.ssl.winssl.session.pas 是 Windows 文件但缺少 {$CODEPAGE UTF8}`

### GREEN
- Updated:
  - `src/fafafa.ssl.winssl.session.pas`
  - `src/fafafa.ssl.winssl.native_handle.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added conditional `{$CODEPAGE UTF8}` to the two WinSSL units.
- Wired the new contract into the repo hygiene batch and coverage contract.

### Regression
- `bash tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but WinSSL `CODEPAGE` warnings reduced from `2` to `0`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
