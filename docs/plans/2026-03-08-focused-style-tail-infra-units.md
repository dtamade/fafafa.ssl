# Focused Style Wave: Tail Infra Units (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 剩余的 12 处缩进问题全部收敛到 0。
- 增加一个 tail focused style contract，锁定这批基础设施尾部单元不再回归。

## Scope
- `src/fafafa.ssl.aesgcm.pool.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.native_handle.pas`
- `src/fafafa.ssl.pkcs11.types.pas`
- `src/fafafa.ssl.http.client.pas`
- `tests/scripts/test_focused_style_contract_tail_infra_units.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增一个 tail focused style contract，检查 style checker 输出中的六个目标文件。
- 先用合同脚本制造 RED，再最小修改 12 处 continuation-line / alignment 奇数缩进，不改变运行时语义。
- 最后将新合同接入 repo hygiene batch，并执行 focused + compile + minimal gate 回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_tail_infra_units.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_tail_infra_units.sh`
   - Expected: FAIL，提示目标文件仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.aesgcm.pool.pas`
   - `src/fafafa.ssl.freepascal.connection.pas`
   - `src/fafafa.ssl.freepascal.context.pas`
   - `src/fafafa.ssl.native_handle.pas`
   - `src/fafafa.ssl.pkcs11.types.pas`
   - `src/fafafa.ssl.http.client.pas`
2. Add the new focused style contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_tail_infra_units.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_tail_infra_units.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_tail_infra_units.sh` => FAIL
  - Key failure: `src/fafafa.ssl.aesgcm.pool.pas:247 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.aesgcm.pool.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.native_handle.pas`
  - `src/fafafa.ssl.pkcs11.types.pas`
  - `src/fafafa.ssl.http.client.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added focused contract wiring:
  - `tests/scripts/test_focused_style_contract_tail_infra_units.sh`
- Reduced the targeted style slice by the final 12 odd-indentation findings.

### Regression
- `bash -n tests/scripts/test_focused_style_contract_tail_infra_units.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_tail_infra_units.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => PASS (`0` errors / `0` warnings)
- Initial parallel validation exposed `pkcs11` warning debt in `--fast-local`; see linked follow-up plan below.
