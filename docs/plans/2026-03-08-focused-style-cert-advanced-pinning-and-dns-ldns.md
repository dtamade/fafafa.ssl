# Focused Style Wave: Cert Advanced + Cert Pinning + DNS ldns (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中 `cert.advanced`、`cert.pinning`、`dns.ldns` 的 12 处缩进问题收敛到 0。
- 增加一个 themed focused style contract，锁定这组三文件不再回归。

## Scope
- `src/fafafa.ssl.cert.advanced.pas`
- `src/fafafa.ssl.cert.pinning.pas`
- `src/fafafa.ssl.dns.ldns.pas`
- `tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增一个 themed focused style contract，检查 style checker 输出中的三份目标文件。
- 先用合同脚本制造 RED，再最小修改 12 处 continuation-line 奇数缩进，不改变运行时语义。
- 最后将新合同接入 repo hygiene batch，并执行 focused + compile + minimal gate 回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
   - Expected: FAIL，提示目标文件仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.cert.advanced.pas`
   - `src/fafafa.ssl.cert.pinning.pas`
   - `src/fafafa.ssl.dns.ldns.pas`
2. Add the new focused style contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh` => FAIL
  - Key failure: `src/fafafa.ssl.cert.advanced.pas:202 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.cert.advanced.pas`
  - `src/fafafa.ssl.cert.pinning.pas`
  - `src/fafafa.ssl.dns.ldns.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added focused contract wiring:
  - `tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh`
- Reduced the targeted style slice by 12 odd-indentation findings.

### Regression
- `bash -n tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but reduced to `12` errors / `0` warnings
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
