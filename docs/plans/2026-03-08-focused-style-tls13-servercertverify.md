# Focused Style Wave: TLS13 Server CertificateVerify (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中 `src/fafafa.ssl.tls13.servercertverify.pas` 的 13 处高密度缩进问题收敛到 0。
- 增加 focused style contract，锁定该文件不再回归。

## Scope
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `tests/scripts/test_focused_style_contract_tls13_servercertverify.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 先加单文件 focused style contract，验证当前 RED。
- 仅修复 continuation-line 奇数缩进，不改控制流和签名逻辑。
- 然后把新合同接入 repo hygiene batch，并执行编译/门禁回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_tls13_servercertverify.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_tls13_servercertverify.sh`
   - Expected: FAIL，提示 `src/fafafa.ssl.tls13.servercertverify.pas` 仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.tls13.servercertverify.pas`
2. Add the new focused style contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_tls13_servercertverify.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_tls13_servercertverify.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_tls13_servercertverify.sh` => FAIL
  - Key failure: `src/fafafa.ssl.tls13.servercertverify.pas:478 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.tls13.servercertverify.pas`
- Cleared 13 targeted odd-indentation findings in the TLS13 server CertificateVerify helper.
- During regression, `tests/scripts/test_focused_compile_zero_noise_contract.sh` surfaced 5 existing `managed type result` warnings in the same file.
- Root cause: several `TBytes`-returning functions used `SetLength(Result, ...)` before explicit `Result := nil`, which Free Pascal warns about under the repo's zero-noise contract.
- Added minimal `Result := nil;` initialization to the affected `TBytes`-returning helpers in the same file.

### Regression
- `bash -n tests/scripts/test_focused_style_contract_tls13_servercertverify.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_tls13_servercertverify.sh` => PASS
- `bash tests/scripts/test_focused_compile_zero_noise_contract.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but target file is now clean and total errors dropped from `45` to `32`
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
