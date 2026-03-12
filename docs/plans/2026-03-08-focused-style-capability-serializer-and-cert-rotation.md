# Focused Style Wave: Capability Serializer + Cert Rotation (2026-03-08)

## Goal
- 把 `scripts/check_code_style.py src` 中当前 `capability.serializer`、`cert.rotation` 的 20 处高信号缩进问题收敛到 0。
- 把两个 focused style contracts 纳入 `tests/scripts/test_repo_hygiene_contract_batch.sh`，避免同类回归。

## Scope
- `src/fafafa.ssl.capability.serializer.pas`
- `src/fafafa.ssl.cert.rotation.pas`
- `tests/scripts/test_focused_style_contract_capability_serializer.sh`
- `tests/scripts/test_focused_style_contract_cert_rotation.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增两个 focused style contracts：
  - `capability.serializer`
  - `cert.rotation`
- 先用合同脚本制造 RED，再最小修改 20 处 continuation-line 奇数缩进，不改变运行时语义。
- 最后将新合同接入 repo hygiene batch，并执行 focused + compile + minimal gate 回归。

## RED
1. Add:
   - `tests/scripts/test_focused_style_contract_capability_serializer.sh`
   - `tests/scripts/test_focused_style_contract_cert_rotation.sh`
2. Run:
   - `bash tests/scripts/test_focused_style_contract_capability_serializer.sh`
   - `bash tests/scripts/test_focused_style_contract_cert_rotation.sh`
   - Expected: FAIL，提示目标文件仍有 style issue。

## GREEN
1. Fix odd indentation in:
   - `src/fafafa.ssl.capability.serializer.pas`
   - `src/fafafa.ssl.cert.rotation.pas`
2. Add the new focused style contracts to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_focused_style_contract_capability_serializer.sh`
- `bash tests/scripts/test_focused_style_contract_cert_rotation.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `python3 scripts/check_code_style.py src`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_focused_style_contract_capability_serializer.sh`
  - `tests/scripts/test_focused_style_contract_cert_rotation.sh`
- RED runs:
  - `bash tests/scripts/test_focused_style_contract_capability_serializer.sh` => FAIL
  - Key failure: `src/fafafa.ssl.capability.serializer.pas:29 缩进不是 2 空格倍数`
  - `bash tests/scripts/test_focused_style_contract_cert_rotation.sh` => FAIL
  - Key failure: `src/fafafa.ssl.cert.rotation.pas:244 缩进不是 2 空格倍数`

### GREEN
- Updated:
  - `src/fafafa.ssl.capability.serializer.pas`
  - `src/fafafa.ssl.cert.rotation.pas`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Added focused contract wiring:
  - `tests/scripts/test_focused_style_contract_capability_serializer.sh`
  - `tests/scripts/test_focused_style_contract_cert_rotation.sh`
- Reduced the targeted style slice by 20 odd-indentation findings.

### Regression
- `bash tests/scripts/test_focused_style_contract_capability_serializer.sh` => PASS
- `bash tests/scripts/test_focused_style_contract_cert_rotation.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh` => PASS
- `python3 scripts/check_code_style.py src` => FAIL overall, but total errors reduced from `65` to `45` and target files are now clean
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
