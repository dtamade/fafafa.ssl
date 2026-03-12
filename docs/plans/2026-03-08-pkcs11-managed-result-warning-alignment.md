# PKCS11 Managed-Result Warning Alignment (2026-03-08)

## Goal
- 消除 `fast-local` 在 `pkcs11.types` / `pkcs11.uri` 上暴露的 zero-noise compile warnings。
- 保持行为不变，只做显式初始化和枚举分支显式化。

## Scope
- `src/fafafa.ssl.pkcs11.types.pas`
- `src/fafafa.ssl.pkcs11.uri.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 对 `TBytes` 返回函数使用 `Result := nil;`。
- 对带托管字段的 record 返回使用 `Result := Default(...)`，替换 `FillChar(Result, ...)`。
- 为 `TPKCS11PINMethod` 的 `case` 增加显式 `pmNone` 分支，避免非穷举 warning。

## Regression
- `bash tests/scripts/test_focused_compile_zero_noise_contract.sh`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### Root Cause
- `fast-local` 首次回归在 `tests/scripts/test_focused_compile_zero_noise_contract.sh` 中暴露 5 个 warning：
  - `src/fafafa.ssl.pkcs11.types.pas:268`
  - `src/fafafa.ssl.pkcs11.types.pas:383`
  - `src/fafafa.ssl.pkcs11.types.pas:430`
  - `src/fafafa.ssl.pkcs11.uri.pas:124`
  - `src/fafafa.ssl.pkcs11.uri.pas:208`
- 具体原因：
  - `TBytes` 返回值在首次 `SetLength(Result, ...)` 前未显式初始化。
  - `FillChar(Result, ...)` 用在包含 `string` / `TBytes` / 回调字段的 record 返回值上。
  - `TPKCS11Config.GetPIN` 的 `case PINMethod of` 漏掉 `pmNone`。

### GREEN
- Updated:
  - `src/fafafa.ssl.pkcs11.types.pas`
  - `src/fafafa.ssl.pkcs11.uri.pas`
- Minimal fixes:
  - `Result := nil;` for `TBytes` return builders
  - `Result := Default(TPKCS11Config)` / `Default(TPKCS11URI)` for managed records
  - explicit `pmNone` branch in `GetPIN`

### Regression
- `bash tests/scripts/test_focused_compile_zero_noise_contract.sh` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
