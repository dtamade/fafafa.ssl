# v1.5.0 Static Audit Inventory Refresh

## Goal

修复 `v1.5.0` 静态审计合同与静态审计页的 current-head inventory drift：

- `tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh`
  仍硬编码 `197` 个 `src/fafafa.ssl*.pas`
- 当前 `master` 实际已经有 `198` 个单元
- 新增项是 `src/fafafa.ssl.context.config.pas`

本批只刷新静态审计 truth，不改生产 Pascal 源码。

## Scope

- Update:
  - `docs/test_reports/STATIC_AUDIT_V1.5.0.md`
  - `tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Current Evidence

- 当前 `src/fafafa.ssl*.pas` inventory：
  - total = `198`
  - compile (Linux compile sieve) = `186`
  - skip = `12`
- 按 `v1.5.0` tag 对比：
  - published tag = `197`
  - current head 新增：
    - `src/fafafa.ssl.context.config.pas`
- `scripts/compile_all_modules.py`
  当前 skip 规则仍然只排除 WinSSL 与显式 deprecated/example seams，
  所以新增单元也进入 Linux compile sieve。

## Minimal Fix

1. 把 `STATIC_AUDIT_V1.5.0.md` 从旧 inventory truth 刷新到 current head：
   - `198 tracked src/*.pas files`
   - `186 core modules`
   - `12 WinSSL-only skips`
   - 明确记录新增单元是 `src/fafafa.ssl.context.config.pas`
2. 把静态审计合同从只盯总数，
   收紧成同时验证：
   - total
   - compile
   - skip
   - static audit page wording
3. focused rerun，确认这次红灯确实只是 stale audit truth，而不是别的 release drift。

## Verification

```bash
bash tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh
git diff --check
```

## Expected Outcome

- `v1.5.0` static audit page 回到 current-head truth
- contract 不再依赖过时 inventory 数字
- 下次如果 `src/` 清单继续变化，会直接给出更精确的静态红灯

## Outcome

- PASS
- `docs/test_reports/STATIC_AUDIT_V1.5.0.md` 已记录 current-head inventory truth。
- `bash tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh` 在当前 head 上通过。
