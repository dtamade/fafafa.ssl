# TSSLLibraryDefaults Surface Adoption

## Goal

继续推进 `TSSLConfig` mixed-scope public record 的收敛，但这批只切
library-scoped defaults 这一块：

- `LogLevel`
- `LogCallback`

新增一个 additive helper surface：

- `TSSLLibraryDefaults`
- `CreateDefaultLibraryDefaults(...)`
- `GetLibraryDefaults(...)`
- `ApplyLibraryDefaults(...)`

把当前“仍然需要手动混用 `GetDefaultConfig / SetDefaultConfig` + `SetLogCallback`”
的低层组合，推进成一个显式的 public helper surface。

## Why This Batch

当前真相已经稳定：

- `LogLevel` / `LogCallback` 是 library-scoped defaults
- `TSSLFactory.CreateContext(const AConfig)` 会拒绝 request-local logging overrides
- `SetDefaultConfig(...)` 不会安装或替换 callback
- `SetLogCallback(...)` 才是 callback owner

但 public surface 仍有一个明显的设计债：

- library defaults 已经在语义上从 `TSSLConfig` 分离
- 但调用方在代码层面还得手动拼：
  - `LLib.GetDefaultConfig`
  - `LConfig.LogLevel := ...`
  - `LLib.SetDefaultConfig(LConfig)`
  - `LLib.SetLogCallback(...)`

这让 `TSSLConfig` 的 mixed-scope 债虽然“被解释清楚了”，
却还没有拿到真正独立的使用面。

## Scope

- Add:
  - `docs/plans/2026-05-21-tssllibrarydefaults-surface-adoption.md`
  - `tests/test_tssllibrarydefaults_surface.pas`
  - `tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
- Update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
  - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 在 `fafafa.ssl.base` 新增 `TSSLLibraryDefaults` record。
2. 在 `fafafa.ssl.base` 新增 3 个 helper：
   - `CreateDefaultLibraryDefaults`
   - `GetLibraryDefaults`
   - `ApplyLibraryDefaults`
3. 在 `fafafa.ssl` facade 重新导出：
   - type alias
   - helper wrappers
4. active docs 改成优先使用新 helper surface。
5. 保持底层 truth 不变：
   - `LogLevel` 仍通过 default-config round-trip 落地
   - `LogCallback` 仍通过 `SetLogCallback(...)` 安装
   - 新 helper 只是把这组组合封装成显式 public surface

## Verification

```bash
bash -n tests/scripts/test_tssllibrarydefaults_surface_contract.sh
bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh
python3 scripts/compile_all_modules.py
git diff --check
```

Focused contract 内部会验证：

- source/facade 中真的存在 `TSSLLibraryDefaults` helper surface
- API reference / ARCHITECTURE / active guides 已切到新 helper
- focused runtime probe 证明：
  - `CreateDefaultLibraryDefaults` 有正确 baseline
  - `ApplyLibraryDefaults(...)` 能同时更新 log level 和 callback
  - `GetLibraryDefaults(...)` 能读回当前 library-owned defaults

## Expected Outcome

- `TSSLConfig` mixed-scope 债里的 library-scoped 这块不再只有“解释”
- 项目正式拥有一个独立的 additive public surface：
  `TSSLLibraryDefaults`
- 后续如果继续做 `TSSLConfig` slimming，
  `LogLevel` / `LogCallback` 这组字段就不再需要依赖 raw `TSSLConfig`
  作为唯一入口

## Execution Result

- PASS
- focused RED 首轮暴露的是真实 facade compile gap：
  - `tests/test_tssllibrarydefaults_surface.pas`
    在
    `uses fafafa.ssl`
    下无法解析：
    - `TSSLLogCallback`
    - `sslLogNone`
    - `sslLogError`
    - `sslLogInfo`
- 最小修复后：
  - `src/fafafa.ssl.pas`
    已补齐：
    - `TSSLLogCallback`
    - `sslLogNone` / `sslLogError` / `sslLogWarning` / `sslLogInfo` / `sslLogDebug` / `sslLogTrace`
  - 新 helper surface 继续保持 additive：
    - `TSSLLibraryDefaults`
    - `CreateDefaultLibraryDefaults(...)`
    - `GetLibraryDefaults(...)`
    - `ApplyLibraryDefaults(...)`
- focused / gate verification：
  - `bash -n tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
    - PASS
  - `bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
    - PASS
  - `python3 scripts/compile_all_modules.py`
    - PASS
      - `186/186`
  - `git diff --check`
    - PASS
