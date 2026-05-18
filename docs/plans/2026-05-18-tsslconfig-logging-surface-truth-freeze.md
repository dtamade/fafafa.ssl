# TSSLConfig Logging Surface Truth Freeze

## Goal

把 `TSSLConfig.LogLevel` / `LogCallback` 这条已经完成 runtime/factory scope clarification 的线，再往前收一刀到 active docs/guidance truth，避免调用方继续从活跃文档学到“只设 callback 就能看到 info/debug 日志”的错误用法。

## Why This Batch

当前 production truth 已经稳定：

- `LogLevel` / `LogCallback` 是 library-scoped defaults
- `TSSLFactory.CreateContext(const AConfig)` 会拒绝 request-local logging overrides
- `CreateDefaultConfig(...)` 会把 request-safe logging baseline 清成 `sslLogError` + `nil`
- backend `Log(...)` 只会在 `ALevel <= configured LogLevel` 时分发给 callback

但 active docs 里还残留一个高可见度 drift：

- `docs/guides/USER_GUIDE.md`
- `docs/guides/TROUBLESHOOTING.md`

它们当前只演示 `ISSLLibrary.SetLogCallback(...)`，却立刻调用 `LLib.Log(sslLogInfo, ...)`。这和当前默认 `LogLevel = sslLogError` 的 runtime truth 冲突，会让用户学到一个实际不会触发回调的示例。

## Deliverables

1. 修正活跃 guide/reference 中的 logging guidance
2. 明确 `LogLevel` 走 library default config，`LogCallback` 走 `SetLogCallback(...)`
3. 补 focused shell contract，防止这条 drift 回流
4. 复用现有 logging-focused Pascal tests，确认 runtime/source truth 没被文档收口扰动

## Scope

- 只收 source/doc/test truth
- 不改 runtime logging owner model
- 不新增 per-context / per-connection logging API

## Files

- `docs/plans/2026-05-18-tsslconfig-logging-surface-truth-freeze.md`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/ARCHITECTURE.md`
- `docs/guides/USER_GUIDE.md`
- `docs/guides/TROUBLESHOOTING.md`
- `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh
bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh
mkdir -p tmp/test_factory_logging_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_factory_logging_scope_clarification \
  -FEtmp/test_factory_logging_scope_clarification \
  -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification \
  tests/test_factory_logging_scope_clarification.pas && \
  ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification
mkdir -p tmp/test_default_config && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_default_config \
  -FEtmp/test_default_config \
  -otmp/test_default_config/test_default_config \
  tests/config/test_default_config.pas && \
  ./tmp/test_default_config/test_default_config
git diff --check
```

## Expected Outcome

- active docs 不再把 logging callback 当成足以打开 info/debug 输出的完整配置
- library-default logging truth 在 docs/reference/guides 中说同一种话
- 以后继续做 `TSSLConfig` slimming 时，不需要再重查这条 logging guidance drift
