# Library Default LogCallback Detachment

## Goal

把 `TSSLConfig.LogCallback` 从 `ISSLLibrary.SetDefaultConfig(...)` 的 live write surface 上进一步剥离，让 runtime 真相和当前公开语义重新对齐：

- `LogLevel` 继续通过 `GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整
- `LogCallback` 只通过 `ISSLLibrary.SetLogCallback(...)` 安装/替换

## Why This Batch

当前文档、migration map、active guides 已经把 logging owner 说清楚了：

- `LogLevel` = library default config
- `LogCallback` = dedicated callback setter

但 5 个 backend 的 `SetDefaultConfig(...)` 仍然会直接吃掉 `LConfig.LogCallback`，这让：

- `LogCallback` 仍像是 `TSSLConfig` 正常主写入口
- `SetDefaultConfig(...)` 和 `SetLogCallback(...)` 继续共享 callback owner

这属于真实 interface/implementation drift，不只是 wording 问题。

## Deliverables

1. 让 focused logging runtime 回归先 RED，证明 `SetDefaultConfig(LogCallback)` 仍会安装回调
2. 增加 source contract，钉住 5 个 backend 不再从 `LConfig.LogCallback` 回写 `FLogCallback`
3. production fix 改成：
   - `SetDefaultConfig(...)` 只更新 `LogLevel` 和其他 default-config 字段
   - callback state 只由 `SetLogCallback(...)` 维护
4. 同步修正仍在测试里通过 `DefaultConfig.LogCallback := ...` 安装库回调的 focused coverage
5. 更新 planning files 和记账证据

## Files

- `docs/plans/2026-05-18-library-default-logcallback-detachment.md`
- `tests/test_factory_logging_scope_clarification.pas`
- `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
- `tests/test_freepascal_library_default_config_server_name_clarification.pas`
- `tests/test_openssl_library_default_config_server_name_clarification.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_library_default_logcallback_detachment_contract.sh
bash tests/scripts/test_library_default_logcallback_detachment_contract.sh
mkdir -p tmp/test_factory_logging_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_factory_logging_scope_clarification \
  -FEtmp/test_factory_logging_scope_clarification \
  -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification \
  tests/test_factory_logging_scope_clarification.pas && \
  ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification
git diff --check
```

## Expected Outcome

- `TSSLConfig.LogCallback` 继续保留在 `v1.x` record 里，但不再作为 active library callback owner
- `SetDefaultConfig(...)` / `SetLogCallback(...)` 的职责边界重新单一
- 下一批若继续做 `TSSLConfig` slimming，不需要再先澄清 logging callback 到底归谁管
