# Live Context ServerName Warning Quarantine

## Goal

在不改变当前 `context-level ServerName` 兼容语义的前提下，收口当前 still-live 的内部 deprecated warning，并把 compile probe 固定到真正会暴露这些 warning 的入口，避免后续继续围绕失真的旧合同反复验证。

## Architecture

- 行为边界：
  - 保留 factory / builder / backend connection constructors 现有兼容语义
  - 不删除 `context-level ServerName` fallback
  - 这批只处理内部编译 warning，不改 runtime contract
- 编译入口边界：
  - `tests/test_builder_integration.pas` 已不再稳定暴露这类 warning，不能再作为 warning contract 主入口
  - `tests/contract/test_capabilities_contract.pas` 当前会稳定编译到 `wolfssl.connection` / `mbedtls.connection` 的兼容读取点，适合作为 live warning probe
  - `WinSSL` 在当前 Linux host 上不走这条 compile path，因此改用静态 source contract 守住局部 warning quarantine

## Files

- `tests/scripts/test_internal_context_servername_warning_contract.sh`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Steps

1. 先用 live compile probe 复现当前 warning：
   - `mkdir -p tmp/internal_context_servername_warning_probe && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/internal_context_servername_warning_probe -FEtmp/internal_context_servername_warning_probe -otmp/internal_context_servername_warning_probe/test_capabilities_contract tests/contract/test_capabilities_contract.pas 2>&1 | tee tmp/internal_context_servername_warning_probe/compile.log`
2. 将 `tests/scripts/test_internal_context_servername_warning_contract.sh` 改为：
   - 编译 `tests/contract/test_capabilities_contract.pas`
   - grep `wolfssl.connection` / `mbedtls.connection` 的 deprecated `GetServerName` warning 必须不存在
   - 静态检查 `src/fafafa.ssl.winssl.connection.pas` 至少有两处局部 `{$PUSH}{$WARN ... OFF}` quarantine
   - 运行编译出的 `test_capabilities_contract`
3. 在三处 backend 兼容读取点加最小局部 quarantine：
   - `src/fafafa.ssl.wolfssl.connection.pas`
   - `src/fafafa.ssl.mbedtls.connection.pas`
   - `src/fafafa.ssl.winssl.connection.pas`
4. 验证：
   - `bash -n tests/scripts/test_internal_context_servername_warning_contract.sh`
   - `bash tests/scripts/test_internal_context_servername_warning_contract.sh`
   - `git diff --check`

## Expected Outputs

- live compile log 不再出现：
  - `fafafa.ssl.wolfssl.connection.pas ... ISSLContext.GetServerName is deprecated`
  - `fafafa.ssl.mbedtls.connection.pas ... ISSLContext.GetServerName is deprecated`
- `WinSSL` 的两处 context fallback 读取都被局部 quarantine 包裹
- `test_capabilities_contract` 仍可正常运行
- planning files 与验证报告同步更新
