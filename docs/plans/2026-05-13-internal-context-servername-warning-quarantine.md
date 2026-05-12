# Internal Context ServerName Warning Quarantine

## Goal

收口内部兼容路径上的 `context-level ServerName` 弃用 warning，避免 `factory` / `builder` / `OpenSSL` 兼容回填逻辑在 focused 编译里持续刷出已知噪音。

## Architecture

- 保持当前兼容行为不变：
  - `TSSLConfig.ServerName` 和 `TSSLContextBuilder.WithSNI(...)` 仍然会回填到 context-level `ServerName`
  - `TOpenSSLConnection` 仍然会从 context fallback 继承默认 `ServerName`
- 这批只收编译 warning，不改 public contract，也不改变 runtime role / SNI 行为。
- 先用 focused compile contract 锁住 warning 现状，再在局部兼容调用点用最小 `{$PUSH}{$WARN ... OFF}` / `{$POP}` 隔离。

## Files

- Add: `tests/scripts/test_internal_context_servername_warning_contract.sh`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 新增 focused compile contract：
   - 编译 `tests/test_builder_integration.pas`
   - grep 当前不应再出现的 4 组 warning：
     - `fafafa.ssl.factory.pas` 的 `ISSLContext.SetServerName`
     - `fafafa.ssl.context.builder.pas` 的 `ISSLContext.SetServerName`
     - `fafafa.ssl.openssl.connection.pas` 的 `ISSLContext.GetServerName`
     - `fafafa.ssl.openssl.backed.pas` 的 `ISSLContext.Get/SetServerName`
2. 在上述 4 个文件的兼容调用点补局部 warning quarantine：
   - 用 `{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}` 包住兼容调用
   - 紧邻调用点 `{$POP}`，不扩大到整段逻辑外
3. 跑 focused GREEN：
   - `bash -n tests/scripts/test_internal_context_servername_warning_contract.sh`
   - `bash tests/scripts/test_internal_context_servername_warning_contract.sh`
   - `./tmp/internal_context_servername_warning_contract/test_builder_integration`
4. 跑仓库级验证：
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`

## Expected Outputs

- focused contract 从 RED 变为 PASS
- `tests/test_builder_integration.pas` 运行仍然 PASS
- compile gate 不再为这 4 个内部兼容调用面打印 deprecated `ServerName` warning
