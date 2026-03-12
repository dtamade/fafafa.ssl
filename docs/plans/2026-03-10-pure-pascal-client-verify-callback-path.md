# 2026-03-10 pure Pascal client verify callback path

## Goal
- 让 pure Pascal / FreePascal 客户端真正消费 `ISSLContext.SetVerifyCallback(...)`。
- 收口“接口已暴露但运行路径不生效”的 completeness 缺口。

## Architecture
- 先写 scripted client-handshake RED：
  - trusted + hostname match + callback 返回 `False` 时，`Connect` 必须失败
  - hostname mismatch + callback 返回 `True` 时，`Connect` 必须成功
- 然后最小扩展 pure Pascal 后握手验证：
  - 复用现有 post-handshake validation 入口
  - 暴露 context verify callback getter
  - 在 built-in verification 结果之后调用 callback
- 最后回归当前 pure Pascal client M1 focused suites。

## Files
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_verify_callback_path.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_verify_callback_path.pas -otmp/test_fp_client_verify_callback && ./tmp/test_fp_client_verify_callback`
- Expected:
  - pure Pascal 当前还没消费 verify callback，因此 veto / override 断言失败

## GREEN
- 只把 `SetVerifyCallback(...)` 接到当前 client post-handshake validation 主线。
- 不把这波扩大成 pinning / info-callback / session-resumption 实现包。

## Verification
- `fpc -Fu./src tests/test_freepascal_client_verify_callback_path.pas -otmp/test_fp_client_verify_callback && ./tmp/test_fp_client_verify_callback`
- `fpc -Fu./src tests/test_freepascal_client_custom_ca_sources_path.pas -otmp/test_fp_client_custom_ca && ./tmp/test_fp_client_custom_ca`
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- `fpc -Fu./src tests/test_freepascal_client_peer_certificate_foundation.pas -otmp/test_fp_client_peer_cert && ./tmp/test_fp_client_peer_cert`
- `python3 -u scripts/compile_all_modules.py`
