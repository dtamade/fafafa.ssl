# 2026-03-10 pure Pascal client info callback path

## Goal
- 让 pure Pascal / FreePascal 客户端真正消费 `ISSLContext.SetInfoCallback(...)`。
- 收口“接口公开但 runtime 不发事件”的 completeness 缺口。

## Architecture
- 先写 scripted client-handshake RED：
  - 成功握手必须至少产生 `handshake_start` 与 `handshake_done`
  - hostname mismatch 必须产生 `verify_failed`
- 然后最小实现 pure Pascal 连接侧 `NotifyInfoCallback(...)`：
  - 复用 WinSSL 的事件命名
  - 接到 `DoConnect` / `DoAccept` 主线
- 最后回归当前 pure Pascal client focused suites。

## Files
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_info_callback_path.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_info_callback_path.pas -otmp/test_fp_client_info_callback && ./tmp/test_fp_client_info_callback`
- Expected:
  - pure Pascal 当前还没触发 info callback，因此状态事件断言失败

## GREEN
- 只把 `SetInfoCallback(...)` 接到 pure Pascal handshake state transition。
- 不把这波扩大成 pinning / session / OCSP 事件包。

## Verification
- `fpc -Fu./src tests/test_freepascal_client_info_callback_path.pas -otmp/test_fp_client_info_callback && ./tmp/test_fp_client_info_callback`
- `fpc -Fu./src tests/test_freepascal_client_verify_callback_path.pas -otmp/test_fp_client_verify_callback && ./tmp/test_fp_client_verify_callback`
- `fpc -Fu./src tests/test_freepascal_client_custom_ca_sources_path.pas -otmp/test_fp_client_custom_ca && ./tmp/test_fp_client_custom_ca`
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- `python3 -u scripts/compile_all_modules.py`
