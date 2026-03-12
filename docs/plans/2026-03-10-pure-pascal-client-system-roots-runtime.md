# 2026-03-10 pure Pascal client system roots runtime

## Goal
- 为 pure Pascal / FreePascal 客户端补一条真实外网 `WithSystemRoots` 运行证据。
- 把 system-roots 从“builder/context 接线存在”推进到“真实 HTTPS 握手可通过”的 integration 证据。

## Architecture
- 先修根问题：让客户端验证消费 `peer chain + trust roots`。
- 然后新增 network-gated integration test：
  - `FAFAFA_RUN_NETWORK_TESTS=1` 时才真正连外网
  - 默认目标 `www.google.com:443`，支持环境变量覆盖
  - 使用 `TSSLContextBuilder.Create.WithBackend(sslFreePascal).WithVerifyPeer.WithSystemRoots.BuildClient`
- 运行结果作为 system-roots M1 证据，但仍保留“部分满足”结论，因为这不是完整的生产矩阵。

## Files
- `tests/integration/test_freepascal_system_roots_runtime.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `FAFAFA_RUN_NETWORK_TESTS=1 fpc -Fu./src tests/integration/test_freepascal_system_roots_runtime.pas -otmp/test_fp_system_roots_runtime && FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_fp_system_roots_runtime`
- `fpc -Fu./src tests/test_freepascal_client_chain_verification_path.pas -otmp/test_fp_client_chain && ./tmp/test_fp_client_chain`
- `python3 -u scripts/compile_all_modules.py`
