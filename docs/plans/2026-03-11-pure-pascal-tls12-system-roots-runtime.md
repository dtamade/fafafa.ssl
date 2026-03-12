# 2026-03-11 pure Pascal TLS1.2 system roots runtime

## Goal
- 为 pure Pascal / FreePascal TLS1.2 客户端补一条 network-gated 的 system-roots runtime harness。
- 保持显式 host 驱动，避免把不稳定的外网站点默认写死成门禁噪音。

## Architecture
- 新增 integration：
  - `tests/integration/test_freepascal_tls12_system_roots_runtime.pas`
- 行为：
  - 只有 `FAFAFA_RUN_NETWORK_TESTS=1` 时才真正联网
  - host 只从环境变量解析：
    - `FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS`
    - `FAFAFA_TLS12_SYSTEM_ROOTS_HOST`
  - 若 host 未提供，则显式 `Skip`
  - 上下文固定：
    - `sslFreePascal`
    - `sslProtocolTLS12`
    - `WithVerifyPeer`
    - `WithSystemRoots`
- 这波先提供 harness 与 compile-level evidence，不默认绑定具体公网主机。

## Files
- `tests/integration/test_freepascal_tls12_system_roots_runtime.pas`
- `tests/scripts/test_freepascal_tls12_system_roots_runtime_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_tls12_system_roots_runtime_contract.sh`
- `fpc -Fu./src tests/integration/test_freepascal_tls12_system_roots_runtime.pas -otmp/test_fp_tls12_system_roots_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS='example-host' ./tmp/test_fp_tls12_system_roots_runtime`
