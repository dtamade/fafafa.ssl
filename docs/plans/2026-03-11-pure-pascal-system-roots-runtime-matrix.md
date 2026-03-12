# 2026-03-11 pure Pascal system roots runtime matrix

## Goal
- 把 pure Pascal / FreePascal 客户端 `WithSystemRoots` 的运行时证据从“单站点握手”提升到“小型多主机矩阵”。
- 保持 network-gated，避免把外网波动引入默认本地门禁。

## Architecture
- 继续沿用现有 integration 程序 `tests/integration/test_freepascal_system_roots_runtime.pas`。
- 增加 `FAFAFA_SYSTEM_ROOTS_HOSTS`：
  - 支持 `, ; 空白` 分隔的多主机列表
  - 继续兼容 `FAFAFA_SYSTEM_ROOTS_HOST`
  - 继续复用 `FAFAFA_SYSTEM_ROOTS_PORT`
- 默认 host 候选保持保守：
  - `www.google.com`
  - `www.cloudflare.com`
  - `www.github.com`
- 运行时逐个 host：
  - 建 TCP
  - `TSSLContextBuilder.Create.WithBackend(sslFreePascal).WithVerifyPeer.WithSystemRoots.BuildClient`
  - 执行 TLS 握手并校验 verify/cipher
- 输出 matrix 摘要，便于 checklist 和月度总结引用。

## Files
- `tests/integration/test_freepascal_system_roots_runtime.pas`
- `tests/scripts/test_freepascal_system_roots_runtime_matrix_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_system_roots_runtime_matrix_contract.sh`
- `fpc -Fu./src tests/integration/test_freepascal_system_roots_runtime.pas -otmp/test_fp_system_roots_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_SYSTEM_ROOTS_HOSTS='www.google.com,www.cloudflare.com' ./tmp/test_fp_system_roots_runtime`
- `python3 -u scripts/compile_all_modules.py`
