# 2026-03-11 pure Pascal TLS1.2 session-id resumption

## Goal
- 把 pure Pascal TLS1.2 从“只有 session surface”推进到真正的 resumed handshake。
- 这波优先走最小可落地的 `session-id` abbreviated handshake，不先碰 TLS1.2 tickets。

## Root Cause
- 当前缺口并不只是 `SetSession(...)` 没生效，而是三层叠加：
  - session snapshot 之前没有保存 TLS1.2 `session_id + master_secret`
  - `ConnectTLS12Client` 只有 full handshake 分支，没有 abbreviated handshake 分支
  - `TryParseTLS12ServerHelloFromHandshake(...)` 默认假设一定带 extensions；resumed `ServerHello` 可能根本不带

## Architecture
- `src/fafafa.ssl.freepascal.session.pas`
  - 为 `IFreePascalResumptionSession` 增加 TLS1.2 resumption material getter
  - `TFreePascalSession` 保存：
    - `TLS12SessionIDBytes`
    - `TLS12MasterSecret`
  - `IsResumable` 对 TLS1.2 session-id 路径返回真
- `src/fafafa.ssl.freepascal.connection.pas`
  - 第一次 full handshake 成功后缓存：
    - `FTLS12SessionID`
    - `FTLS12MasterSecret`
  - 第二次 `Connect` 时若 caller 注入 TLS1.2 resumable session：
    - ClientHello 复用 cached session id
    - 若 server 进入 abbreviated flight，则按 current transcript 验证 server Finished，再回 client Finished
  - resumed path 继续复用 cached peer cert/chain 做验证与 observability
- `src/fafafa.ssl.tls12.serverhello.parser.pas`
  - 允许 `ServerHello` 无 extensions

## Files
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.tls12.serverhello.parser.pas`
- `tests/scripts/test_freepascal_tls12_resumption_openssl_interop_contract.sh`
- `tests/scripts/test_freepascal_tls12_session_surface_contract.sh`
- `tests/scripts/test_freepascal_tls12_resumption_truth_matrix_contract.sh`
- `src/fafafa.ssl.freepascal.lib.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_tls12_resumption_openssl_interop_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_session_surface_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_resumption_truth_matrix_contract.sh`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
