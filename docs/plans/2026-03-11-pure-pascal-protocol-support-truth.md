# 2026-03-11 pure Pascal protocol support truth

## Goal
- 收口 pure Pascal / FreePascal backend 在 `IsProtocolSupported` / capability TLS 版本字段上的真相漂移。
- 保证 capability 文档里的 `runtime truth` 原则不被 pure Pascal 当前 `TLS 1.3-only handshake path` 破坏。

## Architecture
- 先在 `tests/test_freepascal_backend_basic.pas` 增加 RED：
  - `IsProtocolSupported(sslProtocolTLS12)` 应为 `False`
  - `IsProtocolSupported(sslProtocolTLS13)` 应为 `True`
  - `GetCapabilities.MinTLSVersion` / `MaxTLSVersion` 应都指向 `sslProtocolTLS13`
  - `KnownIssues` 需明确保留 `TLS 1.2` 未实现现状
- 再最小修改 `src/fafafa.ssl.freepascal.lib.pas`：
  - protocol support truth
  - capability truth
  - known-issues 文案
- 视回归情况再决定是否继续收口 default config / context 默认协议面。

## Files
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `docs/plans/2026-03-11-pure-pascal-protocol-support-truth.md`
- `docs/plans/2026-03-current-summary.md`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
