# 2026-03-11 pure Pascal connection info richness

## Goal
- 让 pure Pascal 的 `GetConnectionInfo` 从“基础可用”推进到“对框架层足够有用”。
- 重点补齐：
  - `CipherSuiteId`
  - `KeyExchange / Cipher / Hash`
  - `KeySize / MacSize`
  - `SessionId / IsResumed`
  - `PeerCertificate` 快照

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_peer_certificate_foundation.pas`
- `tests/test_freepascal_local_sha384_suite_roundtrip.pas`
- `tests/test_freepascal_local_session_resumption_roundtrip.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
