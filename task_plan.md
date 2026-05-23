# Task Plan: FreePascal TLS 1.3 ALPN/SNI runtime proof

## Goal
Keep the FreePascal TLS 1.3 ALPN/SNI runtime proof aligned with the real negotiated path and keep the completeness gate wiring intact.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-23-freepascal-tls13-alpn-sni-runtime-proof.md](docs/plans/2026-05-23-freepascal-tls13-alpn-sni-runtime-proof.md)

## Done
- Added ALPN parsing/serialization in the FreePascal TLS 1.3 handshake path.
- Extended `tests/test_freepascal_client_session_resumption.pas` to assert negotiated ALPN and SNI truth.
- Extended `tests/test_freepascal_server_accept_skeleton.pas` to cover matched and unmatched ALPN negotiation.
- Wired the new assertions into `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`.

## Verification
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
- `./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh` completed with one unrelated failing group: `test_freepascal_client_ct_sct_surface`
