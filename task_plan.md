# Task Plan: TLS 1.3 PSK binder cleanup

## Goal
Keep the RFC 8448 PSK binder regression, remove temporary debug logging, and wire the new test into the FreePascal TLS 1.3 gate.

## Status
Complete

## Done
- Reproduced the binder path with `tests/test_rfc8448_psk_binder.pas`.
- Removed temporary `DBG-*` logging from the binder/resumption path.
- Added `test_rfc8448_psk_binder` to `scripts/run_freepascal_tls13_completeness_gate.sh`.
- Updated `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` for the new test count.

## Verification
- `./tests/test_rfc8448_psk_binder`
- `./tests/test_freepascal_client_session_resumption`
- `./tests/test_freepascal_server_session_resumption`
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `python3 scripts/compile_all_modules.py` still fails on pre-existing `fafafa.ssl.pkcs11.engine.pas`
