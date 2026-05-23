# Progress Log

## 2026-05-23
- Reproduced the last remaining completeness-gate failure in `tests/test_freepascal_client_ct_sct_surface.pas`: `FAIL: Malformed embedded SCT list should fail-closed`.
- Traced the regression to `src/fafafa.ssl.freepascal.connection.pas`, where the embedded SCT fallback swallowed `TryLoadEmbeddedSignedCertificateTimestampList(...)` errors with `AError := ''`.
- Tightened that path to preserve an SCT-related error, clear the peer certificate cache, and exit early so malformed embedded SCT data fails closed instead of degrading into a no-SCT path.
- Verified:
  - `mkdir -p tmp/test_freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_ct_sct_surface -FEtmp/test_freepascal_client_ct_sct_surface -otmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
  - `python3 -u scripts/compile_all_modules.py`
  - `git diff --check`
- Results:
  - `PASS: FreePascal client CT SCT surface checks passed`
  - `tmp/test-reports/freepascal_tls13_completeness_20260523_191741.md` recorded `18` passing groups and `0` failures.
  - `python3 -u scripts/compile_all_modules.py` compiled `185/186` units and failed only on the pre-existing `fafafa.ssl.pkcs11.engine.pas` boundary.

## 2026-05-23
- Extended the FreePascal TLS 1.3 handshake path to parse and emit ALPN correctly.
- Updated the client session resumption runtime proof to verify negotiated ALPN and SNI truth through `ISSLConnectionInfo`.
- Updated the server accept skeleton runtime proof to verify matched and unmatched ALPN negotiation.
- Updated the completeness gate contract to lock the new ALPN/SNI assertions.
- Verified:
  - `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - `mkdir -p ./tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -FE./tmp/test_freepascal_client_session_resumption -o./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas`
  - `./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p ./tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FE./tmp/test_freepascal_server_accept_skeleton -o./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas`
  - `./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh`
- Error encountered:
  - The first manual `fpc` compile failed because the output directory did not exist; I fixed it with `mkdir -p` and reran.
  - The full completeness gate ended with one unrelated failing group: `test_freepascal_client_ct_sct_surface`.

## 2026-05-23
- Reproduced and verified the RFC 8448 PSK binder path with `tests/test_rfc8448_psk_binder.pas`.
- Removed temporary debug output from the FreePascal binder/resumption flow.
- Added the new binder regression to `scripts/run_freepascal_tls13_completeness_gate.sh`.
- Updated the completeness gate contract to expect 18 test runs and the new summary row.
- Verified:
  - `./tests/test_rfc8448_psk_binder`
  - `./tests/test_freepascal_client_session_resumption`
  - `./tests/test_freepascal_server_session_resumption`
  - `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `python3 scripts/compile_all_modules.py` still fails on pre-existing `fafafa.ssl.pkcs11.engine.pas`.
