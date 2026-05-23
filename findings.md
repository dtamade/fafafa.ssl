# Findings

## Conclusions
- FreePascal TLS 1.3 now records negotiated ALPN end-to-end in the client/server proof path.
- `tests/test_freepascal_client_session_resumption.pas` and `tests/test_freepascal_server_accept_skeleton.pas` both pass with the new ALPN/SNI assertions.
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` passes with the new assertions wired in.
- The full completeness gate still reports one unrelated failing group: `test_freepascal_client_ct_sct_surface`.

## Notes
- The full gate report for this run recorded 17 passing groups and 1 failing group.
