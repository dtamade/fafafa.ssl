# 2026-05-23 FreePascal TLS 1.3 ALPN/SNI Runtime Proof

## Goal

把 FreePascal TLS 1.3 的 ALPN/SNI runtime proof 收口成真实协商路径：客户端/服务端都能记录并回显协商到的 ALPN，server accept skeleton 和 client session resumption 都要用 `ISSLConnectionInfo` 验证结果。

## Scope

- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.tls13.clienthello.parser.pas`
- `src/fafafa.ssl.tls13.parser.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- ClientHello parser must capture ALPN offers.
- Server-side TLS 1.3 handshake must emit a correctly wrapped ALPN `EncryptedExtensions`.
- ALPN mismatch stays fail-open: handshake succeeds, negotiated ALPN stays empty.
- Runtime proof should prefer `ISSLConnectionInfo` for negotiated ALPN, while keeping `GetSelectedALPNProtocol` as the compatibility mirror.

## Steps

1. Extend the FreePascal TLS 1.3 parser/serializer path for ALPN.
2. Update the client session resumption runtime proof to assert negotiated ALPN and SNI truth.
3. Update the server accept skeleton runtime proof to cover both matched and unmatched ALPN cases.
4. Wire the new assertions into the completeness gate contract.
5. Run focused compile/run checks and the completeness gate.

## Verification

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `mkdir -p ./tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -FE./tmp/test_freepascal_client_session_resumption -o./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas`
- `./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
- `mkdir -p ./tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FE./tmp/test_freepascal_server_accept_skeleton -o./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas`
- `./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh`

## Outcome

- Focused ALPN/SNI runtime proofs are green.
- Completeness-gate contract is green.
- Full completeness gate still has one unrelated failure group: `test_freepascal_client_ct_sct_surface`.
