# 2026-05-19 MbedTLS Connection Peer-Certificate Materialization

## Goal

把 `TMbedTLSConnection.GetPeerCertificate()` / `GetPeerCertificateChain()` 从“直接暴露 `mbedtls_ssl_get_peer_cert(...)` 的 borrowed pointer”收紧成真正可独立持有的 owned cert surface，避免 public connection peer-cert API 继续泄漏 MbedTLS 的内部生命周期约束。

## Scope

- 不在本批承诺：
  - MbedTLS 完整证书链解析（当前 native surface 仍只有单个 peer cert）
  - `WolfSSL` / `OpenSSL` / `FreePascal` 同批重构
  - session lane 重开
- 不重开：
  - `MbedTLS/WolfSSL` session metadata/peer-cert completeness 旧 lane
  - WinSSL runtime / workflow / release 旧 lane
- 只收以下缺口：
  1. `TMbedTLSConnection.GetPeerCertificate()` 必须 materialize owned copy
  2. `TMbedTLSConnection.GetPeerCertificateChain()` 的单证书链入口也必须 materialize owned copy
  3. helper 缺失时必须 fail-closed，而不是继续返回 borrowed wrapper

## Files

- `src/fafafa.ssl.mbedtls.connection.pas`
- `tests/test_mbedtls_connection_peer_certificate_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `/usr/include/mbedtls/ssl.h` 对 `mbedtls_ssl_get_peer_cert()` 明确写着：
  - 该指针在后续 SSL API 调用后可能失效
  - 若调用方要跨 API 调用继续使用，必须自己复制
- 但当前 `TMbedTLSConnection.DoGetPeerCertificate()` / `DoGetPeerCertificateChain()` 仍直接：
  - `TMbedTLSCertificate.Create(LPeerCert, False)`
- 这意味着 public `ISSLCertificate` surface 当前仍带着 backend-internal lifetime trap。

## Steps

1. 新增 focused RED contract：
   - returned cert fingerprint matches fixture
   - returned native handle must differ from source fixture handle
   - helper loss must fail closed (`nil` / empty chain)
2. 最小修复：
   - connection peer-cert surface 复用 `DER copy -> owned reload` 路线
3. focused 运行：
   - `tests/test_mbedtls_connection_peer_certificate_contract.pas`
4. cross-check：
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_mbedtls_connection_peer_certificate_contract_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_mbedtls_connection_peer_certificate_contract_units \
  -FEtmp/test_mbedtls_connection_peer_certificate_contract_units \
  -otmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract \
  tests/test_mbedtls_connection_peer_certificate_contract.pas && \
./tmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract

mkdir -p tmp/backend_contract_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp/backend_contract_units \
  -otmp/backend_contract_units/test_backend_contract \
  tests/contract/test_backend_contract.pas && \
./tmp/backend_contract_units/test_backend_contract

git diff --check
```

## Execution Result

- PASS
- `TMbedTLSConnection.GetPeerCertificate()` / `GetPeerCertificateChain()` 不再直接暴露 borrowed cert wrapper，而是统一 materialize owned cert。
- focused verification：
  - `tests/test_mbedtls_connection_peer_certificate_contract.pas`: `8 passed / 0 failed`
  - `tests/test_mbedtls_framework.pas`: `116 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`: PASS
