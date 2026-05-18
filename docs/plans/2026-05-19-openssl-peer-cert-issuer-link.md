# 2026-05-19 OpenSSL Peer Certificate Issuer Link

## Goal

把 `OpenSSL` backend 连接态 peer certificate surface 从“能拿到 leaf / chain，但 `GetIssuerCertificate()` 链接没有被补全”收紧成完整可追踪的证书链语义，避免公共 `ISSLCertificate` 明明暴露了 issuer-link surface，却在成功拿到 peer cert 后继续丢掉这条真相。

## Scope

- 不在本批承诺：
  - `WinSSL` / `MbedTLS` / `WolfSSL` / `FreePascal` 同批对齐
  - OpenSSL verified-chain source 重构
  - 证书验证 / OCSP 行为重做
- 只收以下缺口：
  1. `GetPeerCertificate()` 返回的 leaf cert 要能通过 `GetIssuerCertificate()` 看到 issuer truth
  2. `GetPeerCertificateChain()` 返回的 leaf entry 也要保留这条 issuer-link truth
  3. 不破坏现有 safe-degrade contract

## Files

- `src/fafafa.ssl.openssl.connection.pas`
- `tests/test_openssl_connection_peer_certificate_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `ISSLCertificate` 公共接口明确暴露：
  - `SetIssuerCertificate(...)`
  - `GetIssuerCertificate(...)`
- `TOpenSSLConnection.DoGetPeerCertificate()` 当前只包 `SSL_get_peer_certificate(...)` 返回的 leaf wrapper
- `TOpenSSLConnection.DoGetPeerCertificateChain()` 当前只包 `SSL_get_peer_cert_chain(...)` 返回的 chain wrappers
- 但当前没有把 leaf / chain entries 的 issuer link 接起来
- 结果：
  - `GetPeerCertificate()` 虽然能返回 leaf cert
  - `GetPeerCertificateChain()` 也能返回 chain entries
  - 但 public issuer-link truth 仍可能为空

## Steps

1. 新增 focused RED：
   - `tests/test_openssl_connection_peer_certificate_surface.pas`
   - 锁住 leaf cert preserves issuer link
   - 锁住 chain leaf entry preserves issuer link
2. 最小修复：
   - `TOpenSSLConnection` 在 materialize peer leaf / chain 时补 issuer link
3. focused 运行：
   - `tests/test_openssl_connection_peer_certificate_surface.pas`
4. cross-check：
   - `tests/test_openssl_connection_peer_certificate_contract.pas`
   - `tests/test_openssl_connection_peer_certificate_chain_contract.pas`
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_openssl_connection_peer_certificate_surface_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_openssl_connection_peer_certificate_surface_units \
  -FEtmp/test_openssl_connection_peer_certificate_surface_units \
  -otmp/test_openssl_connection_peer_certificate_surface_units/test_openssl_connection_peer_certificate_surface \
  tests/test_openssl_connection_peer_certificate_surface.pas
./tmp/test_openssl_connection_peer_certificate_surface_units/test_openssl_connection_peer_certificate_surface

mkdir -p tmp/test_openssl_connection_peer_certificate_contract_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_openssl_connection_peer_certificate_contract_units \
  -FEtmp/test_openssl_connection_peer_certificate_contract_units \
  -otmp/test_openssl_connection_peer_certificate_contract_units/test_openssl_connection_peer_certificate_contract \
  tests/test_openssl_connection_peer_certificate_contract.pas
./tmp/test_openssl_connection_peer_certificate_contract_units/test_openssl_connection_peer_certificate_contract

mkdir -p tmp/test_openssl_connection_peer_certificate_chain_contract_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_openssl_connection_peer_certificate_chain_contract_units \
  -FEtmp/test_openssl_connection_peer_certificate_chain_contract_units \
  -otmp/test_openssl_connection_peer_certificate_chain_contract_units/test_openssl_connection_peer_certificate_chain_contract \
  tests/test_openssl_connection_peer_certificate_chain_contract.pas
./tmp/test_openssl_connection_peer_certificate_chain_contract_units/test_openssl_connection_peer_certificate_chain_contract

mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp/backend_contract_units \
  -otmp/backend_contract_units/test_backend_contract \
  tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract

git diff --check
```

## Execution Result

- COMPLETED
- RED first exposed:
  - `OpenSSL peer leaf certificate should preserve issuer link`
- GREEN after fix:
  - `tests/test_openssl_connection_peer_certificate_surface.pas`: PASS
  - `tests/test_openssl_connection_peer_certificate_contract.pas`: `2 passed / 0 failed`
  - `tests/test_openssl_connection_peer_certificate_chain_contract.pas`: `8 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
  - `git diff --check`: PASS
- Outcome:
  - `OpenSSL` connection leaf cert now preserves issuer-link truth
  - peer chain entries now also preserve issuer-link truth
  - existing safe-degrade contracts stayed green while the public peer-cert surface became complete
