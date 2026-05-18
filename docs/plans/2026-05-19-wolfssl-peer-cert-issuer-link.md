# 2026-05-19 WolfSSL Peer Certificate Issuer Link

## Goal

把 `WolfSSL` backend 连接态 peer certificate surface 从“能返回 leaf / chain，但 `GetIssuerCertificate()` 链接没有被补全”收紧成完整可追踪的证书链语义，避免公共 `ISSLCertificate` 在成功拿到对端证书后仍然丢失 issuer-link truth。

## Scope

- 不在本批承诺：
  - `WinSSL` / `MbedTLS` / `OpenSSL` / `FreePascal` 同批对齐
  - `WolfSSL` native chain API 重构
  - OCSP / verification 行为重做
- 只收以下缺口：
  1. `GetPeerCertificate()` 返回的 leaf cert 要能通过 `GetIssuerCertificate()` 看到 issuer truth
  2. `GetPeerCertificateChain()[0]` 也要保留这条 issuer-link truth
  3. 不破坏现有 materialization / safe-degrade contract

## Files

- `src/fafafa.ssl.wolfssl.connection.pas`
- `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `ISSLCertificate` 公共接口明确暴露：
  - `SetIssuerCertificate(...)`
  - `GetIssuerCertificate(...)`
- `TWolfSSLConnection.DoGetPeerCertificate()` 当前会把 native peer cert materialize 成 owned copy
- `TWolfSSLConnection.DoGetPeerCertificateChain()` 当前会把 native chain materialize 成 owned cert array
- 但当前没有把 leaf / chain entries 的 issuer link 接起来
- 结果：
  - public cert objects 虽然存在
  - 但 public issuer-link truth 仍为空

## Steps

1. 在 `tests/connection/test_wolfssl_client_peer_certificate_surface.pas` 增加 RED：
   - leaf cert preserves issuer link
   - chain leaf preserves issuer link
2. 最小修复：
   - `TWolfSSLConnection` 在 materialize peer leaf / chain 后补 issuer link
3. focused 运行：
   - `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
4. cross-check：
   - `tests/test_wolfssl_connection_peer_certificate_contract.pas`
   - `tests/test_wolfssl_framework.pas`
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_wolfssl_client_peer_certificate_surface_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_wolfssl_client_peer_certificate_surface_units \
  -FEtmp/test_wolfssl_client_peer_certificate_surface_units \
  -otmp/test_wolfssl_client_peer_certificate_surface_units/test_wolfssl_client_peer_certificate_surface \
  tests/connection/test_wolfssl_client_peer_certificate_surface.pas
./tmp/test_wolfssl_client_peer_certificate_surface_units/test_wolfssl_client_peer_certificate_surface

mkdir -p tmp/test_wolfssl_connection_peer_certificate_contract_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_wolfssl_connection_peer_certificate_contract_units \
  -FEtmp/test_wolfssl_connection_peer_certificate_contract_units \
  -otmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract \
  tests/test_wolfssl_connection_peer_certificate_contract.pas
./tmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract

mkdir -p tmp/test_wolfssl_framework_units
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas
./tmp/test_wolfssl_framework_units/test_wolfssl_framework

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
  - `WolfSSL peer leaf certificate should preserve issuer link`
- GREEN after fix:
  - `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`: PASS
  - `tests/test_wolfssl_connection_peer_certificate_contract.pas`: `4 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
  - `git diff --check`: PASS
- Outcome:
  - `WolfSSL` connection leaf cert now preserves issuer-link truth
  - peer chain entries now also preserve issuer-link truth
  - existing materialization and safe-degrade contracts stayed green while the public peer-cert surface became complete
