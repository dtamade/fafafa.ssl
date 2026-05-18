# 2026-05-19 FreePascal Peer Certificate Issuer Link

## Goal

把 `FreePascal` backend 连接态 peer certificate surface 从“虽然返回了 leaf + chain，但 leaf 上的 `GetIssuerCertificate()` 仍为空”收紧成完整可追踪的证书链语义，避免 public `ISSLCertificate` 明明公开了 issuer-link surface，却在成功握手后继续丢失这条真相。

## Scope

- 不在本批承诺：
  - `OpenSSL` / `MbedTLS` / `WolfSSL` / `WinSSL` 同批对齐
  - issuer-link 深层 object-identity 统一
  - 证书验证/OCSP 路线重做
- 只收以下缺口：
  1. `GetPeerCertificate()` 返回的 leaf cert 要能通过 `GetIssuerCertificate()` 看到 issuer truth
  2. `GetPeerCertificateChain()[0]` 也要保留这条 issuer-link truth
  3. 不重开现有 scripted handshake 主路径

## Files

- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_peer_certificate_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `ISSLCertificate` 公共接口明确暴露：
  - `SetIssuerCertificate(...)`
  - `GetIssuerCertificate(...)`
- `TFreePascalConnection` 在握手后会构建：
  - `FPeerCertificateChain`
  - `FPeerCertificate := FPeerCertificateChain[0]`
- 但当前没有把 chain 相邻证书之间的 issuer link 接起来。
- 结果：
  - `GetPeerCertificate()` 虽然能返回 leaf cert
  - `GetPeerCertificateChain()` 也能返回完整 chain
  - 但 leaf 上的 `GetIssuerCertificate()` 仍可能为空，public chain truth 不完整

## Steps

1. 在 `tests/test_freepascal_client_peer_certificate_surface.pas` 增加 RED：
   - leaf cert preserves issuer link
   - chain leaf preserves issuer link
2. 最小修复：
   - `TFreePascalConnection` 在构建 `FPeerCertificateChain` 后接上相邻 issuer link
3. focused 运行：
   - `tests/test_freepascal_client_peer_certificate_surface.pas`
4. cross-check：
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_freepascal_client_peer_certificate_surface_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_freepascal_client_peer_certificate_surface_units \
  -FEtmp/test_freepascal_client_peer_certificate_surface_units \
  -otmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface

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

- COMPLETED
- RED first exposed:
  - `Peer leaf certificate should preserve issuer link`
- GREEN after fix:
  - `tests/test_freepascal_client_peer_certificate_surface.pas`: PASS
  - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
  - `git diff --check`: PASS
- Outcome:
  - `FreePascal` connection leaf cert now preserves issuer-link truth
  - chain leaf also preserves the same issuer-link truth
  - public peer-cert surface no longer drops issuer linkage after a successful handshake
