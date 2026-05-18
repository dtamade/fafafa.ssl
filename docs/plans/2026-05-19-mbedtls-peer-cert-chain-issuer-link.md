# MbedTLS Peer Certificate Chain Issuer Link

## Goal

补齐 `MbedTLS` 连接态 peer-certificate public chain truth，确保：

- `GetPeerCertificateChain()` 不再把 native peer chain 截断成单个 leaf
- `GetPeerCertificate()` 与 returned chain leaf 都能保留 `GetIssuerCertificate()` truth
- 现有 owned-copy / fail-closed 边界保持不变

## Scope

- `src/fafafa.ssl.mbedtls.connection.pas`
- `tests/test_mbedtls_connection_peer_certificate_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 扩展现有 MbedTLS focused contract，先锁住 leaf+issuer chain 与 issuer-link truth
2. 在 Linux 上先观察 RED
3. 对 `MbedTLS` connection layer 做最小 chain materialization 修复
4. 重新跑 focused contract 与 backend contract 到 GREEN

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
  -FEtmp \
  -otmp/tmp_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/tmp_backend_contract

git diff --check
```

## Expected Closeout

- MbedTLS focused contract 先 RED 后 GREEN
- backend contract 持续 green
- planning files 记录这条 connection-level chain completeness 收口
