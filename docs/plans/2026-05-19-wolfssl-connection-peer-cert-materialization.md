# 2026-05-19 WolfSSL Connection Peer Certificate Materialization

## Goal

把 `TWolfSSLConnection.GetPeerCertificate()` 从“直接暴露 `wolfSSL_get_peer_certificate(...)` 返回的 native cert wrapper”收紧成和现有 `chain/session/certificate clone` 一致的 owned/materialized public cert surface，避免 `WolfSSL` 单证书连接态入口继续游离在不同 ownership 语义之外。

## Scope

- 不在本批承诺：
  - `OpenSSL` / `MbedTLS` / `WinSSL` 同批重构
  - `GetPeerCertificateChain()` 新一轮重做
  - session / certificate clone 旧 lane 重开
- 只收以下缺口：
  1. `TWolfSSLConnection.GetPeerCertificate()` 必须返回 materialized/owned cert
  2. 返回的 public cert 不应继续别名 source native handle
  3. copy helper 不足时必须 fail-closed

## Files

- `src/fafafa.ssl.wolfssl.connection.pas`
- `tests/test_wolfssl_connection_peer_certificate_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 `TWolfSSLConnection.GetPeerCertificate()` 直接包：
  - `wolfSSL_get_peer_certificate(FWolfSSL)`
- 但：
  - `TWolfSSLConnection.GetPeerCertificateChain()` 已经走 `DER -> owned cert`
  - `TWolfSSLSession.FromConnection()` 也已经走 `MaterializeWolfSSLCertificate(...)`
  - `TWolfSSLCertificate.Clone()` 也已改成 `DER copy -> owned reload`
- 所以当前 `WolfSSL` 连接态 public peer-cert surface 仍存在单证书路径与其它公开 surface 语义不一致的问题。

## Steps

1. 新增 focused contract：
   - `GetPeerCertificate()` materializes owned copy
   - 返回 cert 不再 alias source native handle
   - helper-loss path fail-closed
2. 最小修复：
   - `TWolfSSLConnection.GetPeerCertificate()` 改为走现有 clone/materialization 路线
3. focused 运行：
   - `tests/test_wolfssl_connection_peer_certificate_contract.pas`
4. cross-check：
   - `tests/test_wolfssl_framework.pas`
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_wolfssl_connection_peer_certificate_contract_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_wolfssl_connection_peer_certificate_contract_units \
  -FEtmp/test_wolfssl_connection_peer_certificate_contract_units \
  -otmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract \
  tests/test_wolfssl_connection_peer_certificate_contract.pas && \
./tmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract

mkdir -p tmp/test_wolfssl_framework_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas && \
./tmp/test_wolfssl_framework_units/test_wolfssl_framework

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
  - `GetPeerCertificate must return an owned copy instead of the source native handle`
  - `GetPeerCertificate should fail closed when cert-copy helper is unavailable`
- GREEN after fix:
  - `tests/test_wolfssl_connection_peer_certificate_contract.pas`: `4 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
  - `git diff --check`: PASS
- Outcome:
  - `TWolfSSLConnection.GetPeerCertificate()` now materializes an owned public cert
  - returned cert no longer aliases the source native handle
  - helper-loss path now fails closed instead of returning a source wrapper
