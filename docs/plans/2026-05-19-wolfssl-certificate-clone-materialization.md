# 2026-05-19 WolfSSL Certificate Clone Materialization

## Goal

把 `TWolfSSLCertificate.Clone()` 从“只复制缓存字段、不重建 native X509”的 metadata shell 收紧成真正可用的 cert clone，避免 public `ISSLCertificate` 在 `WolfSSL` backend 上继续出现 clone 后 metadata 退化或 native handle 消失的问题。

## Scope

- 不在本批承诺：
  - `OpenSSL` / `MbedTLS` / `WinSSL` 同批重构
  - 整条 connection/session peer-cert surface 重做
  - issuer chain 深拷贝完善
- 不重开：
  - `MbedTLS` connection peer-cert materialization 旧 lane
  - `MbedTLS/WolfSSL` session metadata completeness 旧 lane
- 只收以下缺口：
  1. `TWolfSSLCertificate.Clone()` 必须保留 native handle truth
  2. clone 后 `subject/issuer/fingerprint` 不得退化成 placeholder shell
  3. clone helper 不足时必须 fail-closed，而不是继续返回假完整 clone

## Files

- `src/fafafa.ssl.wolfssl.certificate.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 `TWolfSSLCertificate.Clone()` 仍只复制：
  - `FPEMData`
  - `FDERData`
  - `FInfo`
- 但不会重新 materialize `FX509`。
- 这意味着 clone 后：
  - `GetNativeHandle = nil`
  - `GetSubject / GetIssuer / GetVersion` 之类依赖 native X509 的路径会退化
- `OpenSSL.Clone()` 当前通过 `X509_up_ref` 保留 native truth。
- `MbedTLS.Clone()` 当前也已经重建 native cert。

## Steps

1. 在 `tests/test_wolfssl_framework.pas` 增加 certificate clone RED：
   - clone keeps native handle
   - clone keeps subject/issuer/fingerprint truth
   - helper-loss path fail-closed
2. 最小修复：
   - `TWolfSSLCertificate.Clone()` 改为 `DER copy -> owned reload`
3. focused 运行：
   - `tests/test_wolfssl_framework.pas`
4. cross-check：
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
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
  - `Clone keeps native handle for loaded certificate`
  - `Clone preserves subject truth`
  - `Clone preserves issuer truth`
  - `Clone fails closed when X509 materialization helper is unavailable`
  - `Clone preserves fingerprint truth` remained PASS
- GREEN after fix:
  - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
  - `git diff --check`: PASS
- Outcome:
  - `TWolfSSLCertificate.Clone()` now re-materializes an owned native cert for loaded certificates
  - loaded clone no longer degrades into a metadata shell
  - helper-loss path now fails closed instead of returning a fake-complete clone
