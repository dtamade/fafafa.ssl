# Certificate Clone Issuer Link

## Goal

补齐各 backend `ISSLCertificate.Clone()` 的 `issuer-link truth`，确保：

- clone 后的 leaf certificate 不会丢失 `GetIssuerCertificate()`
- 已经修好的 connection/session/public surface 不会在 copy/clone 路径上退化
- 用一条跨 backend 的 focused contract 锁住这层语义

## Scope

- `src/fafafa.ssl.openssl.certificate.pas`
- `src/fafafa.ssl.wolfssl.certificate.pas`
- `src/fafafa.ssl.mbedtls.certificate.pas`
- `src/fafafa.ssl.winssl.certificate.pas`
- `tests/test_certificate_clone_issuer_link_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 新增跨 backend clone issuer-link focused contract
2. 先在 Linux 上观察 OpenSSL/WolfSSL/MbedTLS/FreePascal 的 RED
3. 再用 `Win64 cross-target + wine` 观察 WinSSL 的 RED
4. 对各 backend 做最小 clone 修复
5. 重新跑 focused tests 到 GREEN，并补回归记录

## Commands

```bash
mkdir -p tmp/test_certificate_clone_issuer_link_contract_units && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_certificate_clone_issuer_link_contract_units \
  -FEtmp/test_certificate_clone_issuer_link_contract_units \
  -otmp/test_certificate_clone_issuer_link_contract_units/test_certificate_clone_issuer_link_contract \
  tests/test_certificate_clone_issuer_link_contract.pas && \
  ./tmp/test_certificate_clone_issuer_link_contract_units/test_certificate_clone_issuer_link_contract

mkdir -p tmp/test_certificate_clone_issuer_link_contract_win64 && \
  fpc -B -Twin64 -Px86_64 -Fu./src -Fu./tests \
  -FUtmp/test_certificate_clone_issuer_link_contract_win64 \
  -FEtmp/test_certificate_clone_issuer_link_contract_win64 \
  -otmp/test_certificate_clone_issuer_link_contract_win64/test_certificate_clone_issuer_link_contract.exe \
  tests/test_certificate_clone_issuer_link_contract.pas && \
  wine tmp/test_certificate_clone_issuer_link_contract_win64/test_certificate_clone_issuer_link_contract.exe

git diff --check
```

## Expected Closeout

- Linux focused contract 先 RED 后 GREEN
- Win64+wine focused contract 先 RED 后 GREEN
- planning files 记录这条 clone semantics 收口
