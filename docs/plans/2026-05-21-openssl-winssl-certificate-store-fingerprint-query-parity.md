# OpenSSL And WinSSL Certificate Store Fingerprint Query Parity

## Goal

把 `OpenSSL` / `WinSSL` 证书存储对象的
`FindByFingerprint`
收紧到与当前仓库更稳的
`FreePascal`
基线一致的 normalized hex truth，
避免调用方在这两个 backend 上继续遇到：

- 同一张证书
  只因指纹带 `-`
  / 空格
  的展示格式不同
  就查不到
- 空白型查询
  没有先归一化到空值语义

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certstore.pas`
  - `src/fafafa.ssl.winssl.certstore.pas`
  - `tests/openssl/test_openssl_certstore_fingerprint_query_contract.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开 `FindBySubject` / `FindByIssuer` / `FindBySerialNumber`
  - 不重开 broader store index/cache family
  - 不扩到 certificate fingerprint getter 本身

## Architecture Truth

- 当前 `FreePascal`
  的
  `NormalizeFingerprint(...)`
  会统一去掉：
  - `:`
  - `-`
  - 空格
- 但当前源码里：
  - `TOpenSSLCertificateStore`
    只在 index/query
    去掉 `:`
  - `TWinSSLCertificateStore`
    也只在 compare
    去掉 `:`
- 这意味着：
  lower-case + `:`
  可能已经能命中，
  但
  `AA-BB-CC`
  /
  `  aa bb cc  `
  这类常见展示格式
  仍可能在两个主 backend
  上继续分裂

## Steps

1. 新增 OpenSSL focused contract：
   - lower-case
   - `-`
   - 首尾空白
   的 fingerprint query
   仍能命中同一张证书
2. 在 WinSSL 现有 certstore 测试里补同类 memory-backed store 断言
3. 先跑 OpenSSL focused proof，
   观察 RED
4. 在 OpenSSL / WinSSL store 实现中复用现有 hex normalize helper
5. 再跑 OpenSSL focused proof，
   并等 GitHub Windows CI
   做 WinSSL runtime proof

## Focused Proof

```bash
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_openssl_certstore_fingerprint_query_contract_units \
  -FEtmp/test_openssl_certstore_fingerprint_query_contract_units \
  -otmp/test_openssl_certstore_fingerprint_query_contract_units/test_openssl_certstore_fingerprint_query_contract \
  tests/openssl/test_openssl_certstore_fingerprint_query_contract.pas

./tmp/test_openssl_certstore_fingerprint_query_contract_units/test_openssl_certstore_fingerprint_query_contract

git diff --check
```

## Execution Result

- 首轮
  `OpenSSL`
  focused RED
  只打出 1 个失败：
  - `FindByFingerprint supports normalized fingerprint query variant`
- 失败输入是：
  lower-case
  +
  `-`
  +
  首尾空白
  的 fingerprint variant，
  这说明当前 residual
  真实存在于 query normalization，
  不是 fingerprint getter 本身
- 本批最小修法：
  - `TOpenSSLCertificateStore`
    的 fingerprint index/query
    改为复用
    `NormalizeCertificateStoreHex(...)`
  - `TWinSSLCertificateStore`
    的 fingerprint query compare
    也改为复用同一 helper
- 当前本地 proof：
  - `./tmp/test_openssl_certstore_fingerprint_query_contract_units/test_openssl_certstore_fingerprint_query_contract`
    已通过：
    `9 passed / 0 failed`
  - `git diff --check`
    通过
- 当前剩余 proof：
  - `WinSSL`
    runtime
    继续交给
    GitHub Windows CI
