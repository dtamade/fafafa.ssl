# X509 Ed25519 Algorithm Metadata Truth

## Goal

把纯 Pascal `TX509Certificate`
对 `Ed25519` 证书的算法元数据
从 OID / `Unknown` / `0`
收紧成可读且可复用的 public truth，
避免依赖 parser 的 backend
继续在已加载 `Ed25519` 证书上暴露：

- `PublicKeyInfo.Algorithm.Name = 1.3.101.112`
- `PublicKeyInfo.KeyType = Unknown`
- `PublicKeyInfo.KeySize = 0`
- `SignatureAlgorithm.Name = 1.3.101.112`

## Scope

- 修改：
  - `src/fafafa.ssl.asn1.pas`
  - `src/fafafa.ssl.x509.pas`
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - `tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
  - `tests/test_x509_ed25519_algorithm_truth.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不把 `Ed25519` 扩成新的 handshake capability 宣称
- 不扩 `Ed448` runtime fixture
- 不重开 broader backend capability redesign

## Architecture Truth

- `TMbedTLSCertificate` /
  `TWolfSSLCertificate`
  的算法元数据 getter 与 `GetInfo`
  都会优先复用 `TX509Certificate`
- 当前 parser 对 `ECDSA` / `RSA`
  已有名称与 key-size truth，
  但 `Ed25519` 仍缺：
  - OID name mapping
  - key type mapping
  - key size mapping
- 因此即便 backend 已经接上 parser truth，
  在 `Ed25519` 证书上仍会继承 parser 残缺
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  里当前还把这条旧残缺写成 active truth，
  需要同步纠正为：
  - capability 仍不发布
  - 但 certificate metadata 已能暴露 `Ed25519` 算法名与 256-bit 公钥大小

## Steps

1. 新增 focused RED：
   - 生成 `Ed25519` 自签名证书
   - 用 `TX509Certificate` 断言：
     - `Algorithm.Name = Ed25519`
     - `KeyType = Ed25519`
     - `KeySize = 256`
     - `SignatureAlgorithm.Name = Ed25519`
2. 最小修复 parser：
   - 在 ASN.1 OID 表补 `Ed25519`
   - 在 `ParsePublicKeyInfo(...)` 补 `Ed25519` key type / size truth
3. 同步更新 MbedTLS dedicated doc contract：
   - 不再把 getter 讲成 RSA 默认值
   - 说明 certificate metadata truth 与 capability truth 的边界
4. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_x509_ed25519_algorithm_truth_units -FEtmp/test_x509_ed25519_algorithm_truth_bin -otmp/test_x509_ed25519_algorithm_truth_bin/test_x509_ed25519_algorithm_truth tests/test_x509_ed25519_algorithm_truth.pas`
   - `./tmp/test_x509_ed25519_algorithm_truth_bin/test_x509_ed25519_algorithm_truth`
   - `bash -n tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
   - `bash tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
   - `git diff --check`

## Expected Result

- `TX509Certificate`
  对 `Ed25519` 证书发布：
  - `PublicKeyInfo.Algorithm.Name = Ed25519`
  - `PublicKeyInfo.KeyType = Ed25519`
  - `PublicKeyInfo.KeySize = 256`
  - `SignatureAlgorithm.Name = Ed25519`
- parser-backed backend
  不再需要把 `Ed25519` 暴露成裸 OID / `Unknown` / `0`
- MbedTLS dedicated matrix
  不再冻结过时的 RSA-default 叙述

## Execution Result

- PASS
- focused RED 首轮稳定打出 4 个失败：
  - `Algorithm.Name`
  - `KeyType`
  - `KeySize`
  - `SignatureAlgorithm.Name`
  都仍停在
  OID / `Unknown` / `0`
- 最小修复后：
  - `tests/test_x509_ed25519_algorithm_truth.pas`
    `7 passed / 0 failed`
  - `tests/test_cert_utils_ed25519_contract.pas`
    `24 passed / 0 failed`
  - `tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
    PASS
- 当前收口后的 public truth：
  - parser-backed certificate metadata
    已能暴露
    `Ed25519`
    算法名
  - `PublicKeyInfo.KeyType`
    已对齐成
    `Ed25519`
  - `PublicKeyInfo.KeySize`
    已对齐成
    `256`
  - MbedTLS dedicated matrix
    现在明确区分：
    - handshake capability 未发布
    - certificate metadata truth 已发布
