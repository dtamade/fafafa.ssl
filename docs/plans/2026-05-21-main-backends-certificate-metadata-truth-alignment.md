# Main Backends Certificate Metadata Truth Alignment

## Goal

把主 backend 的
`ISSLCertificate`
证书元数据读取面
继续收口到一致的 public truth，
重点解决：

- `OpenSSL.GetInfo`
  仍漏填
  `PublicKeySize`
- `WinSSL.GetSubjectAltNames`
  仍丢
  `email`
  /
  `URI`
- `WinSSL.GetExtendedKeyUsage`
  仍发布
  `OID + 友好名`
  混合语义，
  而不是 shared token truth
- `WinSSL.GetInfo`
  仍漏填
  `PublicKeySize`
  /
  `PathLength`
  /
  `PathLenConstraint`
  /
  `KeyUsage`

避免调用方继续在主 backend 上拿到：

- getter truth 和
  `GetInfo`
  snapshot
  不一致
- `WinSSL`
  的 SAN / EKU
  与其它 backend
  contract 分裂
- 文档声称支持的语义
  与当前实现不符

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certificate.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/openssl/test_openssl_certificate_metadata_truth.pas`
  - `tests/scripts/test_winssl_certificate_metadata_truth_contract.sh`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `tests/certs/san-rich.pem`
  - `docs/reference/ARCHITECTURE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开 broader certificate redesign
- 不扩新的 native API binding
- 不把可选 backend 再拉回这批

## Architecture Truth

- `FreePascal`
  /
  `MbedTLS`
  /
  `WolfSSL`
  当前都通过
  `TX509Certificate`
  对外发布：
  - SAN 纯值数组
  - `KeyUsage`
    token 数组
  - `ExtendedKeyUsage`
    token 数组
  - `GetInfo`
    中的
    `PublicKeySize`
    /
    `IsCA`
    /
    `PathLength`
    /
    `PathLenConstraint`
    /
    `KeyUsage`
    /
    `SubjectAltNames`
- `OpenSSL`
  当前 getter
  已基本对齐，
  但
  `GetInfo`
  仍未把
  `PublicKeySize`
  投影出来
- `WinSSL`
  当前仍残留：
  - SAN native path
    只发
    `DNS/IP`
  - EKU native path
    发
    `OID + friendly name`
  - `GetInfo`
    仍只填
    `SubjectAltNames`
    而没补完整 snapshot 字段
- `tests/certs/san-rich.cnf`
  已经描述了 richer SAN truth，
  需要一个实际证书夹具来固定：
  - DNS
  - IP
  - email
  - URI

## Steps

1. 新增 focused RED：
   - OpenSSL local contract
   - WinSSL static contract
   - WinSSL runtime assertions
2. 最小实现修复：
   - `OpenSSL.GetInfo`
     复用 parser truth
     补齐 metadata snapshot
   - `WinSSL`
     SAN / EKU
     优先走 parser-backed truth，
     并补齐
     `GetInfo`
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certificate_metadata_truth_units -FEtmp/test_openssl_certificate_metadata_truth_bin -otmp/test_openssl_certificate_metadata_truth_bin/test_openssl_certificate_metadata_truth tests/openssl/test_openssl_certificate_metadata_truth.pas`
   - `./tmp/test_openssl_certificate_metadata_truth_bin/test_openssl_certificate_metadata_truth`
   - `bash -n tests/scripts/test_winssl_certificate_metadata_truth_contract.sh`
   - `bash tests/scripts/test_winssl_certificate_metadata_truth_contract.sh`
   - `git diff --check`
   - push 后检查：
     - `CI`
     - `WinSSL Runtime Gate`

## Expected Result

- `OpenSSL.GetInfo`
  不再漏
  `PublicKeySize`
- `WinSSL`
  对 rich SAN / EKU fixture
  发布与其它 backend 一致的 getter truth
- `WinSSL.GetInfo`
  与 getter / parser truth
  对齐
- 架构文档不再描述已删除的旧 fallback

## Execution Result

- completed locally
- shared
  `TX509Certificate`
  已补
  IPv6 SAN
  truth
- `OpenSSL.GetInfo`
  已改为复用 parser snapshot，
  收口
  `PublicKeySize`
  /
  `IsCA`
  /
  `PathLength`
  /
  `PathLenConstraint`
  /
  `KeyUsage`
  /
  `SubjectAltNames`
- `WinSSL`
  已优先复用 parser-backed
  SAN / KU / EKU / `GetInfo`
  truth，
  native decode
  只保留为 fallback
- `docs/reference/ARCHITECTURE.md`
  已移除过时的
  `X509V3_EXT_print`
  SAN fallback 描述，
  并同步成 parser-backed truth
- focused verification:
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certificate_metadata_truth_units -FEtmp/test_openssl_certificate_metadata_truth_bin -otmp/test_openssl_certificate_metadata_truth_bin/test_openssl_certificate_metadata_truth tests/openssl/test_openssl_certificate_metadata_truth.pas`
    - PASS
  - `./tmp/test_openssl_certificate_metadata_truth_bin/test_openssl_certificate_metadata_truth`
    - PASS
  - `bash -n tests/scripts/test_winssl_certificate_metadata_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_winssl_certificate_metadata_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
- push 后继续观察：
  - `CI`
  - `WinSSL Runtime Gate`
