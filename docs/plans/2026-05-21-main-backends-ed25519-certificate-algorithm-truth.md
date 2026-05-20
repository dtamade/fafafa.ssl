# Main Backends Ed25519 Certificate Algorithm Truth

## Goal

把主 backend 里仍然残留的 `Ed25519` 证书算法元数据漂移收口成一致的 public truth，避免调用方在已加载 `Ed25519` 证书后继续看到：

- `OpenSSL.GetPublicKeyAlgorithm = Unknown`
- `OpenSSL.GetInfo.PublicKeyAlgorithm = Unknown`
- `WinSSL.GetPublicKeyAlgorithm = 1.3.101.112`
- `WinSSL.GetSignatureAlgorithm = 1.3.101.112`

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certificate.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/openssl/test_openssl_ed25519_certificate_algorithm_truth.pas`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开已完成的 parser / `TX509Certificate` lane
- 不顺手扩其它未证明的算法家族
- 不重跑整仓重型 gate，只做 focused Linux proof + push 后 Windows CI proof

## Architecture Truth

- `TX509Certificate` 现在已经能把 `Ed25519` 发布成：
  - `PublicKeyInfo.Algorithm.Name = Ed25519`
  - `PublicKeyInfo.KeyType = Ed25519`
  - `SignatureAlgorithm.Name = Ed25519`
- `OpenSSL` 当前 `GetSignatureAlgorithm` 已能给出可读 `ED25519`，
  但 `GetPublicKeyAlgorithm` 仍只映射 `RSA/DSA/DH/EC`
- `WinSSL` 当前 `GetPublicKeyAlgorithm` / `GetSignatureAlgorithm`
  仍直接暴露裸 OID 字符串
- 仓库已有稳定的 `Ed25519` 证书生成能力：
  - `TCertificateUtils.GenerateSelfSigned`
  - `tests/test_cert_utils_ed25519_contract.pas`

## Steps

1. 新增 focused RED：
   - `tests/openssl/test_openssl_ed25519_certificate_algorithm_truth.pas`
   - `tests/winssl/test_winssl_unit_comprehensive.pas`
2. 仅做最小实现修复：
   - `OpenSSL.GetPublicKeyAlgorithm` 补 `Ed25519` / `Ed448`
   - `WinSSL` 对算法 OID 走 `OIDToName(...)`，未知时再 fallback 到原始 OID
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_ed25519_certificate_algorithm_truth_units -FEtmp/test_openssl_ed25519_certificate_algorithm_truth_bin -otmp/test_openssl_ed25519_certificate_algorithm_truth_bin/test_openssl_ed25519_certificate_algorithm_truth tests/openssl/test_openssl_ed25519_certificate_algorithm_truth.pas`
   - `./tmp/test_openssl_ed25519_certificate_algorithm_truth_bin/test_openssl_ed25519_certificate_algorithm_truth`
   - `git diff --check`
   - push 后检查：
     - `CI`
     - `WinSSL Runtime Gate`

## Expected Result

- `OpenSSL` 对生成的 `Ed25519` 证书发布：
  - `GetPublicKeyAlgorithm = Ed25519`
  - `GetInfo.PublicKeyAlgorithm = Ed25519`
- `WinSSL` 对生成的 `Ed25519` 证书发布：
  - `GetPublicKeyAlgorithm = Ed25519`
  - `GetSignatureAlgorithm = Ed25519`
  - `GetInfo.PublicKeyAlgorithm = Ed25519`
  - `GetInfo.SignatureAlgorithm = Ed25519`
- Windows proof 只依赖现有 GitHub Actions，不再反复本地拉起不可执行验证

## Execution Result

- local PASS
- focused RED 首轮稳定打出 2 个 OpenSSL 失败：
  - `GetPublicKeyAlgorithm`
    仍是
    `Unknown`
  - `GetInfo.PublicKeyAlgorithm`
    仍是
    `Unknown`
- 最小修复后：
  - `OpenSSL.GetPublicKeyAlgorithm`
    已补
    `Ed25519`
    /
    `Ed448`
    映射
  - `WinSSL.GetPublicKeyAlgorithm`
    /
    `GetSignatureAlgorithm`
    已改为：
    - 优先发布 `OIDToName(...)`
    - 未知时再 fallback 到原始 OID
  - `tests/openssl/test_openssl_ed25519_certificate_algorithm_truth.pas`
    `9 passed / 0 failed`
- 当前附加 proof：
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
    已补入生成式
    `Ed25519`
    运行时断言，
    push 后应由
    `WinSSL Runtime Gate`
    最终证明
