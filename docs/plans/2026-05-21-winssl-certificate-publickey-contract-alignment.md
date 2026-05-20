# WinSSL Certificate PublicKey Contract Alignment

## Goal

把 `TWinSSLCertificate.GetPublicKey`
从当前的
`SubjectPublicKeyInfo PEM`
导出语义，
收口到当前仓库已经冻结的最小 public contract：

- `GetPublicKey <> ''`
- `GetPublicKey = GetPublicKeyAlgorithm`

避免 `WinSSL`
继续在同一个 `ISSLCertificate`
surface 上，
与
`OpenSSL`
/
`FreePascal`
/
`MbedTLS`
/
`WolfSSL`
发布不同语义。

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `tests/scripts/test_winssl_certificate_publickey_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不在这批恢复完整公钥导出
- 不扩新的 WinCrypt/NCrypt 公钥导出绑定
- 不重开 broader certificate redesign

## Architecture Truth

- 现行仓库 contract 已明确：
  - `OpenSSL.GetPublicKey`
    返回
    `GetPublicKeyAlgorithm`
  - `FreePascal.GetPublicKey`
    也返回
    `GetPublicKeyAlgorithm`
  - `MbedTLS` / `WolfSSL`
    已在
    `2026-05-20 optional backends certificate public surface completeness`
    批次里对齐到同一 contract
- 当前 `WinSSL.GetPublicKey`
  仍单独返回编码后的
  `SubjectPublicKeyInfo`
  PEM 字符串，
  与其它 backend
  public truth 分裂
- 当前最小正确修法
  不是补完整导出，
  而是先收口到现有共享 contract，
  把“完整公钥导出”保留给未来独立专题

## Steps

1. 新增 focused RED：
   - `tests/scripts/test_winssl_certificate_publickey_contract.sh`
   - `tests/winssl/test_winssl_unit_comprehensive.pas`
2. 最小实现修复：
   - `TWinSSLCertificate.GetPublicKey`
     改为返回
     `GetPublicKeyAlgorithm`
3. focused verification：
   - `bash -n tests/scripts/test_winssl_certificate_publickey_contract.sh`
   - `bash tests/scripts/test_winssl_certificate_publickey_contract.sh`
   - `git diff --check`
   - push 后检查：
     - `CI`
     - `WinSSL Runtime Gate`

## Expected Result

- `WinSSL.GetPublicKey`
  与当前仓库其它 backend
  对齐到：
  - 非空
  - 等于
    `GetPublicKeyAlgorithm`
- WinSSL comprehensive runtime suite
  有现成断言，
  后续不会再把这条 surface drift
  重新漏掉

## Execution Result

- local PASS
- focused RED
  先由静态 contract
  干净打出：
  - `WinSSL GetPublicKey is not aligned to GetPublicKeyAlgorithm contract`
- 最小修复后：
  - `TWinSSLCertificate.GetPublicKey`
    已直接收口到
    `GetPublicKeyAlgorithm`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
    已补入
    `GetPublicKey stays aligned with public-key algorithm contract`
    运行时断言
- 当前 focused proof：
  - `bash tests/scripts/test_winssl_certificate_publickey_contract.sh`
    PASS
  - `git diff --check`
    PASS
  - push 后由
    `WinSSL Runtime Gate`
    最终证明这条 runtime truth
