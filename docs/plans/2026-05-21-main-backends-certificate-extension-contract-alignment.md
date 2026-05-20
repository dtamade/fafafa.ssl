# Main Backends Certificate Extension Contract Alignment

## Goal

把主 backend 里
`ISSLCertificate.GetExtension`
仍然分裂的语义
收口到当前仓库已经被
`FreePascal`
/
`MbedTLS`
/
`WolfSSL`
采用的 parser-backed contract：

- 扩展存在且有原始值时：
  返回无分隔符十六进制
- 扩展存在但无原始值时：
  返回扩展名称

避免调用方继续在不同 backend 上遇到：

- `OpenSSL`
  返回 pretty-printed 文本
- `WinSSL`
  返回带 `:`
  的十六进制
- 其它 backend
  返回 parser-backed hex-or-name truth

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certificate.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/openssl/test_openssl_certificate_extension_contract.pas`
  - `tests/scripts/test_winssl_certificate_extension_contract.sh`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不扩新的 native extension decode binding
- 不重开 extension metadata 家族的 broader redesign
- 不把 `GetExtension`
  提升成结构化扩展 API

## Architecture Truth

- 当前 parser-backed truth
  已经由：
  - `TFreePascalCertificate.GetExtension`
  - `TMbedTLSCertificate.GetExtension`
  - `TWolfSSLCertificate.GetExtension`
  采用
- 这条 truth 当前语义是：
  - `HashToHex(LParser.Extensions[I].Value)`
  - 否则 fallback 到扩展名
- `OpenSSL.GetExtension`
  仍在走
  `X509V3_EXT_print(...)`
  pretty-print 路径
- `WinSSL.GetExtension`
  仍直接把
  `CERT_EXTENSION.Value`
  走
  `BinaryToHexString(...)`
  导出成带 `:`
  的字符串
- 现有稳定夹具：
  - `tests/certificate/test_certs/signer_ecdsa_cert.pem`
  - 已在前序 batch
    被证明包含
    `2.5.29.14`
    Subject Key Identifier

## Steps

1. 新增 focused RED：
   - 本地 OpenSSL contract
   - WinSSL source/runtime static contract
2. 最小实现修复：
   - `OpenSSL` 收回到 parser-backed extension truth
   - `WinSSL` 收回到同一 parser-backed extension truth
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certificate_extension_contract_units -FEtmp/test_openssl_certificate_extension_contract_bin -otmp/test_openssl_certificate_extension_contract_bin/test_openssl_certificate_extension_contract tests/openssl/test_openssl_certificate_extension_contract.pas`
   - `./tmp/test_openssl_certificate_extension_contract_bin/test_openssl_certificate_extension_contract`
   - `bash -n tests/scripts/test_winssl_certificate_extension_contract.sh`
   - `bash tests/scripts/test_winssl_certificate_extension_contract.sh`
   - `git diff --check`
   - push 后检查：
     - `CI`
     - `WinSSL Runtime Gate`

## Expected Result

- `OpenSSL`
  /
  `WinSSL`
  对同一张证书的
  `GetExtension('2.5.29.14')`
  与 parser-backed truth 对齐
- Windows runtime suite
  有确定性断言，
  不会再把这条主 backend surface drift 漏掉

## Execution Result

- completed
- focused RED:
  - `bash tests/scripts/test_winssl_certificate_extension_contract.sh`
    首先稳定失败，
    直接证明
    `WinSSL.GetExtension`
    还在发布
    `BinaryToHexString(...)`
    的带 `:`
    原始 hex
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certificate_extension_contract_units -FEtmp/test_openssl_certificate_extension_contract_bin -otmp/test_openssl_certificate_extension_contract_bin/test_openssl_certificate_extension_contract tests/openssl/test_openssl_certificate_extension_contract.pas`
    第一轮暴露了一个额外 contract hole：
    新测试不该直接引用未导出的
    `TFreePascalCertificate`
- implementation:
  - `OpenSSL.GetExtension`
    已改成与
    `FreePascal`
    /
    `MbedTLS`
    /
    `WolfSSL`
    一致的
    parser-backed
    `hex-or-name`
    truth
  - `OpenSSL.GetSubjectAltNames`
    /
    `GetKeyUsage`
    /
    `GetExtendedKeyUsage`
    的 fallback
    也已一起收回到 parser-backed truth，
    避免旧的 pretty-text 依赖在 native helper 缺失时回归
  - `OpenSSL.VerifyEx`
    strict-chain
    EKU gate
    现在直接依赖
    `GetExtendedKeyUsage`
    的 parser/native truth，
    不再混用旧文本合同
  - `WinSSL.GetExtension`
    已从
    `BinaryToHexString(...)`
    改成同一 parser-backed truth
  - OpenSSL focused test
    与 WinSSL runtime proof
    取预期值时，
    现统一通过
    `CreateFreePascalSSLLibrary.CreateCertificate`
    获取 public parser truth
- verification:
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certificate_extension_contract_units -FEtmp/test_openssl_certificate_extension_contract_bin -otmp/test_openssl_certificate_extension_contract_bin/test_openssl_certificate_extension_contract tests/openssl/test_openssl_certificate_extension_contract.pas`
    - PASS
  - `./tmp/test_openssl_certificate_extension_contract_bin/test_openssl_certificate_extension_contract`
    - PASS
  - `bash -n tests/scripts/test_winssl_certificate_extension_contract.sh`
    - PASS
  - `bash tests/scripts/test_winssl_certificate_extension_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
- follow-up:
  - push 后继续看
    `CI`
    与
    `WinSSL Runtime Gate`
    是否一起保持全绿
