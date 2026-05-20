# OpenSSL CertStore Full-Chain Termination Contract

## Goal

修掉
`TOpenSSLCertificateStore.BuildCertificateChain`
当前把整个 store
直接当 trusted store
导致 intermediate
被过早当作 trust anchor
的终止语义错误，确保：

- store 只有 intermediate 时，
  仍然返回允许的最小链
  `leaf -> intermediate`
- store 同时有
  `intermediate + root` 时，
  能继续返回完整链
  `leaf -> intermediate -> root`

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certstore.pas`
  - `tests/openssl/test_openssl_certstore_chain_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重写 shared
  `TSSLCertificateChainVerifier`
  的 root 判断模型
- 不扩大到
  OpenSSL 原生
  `X509_verify_cert`
  全路径验证
- 不跑重型全仓 compile gate

## Architecture Truth

- 当前
  `TOpenSSLCertificateStore.BuildCertificateChain`
  直接：
  - `SetTrustedStore(Self)`
  - `BuildChain(ACert, Result)`
- 但 shared verifier
  的终止条件里：
  - `IsRootCertificate`
    只看
    `FTrustedStore.Contains(CurrentCert)`
- 这意味着：
  - 只要 intermediate
    在 store 中
  - 链走到 intermediate
    那一跳时
    就会提前停住
- 这与调用方更自然的
  public contract
  不一致：
  - intermediate
    应该只是中间证书
  - self-signed root
    才应该充当 trust anchor

## TDD Steps

1. 新增 focused contract：
   - 生成
     `root -> intermediate -> leaf`
   - case A：
     store 只放 intermediate，
     期望最小链
     `leaf -> intermediate`
   - case B：
     store 放
     `intermediate + root`，
     期望完整链
     `leaf -> intermediate -> root`
2. 先跑 focused test，
   确认当前实现 RED
3. 最小修复：
   - 在
     `BuildCertificateChain`
     内临时拆分
     self-signed certs
     与 non-self-signed certs
   - self-signed
     -> trusted store
   - non-self-signed
     -> intermediate store
   - verifier options
     显式加上
     `cvoAllowPartialChain`
4. Focused verification：
   - `tests/openssl/test_openssl_certstore_chain_contract.pas`
   - `git diff --check`

## Expected Output

- RED 时：
  - case B
    当前会停在
    `leaf -> intermediate`
- GREEN 后：
  - case A
    返回长度 `2`
  - case B
    返回长度 `3`
  - `git diff --check`
    PASS
