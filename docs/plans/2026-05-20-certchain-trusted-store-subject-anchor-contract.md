# CertChain Trusted Store Subject Anchor Contract

## Goal

修掉 generic
`TSSLCertificateChainVerifier`
在 trusted store
里查找 issuer certificate
时走错查询面的 bug，
确保当 trusted store
直接持有 issuer / trust anchor
时，
`BuildChain`
能按 subject
命中它并构出最小链。

## Scope

- 修改：
  - `src/fafafa.ssl.certchain.pas`
  - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开更大的 trust-anchor 终止语义设计
- 不在这一批里重写 OpenSSL certstore 的 full-chain 终止规则
- 不扩大到 CRL / OCSP / hostname 验证

## Architecture Truth

- 当前
  `TSSLCertificateChainVerifier.FindIssuer`
  对 trusted store
  调的是：
  - `FindByIssuer(IssuerName)`
- 但语义上要找的是：
  - “谁的 subject
     等于当前 cert 的 issuer”
- 这会导致：
  - trusted store
    即使已经持有
    intermediate trust anchor
  - `BuildChain`
    仍然找不到它
- 如果某条调用路径把 leaf
  自己也放进 trusted store，
  这个错误方向还可能把查询带偏到错误证书

## Steps

1. 新增 focused contract：
   - 生成
     root -> intermediate -> leaf
   - 仅把 intermediate
     放进 trusted store
   - 验证 `BuildChain(leaf)`
     能返回
     `leaf -> intermediate`
2. 最小修复：
   - `FindIssuer`
     在 trusted store
     上改用 `FindBySubject`
3. Focused verification：
   - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
   - `git diff --check`

## Expected Result

- generic chain verifier
  不再把 trusted store
  上的 issuer lookup
  查询错方向
- shared chain-building truth
  对“直接信任 intermediate anchor”
  的调用方式更合理

## Execution Result

- PASS
- 新增 focused contract：
  - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
  - 生成
    `root -> intermediate -> leaf`
  - 仅把
    `intermediate`
    放进 trusted store
- 修复：
  - `TSSLCertificateChainVerifier.FindIssuer`
    对 trusted store
    改为调用
    `FindBySubject(IssuerName)`
- focused verification：
  - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
    - PASS
  - `git diff --check`
    - PASS
