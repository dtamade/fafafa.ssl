# WinSSL CertStore DN Query Runtime Closeout

## Goal

收掉
GitHub Windows runtime
上当前唯一剩余的
`WinSSL CertStore DN Query Contract`
红灯，
确保
`TWinSSLCertificateStore.FindBySubject`
/
`FindByIssuer`
在 WinSSL backend
上继续满足当前 repo
已经对齐过的 shared contract：

- query 先做 DN 归一化
- full DN component query 可命中
- component 顺序不同也可命中
- plain text fragment query 继续可用
- empty query 继续 fail-closed

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.certstore.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开
  chain builder /
  PEM loader /
  fixture path
  这些已完成 lane
- 不在这一批里
  改 public
  `TWinSSLCertificate.GetSubject`
  / `GetIssuer`
  surface
- 不跑本地伪 Windows runtime

## Architecture Truth

- 当前 GitHub Actions
  `WinSSL Runtime Gate`
  run `26139989408`
  已把失败精确收敛到：
  - `按归一化主题片段查询成功`
  - `按归一化颁发者片段查询成功`
- 当前
  `TWinSSLCertificateStore.FindBySubject`
  / `FindByIssuer`
  虽然已经改成
  normalized exact-first
  + substring fallback，
  但 candidate 仍取自：
  - `TWinSSLCertificate.GetSubject`
  - `TWinSSLCertificate.GetIssuer`
- 这两个 getter
  当前走的是
  `CertGetNameStringW(..., CERT_NAME_SIMPLE_DISPLAY_TYPE, ...)`
  更接近 simple display name，
  不是 full X.500 DN
- 所以像：
  - `CN=Test Signer,O=Test Org`
  - `O=Test Org,CN=Test Signer`
  这类 DN component query
  在 WinSSL 上
  即使归一化后
  也无法只靠 simple display name
  匹配成功
- 更安全的最小修复
  是：
  - 在 store 查询内部
    直接从 native
    `CERT_CONTEXT^.pCertInfo^.Subject/Issuer`
    取 full name blob
  - 用
    `CertNameToStrW(..., CERT_X500_NAME_STR ...)`
    生成 canonical candidate
  - 再做
    exact-first
    + component-subset
    + plain substring
    fallback

## TDD Steps

1. 在
   `tests/winssl/test_winssl_certstore.pas`
   把 deterministic query
   调整成逆序 component 变体，
   让 RED
   真正锁到
   order-insensitive DN contract
2. 最小修复
   `src/fafafa.ssl.winssl.certstore.pas`：
   - 增加 native full-DN 提取 helper
   - 增加 component-subset match helper
   - 保留 empty query fail-closed
3. Focused verification：
   - `git diff --check`
   - push 后观察
     `WinSSL Runtime Gate`
     是否转绿

## Expected Result

- WinSSL runtime
  不再只会命中 simple display name
- 当前 deterministic DN query
  在 Windows runner
  上通过
- 当前 repo 的 certstore query contract
  在
  `FreePascal`
  / `OpenSSL`
  / `WinSSL`
  三条主后端上
  更接近一致
