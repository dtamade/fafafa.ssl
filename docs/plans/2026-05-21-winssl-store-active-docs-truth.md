# WinSSL Store Active Docs Truth

## Goal

把活跃
`WinSSL`
文档里两段仍然会误导调用方的
certificate-store
示例重新收紧到当前源码真相，
避免用户继续照着文档写出：

- 不存在的类名
  `TWinSSLCertStore`
- 不属于
  `ISSLCertificateStore`
  的
  `Open(...)`
  调用
- 不属于 public 接口的
  `Certificates`
  枚举心智
- 不存在的
  `Cert.Subject`
  属性式读取

## Scope

- 修改：
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不改任何 WinSSL runtime 实现
  - 不重开 WinSSL runtime gate
  - 不把 backend-specific helper 文档全部改回 generic-only

## Architecture Truth

- 当前公共
  `ISSLCertificateStore`
  只暴露：
  - `LoadSystemStore`
  - `GetCount`
  - `GetCertificate`
  - `FindBy...`
  - `VerifyCertificate`
  - `BuildCertificateChain`
- `TWinSSLCertificateStore`
  才额外拥有：
  - `Open`
  - `Close`
  - `IsOpen`
  - `GetAllCertificates`
- WinSSL helper
  `OpenSystemStore(...)`
  当前返回的是
  `ISSLCertificateStore`，
  因而它适合写进
  “backend-specific helper + public store surface”
  的活跃示例

## TDD Steps

1. 先保留 RED 证据：
   - `WINSSL_BEST_PRACTICES`
     仍写：
     `LStore.Open(SSL_STORE_MY);`
   - `WINSSL_BACKEND_CAPABILITY_MATRIX`
     仍写：
     `TWinSSLCertStore.Open('MY')`
     / `Store.Certificates`
     / `Cert.Subject`
2. 最小修法：
   - `WINSSL_BEST_PRACTICES`
     改成
     `OpenSystemStore(SSL_STORE_MY)`
   - `WINSSL_BACKEND_CAPABILITY_MATRIX`
     改成：
     - `OpenSystemStore(SSL_STORE_MY)`
     - `GetCount`
     - `GetCertificate`
     - `GetSubject`
3. 新增 focused shell contract
   锁住这条 active-doc truth
4. 跑轻量验证

## Verification

```bash
bash -n tests/scripts/test_winssl_store_active_docs_truth_contract.sh
bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃 WinSSL 文档
  不再把错误类名或 concrete-only 成员
  教成普通可抄的示例
- backend-specific helper
  与 public store surface
  的边界重新清楚
- 这条文档真相
  被 focused contract
  长期锁住
