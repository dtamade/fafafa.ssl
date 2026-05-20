# Troubleshooting Store Public API Truth

## Goal

把 `docs/guides/TROUBLESHOOTING.md`
里一段仍在用
`ISSLCertificateStore.Open(SSL_STORE_ROOT)`
的旧心智
收回到当前 shipped
public API truth，
避免活跃排障文档继续把
WinSSL concrete-only
能力误教成通用接口能力。

## Scope

- 修改：
  - `docs/guides/TROUBLESHOOTING.md`
  - `tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不改任何 runtime 实现
  - 不改 WinSSL concrete helper 文档
  - 不重开 broader store redesign

## Why This Batch

当前 source truth 很明确：

- `ISSLCertificateStore`
  公共接口只暴露：
  - `LoadFromFile`
  - `LoadFromPath`
  - `LoadSystemStore`
  - `AddCertificate`
  - `FindBy...`
- `Open(...)`
  与
  `SSL_STORE_ROOT`
  只是
  `fafafa.ssl.winssl.certstore`
  的 concrete helper

但活跃
`TROUBLESHOOTING`
  还在把：

- `LStore.Open(SSL_STORE_ROOT);`

写进一个
`ISSLCertificateStore`
  变量示例里。

这会直接把：

- public cross-backend flow
- WinSSL concrete-only flow

混成一条错误心智。

## TDD Steps

1. 先保留 RED 证据：
   - `TROUBLESHOOTING.md`
     当前命中
     `LStore.Open(SSL_STORE_ROOT);`
2. 最小修法：
   - 把该段代码改成
     `LoadSystemStore` +
     `AddCertificate`
     的 public flow
   - 显式补一句语义说明：
     这是给当前进程验证 store
     注入 CA，
     不是持久写系统存储
3. 新增 focused shell contract
   锁住这条活跃文档 truth
4. 跑轻量验证

## Verification

```bash
bash -n tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh
bash tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃 troubleshooting guide
  不再把
  `ISSLCertificateStore`
  教成拥有
  `Open(SSL_STORE_ROOT)`
  的通用接口
- public generic store flow
  与 WinSSL concrete helper flow
  重新分层清楚
- 这条文档 truth
  被 focused contract
  长期锁住
