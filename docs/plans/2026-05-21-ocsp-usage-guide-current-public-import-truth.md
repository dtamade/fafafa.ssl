# 2026-05-21 OCSP_USAGE_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/OCSP_USAGE_GUIDE.md`
里仍残留的
`fafafa.ssl.base`
导入，
让这份 specialized owner-surface guide
继续保留
FreePascal client runtime /
server-side stapling issuance /
client online OCSP /
OpenSSL helper
四条边界，
但不再偏离当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-ocsp-usage-guide-current-public-import-truth.md`
- Update:
  - `docs/guides/OCSP_USAGE_GUIDE.md`
  - `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重开 stapled-response / online OCSP 语义
- 不把 OpenSSL helper 工作流改写成 generic facade API

## Architecture Truth

- `OCSP_USAGE_GUIDE`
  当前仍是
  specialized owner-surface
  guide
- 这不等于
  active 示例
  还要继续 import：
  - `fafafa.ssl.base`
- 当前 generic public import truth：
  - `ISSLContext`
  - `ISSLConnection`
  - `ISSLCertificateVerification`
  - `ISSLOCSPStapling`
  - `ISSLServerOCSPStaplingContext`
  - `sslCertVerifyCheckOCSP`
  都可直接来自：
  - `fafafa.ssl`
- `TSSLHTTPHooksScope`
  仍来自：
  - `fafafa.ssl.net.hooks`
- OpenSSL helper types
  仍来自：
  - `fafafa.ssl.openssl.api.*`

## Steps

1. 收紧现有
   `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`：
   - `OCSP_USAGE_GUIDE`
     必须继续解释
     为什么需要回到
     `CreateConnection(...)`
   - active OCSP 示例
     不得继续出现
     `fafafa.ssl.base`
2. 跑 contract，拿到 RED。
3. 最小修改 `OCSP_USAGE_GUIDE.md` 三处导入。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
git diff --check
```

## Expected Result

- `OCSP_USAGE_GUIDE`
  不再继续教学
  `fafafa.ssl.base`
- owner-surface
  理由说明
  与
  OCSP 四条路径边界
  保持不变

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `OCSP_USAGE_GUIDE`
    三段 active 示例
    仍在教学
    `fafafa.ssl.base`
- 最小修复后：
  - FreePascal client runtime
    与
    client online OCSP
    示例
    导入
    已切回：
    - `fafafa.ssl`
    - `fafafa.ssl.context.builder`
  - OpenSSL helper
    workflow
    已移除
    不必要的
    `fafafa.ssl.base`
    import
  - owner-surface
    理由说明
    与
    OCSP 四条边界
    保持不变
- focused verification：
  - `bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
    - PASS
  - `bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
