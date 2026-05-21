# 2026-05-21 CT_IMPLEMENTATION_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
里仍残留的
`fafafa.ssl.base`
导入
与旧版页头快照，
让这份 specialized CT guide
继续保留
CT runtime owner-surface
理由、
离线验证/API
与
日志集成边界，
但不再偏离当前 active guide truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-ct-implementation-guide-current-public-import-truth.md`
- Update:
  - `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
  - `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重开 CT 验证语义
- 不把 `TSCTValidator` / `TCTLogClient` 低层 API 改写成 generic facade API

## Architecture Truth

- `CT_IMPLEMENTATION_GUIDE`
  当前仍是
  specialized CT guide
- 这不等于
  active 示例
  还要继续 import：
  - `fafafa.ssl.base`
- 当前 generic public import truth：
  - `ISSLContext`
  - `ISSLConnection`
  - `ISSLClientConnection`
  - `ISSLCertificateTransparency`
  - `ISSLCertificateTransparencyValidation`
  - `ISSLCertificateVerification`
  - `sslFreePascal`
  都可直接来自：
  - `fafafa.ssl`
- `TSSLHTTPHooksScope`
  仍来自：
  - `fafafa.ssl.net.hooks`
- 当前页头
  不应继续停留在：
  - `1.0`
  - `2026-01-30`
  这类历史快照

## Steps

1. 收紧现有
   `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`：
   - `CT_IMPLEMENTATION_GUIDE`
     必须继续解释
     为什么需要回到
     `CreateConnection(...)`
   - active CT 示例
     必须使用：
     - `fafafa.ssl`
     - `fafafa.ssl.context.builder`
   - active CT 示例
     不得继续出现：
     - `fafafa.ssl.base`
   - 页头不得继续停留在
     `1.0 / 2026-01-30 / v1.0+`
2. 跑 contract，拿到 RED。
3. 最小修改 `CT_IMPLEMENTATION_GUIDE.md` 的页头与导入。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
git diff --check
```

## Expected Result

- `CT_IMPLEMENTATION_GUIDE`
  不再继续教学
  `fafafa.ssl.base`
- specialized
  CT owner-surface
  理由
  与
  `TSCTValidator` /
  `TCTLogClient`
  边界
  保持不变
- 页头回到当前 active guide 口径

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `CT_IMPLEMENTATION_GUIDE`
    仍停在
    `1.0 / 2026-01-30 / v1.0+`
    旧页头快照
  - 两段 active 示例
    仍在教学
    `fafafa.ssl.base`
- 最小修复后：
  - CT guide
    页头
    已切回：
    - `rolling`
    - `2026-05-21`
    - current active CT guidance
  - CT runtime
    与
    CT log-client
    示例
    导入
    已切回：
    - `fafafa.ssl`
    - `fafafa.ssl.context.builder`
    - `fafafa.ssl.net.hooks`
    - `fafafa.ssl.ct.log`
  - CT owner-surface
    理由说明
    与
    validator / log-client
    边界
    保持不变
- focused verification：
  - `bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
    - PASS
  - `bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
