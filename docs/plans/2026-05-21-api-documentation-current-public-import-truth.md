# 2026-05-21 API_DOCUMENTATION 当前 public import 真相对齐

## Goal

修复 `docs/reference/API_DOCUMENTATION.md`
里仍保留的
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
拆分导入示例，
让这份高入口 API 参考页继续保留
direct `ISSLConnection`
/
owner-surface
叙事，
但不再偏离当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-api-documentation-current-public-import-truth.md`
- Update:
  - `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - `docs/reference/API_DOCUMENTATION.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重开 `Connect(host, port)` 等旧连接 API 形状问题
- 不扩到 `WINSSL_*` 两份文档
- 不修改 runtime 实现

## Architecture Truth

- `API_DOCUMENTATION`
  当前仍是
  direct connection /
  owner-surface
  高入口参考页
- 这不等于它必须继续 split：
  - `fafafa.ssl.base`
  - `fafafa.ssl.factory`
- 当前更符合 public truth 的导入面是：
  - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
- `ISSLContextBuilder`
  仍来自
  `fafafa.ssl.context.builder`
  单元，
  但
  `ISSLContext`
  /
  `ISSLConnection`
  /
  `ISSLClientConnection`
  /
  `ISSLCertificateVerification`
  /
  `ISSLCertificateTransparency`
  /
  `ISSLCertificateTransparencyValidation`
  等
  已由主门面
  `fafafa.ssl`
  re-export

## Steps

1. 补强现有
   `test_active_connection_api_docs_truth_contract.sh`：
   - `API_DOCUMENTATION`
     必须使用
     `fafafa.ssl`
     +
     `fafafa.ssl.context.builder`
   - 不得继续出现
     `fafafa.ssl.base`
     /
     `fafafa.ssl.factory`
     这组拆分导入
2. 跑 contract，拿到 RED。
3. 最小修改 `API_DOCUMENTATION.md` 两处 import。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
git diff --check
```

## Expected Result

- `API_DOCUMENTATION`
  仍可保持
  direct owner-surface
  参考页定位
- 但导入面会收回到当前 public facade truth，
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
  拆分入口

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `API_DOCUMENTATION`
    活跃示例
    仍在教学
    `fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
    这组旧 split import
- 最小修复后：
  - quick-start
    导入
    已切回：
    - `fafafa.ssl`
    - `fafafa.ssl.context.builder`
  - CT runtime
    配置段导入
    也已切回：
    - `fafafa.ssl`
    - `fafafa.ssl.context.builder`
  - 页面继续保留
    direct connection /
    owner-surface
    参考页定位
- focused verification：
  - `bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_active_connection_api_docs_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
