# 2026-05-21 STORE_USAGE_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/STORE_USAGE_GUIDE.md`
里仍保留的
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
拆分导入示例，
让这份专题页继续保持
跨平台通用 store 用法
与
WinSSL-specific helper
分层，
但不再偏离当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-store-usage-guide-current-public-import-truth.md`
  - `tests/scripts/test_store_usage_guide_current_public_import_truth_contract.sh`
- Update:
  - `docs/guides/STORE_USAGE_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重开 store public API 设计
- 不把 WinSSL-specific `OpenSystemStore(...)` helper 改写成 generic API

## Architecture Truth

- `STORE_USAGE_GUIDE`
  当前定位是：
  - `ISSLCertificateStore`
    的跨平台最小用法
  - OpenSSL
    自定义 CA
    文件/目录
  - WinSSL
    系统证书存储 helper
- 这不等于
  活跃示例
  还要继续 split：
  - `fafafa.ssl.base`
  - `fafafa.ssl.factory`
- 当前更符合 public truth 的 generic 导入面是：
  - `fafafa.ssl`
- WinSSL-specific
  helper 与
  `SSL_STORE_*`
  常量
  仍来自：
  - `fafafa.ssl.winssl.certstore`

## Steps

1. 新增 focused contract：
   - generic store 示例必须使用
     `fafafa.ssl`
   - WinSSL helper 示例必须保留
     `fafafa.ssl.winssl.certstore`
   - 全文不得继续出现
     `fafafa.ssl.base`
     /
     `fafafa.ssl.factory`
2. 跑 focused contract，拿到 RED。
3. 最小修改 `STORE_USAGE_GUIDE.md` 的导入。
4. 重跑 focused contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_store_usage_guide_current_public_import_truth_contract.sh
bash tests/scripts/test_store_usage_guide_current_public_import_truth_contract.sh
git diff --check
```

## Expected Result

- `STORE_USAGE_GUIDE`
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
  这组旧 split import
- 跨平台 generic store 用法
  与
  WinSSL helper
  继续分层清楚

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `STORE_USAGE_GUIDE`
    活跃示例
    仍在教学
    `fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
    这组旧 split import
- 最小修复后：
  - generic store
    示例
    导入
    已统一切回：
    - `fafafa.ssl`
  - WinSSL-specific
    helper
    继续显式保留：
    - `fafafa.ssl.winssl.certstore`
  - 指南继续保持
    generic store flow
    与
    WinSSL helper
    的分层
- focused verification：
  - `bash -n tests/scripts/test_store_usage_guide_current_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_store_usage_guide_current_public_import_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
