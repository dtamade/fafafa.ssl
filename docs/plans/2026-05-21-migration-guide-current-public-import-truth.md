# 2026-05-21 MIGRATION_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/MIGRATION_GUIDE.md`
里仍残留的
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
split import
与正文入口漂移，
让这份 active migration guide
继续保留
OpenSSL-specific low-level helper
边界说明，
但不再偏离当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-migration-guide-current-public-import-truth.md`
- Update:
  - `docs/guides/MIGRATION_GUIDE.md`
  - `tests/scripts/test_migration_guide_active_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重开 `MIGRATION_GUIDE_V1.1.md`
- 不改 OpenSSL low-level helper 的 backend-specific 定位

## Architecture Truth

- `MIGRATION_GUIDE`
  当前仍是
  active migration
  高入口指南
- 这不等于
  它的 active 示例
  还要继续 split：
  - `fafafa.ssl.base`
  - `fafafa.ssl.factory`
- 当前 generic public import truth
  应该是：
  - `fafafa.ssl`
- `GetFriendlyErrorMessage(...)`
  /
  `GetOpenSSLErrorCategory(...)`
  仍然来自：
  - `fafafa.ssl.openssl.api.err`
  它们是 OpenSSL-specific
  low-level helper，
  不是 generic public facade API

## Steps

1. 收紧现有
   `tests/scripts/test_migration_guide_active_truth_contract.sh`：
   - 正文必须明确：
     新代码优先使用 `fafafa.ssl`
   - OpenSSL low-level helper 示例
     必须使用：
     - `fafafa.ssl`
     - `fafafa.ssl.openssl.api.err`
   - active 示例
     不得继续出现：
     - `fafafa.ssl.base`
     - `fafafa.ssl.factory`
2. 跑 contract，拿到 RED。
3. 最小修改 `MIGRATION_GUIDE.md` 的正文与导入。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_migration_guide_active_truth_contract.sh
bash tests/scripts/test_migration_guide_active_truth_contract.sh
git diff --check
```

## Expected Result

- `MIGRATION_GUIDE`
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
  这组旧 split import
- 正文入口说明
  与
  OpenSSL-specific helper
  边界
  一起回到当前 public truth

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - 正文仍把
    `fafafa.ssl.base`
    说成
    普通新代码
    直接入口之一
  - OpenSSL low-level helper
    示例
    仍在教学
    `fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
    这组旧 split import
- 最小修复后：
  - 正文入口
    已改成：
    - 新代码优先使用 `fafafa.ssl`
    - `fafafa.ssl.base`
      只作为
      source-truth reference
  - OpenSSL low-level helper
    示例
    导入
    已收回到：
    - `fafafa.ssl`
    - `fafafa.ssl.openssl.api.err`
  - OpenSSL-specific
    helper
    边界
    保持不变
- focused verification：
  - `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
