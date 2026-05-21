# 2026-05-21 MIGRATION_GUIDE_V1.1 façade backend-name 真相对齐

## Goal

修复 `docs/MIGRATION_GUIDE_V1.1.md`
里 façade-only
capability 示例
仍在使用
`SSL_LIBRARY_NAMES[...]`
直取的问题，
让这份迁移指南继续保留：

- runtime-aware
  capability
  叙事
- backend score
  示例
- WinSSL/OpenSSL
  capability
  边界

但不再把调用方
带回
`fafafa.ssl.base`
常量心智。

## Scope

- Add:
  - `docs/plans/2026-05-21-migration-guide-v11-backend-name-truth.md`
- Update:
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重写整份 `MIGRATION_GUIDE_V1.1`
- 不改 capability 语义
- 不扩张到旧页头/占位链接收尾

## Architecture Truth

- `MIGRATION_GUIDE_V1.1`
  当前这段
  backend scoring
  示例
  已经属于
  façade-only
  capability
  说明
- 这不等于
  backend-name
  输出
  还可以继续直取：
  - `SSL_LIBRARY_NAMES[...]`
- 当前 façade 已公开的
  backend-name
  helper
  是：
  - `LibraryTypeToString(...)`
- 所以这段
  façade-only
  capability 示例
  当前应统一使用：
  - `LibraryTypeToString(Result)`

## Steps

1. 收紧现有
   `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`：
   - 继续冻结
     `MIGRATION_GUIDE_V1.1`
     的 runtime-aware
     capability 真相
   - 新增冻结：
     - 示例必须使用：
       - `LibraryTypeToString(Result)`
     - 不得继续出现：
       - `SSL_LIBRARY_NAMES[...]`
2. 用 `HEAD`
   guide snapshot
   跑同一条合同，
   先拿到 RED。
3. 最小修改
   `MIGRATION_GUIDE_V1.1.md`
   的 backend-name
   输出。
4. 重跑 focused contract
   与
   `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
MIGRATION_GUIDE_V11_DOC=/tmp/fafafa_ssl_migration_guide_v11_head.md bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
git diff --check
```

## Expected Result

- `MIGRATION_GUIDE_V1.1`
  façade-only
  capability 示例
  不再继续教学
  `SSL_LIBRARY_NAMES[...]`
- backend-name
  输出
  统一回到：
  - `LibraryTypeToString(...)`

## Execution Result

- PASS
- focused contract
  先补齐了：
  - `MIGRATION_GUIDE_V11_DOC`
    覆盖入口，
    允许同一条
    focused contract
    对
    `HEAD`
    旧版 guide
    做 RED
- focused RED
  通过
  `HEAD`
  snapshot
  真实暴露：
  - active
    backend-scoring
    示例
    仍在使用
    `SSL_LIBRARY_NAMES[...]`
  - 合同输出：
    - `Migration guide must use the public LibraryTypeToString helper for facade-only backend-name output`
- 最小修复后：
  - active
    backend-name
    输出
    已统一回到：
    - `LibraryTypeToString(Result)`
  - runtime-aware
    capability
    叙事
    与
    migration
    结构
    全部保留
- focused verification：
  - `bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - PASS
  - `MIGRATION_GUIDE_V11_DOC=/tmp/fafafa_ssl_migration_guide_v11_head.md bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - FAIL
  - `bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
