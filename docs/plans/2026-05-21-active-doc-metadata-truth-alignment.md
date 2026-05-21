# 2026-05-21 Active Doc Metadata Truth Alignment

## Goal

收掉当前高可见度 active docs
里仍残留的
旧 footer/header
版本快照，
避免这些文档在内容已经对齐
current truth
之后，
仍继续用过期元数据
把读者带回旧状态。

## Scope

- Add:
  - `tests/scripts/test_active_doc_metadata_truth_contract.sh`
  - `docs/plans/2026-05-21-active-doc-metadata-truth-alignment.md`
- Update:
  - `docs/BACKEND_SELECTION_GUIDE.md`
  - `docs/ARCHITECTURE.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改接口语义
- 不改 capability 叙事
- 不扩张到 `API_DESIGN_GUIDE` / `ERROR_HANDLING_BEST_PRACTICES` / `CODING_STANDARDS`

## Architecture Truth

- `BACKEND_SELECTION_GUIDE`
  头部已经对齐：
  - `v1.5.0`
  - `2026-05-21`
  但 footer
  仍停在：
  - `1.0`
  - `fafafa.ssl v1.3.0+`
  - `2026-02-05`
- `ARCHITECTURE`
  头部已经对齐：
  - `v1.5.0`
  - `2026-05-21`
  但 footer
  仍停在：
  - `1.0`
  - `2026-02-05`
- `MIGRATION_GUIDE_V1.1`
  本质上已经不是
  当前主入口文档，
  而是
  历史 v1.1 / v1.2
  迁移专题，
  但它又被多轮补充成
  current truth
  注释入口，
  所以 footer
  不能继续冒充
  `1.2 / 2026-02-05`
  的静止快照

## Steps

1. 新增 focused contract，
   锁定：
   - `BACKEND_SELECTION_GUIDE`
     footer
   - `ARCHITECTURE`
     footer
   - `MIGRATION_GUIDE_V1.1`
     footer status/date
2. 用 `HEAD`
   snapshot
   跑合同，
   先拿到 RED。
3. 最小修改三份文档的元数据。
4. 重跑 focused contract、
   既有 backend-selection
   import contract、
   `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_active_doc_metadata_truth_contract.sh
TMP_DIR="$(mktemp -d)" && \
  git show HEAD:docs/BACKEND_SELECTION_GUIDE.md > "$TMP_DIR/backend_selection.md" && \
  git show HEAD:docs/ARCHITECTURE.md > "$TMP_DIR/architecture.md" && \
  git show HEAD:docs/MIGRATION_GUIDE_V1.1.md > "$TMP_DIR/migration_v11.md" && \
  BACKEND_SELECTION_DOC="$TMP_DIR/backend_selection.md" \
  ARCHITECTURE_DOC="$TMP_DIR/architecture.md" \
  MIGRATION_V11_DOC="$TMP_DIR/migration_v11.md" \
  bash tests/scripts/test_active_doc_metadata_truth_contract.sh
bash tests/scripts/test_active_doc_metadata_truth_contract.sh
bash tests/scripts/test_backend_selection_guide_current_public_import_truth_contract.sh
git diff --check
```

## Expected Result

- `BACKEND_SELECTION_GUIDE`
  / `ARCHITECTURE`
  不再出现
  header 已更新、
  footer 仍停留旧快照
  的双真相
- `MIGRATION_GUIDE_V1.1`
  会明确回到
  “历史专题 + 当前真相注释”
  的定位
