# 2026-05-21 Active Reference Metadata Truth Alignment

## Goal

收掉当前仍停留在
旧版本/旧日期
元数据快照上的
三份 active
规范/参考文档，
避免这些文档在正文已经有价值的前提下，
仍因为 stale metadata
继续误导
“这是旧规范还是当前规则”。

## Scope

- Add:
  - `tests/scripts/test_active_reference_metadata_truth_contract.sh`
  - `docs/plans/2026-05-21-active-reference-metadata-truth-alignment.md`
- Update:
  - `docs/reference/API_DESIGN_GUIDE.md`
  - `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
  - `docs/guides/CODING_STANDARDS.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改正文 API 设计原则
- 不改错误处理示例语义
- 不改代码规范条目本身
- 不扩张到 `RELEASE_NOTES.md`

## Architecture Truth

- `API_DESIGN_GUIDE`
  当前更像
  设计原则参考，
  不是
  冻结在
  `1.0.0`
  的静态快照
- `ERROR_HANDLING_BEST_PRACTICES`
  当前仍是
  active guide，
  但 metadata
  还停在
  `1.0 / 2025-01-18`
- `CODING_STANDARDS`
  当前仍是
  活跃仓库
  规范入口之一，
  但 metadata
  还停在
  `1.0.0 / 2025-11-26`
- 所以这一批
  更准确的收口
  是：
  - 用当前
    `v1.5.0`
    /
    `2026-05-21`
    口径
    重标 metadata
  - 明确这些文档
    的当前定位

## Steps

1. 新增 focused metadata contract，
   锁定三份文档的：
   - 当前版本
   - 当前更新时间
   - 当前定位/适用范围
2. 用 `HEAD`
   snapshot
   跑合同，
   先拿到 RED。
3. 最小修改三份文档 metadata。
4. 重跑 focused contract，
   并对
   `ERROR_HANDLING_BEST_PRACTICES`
   顺带跑现有小合同，
   再收口 `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_active_reference_metadata_truth_contract.sh
TMP_DIR="$(mktemp -d)" && \
  git show HEAD:docs/reference/API_DESIGN_GUIDE.md > "$TMP_DIR/api_design.md" && \
  git show HEAD:docs/guides/ERROR_HANDLING_BEST_PRACTICES.md > "$TMP_DIR/error_handling.md" && \
  git show HEAD:docs/guides/CODING_STANDARDS.md > "$TMP_DIR/coding_standards.md" && \
  API_DESIGN_DOC="$TMP_DIR/api_design.md" \
  ERROR_GUIDE_DOC="$TMP_DIR/error_handling.md" \
  CODING_DOC="$TMP_DIR/coding_standards.md" \
  bash tests/scripts/test_active_reference_metadata_truth_contract.sh
bash tests/scripts/test_active_reference_metadata_truth_contract.sh
bash tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh
bash tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh
git diff --check
```

## Expected Result

- `API_DESIGN_GUIDE`
  不再停留在
  `1.0.0`
  的静态 header
- `ERROR_HANDLING_BEST_PRACTICES`
  不再停留在
  `1.0 / 2025-01-18`
  的旧 metadata
- `CODING_STANDARDS`
  不再停留在
  `1.0.0 / 2025-11-26`
  的旧 metadata
