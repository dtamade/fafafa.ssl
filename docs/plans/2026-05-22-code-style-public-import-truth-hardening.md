# 2026-05-22 CODE_STYLE Public Import Truth Hardening

## Goal

修复 `docs/guides/CODE_STYLE.md`
这个活跃 style guide
仍在单元结构示例里教学
`fafafa.ssl.base`
的问题，
并补上现有
`test_code_style_and_phase24_safety_doc_truth_contract.sh`
对这类 drift 的漏检，
避免再次出现：

- `CreateConnection(...)`
  语义已经收平
- 但 style guide
  仍把调用方带回
  旧 split import
- focused contract
  却因为只盯连接形态
  继续误报 PASS

## Scope

- Add:
  - `docs/plans/2026-05-22-code-style-public-import-truth-hardening.md`
- Update:
  - `docs/guides/CODE_STYLE.md`
  - `tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh`
  - `docs/plans/2026-05-21-code-style-and-phase24-safety-doc-truth-alignment.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重开 `MIGRATION_GUIDE_PHASE_2.4`
  已收口部分
- 不改 runtime 实现
- 不重跑重型 compile / gate
- 不把 quickstart tree snippet
  的 source-owner 分类问题
  混进这一批

## Architecture Truth

- `CODE_STYLE`
  虽然是风格文档，
  但示例代码会被真实复制到接入代码中，
  所以其 `uses` 代码块属于
  active public import guidance
- 当前普通 public entry
  应回到：
  - `fafafa.ssl`
- 若示例要使用
  `TSSLContextBuilder`
  ，当前 builder entry
  应显式写成：
  - `fafafa.ssl.context.builder`
- 因而：
  - `fafafa.ssl.base`
    不应再出现在
    活跃 style 示例中
  - focused contract
    也不能只冻结
    `CreateConnection(...)`
    而放过 import drift

## Steps

1. 扩当前
   `test_code_style_and_phase24_safety_doc_truth_contract.sh`：
   - `CODE_STYLE`
     必须包含：
     - `fafafa.ssl`
     - `fafafa.ssl.context.builder`
   - `CODE_STYLE`
     不得继续出现：
     - `fafafa.ssl.base;`
2. 先跑 contract，
   在当前 HEAD 上拿到 RED。
3. 最小修改
   `CODE_STYLE.md`
   的单元结构示例。
4. 重跑 focused contract
   与
   `git diff --check`。
5. 在旧
   `2026-05-21`
   plan
   里补一条
   post-closeout correction，
   说明之前只收掉了
   `CreateConnection(...)`
   drift，
   没有真正收掉
   public import drift。

## Verification

```bash
bash -n tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh
bash tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh
git diff --check
```

## Expected Result

- `CODE_STYLE`
  不再继续教学
  `fafafa.ssl.base`
- style guide
  的示例导入
  与当前 façade / builder
  入口真相一致
- 这条 focused contract
  不再只证明
  “连接形态已修”，
  而会同时拦住
  import drift
