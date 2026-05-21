# 2026-05-22 Repository Guidance Entrypoint Classification Truth

## Goal

把两处仍然容易让贡献者或高级调用方
误读 repo 结构与 public 入口关系的高可见文档
收回到同一套当前真相：

- `docs/guides/WINSSL_QUICKSTART.md`
  的项目结构片段
- `docs/AGENTS.md`
  的仓库结构说明

这批不把它们误当成
“普通 quickstart import drift”，
而是明确收成：

- repo-structure / source-owner 分类真相
- public facade / builder / base owner 边界

## Scope

- Add:
  - `docs/plans/2026-05-22-repository-guidance-entrypoint-classification-truth.md`
  - `tests/scripts/test_repository_guidance_entrypoint_classification_contract.sh`
- Update:
  - `docs/guides/WINSSL_QUICKSTART.md`
  - `docs/AGENTS.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重开 `LINUX_QUICKSTART`
  已收口的普通入口合同
- 不把 tree snippet
  改造成新的 API quickstart
- 不改 runtime 实现
- 不重跑重型 compile / gate

## Architecture Truth

- 当前普通 public 入口
  仍是：
  - `fafafa.ssl.pas`
- 当前推荐 builder 入口
  仍是：
  - `fafafa.ssl.context.builder.pas`
- `fafafa.ssl.factory.pas`
  当前更接近：
  - core factory surface
  - direct-library helper
- `fafafa.ssl.base.pas`
  当前更接近：
  - 底层 source truth
  - supporting types owner
  - 不是普通调用方默认 `uses` 入口

- 因而：
  - `WINSSL_QUICKSTART`
    的源码树片段
    如果继续展示这些文件，
    就必须把角色标清楚，
    并说明
    “这是源码树 owner 分类，
    不是默认导入列表”
  - `docs/AGENTS.md`
    作为贡献者入口，
    也不应只写
    “公共抽象在 `fafafa.ssl.base`”
    而漏掉
    当前主门面与 builder 入口

## Steps

1. 新增 focused contract，冻结：
   - `WINSSL_QUICKSTART`
     必须把 tree snippet
     说明成
     repo-structure / owner 分类
   - `WINSSL_QUICKSTART`
     必须明确：
     - `fafafa.ssl.pas`
     - `fafafa.ssl.context.builder.pas`
     - `fafafa.ssl.factory.pas`
     - `fafafa.ssl.base.pas`
     各自的当前角色
   - `docs/AGENTS.md`
     必须明确：
     - 主门面入口
     - 推荐 builder 入口
     - `base` 是 source truth / supporting types owner
   - `docs/AGENTS.md`
     不得继续保留
     “公共抽象在 `fafafa.ssl.base`”
     这种单句式模糊说法
2. 先跑 contract，拿到 RED。
3. 最小修改两份文档的 wording 与 tree labels。
4. 重跑 focused contract 与 `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_repository_guidance_entrypoint_classification_contract.sh
bash tests/scripts/test_repository_guidance_entrypoint_classification_contract.sh
git diff --check
```

## Expected Result

- `WINSSL_QUICKSTART`
  不再把源码树片段误读成普通导入建议
- `docs/AGENTS.md`
  不再遗漏当前 façade / builder / base 的角色边界
- 贡献者和高级用户看到 repo 结构时，
  能拿到与当前 public-entry truth 一致的分类说明
