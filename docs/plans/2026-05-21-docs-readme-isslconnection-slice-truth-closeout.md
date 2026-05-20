# Docs README ISSLConnection Slice Truth Closeout（2026-05-21）

## Goal

收掉 `docs/README.md` 这个最高可见入口里仍容易误导读者的 `ISSLConnection` 摘要漂移：

- 当前代码块看起来像完整接口签名
- 但它实际上只是“框架集成最常关注的最小关注面”
- 现有 convenience-surface / facade contract 还没有守住这层 README 语义

## Why now

- `API_REFERENCE`、`INTERFACE_DESIGN_V2`、`ARCHITECTURE` 已经逐步形成：
  - `current shipped source truth`
  - `future minimal core target`
  - `convenience-core / connection-adjacent surface`
  这三层区分
- `docs/README.md` 仍缺这层显式说明
- 它是文档中心入口，继续模糊会反复把新读者拉回“`ISSLConnection` 只有这几项”的错误心智

## Scope

- `docs/README.md`
- `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
- `docs/plans/2026-05-21-docs-readme-isslconnection-slice-truth-closeout.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不修改任何 production source
- 不重开 `ISSLConnection` public API 拆分设计
- 不批量重写其它 guide / reference 页面

## Architecture Truth

- `docs/README.md` 这里的用途是：
  - 给读者一个“框架集成时优先关注哪些方法”的入口
  - 不是发布当前完整 `ISSLConnection` 逐行签名
- 当前 shipped source 仍正式公开：
  - `Close`
  - `DoHandshake` / `IsHandshakeComplete` / `Renegotiate`
  - `ReadString` / `WriteString`
  - `SetTimeout` / `GetTimeout`
  - `SetBlocking` / `GetBlocking`
  - 以及多组 optional-owner compatibility mirrors
- 这些完整真相仍以：
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  为准

## Steps

1. 先扩 focused contract，把 `docs/README.md` 也纳入 `ISSLConnection` convenience-surface truth 守护。
2. 运行合同拿到预期 RED。
3. 最小修改 `docs/README.md`：
   - 明确这是最小关注面 / conceptual slice
   - 指回 `docs/reference/API_REFERENCE.md`
   - 说明当前 shipped source 仍保留的关键 connection-adjacent / compatibility-core 方法
4. 跑 focused 合同和相关 README 回归合同。
5. 更新台账并提交。

## Commands

```bash
bash -n tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh
bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh
bash tests/scripts/test_facade_main_entry_truth_contract.sh
git diff --check
git status --short
```

## Expected Result

- `docs/README.md` 不再把最小摘要误教成完整 `ISSLConnection`
- README 入口与 `ARCHITECTURE` / `API_REFERENCE` 说同一张图
- 将来如果 README 再退回模糊摘要，focused contract 会直接报警
