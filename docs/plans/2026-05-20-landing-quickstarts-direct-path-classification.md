# Landing Quickstarts Direct-Path Classification（2026-05-20）

## Goal
- 统一最高入口文档对 direct `ISSLConnection` 路径的分层说明，避免新用户把：
  - `TSSLFactory.CreateContext(...)`
  - `Ctx.CreateConnection(...)`
  - `Conn.Connect`
  误读成普通新代码的首选主路径。
- 当前正确 truth 应明确：
  - 普通客户端/服务端接入优先 `TSSLContextBuilder` + `TSSLConnector` / `TSSLAcceptor` + `TSSLStream`
  - direct `ISSLConnection` 仍是 shipped 的低层/高级/特定场景入口
  - 像 WinSSL session resumption 这类连接级能力示例，之所以回到 direct path，是因为 public capability 当前挂在 connection 上

## Why now
- 上一批已把：
  - `INTEGRATION_GUIDE`
  - `MIGRATION_GUIDE`
  - `USER_GUIDE`
  里的 convenience/helper 语义重新分类。
- 但最高入口文档里仍有 residual：
  - `README.md` 的 `核心 API -> TLS 连接` 代码块直接展示 raw `ISSLConnection`
  - `docs/guides/GETTING_STARTED.md` 的第 4 节直接展示 `ISSLConnection`
  - `docs/guides/QUICKSTART.md` 的 WinSSL session resumption 示例直接走 `CreateConnection(...)`
- 这些例子本身不一定错误，但如果没有显式写明“这是 direct/advanced/specific capability path”，后续仍会把主路径重新带偏。

## Scope
- `README.md`
- `docs/guides/GETTING_STARTED.md`
- `docs/guides/QUICKSTART.md`
- `tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
- `docs/plans/2026-05-20-landing-quickstarts-direct-path-classification.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal public source。
- 不删除 direct `ISSLConnection` 示例。
- 不重开 backend runtime / capability CI。
- 不重做已经收口的 SNI、convenience helper、integration canonical path 批次。

## Approach
1. 新增 focused shell contract，冻结：
   - `README.md` 必须明确：
     - quickstart 主路径是 builder + connector + stream
     - `核心 API -> TLS 连接` 展示的是低层 core surface reference
   - `GETTING_STARTED.md` 必须明确：
     - 第 4 节 direct `ISSLConnection` 是低层/高级入口
     - 普通接入优先 `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
   - `QUICKSTART.md` 必须明确：
     - WinSSL session resumption 示例回到 direct path 的原因是 `ISSLSessionResumption` 当前挂在 connection 上
     - 这不替代前面普通 HTTPS 客户端的 connector + stream 主路径
2. 先跑合同拿到 RED。
3. 只做最小文档修正，不扩大到其它 docs。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh
bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- 新用户先打开的 landing/quickstart 文档不再把 direct `ISSLConnection` 路径误教成推荐主路径
- WinSSL session-resumption 示例会明确说明为什么需要 direct path
- 未来如果 root README / quickstart 又把层级写乱，focused contract 会第一时间报警

