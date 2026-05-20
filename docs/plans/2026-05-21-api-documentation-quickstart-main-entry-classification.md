# API Documentation Quickstart Main-Entry Classification

## Goal

把 `docs/reference/API_DOCUMENTATION.md` 开头的 `5 分钟上手` 从“看起来像当前唯一推荐主路径”
收口成：

- active API reference 的低层 `ISSLConnection` / owner-surface reference
- 不是普通新代码的默认 bootstrap 入口

避免调用方在最高可见 active API 入口继续把：

- `CreateConnection(...)`
- `ISSLConnection`
- `WriteString / ReadString`

误读成高层文档当前推荐的普通接入路径。

## Scope

只处理 active docs、focused contract 与台账：

- `docs/reference/API_DOCUMENTATION.md`
- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不改 `GETTING_STARTED` / `README` 已经收口的主入口文档
- 不重跑重型 compile / repo gate

## Architecture Truth

- 普通 client / server 接入的推荐主路径已经是：
  - `TSSLConnector`
  - `TSSLAcceptor`
  - `TSSLStream`
- `API_DOCUMENTATION` 这页之所以仍会展示 direct `CreateConnection(...)`，
  是因为它本身是 active API reference，
  后续会继续展开：
  - `ISSLOCSPStapling`
  - `ISSLCertificateVerification`
  等连接侧 owner surface
- 因此正确收口方式不是把 quickstart 全改成 connector 风格，
  而是显式标注：
  - 这是低层 reference 入口
  - 普通新代码优先回到 `GETTING_STARTED`
    / `README`
    的主路径

## Planned Changes

1. 先补 focused contract：
   - 要求 `API_DOCUMENTATION` 说明 quickstart 是低层 direct path
   - 要求它把普通新代码主路径回跳到 `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
   - 要求它解释为什么这页仍直接展示 `CreateConnection(...)`
2. 跑 focused contract，拿到预期 RED。
3. 最小修正文档说明，不重写示例代码形状。
4. 同步台账并提交。

## Verification

```bash
bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
git diff --check
```

## Expected Outcome

- `API_DOCUMENTATION` 开头 quickstart 不再冒充当前默认主入口
- 高可见入口形成一致路线：
  - `README` / `GETTING_STARTED` = ordinary bootstrap path
  - `API_DOCUMENTATION` = active API reference / low-level owner-surface entry

## Result

- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  先新增：
  - quickstart 是 direct low-level reference path
  - ordinary bootstrap flow 回跳到
    `GETTING_STARTED`
  - `CreateConnection(...)`
    在这里继续存在的原因是
    后续 owner-surface 展开
- 首轮 RED 已捕获：
  - `API_DOCUMENTATION quick-start must classify itself as a direct low-level reference path`
- `docs/reference/API_DOCUMENTATION.md`
  现已明确：
  - 开头 `5 分钟上手`
    不是普通新代码唯一推荐入口
  - 普通 client/server 接入
    应优先回到
    `TSSLConnector`
    /
    `TSSLAcceptor`
    /
    `TSSLStream`
  - 这页继续使用
    `CreateConnection(...)`
    是因为它本身还会展开连接侧 owner surface

## Route Impact

- `README`
  /
  `GETTING_STARTED`
  /
  `API_DOCUMENTATION`
  现在对主入口的叙事进一步对齐成：
  - landing / getting-started 负责 ordinary bootstrap
  - active API docs 负责 low-level reference / owner-surface teaching
- 这批之后，
  `API_DOCUMENTATION`
  不再只是在
  `ISSLConnection`
  section 里有 slice / mirror classification，
  连页面最顶部 quickstart 的路线身份也已经讲清楚
