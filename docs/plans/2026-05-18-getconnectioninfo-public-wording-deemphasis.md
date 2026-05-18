# `GetConnectionInfo` Public Wording De-emphasis

## Goal

把 `ISSLConnection.GetConnectionInfo` 从“虽然已经是 mirror，但公开表述还不够强”的状态，进一步收紧成统一的 source/doc truth：

- 默认 owner 是 `ISSLConnectionInfo.GetConnectionInfo`
- `ISSLConnection.GetConnectionInfo` 只作为 `v1.x` compatibility-core mirror 保留
- 新代码不再把它当作正常主入口

## Scope

- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产实现
- 不直接加 compiler `deprecated`
- 不重跑重型 Pascal / CI gate

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 `GetConnectionInfo` 注释里补更强 owner / compatibility wording。
2. 在 `API_REFERENCE` 里把 `GetConnectionInfo` 的公开声明、示例和结构说明统一成同一套 de-emphasis 叙事。
3. 在 `INTERFACE_DESIGN_V2` 里去掉“只是仍然存在”的弱表述，改成“仅兼容保留，不再作为新代码推荐入口”。
4. 新增 focused shell contract，守住这组 source/doc wording。

## Verification

```bash
bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh
git diff --check
```

## Expected Outcome

- `GetConnectionInfo` 的 owner/mirror truth 在 source + active docs + v2 design doc 上完全对齐
- 后续不会再从“它还在 core 接口里，所以看起来还是正常入口”这个误区重复拉起
- 下一步可以自然转入第一条真正的 public slimming slice 选择，而不是继续做 wording archaeology

## Result

- `src/fafafa.ssl.base.pas` 的 `GetConnectionInfo` 注释现在明确写出：
  - 默认 owner 为 `ISSLConnectionInfo.GetConnectionInfo`
  - core 侧此入口仅兼容保留
  - 不再作为新代码 primary entry
- `docs/reference/API_REFERENCE.md` 现在在 3 个位置统一同一叙事：
  - `ISSLConnection` 声明块
  - 连接信息示例
  - `TSSLConnectionInfo` 结构说明
- `docs/reference/INTERFACE_DESIGN_V2.md` 不再只写“仍然存在”，而是明确：
  - `GetConnectionInfo` 在 core 上只是 compatibility mirror
  - 默认 owner 已切到 `ISSLConnectionInfo`
- 新增 focused contract：
  - `tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`

## Route Impact

- 这批之后，`GetConnectionInfo` 路线不再需要重复做 wording 对齐
- 默认下一步应进入第一条真正的 public slimming slice 选择：
  1. 评估 `ISSLConnection.GetConnectionInfo` 是否值得进入 compiler-level deprecation feasibility freeze
  2. 或确认 source/doc de-emphasis 已足够，然后把主线切到下一条 mirror 的 slimming 选择
