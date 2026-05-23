# ISSLConnectionInfo Active Guidance De-emphasis

## Goal

把 active docs 中仍把 `ISSLConnection` core mirrors 当推荐路径的示例和说明，统一改成优先走 `ISSLConnectionInfo`，让用户可见指导和刚冻结的 Stage-A demotion 路线真正同向。

## Scope

本批只处理 active docs 与 focused contract：

- `docs/reference/API_REFERENCE.md`
- `docs/INTEGRATION_GUIDE.md`
- `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不改 `src/` 下 public signature
- 不重跑重型 compile-all / minimal-ci gates

## Why This Batch

上一批已经冻结了 `ISSLConnectionInfo` 的 Stage-A demotion map，但 active docs 仍然在直接教：

- `LConn.GetConnectionInfo`
- `LConn.GetSelectedALPNProtocol`
- `LConn.GetStateString`

这会让用户文档继续和设计路线打架，也会降低后续真正收 core 的可执行性。

## Planned Changes

1. 把 `API_REFERENCE.md` 的 “连接信息与状态” 示例改成：
   - 先 `Supports(LConn, ISSLConnectionInfo, ...)`
   - 再从 `ISSLConnectionInfo` 取 connection info / ALPN / state string
2. 把 `INTEGRATION_GUIDE.md` 里关于 ALPN / 排错的示例改成同一条路径。
3. 新增 focused contract，防止 active guidance 回流到 direct core mirror teaching。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh
bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh
git diff --check
```

## Expected Outcome

- active docs 开始主动 de-emphasize `ISSLConnection` core mirrors
- 用户可见教学路径与 `ISSLConnectionInfo` migration map 对齐
- 下一批更适合进入 source-facing slimming prep，而不是继续修文档推荐路径

## Execution Result

- PASS.
- `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh` had one stale exact phrase after `GetContext` joined the same `ISSLConnectionInfo` owner-family wording.
- The contract now expects the current expanded phrase: connection info / context reference / ALPN / state string.
- Revalidated the script with `bash -n` and `bash`; active docs did not need another wording change.
