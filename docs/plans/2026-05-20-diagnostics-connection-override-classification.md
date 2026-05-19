# Diagnostics Connection Override Classification（2026-05-20）

## Goal
- 把 active diagnostics / backend guide 中的 connection-level timeout/blocking 示例标回当前 shipped truth：
  - `SetTimeout(...)` / `SetBlocking(...)` 仍然存在
  - 但在这些页面里它们更适合作为 direct-connection 诊断 / override 入口
  - 普通新代码仍优先 builder/connector/acceptor 配置与外围 timer / event-loop 管理

## Why now
- generic guides / landing quickstarts / backend quickstarts 已经分别收口了：
  - convenience text helpers 的分类
  - direct `ISSLConnection` 路径的主次分层
  - backend-specific quickstart 为什么会回到 direct path
- 当前剩余高可见 residual 主要落在诊断类页面：
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
- 这些示例本身不是错，但如果不标明“这是 connection-level diagnostic override”，
  调用方仍会把它们误读成普通主路径配置建议。

## Scope
- `docs/guides/TROUBLESHOOTING.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
- `docs/plans/2026-05-20-diagnostics-connection-override-classification.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不删除 `SetTimeout(...)` / `SetBlocking(...)` 示例。
- 不重开 runtime / CI / backend capability 线路。

## Approach
1. 新增 focused shell contract，冻结：
   - `TROUBLESHOOTING.md` 必须说明：
     - `LConn.SetTimeout(...)` / `LConn.SetBlocking(...)` 在这里属于 direct-connection 诊断 override
     - 普通新代码优先 builder/connector/acceptor 与外围 timer/event-loop
   - `MBEDTLS_USER_GUIDE.md` 的 timeout 故障小节必须说明：
     - `Connection.SetTimeout(...)` 只是 connection-level override
     - 普通跨后端客户端仍优先统一的 builder/connector/transport timer 路线
2. 先跑合同拿到 RED。
3. 只做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_diagnostics_connection_override_classification_contract.sh
bash tests/scripts/test_diagnostics_connection_override_classification_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- diagnostics / backend 故障页面不会再把 connection-level timeout/blocking 误教成普通主路径配置
- `SetTimeout(...)` / `SetBlocking(...)` 在这些页面里的角色被明确固定为 direct-connection diagnostic override
- 将来如果这些页面又回漂，focused contract 会立即报警

