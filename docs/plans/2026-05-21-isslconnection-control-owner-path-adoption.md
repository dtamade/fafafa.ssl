# ISSLConnection Control Owner-Path Adoption

## Goal

继续收口 `ISSLConnection` core-too-fat 这条主残口，但这批只切最小、真实可落地的一刀：

- 为 `SetTimeout` / `GetTimeout`
- 以及 `SetBlocking` / `GetBlocking`

补上正式的 optional owner interface：`ISSLConnectionControl`

让当前 `v1.5.0` shipped truth 从：

- “builder-first + core convenience 仍在”

推进到：

- “builder-first 仍是高入口推荐”
- “connection 创建后也有正式 owner path”
- “core 上这 4 个方法继续兼容保留”

## Why This Batch

上轮静态审查已经确认：

- `ISSLConnectionInfo`
- `ISSLDiagnostics`
- `ISSLSessionResumption`
- `ISSLCertificateVerification`

都已经承接了各自的 owner path；但 `timeout / blocking` 这组
connection-adjacent control surface 还停留在：

- `ISSLConnection` core 方法
- builder/connector/acceptor-first 文档说明

缺口在于：

- 没有正式 optional owner interface
- 仓库内部的 builder / connector 也还直接打 core 方法

这会让 `ISSLConnection` slimming 路线在这 4 个方法上继续卡住。

## Scope

- Add:
  - `docs/plans/2026-05-21-isslconnection-control-owner-path-adoption.md`
  - `tests/scripts/test_isslconnection_control_owner_path_contract.sh`
- Update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.connection.builder.pas`
  - `src/fafafa.ssl.tls.pas`
  - `src/fafafa.ssl.debug.utils.pas`
  - `tests/contract/test_connector_timeout_safety_entry.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `docs/ARCHITECTURE.md`
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 在 `src/fafafa.ssl.base.pas` 新增：
   - `ISSLConnectionControl`
   - owner:
     - `SetTimeout`
     - `GetTimeout`
     - `SetBlocking`
     - `GetBlocking`
2. 保持 `ISSLConnection` 上这 4 个方法不删、不 deprecated。
3. 让 `TBaseSSLConnection` 显式实现 `ISSLConnectionControl`。
4. 让内部高入口开始认 owner path：
   - `TSSLConnectionBuilder`
   - `TSSLConnector`
   - `TSSLAcceptor`
   优先 `Supports(..., ISSLConnectionControl, ...)`，再 fallback 到 core convenience。
5. 更新 canonical docs / audit：
   - `API_REFERENCE`
   - `INTERFACE_DESIGN_V2`
   - `ARCHITECTURE`
   - `INTERFACE_DESIGN_AUDIT_V1.5.0`

## Verification

```bash
bash -n tests/scripts/test_isslconnection_control_owner_path_contract.sh
bash tests/scripts/test_isslconnection_control_owner_path_contract.sh
python3 scripts/compile_all_modules.py
git diff --check
```

Focused contract 内部会验证：

- source 中真的声明了 `ISSLConnectionControl`
- `TBaseSSLConnection` 真的实现了它
- builder / connector / acceptor 已开始优先走 owner path
- docs truth 已同步
- compile/runtime probe 证明：
  - mock connection 暴露 `ISSLConnectionControl`
  - timeout value 通过 builder / connector / acceptor 仍然正确落地

## Expected Result

- `timeout / blocking` 不再只是“文档上说 builder-first”的半收口状态
- `ISSLConnection` slimming 路线在这 4 个方法上拿到正式 owner path
- v1.x 兼容面不破坏
- 后续 residual 更清晰地收敛到：
  - `ReadString` / `WriteString`
  - 以及其他仍留在 core 的 convenience/history baggage
