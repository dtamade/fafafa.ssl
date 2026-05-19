# Session-Resumption Guide Old-Name Truth Freeze

## Goal

把仍在 active guides 里继续教学旧 session-resumption 连接核心路径的一批高可见文档收口到当前 public 真相：

- `docs/guides/QUICKSTART.md` 不再使用 `GetSessionID` / direct `Conn.GetSession` / `Conn.SetSession` / `Conn.IsSessionResumed`
- `docs/guides/TROUBLESHOOTING.md` 的 WinSSL 复用排障与性能示例不再把 direct core session mirrors 当推荐路径
- `docs/guides/USER_GUIDE.md` 的性能优化示例不再继续教学 `IsSessionResumed`

## Scope

- `docs/guides/QUICKSTART.md`
- `docs/guides/TROUBLESHOOTING.md`
- `docs/guides/USER_GUIDE.md`
- `tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不重开 `API_REFERENCE` / `API_DOCUMENTATION` / `tests/integration` 那批已完成的 `ISSLSessionResumption` owner-path 收口
- 不重跑重型 Pascal/repo gate

## Why This Batch

`ISSLSessionResumption` 的 ordinary active-guidance de-emphasis 之前已经在：

- `docs/reference/API_REFERENCE.md`
- `docs/reference/API_DOCUMENTATION.md`
- `docs/INTEGRATION_GUIDE.md`
- `tests/integration/test_e2e_scenarios.pas`

完成过一轮 focused 收口。

但当前还有三份更高可见的 active guides 继续把 session save/restore/reuse 写成 direct connection-core 调用：

- `QUICKSTART.md` 仍写 `Conn1.GetSessionID` / `Conn1.GetSession` / `Conn2.SetSession` / `Conn2.IsSessionResumed`
- `TROUBLESHOOTING.md` 仍把 `IsSessionResumed` / `SetSession` 当 WinSSL 排障与性能示例的默认路径
- `USER_GUIDE.md` 的性能优化示例还在直接检查 `IsSessionResumed`

所以这批最小正确动作不是改实现，而是把这些 active guides 统一切回已经建立好的 owner truth：

- `Supports(..., ISSLSessionResumption, ...)`
- `ISSLSessionResumption.GetSession`
- `ISSLSessionResumption.SetSession`
- `ISSLSessionResumption.IsSessionReused`

## Planned Changes

1. 新增 focused shell contract，锁住这 3 份 guide 不再回退到旧 session-resumption 名字和 direct connection-core 路径。
2. 更新 `docs/guides/QUICKSTART.md`：
   - Session 保存/恢复/复用示例改走 `ISSLSessionResumption`
3. 更新 `docs/guides/TROUBLESHOOTING.md`：
   - WinSSL Session 复用排障与性能示例改走 `ISSLSessionResumption`
4. 更新 `docs/guides/USER_GUIDE.md`：
   - 性能优化中的“会话已复用”检查改走 owner path

## Verification

```bash
bash -n tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh
bash tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh
git diff --check
```

## Expected Outcome

- active guides stop teaching `GetSessionID` / `IsSessionResumed` / direct `Connection.SetSession` as the recommended path
- session save/restore/reuse examples in guides align with the existing `ISSLSessionResumption` owner truth
- future drift back to stale session-resumption guide names trips a cheap focused contract
