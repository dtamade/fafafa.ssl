# Troubleshooting WinSSL Session Truth（2026-05-20）

## Goal
- 把 `docs/guides/TROUBLESHOOTING.md` 里 WinSSL session 排障段收回当前 truth，避免高入口故障页继续把 `SetSession(...)` + `Connect` 误教成默认已命中的 resumed-handshake。
- 同时补上这段 direct `CreateConnection(...)` / `ISSLSessionResumption` 示例的页面级分类，说明它是排障时为了观察 connection owner surface 而保留的 direct path，不是普通跨后端 facade 主入口。
- 当前需要锁住的 truth：
  - 这段保留 `ISSLSessionResumption`，是因为排障时要直接观察连接对象上的 session owner surface
  - 普通跨后端 HTTPS 客户端仍优先使用
    `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
  - 当前 dedicated Windows runtime truth 仍应按保守口径理解：
    - `observed_reuse=false`
    - `session_configured=true`
  - 因此 `LResumption2.SetSession(...)` + `LConn2.Connect`
    不能直接被写成“快速复用”或“快速握手”

## Why now
- `README` / `WINSSL_USER_GUIDE` / `WINSSL_BEST_PRACTICES` /
  `PERFORMANCE_PROFILING_GUIDE` 已经分别收回当前 session/direct-path truth。
- `TROUBLESHOOTING.md` 仍保留：
  - `1. **启用 Session 复用**`
  - `// 后续连接 - 快速复用`
  - `LConn2.Connect;  // 快速握手`
- 这些表述会把当前仍偏实验性的 WinSSL session public surface 误读成排障页里的默认成功路径，也会冲掉“这只是 owner-surface 观察路径”的真实边界。

## Scope
- `docs/guides/TROUBLESHOOTING.md`
- `tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh`
- `docs/plans/2026-05-20-troubleshooting-winssl-session-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不删除 `ISSLSessionResumption` 排障示例。
- 不重开 WinSSL runtime/native resumed-handshake 实现调查。
- 不重做已闭合的 WinSSL quickstart / user guide / best-practices / profiling truth 批次。

## Approach
1. 新增 focused shell contract，冻结：
   - `TROUBLESHOOTING.md` 必须明确：
     - 这段 direct `CreateConnection(...)` / `ISSLSessionResumption`
       是排障时为了直接观察 session owner surface
     - 普通跨后端 HTTPS 客户端仍优先 generic facade
     - 当前 dedicated Windows runtime truth 仍是
       `observed_reuse=false` / `session_configured=true`
     - 没有 dedicated Windows / target-specific validation 时，
       不要把 `SetSession(...)` + `Connect`
       直接读成已稳定命中的 resumed-handshake
   - 同时禁止：
     - `1. **启用 Session 复用**`
     - `快速复用`
     - `快速握手`
2. 先跑合同拿到 RED。
3. 只做最小文档修复，保留 owner-path 示例与既有旧合同要求的 API 调用。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh
bash tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh
bash tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh
bash tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh
bash tests/scripts/test_diagnostics_connection_override_classification_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- `TROUBLESHOOTING.md` 不再把 WinSSL session owner surface 误教成默认已命中的 resumed-handshake
- 读者可以清楚知道这段 direct path 为什么存在，以及它和 generic facade 主入口的边界
- 将来如果排障页又回漂，focused contract 会立即报警
