# WinSSL Best-Practices Session Truth（2026-05-20）

## Goal
- 把 `docs/guides/WINSSL_BEST_PRACTICES.md` 中 WinSSL session public surface 的当前 truth 写清楚，避免高入口最佳实践页继续把实验性 session surface 误教成默认性能优化路径。
- 同时补上这页 direct `ISSLConnection` / `CreateConnection(...)` /
  `ISSLSessionResumption` 的页面级分类，避免读者把 backend-facing 示例误解成 generic facade 主入口。
- 当前需要锁住的 truth：
  - 这页作为 WinSSL-specific 最佳实践页，会直接展示
    `ISSLConnection` / `CreateConnection(...)` /
    `ISSLSessionResumption`
    这类 backend-facing path
  - 普通跨后端 HTTPS 客户端仍优先使用
    `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
  - WinSSL session public surface 当前仍应按实验性 public surface 理解：
    - `observed_reuse=false`
    - `session_configured=true`
  - 因此 `ISSLSessionResumption.SetSession(...)` 不能直接被写成
    “默认快速握手”或稳定收益

## Why now
- `WINSSL_USER_GUIDE` / `WINSSL_QUICKSTART` 已经收回当前 WinSSL 文档主路径与 session truth。
- `WINSSL_BEST_PRACTICES` 仍保留：
  - `### 2. 启用 Session 复用`
  - `LConn.Connect;  // 快速握手`
  - checklist 里的 `启用 Session 复用`
- 这些表述会把当前实验性 session public surface 误读成已稳定命中的 WinSSL 默认优化路径。

## Scope
- `docs/guides/WINSSL_BEST_PRACTICES.md`
- `tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
- `docs/plans/2026-05-20-winssl-best-practices-session-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不重开 WinSSL runtime/native resumed-handshake 实现调查。
- 不重做既有 WinSSL capability / performance / session evidence lane。

## Approach
1. 新增 focused shell contract，冻结：
   - `WINSSL_BEST_PRACTICES`
     必须明确：
       - 这页是 WinSSL-specific 最佳实践页，会直接展示 backend-facing path
       - 普通跨后端 HTTPS 客户端仍优先 generic facade
       - 当前 dedicated Windows runtime truth 仍是
         `observed_reuse=false` / `session_configured=true`
       - `ISSLSessionResumption` 示例只应按实验性 public surface 理解，
         不能再直接写成“快速握手”或 checklist 默认项
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_winssl_best_practices_session_truth_contract.sh
bash tests/scripts/test_winssl_best_practices_session_truth_contract.sh
bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh
bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- `WINSSL_BEST_PRACTICES` 不再把 WinSSL session public surface 教成默认性能优化路径
- 读者可以清楚知道这页 direct connection/session 示例为什么是 backend-facing path
- 将来如果这页又回漂，focused contract 会立即报警
