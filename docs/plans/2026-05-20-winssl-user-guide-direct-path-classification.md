# WinSSL User Guide Direct-Path Classification（2026-05-20）

## Goal
- 把 `docs/guides/WINSSL_USER_GUIDE.md` 中 direct `ISSLConnection` /
  `CreateConnection(...)` 的“使用原因”写得足够明确，避免读者把 WinSSL 用户指南入口页里的 backend-facing 示例误解成 generic facade 主路径。
- 当前需要锁住的 truth：
  - 这页作为 WinSSL-specific 用户指南，会直接展示
    `ISSLConnection` / `CreateConnection(...)` 等 backend-facing path
  - 如果调用方只是普通跨后端 HTTPS 客户端，仍应优先使用通用的
    `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
  - 这里直接写 `CreateConnection(...)` +
    `ISSLClientConnection.SetServerName(...)`，是因为 hostname/SNI
    的 published surface 挂在连接对象上

## Why now
- generic guides、landing quickstarts、backend quickstarts、diagnostics、
  高频专题页、specialized owner-surface 页面，以及 `EARLY_DATA_GUIDE`
  的 direct-path 语义已经逐步收口。
- `WINSSL_USER_GUIDE` 仍是高可见入口页，但还缺一句
  “为什么这里会直接展示 WinSSL-specific / connection-level path，以及 generic main path 仍是什么”。

## Scope
- `docs/guides/WINSSL_USER_GUIDE.md`
- `tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
- `docs/plans/2026-05-20-winssl-user-guide-direct-path-classification.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不改 WinSSL runtime/capability truth。
- 不重做已有 WinSSL performance / session runtime 文档批次。

## Approach
1. 新增 focused shell contract，冻结：
   - `WINSSL_USER_GUIDE`
     必须明确：
       - 这页作为 WinSSL-specific 用户指南，会直接展示
         `ISSLConnection` / `CreateConnection(...)`
       - 普通跨后端 HTTPS 客户端仍优先使用
         `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
       - SNI 配置段落里的 direct `CreateConnection(...)`
         是因为 hostname/SNI 挂在连接对象上
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh
bash tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh
bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh
bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- `WINSSL_USER_GUIDE` 不再让 backend-facing `CreateConnection(...)` 示例看起来像 generic facade 主入口
- 读者可以清楚知道为什么这页会直接展示 WinSSL-specific path
- 将来如果这页又回漂，focused contract 会立即报警
