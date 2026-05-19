# Backend Quickstarts Direct-Path Classification（2026-05-20）

## Goal
- 把 backend-specific quickstarts 中 direct `ISSLConnection` 的使用原因讲清楚，避免用户把 backend 深入示例误读成通用 facade 主路径。
- 当前需要压实的 truth：
  - `MBEDTLS_USER_GUIDE` 的简单 HTTPS 示例直接走 `CreateConnection(...)`，是为了展示当前 backend raw shipped surface
  - `WINSSL_QUICKSTART` 聚焦 Windows-native / WinSSL-specific path，因此大量示例直接操作 connection
  - 普通跨后端 HTTPS 客户端仍应优先使用通用的
    `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`

## Why now
- landing docs / generic quickstarts 已经明确：
  - builder + connector + stream 是普通新代码主路径
  - direct `ISSLConnection` 是低层/高级/特定场景入口
- 但 backend-specific high-entry guides 仍缺这层解释，特别是：
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
  - `docs/guides/WINSSL_QUICKSTART.md`

## Scope
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `docs/guides/WINSSL_QUICKSTART.md`
- `tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
- `docs/plans/2026-05-20-backend-quickstarts-direct-path-classification.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 backend runtime 实现。
- 不删除 direct `ISSLConnection` 示例。
- 不重做已经收口的 generic landing docs / session-resumption runtime / capability matrix 线路。

## Approach
1. 新增 focused shell contract，冻结：
   - `MBEDTLS_USER_GUIDE` 必须说明：
     - 当前简单 HTTPS 示例直连 connection 是为了展示 backend raw surface
     - 普通跨后端客户端仍优先 builder + connector + stream
   - `WINSSL_QUICKSTART` 必须说明：
     - 这页聚焦 WinSSL-specific path，所以很多示例直接操作 connection
     - 普通跨后端客户端仍优先通用 facade 主路径
2. 先跑合同拿到 RED。
3. 做最小文档修正。
4. 跑 focused 合同和相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh
bash tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- backend quickstarts 不再把 backend 深入示例误教成通用主路径
- MbedTLS / WinSSL 两份高入口专项指南会明确 direct path 的使用理由
- 将来如果这两页重新把 direct path 写成默认主路径，focused contract 会立即报警

