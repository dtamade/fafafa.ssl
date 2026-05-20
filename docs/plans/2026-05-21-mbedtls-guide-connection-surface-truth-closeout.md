# MbedTLS Guide Connection Surface Truth Closeout（2026-05-21）

## Goal

收掉 `docs/guides/MBEDTLS_USER_GUIDE.md` 里仍在活跃传播的连接接口漂移，并同时修复守护脚本本身的 workflow 反向锁定问题：

- 示例仍把 `Connection.GetLastErrorString` 当成当前 `ISSLConnection` API
- 接口摘要仍把 `GetProtocolVersion` 写成 `string`
- 现有 `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh` 还把这两处旧写法当成正确真相

## Why now

- `ISSLServerConnection` 这条活跃文档漂移已经收口，不值得再重开
- `ISSLConnection` 的 Stage-A demotion 也已经有清晰台账
- 目前真正还会误导使用者、并且会让后续审查反复拉起的，是：
  - 高入口 MbedTLS 指南继续发布错误接口
  - contract 绿灯掩盖了这条错误

## Scope

- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
- `docs/plans/2026-05-21-mbedtls-guide-connection-surface-truth-closeout.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不修改任何 backend runtime 实现
- 不重开 `TSSLConfig` / capability dual-truth / `ISSLServerConnection` 设计讨论
- 不批量重写其它 backend 指南

## Architecture Truth

- `ISSLLibrary` 才发布：
  - `GetLastError`
  - `GetLastErrorString`
- `ISSLConnection` 当前 shipped source truth 是：
  - `GetProtocolVersion: TSSLProtocolVersion`
  - `GetCipherName: string`
  - `GetError(ARet: Integer): TSSLErrorCode`
- `ReadString` / `WriteString` 仍是当前 shipped convenience-core surface，可以在 backend raw guide 里展示
- `MBEDTLS_USER_GUIDE` 可以继续展示 direct `CreateConnection(...)` 路径，但不能再发明不存在的连接级错误字符串接口

## Steps

1. 先把 `test_mbedtls_active_docs_capability_truth_contract.sh` 改成当前源码真相。
2. 运行该 contract，先拿到预期 RED。
3. 最小修改 `MBEDTLS_USER_GUIDE.md`：
   - 连接失败改用 `Lib.GetLastError` / `Lib.GetLastErrorString`
   - 接口摘要改成当前 `ISSLConnection` 常用片段，并明确不是完整源码镜像
4. 跑 focused contract 与相关回归 contract。
5. 同步 `task_plan.md` / `findings.md` / `progress.md` 后提交。

## Commands

```bash
bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
bash tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
git diff --check
git status --short
```

## Expected Result

- `MBEDTLS_USER_GUIDE` 不再教授不存在的连接级错误字符串接口
- 指南里的 `ISSLConnection` 摘要不再把协议版本写成错误类型
- 守护脚本不再为旧文档站岗；以后这类 drift 会直接 RED，而不是继续被 workflow 掩盖
