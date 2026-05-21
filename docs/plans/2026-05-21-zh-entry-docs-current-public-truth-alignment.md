# 2026-05-21 中文高入口文档 current public truth 对齐

## Goal

修复 `docs/zh` 高入口文档族中仍会把中文读者带去旧工厂签名、
旧连接形态、
以及旧系统根证书加载心智的内容，
让中文入口页重新对齐到当前 `v1.5.0` public truth。

## Why Now

当前这组中文文档不是单点 drift，而是一整组会直接误导调用方的旧入口：

1. 多处仍写旧参数顺序：
   `TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient)`
2. 多处仍写旧连接形态：
   - `LContext.CreateConnection;`
   - `LConnection.Connect(AHost, APort)`
3. 多处仍写旧系统根证书加载方式：
   `LoadSystemCertificates`
4. 这些页都属于高入口中文文档：
   - `docs/zh/FAQ.md`
   - `docs/zh/快速入门.md`
   - `docs/zh/安装配置.md`
   - `docs/zh/使用指南/客户端开发.md`
   - `docs/zh/API参考/概述.md`

## Scope

- Add:
  - `docs/plans/2026-05-21-zh-entry-docs-current-public-truth-alignment.md`
  - `tests/scripts/test_zh_entry_docs_current_public_truth_contract.sh`
- Update:
  - `docs/zh/FAQ.md`
  - `docs/zh/快速入门.md`
  - `docs/zh/安装配置.md`
  - `docs/zh/使用指南/客户端开发.md`
  - `docs/zh/API参考/概述.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把普通入口示例切回当前推荐面：
   - `fafafa.ssl`
   - `TSSLContextBuilder`
   - `TSSLConnector`
2. 把 fixed-backend 场景切回当前正确路径：
   - `TSSLContextBuilder.Create.WithBackend(...)`
   - 或
     `TSSLFactory.GetLibraryInstance(...)`
     +
     `Lib.CreateContext(...)`
3. 把低层 direct-connection 示例切回当前真实形态：
   - `CreateConnection(YourConnectedSocket)`
   - `ISSLClientConnection.SetServerName(...)`
   - `Connect`
4. 把系统根证书示例切回当前推荐口径：
   - `WithSystemRoots`
   - 或更明确的 `WithCAFile(...)`

## Verification

```bash
bash -n tests/scripts/test_zh_entry_docs_current_public_truth_contract.sh
bash tests/scripts/test_zh_entry_docs_current_public_truth_contract.sh
git diff --check
```

## Expected Result

- 中文入口文档不再继续发布旧工厂签名
- 中文标准 API 示例不再继续发布不存在的连接形态
- 当前 builder/connector / fixed-backend truth
  在中文入口页里也保持一致
