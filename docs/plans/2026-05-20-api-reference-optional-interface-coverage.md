# API Reference Optional Public Interface Coverage

## Goal

补齐 `docs/reference/API_REFERENCE.md` 对当前 shipped optional public interfaces 的 canonical 覆盖，避免项目主参考只剩一部分接口、而其余活跃接口只能去二级文档或源码里找。

本批尤其要把这几组 public surface 写回主参考：

- `ISSLHttpHooksAccess`
- `ISSLServerOCSPStaplingContext`
- `ISSLEarlyDataContext`
- `ISSLEarlyDataConnection`
- `ISSLConnectionInfo`
- `ISSLDiagnostics`
- `ISSLSessionResumption`
- `ISSLCertificateVerification`
- `ISSLOCSPStapling`

同时把 server-side 当前建模真相也写进 canonical reference：

- 当前 public Pascal source 尚未声明 `ISSLServerConnection`
- 服务端特有能力主要通过可选 context 扩展接口暴露

## Scope

本批只做 canonical docs completeness 收口，不改 runtime 实现：

- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_api_reference_optional_interface_coverage_contract.sh`
- `docs/plans/2026-05-20-api-reference-optional-interface-coverage.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不新增 `ISSLServerConnection`
- 不调整 backend 能力或 runtime contract
- 不改 secondary guides 的既有 owner-path 示例

## Why This Batch

当前 repo 出现了一个比较隐蔽但高价值的文档完整性问题：

- source / facade 已经导出了多组 optional public interfaces
- 活跃 guide / `API_DOCUMENTATION.md` 也已经在教学这些 surface
- 但 canonical `API_REFERENCE.md` 仍主要只显式列出了 `ISSLNativeHandleAccess`

这会让用户在查“当前 shipped API 全貌”时得到一张不完整的图，尤其会误判：

- server-side optional surface 是否真的公开存在
- early-data / connection-owner / diagnostics / OCSP owner surface 是否只是二级设计概念

## Planned Changes

1. 新增 focused shell contract，冻结 `API_REFERENCE.md` 的 optional-interface 最小覆盖面。
2. 在 `API_REFERENCE.md` 中新增一组 optional public interface sections：
   - http hooks
   - server stapled OCSP context
   - early-data context / connection
   - connection owner surfaces
3. 在 canonical reference 中补一条 server-side truth note：
   - 当前没有 `ISSLServerConnection`
   - server-side 特有能力当前主要通过 context optional surfaces 暴露

## Verification

```bash
bash -n tests/scripts/test_api_reference_optional_interface_coverage_contract.sh
bash tests/scripts/test_api_reference_optional_interface_coverage_contract.sh
git diff --check
```

## Expected Outcome

- `API_REFERENCE.md` 不再只靠二级文档间接提到 optional public interfaces
- server-side optional surface 在 canonical reference 中有明确落点
- 后续再推进 `ISSLServerConnection` / server-side symmetry 时，
  主参考不会继续缺这一层地图
