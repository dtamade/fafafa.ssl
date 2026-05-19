# 2026-05-20 Facade Optional Owner Surface Export Alignment

## Goal

把 `src/fafafa.ssl.pas` 这个主门面入口收回到“当前活跃文档宣称的 public truth”：

- `uses fafafa.ssl;` 应该足够访问当前已教学的 connection-side optional owner surfaces
- 主门面应显式重导出这些 surface 依赖的 supporting types
- 活跃文档中把 `fafafa.ssl` 作为主入口的示例，不应再因为 alias 缺口被迫回退到 `fafafa.ssl.base`

## Scope

- `src/fafafa.ssl.pas`
- `tests/contract/test_facade_optional_owner_surface_entry.pas`
- `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不重开 broader facade slimming / unit reorganization
- 不处理 backend-private FreePascal seam 的 public remount
- 不重新跑大门禁

## Why This Batch

当前活跃文档已经把 `fafafa.ssl` 当作主入口，但主门面并没有把下列 live optional owner surfaces 全部显式导出：

- `ISSLConnectionInfo`
- `ISSLDiagnostics`
- `ISSLSessionResumption`
- `ISSLCertificateVerification`
- `ISSLOCSPStapling`
- `ISSLCertificateTransparency`
- `ISSLCertificateTransparencyValidation`

同时还缺它们依赖的 supporting types：

- `TSSLHealthStatus`
- `TSSLPerformanceMetrics`
- `TSSLDiagnosticInfo`
- `TSSLCertificateArray`

这不是“文档措辞”问题，而是主门面的实际 public completeness gap。

## Steps

1. 新增 focused shell contract + facade-only 编译 proof，先做 RED
2. 最小补齐 `src/fafafa.ssl.pas` 的 alias 导出
3. 跑 focused contract（含编译 proof）
4. 更新 planning files，收口提交

## Verification

```bash
bash -n tests/scripts/test_facade_optional_owner_surface_export_contract.sh
bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh
git diff --check
```

## Expected Outcome

- `uses fafafa.ssl;` 足以访问当前活跃文档已教学的 connection-side optional owner surfaces
- 主门面不再遗漏这些 surface 的 supporting types
- facade truth 与当前 API reference / quickstart / performance / troubleshooting 方向重新一致
