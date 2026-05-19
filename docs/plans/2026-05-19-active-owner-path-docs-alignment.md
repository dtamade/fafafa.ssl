# Active Owner-Path Docs Alignment

## Goal

把当前仍残留 direct-core compatibility-mirror 教学的活跃文档统一切回
`ISSLSessionResumption` / `ISSLDiagnostics` owner path，避免公共接口设计已经
收口后，活跃指南和参考页又把读者带回旧接口心智。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结活跃文档里的 owner-path guidance
- 只修改：
  - `docs/reference/API_REFERENCE.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
  - `docs/reference/WINSSL_DESIGN.md`
- 不改生产实现
- 不扩大到 archive / plans / old reports

## Files

- Add: `docs/plans/2026-05-19-active-owner-path-docs-alignment.md`
- Add: `tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `docs/guides/WINSSL_BEST_PRACTICES.md`
- Modify: `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- Modify: `docs/reference/WINSSL_DESIGN.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前接口设计主线已经明确：

- diagnostics 默认 owner = `ISSLDiagnostics`
- session resumption 默认 owner = `ISSLSessionResumption`
- core `ISSLConnection` mirror 只做 compatibility 保留

但活跃文档里仍有几处 residual drift：

- `API_REFERENCE.md`
  - 仍写“通过 `ISSLConnection.GetHealthStatus` / `GetPerformanceMetrics` /
    `GetDiagnosticInfo` 获取...”
- `WINSSL_BEST_PRACTICES.md`
  - 仍示范 `LConn.GetSession` / `LConn.SetSession`
- `PERFORMANCE_PROFILING_GUIDE.md`
  - 仍示范 `Conn1.GetSession` / `Conn2.SetSession`
- `WINSSL_DESIGN.md`
  - 仍在 warmup 伪代码里写 `LConn.GetSession`

这类 drift 会直接扭曲新读者对接口 owner 的理解，比一般 wording 漂移更接近
“设计回退”。

## Verification

```bash
bash -n tests/scripts/test_active_owner_path_docs_alignment_contract.sh
bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh
npx prettier --write docs/reference/API_REFERENCE.md docs/guides/WINSSL_BEST_PRACTICES.md docs/guides/PERFORMANCE_PROFILING_GUIDE.md docs/reference/WINSSL_DESIGN.md
git diff --check
```

## Expected Outcome

- API 参考页描述回到 `ISSLDiagnostics` owner path
- WinSSL / profiling / design 文档里的 session 示例回到
  `ISSLSessionResumption`
- direct-core mirror 继续只存在于 intentional residual proofs，而不是活跃教学文档
