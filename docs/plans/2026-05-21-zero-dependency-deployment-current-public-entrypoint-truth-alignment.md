# Zero Dependency Deployment Current Public Entrypoint Truth Alignment

## Goal

修复
`docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
里的旧 public helper / 旧导入面 / 同页显式签名漂移，
让这份 WinSSL 零依赖部署指南重新对齐当前
`v1.5.0`
公开入口真相：

- 不再教学已移除的
  `CreateSSLLibrary(...)`
  /
  `CreateOpenSSLLibrary(...)`
  /
  `CreateWinSSLLibrary(...)`
- 不再要求
  `fafafa.ssl.abstract.*`
  旧单元
- auto-detect 叙述回到当前工厂真相
- 同页明显 compile drift
  一次收掉
  （如 `IsFeatureSupported('SNI')`）

这批不改 runtime，
只做：

- active docs truth repair
- 一个静态 contract，
  防止旧 public helper 再回流到零依赖部署指南
- 账本同步

## Why This Batch

继续沿高入口活跃文档往下扫时，
`docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
命中了成组的旧入口残留：

- 多处仍使用
  `CreateSSLLibrary(...)`
- 同页仍使用
  `fafafa.ssl.abstract.types`
  /
  `fafafa.ssl.abstract.intf`
- auto-detect 示例仍写：
  - `Windows: 优先 WinSSL，回退 OpenSSL`
  - `Linux/macOS: 使用 OpenSSL`
  这种平台硬编码口径
- 诊断示例还在写
  `Lib.IsFeatureSupported('SNI')`
  /
  `('ALPN')`
  这与当前签名
  `IsFeatureSupported(AFeature: TSSLFeature)`
  已不一致
- FAQ 里还残留固定性能数字表，
  这与当前 capability/performance 文档
  “不要把单次 `ms` / `MB/s` 写成长期 truth”
  的口径冲突

## Scope

- Add:
  - `docs/plans/2026-05-21-zero-dependency-deployment-current-public-entrypoint-truth-alignment.md`
  - `tests/scripts/test_zero_dependency_deployment_current_public_entrypoint_truth_contract.sh`
- Update:
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 全页把旧 helper
   收回到：
   - `TSSLFactory.GetLibraryInstance(sslWinSSL)`
   - `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
   - `TSSLFactory.GetLibraryInstance(sslAutoDetect)`
2. 全页把旧导入面
   收回到：
   - `uses fafafa.ssl;`
3. auto-detect 示例明确写成：
   - 按当前注册优先级与可用性选择 backend
   - 不再把 Linux/macOS 硬编码成 OpenSSL
4. 修复同页 compile drift：
   - `Lib.IsFeatureSupported(sslFeatSNI)`
   - `Lib.IsFeatureSupported(sslFeatALPN)`
5. 把固定性能数字表改成：
   - 定性说明
   - 指向当前性能真相源

## Verification

```bash
bash -n tests/scripts/test_zero_dependency_deployment_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_zero_dependency_deployment_current_public_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 零依赖部署指南不再继续教学已移除的 public helper
- 同页示例重新与当前 `fafafa.ssl` 主门面 / `TSSLFactory.*` 真相对齐
- 明显 compile drift
  不再残留在这份活跃文档里
