# Platform Support Current Public Entrypoint And Backend Truth Alignment

## Goal

修复
`docs/PLATFORM_SUPPORT.md`
里的高入口平台文档漂移，
让它重新对齐当前
`v1.5.0`
公开入口与 backend truth：

- 不再教学已经移除的
  `CreateSSLLibrary(...)`
- 不再把 backend 列表写窄到只剩
  `OpenSSL / WinSSL`
- 自动后端选择说明与当前工厂真相一致
- macOS 当前发布状态不再继续写成“仍在验证中”

这批不改 runtime，
只做：

- active docs truth repair
- 一个静态 contract，
  防止平台文档再漂回旧 public entrypoint
- 账本同步

## Why This Batch

继续沿高入口活跃文档往下扫时，
`docs/PLATFORM_SUPPORT.md`
暴露出一条非常清晰的旧 public-entrypoint 残留：

- 仍教学
  `CreateSSLLibrary()`
- 仍教学
  `CreateOpenSSLLibrary()`
  /
  `CreateWinSSLLibrary()`
- Linux/macOS/Windows 平台 backend 列表
  仍在静默漏掉
  shipped 的
  `sslFreePascal`
- 自动后端选择优先级表
  也漏掉
  `FreePascal=50`
- macOS 页面顶部已写
  `✅ 已发布`
  但已知问题区仍残留
  “验证进行中 / CI 待完成”

而根据当前已固定的 public truth：

- `CreateSSLLibrary(...)`
  不是当前 shipped source public function
- 当前常见高入口创建路径是：
  - `TSSLFactory.GetLibraryInstance(...)`
  - `TSSLFactory.CreateContext(...)`
  - `TSSLContextBuilder.Create...`
- `sslFreePascal`
  已是当前 shipped backend family，
  不能继续从平台支持页里静默缺席

## Scope

- Add:
  - `docs/plans/2026-05-21-platform-support-current-public-entrypoint-and-backend-truth-alignment.md`
  - `tests/scripts/test_platform_support_current_public_entrypoint_truth_contract.sh`
- Update:
  - `docs/PLATFORM_SUPPORT.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把平台页里的
   `CreateSSLLibrary()`
   /
   `CreateOpenSSLLibrary()`
   /
   `CreateWinSSLLibrary()`
   统一切回：
   `TSSLFactory.GetLibraryInstance(...)`
2. 把 auto-select 说明改成当前真实口径：
   - `TSSLFactory.DetectBestLibrary()`
   - `TSSLFactory.GetLibraryInstance(sslAutoDetect)`
3. 把
   `sslFreePascal`
   补回平台 backend 列表与优先级说明
4. 把 macOS 已知问题收回到当前发布事实，
   不再继续发布“验证进行中”旧状态

## Verification

```bash
bash -n tests/scripts/test_platform_support_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_platform_support_current_public_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 平台支持页不再继续教授不存在的 public factory helpers
- `sslFreePascal`
  重新进入当前平台/backend 叙事
- auto-select / priority / macOS status
  与当前源码/发布口径保持一致
