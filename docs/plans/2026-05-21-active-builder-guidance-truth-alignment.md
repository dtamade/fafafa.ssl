# 2026-05-21 Active Builder Guidance Truth Alignment

## Goal

收紧当前活跃文档里关于 `TSSLContextBuilder` 的 public guidance，修复几处“文档里像是现有 API、源码里其实不存在”的假接口教学，避免把后续接口设计推进到错误方向。

## Scope

- 不在本批新增新的 builder fluent API。
- 不把 `TVerificationMode` / `TSessionCacheMode` 直接扩成新的 builder seam，除非当前源码已经存在明确高入口。
- 只修当前活跃文档的 builder/source truth drift：
  1. `PERFORMANCE_PROFILING_GUIDE` 把 `WithSessionCache` 写成了 size/count overload
  2. `security-best-practices` 继续教学若干不存在的 builder 方法

## Files

- `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `tests/scripts/test_active_builder_guides_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 `TSSLContextBuilder` 的 session-cache 高入口只有：
  - `WithSessionCache(AEnabled: Boolean)`
- 当前 session cache size 的公开入口仍在 `ISSLContext`：
  - `SetSessionCacheSize(ASize: Integer)`
- 当前 builder 并不存在这些 fluent 方法：
  - `WithStrongCipherSuites`
  - `WithPerfectForwardSecrecy`
  - `WithSessionTickets`
  - `WithoutVerifyPeer`
  - `WithSSL3`
  - `WithTLS10`
- 当前活跃 builder public truth 更接近：
  - `.WithSafeDefaults`
  - `.WithOption(ssoEnableSessionTickets)`
  - `.WithVerifyNone`
  - `.WithProtocols([...])`

## Steps

1. 新增 focused contract，先用 RED 固定活跃文档里的假接口漂移。
2. 最小修复两份 active guides，只改回当前 shipped source truth。
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused contract 与 `git diff --check`，完成 batch 收口。

## Commands

```bash
bash -n tests/scripts/test_active_builder_guides_truth_contract.sh
bash tests/scripts/test_active_builder_guides_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃文档不再教学不存在的 builder fluent API。
- `WithSessionCache` 的 current truth 会明确回到 `Boolean` 开关。
- session cache size 会明确回到 `ISSLContext.SetSessionCacheSize(...)`。
- 安全最佳实践文档会回到当前真实 builder surface，而不是虚构 fluent method。
