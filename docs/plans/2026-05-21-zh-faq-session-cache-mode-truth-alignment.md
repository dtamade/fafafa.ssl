# 2026-05-21 zh FAQ Session Cache Mode Truth Alignment

## Goal

修复 `docs/zh/FAQ.md` 里关于会话复用/会话缓存模式的旧写法，避免继续把已不存在的 `sslSessCacheClient` 风格参数教成当前 public API。

## Scope

- 不在本批新增新的 session-cache typed bridge。
- 不把 `TSessionCacheMode` 直接扩成当前 `ISSLContext.SetSessionCacheMode(...)` 的参数类型。
- 只修中文 FAQ 里的 active-doc drift，并明确当前 seam：
  1. `ISSLContext.SetSessionCacheMode(...)` 现在仍接收 `Boolean`
  2. `TSessionCacheMode` / `scm_*` 更适合作为调用方自己的 policy wrapper，而不是当前直接 context 参数

## Files

- `docs/zh/FAQ.md`
- `tests/scripts/test_zh_faq_session_cache_mode_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 `ISSLContext.SetSessionCacheMode(AEnabled: Boolean)` 仍是 shipped source truth。
- 当前 repo 没有把 `TSessionCacheMode` 直接桥接到 `ISSLContext.SetSessionCacheMode(...)`。
- `TSessionCacheMode` 虽然仍由主门面 re-export，但当前更接近 safety/policy type，而不是当前 context public seam。

## Steps

1. 新增 focused contract，先以 RED 固定中文 FAQ 的旧模式值漂移。
2. 最小修复 `docs/zh/FAQ.md`，把 Q12 收回当前 Boolean seam。
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_zh_faq_session_cache_mode_truth_contract.sh
bash tests/scripts/test_zh_faq_session_cache_mode_truth_contract.sh
git diff --check
```

## Expected Outcome

- 中文 FAQ 不再教学 `sslSessCacheClient` 这类旧参数值。
- 中文 FAQ 会明确当前 `SetSessionCacheMode(...)` 是 Boolean seam。
- 后续不会再因为 facade 里还 re-export 了 `TSessionCacheMode`，就误以为它已经是当前 context 直接参数。
