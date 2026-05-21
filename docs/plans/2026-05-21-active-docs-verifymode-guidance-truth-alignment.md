# 2026-05-21 Active Docs VerifyMode Guidance Truth Alignment

## Goal

把活跃文档里关于“禁用证书验证”的 public guidance 收成一条清晰口径，避免 `builder` / `config` / `direct-context` 三条入口继续混用 `WithVerifyNone`、`[]`、`[sslVerifyNone]` 到让调用方误解当前 shipped surface。

## Scope

- 修改：
  - `README.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/WINSSL_QUICKSTART.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - `docs/zh/FAQ.md`
  - `docs/zh/快速入门.md`
  - `docs/zh/API参考/概述.md`
  - `tests/scripts/test_active_docs_verifymode_guidance_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不扩到 archive/history/plans 文档
  - 不改变 runtime 语义
  - 不重构 `WithVerifyNone` / `SetVerifyMode(...)` public surface

## Architecture Truth

- `builder` 当前公开入口仍是：
  - `.WithVerifyNone`
- `direct-context` / `config` 当前 runtime truth 上：
  - `SetVerifyMode([])`
  - `VerifyMode := []`
  都表示 no-verify
- 当前 runtime 上：
  - `[]`
  - `[sslVerifyNone]`
  都会落成 no-verify
- 但活跃文档目前混用了：
  - `.WithVerifyNone`
  - `SetVerifyMode([])`
  - `SetVerifyMode([sslVerifyNone])`
  且没有在关键入口处明确说明各 surface 的推荐写法和风险

## Steps

1. 新增 focused docs truth contract，锁定关键活跃文档必须统一的 verify-disable guidance。
2. 最小修改关键活跃文档：
   - builder 示例继续使用 `.WithVerifyNone`
   - config/direct-context 示例统一用 `[]`
   - 在关键入口明确写出：
     - builder 请用 `.WithVerifyNone`
     - config/direct-context 当前 no-verify 语义是 `[]`
     - 生产环境仍应优先启用验证
   - 顺手把仍裸写 `SetVerifyMode([])` 的活跃 WinSSL / 通用 direct-context 指南也补齐同一条说明
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused docs contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_active_docs_verifymode_guidance_truth_contract.sh
bash tests/scripts/test_active_docs_verifymode_guidance_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃文档不再把 `SetVerifyMode([sslVerifyNone])` 当成 direct-context 推荐写法。
- `README` / `API_REFERENCE` 这类高入口文档会明确：
  - builder 禁用验证请用 `.WithVerifyNone`
  - config/direct-context 当前 no-verify 语义是 `[]`
- verify 线上的 public guidance 不再和最近修复过的 runtime / builder / config 真相互相打架。
