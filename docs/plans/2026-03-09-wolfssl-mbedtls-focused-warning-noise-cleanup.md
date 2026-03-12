# WolfSSL / MbedTLS Focused Warning Noise Cleanup Plan

**Goal**
- 清掉 WolfSSL / MbedTLS focused compile 路径里的 backend-local warning/noise。
- 保持范围只收口 backend-local 噪音；`src/fafafa.ssl.openssl.api.sha.pas` 的既有 warning 不在本波处理。

**Architecture**
- runtime 真相与上一波 shim / ServerName policy 不变；本波只做 warning-noise 收口，不改行为契约。
- 通过 `tests/scripts/test_wolfssl_mbedtls_focused_warning_cleanup_contract.sh` 把范围锁在 WolfSSL / MbedTLS 本地 warning，避免 unrelated OpenSSL warning 抢占波次。
- 对 managed result 统一采用 `Result := nil` 的零值初始化；对枚举分支补全显式 `else`，避免 FPC case coverage warning。
- 运行时兼容性仍由现有 focused regressions 兜底，确保“静音”不是靠行为漂移换来的。

**Files**
- Add: `docs/plans/2026-03-09-wolfssl-mbedtls-focused-warning-noise-cleanup.md`
- Add: `tests/scripts/test_wolfssl_mbedtls_focused_warning_cleanup_contract.sh`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas`
- Modify: `src/fafafa.ssl.mbedtls.certificate.pas`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 跑 focused warning contract，确认 RED 只剩 backend-local 残点。
2. 修 `managed result` 初始化与 `case` 覆盖残点。
3. 跑 focused contract 与 WolfSSL / MbedTLS runtime regressions。
4. 跑 `python3 -u scripts/compile_all_modules.py` 与 `git diff --check`。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- focused WolfSSL / MbedTLS compile 路径不再输出 backend-local warning/noise。
- runtime compatibility regressions 继续保持绿色。
- warning contract 维持清晰边界：backend-local 噪音在本波清零，unrelated OpenSSL warning 留给独立波次处理。
