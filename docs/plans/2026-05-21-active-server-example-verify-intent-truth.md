# 2026-05-21 Active Server Example Verify Intent Truth

## Goal

把活跃文档里会进入 `BuildServer` 的主要 server 示例收成“显式写 verify 意图”的 current truth，避免自动选择快捷方法、PKCS#11 server 示例、错误处理示例继续把 server verify policy 讲成隐式默认。

## Scope

- 修改：
  - `docs/BACKEND_SELECTION_GUIDE.md`
  - `docs/guides/PKCS11_USER_GUIDE.md`
  - `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
  - `docs/reference/API_DOCUMENTATION.md`
  - `docs/reference/PKCS11_ARCHITECTURE.md`
  - `tests/scripts/test_active_server_example_verify_intent_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不重构 preset / validation runtime 语义
  - 不扩到 archive/history/plans 文档
  - 不改变 `.WithVerifyNone` / `.WithMutualTLS(...)` 的 public surface

## Architecture Truth

- `WithSecurityFirst` / `WithPerformanceFirst` / `WithCompatibilityFirst`
  当前只负责：
  - backend requirement / auto-selection
- 它们不会额外决定：
  - client/server verify policy
- server 调用方当前仍必须显式表达 verify 意图：
  - 普通单向 TLS server:
    `.WithVerifyNone`
  - mTLS server:
    `.WithMutualTLS(...)`
    或 direct-context `SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert])`
- 最近已经收口过：
  - helper 默认 verify baseline
  - active no-verify guidance
  现在轮到把活跃 server 示例也讲成同一套真相

## Steps

1. 新增 focused docs truth contract，锁定关键活跃 server 示例必须显式写 verify 意图。
2. 最小修改主要活跃文档：
   - `BACKEND_SELECTION_GUIDE` 明写快捷方法不决定 verify mode
   - 普通 server 示例显式加 `.WithVerifyNone`
   - 必要处补一句如果要 mTLS 请改用 `.WithMutualTLS(...)`
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused docs contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_active_server_example_verify_intent_truth_contract.sh
bash tests/scripts/test_active_server_example_verify_intent_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃 server 示例不再把 verify policy 藏在默认值里。
- 自动选择快捷方法的文档边界更清楚：它们选 backend，不代替 verify 策略。
- 调用方看到 server 示例时，能直接分辨当前是：
  - 普通单向 TLS
  - 还是 mTLS
