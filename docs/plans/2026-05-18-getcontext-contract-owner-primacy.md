# `GetContext` Contract Owner Primacy

## Goal

把 `tests/contract/test_backend_contract.pas` 里关于 `GetContext` 的 cross-backend contract 明确改成 `ISSLConnectionInfo.GetContext` 为主 owner、`ISSLConnection.GetContext` 只作为 mirror-equality 对照，让下一批 `GetContext` source/class split feasibility 不再被测试层的隐式双 owner 语义拖住。

## Scope

本批只处理 focused contract、contract-source guard 与台账：

- `tests/contract/test_backend_contract.pas`
- `tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/` 下 public signature
- 不改 backend 连接实现
- 不重跑 compile-all / minimal-ci gates

## Why This Batch

当前 `GetContext` 的 active docs 已经不再教 core getter，但 live coupling 还剩一处关键 residual：

- `tests/contract/test_backend_contract.pas` 仍先把 `ISSLConnection.GetContext` 和 `ISSLConnectionInfo.GetContext` 并列取值
- 失败信息也仍然把它们写成双 owner，而不是 “optional owner + core mirror”

这会让下一批进入 `GetContext` source/class split 时，测试层仍然像是在承认两条同级 owner 路径。

## Planned Changes

1. 把 `ConnectionInfoInterfaceAligned` contract 的 `GetContext` 校验顺序改成：
   - 先验证 `ISSLConnectionInfo.GetContext` 非空且与创建 context type 一致
   - 再验证 `ISSLConnection.GetContext` mirror 非空且与 optional owner 一致
2. 调整失败文案，让它们明确表达 “optional owner + core mirror” 关系。
3. 新增一个 focused source contract，防止测试层又回流到双 owner 叙事。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh
mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract
git diff --check
```

## Expected Outcome

- `GetContext` 的 contract 语义会先承认 `ISSLConnectionInfo` 是当前 owner
- `ISSLConnection.GetContext` 保留为 mirror-equality proof，而不是隐含的同级 owner
- 下一批可以直接讨论 `GetContext` 是否进入更强的 deprecation / removal 路线
