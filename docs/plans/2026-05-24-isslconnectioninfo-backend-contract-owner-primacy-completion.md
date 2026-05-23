# 2026-05-24 ISSLConnectionInfo Backend Contract Owner Primacy Completion

## Goal

把 `tests/contract/test_backend_contract.pas` 中 `GetSelectedALPNProtocol` / `GetStateString`
这两条 `ISSLConnectionInfo` mirror proof 的叙事，收口成和
`GetConnectionInfo` / `GetContext` 一样的 owner-first 语义：

- `ISSLConnectionInfo` 是当前默认 owner
- `ISSLConnection` 上的 getter 只是 compiler-deprecated core mirror

## Why This Batch

上一轮 whole-surface taxonomy 已经把 `ISSLConnectionInfo` family 定性清楚。

当前真正还剩的 drift 很窄：

- active docs 已经优先走 `ISSLConnectionInfo`
- source comments / compiler deprecation 已经对齐
- residual allowlist 也已经冻结
- 但 `tests/contract/test_backend_contract.pas` 里，
  `GetSelectedALPNProtocol` / `GetStateString` 的失败文案仍像是在说
  “optional owner drifted from core getter”

这会让 backend contract 继续泄露双 owner / core-first 心智，
与同一段 contract 里已经修好的 `GetConnectionInfo` / `GetContext` 不一致。

## Scope

只处理：

- `tests/contract/test_backend_contract.pas`
- `tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不处理：

- 不修改 public signature
- 不修改 backend runtime implementation
- 不重跑重型 Pascal suite

## Planned Changes

1. 新增 focused shell contract，要求 backend contract：
   - 对 ALPN / state-string 先承认 `ISSLConnectionInfo` owner
   - 再把 core getter 描述成 mirror proof
   - 不再保留 “optional interface drifted from core getter” 这类旧文案
2. 修改 `tests/contract/test_backend_contract.pas`：
   - 调整 ALPN / state-string 注释
   - 调整失败信息为 owner-first wording
3. 如果验证暴露出新的 `GetSelectedALPNProtocol` 普通 proof drift，则把它们切回 `ISSLConnectionInfo.GetSelectedALPNProtocol`，不要放宽 residual allowlist 去吞掉它。
4. 更新台账并提交

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
git diff --check
```

## Expected Outcome

- `ISSLConnectionInfo` 这组 backend contract mirror proof 终于在同一段 contract 内部讲同一张图
- FreePascal TLS1.3 ALPN runtime proof 不再把 direct core getter 偷渡回 ordinary proof path
- 下一批可以更自然地离开这条 family，转向 diagnostics / session / OCSP 等后续 owner cluster

## Execution Result

- PASS.
- Revalidated the focused owner-primacy completion contracts:
  - `tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
  - `tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - `tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- The family is closed as owner-first contract truth; no runtime implementation edits were needed in this pass.
