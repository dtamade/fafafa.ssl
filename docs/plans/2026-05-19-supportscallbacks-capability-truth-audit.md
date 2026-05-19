# 2026-05-19 SupportsCallbacks Capability Truth Audit

## Goal

继续沿着 interface/backend completeness 主线推进，收口 `SupportsCallbacks` 这个 capability bool 在各 backend 上的真实含义与发布真相，避免出现：

- backend runtime 已经消费 callback，但 capability 仍未发布
- backend 只有 setter / 存储占位，却把 `SupportsCallbacks` 提前发布为 `True`

这种会误导 selector / capability audit / downstream caller 的结构性漂移。

## Scope

- 审查并分类：
  - `OpenSSL`
  - `WinSSL`
  - `FreePascal`
  - `WolfSSL`
  - `MbedTLS`
- 为当前 shipped source 补 focused contracts：
  - source-truth contract
  - runtime capability truth contract
- 只做最小 capability truth 修复
- 不在本批重做 callback API 设计
- 不修改与 callback 无关的 backend 行为

## Files

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `tests/scripts/test_callback_capability_truth_contract.sh`
- `tests/test_backend_callback_capability_truth_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL`
  - 已发布 `SupportsCallbacks = True`
  - verify/password/info callback 都有真实 thunk/runtime wiring
- `WinSSL`
  - verify/info callback 在 connection/runtime path 被真实消费
  - 但 capability 目前未显式发布，存在 source truth drift
- `FreePascal`
  - 当前仅看到 setter / field 存储
  - 未看到 verify/password/info callback 的 runtime use-site
  - 因而当前 `SupportsCallbacks = True` 更像提前发布
- `WolfSSL` / `MbedTLS`
  - 当前同样更接近 setter-only / storage-only
  - 在没有真实 runtime wiring 前，不应发布 `SupportsCallbacks = True`

## Steps

1. 新增 focused contracts，让当前 callback capability drift 先 RED。
2. 把各 backend 的 `SupportsCallbacks` capability truth 明确写回源码。
3. 运行 focused contracts 与最小编译验证。
4. 回写 plan/findings/progress，并作为下一轮 callback runtime completeness 审查基线冻结。

## Commands

```bash
bash -n tests/scripts/test_callback_capability_truth_contract.sh
bash tests/scripts/test_callback_capability_truth_contract.sh
mkdir -p tmp/test_callback_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas
./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract
git diff --check
```

## Expected Result

- `SupportsCallbacks` 只在当前真实发布 callback runtime path 的 backend 上为 `True`
- `FreePascal` / `WolfSSL` / `MbedTLS` 不再把 setter-only placeholder 冒充成 published callback capability
- `WinSSL` capability truth 回到与当前 verify/info runtime wiring 一致的状态
