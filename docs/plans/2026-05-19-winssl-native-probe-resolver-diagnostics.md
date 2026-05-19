# WinSSL Native Probe Resolver Diagnostics

## Goal

把 `QueryContextAttributesEx*` resolver 的可观测性补齐，并扩大到最小必要的官方候选导出名集合：

- `QueryContextAttributesExW`
- `QueryContextAttributesExA`
- `QueryContextAttributesEx`

同时记录到底是从哪个模块/符号解析成功，还是全部失败后回退到 `QueryContextAttributesW`。

## Scope

- 仅作用于 `tests/winssl/test_winssl_session_resumption.pas` 的 isolated native probe lane
- 不改 canonical shared/public path
- 不改 broader suite 其它 WinSSL 用例

## Files

- Add: `docs/plans/2026-05-19-winssl-native-probe-resolver-diagnostics.md`
- Add: `tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

最新 Windows run `26106025515` 已经给了我们两个高价值事实：

1. probe-side safe-query patch 没有破坏 quick smoke / Wave B gate。
2. native probe 仍失败，而且 fresh log 明确显示：
   - `stage=query_api api=query_context_attributesw`
   - 说明 `QueryContextAttributesEx*` 解析根本没成功

因此当前更值钱的不是继续盲猜 provider 行为，而是先把 resolver 真相打亮。

## Steps

1. 新增 focused contract，锁住候选符号与 resolver marker。
2. 给 proof program 增加：
   - 候选模块/符号遍历
   - 解析结果缓存
   - `stage=query_resolver module=... symbol=... resolved=...`
3. 跑 focused contract、Win64 compile、`git diff --check`。
4. 推送后再跑一条 Windows native-probe manual lane。

## Expected Outcome

- 下一条 Windows runtime log 会明确告诉我们：
  - 是否解析到了 `ExW/A/undecorated`
  - 是从 `secur32.dll` 还是别的模块拿到的
  - 还是所有候选都失败后才回退到 `QueryContextAttributesW`
