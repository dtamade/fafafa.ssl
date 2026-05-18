# TSSLConfig Active Guidance Cleanup

## Goal

把仍会误导用户的活跃 example / reference guidance 收口到当前 `TSSLConfig` scope truth，避免用户继续从主入口学到 mixed-scope 或 deprecated 配置面。

## Why This Batch

当前 `TSSLConfig` 的 scope buckets、option-bridge truth、compatibility-only surface 都已经冻结，但活跃指导面里仍残留两类漂移：

1. `examples/example_factory_usage.pas`
   - 还在把 `BufferSize` / `HandshakeTimeout` 当成 `TSSLFactory.CreateContext(...)` 的配置字段演示。
2. `docs/reference/ARCHITECTURE.md`
   - 还保留一份过时的伪 `TSSLConfig` 结构，字段名与当前 public source 明显不符。

## Files

- `examples/example_factory_usage.pas`
- `docs/reference/ARCHITECTURE.md`
- `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`

## Scope

- 不改 runtime semantics
- 不改 `TSSLConfig` record shape
- 不重开 `Options vs legacy booleans`
- 只修活跃用户指导面和对应的 focused contract

## Steps

1. 从 example 中移除 `BufferSize` / `HandshakeTimeout` 的错误指导。
2. 明确把 timeout / buffering 导向 connection / transport-level APIs。
3. 把 `reference/ARCHITECTURE.md` 的配置层次改成当前真实 scope buckets。
4. 新增 focused contract，防止 example/reference 漂回旧写法。
5. 做最小验证：contract + example 编译。

## Verification

```bash
bash -n tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh
bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh
mkdir -p tmp/example_factory_usage
fpc -B -Fu./src -Fu./examples -FUtmp/example_factory_usage -FEtmp/example_factory_usage -otmp/example_factory_usage/example_factory_usage examples/example_factory_usage.pas
git diff --check
```

## Expected Outcome

- 活跃 example 不再把 connection-scoped 字段教成 factory/config 主路径
- 活跃架构参考文档不再描述过时或不存在的 `TSSLConfig` 字段
- 下一条路线可以真正进入 `TSSLConfig` slimming / migration design，而不是继续做指导面纠偏
