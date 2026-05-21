# Context Builder Session Timeout Safety Adoption

## Goal

把
`TTimeoutDuration`
从
“connector timeout 已进入高入口”
继续推进到
当前 context 配置最常见的 fluent path：

- `TSSLContextBuilder.WithSessionTimeout(...)`

让 builder 的 session-lifetime 配置
不再只暴露裸秒整数，
而是同时支持明确单位的
`TTimeoutDuration`
overload。

## Why This Batch

当前 repo
已经把
`TTimeoutDuration`
接进了：

- `TSSLConnector`
- `TSSLAcceptor`
- `ISSLConnectionBuilder`

但静态审查说明，
context builder
这条高可见 public path
仍然停留在：

- `WithSessionTimeout(ASeconds: Integer)`

同时活跃 builder 示例也仍在写：

- `.WithSessionTimeout(7200)`

这会让：

- main facade 已 re-export 的 type-safety truth
- TLS context builder 的活跃教学面

继续分叉。

## Current Truth

- `TSSLConfig.SessionTimeout`
  当前 source comment
  明确是：
  context-scoped session lifetime（秒）
- `ISSLContext.SetSessionTimeout(...)`
  当前底层真相也仍是：
  `Integer`
  秒
- builder validation 当前规则：
  - `< 0` -> error
  - `> 86400` -> warning

因此：

- 这批不重构底层 seconds storage
- 只做 builder bridge：
  `TTimeoutDuration -> Integer seconds`
- 并且必须显式锁住语义：
  - 不接受 `Infinite`
  - 不接受非整秒 duration

## Scope

- Add:
  - `docs/plans/2026-05-21-context-builder-session-timeout-safety-adoption.md`
  - `tests/contract/test_context_builder_session_timeout_safety_entry.pas`
  - `tests/scripts/test_context_builder_session_timeout_safety_contract.sh`
- Update:
  - `src/fafafa.ssl.context.builder.pas`
  - `README.md`
  - `docs/reference/API_REFERENCE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. `ISSLContextBuilder`
   增加 overload：
   - `WithSessionTimeout(const ATimeout: TTimeoutDuration)`
2. 保留旧：
   - `WithSessionTimeout(ASeconds: Integer)`
3. bridge 规则：
   - `Infinite` -> reject
   - 非整秒 duration -> reject
   - 可表示的整秒 duration -> `Integer` seconds
   - 超出当前 `Integer` 秒范围 -> reject
4. README / API reference
   里的 builder 示例
   改成 type-safe 写法

## Verification

```bash
bash -n tests/scripts/test_context_builder_session_timeout_safety_contract.sh
bash tests/scripts/test_context_builder_session_timeout_safety_contract.sh
git diff --check
```

contract 脚本内部会完成：

- public overload 静态检查
- docs builder example adoption 静态检查
- compile/run focused probe：
  - typed minutes -> `120` seconds
  - legacy integer -> `90`
  - `1500ms` reject
  - `Infinite` reject

## Expected Result

- `TSSLContextBuilder`
  的 session-lifetime 配置
  不再只停留在裸秒整数
- 活跃 builder 示例
  不再继续教学
  `.WithSessionTimeout(7200)`
- timeout type-safety
  继续从 facade/doc truth
  往真实 context builder path
  再推进一层
