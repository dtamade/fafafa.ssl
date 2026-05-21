# Migration Guide Phase 2.4 TBufferSize Truth Alignment

## Goal

收口
`docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
里关于
`TBufferSize`
的活跃迁移示例漂移，
避免这份仍会被读到的类型安全历史指南
继续把
buffer sizing
讲成
`fafafa.ssl`
当前 TLS context / factory path
里存在的直接 public entrypoint。

## Why This Batch

当前源码与 focused contracts
已经把这条 truth
说得很清楚：

- `TSSLConfig.BufferSize`
  是
  connection-scoped buffering hint
- factory /
  direct-library
  创建路径
  对自定义
  `BufferSize`
  会显式 reject
- 当前推荐入口
  是外围
  socket / stream / transport / app-level
  buffering policy

但
`MIGRATION_GUIDE_PHASE_2.4`
里仍残留两类容易误导的示意：

- 把
  `TBufferSize`
  混进一个看似当前库内统一存在的
  `ConfigureSSLConnection(...)`
  入口
- 在示意代码里继续写
  `SetBuffer(...)`

这会让读者误读成：

- `TBufferSize`
  已经进入当前 TLS builder / factory path
- 或者库里存在一个
  context-level / SSL-level
  buffer-sizing public API

## Scope

- Add:
  - `docs/plans/2026-05-21-migration-guide-phase24-tbuffersize-truth-alignment.md`
  - `tests/scripts/test_migration_guide_phase24_tbuffersize_truth_contract.sh`
- Update:
  - `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 保留
   `TBufferSize`
   作为类型安全单位类型的历史背景说明。
2. 明确补一句当前 truth：
   - 当前 `fafafa.ssl` 没有单独的
     `WithBufferSize(...)`
     /
     `SetBuffer(...)`
     TLS public entrypoint
   - buffer sizing
     应放在外围
     transport / IO
     层
3. 把组合示例改成：
   - typed policy / wrapper boundary
     的示意
   - 不再伪造当前库内
     `SetBuffer(...)`
     路径
4. 用 focused shell contract
   锁住：
   - guide 的当前 truth 注释
   - `SetBuffer(...)`
     不再出现在这份历史 guide 里

## Verification

```bash
bash -n tests/scripts/test_migration_guide_phase24_tbuffersize_truth_contract.sh
bash tests/scripts/test_migration_guide_phase24_tbuffersize_truth_contract.sh
git diff --check
```

## Expected Outcome

- `Phase 2.4`
  历史迁移指南
  不再继续把
  `TBufferSize`
  教成当前
  TLS context / factory
  高入口能力
- `TBufferSize`
  在 repo 内的当前定位
  收敛为：
  - facade 可见的类型安全单位类型
  - 适合调用方自己的
    transport / buffer policy helper
  - 不是当前
    context builder / factory
    的 buffer sizing seam
