# Builder Import Guidance Closure

## Goal

收口高入口文档里一个仍然会误导调用方的 public import drift：

- 多处文字说明把
  `TSSLContextBuilder`
  讲得像是
  `fafafa.ssl`
  主门面自带 surface
- 但当前真实公开单元边界仍然是：
  - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
  - `fafafa.ssl.tls`

也就是说，
示例代码已经大多写对了，
但高可见度“入口说明”文字
还没有完全回到当前 shipped import truth。

## Scope

- Update:
  - `docs/guides/FAQ.md`
  - `docs/ARCHITECTURE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不改 runtime 实现
- 不把 builder classes 全量 re-export 到 `fafafa.ssl`
- 不重开 broader facade slimming / unit reorganization

## Why This Batch

当前仓库真相已经明确：

- `TSSLConnector`
  由
  `fafafa.ssl`
  主门面提供
- `TSSLContextBuilder`
  由
  `fafafa.ssl.context.builder`
  提供

但这些高入口文档的说明文字
仍残留类似：

- `uses fafafa.ssl;` + `TSSLContextBuilder` / `TSSLConnector`

这种写法会让读者误以为：

- 只引 `fafafa.ssl`
  就能直接拿到
  `TSSLContextBuilder`

这与当前 source truth 不一致，
属于真正的 public import guidance drift。

## Minimal Fix

1. 扩展现有
   `test_public_unit_import_guidance_truth_contract.sh`
   ，让它要求：
   - 高入口说明明确写出
     `fafafa.ssl, fafafa.ssl.context.builder`
   - 旧的
     `uses fafafa.ssl;` + `TSSLContextBuilder` / `TSSLConnector`
     表述必须消失
2. 把四份高可见度说明文档
   改回当前 shipped import truth
3. 跑 focused contract + `git diff --check`

## Verification

```bash
bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
git diff --check
```

## Expected Outcome

- 高入口说明文字不再把
  `TSSLContextBuilder`
  误教成
  `fafafa.ssl`
  主门面自带 surface
- 调用方可以从文档直接学到当前正确导入组合：
  - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
- 已完成的 public-unit-import guidance 工作不再在标题说明层反弹

## Execution Result

- PASS
- focused RED 首轮证明的是真实文档漂移：
  - `FAQ`
    仍写着
    `uses fafafa.ssl;` + `TSSLContextBuilder` / `TSSLConnector`
- 最小修复后：
  - `docs/guides/FAQ.md`
  - `docs/ARCHITECTURE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
    现已统一回到：
    - `uses fafafa.ssl, fafafa.ssl.context.builder;`
- focused verification：
  - `bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
