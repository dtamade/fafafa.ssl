# Capability Matrix Guide Current Entrypoint And Backend Truth Alignment

## Goal

修复
`docs/CAPABILITY_MATRIX_GUIDE.md`
里的活跃 capability 指南漂移，
让它重新对齐当前
`v1.5.0`
源码 / API / backend truth：

- 文档版本与支持入口回到当前发布线
- capability 查询示例回到当前公开导入面
- shipped backend 集合不再漏掉
  `sslFreePascal`
- 支持链接 / 反馈入口不再残留 placeholder

这批不改 runtime，
只做：

- active docs truth repair
- 一个静态 contract，
  防止 guide 再次漂回旧版本/旧 backend 集合
- 账本同步

## Why This Batch

上一批收掉
`FAQ` / `COMMON_PITFALLS`
之后，
继续扫描高入口活跃文档时，
最清晰的新 residual
已经落在
`docs/CAPABILITY_MATRIX_GUIDE.md`：

- 头部仍写
  `v1.2.0`
- quickstart 仍拆成
  `uses fafafa.ssl.base, fafafa.ssl.factory`
  而不是当前更高入口的
  `uses fafafa.ssl;`
- 实战示例仍用硬编码 backend 列表，
  漏掉已经 shipped 的
  `sslFreePascal`
- 兼容度 FAQ 只列
  `OpenSSL / WinSSL / WolfSSL / MbedTLS`
  没列
  `FreePascal`
- 反馈链接仍是
  `your-org`
  placeholder

这类 drift
会直接误导：

- capability 审查入口
- backend completeness 判断
- 新调用方对当前 backend 集合的理解

## Scope

- Add:
  - `docs/plans/2026-05-21-capability-matrix-guide-current-entrypoint-and-backend-truth-alignment.md`
  - `tests/scripts/test_capability_matrix_guide_current_truth_contract.sh`
- Update:
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把
   `CAPABILITY_MATRIX_GUIDE`
   头部版本信息
   改成当前
   `v1.5.0`
2. 把 quickstart capability 查询示例
   收回到当前公开导入面：
   - `uses fafafa.ssl;`
   - 保留
     `TSSLFactory.GetLibraryInstance(...)`
     作为当前 library-entrypoint
3. 把 backend 示例从硬编码列表改成：
   `TSSLFactory.GetAvailableLibraries`
   驱动，
   防止继续漏掉
   `sslFreePascal`
4. 把 compatibility FAQ
   改成当前 backend-published truth，
   并补入
   `FreePascal = 64`
5. 修掉
   `your-org`
   placeholder、
   文档版本尾注和更新时间漂移

## Verification

```bash
bash -n tests/scripts/test_capability_matrix_guide_current_truth_contract.sh
bash tests/scripts/test_capability_matrix_guide_current_truth_contract.sh
git diff --check
```

## Expected Result

- `CAPABILITY_MATRIX_GUIDE`
  不再继续发布
  `v1.2.0`
  时代的入口和 backend 集合
- capability 实战示例会自动覆盖当前可用 backend，
  不再静态漏掉
  `sslFreePascal`
- 文档重新成为可信的 capability 入口
