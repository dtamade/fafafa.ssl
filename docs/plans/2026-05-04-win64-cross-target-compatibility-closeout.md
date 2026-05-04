# Win64 Cross-Target Compatibility Closeout Plan

**Goal:** 收口 Linux 侧剩余的 Win64 交叉编译漂移，确认选定的 WinSSL / backend comparison 路径都能生成 Win64 二进制，并把当前真实边界写清楚：compile surface 继续前进，但 WinSSL runtime proof 仍需 Windows 环境。

**Architecture:** 这批优先做 compile-surface repair，不重开 backend 行为设计。先复跑上一轮未收尾的 `tests/integration/test_backend_comparison.pas` Win64 交叉编译，定位是否还有共享单元漂移；如果有，只做最小 target-conditioned 语法修复；然后复跑交叉编译和仓库门禁，最后把新增 compile proof 与 runtime blocker 写回工作记忆和状态文档。

**Files:**

- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- Modify: `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - confirm whether any shared Win64 cross-target drift remains

Run:

```bash
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_comparison.exe tests/integration/test_backend_comparison.pas
```

Expected:

- 如果全绿，说明 Linux 侧只剩 runtime blocker，需要直接写回文档。
- 如果失败，优先判断是不是 target-conditioned `uses` / unit-path / shared helper 漂移，而不是立刻重开 backend 行为问题。

## Task 2: GREEN - fix only the concrete compile drift

Change:

- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
  - 把只在 `UNIX` 目标需要的 `implementation uses Unix` 整体放进条件编译块
  - 避免 `-Twin64` 时展开成非法的空 `uses`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
  - 同样收紧只在 `UNIX` 目标需要的 `implementation uses Unix`
  - 避免 replay store 家族继续残留同类 compile drift

Constraints:

- 不修改 early-data replay provider 的行为、存储格式或 public API
- 不把这批扩大成新的 FreePascal early-data 功能波次
- 不把 Linux 上的交叉编译成功表述成 Windows runtime 已验证

## Task 3: Verification

Run:

```bash
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_comparison.exe tests/integration/test_backend_comparison.pas
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
wine --version
command -v pwsh
```

Expected:

- `test_backend_comparison.pas` 的 Win64 交叉编译成功
- Linux host-target compile gate 继续全绿
- minimal CI gate 继续全绿
- `wine` / `pwsh` 结果继续把 runtime blocker 的边界说明清楚

## Definition Of Done

- 新发现的 Win64 cross-target compile drift 被最小修复
- `tests/integration/test_backend_comparison.pas` 可以在 Linux 上成功交叉编译到 Win64
- 台账和 WinSSL 状态文档同步到新的 compile truth
- 当前剩余阻塞被明确收敛到 Windows runtime proof
