# Task Plan - Win64 Cross-Target Compatibility Closeout

## Goal
收口 Linux 侧剩余的 Win64 交叉编译漂移，确认选定的 WinSSL / backend-comparison 路径都能成功生成 Win64 二进制；同时把当前真实边界写清楚：源码与交叉编译面继续前进，但 WinSSL runtime proof 仍然需要可用的 Windows 运行环境。

## Current Batch
1. 先复核 Win64 交叉编译真值：
   - 补拿 `tests/integration/test_backend_comparison.pas` 的最终退出码
   - 确认当前 Linux 主机上 `wine` / `pwsh` 是否能承担 WinSSL runtime proof
2. 然后做最小修复：
   - 修 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
   - 修 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
   - 不重开 FreePascal early-data provider 的行为逻辑
3. 复跑交叉编译与仓库门禁，把新增 compile proof 和 runtime 边界写回台账、计划和状态文档。

## Status
- [completed] 现状重载与风险收敛
- [completed] Win64 交叉编译漂移修复与验证
- [completed] 台账/状态文档写回
- [completed] Review and commit

## Risks
- Linux 主机上的 `compile_all_modules.py` 只覆盖 host-target 编译，抓不到 `-Twin64` 下条件编译坍塌成空 `uses` 这类问题。
- 当前本机 `wine` 直接退出 `159`、`pwsh` 缺失，因此即使 Win64 二进制能生成，也不能把 Linux 侧运行结果包装成 Windows runtime 已验证。
- 这批只修共享 compile surface；如果复跑后暴露新的 Win64 目标错误，必须按 fresh RED 继续最小处理，不能提前宣布 WinSSL 全面完成。

## Follow-up Queue
1. 如果这批交叉编译全部转绿，下一条硬阻塞就是 Windows 主机上的 focused runtime proof。
2. 如果后续还有新的 `-Twin64` compile drift，优先继续补共享 target-conditioned surface，而不是重开无关的 backend 设计。
