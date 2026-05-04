# Task Plan - WinSSL Documentation Truth Alignment

## Goal
把 WinSSL 设计文档和状态报告收口到当前可验证真相，不再继续发布“100% 完成”或“运行时已证实”的过宽表述；明确区分 Linux 上已拿到的 source/compile 证据与仍需 Windows 主机的 runtime proof。

## Current Batch
1. 先对齐文档真相源：
   - 读取 `docs/reference/WINSSL_DESIGN.md`
   - 读取 `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
   - 读取 `docs/BACKEND_CAPABILITY_MATRIX.md` 与 `src/fafafa.ssl.winssl.lib.pas`
   - 明确当前哪些表述已经和 capability / compile truth 冲突
2. 然后做最小文档修复：
   - `WINSSL_DESIGN.md` 改成 evidence-based 状态说明
   - `WINSSL_BACKEND_STATUS_REPORT.md` 改成当前证据报告
   - 只保留 public surface、cross-target compile、Windows runtime blocker 这三层真相
3. 跑 `git diff --check`、格式化相关 Markdown、回写台账并提交。

## Status
- [completed] 文档漂移审计
- [completed] WinSSL 设计文档与状态报告修复
- [completed] Verification and formatting
- [completed] Review and commit

## Verification Summary
- doc truth audit:
  - `docs/reference/WINSSL_DESIGN.md` 原先仍写“100% 完成”，且核心类型示例落后于当前 `ISSLNativeHandleAccess` / internal access seam
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 原先仍把 `DTLS`、`OCSP Stapling`、`Session Ticket` 等写成已证实支持
  - 真相源以 `docs/BACKEND_CAPABILITY_MATRIX.md` 和 `src/fafafa.ssl.winssl.lib.pas` 为准
- formatting:
  - `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/WINSSL_DESIGN.md /home/dtamade/projects/fafafa.ssl/docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md /home/dtamade/projects/fafafa.ssl/task_plan.md /home/dtamade/projects/fafafa.ssl/findings.md /home/dtamade/projects/fafafa.ssl/progress.md /home/dtamade/projects/fafafa.ssl/docs/plans/2026-05-04-winssl-documentation-truth-alignment.md`
- hygiene:
  - `git diff --check`
  - 结果：无 whitespace / conflict 标记问题
  - `git diff --stat`
  - 结果：仅文档与台账变更，没有生产代码改动

## Risks
- 这批只改文档和台账，不碰 WinSSL 生产代码。
- 文档必须服从当前 capability matrix 和已跑过的验证证据，不能继续保留 wishful thinking 式表述。
- 不能把 Linux 上的 source contract / Win64 compile / CI gate 写成 Windows runtime 已证实。

## Follow-up Queue
1. 如果环境恢复，下一步优先补 WinSSL Windows runtime proof。
2. 在 runtime 环境缺失前，只继续做能直接减少 WinSSL compile/public-contract/document drift 的静态批次。
3. 更广的 backend completeness 仍要继续批次化推进，但每次只锁一组 capability/interface truth。
