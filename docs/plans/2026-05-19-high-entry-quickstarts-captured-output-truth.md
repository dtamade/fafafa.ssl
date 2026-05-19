# High Entry Quickstarts Captured Output Truth

## Goal

把 `docs/guides/QUICKSTART_30SEC.md` 和 `docs/guides/5_MINUTE_QUICKSTART.md` 里的 captured `预期输出` 与错误 clone 入口收紧到当前可执行 quickstart truth。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结两份高入口 quickstart 的 captured-output / clone-url truth 边界
- 只修改 `QUICKSTART_30SEC.md` 和 `5_MINUTE_QUICKSTART.md`
- 不改生产实现
- 不扩大到 `ARCHITECTURE.md` 或其它指南

## Files

- Add: `docs/plans/2026-05-19-high-entry-quickstarts-captured-output-truth.md`
- Add: `tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
- Modify: `docs/guides/QUICKSTART_30SEC.md`
- Modify: `docs/guides/5_MINUTE_QUICKSTART.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

这两份 high-entry quickstart 当前仍在正文里嵌入：

- 固定 `预期输出`
- 固定 OpenSSL 版本示例
- 固定 TLS 版本 / 密码套件 / HTTP 响应示例
- `5_MINUTE_QUICKSTART.md` 里的错误 clone 地址：`https://github.com/your-org/fafafa.ssl.git`

这些内容会把“如何开始”与“某次历史运行文本”混为一体，还会把用户带去错误仓库地址。

## Verification

```bash
bash -n tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh
bash tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh
npx prettier --write docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md
git diff --check
```

## Expected Outcome

- 两份 quickstart 继续保留：
  - 当前编译/运行命令
  - 当前示例入口
  - 成功标准
- 但不再把：
  - captured `预期输出`
  - 固定 OpenSSL/TLS/HTTP 响应文本
  - placeholder clone URL
  写成当前正文 truth
