# Active Guide Convenience-Helper Truth Alignment

## Goal

在 `ISSLConnection` 主残口继续推进时，收掉 `docs/guides/USER_GUIDE.md` 里仍会让 focused contract 误判的 convenience-helper wording drift，明确：

- `ReadString` / `WriteString` 当前仍是 `v1.x` convenience-core 文本 helper
- 活跃用户指南要直接讲清：
  - 文本示例可以继续用它们
  - 框架 / 事件循环 / 分帧协议集成优先走 `Read` / `Write` 或 `TSSLStream`
- 这批不动实现、不动 public interface，只修 active guide truth 与执行记录

## Architecture

- 保持 `src/fafafa.ssl.base.pas`、`API_REFERENCE`、`INTERFACE_DESIGN_V2` 的现状不变
- 复用现有 focused contract：
  - `tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
- 只把 `USER_GUIDE` wording 收回到当前 shipped truth，避免活跃指南继续偏离已经稳定的 convenience-surface classification

## Current Evidence

- `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh` 当前已 PASS，说明 canonical/source classification 已收口
- `tests/scripts/test_readstring_active_example_signature_truth_contract.sh` 当前已 PASS，说明示例签名仍符合当前 out-parameter truth
- `tests/scripts/test_active_guide_convenience_surface_classification_contract.sh` 当前 RED，直接点出：
  - `user guide must explain the current convenience-helper status of ReadString/WriteString`
- `docs/guides/USER_GUIDE.md` 当前实际 wording 使用了
  - `或直接把 TSSLStream 交给上层协议`
  - `或直接使用 TSSLStream`
  这两种自由表达，尚未对齐现有 focused contract 锁定的 active-guide truth

## Files

- Add: `docs/plans/2026-05-21-active-guide-convenience-helper-truth-alignment.md`
- Update: `docs/guides/USER_GUIDE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 记录本批 focused plan，明确这是一条 active guide residual，不是接口/实现层 surgery。
2. 以现有 contract RED 作为进入证据。
3. 仅修 `USER_GUIDE` 中两句 convenience-helper guidance wording。
4. 重新跑 focused contracts 与 diff hygiene。
5. 同步 `task_plan.md` / `findings.md` / `progress.md`，再提交推送。

## Verification

1. `bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
2. `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
3. `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
4. `git diff --check`

## Risks

- 不要把 scope 扩成新一轮 `ISSLConnection` 结构重写。
- 不要回退 `USER_GUIDE` 现有 builder / stream-first 主入口叙事。
- 不要让 guide wording 再次偏离已冻结的 focused contract truth。
