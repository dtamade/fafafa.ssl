# 2026-03-11 API cancellation model

## Goal
- 固定 fafafa.ssl 当前的 cancellation truth，避免业务代码、框架层和后端实现各自脑补。
- 明确当前 `cancel / timeout / close / shutdown` 的边界。

## Why Now
- pure Pascal timeout/error model 这一波已经补到 read / write / handshake。
- 但 `cancel surface` 仍没有独立 contract。
- 在继续加行为前，先把当前真相固定下来，能避免后续 API 漂移。

## Files
- `docs/reference/API_CANCELLATION_MODEL.md`
- `docs/reference/API_CONTRACT_CURRENT_INDEX.md`
- `docs/reference/ARCHITECTURE.md`
- `tests/scripts/test_api_cancellation_model_doc_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
