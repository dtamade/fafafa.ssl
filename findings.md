# Findings - v1.5.0 Release-Prep Push

## 2026-05-15

- 当前项目的“停滞感”主要不是来自新的代码故障，而是来自交付边界没有外显：
  - `docs/ROADMAP.md` 已经锁定 `release-control / v1.5.0 formalization`
  - `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已经写明 `READY_FOR_MAIN_MERGE`
  - 但本地 `master` 仍比 `origin/master` 超前 94 个提交
  - 这意味着大量真实进展还没有形成一个对外可消费的 release-prep 分支

- 继续“深审查 + 文档修边角”边际收益已经很低：
  - 最近高价值修复已经覆盖 focused gate / Wave C orchestration shell hardening / control-plane drift
  - 如果继续不设边界地审查，只会把 release-control 路线重新拖回本地循环

- 这批的正确目标不是再判断“能不能发”，而是把已经成立的收口事实外显：
  - 建一个独立的 `release/v1.5.0-prep-2026-05-15` 分支
  - 重跑当前 release-control gates
  - 推送一个明确的 handoff 分支
  - 继续保留 `no-merge / no-tag` 边界

- `glm51` 不适合作为这次收口载体：
  - 它代表旧的 Linux-only release closeout 轨迹
  - 继续复用它会把这批新的 release-control / control-plane 重构上下文重新混在旧分支语义里

- Windows/WinSSL 现在仍应保持 `deferred`：
  - 本地 Linux-side static closeout 已经成立
  - 但没有新的 Windows runtime 真证据
  - 所以这批只能把它作为 follow-up truth 写清楚，不能冒充成已完成项

- release-prep 路线已经通过完整本地收口复核：
  - release-control 契约全绿
  - `compile_all_modules.py` 185/185 通过
  - `run_minimal_ci_gate.sh --fast-local` 通过
  - `run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_prep_20260515` 通过
  - `check_code_style.py src` 通过
  - `run_phase2_performance_baseline.sh --dry-run --fast-local` 通过
  - 所以当前真正剩下的不是“继续判断项目是否健康”，而是把这份健康状态推送出去
