# Task Plan - v1.5.0 Linux Static Audit Closeout

## Goal
把 `v1.5.0` 的正式发布收口到 Linux-only 可合并状态：本地 gate 全绿、Pascal 公共接口与实现完整、release 文档与 readiness 对齐，并在 review 后完整合回 `main`。

## Current Batch
1. 复跑 Linux release gates，确认当前仓库仍然全绿。
2. 做 Pascal 静态审查，锁住 public facade、factory API、placeholder scan 和 WinSSL 骨架测试的边界。
3. 更新 release notes、readiness report 和静态审查报告，使文档真实反映 Linux-only closeout。
4. 复核 diff hygiene，提交后将已验证分支合回 `main`。

## Status
- [completed] Linux gates green on the current branch
- [completed] static Pascal audit and docs alignment
- [completed] review, commit, and merge back to `main`

## Notes
- 这批不再把 GitHub Actions 额度不足当成阻塞项；Windows runtime proof 明确转为后续独立批次。
- `TSSLHelper` 仍然是公开辅助类；移除的是旧全局 helper 别名/函数，不是 helper 类本身。
- `src/fafafa.ssl*.pas` 里不应再有 `TODO` / `FIXME` / `skeleton` / `placeholder` 这类未完成信号。

## Verification Plan
1. `python3 scripts/compile_all_modules.py`
2. `bash scripts/run_minimal_ci_gate.sh --fast-local`
3. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
4. `python3 scripts/check_code_style.py src`
5. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
6. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
7. `bash tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh`
8. `git diff --check`
9. merge the verified branch back into `main`

## Definition Of Done
- Linux gates are green
- static Pascal audit is green
- release notes and readiness report match the Linux-only closeout policy
- branch is merged back into `main`

# Task Plan - v1.5.0 Release Formalization

## Goal
把 `v1.5.0` 的正式发布准备收口到可审查、可复跑、可签字的状态：先确认版本真相和本地发布门禁，再把 release workflow / release notes / README / changelog 对齐，补一份 release workflow 契约和最终 readiness 报告，最后只在用户明确批准后再打 tag。

## Current Batch
1. 复核 baseline 真相与本地 release 门禁。
2. 落地 release workflow、release notes、版本文档对齐。
3. 触发并收集 Wave B/B2 Windows runtime 证据。
4. 生成 release readiness 报告，review 后提交。

## Status
- [completed] baseline truth / local release gates
- [completed] release workflow + notes + docs alignment
- [completed] readiness report drafted and updated with remote blocker evidence
- [completed] local release-prep batch committed as `8491b91`
- [completed] pushed `glm51` and dispatched Wave B/B2
- [blocked] Windows runtime proof refresh is blocked by GitHub Actions billing/spending-limit settings
- [pending] rerun Wave B/B2 after billing access is restored, then collect artifacts

## Notes
- 版本真相已经在 `src/fafafa.ssl.base.pas`：`FAFAFA_SSL_VERSION_STRING = '1.5.0'`，`FAFAFA_SSL_INTERFACE_VERSION = 10500`
- `CHANGELOG.md`、`README.md`、`fafafa_ssl.lpk`、`RELEASE_NOTES_V1.5.0.md` 已对齐到 `v1.5.0`
- `.github/workflows/release.yml` 已启用，`.github/workflows/release.yml.disabled` 已同步成同一份当前模板
- `python3 scripts/check_code_style.py src` 首轮打出 369 个缩进错误；已按 checker 实际报错做 44 个文件 / 369 行机械缩进修复，复跑通过
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已生成，并已记录 GitHub run `25698425400` 的外部 billing blocker
- `glm51` 已推送；刷新 Windows 证据链需要先恢复 GitHub Actions billing/spending-limit access，或使用等价可信 Windows 主机执行同一验证链

## Verification Plan
1. `git status --short`
2. `git clean -nd`
3. `git clean -ndX`
4. `python3 scripts/compile_all_modules.py`
5. `bash scripts/run_minimal_ci_gate.sh --fast-local`
6. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
7. `python3 scripts/check_code_style.py src`
8. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
9. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
10. `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
11. `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
12. `bash tests/scripts/test_active_roadmap_references_contract.sh`
13. `gh workflow run .github/workflows/wave-b-b2-manual.yml --ref glm51 ...` and collect artifacts if Windows proof is still missing

## Definition Of Done
- release workflow contract passes
- docs and package version align to `v1.5.0`
- local gates are green
- Wave B/B2 Windows evidence is captured or a concrete external blocker is recorded
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` exists and says whether the batch is ready for tag approval
- batch is committed after review

---

# Task Plan - Repo Hygiene And Ignore Consolidation

## Goal
收口仓库里的 build/output 噪音，补齐 nested `tests/**/test_*` 可执行文件的 ignore 规则，并安全清理已知输出目录，让工作树保持可复现、可审查。

## Current Batch
1. 收紧根目录专属 ignore 规则，避免把归档文档里的同名文件误判成工作记忆。
2. 移除示例目录里仓库不该自带的生成型 PEM 成品。
3. 修复数字签名示例契约对 `tmp/` 已存在的隐式假设。
4. 复核状态和 diff hygiene。
5. 提交仓库整理批次。

## Status
- [completed] Inventory ignored/untracked noise and size the safe cleanup scope
- [completed] Expand ignore coverage and clean safe generated outputs
- [completed] Update working-memory records for the new hygiene batch
- [completed] Root-anchor repo-local ignore entries and drop sample key artifacts
- [completed] Make digital-signature contract create its ignored tmp parent
- [completed] Verify diff hygiene and commit

## Current Evidence
- `git clean -ndX` showed the repository had a lot of ignored build output, including:
  - `bin/` around `728M`
  - `tests/bin/` around `131M`
  - `tests/lib/` around `5.2M`
  - `examples/bin/` around `107M`
  - `artifacts/` around `1.7M`
  - `tmp/` around `6.0G`
  - `tools/test_audit/bin/` around `2.0M`
- `tests/**/test_*` was not covered by the existing top-level test-binary ignore rule, so nested generated executables could still surface as untracked files.
- The cleanup sweep removed generated output directories. The first broad pass also swept local ignored agent/config folders and `archive/`, so this batch now makes local agent/cache ignores explicit.
- After the `.gitignore` update, `git check-ignore -v` confirms nested `tests/**/test_*` executables are ignored, test sources remain visible, and benchmark report markdown stays ignored.
- `git clean -nd` only reports this new plan doc; `git clean -ndX` only reports `.agents/` and `.codex/` as ignored local caches.
- The follow-up sweep found two more repo-hygiene nits:
  - rootless `task_plan.md` / `findings.md` / `progress.md` / `WARP.md` ignore patterns can accidentally match archive docs such as `docs/archive/old_reports/PROGRESS.md`
  - `examples/digital_signature/private.pem` and `public.pem` are generated outputs that the README already instructs users to create locally
- `git check-ignore -v --no-index` now confirms the root-local working-memory files are matched only at the repo root, while `docs/archive/old_reports/PROGRESS.md` is no longer caught by those patterns.
- `tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh` now creates `tmp/` itself, so it still works after a clean artifact sweep removes the ignored directory.
- After the tmp parent fix and follow-up cleanup, `git clean -nd` and `git clean -ndX` are back to empty.

## Risks
- Do not delete local agent/config folders or archived notes.
- Do not broaden the cleanup into tracked source trees.
- Preserve the test source files and docs under `tests/**`.
- Keep the ignore rules root-scoped for repo-local files only.

## Follow-up Queue
1. Organization batch committed.

# Task Plan - Working-Memory, Artifact Hygiene, And WinSSL Workflow Closeout

## Goal
把当前工作树从“历史批次 + 本地产物残留”收口回 `HEAD` `e80100a` 的真实状态，清掉 3 个测试二进制残留，并把 `wave-b-b2-manual.yml` 的 Windows lane 对齐到 WinSSL runtime checklist。

## Current Batch
1. 清理 `tests/contract/` 与 `tests/wolfssl/` 下的无扩展名 ELF 测试产物。
2. 新增本批 plan 文档，作为可恢复的工作记忆。
3. 把 `task_plan.md` / `findings.md` / `progress.md` 顶部对齐到当前真相。
4. 修复 `test_wave_b_b2_windows_runtime_workflow_contract.sh` 打出的 workflow RED。
5. 复跑 focused contracts、diff hygiene，并提交。

## Status
- [completed] Freeze current state and remove generated test binaries
- [completed] Resync working-memory files to current HEAD and next queue
- [completed] Align Wave B/B2 Windows workflow to the runtime checklist
- [completed] Verify diff hygiene and record results
- [pending] Commit the batch

## Current Evidence
- `git log --oneline -1` shows current `HEAD` as `e80100a fix: batch 6 - compiler warning reduction and capabilities contract test`
- `git status --short` initially listed only three untracked ELF test binaries:
  - `tests/contract/test_capabilities_contract`
  - `tests/wolfssl/test_wolfssl_connection_contract`
  - `tests/wolfssl/test_wolfssl_context_contract`
- `file` confirmed those artifacts are Linux ELF executables, not source files
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` produced a real RED: the workflow did not install or verify Lazarus / `lazbuild`
- current product-side blocker remains real Windows runtime evidence, but the GitHub Actions lane can now be made capable of collecting it

## Risks
- Do not reopen `src/fafafa.ssl.winssl.*` in this batch.
- Do not treat workflow readiness as a substitute for Windows runtime proof.
- Keep the batch narrow so the commit stays reviewable.

## Follow-up Queue
1. Commit the closeout batch.
2. Trigger the updated `wave-b-b2-manual.yml` on GitHub Actions when a real Windows runtime proof run is needed.

# Task Plan - Wave B/B2 WinSSL Runtime Workflow Alignment

## Goal
把当前手动 Windows CI workflow 对齐到最新 `WinSSL` runtime checklist，让仓库在没有本地 Windows 主机时，仍能通过 `wave-b-b2-manual.yml` 去推进 quick smoke、Wave B gate、broader suite 这条真实运行时证据链。

## Current Batch
1. 用 focused contract 证明当前 `wave-b-b2-manual.yml` 的 Windows job 仍低于最新 runtime checklist。
2. 最小修改 workflow / docs，把 quick smoke、Wave B gate、broader suite transcript 接进现有 Windows lane。
3. 复跑 focused contracts，并更新计划/台账。

## Status
- [pending] RED workflow contract for Windows runtime checklist alignment
- [pending] Align wave-b-b2-manual Windows lane
- [pending] Re-run workflow + bundle contracts
- [pending] Record evidence and commit the batch

## Current Evidence
- fresh broad completion audit 已证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source contract / bundle contract：全部 PASS
- fresh Win64 cross-target compile 已补齐：
  - `tests/winssl/test_winssl_session_management.pas` 可成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 可成功交叉编译到 Win64
- 当前 Linux 主机环境边界已确认：
  - `command -v pwsh`：空 / exit `1`
  - `wine --version`：exit `159`
- 结论：
  - Linux 侧 public surface、capability truth、repo gates、source contract、Win64 compile proof 都已闭合
  - 唯一未闭合 requirement 是真实 Windows 主机上的 `WinSSL` runtime proof
- 当前仓库已有的 Windows CI 入口是 `.github/workflows/wave-b-b2-manual.yml`，但从源码可见：
  - 只跑 `scripts/run_wave_b_windows_gate.ps1`
  - 还没显式安装/验证 `lazbuild`
  - 还没把 quick smoke 和 broader suite transcript 纳入 artifact
- 因此“有 Windows workflow”还不等于“这条 workflow 已覆盖当前 runtime checklist”。

## Risks
- 这批只能把 CI lane 对齐到 checklist，不能替代真实 Windows runtime 结果本身。
- 如果 workflow 只补命令不补 artifact，后续仍然没法做可审查闭环。
- 如果只改 `.yml` 不同步 `.disabled` 模板和文档，后续容易再次漂移。

## Follow-up Queue
1. 触发对齐后的 `wave-b-b2-manual.yml` Windows lane。
2. 审查 quick smoke / Wave B / broader suite artifacts。
3. 只有当这些 Windows runtime 结果真实返回后，才继续判断 broad objective 是否可标记完成。
