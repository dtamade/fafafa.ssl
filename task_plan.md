# Project Review Plan

## 2026-04-29 Plan (Execution / Dirty worktree batched closeout)

### Goal
- Drain the current mixed dirty worktree into verified, topic-scoped commits without reverting user work or mixing unrelated families.

### Files
- `docs/plans/2026-04-29-dirty-worktree-batched-closeout.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Later scoped batches: OpenSSL/cert-utils, TLS 1.3, builder/config/SNI, Wave C, docs/API/capability truth.

### Steps
- [x] Load repo instructions and current working-memory context.
- [x] Freeze current state with `git status --short`, `git diff --stat`, and untracked inventory.
- [x] Start read-only parallel explorers for OpenSSL/cert-utils, TLS 1.3, Wave C, and builder/config/SNI batching.
- [x] Write this total-control plan and record the batch boundaries.
- [x] Run baseline non-invasive verification.
- [x] Commit the planning/working-memory boundary if clean.
- [ ] Execute Batch 1 through Batch 5, each with focused verification, review conclusion, and commit.

### Status
- In progress. Planning boundary, Batch 0A gate hardening, Batch 1 cert-utils / certchain hardening, Batch 2 OpenSSL core helper/capability hardening, and Batch 3A backend selector minimum-score filtering are committed. Batch 3B server-name runtime/isolation is verified and ready for scoped review + commit.

### Batch 0A: Default gate hardening
- [x] Verify shell syntax for changed gate scripts and new contracts.
- [x] Verify `compile_all_modules.py` fails closed on 99/100 success.
- [x] Verify `run_minimal_ci_gate.sh` no longer executes shell payloads through module arguments.
- [x] Verify minimal gate passes `--fast-local` through to phase2 dry-run without changing git status.
- [x] Verify phase2 dry-run fast-local paths stay under `tmp/`.
- [x] Verify cleanup script covers new fast-local output directories while preserving safe dry-run defaults.
- [x] Commit scoped gate hardening batch.

### Batch 1: Cert-utils and certchain hardening
- [x] Verify previously failing `test_cert_utils_verify_chain_bio_contract` is green.
- [x] Verify adjacent `VerifyChain` bundled intermediate regressions are green.
- [x] Run the full `tests/test_cert_utils_*_contract.pas` sweep.
- [x] Run certchain-specific revocation / verify-flags focused regressions.
- [x] Run `python3 scripts/compile_all_modules.py`.
- [x] Run scoped diff hygiene.
- [x] Review and commit scoped cert-utils / certchain batch.

### Batch 2: OpenSSL core helper/capability hardening
- [x] Run OpenSSL focused contract sweep over `tests/test_openssl_*.pas` plus `tests/openssl/test_openssl_features.pas` and `test_openssl_ca_autoload.pas`.
- [x] Debug and fix `test_openssl_loader_required_symbol_contract` access violation.
- [x] Re-run OpenSSL focused contract sweep.
- [x] Run `python3 scripts/compile_all_modules.py`.
- [x] Run scoped diff hygiene.
- [x] Review and commit scoped OpenSSL core batch.

### Batch 3A: Backend selector minimum-score filtering
- [x] Verify focused selector regression `tests/test_backend_selector_minimum_score_filtering.pas`.
- [x] Verify adjacent selector/context-builder coverage:
  - `tests/test_backend_selector_basic.pas`
  - `tests/config/test_context_builder_try.pas`
- [x] Run `python3 scripts/compile_all_modules.py`.
- [x] Run scoped diff hygiene.
- [x] Review and commit scoped backend selector batch.

### Batch 3B: Server-name runtime / factory isolation
- [x] Verify `tests/test_context_builder_server_servername_runtime_consistency.pas`.
- [x] Verify `tests/test_factory_config_server_name_isolation.pas`.
- [x] Verify `tests/test_connection_builder_hostname_precedence.pas`.
- [x] Verify `tests/test_freepascal_context_server_name_inheritance.pas`.
- [x] Run `python3 scripts/compile_all_modules.py`.
- [x] Run scoped diff hygiene.
- [ ] Review and commit scoped server-name runtime/isolation batch.

## 2026-04-19 Plan (Execution / FreePascal completeness docs-history absorption batch)

### Goal
- 在 `5bf7be2` 已吸收 FreePascal completeness code/test/gate 主线之后，把当前 dirty worktree 中仍直接属于这条主线的高置信文档真相与执行历史单独吸收成第二个提交。
- 本批只吸收两类内容：
  - high-confidence tracked docs：focused gate、testing entrypoint、architecture truth、FreePascal integration / CT / OCSP guides
  - untracked `docs/plans/` FreePascal completeness 历史链
- 明确排除 `README.md`、`docs/README.md`、`docs/DOCUMENTATION_INDEX.md`、`docs/reference/API_REFERENCE.md` 这类广域刷新，以及 Wave C / OpenSSL BIO / symbol 主题。

### Files
- `.github/README.md`
- `.github/GITHUB_ACTIONS_GUIDE.md`
- `docs/testing/TESTING_README.md`
- `docs/ARCHITECTURE.md`
- `docs/reference/ARCHITECTURE.md`
- `docs/INTEGRATION_GUIDE.md`
- `docs/ROADMAP.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- `docs/plans/2026-03-20-freepascal-context-server-name-inheritance.md`
- `docs/plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md`
- `docs/plans/2026-03-25-freepascal-tls13-client-session-resumption-psk.md`
- `docs/plans/2026-03-26-freepascal-tls13-server-session-resumption-psk.md`
- `docs/plans/2026-03-27-freepascal-tls13-0rtt-early-data-primitives.md`
- `docs/plans/2026-04-08-connector-client-early-data-convenience.md`
- `docs/plans/2026-04-08-early-data-public-api-ergonomics.md`
- `docs/plans/2026-04-08-factory-config-early-data-parity.md`
- `docs/plans/2026-04-08-freepascal-*.md`
- `docs/plans/2026-04-09-freepascal-*.md`
- `docs/plans/2026-04-10-freepascal-*.md`
- `docs/plans/2026-04-11-freepascal-*.md`
- `docs/plans/2026-04-12-freepascal-*.md`
- `docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md`
- `docs/plans/2026-04-13-freepascal-*.md`
- `docs/plans/2026-04-14-freepascal-*.md`
- `docs/plans/2026-04-15-freepascal-*.md`
- `docs/plans/2026-04-16-freepascal-*.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 锁定第二批提交边界：只纳入 high-confidence tracked docs 与 FreePascal completeness 历史 plans。
- [x] 更新 working-memory，写清包含/排除清单与验证要求。
- [x] 只按白名单 stage scoped files，不混入 README/API/Wave C/OpenSSL 主题。
- [x] 跑 docs/history 批次所需 contract / gate / compile / diff hygiene。
- [x] 做简短 review 结论并提交第二批次。

### Status
- 已完成。第二批按 scoped docs/history 白名单完成 stage、hygiene 修正与 fresh verification，直接进入 commit 收口。

## 2026-04-19 Plan (Review / FreePascal docs batch candidate triage after 5bf7be2)

### Goal
- 在 `5bf7be2` 已吸收 FreePascal completeness code/test/gate batch 之后，从当前超脏 worktree 中筛出“仍直接属于 FreePascal TLS 1.3 early-data / completeness 主线”的已跟踪文档改动，形成后续独立 docs batch 候选。
- 明确排除 `Wave C`、OpenSSL `BIO` / symbol 主题，以及广域 README / API / repo-wide docs refresh，除非它们对这条主线的 truth preservation 绝对必要。

### Files
- Tracked modified docs candidates discovered from `git status` / `git diff`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [ ] 盘点当前 tracked modified documentation files，并限定只看已跟踪文档路径。
- [ ] 逐个阅读 diff，判断是否直接服务于 FreePascal TLS 1.3 early-data / completeness mainline。
- [ ] 输出可安全成批的候选文件、排除文件、归属理由与所需 verification commands。

### Status
- 进行中。当前重点不是修改文档内容，而是做 docs batch 边界判定，确保后续提交不混入无关主题。

## 2026-04-19 Plan (Execution / FreePascal completeness code-test-gate absorption batch)

### Goal
- 在不吸收 OpenSSL guard sweep、Wave C canonicalization、README/API 广域文档刷新的前提下，把当前已在工作区中的 FreePascal completeness 主线代码、focused tests 与 completeness gate 单独摘成首个可提交批次。

### Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `tests/test_capability_cache.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/test_tls13_resumption.pas`
- `tests/test_early_data_public_api_contract.pas`
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- `tests/test_freepascal_client_certificate_flight_requirements.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `tests/test_freepascal_client_chain_trust_runtime.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `tests/test_freepascal_client_peer_certificate_surface.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/test_freepascal_context_server_name_inheritance.pas`
- `tests/test_freepascal_revocation_fast_contracts.pas`
- `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_server_session_resumption.pas`
- `tests/test_tls_connector_early_data_contract.pas`
- `tests/config/test_context_builder_early_data_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 盘点脏工作区，确认首批只吸收 FreePascal completeness / early-data / validation 主线，不混入 OpenSSL / Wave C / 广域 docs 主题。
- [x] 跑 contract / focused tests / completeness gate / compile-all，确认当前 worktree version fresh GREEN。
- [x] 把本轮分批策略与 fresh evidence 回写 working-memory。
- [x] 对 scoped files 做 review、stage、commit 代码/测试/门禁批次。
- [x] 再决定是否继续单独吸收 FreePascal docs / plans 批次。

### Status
- 已完成并提交到 `5bf7be2 feat: absorb freepascal tls13 completeness mainline`。README / API reference / docs plans 已移交到后续 docs/history 批次处理。

## 2026-04-19 Plan (Execution / FreePascal early-data mainline closeout verification sweep)

### Goal
- 在不重开新持久化形态、不改变默认 shipped behavior、public surface、capability wording 的前提下，对 FreePascal TLS 1.3 early-data 主线做一轮权威 verification sweep，确认当前主线已经收口到“只在 fresh RED 出现时再重开”。

### Files
- `docs/plans/2026-04-19-freepascal-early-data-mainline-closeout-verification-sweep.md`
- Reference only: `docs/ROADMAP.md`
- Reference only: `README.md`
- Reference only: `docs/reference/API_REFERENCE.md`
- Reference only: `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
- Reference only: `tests/test_factory_config_early_data_isolation.pas`
- Reference only: `tests/test_freepascal_tls13_early_data.pas`
- Reference only: `tests/test_capability_cache.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / README / API reference / builder / factory / config live truth，确认当前主线已经表达 file-backed + directory-backed public opt-in，且 default wording 仍保持 in-memory single-process caveat。
- [x] 跑 docs contract、factory/config isolation、runtime early-data focused suite、capability wording alignment suite。
- [x] 跑 completeness gate，确认当前主线 fresh PASS。
- [x] 若无 fresh RED，则以 verification-only closeout 收口，不改 `src/`；若出现 fresh RED，只允许最小修复直接命中的 surface。
- [x] 回写 working-memory；若存在 repo-tracked scoped 改动则 commit，否则报告“无需新的生产提交”。

### Status
- 已完成。本批是 verification-only closeout：docs contract、factory/config isolation、runtime early-data focused suite、capability wording alignment、completeness gate 全部 fresh PASS，没有任何 fresh RED 指向 `src/` 或新的 persistence queue。
- 因此本批不改生产代码、不重开 directory-store family、不引入 SQLite / distributed anti-replay / provider redesign；唯一需要入库的 tracked 产物是本轮执行计划文件。

### Verification
- `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - PASS：`[PASS] FreePascal early-data public opt-in docs contract passed`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_factory_config_early_data_isolation_plan_exec && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_early_data_isolation_plan_exec -FEtmp/test_factory_config_early_data_isolation_plan_exec -otmp/test_factory_config_early_data_isolation_plan_exec/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/test_factory_config_early_data_isolation_plan_exec/test_factory_config_early_data_isolation`
  - PASS：`Tests Passed: 61` / `Tests Failed: 0`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data_plan_exec && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data_plan_exec -FEtmp/freepascal_tls13_early_data_plan_exec -otmp/freepascal_tls13_early_data_plan_exec/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data_plan_exec/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/capability_cache_plan_exec && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_plan_exec -FEtmp/capability_cache_plan_exec -otmp/capability_cache_plan_exec/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_plan_exec/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_mainline_closeout_verification_sweep_20260419`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`

## 2026-04-19 Plan (Execution / FreePascal early-data public opt-in docs closeout)

### Goal
- 在不改任何 `src/` 行为、保持 default capability wording 与 default `in-memory single-process anti-replay ledger` truth 不变的前提下，把已经 landed 的 FreePascal early-data public replay-store opt-in 路径做文档收口，并用 docs contract 锁住 README / API reference 里的关键名称与约束。

### Files
- Reference only: `docs/plans/2026-04-19-freepascal-early-data-directory-store-public-optin-parity.md`
- `README.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 public opt-in parity plan 与现有 runtime / factory contracts，确认本批只做 docs closeout，不重开 persistence 设计、capability wording 或新的 production wiring。
- [x] 先补 docs contract，锁定 default in-memory wording、`TSSLConfig` file/directory 字段名、builder helper 名称以及 `mutually exclusive` 约束。
- [x] 更新 `README.md` 与 `docs/reference/API_REFERENCE.md`，补齐 builder / config/factory 用法与互斥约束说明。
- [x] 跑 docs contract、focused public-opt-in suites、completeness gate 与 scoped diff hygiene，并回写 working-memory。

### Status
- 已完成。本批只修改文档和 1 个 shell 合同，没有新的 `src/` / Pascal 测试代码改动。
- 这批 closeout 后，公开 guidance 现在明确锁住：
  - default FreePascal early-data path 仍然是 `in-memory single-process anti-replay ledger`
  - public opt-ins:
    - `TSSLConfig.ServerEarlyDataReplayStoreFile`
    - `TSSLConfig.ServerEarlyDataReplayStoreDirectory`
    - `WithServerEarlyDataReplayStoreFile(...)`
    - `WithServerEarlyDataReplayStoreDirectory(...)`
  - `file` 与 `directory` 配置保持 mutually exclusive
- 这批最值钱的交付是防止文档漂移，而不是继续碰 production anti-replay/persistence 逻辑。

### Verification
- `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - RED：初次失败，缺口落在 `README.md` 未提及 `TSSLConfig.ServerEarlyDataReplayStoreFile`
  - GREEN：`[PASS] FreePascal early-data public opt-in docs contract passed`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_factory_config_early_data_isolation_docs_closeout && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_early_data_isolation_docs_closeout -FEtmp/test_factory_config_early_data_isolation_docs_closeout -otmp/test_factory_config_early_data_isolation_docs_closeout/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/test_factory_config_early_data_isolation_docs_closeout/test_factory_config_early_data_isolation`
  - PASS：`Tests Passed: 61` / `Tests Failed: 0`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data_docs_closeout && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data_docs_closeout -FEtmp/freepascal_tls13_early_data_docs_closeout -otmp/freepascal_tls13_early_data_docs_closeout/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data_docs_closeout/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id docs_closeout_freepascal_early_data_public_optin_20260419`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `git diff --check -- README.md docs/reference/API_REFERENCE.md tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - PASS：无输出

## 2026-04-19 Plan (Execution / FreePascal early-data directory store deterministic rename-denial closeout)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store update-path 上 `tempdir -> main` deterministic denied / `main -> .bakdir` deterministic denied 两条子族锁成 focused direct/runtime contracts，并确认当前实现是否已经天然满足。

### Files
- `docs/plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / `dirstore` / file-backed 同族 rename-denial 合同，确认这批只打 deterministic `tempdir -> main` denied 与 deterministic `main -> .bakdir` denied，不重开 public wiring、capability wording 或已绿的 blocker/corruption/residue 家族。
- [x] 新增 `docs/plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md`，把目标、边界、验证命令和 DoD 写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct/runtime 4 条合同，并新增两个 scripted directory-store rename-denial subclass。
- [x] 跑 focused suite，确认 current source 对这 4 条新合同直接 GREEN，不需要新的生产修复。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all，并回写 roadmap / working-memory / hygiene。

### Status
- 已完成。本批新增了 1 份执行计划、4 条 focused deterministic rename-denial contracts；fresh focused run 直接 GREEN，说明上一批补进来的 `RenamePathAt(...)` seam 已经足够表达这组 family，当前实现没有新的 production drift。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreFailsClosedOnDeterministicTempPromotionRenameDeniedAndRecovers`
  - `TestDirectoryReplayStoreFailsClosedOnDeterministicBackupPromotionRenameDeniedAndRecovers`
  - `TestDirectoryReplayStoreFailsClosedOnDeterministicTempPromotionRenameDeniedAtRuntime`
  - `TestDirectoryReplayStoreFailsClosedOnDeterministicBackupPromotionRenameDeniedAtRuntime`
- 这批 fresh evidence 明确说明：
  - deterministic `tempdir -> main` denied 时，provider/runtime 都会 fail closed，canonical `main` 不会被误 materialize，`.tmpdir` 会被清理，`.bakdir` 不会被误创建
  - deterministic `main -> .bakdir` denied 时，provider/runtime 都会 fail closed，同时 preserving canonical `main` replay truth
  - rebuild 之后 blocked session 仍会恢复 first accept，后续 replay 继续 reject
  - 本批没有新的 `src/` 改动

### Verification
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_deterministic_rename_denial_20260419`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出
- `for f in docs/plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md; do out=$(git diff --no-index --check -- /dev/null "$f" 2>&1 || true); if [ -n "$out" ]; then printf 'DIRTY %s\n%s\n' "$f" "$out"; exit 1; else printf 'CLEAN %s\n' "$f"; fi; done`
  - PASS：全部 `CLEAN`

## 2026-04-19 Plan (Execution / FreePascal early-data directory store backup-assisted replace and restore failure)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 更深一层的 update-path write-interruption family 锁成 focused direct/runtime contracts：当 `main -> .bakdir` 成功、`tempdir -> main` 失败时继续保住旧 replay truth；当 `.bakdir -> main` restore 也失败时继续 fail closed，并允许后续从 `.bakdir` fallback 恢复。

### Files
- `docs/plans/2026-04-19-freepascal-early-data-directory-store-backup-assisted-replace-and-restore-failure.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed 同族合同，确认这批只打 directory-store backup-assisted replace failure / backup restore failure recovery，不重开 public wiring、capability wording 或已绿的 blocker/corruption/residue 家族。
- [x] 新增 `docs/plans/2026-04-19-freepascal-early-data-directory-store-backup-assisted-replace-and-restore-failure.md`，把目标、边界、验证命令和 DoD 写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED：新增两个 scripted directory-store rename-failure subclass，并补 direct/runtime 4 条合同。
- [x] 跑 focused suite，确认 first RED 落在 directory-store 缺 rename seam，而不是新的行为分支漂移。
- [x] 在 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 做最小 GREEN：新增 `RenamePathAt(...)` 内部 override seam，只替换 `SaveEntries(...)` 内部 rename 调用点。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all，并回写 roadmap / working-memory / hygiene。

### Status
- 已完成。本批新增了 1 份执行计划、4 条 focused direct/runtime contracts，以及 1 处最小内部 rename seam；没有 fresh RED 继续落到 `SaveEntries(...)` 分支语义，因此本轮没有扩 production behavior，只把现有路径 rename 操作抽成可脚本化的内部 wrapper。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStorePreservesExistingTruthAcrossBackupAssistedReplaceFailure`
  - `TestDirectoryReplayStoreRecoversReplayTruthFromBackupAfterRestoreFailure`
  - `TestDirectoryReplayStorePreservesExistingTruthAcrossBackupAssistedReplaceFailureAtRuntime`
  - `TestDirectoryReplayStoreRecoversReplayTruthFromBackupAfterRestoreFailureAtRuntime`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 这批唯一生产改动是：
  - 新增 `RenamePathAt(...)`，默认仍调用 `RenameFile(...)`
  - `SaveEntries(...)` 的路径 rename 统一走这个内部 seam
  - 没有新的 `SaveEntries(...)` 行为改动

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_backup_restore_families_20260419`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`

## 2026-04-19 Plan (Execution / FreePascal early-data directory store `.bakdir` residue semantics)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 的 `.bakdir` cleanup-failure residue + stale `.bakdir` undeletable next-save fail-closed 语义锁成 focused direct/runtime contracts；如果 fresh RED 只暴露 tests 无法 script cleanup delete failure，则只允许增加内部测试 seam，不重开 `SaveEntries(...)` 行为。

### Files
- `docs/plans/2026-04-19-freepascal-early-data-directory-store-bakdir-residue-semantics.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / existing file-backed `.bak` residue contracts，确认这批只打 directory-store `.bakdir` residue semantics，不重开 public wiring、capability wording 或更深的 directory-store drift。
- [x] 新增 `docs/plans/2026-04-19-freepascal-early-data-directory-store-bakdir-residue-semantics.md`，把目标、边界、验证命令和 DoD 写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED：新增 scripted directory-store cleanup-delete-failure subclass，并补 direct/runtime 两条 `.bakdir` residue semantics 合同。
- [x] 跑 focused suite，确认 first RED 落在 directory-store cleanup helper 无法 override，而不是 `SaveEntries(...)` 行为漂移。
- [x] 在 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 做最小 GREEN：把 `RemovePathTree(...)` 调整为 `protected virtual`，仅供 scripted test store 注入 `.bakdir` cleanup delete failure。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all，并回写 roadmap / working-memory / hygiene。

### Status
- 已完成。本批新增了 1 份执行计划、2 条 focused `.bakdir` residue contracts，以及 1 处最小内部测试 seam；没有 fresh RED 继续落到 `SaveEntries(...)` 逻辑，因此本轮没有扩 production behavior。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreLeavesBackupResidueAfterCleanupFailureAndFailsClosedOnUndeletableStaleBackup`
  - `TestDirectoryReplayStoreLeavesBackupResidueAfterCleanupFailureAndFailsClosedOnUndeletableStaleBackupAtRuntime`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 这批唯一生产改动是：
  - `RemovePathTree(...)` 从 private 调整为 `protected virtual`
  - 用途仅限 scripted test store 注入 `.bakdir` cleanup delete failure
  - 没有新的 `SaveEntries(...)` 行为改动

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_bakdir_residue_semantics_20260419`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`

## 2026-04-16 Plan (Execution / FreePascal early-data directory store runtime crash-update restart)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 更接近真实 accepted update-path 的 runtime crash-window 语义锁成 focused contract：existing replay truth 先已存在，fresh resumed early-data 在新进程里被 accept 并更新 anti-replay state，随后进程在 accept 后立刻 crash；重启后，既有 replay truth 与刚 accept 的 replay truth 都必须继续 reject。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / runtime crash-window helpers，确认这批只锁 directory-store accepted update-path restart truth，不重开 public wiring 或 capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md`，把 runtime harness、focused contract 和验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，扩 `--runtime-crash-accept` / `--runtime-replay-probe` 现有 child harness 对 `directory_store` context path 的支持，并新增 focused runtime contract。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划、1 条 focused runtime crash-update restart contract，并把现有 runtime crash-accept / replay-probe harness 最小扩到 `directory_store` context path；没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此本轮保持 tests-only closeout。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreRetainsExistingAndAcceptedReplayTruthAcrossCrashWindowRestart`
- 这批 tests-only 扩展一起锁住：
  - existing canonical directory replay truth 先已存在
  - child 进程 accept fresh blocked session 并 materialize updated replay truth 后 simulated crash
  - crash-window restart 之后，original replay truth 继续 reject
  - 同一 blocked session 在新的 replay-probe child 里也继续 reject
  - runtime helper 会记录 `directory_store` context path，且 canonical directory replay truth 在 crash 后仍存在

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_runtime_crash_update_restart_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出
- `for f in docs/plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md; do git diff --no-index --check -- /dev/null "$f"; done`
  - PASS：全部 `CLEAN`；期间发现 `findings.md` 有历史 trailing whitespace，已清理后重跑为绿

## 2026-04-16 Plan (Execution / FreePascal early-data directory store dual fallback conflict)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把更像真实 crash-window 的 directory-store 双 fallback 冲突态锁成 focused contracts：当 canonical `main` 缺失、优先级更高的 `.tmpdir` 已损坏、而 `.bakdir` 仍承载健康旧 replay truth 时，provider/runtime 都必须 fail closed，不能 silent fallback/self-heal；只有坏 `.tmpdir` 被显式移除后，才允许从 `.bakdir` 恢复。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-dual-fallback-conflict.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / `dirstore` fallback resolution，确认这批只打 `corrupt .tmpdir + healthy .bakdir` 组合态，不重开 public wiring 或 capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-dual-fallback-conflict.md`，把 direct/runtime 交付与验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct/runtime RED，锁 preferred corrupt `.tmpdir` shadowing healthy `.bakdir` 的 fail-closed / recovery 语义。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all、diff hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划、2 条 focused dual-fallback conflict contracts；没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此本轮保持 tests-only + docs closeout。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreFailsClosedWhenCorruptTempFallbackShadowsHealthyBackupFallback`
  - `TestDirectoryReplayStoreFailsClosedWhenCorruptTempFallbackShadowsHealthyBackupFallbackAtRuntime`
- 这两条合同一起锁住：
  - `main` 缺失且 `corrupt .tmpdir + healthy .bakdir` 同时存在时，fresh blocked session 必须 fail closed
  - same conflict state 下 original replay truth 也继续 reject，不允许 silent fallback/self-heal
  - canonical `main` 会继续缺失，`.tmpdir` / `.bakdir` 两个 artifact 都继续保留
  - 只有坏 `.tmpdir` 被显式移除后，fresh blocked session 才恢复 accept，并重新 materialize canonical `main`

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_dual_fallback_conflict_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`

## 2026-04-16 Plan (Execution / FreePascal early-data directory store blocker edge closeout)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，对 directory-store blocker family 做最后一轮最小边界扫尾：把 `.bakdir` wrong-shape blocker 在 first acquire 上的 direct/runtime fail-closed + recovery 语义补成 focused contracts，确认 blocker queue 真正收口。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-blocker-edge-closeout.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / blocker closeout truth，确认这批只补 `.bakdir` first-acquire blocker，不重开 update-path / public wiring / capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-blocker-edge-closeout.md`，把 direct/runtime 交付与验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct/runtime RED，锁 `.bakdir` regular-file first-acquire fail-closed / recovery 语义。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all、diff hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划，并把 `.bakdir` wrong-shape blocker 在 first acquire 上的 direct/runtime 语义补成 focused contracts；没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此本轮保持 tests-only closeout。
- `tests/test_freepascal_tls13_early_data.pas` 在现有 blocker family 里新增了两组高价值子用例：
  - direct provider path 的 `.bakdir` regular-file first-acquire fail-closed / recovery
  - runtime resumed early-data path 的 `.bakdir` regular-file first-acquire fail-closed / recovery
- 这批 fresh GREEN 明确说明：
  - regular-file `.bakdir` blocker 在 canonical `main` 缺失时也会继续 fail closed
  - blocker file 不会被 silent delete，canonical `main` / `.tmpdir` 不会被误 materialize
  - blocker 移除后，同一 session 会恢复 accept，rebuild 后 replay truth 继续 reject

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- Final shared verification reused by the immediately following dual-fallback batch:
  - backend basic：PASS
  - capability cache：PASS
  - completeness gate：PASS
  - compile-all：PASS

## 2026-04-16 Plan (Execution / FreePascal early-data directory store filesystem blocker semantics)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 剩余最高 ROI 的 filesystem blocker family 直接锁成 focused contracts：当 canonical `main`、staging `.tmpdir`、或 replace-target `.bakdir` 被 regular file 等错误形态占住时，provider/runtime 都必须 fail closed；已有 replay truth 也不能被 silent delete、silent replace、或 silent heal。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-filesystem-blocker-semantics.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / directory-store seam / existing file-backed blocker contracts，确认这批只打 directory-store filesystem blocker，不重开 public wiring 或 capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-filesystem-blocker-semantics.md`，把 direct/runtime blocker delivery order 与验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct/runtime RED，覆盖 `main` / `.tmpdir` / `.bakdir` 的 wrong-shape blocker 语义。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all、diff hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划、2 条 focused blocker contracts，并把 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 的真实剩余缺口最小收口到 `SaveEntries(...)`：wrong-shape `.tmpdir` / `.bakdir` 现在不再被预删后继续写入，而是直接 fail closed。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAndRecovers`
  - `TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAtRuntime`
- 这两条合同现在一起锁住：
  - regular-file canonical `main` blocker 在 direct/runtime first acquire 上 fail closed
  - regular-file `.tmpdir` blocker 在 direct/runtime first acquire 上 fail closed
  - regular-file `.tmpdir` / `.bakdir` blocker 在 update path 上 preserve existing replay truth
  - blocker 移除后，同一 blocked session 才恢复 accept，provider rebuild / runtime rebuild 后 replay truth 继续成立

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - RED first: `❌ Regular-file directory replay-store .tmpdir blocker should fail closed while updating existing replay truth`
  - GREEN rerun: `✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_filesystem_blockers_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`

## 2026-04-16 Plan (Execution / FreePascal early-data directory store fallback corruption hardening)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 剩余最高 ROI 的 fallback corruption family 直接锁成 focused contracts：当 canonical `main` 缺失且 readable resolution 退回 `.tmpdir` / `.bakdir` 时，`invalid_version` 与 `trailing_garbage` 都必须继续 fail closed，不允许 silent accept、silent heal、或隐式重建 canonical `main`。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-fallback-corruption-hardening.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / directory-store seam / existing fallback helpers，确认这批只锁 `.tmpdir` / `.bakdir` corruption fail-closed，不重开 file-backed / public wiring / capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-fallback-corruption-hardening.md`，把 direct/runtime delivery order 与验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 与 real runtime 的 fallback corruption contracts。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all、diff hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划、2 条 focused fallback corruption contracts；没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此本轮保持 tests-only + docs closeout。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStoreFailsClosedOnCorruptFallbackDirectoriesAcrossProviderRebuild`
  - `TestDirectoryReplayStoreFailsClosedOnCorruptFallbackDirectoriesAtRuntime`
- 这两条合同已经直接覆盖 `.tmpdir` / `.bakdir` 的 4 组 corruption truth：
  - `invalid_version`
  - `trailing_garbage`
- fresh RED 最先暴露的是 tests-only fixture drift，而不是 production drift：
  - direct provider 子用例里 manual session label 过长
  - directory-store 会把 replay key 再编码成 `.entry` 文件名，命中单文件名长度上限
  - 缩短 fixture label 后，focused suite 立即恢复 GREEN
- fresh focused / adjacent / gate / compile 证据现在共同证明：
  - corrupt fallback directory 不会被 silent accept 或误当成 empty store
  - fresh blocked session 会继续 fail closed
  - original replay truth 不会被误恢复
  - canonical `main` 会继续保持缺失，corrupt fallback artifact 会继续保留
  - runtime resumed early-data path 仍然是 handshake success + session reused，但 early-data 必须 reject
- next queue 继续收紧到：
  - directory-store filesystem blocker 语义
  - 更深的 crash-window write-interruption drift，仅在 fresh RED 出现时再重开

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - RED first: `❌ Invalid-version .tmpdir fallback replay directory should materialize canonical main directory replay truth before corrupting the fallback path`
  - Root cause: tests-only manual session label too long, causing directory entry filename overflow after replay-key re-encoding
  - GREEN rerun: `✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_fallback_corruption_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-fallback-corruption-hardening.md tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data directory store crash-window tempdir residue)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 的下一条最高 ROI 剩余语义补成 focused contracts：当 canonical `main` 缺失而 live `.tmpdir` residue 承载 replay truth 时，pure replay reject 与 repeated process restart 都不能误消费 fallback；只有后续 fresh acquire / fresh resumed accept 才允许重新 materialize canonical `main` 并 consume `.tmpdir`。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-crash-window-tempdir-residue.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- Reference only: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

### Steps
- [x] 重读 roadmap / working-memory / directory-store seam / runtime replay probe，确认这批只锁 `.tmpdir` residue 语义，不重开 file-backed / public wiring / capability wording。
- [x] 新增 `docs/plans/2026-04-16-freepascal-early-data-directory-store-crash-window-tempdir-residue.md`，把 direct/runtime delivery order 与验证命令写清楚。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct residue contract、runtime replay-only restart contract，以及最小 tests-only helper 能力。
- [x] 跑 focused suite，判断 fresh evidence 是否需要触碰 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile-all、diff hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。本批新增了 1 份执行计划、2 条 focused residue contracts、以及 1 个 tests-only runtime probe expectation；没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此本轮保持 tests-only closeout。
- `tests/test_freepascal_tls13_early_data.pas` 新增的高价值合同是：
  - `TestDirectoryReplayStorePreservesTempDirResidueAcrossRepeatedReplayRejects`
  - `TestDirectoryReplayStorePreservesTempDirResidueAcrossRepeatedReplayOnlyRestarts`
- runtime harness 只做了最小扩展：
  - 新增 `TEST_REPLAY_PROVIDER_EXPECT_REJECT_ONLY`
  - `RunReplayProviderRuntimeReplayProbeMode(...)` 现在支持 replay-only child，不新增新的 child mode
- fresh focused run 直接 GREEN，说明当前 directory-store 已经天然满足这批最值钱的 residue semantics：
  - pure replay reject 不会误 consume live `.tmpdir`
  - repeated provider rebuild / repeated replay-only restart 仍会继续 reject original session
  - 只有后续 fresh acquire / fresh resumed accept 才重新 materialize canonical `main` 并 consume `.tmpdir`
- next queue 继续收紧到：
  - `.tmpdir` / `.bakdir` fallback corruption fail-closed
  - directory-store filesystem blocker 语义
  - 更深的 crash-window write-interruption drift 仅在 fresh RED 出现时再重开

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_crash_window_tempdir_residue_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-crash-window-tempdir-residue.md tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data directory store durability hardening)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把刚落地的 `TFreePascalDirectoryEarlyDataReplayStore` 从 prototype 推进到第一批真正高价值的 durability hardening：cross-process lock fail-closed、orphan lock ignore、以及 `.tmpdir` / `.bakdir` replay truth 在 provider rebuild 与 runtime restart 两条路径上的 bounded recovery。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-durability-hardening.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 prototype batch、working-memory、directory-store seam 与 runtime replay probe，确认这批应该只补 directory-store durability，而不是重开 file-backed / public wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 与 runtime restart 的 durability contracts，并补最小 tests-only helper。
- [x] 跑 focused suite，观察 fresh 结果只指向 directory-store fallback / cross-process truth。
- [x] 最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，把 readable resolution 收紧到 `main > .tmpdir > .bakdir` 且保持 bad-canonical fail-closed。
- [x] 跑 focused / adjacent / completeness / compile / hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 现在新增了 7 条 directory-store durability contracts，分别覆盖：
  - cross-process lock fail-closed
  - orphan lock ignore across provider rebuild
  - orphan `.tmpdir` replay-truth recovery across provider rebuild
  - `.bakdir` replay-truth recovery across provider rebuild
  - runtime cross-process lock fail-closed
  - runtime restart replay-truth recovery from orphan `.tmpdir`
  - runtime restart replay-truth recovery from `.bakdir`
- 这批最小生产修复继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`：
  - 新增 `ResolveReadableDirectoryName(...)` 的 bounded precedence：`main > .tmpdir > .bakdir`
  - 新增 `LoadEntriesFromDirectory(...)`，确保 fallback 目录继续走与 canonical `main` 相同的 fail-closed 校验路径
  - 保持 `main` 已存在但形态错误时不允许静默回退到 fallback
- runtime child helper 现在也已经能显式消费 `directory_store` context path；同批 tests-only cleanup 也已覆盖 `.ready` / `.release` / `.session.bin` / `.graceful` / `.context_path` marker，避免目录型批次污染后续 focused run。
- 这说明 directory-store 已经不再只是 prototype：第一批最值钱的 cross-process / restart durability truth 已被锁住，同时 public API、builder / factory / config、`TFreePascalContext`、`TFreePascalConnection` 与 capability wording 都保持不变。
- 下一条最高 ROI 队列已继续收紧到更重但仍 bounded 的 directory-store family：crash-window / tempdir residue、fallback corruption fail-closed（`.tmpdir` / `.bakdir`）、filesystem blocker 语义；不再建议回头重开 file-backed `.bak` family、managed boundary 或 public wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_durability_hardening_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-durability-hardening.md src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data directory store prototype)

### Goal
- 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加第二个 backend-private 单机可控持久化 concrete store：directory-backed replay store prototype，并用 direct/runtime focused contracts 证明现有 `IFreePascalEarlyDataReplayStore` seam 已经足够承载第二种本地持久化形态。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-directory-store-prototype.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / store seam / focused early-data tests，确认当前最高 ROI 已从 file-backed `.bak` family 转到“第二 concrete local persistence shape”。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 directory-store helper 与 4 条 focused RED。
- [x] 跑 focused suite，观察 fresh RED 落在缺失 `fafafa.ssl.freepascal.earlydatareplay.dirstore` 新单元。
- [x] 新增 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，以最小目录型 store 实现 `IFreePascalEarlyDataReplayStore`。
- [x] 跑 focused / adjacent / completeness / compile / hygiene，并回写 roadmap / working-memory。

### Status
- 已完成。新增了 backend-private `TFreePascalDirectoryEarlyDataReplayStore`，通过现有 `TFreePascalStoreBackedEarlyDataReplayProvider` / `TFreePascalProviderBackedEarlyDataReplayLedger` / `InstallStoreBackedReplayLedger(...)` 接线表达第二个单机本地持久化形态，不需要改 `src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas`、builder / factory / config 或 public API。
- `tests/test_freepascal_tls13_early_data.pas` 新增了 4 条最值钱的 focused contracts：
  - direct provider rebuild durability
  - expired-entry prune after rebuild
  - runtime cross-context replay reject
  - corrupt directory entry fail-closed + cleanup recovery
- 并行 reviewer 后续又指出了两个真实 residual gap，且都已在同批收口：
  - existing-but-unreadable directory 被误当成 empty store 的 fail-open 风险
  - snapshot write failure 之后 `.tmpdir` 残留的 cleanup 风险
- 因此本批 focused suite 最终新增到 6 条 directory-store 合同，其中 2 条是 review-driven hardening：
  - unreadable store directory fail-closed at runtime
  - `.tmpdir` cleanup after partial snapshot write failure
- fresh RED 先准确落在缺失新单元；最小 GREEN 后 focused suite 全绿，说明当前 store/provider seam 已经能稳定承载第二 concrete store shape，而不是只对 shared in-memory / file-backed 特化成立。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`；本批只是新增 opt-in backend-private prototype，不是默认 durability 升级。
- 下一条最高 ROI 队列已经收紧到 directory-store 的更重 durability family：cross-process coordination、crash-window / tempdir residue、backupdir fallback、filesystem blocker 语义。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first RED：`Fatal: Can't find unit fafafa.ssl.freepascal.earlydatareplay.dirstore used by test_freepascal_tls13_early_data`
  - review-driven RED：`❌ Unreadable directory replay-store through runtime path should reject early-data`
  - rerun PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_prototype_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 185 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-prototype.md src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data `.bak` trailing-garbage fallback closeout)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 的 `.bak` fallback 读路径补齐最后一个极小 corruption sidecar：当 `main` 缺失、`.tmp` 不存在、provider 退回读取 `.bak`，即使 `.bak` 拥有合法 header + 合法 entry，只要尾部还带 trailing garbage bytes，也必须继续 fail closed。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-bak-trailing-garbage-fallback-closeout.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`（仅 fresh RED 需要时）
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / `.bak` fallback corruption contracts / file-backed load path，确认当前最高 ROI 剩余缺口已收缩到 trailing-garbage sidecar。
- [x] 新增 2026-04-16 计划文件，并把本批目标收敛到 direct/runtime `.bak` trailing-garbage fail-closed contracts。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 的现有 `.bak` corruption 矩阵中各追加一个 `trailing_garbage` case，并补最小 fixture helper。
- [x] 跑 focused test 观察 fresh 结果，并只在 fresh RED 指向 provider drift 时最小查看/修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。本批只在 `tests/test_freepascal_tls13_early_data.pas` 里新增 1 个 trailing-garbage fixture helper，并在现有 direct provider / installer runtime 两条 `.bak` corruption matrix 中各追加 1 个 `trailing_garbage` case。
- 这批 focused fresh run 直接 GREEN：当前 provider 已经天然满足 `.bak` trailing-garbage fallback fail-closed contracts，不需要修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- 新增合同现在直接锁住两件最值钱的 truth：
  - 当 `main` 缺失、`.tmp` 不存在且 readable resolution 退回 `.bak` 时，合法 header + 合法 entry 后的尾随垃圾字节不会被误当成有效 replay truth
  - direct provider 与 runtime resumed early-data path 都继续 reject fresh blocked session，也不会误恢复 original replay truth；同时 canonical main 仍缺失、`.tmp` 不残留、corrupt `.bak` 保留
- 并行 reviewer（`gpt-5.4`）已确认无高严重度问题；本批保持 tests-only 是最稳的收口方式。
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列已回到更重的 provider / durability / persistence 形态；不再建议重开当前 `.bak` corruption family、managed boundary 或现有 parity 接线。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_trailing_garbage_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-bak-trailing-garbage-fallback-closeout.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data `.bak` fallback corruption hardening)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 新增 `.bak` fallback 读路径的 corruption fail-closed 语义：当 `main` 缺失、`.tmp` 不存在、provider 退回读取 `.bak` 时，invalid version / truncated payload / invalid count / invalid key-length 都必须继续 fail closed，且不允许隐式重建 canonical main。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-bak-fallback-corruption-hardening.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`（仅 fresh RED 需要时）
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed store readable resolution / corrupt-store contracts，确认当前最高 ROI 已收缩到 `.bak` fallback corruption hardening。
- [x] 新增 2026-04-16 计划文件，并把本批目标收敛到 direct/runtime `.bak` fallback corruption fail-closed contracts。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 与 installer runtime 的 `.bak` fallback corruption contracts。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小查看/修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 2 个 `.bak` fallback helper 与 2 条 focused contracts，分别覆盖 direct provider 与 store-backed runtime path 在 `main` 缺失、`.tmp` 不存在时读取 corrupt `.bak` 的 fail-closed 语义。
- 这批 focused fresh run 直接 GREEN：当前 provider 已经天然满足 `.bak` fallback corruption contracts，不需要修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- 新增合同现在直接锁住两件最值钱的 truth：
  - invalid version / truncated payload / invalid count / invalid key-length 的 corrupt `.bak` 都不会被误当成空 ledger 或有效 replay truth
  - runtime resumed early-data path 会继续 handshake success、session reused、early-data rejected，同时不隐式重建 canonical main，`.tmp` 不残留，corrupt `.bak` 保留
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列继续保持为更重的 provider / durability / persistence 形态；如果只想补一个极小 sidecar，可再单独锁 `.bak` 合法头部后带 trailing garbage` 的 fail-closed 分支。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_fallback_corruption_hardening_20260416`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-bak-fallback-corruption-hardening.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-16 Plan (Execution / FreePascal early-data permission-write-failure shapes)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 的下一批高 ROI permission/write-failure family：deterministic `.tmp` write-open denied 与 deterministic existing-main backup-promotion denied，都要继续 fail closed、保住 canonical main truth，并在 normal rebuild 后恢复 fresh session accept + replay reject。

### Files
- `docs/plans/2026-04-16-freepascal-early-data-permission-write-failure-shapes.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`（仅 fresh RED 需要时）
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed store / scripted runtime helper，确认当前最高 ROI 已收缩到 deterministic permission/write-failure shapes，而不是重开现有 seam / wiring。
- [x] 新增 2026-04-16 计划文件，并把本批目标收敛到 hook-based temp-write-open denied 与 backup-promotion rename denied。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的两组 deterministic contracts。
- [x] 继续补 store-backed runtime `Accept` path 的对应 contracts，优先复用 `InstallStoreBackedReplayLedger(...)`。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小查看/修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 2 个 scripted file-backed store subclasses 与 4 条 focused contracts，分别覆盖 direct provider 与 store-backed runtime path 上的 deterministic temp write-open denied / backup-promotion denied。
- 这批 focused fresh run 直接 GREEN：当前 provider 已经天然满足这组 permission/write-failure contracts，不需要修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- 新增合同现在直接锁住两件最值钱的 truth：
  - `.tmp` write-open denied 时，fresh blocked session 继续 fail closed，canonical main bytes 不变，`.tmp` / `.bak` 都不残留
  - existing-main replace fallback 上 `main -> .bak` promotion denied 时，fresh blocked session 继续 fail closed，canonical main bytes 不变，`.tmp` cleaned 且 `.bak` 不产生
- normal provider / installer rebuild 后，两组失败形态都会继续 reject original truth、accept previously blocked fresh session、并在下一次 replay reject。
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列已移动到更重的 provider / durability / persistence 形态；本批不重开 seam / builder / factory / context / connection wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_permission_write_failure_shapes_20260416 > tmp/freepascal_tls13_completeness_early_data_permission_write_failure_shapes_20260416.log 2>&1; rc=$?; tail -n 40 tmp/freepascal_tls13_completeness_early_data_permission_write_failure_shapes_20260416.log; exit $rc`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-permission-write-failure-shapes.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-15 Plan (Execution / FreePascal early-data `.bak` residue semantics)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 的 `.bak` residue semantics：成功路径 cleanup failure 不应回滚 fresh truth；而残留 `.bak` 在下一次写入前无法删除时，provider / runtime path 仍应 fail closed 并保住旧 truth。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-bak-residue-semantics.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`（仅 fresh RED 需要时）
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed store / scripted runtime helper，确认当前最高 ROI 已收缩到 `.bak` residue semantics，而不是重开现有 seam / wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的 `.bak` cleanup failure success + stale `.bak` delete-failure fail-closed 合同。
- [x] 继续补 runtime `Accept` path 的对应合同，优先复用 `InstallStoreBackedReplayLedger(...)`。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小查看/修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 1 个 scripted `.bak` delete-failure store 与 2 条 focused contracts，分别覆盖 direct provider 与 store-backed runtime path。
- 这批 focused fresh run 直接 GREEN：当前 provider 已经天然满足 `.bak` residue semantics，不需要修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- 新增合同现在直接锁住两件最值钱的 residual truth：
  - backup-assisted replace 成功后，`.bak` cleanup delete failure 仍保持 fresh truth 成功落在 canonical main
  - 下一次 fresh save 遇到无法预清理的 stale `.bak` 时，provider / runtime path 都继续 fail closed，并保住 canonical main truth
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列已继续收紧到更真实的 permission/write-failure 形态；本批不重开 seam / builder / factory / context / connection wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_residue_semantics_20260415 > tmp/freepascal_tls13_completeness_early_data_bak_residue_semantics_20260415.log 2>&1; rc=$?; tail -n 40 tmp/freepascal_tls13_completeness_early_data_bak_residue_semantics_20260415.log; exit $rc`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-bak-residue-semantics.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-15 Plan (Execution / FreePascal early-data backup restore failure recovery)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 在 backup-assisted replace 上的 restore-failure residual risk：当 `main -> .bak` 成功、second `temp -> main` 失败、且 `.bak -> main` restore 也失败时，既有 persisted replay truth 仍应可通过最小恢复路径继续被消费，而不是因为 canonical main 缺失而完全丢失。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-backup-restore-failure-recovery.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed store / store-backed runtime helper，确认当前最高 ROI 已收缩到 backup restore failure branch，而不是重开现有 seam / wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的 deterministic restore-failure RED。
- [x] 继续补 runtime `Accept` path 的对应 deterministic contract，优先复用 `InstallStoreBackedReplayLedger(...)`。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 2 组 deterministic backup restore failure recovery contracts，分别覆盖 direct provider 与 store-backed runtime path。
- fresh focused RED 暴露的真实 provider gap 是：当 `main -> .bak` 成功、second `temp -> main` 失败、且 `.bak -> main` restore 也失败时，既有 replay truth 虽然还留在 `.bak`，但当前 readable resolution 不会继续消费它，导致同一 store / rebuild 后都看不到旧 truth。
- 最小生产修复继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 内部：`ResolveReadableStoreFileName(...)` 现在改为优先读取 `main`，其次 orphan `.tmp`，最后只在 `main` 缺失且 `.tmp` 不存在时把 `.bak` 当作 restore-failure-only 的 bounded fallback truth source。
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列已继续收紧到更真实的 permission/write-failure 形态与 `.bak` residue semantics；本批不重开 seam / builder / factory / context / connection wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first RED：`Backup restore failure should still reject the original replay truth through the same scripted store`
  - rerun PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_backup_restore_failure_recovery_20260415 > tmp/freepascal_tls13_completeness_early_data_backup_restore_failure_recovery_20260415.log 2>&1; rc=$?; tail -n 40 tmp/freepascal_tls13_completeness_early_data_backup_restore_failure_recovery_20260415.log; exit $rc`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-backup-restore-failure-recovery.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-15 Plan (Execution / FreePascal early-data existing-main replace truth preservation)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store `SaveEntries(...)` 在 existing-main replace fallback 上的 truth-preservation gap：当 first `temp -> main` 失败、fallback 迁移 old main 时，既有 persisted replay truth 不应因 second `temp -> main` 再失败而丢失。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-existing-main-replace-truth-preservation.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / file-backed store / store-backed runtime helper，确认当前最高 ROI 已收缩到 existing-main replace fallback / atomic-replace truth-preservation，而不是重开现有 seam / wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的 deterministic existing-main replace truth-preservation RED。
- [x] 继续补 runtime `Accept` path 的对应 deterministic contract，优先复用 `InstallStoreBackedReplayLedger(...)`。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 2 组 deterministic existing-main replace fallback truth-preservation contracts，分别覆盖 direct provider 与 installer runtime path。
- fresh focused RED 暴露的真实 provider gap 是：focused subclass 试图 override `RenameFileAt(...)` 时直接编译失败，说明 file-backed replay store 还没有最小 overrideable file-op hooks，也就无法稳定锁住 existing-main replace fallback failure shape。
- 最小生产修复继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 内部：为 `TFreePascalFileEarlyDataReplayStore` 增加 protected virtual file-op wrappers，并把 `SaveEntries(...)` 改成 backup-assisted replace（`main -> .bak` / failure restore / success best-effort cleanup）。
- fresh focused / adjacent / completeness / compile / hygiene 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列已继续收紧到 backup restore failure branch 与更真实的 permission/write-failure 形态；本批不重开 seam / builder / factory / context / connection wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first RED：`There is no method in an ancestor class to be overridden: "RenameFileAt(...)"`
  - rerun PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_existing_main_replace_truth_preservation_20260415`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-existing-main-replace-truth-preservation.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-15 Plan (Execution / FreePascal early-data SaveEntries boundaries)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 的 `SaveEntries(...)` 再补一批高 ROI 的持久化写入边界合同：`.tmp` 写入失败时保住既有 replay truth，canonical main-path replace / rename 边界失败时 fail closed，blocker 移除后 recovery 重新 accept，随后 replay 继续 reject。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-saveentries-boundaries.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`（仅 fresh RED 需要时）
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / focused helpers / file-backed provider，确认下一批最高 ROI 已经收缩到 `SaveEntries(...)` 的 temp/main-path 写入边界，而不是重开 seam / builder / factory / context / connection wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的 temp-path write-failure preserves-old-truth 合同与 canonical main-path rename-boundary fail-closed 合同。
- [x] 继续补 installer runtime 的对应 contracts，保持真实 resumed early-data `Accept` path。
- [x] 跑 focused test 观察 fresh RED，并只在 fresh RED 指向 provider drift 时最小查看/修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 4 条 SaveEntries 边界合同，分别覆盖 direct provider / installer runtime 的 temp-path write-failure preserves-existing-truth 与 canonical main-path rename-boundary fail-closed / recovery。
- fresh focused RED 暴露的唯一生产漂移是 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 在 canonical main-path rename-boundary fail-closed 时会遗留 `<store>.tmp`；已通过把 temp cleanup 包到外层 `finally` 最小修复。
- fresh focused / adjacent / completeness / compile 均已通过；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 下一条最高 ROI 队列继续收紧到 existing-main replace fallback / atomic-replace truth-preservation gap，再往后才是更真实 permission/write-failure 形态；本批不重开 seam / builder / factory / context / connection wiring。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_saveentries_boundaries_20260415_final4`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-saveentries-boundaries.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-15 Plan (Execution / FreePascal early-data filesystem failure shapes fail-closed and recovery)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 再补一批高 ROI 的 filesystem failure-shape 合同：路径阻塞时 fail closed，blocker 清除后 recovery 重新 accept，同一 session 后续继续 reject。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / focused helper / file-backed provider，确认当前最高 ROI 是 filesystem failure shapes，而不是重开 seam / builder / factory / context / connection wiring。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider filesystem blocker fail-closed/recovery 合同。
- [x] 继续补 installer runtime filesystem blocker fail-closed/recovery 合同。
- [x] 跑 focused test，并根据 fresh evidence 决定是否需要任何 `src/` 变更（结果：focused suite 直接 GREEN，继续保持 tests-only）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited git hygiene + direct file scan。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 direct provider 与 installer runtime 两组 filesystem blocker contracts，并以一个最小 tests-only cleanup helper 扩展 `CleanupReplayProviderStoreFiles(...)` 对 empty-directory blocker 的清理；fresh focused / adjacent regressions / completeness gate / compile gate 均通过，无需修改任何 `src/` 单元。

## 2026-04-15 Plan (Execution / FreePascal early-data store-path identity and cross-process boundary)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，先锁住同一物理 replay-store file 的路径身份收敛合同，再锁住跨进程 different-store boundary isolation，继续以最小返工成本推进 file-backed early-data durability truth。

### Files
- `docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / runtime replay-probe / installer runtime helpers，确认本批最高 ROI 是 store-path identity convergence 与 cross-process different-store boundary，而不是重开 seam / builder / factory / context / connection wiring。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 store-path identity / cross-process boundary 批次。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 same-file relative/absolute identity contract，再补 parent/child same-file alias contract。
- [x] 继续补 cross-process different-store boundary contract；最终只对现有 replay-probe 增加一个最小 expectation 分支，没有新增 child mode。
- [x] 跑 focused test，并根据 fresh RED 决定是否需要任何 `src/` 变更（结果：fresh RED 只落在 tests/harness，未触碰任何 `src/`）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited git hygiene + direct file scan。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。fresh evidence 证明这批继续保持 tests/docs/working-memory only；same-file path identity convergence 与 cross-process different-store boundary isolation 已收口，没有出现任何需要修改 `src/` 的 runtime drift。

## 2026-04-14 Plan (Execution / FreePascal early-data runtime file-store fail-closed recovery and isolation)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 file-backed anti-replay 已有 provider-level corrupt/orphan semantics 提升到真实 runtime early-data `Accept` path，并补一条最小 store-boundary isolation/path-swap contract。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 provider-level corrupt/orphan/lock contracts、runtime installer/public contracts与 working-memory，确认下一批最高 ROI 是 runtime file-store fail-closed/recovery/isolation，而不是重开现有 seam / public wiring。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 runtime file-store fail-closed/recovery/isolation 批次。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 corrupt main store、corrupt orphan temp、orphan recovery / ignore、store-boundary isolation 的 runtime RED/contracts。
- [x] 只做最小 tests/harness cleanup；默认不碰 `src/`。
- [x] 跑 focused test，并根据 fresh RED 决定是否需要任何 `src/` 变更（结果：focused suite 直接 GREEN，继续保持 tests/harness only）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited git hygiene + direct file scan。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 5 条 installer runtime contracts 与 4 个最小 runtime helpers；fresh focused run 直接 GREEN，说明 file-backed provider 的 main/orphan/lock/path-swap 语义已经自然延伸到真实 resumed early-data `Accept` path，无需修改任何 `src/` 单元，capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_runtime_file_store_failclosed_recovery_isolation_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出
- 直接文件空白扫描
  - `docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md`、`docs/ROADMAP.md`、`tests/test_freepascal_tls13_early_data.pas`、`task_plan.md`、`progress.md` 无 trailing whitespace，且保留 final newline
  - `findings.md` 顶部本批 writeback 无问题；文件更深处仍有 pre-existing trailing whitespace 历史噪音，本批未顺手重写

## 2026-04-14 Plan (Execution / FreePascal early-data installer-to-public mixed cross-process convergence)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 再补一层最高 ROI 的 mixed cross-process convergence evidence：backend-private installer 父进程 materialize 的 replay truth，在子进程经 builder 或 one-shot factory public path 重建后仍会 reject replay，同时 fresh resumed early-data 继续 accept。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / installer restart helper / child selector helper，确认当前最高 ROI 是 installer-to-public mixed cross-process convergence，而不是再扩 crash/lock/stress 或重开 `src/` seam。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 installer-to-public mixed cross-process convergence 批次。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 installer-parent -> builder-child 与 installer-parent -> factory-child 的 runtime RED/contract。
- [x] 继续复用现有 `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE` 与 selector/marker，不新增 child mode。
- [x] 跑 focused test，并根据 fresh RED 决定是否需要任何 `src/` 变更（结果：只需最小 tests/harness fix，无需任何 `src/` 变更）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、limited git hygiene + direct file scan。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 installer-parent -> builder-child / factory-child cross-process runtime contracts；fresh RED 只暴露 child `context_path` marker 证据不够强，最小 GREEN 仅把 marker 从 `TouchFile(...)` 收紧为写入 normalized public path 文本，随后 focused / adjacent regressions / completeness gate / compile gate / limited hygiene 全部通过；无需修改任何 `src/` 单元，capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first fresh run: FAIL（`❌ Installer-parent/builder-child runtime replay probe should record the requested builder public path`）
  - after minimal harness fix: PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_installer_public_mixed_cross_process_convergence_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-14 Plan (Execution / FreePascal early-data mixed public-path cross-process durability)

### Goal
- 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 再补一层最高 ROI 的 mixed public-path cross-process runtime evidence：父进程经 builder 或 one-shot factory materialize 的 replay truth，在子进程经另一条 public path 重建后仍会 reject replay，同时 fresh resumed early-data 继续 accept。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-cross-process-durability.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / focused runtime helpers，确认上一批已完成，而下一批最高 ROI 是 mixed public-path cross-process durability，不是再碰 seam、builder/factory same-process parity 或默认 capability wording。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 mixed public-path cross-process durability 批次。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 builder-parent -> factory-child 与 factory-parent -> builder-child 的 runtime RED/contract。
- [x] 最小扩展现有 `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`，给 child replay probe 加 optional public-path selector 与最小 marker evidence；默认 installer path 保持不变。
- [x] 跑 focused test，并根据 fresh RED 决定是否需要任何 `src/` 变更（结果：无需任何 `src/` 变更，继续保持 tests/harness only）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 mixed cross-process public-path runtime contracts，并以最小 harness-only 方式扩展了 existing replay child selector / marker evidence；fresh focused / adjacent regressions / completeness gate / compile gate 全部通过，且当前 worktree 下相关文件的 hygiene 也已通过“空 `git diff --check` + 直接文件空白扫描”记录收口；无需修改任何 `src/` 单元。

## 2026-04-14 Plan (Execution / FreePascal early-data mixed public-path durability closeout)

### Goal
- 在不改默认 shipped behavior、capability wording 或现有 `src/` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 的 mixed public path 再补一层低返工高收益的 durable truth：builder 与 one-shot factory 互相消费同一个 replay store 时，跨 context replay 仍正确 reject，且 expired persisted entries 通过 public-installed ledger 仍会 prune。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-durability-closeout.md`
- `docs/ROADMAP.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 roadmap / working-memory / focused test helpers，确认下一批最高 ROI 是 mixed public-path durability closeout，而不是重开当前 seam、builder/factory parity 或 restart smoke。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 mixed public-path durability closeout 批次。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 builder->factory 与 factory->builder mixed runtime RED/contract。
- [x] 再补 public-installed ledger expired-entry prune RED/contract。
- [x] 跑 focused test，并根据 fresh evidence 决定是否需要任何生产修复（结果：无需生产修复）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、diff hygiene。
- [x] 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 mixed builder/factory runtime contracts 与 public-installed prune contract，fresh focused / adjacent regressions / completeness gate / compile gate / diff hygiene 全部通过，且无需修改任何 `src/` 单元。

## 2026-04-14 Plan (Execution / FreePascal early-data public opt-in runtime durability)

### Goal
- 在不改 capability wording、默认 shipped behavior、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data 的 public opt-in 入口补齐一层真实 runtime durability evidence，优先锁住 builder 与 `TSSLFactory.CreateContext(const AConfig)` 这两条路径。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-public-optin-runtime-durability.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 public builder/config/factory wiring、focused runtime restart helpers 与 working-memory，确认下一批最高 ROI 是 public opt-in runtime durability，而不是再碰底层 seam。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 public opt-in runtime durability 批次。
- [x] 拉起 `gpt-5.4` 团队：tests worker 只负责 `tests/test_freepascal_tls13_early_data.pas`，reviewer 只看 public-path runtime contract 最小 shape。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 builder runtime durability RED/contract。
- [x] 若仍保持低风险，再补 one-shot factory config runtime durability RED/contract。
- [x] 跑 focused test，并根据 fresh evidence 决定是否需要任何生产修复（结果：无需生产修复）。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 builder 与 one-shot factory public-path runtime durability 合同，fresh focused / adjacent regressions / completeness gate / compile gate / diff hygiene 全部通过，且无需修改任何 `src/` 单元。

## 2026-04-14 Plan (Execution / FreePascal early-data file-backed runtime tiny restart loop and harness cleanup)

### Goal
- 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补一个最小 3-round runtime restart smoke，并做最小 tests-only harness cleanup，继续以最低返工成本锁住 restart durability truth。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-tiny-restart-loop-and-harness-cleanup.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 runtime restart / crash / lock 合同、focused helpers 与 working-memory，确认下一批最高 ROI 是 tiny repeated-restart smoke + minimal harness cleanup，而不是继续碰 production seam。
- [x] 写本轮执行计划，并把 working-memory 顶部切到 tiny restart loop 批次。
- [x] 拉起 `gpt-5.4` 团队：tests worker 只负责 `tests/test_freepascal_tls13_early_data.pas`，reviewer 只看 harness brittleness / cleanup。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 tiny 3-round restart runtime RED。
- [x] 只做了最小 tests-only helper cleanup 与 suite registration。
- [x] 跑 focused test，并根据 fresh evidence 确认不需要任何生产修复。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。当前 fresh evidence 继续证明这批保持了 tests/plan/working-memory only；未触碰 `src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。

## 2026-04-14 Plan (Execution / FreePascal early-data file-backed runtime crash, lock, and stress)

### Goal
- 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补齐下一批最高 ROI 的 runtime durability evidence：crash-window recovery、runtime lock-contention fail-closed、以及一个最小 restart stress smoke。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-crash-lock-and-stress.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 live seams、working-memory 与已有 restart / lock / orphan-temp contracts，确认当前最高 ROI 的下一波是 crash-window + runtime lock-contention，而不是继续扩 public/config/builder surface。
- [x] 写本轮执行计划，并把 working-memory 顶部切到这批 crash/lock/stress 目标。
- [x] 拉起 `gpt-5.4` 团队：tests worker 专注 `tests/test_freepascal_tls13_early_data.pas`，read-only reviewers 专注 provider/runtime 风险、crash-window 合同强度与 helper 复用建议。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 P0 crash-window runtime contract，再补 P0 runtime lock-contention fail-closed contract。
- [x] 评估 P1 tiny restart stress smoke；结论是本批 defer，不强行引入额外 child-state 协议和更脆的 timing/state 负担。
- [x] 跑 focused test；fresh RED 只暴露出当前 FPC `TProcess.ExitCode` 对非零退出码不可依赖，因此把 crash-window 合同收紧为“真实 crash child 不会回到 graceful return path”的行为证据，未触碰生产代码。
- [x] 跑 backend basic、capability cache、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。P0 crash-window 与 runtime lock-contention 合同都已通过 fresh runtime evidence 锁住；本批保持 tests/plan/working-memory only，没有改 `src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first strengthened crash-window run: FAIL（`Crash-window runtime child should exit via the simulated crash code after accepting early-data (expected=86 actual=0)`）
  - after adapting the contract to avoid unreliable `TProcess.ExitCode`: PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_file_backed_runtime_crash_lock_stress_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- tests/test_freepascal_tls13_early_data.pas docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-crash-lock-and-stress.md task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-14 Plan (Execution / FreePascal early-data file-backed runtime restart durability)

### Goal
- 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补一条真实的“跨进程 / 重启后 replay truth 仍保留”的 runtime durability 合同。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-restart-durability.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 file-backed provider/runtime helper、working-memory 与已有 cross-context / cross-process contracts，确认当前最高 ROI 剩余缺口是 restart durability runtime evidence，而不是再扩 public/config/builder surface。
- [x] 拉起 `gpt-5.4` 团队 sidecar 做只读 review，确认最佳最小批次是 “1 个 installer-based restart runtime test + 1 个 child-mode helper”，并继续避免先碰 `context.pas` / `connection.pas`。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 child restart helper + installer-based runtime RED，锁住 replay truth 穿过进程边界后仍 reject，而 fresh resumed session 仍 accept。
- [x] 跑 focused test；fresh RED 只暴露 Pascal declaration-order 问题，补 forward declarations 后直接 GREEN，因此没有任何 production 修复。
- [x] 跑 capability wording regressions、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。file-backed installer/runtime path 现在已有 fresh “跨进程 / 重启后 replay truth 仍 reject，fresh resumed session 仍 accept” evidence；本批只改 tests/plan/working-memory，未触碰 production code 或 capability wording。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first fresh run after child fresh-accept extension：FAIL（`Identifier not found "CaptureServerIssuedSession"`）
  - after forward declaration fix：PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_file_backed_runtime_restart_durability_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`

## 2026-04-14 Plan (Execution / FreePascal early-data runtime parity and doc closeout)

### Goal
- 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data callback/file-backed opt-in 路径补一层真实 runtime parity evidence，并把当前 shipped truth / opt-in 边界回写到用户文档。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-runtime-parity-and-doc-closeout.md`
- `tests/test_freepascal_tls13_early_data.pas`
- `docs/ROADMAP.md`
- `docs/INTEGRATION_GUIDE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 runtime test surfaces、roadmap/integration 文档与 working-memory，确认当前最高 ROI 剩余工作是 runtime parity + docs closeout，而不是再改生产 seam。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 fresh runtime contracts，锁住 callback/file-backed opt-in path 在 local lifecycle toggle 前后仍保留 replay truth。
- [x] 运行 focused test；fresh evidence 先暴露了“`SetSessionCacheMode/Size` 会关闭 resumption 本身”的 runtime 假设错误，随后把新 contracts 收紧为 managed ledger local-gate toggles，并在不改生产代码的前提下获得 GREEN。
- [x] 更新 `docs/ROADMAP.md` / `docs/INTEGRATION_GUIDE.md`，把当前 shipped truth、opt-in 边界与 next queue 写清，并同步路线图日期。
- [x] 跑 focused regression、capability wording regressions、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。callback/file-backed opt-in replay path 的 runtime parity 现在通过 local managed-ledger gate toggles 获得 fresh evidence；docs closeout 也已同步到当前 shipped truth；`TFreePascalContext` / `TFreePascalConnection` / public surface / capability wording 均保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - fresh failure first：`Failed to decrypt server handshake flight: AES-GCM decryption/authentication failed`
  - green after tightening tests to local managed-ledger toggles：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_runtime_parity_doc_closeout_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-runtime-parity-and-doc-closeout.md docs/ROADMAP.md docs/INTEGRATION_GUIDE.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## 2026-04-14 Plan (Execution / FreePascal early-data managed seam contract lock)

### Goal
- 在不改变 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，把新引入的 managed replay seam 边界锁成 focused contracts，并补最小 internal-only 注释，减少后续误用与返工。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-managed-seam-contract-lock.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 managed replay seam、focused tests、KnownIssues wording tests 与 working-memory，确认当前最高 ROI 剩余工作是 boundary contract lock + comment closeout，而不是再改 context / connection / public surface。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED / fresh evidence，锁住：shared in-memory managed semantics 只作用于 default seam、callback/file-backed non-managed providers 不被 local lifecycle toggle 隐式 wipe、managed hook exception 继续 bounded swallow。
- [x] 运行 focused RED；fresh result 已天然 GREEN，因此不伪造行为修复，只在 `src/fafafa.ssl.freepascal.earlydatareplay.pas` 做 minimal comment closeout，并按 review 建议把一个依赖构造器 side-effect 的断言收紧成增量断言。
- [x] 跑 focused regression、capability wording regressions、completeness gate、compile gate、diff hygiene。
- [x] 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。managed seam 的“shared in-memory only”边界、non-managed provider lifecycle 语义与 exception swallow 语义现在都有显式 focused contracts；`earlydatareplay.pas` 只补了 internal-only / thin-wrapper / deliberate swallow 注释；public surface、context/connection wiring 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS（`✅ FreePascal backend basic checks passed`）
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS（`✓ FreePascal KnownIssues runtime alignment verified`）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_managed_seam_contract_lock_20260414` => PASS（`[PASS] freepascal tls13 completeness gate finished`）
- `python3 scripts/compile_all_modules.py` => PASS（`编译成功: 184 (100.0%)`）
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-managed-seam-contract-lock.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS（无输出）

## 2026-04-14 Plan (Execution / FreePascal early-data default in-memory ledger convergence)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，把 FreePascal TLS 1.3 early-data 默认 in-memory anti-replay 路径收敛到已经稳定的 replay-store seam，移除默认路径里残留的独立 replay / prune / capacity 实现，并补齐 shared in-memory store-backed path 的 managed parity 合同。

### Files
- `docs/plans/2026-04-14-freepascal-early-data-default-inmemory-ledger-convergence.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 2026-04-14 计划、working-memory、`earlydatareplay.pas` / `context.pas` / focused tests，并确认最佳收敛路径仍是 Option C：保持 context 装配不动，把默认 in-memory ledger 收敛成 store-backed thin wrapper。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 shared in-memory store-backed path 的 RED，锁住 disable / re-enable clear parity 与 capacity=2 bounded eviction parity，并补上 default context-level parity contract 作为 shipped wrapper 的 GREEN evidence。
- [x] 运行 focused RED，并观察到真实失败点落在 shared in-memory store-backed path 缺少 managed clear / capacity 行为，而不是 accept path 或 context wiring。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.pas`，新增 backend-private managed store/provider seam、给 shared in-memory store 补 clear / capacity / eviction、让 provider-backed ledger forward managed hooks，并把 `TFreePascalInMemoryEarlyDataReplayLedger` 收敛成 wrapper。
- [x] 跑 focused regression、focused completeness gate、compile gate，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。default in-memory replay ledger 不再维持独立 replay-state 实现，而是通过 shared in-memory replay store + store-backed provider + provider-backed ledger 的单一路径工作；shared in-memory store-backed direct contracts 现在覆盖 disable / re-enable clear parity 与 bounded eviction parity；context / accept path / public surface 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（`❌ Re-enabled shared in-memory store-backed ledger should clear replay truth so the same session can acquire again`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_inmemory_ledger_convergence_20260414` => PASS（`[PASS] freepascal tls13 completeness gate finished`）
- `python3 scripts/compile_all_modules.py` => PASS（`编译成功: 184 (100.0%)`）
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-default-inmemory-ledger-convergence.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS（补入 default context-level parity contract 后 fresh rerun）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_inmemory_ledger_convergence_20260414_refresh` => PASS（`[PASS] freepascal tls13 completeness gate finished`）
- `python3 scripts/compile_all_modules.py` => PASS（补入 default context-level parity contract 后 fresh rerun，`184/184`）
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-default-inmemory-ledger-convergence.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS（post-review refresh）

## 2026-04-13 Plan (Execution / FreePascal early-data store runtime fail-closed closeout)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 `InstallStoreBackedReplayLedger(...)` 接入的真实 resumed early-data runtime path 补齐 fresh fail-closed evidence；若这条路径已天然满足，则顺势补一个 backend-private shared in-memory replay-store prototype，证明新的 store seam 不只服务于 file-backed 实现。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-store-runtime-failclosed-closeout.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 store-backed provider / context wiring / resumed accept path / focused tests，并写出本轮 closeout + pivot plan。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 `InstallStoreBackedReplayLedger(...)` runtime fail-closed RED，覆盖 store guard/load/save 的 exception 与 `False` 返回模式，且通过真实 resumed early-data `Accept` path 断言 fail closed。
- [x] 运行 focused RED；fresh evidence 显示 runtime fail-closed closeout 已经是 GREEN，因此不伪造生产修复。
- [x] 把剩余批次价值投到 `src/fafafa.ssl.freepascal.earlydatareplay.pas`，新增 backend-private `TFreePascalSharedInMemoryReplayStore` / guard，并把现有 success-path store-backed tests 切到真实 store class，保留 test-only helper 只服务 failure-mode 注入。
- [x] 跑 focused regression、focused completeness gate、compile gate，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。真实 resumed early-data runtime path 上的 store fail-closed 合同现在有 fresh evidence；该路径本身无需生产修复。与此同时，代码库新增了一个 backend-private shared in-memory replay-store concrete implementation，并已通过现有 store-backed provider rebuild / cross-context runtime contracts 证明新 seam 不再是 file-only。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS（首次 runtime fail-closed RED 尝试已直接 GREEN，`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS（补入 shared in-memory store prototype 后仍保持 GREEN）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_store_runtime_failclosed_closeout_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）

## 2026-04-13 Plan (Execution / FreePascal early-data internal replay-store shape validation)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加 backend-private internal replay-store seam，并验证它能通过现有 context/provider wiring 接到 resumed early-data accept path。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-internal-replay-store-shape-validation.md`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 current replay seams、file-backed provider/store logic、resumed accept wiring 与 working-memory，确认这批最高 ROI 是在 provider 之下补 internal replay-store seam，而不是再改 context / connection / public surface。
- [x] 新建 `docs/plans/2026-04-13-freepascal-early-data-internal-replay-store-shape-validation.md`，把范围锁定为 RED -> 最小 GREEN -> focused verification -> ledger closeout。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 增加 internal replay-store RED：store-backed provider rebuild-shape、store failure fail-closed、install-helper cross-context runtime contract。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.session.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`，新增 store contract、generic store-backed provider/helper，并把 file-backed provider 退化成 store specialization。
- [x] 跑 focused regression、focused completeness gate、compile gate，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。provider 之下现在有一个 backend-private internal replay-store seam；generic store-backed provider 统一承接 replay acquire 语义并对 store failures fail close；file-backed provider 已退化成这个 store seam 的特化实现；现有 context/resumed accept wiring、public API、builder/factory/config surface 与 capability wording 均保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（缺少 `IFreePascalEarlyDataReplayStoreGuard`、`IFreePascalEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`InstallStoreBackedReplayLedger`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_internal_replay_store_shape_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-internal-replay-store-shape-validation.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-13 Plan (Execution / FreePascal early-data callback provider fail-closed and durable shape validation)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data callback/provider 路径补齐 fail-closed 异常语义，并验证 shared callback-owned replay truth 在 provider / ledger 重建后仍成立。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-callback-provider-failclosed-and-durable-shape-validation.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 callback/provider replay seam、context accept path、focused tests 与 working-memory，确认这批最高 ROI 真缺口是 provider exception fail-closed，而不是再开新的 persistence/provider abstraction。
- [x] 新建 `docs/plans/2026-04-13-freepascal-early-data-callback-provider-failclosed-and-durable-shape-validation.md`，把范围锁定为 RED -> 最小 GREEN -> focused verification -> ledger closeout。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 增加 callback/provider exception fail-closed 与 shared callback-store rebuild-shape RED。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.pas`，在 provider-backed acquire 路径收口异常为 fail closed。
- [x] 跑 focused regression、focused completeness gate、compile gate、diff hygiene，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。callback/provider 异常现在不会再打断 resumed early-data handshake；provider-backed replay acquire 在异常场景下会 fail closed 返回 `False`；shared callback-owned replay truth 在 provider / ledger 重建后已有 fresh coverage；public API、builder/factory/config surface 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（`Exploding callback-backed replay provider should not escape exception: Exploding replay provider should be fail-closed`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_callback_provider_failclosed_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- final diff hygiene / whitespace evidence 见 `progress.md` 本批条目。

## 2026-04-13 Plan (Execution / FreePascal early-data custom replay provider installer shape validation)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加 backend-private custom replay-provider installer seam，并用 callback/provider shape 验证当前边界足够稳定。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-custom-replay-provider-installer-shape-validation.md`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 replay provider seam、context installer seam、callback provider helper 与 focused tests，确认这批只做 backend-private generic installer / helper，不重开 public surface。
- [x] 新建 `docs/plans/2026-04-13-freepascal-early-data-custom-replay-provider-installer-shape-validation.md`，把范围锁定为 RED -> 最小 GREEN -> focused verification -> ledger closeout。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 增加 custom provider installer / helper 的 lifecycle、cache-sync、cross-context replay RED。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.context.material.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`，把 file-backed installer 收口成 generic installer 的特化调用。
- [x] 跑 focused regression、focused completeness gate、compile gate、diff hygiene，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。FreePascal context 现在支持 backend-private generic custom replay-provider installer seam；callback/provider shape 已能通过 internal helper 安装到 active replay ledger；现有 file-backed installer 保持可用并退化为 generic installer 的特化调用；public API、builder/factory/config surface 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（缺少 `IFreePascalContextEarlyDataReplayProviderInstaller`、`InstallReplayProviderBackedLedger`、`InstallCallbackBackedReplayLedger`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_custom_replay_provider_installer_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- final diff hygiene / whitespace evidence 见 `progress.md` 本批条目。

## 2026-04-13 Plan (Execution / FreePascal early-data replay-store cross-process coordination hardening)

### Goal
- 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay provider 增加最小跨进程 coordination hardening，避免并发 file-backed acquire 在不同进程间绕过 replay truth。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-replay-store-cross-process-coordination-hardening.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 file-backed replay provider、前一批 orphan `.tmp` recovery hardening、focused tests 与 working-memory，确认当前最高 ROI 剩余缺口已经收窄到 cross-process coordination，而不是重开 public seam / distributed persistence。
- [x] 新建 `docs/plans/2026-04-13-freepascal-early-data-replay-store-cross-process-coordination-hardening.md`，把范围锁定为 RED -> 最小 GREEN -> focused verification -> ledger closeout。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 增加 cross-process lock contention fail-closed 与 orphan lock-file regression RED，并补齐 child-mode helper plumbing。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`，用 sidecar `.lock` advisory lock 把 `load -> prune -> replay check -> append -> save` 包进单次跨进程可见 acquire；orphan `.lock` file 本身不构成失败条件。
- [x] 跑 focused regression、focused completeness gate、compile gate，并把 final diff hygiene / whitespace evidence 记录到 `progress.md`。

### Status
- 已完成。file-backed replay provider 现在会在 Unix/Linux 上对同一 replay-store file 做 bounded cross-process advisory locking；active contention 时 fail closed；orphan `.lock` file without active holder 不会误拒绝；public API、builder/factory/config surface 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_cross_process_lock_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`184/184`)
- final diff hygiene / whitespace evidence 见 `progress.md` 本批条目。

## 2026-04-13 Plan (Execution / FreePascal early-data replay-store orphan temp recovery hardening)

### Goal
- 在不改变默认 shipped behavior、public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay provider 增加最小 orphan `.tmp` replay-store recovery hardening。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-replay-store-temp-recovery-hardening.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读现有 replay seam、file-backed provider、focused tests 与 working-memory，确认当前最高 ROI 剩余缺口不是再做新 persistence abstraction，而是补上 `temp file + RenameFile` 中断后 orphan `.tmp` replay truth 不会被恢复的问题。
- [x] 新建 `docs/plans/2026-04-13-freepascal-early-data-replay-store-temp-recovery-hardening.md`，把范围锁定为 RED -> 最小 GREEN -> focused verification -> ledger closeout。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 增加 orphan temp replay truth / fail-closed corruption RED，锁住“main file 缺失、只有 `.tmp` 存在”场景下的恢复合同。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`，新增受限 recovery resolver，让 provider 在 main file 缺失时可读取 orphan `.tmp`，但不改变 canonical main-file write path。
- [x] 跑 focused regression、focused completeness gate、compile gate、diff hygiene，并回填 `task_plan.md` / `findings.md` / `progress.md`。

### Status
- 已完成。file-backed replay provider 现在会在 canonical main store 缺失且 orphan `.tmp` 存在时恢复 live replay truth；corrupt orphan temp store 仍 fail closed；public API、builder/factory/config surface 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_temp_recovery_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`184/184`)
- `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-replay-store-temp-recovery-hardening.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS（无输出）
- `sed -n '1,80p' task_plan.md | rg -n '[[:blank:]]+$'`、`sed -n '1,80p' findings.md | rg -n '[[:blank:]]+$'`、`sed -n '1,100p' progress.md | rg -n '[[:blank:]]+$'` => PASS（exit 1，表示本批新增顶部区块无尾随空白）

## 2026-04-13 Plan (Execution / FreePascal early-data replay-store factory parity and error contracts)

### Goal
- 在不改变默认行为、capability wording 与既有 backend-private replay seam 的前提下，把 FreePascal TLS 1.3 early-data replay-store file opt-in 从 builder 扩到 `TSSLConfig` / `TSSLFactory` 路径，并补齐 builder/factory 的 clear error contracts。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-replay-store-factory-parity-and-error-contracts.md`
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.debug.utils.pas`
- `tests/config/test_default_config.pas`
- `tests/test_factory_logic.pas`
- `tests/test_factory_config_early_data_isolation.pas`
- `tests/config/test_context_builder_try.pas`
- `docs/ROADMAP.md`
- `docs/INTEGRATION_GUIDE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 replay installer seam、builder error wording、factory/config early-data surfaces 与现有 roadmap truth，确认这批只做 config/factory parity 和 clear error contracts，不新增 provider/callback public abstraction。
- [x] 新建 2026-04-13 factory parity 计划文件，并把范围锁定在 RED -> 最小 GREEN -> docs/ledger closeout。
- [x] 在 `tests/config/test_default_config.pas`、`tests/test_factory_logic.pas`、`tests/test_factory_config_early_data_isolation.pas`、`tests/config/test_context_builder_try.pas` 先补 RED，锁住默认值、factory parity 与 builder negative-path contracts。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.pas`、`src/fafafa.ssl.debug.utils.pas`，builder 生产代码保持不动。
- [x] 跑 focused tests、`tests/test_freepascal_tls13_early_data.pas`、focused completeness gate、compile gate，并同步更新 roadmap / integration guide / working memory。

### Status
- 已完成。`ServerEarlyDataReplayStoreFile` 现在贯通 `TSSLConfig` / `TSSLFactory`，default shipped path 与 capability wording 保持不变，builder/factory negative-path contracts 与 fresh verification evidence 都已收口。

### Verification
- `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config` => PASS
- `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic` => PASS
- `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation` => PASS
- `mkdir -p tmp/test_context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_try -FEtmp/test_context_builder_try -otmp/test_context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try/test_context_builder_try` => PASS
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_factory_parity_20260413` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`184/184`)
- `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-replay-store-factory-parity-and-error-contracts.md src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.debug.utils.pas tests/config/test_default_config.pas tests/test_factory_logic.pas tests/test_factory_config_early_data_isolation.pas tests/config/test_context_builder_try.pas docs/ROADMAP.md docs/INTEGRATION_GUIDE.md task_plan.md findings.md progress.md` => PASS（tracked batch files）
- `sed -n '1,120p' task_plan.md | rg -n '[[:blank:]]+$'`, `sed -n '1,120p' findings.md | rg -n '[[:blank:]]+$'`, `sed -n '1,160p' progress.md | rg -n '[[:blank:]]+$'`, `sed -n '1,140p' docs/ROADMAP.md docs/INTEGRATION_GUIDE.md | rg -n '[[:blank:]]+$'` => PASS（本批新增顶部区块与 docs 无尾随空白）

## 2026-04-13 Plan (Execution / FreePascal early-data anti-replay builder opt-in)

### Goal
- 在不改变默认行为、public capability wording 与现有 replay seam 架构的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay 增加最小 public builder/config opt-in。

### Files
- `docs/plans/2026-04-13-freepascal-early-data-antireplay-builder-optin.md`
- `src/fafafa.ssl.context.builder.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `tests/config/test_config_import_export.pas`
- `tests/config/test_config_snapshot_clone.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 builder/config surfaces、existing OCSP file-based pattern、installer seam 与 runtime tests，确认这批只做 builder/config 薄接，不重开 provider/context 结构调整。
- [x] 新建 2026-04-13 builder opt-in 计划文件，并把 key 固定为 `server_early_data_replay_store_file`。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas`、`tests/config/test_config_import_export.pas`、`tests/config/test_config_snapshot_clone.pas` 先补 RED。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.context.builder.pas`，把 `BuildServer` 接到现有 backend-private installer seam。
- [x] 跑 focused tests、adjacent regressions、completeness gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。builder/config opt-in、runtime/config contracts、focused gate、compile gate 与 ledger evidence 都已收口；default behavior 和 capability wording 保持不变。

## 2026-04-12 Plan (Execution / FreePascal early-data anti-replay file installer lifecycle seam)

### Goal
- 为 FreePascal TLS 1.3 0-RTT / early-data 的 file-backed anti-replay path 增加一个更明确的 backend-private context installer seam，并用 fresh RED 锁住 install / reinstall / reset / session-cache-sync 生命周期，同时保持 public API、builder 与 capability wording 不变。

### Files
- `docs/plans/2026-04-12-freepascal-early-data-antireplay-file-installer-lifecycle-seam.md`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 installer/helper、context material private interfaces、replay-ledger access seam 与现有测试 helper，确认本轮最高 ROI 是 explicit context installer seam + lifecycle contracts，而不是 public builder 扩面。
- [x] 新建 2026-04-12 file-installer lifecycle seam 计划文件，并把本轮范围锁定在 RED -> explicit backend-private installer seam -> focused verification。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，覆盖 install / reinstall / reset / file-isolation / session-cache-sync contract，并把 cross-context runtime test 切到新 seam。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.context.material.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 与必要测试。
- [x] 跑 focused tests、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。backend-private context installer seam、install/reinstall/reset/file-isolation/session-cache-sync contracts、cross-context runtime rejection 与 verification evidence 都已收口；public API、builder、config import/export 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（缺少 `IFreePascalContextEarlyDataReplayInstaller`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_installer_lifecycle_seam_20260412` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- `git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-installer-lifecycle-seam.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-12 Plan (Execution / FreePascal early-data anti-replay file provider hardening + installer)

### Goal
- 为 FreePascal TLS 1.3 0-RTT / early-data 的 file-backed anti-replay provider prototype 补齐失败语义 hardening，并增加 backend-private 安装 helper，让真实 context 装配路径无需手工拼 provider + ledger，同时保持 public API、builder 与 capability wording 不变。

### Files
- `docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-hardening-installer.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读刚收口的 file-backed provider prototype、context replay-ledger seam、resumed accept path 与现有 backend-private file install patterns，确认最高 ROI 的下一步是失败语义 hardening + internal installer/helper，而不是 builder / public API 扩面。
- [x] 新建 2026-04-12 file-provider hardening + installer 计划文件，并把本轮范围锁定在 RED -> hardening/helper -> focused verification。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，覆盖 corrupt-store fail-closed、expired persisted-entry prune、helper-based install contract。
- [x] 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 与必要测试，补 hardening + backend-private installer/helper。
- [x] 跑 focused tests、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。file-backed provider corruption hardening、expired persisted-entry prune contract、backend-private installer/helper、helper-based cross-context replay rejection、focused gate、compile gate 与 diff hygiene 都已收口；public API、builder 与 capability wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（缺少 `InstallFileBackedReplayLedger`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_provider_hardening_installer_20260412` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- `git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-hardening-installer.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-12 Plan (Execution / FreePascal early-data anti-replay file provider prototype)

### Goal
- 为 FreePascal TLS 1.3 0-RTT / early-data 增加最小本地文件型 anti-replay provider prototype，让 replay truth 在 provider / ledger / context 重建后仍能持续存在，同时保持 public surface 与 current experimental wording 不变。

### Files
- `docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-prototype.md`
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读当前 replaceable seam、provider-backed ledger、resumed early-data accept path 和 repo 内现有文件读写模式，确认最高 ROI 的下一步是 internal file-backed provider prototype，而不是 builder / public API 扩面。
- [x] 新建 2026-04-12 file-backed anti-replay provider 计划文件，并把本轮范围锁定在 RED -> file provider prototype -> focused verification。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，覆盖 file-backed provider persistence 和 cross-context replay rejection。
- [x] 最小 GREEN：新增 internal file-backed provider 单元，并复用现有 provider-backed ledger / context seam。
- [x] 跑 focused tests、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。internal file-backed provider prototype、provider / ledger / context 重建后的 replay persistence、cross-context replay rejection、focused gate、compile gate 与 diff hygiene 都已收口；default in-memory path 与 experimental wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（`Fatal: Can't find unit fafafa.ssl.freepascal.earlydatareplay.fileprovider`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_provider_proto_20260412` => PASS
- `python3 scripts/compile_all_modules.py` => PASS（`184/184`）
- `git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-prototype.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-12 Plan (Execution / FreePascal early-data anti-replay provider persistent prototype)

### Goal
- 为 FreePascal TLS 1.3 0-RTT / early-data 的 anti-replay 增加最小 provider-backed prototype，让多个 context 可以通过共享 provider 协同拒绝 replay，同时保持 default in-memory path 与 experimental wording 不变。

### Files
- `docs/plans/2026-04-12-freepascal-early-data-antireplay-provider-persistent-prototype.md`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读当前 replay seam、resumption cache、early-data runtime tests 与现有 callback/provider patterns，确认最高 ROI 的最小下一步是 internal provider-backed prototype，而不是 public API / file persistence。
- [x] 新建 2026-04-12 provider-backed anti-replay 计划文件，并把本轮范围锁定在 RED -> provider prototype -> focused verification。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，覆盖 shared provider-backed ledger 和 cross-context replay coordination。
- [x] 最小 GREEN：在 internal replay 单元增加 narrow provider contract 与 provider-backed ledger prototype，并复用现有 context assembly path。
- [x] 跑 focused tests、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。provider-backed prototype、cross-context replay coordination、focused gate、compile gate 与 diff hygiene 都已收口；default in-memory path 与 experimental wording 保持不变。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（新增 provider-backed contract 先失败在缺少 `IFreePascalEarlyDataReplayProvider` / `TFreePascalCallbackEarlyDataReplayProvider` / `TFreePascalProviderBackedEarlyDataReplayLedger`）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_provider_proto_20260412` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`183/183`)
- `git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-provider-persistent-prototype.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-12 Plan (Execution / FreePascal early-data anti-replay replaceable / persistent seam)

### Goal
- 把 FreePascal TLS 1.3 0-RTT / early-data 的 anti-replay 从 context 内嵌内存 ledger 解耦成“默认内存实现 + 可替换实现”的抽象层，并把 seam 接到 resumed early-data accept path，同时锁住 experimental capability wording。

### Files
- `docs/plans/2026-04-12-freepascal-early-data-antireplay-replaceable-persistent-seam.md`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读当前 FreePascal early-data / anti-replay 内部链路、focused gate 与 capability wording，确认真实剩余缺口只在 replaceable / persistent seam。
- [x] 新建 2026-04-12 anti-replay seam 计划文件，并把本轮约束写清楚：先 RED、默认内存行为保持、wiring resumed accept path、不提前上调 capability wording。
- [x] 在 `tests/test_freepascal_tls13_early_data.pas` 先补 RED，覆盖 default ledger、replaceable seam、生存期/重放语义与 resumed accept-path wiring。
- [x] 最小 GREEN：新增 internal in-memory ledger 单元，给 context 增加 get/set/reset seam，并让 connection accept path 走 active ledger。
- [x] 跑 focused tests、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。replaceable anti-replay seam、默认 in-memory ledger 保序行为、resumed early-data accept-path wiring 与 verification evidence 都已收口；persistent prototype / wording 升级继续留到后续批次。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => RED first, FAIL（新增 seam contract 先失败在缺少 `IFreePascalEarlyDataReplayLedgerAccess` / setter-getter wiring）
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data` => GREEN / PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_replaceable_20260412` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`183/183`)
- `git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-replaceable-persistent-seam.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-12 Plan (Execution / Root roadmap truth alignment and early-data next wave queue)

### Goal
- 收口 `docs/ROADMAP.md` 的陈旧主线描述，让 root-level 入口重新对齐当前 CI / capability truth，并把 FreePascal early-data 的真实下一步收敛成 replaceable / persistent anti-replay coordination seam。

### Files
- `docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 root docs、GitHub Actions docs、`ci.yml` 和 FreePascal capability truth，确认 roadmap 中仍有 stale queue。
- [x] 新建 2026-04-12 批次计划文件，明确本轮只做 roadmap truth alignment + next-wave queue clarification。
- [x] 改写 `docs/ROADMAP.md`，移除“focused gate CI promotion / OCSP-CT-validation hardening 仍待推进”的旧口径。
- [x] 把当前 next queue 收紧为 experimental 0-RTT 的 replaceable / persistent anti-replay coordination seam。
- [x] 跑最终验证并把证据回写到 ledger。

### Status
- 已完成。root roadmap、next-wave queue 与 working-memory 已收口到当前真实状态，没有重开任何缺少 fresh RED 的实现线。

### Verification
- `rg -n '提升到更显眼的 CI 层|再推进 \`OCSP stapling / Certificate Transparency / validation hardening\`' docs/ROADMAP.md` => PASS（exit 1，无陈旧 queue 命中）
- `rg -n 'freepascal-tls13-completeness|single-process anti-replay ledger|replaceable / persistent anti-replay' docs/ROADMAP.md docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md task_plan.md findings.md progress.md` => PASS
- `rg -n '[[:blank:]]+$' docs/ROADMAP.md docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md` => PASS（exit 1）
- `sed -n '1,40p' task_plan.md findings.md progress.md | rg -n '[[:blank:]]+$'` => PASS（exit 1；只检查本轮新增 ledger 顶部区块，避免误改 `findings.md` 历史段落中的旧尾随空白）
- `rg -n '^## 2026-04-12 (Plan|Findings|Progress)' task_plan.md findings.md progress.md` => PASS

## 2026-04-11 Plan (Execution / FreePascal server-side OCSP stapling public closeout)

### Goal
- 把已落地的 FreePascal server-side OCSP stapling issuance 收口成 public optional API，并同步 capability truth、OCSP/API/roadmap 文档与 ledger。

### Files
- `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-public-closeout.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `docs/reference/API_DOCUMENTATION.md`
- `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md`
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `tests/test_transformation_methods.pas`
- `tests/config/test_config_import_export.pas`
- `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 closeout 计划，确认 scope 只收 public optional interface、builder file-based config、capability truth 与文档收口，不扩 online fetch / refresh / responder。
- [x] 先用 capability / builder / runtime contract 做 RED，锁定旧 truth 仍把 server-side stapling issuance 当作未完成。
- [x] 最小 GREEN：新增 `ISSLServerOCSPStaplingContext`、builder `WithServerOCSPStapledResponseFile(...)`，并让 FreePascal backend 复用既有 backend-private stapling seam。
- [x] 收紧 `KnownIssues` 到 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`，并把 server runtime test 改走 public interface + builder-driven path。
- [x] 同步 OCSP / API / roadmap / superseded-plan 文档到当前真相。
- [x] 跑 focused tests、focused gate、compile gate。
- [x] 跑最终 `git diff --check` 并确认收尾 hygiene。

### Status
- 已完成。public surface、builder config、capability truth、docs truth、ledger 与验证证据都已收口。

### Verification
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `mkdir -p tmp/test_transformation_methods && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_transformation_methods -FEtmp/test_transformation_methods -otmp/test_transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods/test_transformation_methods` => PASS
- `mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export` => PASS
- `mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime` => PASS
- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => PASS
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_server_stapling_public_closeout_20260411` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-public-closeout.md docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/reference/API_DOCUMENTATION.md src/fafafa.ssl.base.pas src/fafafa.ssl.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas tests/test_transformation_methods.pas tests/config/test_config_import_export.pas tests/test_freepascal_server_ocsp_stapling_runtime.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-11 Plan (Execution / FreePascal server-side OCSP stapling issuance)

### Goal
- 给 FreePascal TLS 1.3 server accept path 增加 bounded server-side OCSP stapling issuance，并把 focused runtime 纳入 completeness gate。

### Files
- `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-implementation.md`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.tls13.servercertificate.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读实施计划、gate 脚本和 ledger 约定，确认本轮只收 server-side stapling issuance + gate promotion，不扩 online fetch / cache policy。
- [x] 在 `tests/test_freepascal_server_ocsp_stapling_runtime.pas` 先做 RED，证明 server `DoAccept` 还不会在 requested + configured 时发 stapled OCSP response。
- [x] 最小 GREEN：新增 FreePascal backend 私有 server stapling material seam，并让 `TFreePascalConnection.DoAccept` 只在 full handshake + requested + configured 时发 stapled response。
- [x] 把 `tests/test_freepascal_server_ocsp_stapling_runtime.pas` 纳入 completeness gate，并收紧 gate contract 的 dry-run / fake-fpc / summary 断言。
- [x] 跑 focused runtime、gate contract、dry-run、full gate、compile gate，并回填 ledgers / diff hygiene。

### Status
- 已完成。FreePascal server 现在只会在 client 明确请求 `status_request` 且 context 预置 stapled OCSP DER 时，把 response 挂到 leaf `CertificateEntry`；focused completeness gate 已同步覆盖该 runtime。

### Verification
- `mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime` => PASS
- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => PASS
- `bash -n scripts/run_freepascal_tls13_completeness_gate.sh` => PASS
- `bash -n tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --dry-run --fast-local --run-id fp_server_stapling_dryrun_20260411` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_server_stapling_exec_20260411` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-implementation.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_server_ocsp_stapling_runtime.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md` => PASS


## 2026-04-11 Plan (Execution / FreePascal stapling truth and fast revocation regression)

### Goal
- 把 FreePascal validation closeout 再收紧一层：`KnownIssues` 只保留真实剩余边界，新增轻量 revocation fast contract，并把它前置进 completeness gate。

### Files
- `docs/plans/2026-04-11-freepascal-stapling-truth-and-fast-revocation-regression.md`
- `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md`
- `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `tests/test_freepascal_revocation_fast_contracts.pas`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 写本轮计划文件，并确认 scope 只收 truth alignment、fast regression 和 gate inventory，不扩新的 client-side revocation/runtime 行为。
- [x] 在 `tests/test_freepascal_backend_basic.pas` / `tests/test_capability_cache.pas` 收紧 `KnownIssues` 断言，先做 RED 证明 wording drift 仍然存在。
- [x] 新增 `tests/test_freepascal_revocation_fast_contracts.pas`，直接覆盖 `TX509CRL.Issuer.ToString` 与 `TSSLCertificateChainVerifier` 的 non-revoked / revoked / unavailable truth。
- [x] 收紧 `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues`，把 focused gate 前置加入 fast contract，并同步 OCSP / security 文档。
- [x] 新建 `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md`，把真实下一阶段单独立项，不混进本批。
- [x] 跑 focused tests、gate contract、full gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。FreePascal `KnownIssues` 已收紧到唯一真实剩余边界 `server-side OCSP stapling issuance`；轻量 revocation 快测已纳入 completeness gate 并在完整 gate 中先于重型 validation runtime tests 运行。

### Verification
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `mkdir -p tmp/freepascal_revocation_fast_contracts && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_revocation_fast_contracts -FEtmp/freepascal_revocation_fast_contracts -otmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts tests/test_freepascal_revocation_fast_contracts.pas && ./tmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts` => PASS
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_fast_revocation_exec_20260411` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)


## 2026-04-11 Plan (Execution / FreePascal revocation evidence material plumbing)

### Goal
- 给 FreePascal client runtime trust path 接入 caller-provided CRL material，让 `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 对 CRL-backed path 能返回 good / revoked / unavailable 的更细 truth。

### Files
- `docs/plans/2026-04-11-freepascal-revocation-evidence-material-plumbing.md`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.certchain.pas`
- `src/fafafa.ssl.crl.pas`
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- `tests/certificate/test_certs/revocation_revoked_crl.pem`
- `tests/certificate/test_certs/revocation_nonmatching_crl.pem`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 写本轮计划文件，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部加入口。
- [x] 在 `tests/test_freepascal_client_cert_verify_flags_runtime.pas` 先补 RED，证明 caller-provided CRL material 还没接到 runtime trust path。
- [x] 最小 GREEN：新增 FreePascal 私有 revocation material interface，把 CRL material 从 context 传到 connection / chain verifier。
- [x] 让 chain verifier 对 caller-provided CRL material 给出 good / revoked / unavailable truth，并把 revoked 映射成更准确的连接错误。
- [x] 修正 `src/fafafa.ssl.crl.pas` 的 `TX509CRL.ParseName(...)` DN 解析，恢复 CRL issuer 与证书 issuer 的可比较字符串。
- [x] 跑 focused runtime、adjacent trust runtime、focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。caller-provided CRL material 已接入 FreePascal client runtime trust path；最终补刀点是 `TX509CRL.ParseName(...)` 漏掉了 attribute short name，导致非 revoked CRL 也被误判成“无适用 CRL 材料”。

### Verification
- `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime` => PASS
- `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id revocation_material_exec_20260411` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)


## 2026-04-11 Plan (Execution / FreePascal validation closeout and focused gate)

### Goal
- 把已经完成的 FreePascal validation 下一波实现一次性做成真实 closeout：更新 capability truth、扩 focused gate、收 roadmap / ledger，并同步 OCSP / CT 对外文档。

### Files
- `docs/plans/2026-04-11-freepascal-validation-closeout-and-focused-gate.md`
- `src/fafafa.ssl.freepascal.lib.pas`
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/README.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 复核 `KnownIssues`、focused gate、roadmap、OCSP/CT 文档与现有实现，确认本轮 scope 只收 closeout，不再顺手扩 revocation / CRL material plumbing。
- [x] 写本轮 closeout 计划文件，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部加入口。
- [x] 在 `tests/test_freepascal_backend_basic.pas`、`tests/test_capability_cache.pas`、`tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 做 RED，证明 wording / gate inventory 仍有 drift。
- [x] 最小 GREEN：更新 `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues`，扩 `scripts/run_freepascal_tls13_completeness_gate.sh`，并同步 roadmap / docs。
- [x] 跑 focused tests、gate contract、gate dry-run、full focused gate、compile gate、diff hygiene，并回填 ledgers。

### Status
- 已完成。closeout truth、focused gate 覆盖面、roadmap 口径和 OCSP/CT 对外文档已经同步到当前实现状态。

### Verification
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic` => PASS
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache` => PASS
- `bash -n scripts/run_freepascal_tls13_completeness_gate.sh` => PASS
- `bash -n tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --dry-run --fast-local --run-id closeout_dryrun_20260411` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id closeout_exec_20260411` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-validation-closeout-and-focused-gate.md docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md docs/guides/OCSP_USAGE_GUIDE.md docs/guides/CT_IMPLEMENTATION_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md docs/README.md scripts/run_freepascal_tls13_completeness_gate.sh src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md` => PASS
- `rg -n '[[:blank:]]+$' docs/plans/2026-04-11-freepascal-validation-closeout-and-focused-gate.md docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS（exit 1，无尾随空白命中）

## 2026-04-11 Plan (Execution / FreePascal remaining cert verify flags runtime parity closeout)

### Goal
- 把 FreePascal client runtime 里剩余被静默忽略的 `sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 收成 bounded parity，并确保失败原因可见、fail-closed truth 明确。

### Files
- `docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.certchain.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 Batch 5 计划、focused test、trust path / chain verifier / `VerifyEx(...)` 调用链，确认 scope 只收 strict-chain、revocation、CRL。
- [x] 在 `tests/test_freepascal_client_cert_verify_flags_runtime.pas` 先补 RED，锁定 strict-chain / revocation / CRL 仍未形成 runtime truth。
- [x] 最小扩 `ValidateClientPeerCertificateTrust(...)`、`TSSLCertificateChainVerifier`、`TFreePascalCertificate.VerifyEx(...)`，让 flags 真正进入 runtime trust path，并把 unavailable truth surface 出来。
- [x] GREEN 过程中继续追根因，确认 strict-chain 没停在 EKU，而是被 `TFreePascalCertificate.RebuildInfo` 未回填 `FInfo.KeyUsage` 提前误判；补齐 key-usage bitfield 后移除临时 debug。
- [x] 跑 focused、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。Batch 5 现在不会再静默吞掉 `sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL`；strict-chain 会落到真实 key-usage / EKU gate，revocation / CRL 在当前 bounded architecture 下也会明确 fail-closed 为 unavailable truth。

### Verification
- `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime` => PASS
- `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime` => PASS
- `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime` => PASS
- `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface` => PASS
- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.certchain.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-11 Plan (Execution / FreePascal online OCSP broader hardening closeout)

### Goal
- 把 FreePascal client online OCSP path 从“只看 cert status 整数”收紧到对 responder / cryptographic verification failure 的真实 fail-closed truth，不再把 `good` status 和 `verified` 混为一谈。

### Files
- `docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md`
- `src/fafafa.ssl.openssl.api.ocsp.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 Batch 4 计划、runtime harness 与 OCSP helper，确认缺口集中在 online OCSP richer result / fail-closed truth，不再重做 fetch parity。
- [x] 在 `tests/test_freepascal_client_online_ocsp_runtime.pas` 先补 RED，覆盖 `good status` 但 cryptographic verify 失败、以及 responder verification failure 的 contract。
- [x] 最小扩 `src/fafafa.ssl.openssl.api.ocsp.pas`，新增 `TOCSPCheckFailureStage` / `TOCSPCheckResult` / `CheckCertificateStatusDetailed(...)`，把失败阶段与 `Verified` truth 保留下来。
- [x] 修改 `TFreePascalConnection.ValidateClientOnlineOCSP(...)`，让连接层消费 richer result，并把 cryptographic / responder verification failure 明确映射成 fail-closed 错误文本。
- [x] 跑 focused、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。Batch 4 现在不会再把 `good` cert status 误当成 `verified`；online OCSP cryptographic / responder verification failure 会明确 fail-closed，并 surface 可读原因。

### Verification
- `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime` => PASS
- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => PASS
- `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime` => PASS
- `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md src/fafafa.ssl.openssl.api.ocsp.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-11 Plan (Execution / FreePascal OCSP stapling cryptographic hardening)

### Goal
- 让 FreePascal client 只有在 stapled OCSP response 通过 cryptographic verification 时才 surface `Verified`；optional path 继续放行但不谎报 verified，required path 对这类失败 fail-closed。

### Files
- `docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md`
- `src/fafafa.ssl.ocsp.stapling.pas`
- `src/fafafa.ssl.openssl.api.ocsp.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 Batch 3 计划与 stapling / OCSP helper / runtime harness，确认缺口集中在 `ProcessStapledResponse(...)` 仍只看 parse / freshness / cert status。
- [x] 在 `tests/test_freepascal_client_ocsp_stapling_runtime.pas` 新增 focused RED，覆盖 `good status` 但没有 cryptographic proof 的 optional / required contract。
- [x] 最小扩展 `src/fafafa.ssl.openssl.api.ocsp.pas`，新增 raw response DER + leaf/issuer DER 的 bounded cryptographic verify helper。
- [x] 在 `src/fafafa.ssl.ocsp.stapling.pas` 中把 cryptographic verify 接到 `ossVerified` 之前，失败统一落成 verification failure。
- [x] 跑 focused GREEN、邻近 CT / online OCSP regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。Batch 3 现在不会再把只有 parse-level success 的 stapled OCSP response 表述成 `Verified`；optional / required path 对 cryptographic failure 也已经一致。

### Verification
- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => RED（修复前失败在 `Good-status stapled response without cryptographic proof must not be marked as verified`），修复后 PASS
- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md src/fafafa.ssl.ocsp.stapling.pas src/fafafa.ssl.openssl.api.ocsp.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS




## 2026-04-11 Plan (Execution / FreePascal OCSP-delivered CT source parity)

### Goal
- 让 FreePascal client 在 TLS SCT extension 与 embedded X.509 SCT 都缺失时，能从 stapled OCSP response 中提取、surface 并验证 SCT list，同时保持范围只收 source parity，不扩到 OCSP cryptographic hardening。

### Files
- `docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md`
- `src/fafafa.ssl.ocsp.pas`
- `src/fafafa.ssl.ct.sct.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重读 Batch 2 计划与 OCSP / CT 相关代码，确认缺口集中在 OCSP extension 解析、`ValidateFromOCSP(...)` 空实现，以及连接层没有把 `FOCSPResponse` 作为 SCT source。
- [x] 在 `tests/test_freepascal_client_ct_sct_surface.pas` 新增只依赖 OCSP-delivered SCT 的 focused RED，并确认当前实现不会 surface 该 source。
- [x] 最小扩展 `TOCSPResponse` / `TOCSPSingleResponse` 以解析 response-level 与 single-response-level SCT-bearing OCSP extensions。
- [x] 补齐 `TSCTValidator.ValidateFromOCSP(...)`，让它能从已解析 OCSP response 提取 SCT list 并复用现有 `ValidateSCTList(...)`。
- [x] 在 `TFreePascalConnection` 中仅在 TLS / embedded source 都为空时，才从 `FOCSPResponse` 提取并 surface OCSP-delivered SCT list。
- [x] 跑 focused GREEN、邻近 OCSP 回归、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。Batch 2 现在已经把 OCSP-delivered SCT source 接进 FreePascal client CT surface / validation pipeline，同时保持 stapled / online OCSP cryptographic hardening 仍留在后续批次。

### Verification
- `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface` => RED（修复前失败在 `OCSP-delivered SCT list should surface CT as enabled`），修复后 PASS
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md src/fafafa.ssl.ocsp.pas src/fafafa.ssl.ct.sct.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas task_plan.md findings.md progress.md` => PASS



## 2026-04-11 Plan (Execution / FreePascal CT issuer-source evidence hardening)

### Goal
- 用更强 runtime contract 真正确认 FreePascal client CT path 在 leaf-only server-chain + trust-store issuer 场景下吃到的是谁；若 fresh RED 证实 issuer source 错误，则最小复用 `TryResolvePeerIssuerCertificate(...)` 修复。

### Files
- `docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 复核 Batch 1 计划与 runtime 路径，确认 `RefreshCertificateTransparencyValidationState(...)` 仍是 `chain[1] else leaf`。
- [x] 在 `tests/test_freepascal_client_ct_sct_surface.pas` 加入 issuer-observation harness，并 fresh 跑 focused RED。
- [x] RED 证实 CT eval context 实际拿到的是 leaf issuer source 后，最小修改 `src/fafafa.ssl.freepascal.connection.pas`，让 CT path 复用 `TryResolvePeerIssuerCertificate(...)`。
- [x] 跑 focused GREEN、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。Batch 1 现在不再只是“availability 没坏”，而是有了能直接证明 issuer source 的 runtime evidence；下一步可以按路线图进入 Batch 2（OCSP-delivered CT source parity）。

### Verification
- `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface` => RED（修复前失败在 `CT validation should use the self-signed trust-store CA issuer, not the leaf`），修复后 PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas task_plan.md findings.md progress.md` => PASS


## 2026-04-11 Plan (FreePascal validation next-wave roadmap)

### Goal
- 把当前 FreePascal validation 剩余缺口一次性编排成可执行队列：一份总路线图 + 5 份分批实施计划，并同步更新 working memory，保证下一波执行能直接按批次落地。

### Files
- `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- `docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md`
- `docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md`
- `docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md`
- `docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md`
- `docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 复核当前 `KnownIssues`、关键代码符号、现有计划与 working memory，确认下一波不再是泛化的 “broader hardening”。
- [x] 把下一波优先级收敛为 5 个批次：
  - CT issuer-source 证据面补强
  - OCSP-delivered CT source parity
  - stapled OCSP cryptographic hardening
  - online OCSP broader hardening
  - remaining cert verify flags runtime parity
- [x] 写出一份总路线图与 5 份实施计划，每份都包含范围、TDD 顺序、验证命令与停止条件。
- [x] 更新 `task_plan.md` / `findings.md` / `progress.md`，把本轮定位为“下一波计划编排”，而不是直接实现。
- [ ] 后续按路线图从 Batch 1 开始逐批执行。

### Status
- 已完成。当前下一波的执行顺序、依赖关系与边界已经固定，不需要再重复大范围调研。后续直接按 `docs/plans/2026-04-11-*.md` 从 Batch 1 开始推进即可。

### Verification
- `git diff --check -- docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal CT trust-store issuer fallback)

### Goal
- 核对 FreePascal client `ISSLCertificateTransparencyValidation` 在 leaf-only server-chain + 本地 CAFile 场景下的真实行为；若 CT validation 会退化成 `Validation unavailable`，再最小收口 issuer fallback。最终结果证明当前 bounded surface 已可用，因此这批只补 runtime contract，不改生产代码。

### Files
- `docs/plans/2026-04-10-freepascal-ct-trust-store-issuer-fallback.md`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 CT validation issuer 来源与已有 online OCSP / trust verification helper 的分叉。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 在 `tests/test_freepascal_client_ct_sct_surface.pas` 加 leaf-only server-chain contract，并 fresh 运行验证。
- [x] focused contract 直接为绿，确认当前范围无需修改 `TFreePascalConnection` 生产代码。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。focused contract 证伪了“当前 CT validation 会在 leaf-only server-chain 下退化成 unavailable”这一假设；当前范围因此收口为 leaf-only CT validation contract hardening，不顺手扩 CT policy、required boundary、SCT source 或更大的证书验证面。

### Verification
- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- task_plan.md findings.md progress.md` => PASS
- `git diff --no-index --check -- /dev/null docs/plans/2026-04-10-freepascal-ct-trust-store-issuer-fallback.md` => clean output（`/dev/null` 比较返回 `1` 属预期）
- `git diff --no-index --check -- /dev/null tests/test_freepascal_client_ct_sct_surface.pas` => clean output（`/dev/null` 比较返回 `1` 属预期）

## 2026-04-10 Plan (FreePascal online OCSP trust-store issuer fallback)

### Goal
- 让 FreePascal client 在 `sslVerifyPeer + sslCertVerifyCheckOCSP` 的 online OCSP 路径上，即使服务端只发送 leaf 证书、issuer 只存在于本地 trust store，也能解析出正确 issuer 并继续完成 bounded fail-closed OCSP 校验。

### Files
- `docs/plans/2026-04-10-freepascal-online-ocsp-trust-store-issuer-fallback.md`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 online OCSP issuer 来源与 trust verification 的脱节点。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先在 `tests/test_freepascal_client_online_ocsp_runtime.pas` 加 leaf-only server-chain RED。
- [x] 最小修改 `TFreePascalConnection`，让 online OCSP issuer 解析优先 peer chain、其次 trust store。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在 FreePascal client online OCSP 的 issuer material fallback，没有顺手扩到 CT validation issuer fallback、OCSP capability wording，或更大的证书验证面。

### Verification
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-online-ocsp-trust-store-issuer-fallback.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal online OCSP wording alignment)

### Goal
- 对齐 FreePascal backend capability `KnownIssues` 与 OCSP 文档表述，去掉已经过期的 “online OCSP fetch parity / OCSP stapling validation hardening 未完成” 叙事，同时保留真实剩余边界。

### Files
- `docs/plans/2026-04-10-freepascal-online-ocsp-wording-alignment.md`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `docs/DOCUMENTATION_INDEX.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 FreePascal client online OCSP / stapling 已落地的 runtime truth 与当前 capability wording drift。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先收紧 capability wording tests，证明旧 `KnownIssues` 文案仍在。
- [x] 最小更新 `KnownIssues` 与相邻 OCSP 文档，不扩运行时实现。
- [x] 跑 focused verification、docs grep、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在 wording / documentation truth alignment，没有继续扩 FreePascal OCSP / CT 运行时能力，也没有提升 capability 等级。

### Verification
- `tests/test_freepascal_backend_basic.pas` => PASS
- `tests/test_capability_cache.pas` => PASS
- `rg -n -e "online AIA OCSP fetch parity" -e "online OCSP fetch parity" -e "OCSP stapling validation hardening" -e "OpenSSL 在线 OCSP" docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md src/fafafa.ssl.freepascal.lib.pas || true` => PASS（无输出，说明旧 wording 已消失）
- `rg -n -e "broader OCSP validation hardening" -e "OCSP-delivered Certificate Transparency source parity" -e "sslCertVerifyCheckOCSP" -e "FreePascal client runtime" docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md src/fafafa.ssl.freepascal.lib.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-online-ocsp-wording-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal client online OCSP fetch parity)

### Goal
- 让 FreePascal client 在 `sslVerifyPeer + sslCertVerifyCheckOCSP` 的 full-handshake 路径上真正执行 AIA online OCSP fetch，并通过 context HTTP hooks 走上层注入的 HTTP POST；当 responder 报 `revoked/unknown` 或在线验证不可用时 fail-closed。

### Files
- `docs/plans/2026-04-10-freepascal-client-online-ocsp-fetch-parity.md`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `sslCertVerifyCheckOCSP`、AIA URL 提取、HTTP hooks 与 FreePascal client verify 调用链的接线缺口。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 runtime RED，证明 FreePascal context 还不支持 HTTP hooks access，且 client path 还没有执行 online OCSP fetch。
- [x] 最小实现 context hook access + client online OCSP helper，并把 helper 接入 full-handshake verify path。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在 FreePascal client `sslCertVerifyCheckOCSP` 的 online fetch parity；没有扩到 responder signature / issuer-chain cryptographic parity、CT source parity 或 server-side 行为。

### Verification
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-client-online-ocsp-fetch-parity.md src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal client OCSP stapling validation hardening)

### Goal
- 收紧 FreePascal client 对 stapled OCSP `non-good` 证书状态的验证语义，确保 optional surface 不再把 `unknown/revoked` 之类的响应说成 `Verified`，且 `required` 模式会对这类响应 fail-closed。

### Files
- `docs/plans/2026-04-10-freepascal-client-ocsp-stapling-validation-hardening.md`
- `src/fafafa.ssl.ocsp.stapling.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 OCSP stapling verifier 对 `non-good` cert status 的状态映射与 requirement gate。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 runtime RED，证明 optional surface 仍会把 `unknown` 响应表述成 verified-ish，且 required path 会误放行。
- [x] 最小实现 `TOCSPStaplingClient` 对 `ocspGood` 之外状态的拒绝语义。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在 `non-good stapled response` 的校验一致性，没有扩到 online OCSP fetch、responder signature verification 或更大的证书验证面。

### Verification
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-client-ocsp-stapling-validation-hardening.md src/fafafa.ssl.ocsp.stapling.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal TLS 1.3 CertificateVerify focused gate promotion)

### Goal
- 把刚完成的 FreePascal TLS 1.3 `CertificateVerify` 主线正式提升进 focused completeness gate，让默认 CI/focused verification 不再遗漏这条默认协商路径。

### Files
- `docs/plans/2026-04-10-freepascal-tls13-certificateverify-focused-gate-promotion.md`
- `.github/README.md`
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 focused gate inventory 与最近刚落地的 CertificateVerify 主线测试。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 gate contract RED，证明 focused gate 仍未覆盖 CertificateVerify 主线。
- [x] 最小实现 gate inventory 与说明文本更新。
- [x] 跑 contract、真实 gate、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在“focused gate promotion”，没有改 `CertificateVerify` 生产代码，也没有顺手扩到更大的 validation hardening。

### Verification
- `bash -n scripts/run_freepascal_tls13_completeness_gate.sh` => PASS
- `bash -n tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id certverify_gate_promotion_20260410` => PASS (`10` passed / `0` failed)
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-tls13-certificateverify-focused-gate-promotion.md .github/README.md scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md` => PASS

## 2026-04-10 Plan (FreePascal CertificateVerify suite-aware RSA SHA384 negotiation)

### Goal
- 让 pure Pascal TLS 1.3 在默认协商路径里真正 advertize 并选中 RSA `*_SHA384` `CertificateVerify` schemes，使 `TLS_AES_256_GCM_SHA384` 不再依赖 forced test hook 才能走到 RSA SHA384。

### Files
- `docs/plans/2026-04-10-freepascal-certificateverify-suite-aware-rsa-sha384-negotiation.md`
- `src/fafafa.ssl.tls13.clienthello.pas`
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_tls13_clienthello_parser.pas`
- `tests/test_tls13_servercertverify.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对默认 ClientHello advertisement、当前 selector 与 runtime 默认协商路径。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 RED，证明默认 ClientHello 仍不广告 RSA SHA384，且 selector 不是 suite-aware。
- [x] 最小实现 ClientHello RSA SHA384 advertisement + suite-aware selector。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。当前范围保持在“RSA SHA384 advertisement + suite-aware selection”，没有扩到 `secp384r1`、Ed25519 或更大的 TLS 1.3 state machine。

### Verification
- `tests/test_tls13_clienthello_parser.pas` => PASS
- `tests/test_tls13_servercertverify.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-10-freepascal-certificateverify-suite-aware-rsa-sha384-negotiation.md src/fafafa.ssl.tls13.clienthello.pas src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_clienthello_parser.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal CertificateVerify RSA SHA384 schemes)

### Goal
- 补齐 pure Pascal TLS 1.3 `CertificateVerify` 对 RSA `*_SHA384` 签名方案的支持，让 `TLS_AES_256_GCM_SHA384` path 不再只停留在 transcript parity，而能完成真实的 RSA SHA384 选型、签名与验签。

### Files
- `docs/plans/2026-04-09-freepascal-certificateverify-rsa-sha384-schemes.md`
- `src/fafafa.ssl.tls13.wire.pas`
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `src/fafafa.ssl.freepascal.connection.pas`（已复核，最终无需改动）
- `tests/test_tls13_servercertverify.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 SHA384 gap 是否只在 transcript parity 之上继续缺少 RSA `*_SHA384` signature scheme 支持。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 unit/runtime RED，证明 RSA SHA384 `CertificateVerify` 仍被当作 unsupported。
- [x] 最小实现 `wire` 常量、RSA SHA384 signer/verify helper 与选择器支持。
- [x] 跑 focused verification、邻近 regressions、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。最终实现只扩 RSA `*_SHA384`，没有把 `secp384r1`、Ed25519 或更大的 signature family 一起拉进来；连接层也不需要额外状态机改动。

### Verification
- `tests/test_tls13_servercertverify.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-rsa-sha384-schemes.md src/fafafa.ssl.tls13.wire.pas src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal CertificateVerify SHA384 transcript parity)

### Goal
- 补齐 FreePascal TLS 1.3 `CertificateVerify` 在 `TLS_AES_256_GCM_SHA384` path 上的 transcript-input parity，确保 client verify 和 server-side signer 都不再把 48-byte transcript hash 误当成 SHA256-only 输入。

### Files
- `docs/plans/2026-04-09-freepascal-certificateverify-sha384-transcript-parity.md`
- `tests/test_tls13_servercertverify.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `ValidateServerCertificateVerify(...)`、shared builder、runtime tests 与 SHA384 suite 路径。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先补 shared-helper RED 与 runtime RED。
- [x] 跑 RED，确认失败形状。
- [x] 最小实现 shared builder / connection call-site parity。
- [x] 跑 focused verification、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。SHA384 suite 的 `CertificateVerify` transcript-input parity 已经补齐，helper / runtime / 邻近回归 / compile gate 都已收口。

### Verification
- `tests/test_tls13_servercertverify.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-sha384-transcript-parity.md src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (README OCSP/CT guide entrypoints)

### Goal
- 把 OCSP / CT 指南补进顶层 `README.md` 与 `docs/README.md` 的高可见度导航区，让用户从仓库首页也能直接发现这两条当前主路径。

### Files
- `docs/plans/2026-04-09-readme-ocsp-ct-guide-entrypoints.md`
- `README.md`
- `docs/README.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对顶层 README / docs README 是否缺少 OCSP / CT guide 入口。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 README / docs README 的导航条目。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。OCSP / CT guide 现在已经进入仓库首页和 docs 首页的高可见度导航区。

### Verification
- `rg -n "OCSP 指南|CT 指南|OCSP_USAGE_GUIDE|CT_IMPLEMENTATION_GUIDE|\\| OCSP \\|" README.md docs/README.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/README.md /home/dtamade/projects/fafafa.ssl/docs/README.md` => PASS
  - `README.md` => formatted
  - `docs/README.md` => formatted
- `git diff --check -- docs/plans/2026-04-09-readme-ocsp-ct-guide-entrypoints.md README.md docs/README.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (Documentation index CT guide entry)

### Goal
- 把 `docs/guides/CT_IMPLEMENTATION_GUIDE.md` 补进 `docs/DOCUMENTATION_INDEX.md`，让 CT guide 进入当前高可见度文档入口。

### Files
- `docs/plans/2026-04-09-documentation-index-ct-guide-entry.md`
- `docs/DOCUMENTATION_INDEX.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `DOCUMENTATION_INDEX.md` 的使用与集成入口是否缺少 CT guide。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 在相邻 guide 区补入 CT guide 条目。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。CT guide 现在已经进入 `DOCUMENTATION_INDEX.md` 的高可见度入口层。

### Verification
- `rg -n "CT_IMPLEMENTATION_GUIDE|CT 实现指南|OCSP 使用指南|TS 使用指南" docs/DOCUMENTATION_INDEX.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-documentation-index-ct-guide-entry.md docs/DOCUMENTATION_INDEX.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (API documentation resource link repair)

### Goal
- 修复 `docs/reference/API_DOCUMENTATION.md` 的“更多资源”断链，把它们改成当前仓库里实际存在的入口。

### Files
- `docs/plans/2026-04-09-api-documentation-resource-link-repair.md`
- `docs/reference/API_DOCUMENTATION.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `API_DOCUMENTATION.md` 资源区的实际断链与目标文件存在性。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新三条资源链接到现有路径。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`API_DOCUMENTATION.md` 的资源入口已经修正到真实存在的路径。

### Verification
- 固定字符串检查 => PASS
  - `../test_reports/P2_OCSP_MODULE_REPORT.md`
  - `../guides/CT_IMPLEMENTATION_GUIDE.md`
  - `../../examples/`
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-resource-link-repair.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (API documentation CT runtime boundary alignment)

### Goal
- 更新 `docs/reference/API_DOCUMENTATION.md` 的 CT section，把它从旧的通用 validator 叙事收紧到当前 FreePascal client/runtime CT surface 与 `required` boundary truth。

### Files
- `docs/plans/2026-04-09-api-documentation-ct-runtime-boundary-alignment.md`
- `docs/reference/API_DOCUMENTATION.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 API doc CT section、builder surface 与 fresh CT runtime truth。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 builder/CT section wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`API_DOCUMENTATION.md` 的 CT section 现在已经对齐当前 FreePascal client/runtime truth，没有扩到其他文档。

### Verification
- `rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|ISSLCertificateTransparencyValidation|verify-none|resumed|full-handshake|SCT list|validation 结果不可用|policy 不满足|TSCTValidator" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ct-runtime-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (OCSP doc entrypoint and migration wording alignment)

### Goal
- 收紧 `docs/DOCUMENTATION_INDEX.md` 与 `docs/MIGRATION_GUIDE_V1.1.md` 的 OCSP / capability wording，让入口描述与示例代码都对齐当前 FreePascal capability truth。

### Files
- `docs/plans/2026-04-09-ocsp-doc-entrypoint-and-migration-wording-alignment.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/MIGRATION_GUIDE_V1.1.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 OCSP guide entrypoint 描述、migration capability 示例与 FreePascal capability truth。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 documentation index 与 migration guide 的 drift wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。入口描述和 migration 示例现在都已经对齐当前 FreePascal capability truth，没有扩到更大的 docs family。

### Verification
- `rg -n "OCSP 使用指南|OpenSSL|FreePascal|OCSPStaplingSupport|RenegotiationSupport|deprecated" docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md /home/dtamade/projects/fafafa.ssl/docs/MIGRATION_GUIDE_V1.1.md` => PASS
  - `docs/DOCUMENTATION_INDEX.md` => `unchanged`
  - `docs/MIGRATION_GUIDE_V1.1.md` => formatted
- `git diff --check -- docs/plans/2026-04-09-ocsp-doc-entrypoint-and-migration-wording-alignment.md docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (API documentation OCSP required boundary alignment)

### Goal
- 更新 `docs/reference/API_DOCUMENTATION.md` 的 OCSP section，把 `required OCSP` 的 `verify-none` / resumed boundary 写实。

### Files
- `docs/plans/2026-04-09-api-documentation-ocsp-required-boundary-alignment.md`
- `docs/reference/API_DOCUMENTATION.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 API reference wording 与 fresh OCSP required boundaries。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 API reference 的 OCSP boundary wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`API_DOCUMENTATION.md` 现在已经写实了 `required OCSP` 的 `verify-none` / resumed boundary。

### Verification
- `rg -n "WithOCSPStaplingRequired|verify-none|resumed|fail-closed|stapled response|client/runtime path|required enforcement" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-required-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (Security best practices OCSP required boundary alignment)

### Goal
- 更新 `docs/guides/security-best-practices.md` 的 OCSP 建议，把 `required OCSP` 的 `verify-none` / resumed boundary 写实。

### Files
- `docs/plans/2026-04-09-security-best-practices-ocsp-required-boundary-alignment.md`
- `docs/guides/security-best-practices.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 security guide wording 与 fresh OCSP required boundaries。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 security guide 的 OCSP boundary wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`security-best-practices.md` 现在已经写实了 `required OCSP` 的 `verify-none` / resumed boundary。

### Verification
- `rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|verify-none|resumed|fail-closed|WithVerifyPeer" docs/guides/security-best-practices.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-required-boundary-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (OCSP usage guide required boundary alignment)

### Goal
- 更新 `docs/guides/OCSP_USAGE_GUIDE.md`，把 FreePascal client runtime `required OCSP` 的 `verify-none` / resumed 边界写实。

### Files
- `docs/plans/2026-04-09-ocsp-usage-guide-required-boundary-alignment.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对当前 OCSP guide wording、verify-none boundary、resumed boundary 与 fresh runtime tests。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 OCSP guide 的 required-mode 边界说明。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`OCSP_USAGE_GUIDE.md` 现在已经写实了 `required OCSP` 的 `verify-none` / resumed boundary，且没有扩到其他文档。

### Verification
- `rg -n "verify-none|resumed|WithOCSPStaplingRequired|ISSLOCSPStapling|fail-closed|full-handshake" docs/guides/OCSP_USAGE_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-ocsp-usage-guide-required-boundary-alignment.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client OCSP required verify-none boundary)

### Goal
- 把 `ssoRequireOCSPStapling` 在 `verify-none` client full-handshake 路径上的边界契约补齐，确保关闭 `sslVerifyPeer` 时不会被 required OCSP fail-closed 误伤。

### Files
- `docs/plans/2026-04-09-freepascal-client-ocsp-required-verifynone-boundary.md`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `ValidateClientOCSPStapling`、`ProbeServerHello(...)` 的 OCSP request wiring，以及现有 OCSP runtime harness。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写 RED，补 verify-none + required OCSP boundary contract。
- [x] 最小实现 verify-mode guard。
- [x] 跑 focused verification、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`verify-none + ssoRequireOCSPStapling` 现在不会再被 required-policy 误伤，同时保持了现有 OCSP request trigger 和 verify-peer/full-handshake 语义不变。

### Verification
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-required-verifynone-boundary.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client OCSP required resumption boundary)

### Goal
- 把 `ssoRequireOCSPStapling` 在 resumed TLS 1.3 client path 上的边界契约补齐并跑实，确保 resumed flight 缺少 certificate / stapled response 时不会被 required OCSP 误伤。

### Files
- `docs/plans/2026-04-09-freepascal-client-ocsp-required-resumption-boundary.md`
- `tests/test_freepascal_client_session_resumption.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 `ValidateClientOCSPStapling`、相邻 resumed guards 与现有 session resumption harness。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写 RED，补 resumed + required OCSP boundary contract。
- [x] 最小实现 resumed guard。
- [x] 跑 focused verification、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。`required OCSP` 现在不会误伤 resumed TLS 1.3 client path，范围仍锁定在 resumed boundary，没有扩到 verify-none 语义或更大的 OCSP 叙事。

### Verification
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-required-resumption-boundary.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (API documentation OCSP runtime alignment)

### Goal
- 更新 `docs/reference/API_DOCUMENTATION.md` 里的 OCSP 相关条目，让 API reference 与当前 runtime truth 对齐，去掉过头的 server-side 自动 stapling 表述。

### Files
- `docs/plans/2026-04-09-api-documentation-ocsp-runtime-alignment.md`
- `docs/reference/API_DOCUMENTATION.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对公开 OCSP surface、runtime tests 和当前 API reference 的落差，只写已有实现和测试证据。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 API reference 的 OCSP section、best practices 与 troubleshooting wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。API reference 里的 OCSP 相关条目现在对齐到了当前 runtime truth，并移除了过头的 server-side 自动语义。

### Verification
- `rg -n "ISSLOCSPStapling|WithOCSPStaplingRequired|backend-specific|自动获取 OCSP 响应|完整在线 revocation|GetOCSPResponseStatus" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-runtime-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (Security best practices OCSP runtime alignment)

### Goal
- 更新 `security-best-practices` 里的 OCSP 建议，让它与当前 FreePascal client runtime stapling truth 对齐，而不是只给一个裸 `.WithOCSPStapling` 示例。

### Files
- `docs/plans/2026-04-09-security-best-practices-ocsp-runtime-alignment.md`
- `docs/guides/security-best-practices.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 OCSP guide、runtime tests 和当前 security guide 的落差，只写已有实现和测试证据。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 security guide 的 OCSP section 与 checklist wording。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。security best practices 里的 OCSP 建议现在对齐到了当前 FreePascal runtime truth，并明确了 `required` 与范围边界。

### Verification
- `rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|stapled OCSP response|在线 revocation|WithVerifyPeer|required" docs/guides/security-best-practices.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md` => PASS
- `git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-runtime-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client OCSP guide runtime required)

### Goal
- 更新 OCSP guide，让文档覆盖当前真实可用的 FreePascal client runtime OCSP stapling surface 与 `required` 失败策略，而不是只停留在 OpenSSL 在线 OCSP 工作流。

### Files
- `docs/plans/2026-04-09-freepascal-client-ocsp-guide-runtime-required.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 OCSP public API、runtime tests 与当前 guide 的落差，只写已有实现和测试证据。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 guide，补 FreePascal client runtime section、`required` 失败策略和当前未覆盖范围。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。OCSP guide 现在覆盖了 FreePascal client runtime surface、`required` 失败策略和当前未覆盖范围。

### Verification
- `rg -n "FreePascal client runtime|WithOCSPStaplingRequired|ISSLOCSPStapling|status_request|online AIA OCSP fetch|OpenSSL 在线 OCSP" docs/guides/OCSP_USAGE_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-guide-runtime-required.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal capability OCSP truth alignment)

### Goal
- 对齐 FreePascal backend capability matrix 与当前 OCSP stapling runtime truth，避免 capability/wording 继续把 OCSP stapling 整体说成未完成。

### Files
- `docs/plans/2026-04-09-freepascal-capability-ocsp-truth-alignment.md`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 capability matrix、`IsFeatureSupported`、`KnownIssues` 与现有 OCSP runtime/support tests。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写 RED，收紧 OCSP capability truth 与 wording 契约。
- [x] 最小实现 capability truth + wording 对齐。
- [x] 跑 focused verification、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。FreePascal capability matrix 现在对齐到了当前 OCSP stapling runtime truth，但仍保持 experimental/有剩余缺口的表述。

### Verification
- `tests/test_freepascal_backend_basic.pas` => PASS
- `tests/test_capability_cache.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-capability-ocsp-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal capability CT truth alignment)

### Goal
- 对齐 FreePascal backend capability matrix 与当前 CT runtime truth，避免 capability/wording 继续把 CT 整体说成未完成。

### Files
- `docs/plans/2026-04-09-freepascal-capability-ct-truth-alignment.md`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 capability matrix、`IsFeatureSupported` 和 `KnownIssues` 当前 truth。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写 RED，收紧 CT capability truth 与 wording 契约。
- [x] 最小实现 capability truth + wording 对齐。
- [x] 跑 focused verification、compile gate 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。FreePascal capability matrix 现在对齐到了当前 CT runtime truth，但仍保持 experimental/有剩余缺口的表述。

### Verification
- `tests/test_freepascal_backend_basic.pas` => PASS
- `tests/test_capability_cache.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-capability-ct-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client CT guide runtime required)

### Goal
- 更新 CT guide，让文档覆盖当前真实可用的 FreePascal client CT surface 与 `CT required` 运行时边界，而不是只停留在底层 OpenSSL validator 用法。

### Files
- `docs/plans/2026-04-09-freepascal-client-ct-guide-runtime-required.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

### Steps
- [x] 重新核对 CT public API 与 runtime 边界，只写已有实现和测试证据。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 更新 CT guide，补 FreePascal client runtime section、`required` 配置方式和 guard 边界。
- [x] 跑 docs-focused verification 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。CT guide 现在覆盖了 FreePascal client runtime surface、`required` 边界和当前未覆盖范围。

### Verification
- `rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|verify-peer|resumed|validation unavailable|policy failed|embedded SCT" docs/guides/CT_IMPLEMENTATION_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/CT_IMPLEMENTATION_GUIDE.md` => PASS
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-guide-runtime-required.md docs/guides/CT_IMPLEMENTATION_GUIDE.md task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client CT required boundary contracts)

### Goal
- 把 `CT required` 在 `verify-none` 与 `session resumption` 两个显式 guard 上的 runtime contract 补齐，确认 required enforcement 不会越界到非 verify-peer 或 resumed client path。

### Files
- `docs/plans/2026-04-09-freepascal-client-ct-required-boundary-contracts.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `src/fafafa.ssl.freepascal.connection.pas`（仅当 focused RED 证明 guard 缺口存在时）

### Steps
- [x] 重新核对 `ValidateClientCertificateTransparency` 以及相邻 trust/flags gate，确认这批只做 verify-none / resumed 边界契约，不扩范围。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写两个 focused boundary tests，跑 RED 判断是新缺口还是已有行为。
- [x] 两个新契约都直接为绿，因此没有做生产修复，按 contract-only 收口。
- [x] 跑 focused verification、模块级编译门禁与 diff hygiene，并回填 ledgers。

### Status
- 已完成。两个新边界契约都直接证明当前 guard 正确，因此本批只补测试，不改生产代码。

### Verification
- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-required-boundary-contracts.md tests/test_freepascal_client_ct_sct_surface.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client CT required policy fail-closed)

### Goal
- 让 pure Pascal TLS 1.3 client 在已有 CT raw/validation surface 基础上，新增 `CT required` 配置位，并在 missing SCT、validation unavailable、policy failed 时 fail-closed。

### Files
- `docs/plans/2026-04-09-freepascal-client-ct-required-policy.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `tests/config/test_context_builder_try.pas`
- `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas`
- `tests/test_transformation_methods.pas`

### Steps
- [x] 重新核对 CT required 最小边界，确认这批只做 option + builder 接线 + client runtime gate，不扩到 OCSP-delivered SCT source 或正向 CT fixture。
- [x] 写 plan 与 working-memory planning 入口。
- [x] 先写 RED，补 runtime 与 builder 契约并确认当前树上缺少 CT required wiring。
- [x] 最小实现 `ssoRequireCertificateTransparency`、builder import/export/override/merge/clone 支持，以及 FreePascal client fail-closed gate。
- [x] 跑 focused tests、相邻 regressions、`python3 scripts/compile_all_modules.py` 与 diff hygiene，并回填 ledgers。

### Status
- 已完成。实现保持了“required-only option + bounded runtime enforcement”的边界，没有把 CT request trigger、OCSP-delivered SCT source 或正向 CT fixture 一起拉进来。

### Verification
- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/config/test_context_builder_try.pas` => PASS
- `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas` => PASS
- `tests/test_transformation_methods.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `tests/test_freepascal_tls13_early_data.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-required-policy.md src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas tests/config/test_context_builder_try.pas tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas tests/test_transformation_methods.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Plan (FreePascal client CT validation surface)

### Goal
- 让 pure Pascal TLS 1.3 client 在已有 SCT raw surface 之外，再暴露 bounded 的 CT cryptographic validation / policy status；validation 失败只做 surface，不改变握手放行逻辑。

### Files
- `docs/plans/2026-04-09-freepascal-client-ct-validation-surface.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`

### Steps
- [x] 重新核对当前 CT/FreePascal 实现边界，确认本批只做 validation result / policy surface，不引入新的 context policy 开关，也不改握手 fail-closed 语义。
- [x] 写 plan 与 working-memory 入口，锁定范围为 FreePascal client CT validation surface。
- [x] 先写 RED，证明连接层还没有 CT validation optional interface 或没有给出 validation result/policy status。
- [x] 最小实现只补新的 validation optional interface、base stub 与 FreePascal connection 对 OpenSSL CT validator 的 bounded bridge。
- [x] 跑 focused FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene，并回填 ledgers。

### Status
- 已完成。最终实现保留了“只 surface validation/policy truth，不改变握手放行”的边界。
- 实际执行时没有继续沿计划里的 `o2i_SCT_LIST + TSCTValidator.ValidateSCTList(...)` 落地；在拿到 `Access violation` 和 list decode failure 证据后，切到逐个 serialized SCT 做 `o2i_SCT` / `SCT_validate` / `SCT_get_validation_status` 的更稳路径。

### Verification
- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `tests/test_freepascal_tls13_early_data.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- src/fafafa.ssl.base.pas src/fafafa.ssl.connection.base.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas docs/plans/2026-04-09-freepascal-client-ct-validation-surface.md task_plan.md findings.md progress.md` => PASS

## 2026-04-19 Plan (FreePascal early-data directory-store public opt-in parity)

### Goal
- 在不改变默认 shipped behavior、capability wording、默认 in-memory single-process anti-replay truth 的前提下，把已经稳定的 FreePascal directory-backed replay store 暴露为最小 public opt-in：builder / `TSSLConfig` / `TSSLFactory` 都能配置 directory-backed early-data replay store，并保持 clear error contracts。

### Files
- `docs/plans/2026-04-19-freepascal-early-data-directory-store-public-optin-parity.md`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.debug.utils.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.pas`
- `tests/config/test_config_import_export.pas`
- `tests/config/test_config_snapshot_clone.pas`
- `tests/config/test_context_builder_try.pas`
- `tests/test_factory_logic.pas`
- `tests/test_factory_config_early_data_isolation.pas`
- `tests/test_freepascal_tls13_early_data.pas`

### Steps
- [x] 核对现有 directory-store 已经稳定存在于 backend-private 层，只补 public opt-in wiring，不重开 default path / capability wording / durability family。
- [x] 用 RED 锁定 config/import-export/clone/reset/merge、builder/factory 错误契约、以及真实 runtime wiring。
- [x] 最小实现 public directory path 字段、builder fluent method、backend-private directory installer seam，以及 builder/factory mutual-exclusion fail-fast。
- [x] 跑 focused tests、completeness gate、`python3 scripts/compile_all_modules.py` 与 diff hygiene。
- [x] 回填 working memory，准备 scoped commit。

### Status
- 已完成。directory-backed replay store 现在具备 builder / config / factory 的 public opt-in parity；默认行为仍保持 in-memory single-process anti-replay，capability wording 未改变。

### Verification
- `tests/config/test_config_import_export.pas` => PASS
- `tests/config/test_config_snapshot_clone.pas` => PASS
- `tests/config/test_context_builder_try.pas` => PASS
- `tests/test_factory_logic.pas` => PASS
- `tests/test_factory_config_early_data_isolation.pas` => PASS
- `tests/test_freepascal_tls13_early_data.pas` => PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_public_optin_parity_20260419` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`185/185`)
- `git diff --check -- docs/ROADMAP.md src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.debug.utils.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.pas tests/config/test_config_import_export.pas tests/config/test_config_snapshot_clone.pas tests/config/test_context_builder_try.pas tests/test_factory_logic.pas tests/test_factory_config_early_data_isolation.pas tests/test_freepascal_tls13_early_data.pas` => PASS
