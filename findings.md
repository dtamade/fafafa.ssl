# Findings

## 2026-04-29 Findings (Execution / Dirty worktree batched closeout)
- The current risk is not a single failing feature; it is a mixed dirty worktree with unrelated work sharing the same staging surface.
- The highest-value control is scoped batching:
  - OpenSSL/cert-utils fail-closed hardening
  - TLS 1.3 primitives
  - builder/config/SNI/backend-selection
  - Wave C script and historical-governance cleanup
  - final docs/API/capability truth
- FreePascal TLS 1.3 early-data is already closed out in the roadmap and should not be reopened without fresh failing evidence.
- Broad docs should be last, because they depend on the behavior batches being verified first.
- Fresh baseline is currently green on the dirty tree, so the backlog can be drained by scoped commits instead of first repairing a global build break.
- Default gate hardening is a good first implementation batch because it protects every later verification step:
  - `compile_all_modules.py` now requires 100% module compile success.
  - `run_minimal_ci_gate.sh` executes argv arrays instead of `eval`-built strings.
  - fast-local phase2 output is scoped under `tmp/`, keeping the dirty worktree stable during dry-runs.
- Batch 1 belongs together as cert-utils / certchain hardening:
  - `TCertificateUtils.VerifyChain(...)` is a generic chain helper with no CRL or EKU input channel, so it should use `DefaultChainVerifyOptions`.
  - `StrictChainVerifyOptions` remains meaningful through explicit verifier / runtime flag paths, where EKU and caller-provided CRL material can fail closed.
  - The previous `VerifyChain BIO` failure was not an OpenSSL BIO regression; it was the generic helper accidentally inheriting strict-chain expectations.
- Certchain revocation behavior should continue to distinguish revoked from unavailable CRL truth instead of warning and silently accepting when revocation checks are requested.
- Batch 2 OpenSSL root cause:
  - `LoadAESFunctions` / `LoadSHAFunctions` and same-pattern low-level loaders accepted `THandle`.
  - On Linux, `THandle` is 4 bytes while `TLibHandle` is 8 bytes, so passing a dynamic library handle could truncate it and make `GetProcAddress` access an invalid handle.
  - The correct fix is to expose/use a real OpenSSL dynamic-library handle type (`TOpenSSLLibHandle`) for these low-level load functions, not to special-case the failing test.
- OpenSSL capability truth should keep following loaded helper surface, not version constants alone; the current OpenSSL focused sweep exercises capability drift, loader readiness, DER private-key context loading, and connection/context/session BIO guard behavior.

## 2026-04-19 Findings (Execution / FreePascal completeness docs-history absorption batch)
- 当前最高 ROI 的第二批，不是继续碰 `src/`，而是把已经完成的 FreePascal completeness 主线对应的 docs truth 和 execution history 入库，减少后续继续/重扫的上下文成本。
- tracked docs 的安全边界已经足够明确：
  - 应纳入：focused gate / testing entrypoint / architecture truth / FreePascal integration / CT / OCSP guides
  - 应排除：`README.md`、`docs/README.md`、`docs/DOCUMENTATION_INDEX.md`、`docs/reference/API_REFERENCE.md` 这类广域刷新
- untracked `docs/plans/` 中，真正值得这批吸收的是 FreePascal completeness 历史链，而不是整个仓库所有未跟踪计划文件：
  - foundation / resumption / early-data / validation / OCSP / CT / directory-store hardening
  - 不含 Wave C、OpenSSL BIO / symbol、PKCS11、通用 docs 导航重排
- `docs/ROADMAP.md` 已经把其中一部分 plans 当作 live links；如果这些计划文件继续留在未跟踪状态，repo 内部 truth-source 会保持“文档引用存在但文件不在版本库”的坏状态。
- 因为这批是 docs/history absorption，最关键的验证不是新增运行时行为，而是：
  - roadmap/contracts 仍然 green
  - compile / minimal gate / focused gate 继续 green
  - scoped diff hygiene 无格式问题
- 这批 fresh verification 暴露了两个纯 hygiene drift，而不是新的实现缺口：
  - `docs/ROADMAP.md` 的 `approval_gate` 文案写成了 `governance work`，contract 要求的是 `mainline work`
  - 7 份历史 plan 文件有 `new blank line at EOF`
- 这两个问题都通过最小文档修正解决，没有牵出新的 runtime / API / gate 行为变更。

## 2026-04-19 Findings (Review / FreePascal docs batch candidate triage after 5bf7be2)
- 本次任务的关键不是“哪些文档看起来提到了 early-data”，而是“哪些 tracked modified docs 仍然直接追随 `5bf7be2` 那条 FreePascal completeness 主线的 truth source”。
- 初步边界已经明确：
  - 可疑相关项优先看 `docs/ROADMAP.md`、`docs/plans/*freepascal*`、以及 working-memory 文件
  - `README.md`、`docs/reference/API_REFERENCE.md` 这类广域刷新默认排除，除非 diff 明确只是在补 mainline 所需的单点真相
  - `Wave C`、OpenSSL symbol/BIO、仓库级 canonicalization 一律视为噪音主题
- 这轮 triage 的产物应该是“可提交 docs batch 清单 + 排除清单 + verification 要求”，而不是再扩写或修正文档本身。
- 高置信度、可单独成批的 tracked docs 候选集中在三类：
  - focused gate / verification 入口：`.github/README.md`、`.github/GITHUB_ACTIONS_GUIDE.md`、`docs/testing/TESTING_README.md`
  - FreePascal mainline 定位：`docs/ARCHITECTURE.md`、`docs/reference/ARCHITECTURE.md`
  - focused capability guides：`docs/INTEGRATION_GUIDE.md`、`docs/guides/OCSP_USAGE_GUIDE.md`、`docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- 需要排除的 tracked docs 主要是两类：
  - 广域入口/导航刷新：`README.md`、`docs/README.md`、`docs/DOCUMENTATION_INDEX.md`
  - 非 mainline 或主题漂移：`docs/reference/API_REFERENCE.md`、`docs/reference/API_DOCUMENTATION.md`、`docs/AGENTS.md`、`docs/DEVELOPMENT_ROADMAP_2026.md`

## 2026-04-19 Findings (Execution / FreePascal completeness code-test-gate absorption batch)
- 当前仓库最大的风险不是 FreePascal 主线本身不稳，而是多条主题叠在同一个超脏 worktree 里；因此最高 ROI 不是继续扩实现，而是先把 FreePascal completeness 主线摘成独立提交边界。
- 这条批次的真实代码中心是：
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.freepascal.session.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay*.pas`
  - `scripts/run_freepascal_tls13_completeness_gate.sh`
  - 与之配套的一组 FreePascal focused tests
- fresh verification 证明这批 worktree 已经形成自洽主线：
  - completeness gate 通过，覆盖 resumption / CT / OCSP / early-data / capability wording
  - `compile_all_modules.py` 通过，说明新增单元已被仓内依赖正确消费
  - builder / connector / server-name / certificate-flight 这些不在 gate 内的 focused contracts 也通过
- 因为 `src/fafafa.ssl.freepascal.connection.pas` 同时承载 session resumption、peer certificate surface、OCSP、CT、early-data 等多条能力线，继续硬拆成更细代码提交的冲突成本很高；先按“FreePascal completeness 主线”整体提交，比尝试把 connection 文件按子特性拆开更稳妥。
- README / `docs/reference/API_REFERENCE.md` 当前 diff 已经超出 early-data / completeness 主线本身，更像广域文档刷新；这批不应混入代码提交。
- 大量 `docs/plans/*freepascal*` 未跟踪文件属于过程沉淀，价值主要在审计与追溯；建议作为后续单独 docs 批次处理，不与首个代码批次混提。

## 2026-04-19 Findings (Execution / FreePascal early-data mainline closeout verification sweep)
- 当前 authoritative truth 已经一致：`docs/ROADMAP.md`、`README.md`、`docs/reference/API_REFERENCE.md`、public wiring 与 focused contracts 都继续表达同一件事：
  - default shipped path 仍然是 `in-memory single-process anti-replay ledger`
  - public opt-in 继续是 file-backed / directory-backed 两条路径
  - `file` / `directory` 继续 mutually exclusive
- 这轮 fresh verification sweep 最重要的发现不是新的实现缺口，而是“当前真的没有 fresh RED 需要修”。docs contract、factory/config isolation、runtime early-data focused suite、capability wording alignment 都直接 GREEN。
- 这说明当前最高 ROI 的动作不是再补新的 persistence shape，也不是无 RED 重开 directory-store family，而是把 early-data 主线正式收口成“仅在 fresh failing evidence 出现时再重开”。
- 当前批次也再次证明 capability wording 维持 `experimental` 是正确的：public opt-in 已经存在，但 default shipped path 仍没有升级成 durable-by-default，更不意味着 distributed readiness。
- completeness gate 也 fresh GREEN 之后，可以明确把这批定性为 verification-only closeout，而不是新的功能批次；不需要新的 `src/` 生产改动。
- 当前真正需要保留的 execution discipline 是：
  - 继续以 docs contract + focused suites + completeness gate 作为早期预警
  - 继续把 next queue 锁成“只有 fresh RED 才重开”
  - 不因“想更完整”而主动制造新的 persistence 方案或 family churn

## 2026-04-19 Findings (Execution / FreePascal early-data public opt-in docs closeout)
- public opt-in parity 已经 landed 之后，当前最高 ROI 的收口不是再碰 persistence concrete shape 或 capability wording，而是把 README / API reference 补到与现有 public surface 完全对齐。
- 这批 fresh RED 指向的是纯文档漂移，不是实现缺口：`README.md` 当时没有明确提到 `TSSLConfig.ServerEarlyDataReplayStoreFile`，因此 docs contract 首次失败。
- 当前最需要锁住的 public truth 只有三组，而且都已经通过 docs closeout 明确写死：
  - default FreePascal early-data path 仍是 `in-memory single-process anti-replay ledger`
  - public opt-ins 是 `TSSLConfig.ServerEarlyDataReplayStoreFile` / `TSSLConfig.ServerEarlyDataReplayStoreDirectory` 与对应 builder helpers `WithServerEarlyDataReplayStoreFile(...)` / `WithServerEarlyDataReplayStoreDirectory(...)`
  - `file` 与 `directory` 配置 mutually exclusive，不做 silent precedence
- 新增的 `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh` 给这批 closeout 一个很便宜但高收益的防回退护栏：以后 README / API reference 只要丢失关键 public 名称或互斥措辞，就会直接红。
- 这批 focused public-opt-in suite 与 completeness gate 都继续绿，说明当前 batch 的正确边界就是 docs + contract closeout；没有任何 fresh evidence 需要重开 `src/`、runtime wiring 或更重的持久化方案讨论。
- capability wording 保持不变仍然正确：public opt-in 已经存在，但默认 shipped path 仍不是 durable-by-default。

## 2026-04-19 Findings (Execution / FreePascal early-data directory store deterministic rename-denial closeout)
- 在上一批已经把 `RenamePathAt(...)` seam 补进 `TFreePascalDirectoryEarlyDataReplayStore` 之后，当前 directory-store line 剩余最高 ROI 的轻量收口就是把 deterministic rename-denial 子族锁进 direct/runtime contracts，而不是继续猜测是否还需要新的生产修法。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 4 组语义直接锁住了：
  - direct provider path：
    - `TestDirectoryReplayStoreFailsClosedOnDeterministicTempPromotionRenameDeniedAndRecovers`
    - `TestDirectoryReplayStoreFailsClosedOnDeterministicBackupPromotionRenameDeniedAndRecovers`
  - real runtime path：
    - `TestDirectoryReplayStoreFailsClosedOnDeterministicTempPromotionRenameDeniedAtRuntime`
    - `TestDirectoryReplayStoreFailsClosedOnDeterministicBackupPromotionRenameDeniedAtRuntime`
- 这 4 条合同一起明确了当前 `SaveEntries(...)` 的稳定边界：
  - 当 canonical `main` 缺失、`tempdir -> main` deterministic denied 时，provider/runtime 都会 fail closed；canonical `main` 不会被误 materialize，`.tmpdir` 会被清理，`.bakdir` 不会被误创建
  - 当 canonical `main` 已存在、`main -> .bakdir` deterministic denied 时，provider/runtime 都会 fail closed，同时 preserving canonical `main` replay truth；`.tmpdir` 会被清理，`.bakdir` 不会残留
  - 两条 family 在 rebuild 后都继续恢复 expected truth：blocked session first accept，后续 replay reject
- 这批 fresh focused run 直接 GREEN，没有出现新的 production RED；这说明上一批补进来的 `RenamePathAt(...)` seam 已经足够表达 rename-denial contract，本批不需要继续改 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。
- 结论是，这批最值钱的交付是 tests-only closeout，而不是再引入新的内部 branch logic；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确。
- next queue 继续收紧到“仅在 fresh failing evidence 出现时再重开更深的 crash-window / write-interruption drift”；不建议无 RED 重开现有 directory-store family。

## 2026-04-19 Findings (Execution / FreePascal early-data directory store backup-assisted replace and restore failure)
- 这批 fresh evidence 说明在 directory-store family 已经大面积收口后，真正还值得开的更深一层 write-interruption queue，是 update-path 上 `main -> .bakdir` 已成功之后的两条分支：
  - `tempdir -> main` 失败但 `.bakdir -> main` restore 成功
  - `tempdir -> main` 失败且 `.bakdir -> main` restore 也失败
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 4 组语义直接锁住了：
  - direct provider path：
    - `TestDirectoryReplayStorePreservesExistingTruthAcrossBackupAssistedReplaceFailure`
    - `TestDirectoryReplayStoreRecoversReplayTruthFromBackupAfterRestoreFailure`
  - real runtime path：
    - `TestDirectoryReplayStorePreservesExistingTruthAcrossBackupAssistedReplaceFailureAtRuntime`
    - `TestDirectoryReplayStoreRecoversReplayTruthFromBackupAfterRestoreFailureAtRuntime`
- 这 4 条合同一起明确了当前 directory-store update-path 的正确边界：
  - 当 `tempdir -> main` 失败但 restore 成功时，canonical `main` replay truth 继续存在，`.tmpdir` cleaned，`.bakdir` 不残留
  - original replay truth 立即继续 reject，blocked session 在 rebuild 后 first accept，后续 replay 继续 reject
  - 当 restore 也失败时，canonical `main` 会继续缺失，但 `.bakdir` replay truth 仍保留
  - original replay truth 会继续通过 `.bakdir` fallback reject；后续 rebuild 能消费 `.bakdir`，恢复 canonical `main`，并 accept blocked session
- 这批 TDD 的 first RED 没有继续打到 `SaveEntries(...)` 逻辑本身，而是暴露了一个更小的测试表达缺口：
  - directory-store 之前没有 rename seam，tests 无法像 file-backed family 那样脚本化 update-path rename failure
  - 最小 GREEN 只需要新增内部 `RenamePathAt(...)`
  - focused suite 在这一步之后直接恢复 GREEN，没有继续要求新的行为修法
- 结论是，这批唯一值得接受的 production change 只是内部 rename seam，而不是新的 branch logic；因此 capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确。
- 这批也验证了 roadmap 上“directory-store family 只在 fresh RED 出现时重开”的策略是对的：fresh RED 确实存在，但它是更深的 write-interruption contract gap，而不是回头重开已绿的 blocker/corruption/residue 家族。

## 2026-04-19 Findings (Execution / FreePascal early-data directory store `.bakdir` residue semantics)
- 这批 fresh evidence 说明 directory-store 里仍值得锁的高 ROI 小尾差，不是再改 `SaveEntries(...)` 语义，而是把 `.bakdir` cleanup-failure residue / stale `.bakdir` undeletable next-save fail-closed 这组 direct/runtime truth 明确写进 focused contracts。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 2 组语义直接锁住了：
  - direct provider path：`TestDirectoryReplayStoreLeavesBackupResidueAfterCleanupFailureAndFailsClosedOnUndeletableStaleBackup`
  - real runtime path：`TestDirectoryReplayStoreLeavesBackupResidueAfterCleanupFailureAndFailsClosedOnUndeletableStaleBackupAtRuntime`
- 这两条合同一起明确了当前 directory-store 的正确边界：
  - canonical replace 成功后，即使 `.bakdir` cleanup delete 失败，fresh replay truth 仍然落在 canonical `main`
  - `.tmpdir` 会继续被清理，stale `.bakdir` residue 会保持可见
  - original replay truth 与刚 accept 的 replay truth 都会立即继续 reject，证明 residue 不会遮蔽 canonical `main`
  - 下一次 fresh save 遇到无法预删的 stale `.bakdir` 时，provider/runtime 都继续 fail closed，并保持 canonical `main` truth 不变
  - 用正常 store / runtime rebuild 后，stale `.bakdir` 会被正常消费，blocked session 恢复 first accept，随后 replay 继续 reject
- 这批 TDD 的 fresh RED 没有落到 `SaveEntries(...)` 逻辑本身，而是暴露了一个更小的测试表达缺口：
  - directory-store 的 cleanup helper 之前是 `private`，现有 tests 无法像 file-backed family 那样脚本化 cleanup delete failure
  - 最小 GREEN 只需要把 `RemovePathTree(...)` 调整为 `protected virtual`
  - focused suite 在这一步之后直接恢复 GREEN，没有继续要求新的生产逻辑改动
- 结论是，这批唯一值得接受的 production change 只是内部测试 seam，而不是行为修法；因此 capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确。
- directory-store family 的默认 next queue 继续保持收缩状态：没有 fresh RED 时，不建议再无边界扩写 `.bakdir` / crash-window 细支线。

## 2026-04-16 Findings (Execution / FreePascal early-data directory store runtime crash-update restart)
- 这批 fresh evidence 说明 directory-store 剩余更像真实 accepted update-path crash-window 的队列里，当前最高 ROI 的缺口不是新的 `src/` 生产修法，而是 focused runtime contract：existing replay truth 已先 materialize 后，再让 child 进程 accept blocked session 并在 accept 后立刻 crash，确认 restart 之后两份 replay truth 都不会漂移。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 runtime 语义直接锁住了：
  - `TestDirectoryReplayStoreRetainsExistingAndAcceptedReplayTruthAcrossCrashWindowRestart`
- 这条合同和最小 harness 扩展一起明确了当前实现的正确边界：
  - `RunReplayProviderRuntimeCrashAcceptMode(...)` 现在支持 `directory_store` context path，并复用统一的 runtime context builder / canonical store-state check
  - existing replay truth 先已存在时，child accept fresh blocked session 后的 simulated crash 不会让 canonical directory replay truth 消失
  - restart 后 original replay truth 继续 reject，just-accepted blocked session 也继续 reject
  - replay-probe child 会记录 `directory_store` context path，说明 runtime seam 走到的确是 directory-backed store，而不是 installer/file-backed fallback
- 这批 TDD 的 fresh RED 也说明缺口只在 tests：
  - 第一轮 RED 是 crash helper 调用了后置 helper 但缺 forward declarations
  - 第二轮 RED 是新合同误把 simulated crash 语义锁成固定 exit code；现有 runtime crash-window family 的稳定合同其实是 `ready` 已出现、`graceful` 不应出现
  - 两轮都没有把失败落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- focused、adjacent、completeness 和 compile-all 全绿后，可以确认当前 `dirstore` 已天然满足这批 runtime crash-update restart truth；更深的 crash-window / write-interruption drift 仍只建议在 fresh failing evidence 出现时再重开。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in directory-store durability evidence，不是默认 shipped path 升级。

## 2026-04-16 Findings (Execution / FreePascal early-data directory store dual fallback conflict)
- 这批 fresh evidence 说明当前最高 ROI 的 remaining queue 里，更像真实 crash-window / write-interruption 现场的组合态，首先值得锁住的是 `main` 缺失、`corrupt .tmpdir + healthy .bakdir` 同时存在时的 fail-closed precedence，而不是再去扩 blocker 边角或提前改 `dirstore` 生产逻辑。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 2 组语义直接锁住了：
  - direct provider path：`TestDirectoryReplayStoreFailsClosedWhenCorruptTempFallbackShadowsHealthyBackupFallback`
  - real runtime path：`TestDirectoryReplayStoreFailsClosedWhenCorruptTempFallbackShadowsHealthyBackupFallbackAtRuntime`
- 这两条合同一起明确了当前实现的正确边界：
  - `ResolveReadableDirectoryName(...)` 的 `main > .tmpdir > .bakdir` precedence 在 preferred `.tmpdir` 已损坏时仍然坚持 fail closed，而不是回退到健康 `.bakdir` 自愈
  - fresh blocked session 会继续 reject，因此不会把 healthy `.bakdir` 当作隐式 accept + rewrite 入口
  - original replay truth 也继续 reject，canonical `main` 会继续保持缺失
  - `.tmpdir` / `.bakdir` 两个 artifact 都继续保留，直到坏 `.tmpdir` 被显式移除
  - 坏 `.tmpdir` 移除后，healthy `.bakdir` replay truth 才恢复可消费，并重新 materialize canonical `main`
- 这批 focused suite 直接 GREEN，没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此当前 `dirstore` 不需要新的 production 修法；真正剩余的实现队列仍然只建议在 fresh failing evidence 出现时再去重开更深的 crash-window / write-interruption drift。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in directory-store durability evidence，不是默认 shipped path 升级。

## 2026-04-16 Findings (Execution / FreePascal early-data directory store blocker edge closeout)
- 这批 fresh evidence 说明 blocker family 里最后一个还没被显式命名锁住的小尾差，是 `.bakdir` regular-file blocker 在 first acquire 上的 direct/runtime 语义；当前实现已经天然满足这条边界，缺的是 focused contracts，而不是新的生产修法。
- 这轮 tests-first closeout 把现有 blocker family 扩到了更完整的 direct/runtime truth：
  - canonical `main` regular-file blocker
  - `.tmpdir` regular-file blocker
  - `.bakdir` regular-file blocker
  - `.tmpdir` / `.bakdir` regular-file blocker 在 update path 上 preserve existing replay truth
- fresh GREEN 明确证明：
  - `SaveEntries(...)` 在 canonical `main` 缺失、`.bakdir` 被 wrong-shape object 占住时也继续 fail closed
  - blocker file 不会被 silent delete，canonical `main` / `.tmpdir` 不会被误 materialize
  - blocker 移除后，同一 session 才恢复 accept，rebuild 后 replay truth 继续 reject
- 这批没有 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，因此 directory-store blocker queue 现在可以认为已经收口；后续只有在 fresh failing evidence 出现时，才值得重开更深的 crash-window / write-interruption drift。

## 2026-04-16 Findings (Execution / FreePascal early-data directory store filesystem blocker semantics)
- 这批 fresh evidence 说明 directory-store 剩余最高 ROI 的 queue 里，真正需要 production 修法的不是 read path，而是 `SaveEntries(...)` 的 write-path asymmetry：`ResolveReadableDirectoryName(...)` 早已对 wrong-shape `main` / `.tmpdir` / `.bakdir` 继续 fail closed，但 `SaveEntries(...)` 之前会把 existing `.tmpdir` / `.bakdir` path 先删掉，导致 update path 对 regular-file blocker 出现 silent self-heal。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 2 组语义直接锁住了：
  - direct provider path：`TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAndRecovers`
  - real runtime path：`TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAtRuntime`
- 这两条合同一起明确了 4 个关键边界：
  - regular-file canonical `main` blocker 在 direct/runtime first acquire 上继续 fail closed
  - regular-file `.tmpdir` blocker 在 direct/runtime first acquire 上继续 fail closed
  - regular-file `.tmpdir` update blocker 不会再被 silent delete；fresh blocked session fail closed，original replay truth 继续保留
  - regular-file `.bakdir` update blocker 也不会再被 silent delete；fresh blocked session fail closed，original replay truth 继续保留
- fresh RED 准确落在真实生产缺口，而不是 tests-only drift：
  - focused suite first RED：`❌ Regular-file directory replay-store .tmpdir blocker should fail closed while updating existing replay truth`
  - 这直接暴露 `SaveEntries(...)` 预删 wrong-shape `.tmpdir` 的问题
- 最小 GREEN 只需要收紧 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 的一个点：
  - `.tmpdir` / `.bakdir` path 若存在但不是目录，直接 `Exit(False)`
  - 不再隐式 `RemovePathTree(...)` 后继续写入
  - 其他 public wiring、`TFreePascalContext`、`TFreePascalConnection`、builder / factory / config 全都不用动
- fresh GREEN 现在共同证明：
  - wrong-shape blocker artifact 会继续保留，直到显式移除
  - existing replay truth 在 update blocker 失败后不会丢
  - blocker 移除后，同一 blocked session 才恢复 accept
  - direct provider rebuild 与 runtime rebuild 之后，original / recovered replay truth 都继续 reject
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；这批只是继续加固 opt-in directory-store durability 边界，不是默认 shipped path 升级。
- next queue 因此进一步收紧到单一残余线：
  - 更深的 crash-window / write-interruption drift，仅在 fresh RED 出现时再重开；不建议现在无新证据继续扩 directory-store family

## 2026-04-16 Findings (Execution / FreePascal early-data directory store fallback corruption hardening)
- 这批 fresh evidence 说明当前最高 ROI 的 remaining queue 里，`.tmpdir` / `.bakdir` fallback corruption family 真正缺的是 focused contracts，而不是新的 `dirstore` 生产修法：现有 `ResolveReadableDirectoryName(...) -> LoadEntriesFromDirectory(...) -> TryLoadEntry(...)` 路径已经天然对 corrupt fallback 继续 fail closed。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 2 组语义直接锁住了：
  - direct provider path：`TestDirectoryReplayStoreFailsClosedOnCorruptFallbackDirectoriesAcrossProviderRebuild`
  - real runtime path：`TestDirectoryReplayStoreFailsClosedOnCorruptFallbackDirectoriesAtRuntime`
- 这两条合同一起覆盖了 4 组 fallback truth：
  - `.tmpdir` + `invalid_version`
  - `.tmpdir` + `trailing_garbage`
  - `.bakdir` + `invalid_version`
  - `.bakdir` + `trailing_garbage`
- fresh RED 的唯一真实缺口并不在 `src/`，而在 tests-only fixture：我最初给 direct provider 子用例造的 manual session label 太长，directory store 会把 replay key 再编码成 `.entry` 文件名，导致单文件名长度越过 filesystem 上限，第一次 materialize canonical truth 就写盘失败。把 fixture label 缩短后，focused suite 立即恢复 GREEN。
- fresh GREEN 明确锁住了当前正确边界：
  - corrupt `.tmpdir` / `.bakdir` fallback 不会被当成 empty store 或 valid truth
  - fresh blocked session 会继续 fail closed
  - original replay truth 不会被误恢复
  - canonical `main` 会继续保持缺失，corrupt fallback artifact 会继续保留
  - runtime resumed early-data path 仍然是 handshake success + session reused，但 early-data 必须 reject、无 accepted signal、discarded bytes 不可读
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in directory-store durability evidence，不是默认 shipped path 升级。
- next queue 因此进一步收紧到 2 条更直接、也更不容易返工的 directory-store 后续线：
  - filesystem blocker 语义
  - 更深的 crash-window write-interruption drift，仅在 fresh RED 出现时再重开

## 2026-04-16 Findings (Execution / FreePascal early-data directory store crash-window tempdir residue)
- 这批 fresh evidence 说明当前最高 ROI 的 remaining queue 里，“`.tmpdir` residue 语义”真正缺的是 focused contracts，而不是新的 production seam：现有 directory-store 已经天然满足 pure replay reject / repeated restart 下的 residue-preservation truth。
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 2 组语义直接锁住了：
  - direct provider path：`TestDirectoryReplayStorePreservesTempDirResidueAcrossRepeatedReplayRejects`
  - runtime restart path：`TestDirectoryReplayStorePreservesTempDirResidueAcrossRepeatedReplayOnlyRestarts`
- 这两条合同一起明确了 directory-store 当前的正确边界：
  - canonical `main` 缺失且 live `.tmpdir` 承载 replay truth 时，pure replay reject 不会误 consume fallback
  - repeated provider rebuild / replay-only restart 仍会继续 reject original replay session
  - 只有后续 fresh acquire / fresh resumed accept 真正写回时，才会重新 materialize canonical `main` 并 consume `.tmpdir`
- runtime harness 这轮只需要一个最小 tests-only 扩展：`RunReplayProviderRuntimeReplayProbeMode(...)` 增加 `reject_only` expectation，就足够表达 repeated restart residue contracts；不需要新增 child mode，也不需要改 `TFreePascalContext` / `TFreePascalConnection`。
- 并行 reviewer 提醒了 `SaveEntries(...)` 预删 `.tmpdir` 的潜在歧义，但这轮 fresh contracts 没有把问题打成 RED：当前实现至少已经证明“读取/拒绝 replay 不会误消费 residue”这条最值钱的真相。后续只有拿到 fresh failing evidence，才值得再打开 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 深挖 write-interruption drift。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in directory-store durability evidence，不是默认 shipped path 升级。
- next queue 因此进一步收紧到 2 条更直接、也更不容易返工的 directory-store 后续线：
  - `.tmpdir` / `.bakdir` fallback corruption fail-closed
  - directory-store filesystem blocker 语义

## 2026-04-16 Findings (Execution / FreePascal early-data directory store durability hardening)
- prototype batch 证明了 `IFreePascalEarlyDataReplayStore` seam 能承载第二种本地持久化 concrete shape；而这批 durability hardening 进一步证明，它也能承载第一批真正有价值的 cross-process / restart truth，而不是只在 happy-path rebuild 上成立。
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 的关键边界现在更清晰了：
  - readable resolution 明确是 `main > .tmpdir > .bakdir`
  - 只要 canonical `main` 已存在但形态错误，就继续 fail closed，不允许静默跌到 fallback
  - `.tmpdir` / `.bakdir` fallback 继续复用与 canonical `main` 相同的 entry validation / trailing-bytes reject 路径
- `tests/test_freepascal_tls13_early_data.pas` 现在把这批最值钱的 7 组语义直接锁住了：
  - active cross-process `.lock` holder 会让 direct provider fail closed
  - orphan `.lock` sidecar 在没有 active holder 时不会阻断 fresh acquire，也不会丢失 replay truth
  - orphan `.tmpdir` replay truth 会在 provider rebuild 后继续 reject replay，并在 fresh acquire 后重新 materialize canonical `main`
  - `.bakdir` replay truth 会在 provider rebuild 后继续 reject replay，并在 fresh acquire 后重新 materialize canonical `main`
  - runtime resumed early-data path 在 active cross-process lock 下继续 handshake success + session reused，但 early-data 必须 reject
  - runtime child process 会继续从 orphan `.tmpdir` 消费 replay truth，并在 reject 后恢复 canonical `main`
  - runtime child process 会继续从 `.bakdir` 消费 replay truth，并在 reject 后恢复 canonical `main`
- 这批也说明 directory-store 的恢复边界目前是“bounded durability”，不是无边界兜底：
  - 它会消费 `main` 缺失时的 `.tmpdir` / `.bakdir`
  - 但不会在 canonical `main` 已存在且坏形态时绕过错误
  - 也还没有承诺 crash-window residue、fallback corruption recovery 或 filesystem blocker 全覆盖
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；因为本批仍然只是 opt-in backend-private durability hardening，而不是默认 shipped path 升级。
- next queue 因此继续收紧到 3 条更高 ROI、且不易返工的 directory-store 后续线：
  - crash-window / tempdir residue 语义
  - `.tmpdir` / `.bakdir` fallback corruption fail-closed
  - directory-store filesystem blocker 语义
- 不应因为这批 closeout 再回头重开 file-backed `.bak` family、managed boundary、builder / factory / context / connection wiring；除非后续拿到 fresh failing evidence。

## 2026-04-16 Findings (Execution / FreePascal early-data directory store prototype)
- 刚完成的 file-backed `.bak` / permission-write-failure family 已经把现有 file-backed 实现压实；当前最高 ROI 的下一条线确实不再是重开同一 family，而是证明 `IFreePascalEarlyDataReplayStore` seam 真能承载第二种本地持久化 concrete shape。
- `TFreePascalDirectoryEarlyDataReplayStore` 现在给出了这个 fresh evidence：它只实现 `AcquireUpdateGuard / LoadEntries / SaveEntries`，上层 replay-check、expired prune、append、fail-closed 语义仍然完全复用 `TFreePascalStoreBackedEarlyDataReplayProvider`。这说明现有 store/provider seam 不是“只对 file-backed 特化成立”的偶然结构，而是真正可扩展的 backend-private 边界。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 4 组 truth 直接锁住了：
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 6 组 truth 直接锁住了：
  - direct provider rebuild 仍保持 directory-backed replay truth
  - expired persisted directory entry 会在 fresh acquire 前被 prune
  - runtime cross-context resumed early-data 仍会消费同一 directory store truth 并 reject replay
  - corrupt directory `.entry` 在 runtime path 上继续 fail closed，清理后恢复 fresh accept
  - existing-but-unreadable directory 在 runtime path 上继续 fail closed，不会被误当成 empty store
  - partial snapshot write failure 后 `.tmpdir` 会被清理，不留下下一次 save 的毒性残留
- 这批保持了最小正确范围：不改 `src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas`、builder / factory / config / public API；runtime 接线完全继续复用已有 `InstallStoreBackedReplayLedger(...)` 与 resumed accept path。
- 并行 reviewer 抓到的两个 residual gap 都证明这条原型线值得继续做 focused hardening，而不是停在 happy-path green：
  - `LoadEntries(...)` 对现有但不可枚举目录的边界必须 fail closed
  - `SaveEntries(...)` 在 snapshot write failure 后必须清掉 staging `.tmpdir`
- 这两个点现在都已经被最小修复并直接落成 focused contracts，说明本批不是“happy-path prototype”，而是已经开始给第二 concrete store shape 补可靠性边界。
- 当前 directory-store prototype 也有意保持了 bounded scope：它只锁 canonical directory path truth，不读取 `.tmpdir` / `.bakdir` fallback，也不承诺 cross-process crash recovery。这样本批先证明接口边界正确，再把更重 durability family 单独拆出来，返工成本最低。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批没有把默认 shipped path 升级为 durable-by-default，只是把 opt-in backend-private concrete shape 从 1 种扩到 2 种。
- next queue 因此收紧到 directory-store 的更重 durability family：cross-process coordination、crash-window / tempdir residue、backupdir fallback、filesystem blocker 语义；不再建议回头重开现有 file-backed `.bak` / parity / managed-boundary 家族。

## 2026-04-16 Findings (Execution / FreePascal early-data `.bak` trailing-garbage fallback closeout)
- 继 `.bak` fallback corruption hardening 之后，当前最高 ROI 的极小 sidecar 确实只剩下 `.bak` 合法头部 + 合法 entry 后尾随垃圾字节` 的 fail-closed 合同，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 2 组语义直接锁住了：
  - direct provider 的 `.bak` trailing-garbage fallback fail-closed
  - installer runtime path 的同组 `.bak` trailing-garbage fallback fail-closed
- 这批 focused fresh run 直接 GREEN，说明当前 provider 已经天然满足这组 trailing-garbage contracts；不需要再打开 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 做额外修法。
- 关键 truth 现在被更明确地锁住了：
  - 当 `main` 缺失、`.tmp` 不存在且 readable resolution 退回 `.bak` 时，即使 `.bak` 前半段是合法单 entry replay store，只要后面还有 residue bytes，就继续 fail closed
  - fresh blocked session 不会因为 trailing garbage 被误放行为有效新 truth
  - original replay truth 也不会因为 trailing garbage 被误恢复
  - runtime resumed early-data path 会继续 handshake success、session reused，但 early-data 必须 reject，且不会隐式重建 canonical main
- 这批 fresh evidence 也进一步说明 provider 内部边界已经收口完整：`ResolveReadableStoreFileName(...)` 只决定 fallback 读哪一个文件，而真正的 corruption / residue reject 继续统一落在 `LoadEntries(...)` 的 `LStream.Position <> LStream.Size` 边界；因此本批的真实收益继续来自补齐 direct/runtime 回归证据，而不是再碰 `src/`。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是把 opt-in file-backed `.bak` corruption family 最后一条轻量 sidecar 锁死，不是默认 durability 升级。
- next queue 因此正式回到更重的 provider / durability / persistence 形态；当前 `.bak` fallback corruption / trailing-garbage family 已不再是默认 future queue。

## 2026-04-16 Findings (Execution / FreePascal early-data `.bak` fallback corruption hardening)
- 继 deterministic permission/write-failure shapes 之后，当前最高 ROI 的下一批确实继续收缩到了 `.bak` fallback corruption fail-closed contracts，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 2 组语义直接锁住了：
- direct provider 的 corrupt `.bak` fallback fail-closed
  - store-backed runtime path 的同组 corrupt `.bak` fallback fail-closed
- 这批 focused fresh run 直接 GREEN，说明当前 provider 已经天然满足这组 `.bak` fallback corruption contracts；不需要再打开 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 做额外修法。
- 关键 truth 现在被更明确地锁住了：
  - 当 `main` 缺失、`.tmp` 不存在且 readable resolution 退回 `.bak` 时，invalid version / truncated payload / invalid count / invalid key-length 都继续 fail closed
  - fresh blocked session 不会因为 corrupt `.bak` 被误放行为“空 ledger”
  - original replay truth 也不会因为 corrupt `.bak` 被误恢复
  - runtime resumed early-data path 会继续 handshake success、session reused，但 early-data 必须 reject，且不会隐式重建 canonical main
- 这批 fresh evidence 也说明当前 provider 内部边界已经对齐：`ResolveReadableStoreFileName(...)` 负责把 `.bak` 放在 `main`/orphan `.tmp` 之后的 bounded fallback 位点，而 `LoadEntries(...)` 会对被选中的 readable file 继续统一执行同一套 corruption fail-closed 校验；因此本批的真实收益来自补齐 direct/runtime 回归证据，而不是再碰 `src/`。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in file-backed durability truth，不是默认 durability 升级。
- next queue 因此继续收紧到更重的 provider / durability / persistence 形态；若只想补一个极小 sidecar，当前剩余最轻的 corruption 变体是 `.bak` 合法头部 + 合法 entry 后尾随垃圾字节` 的 fail-closed 合同。

## 2026-04-16 Findings (Execution / FreePascal early-data permission-write-failure shapes)
- 继 `.bak` residue semantics 之后，当前最高 ROI 的下一批确实继续收缩到了 deterministic permission/write-failure shapes，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 4 组语义直接锁住了：
  - direct provider 的 deterministic temp write-open denied preserves-existing-truth / recovery
  - direct provider 的 deterministic backup-promotion rename denied fail-closed / recovery
  - store-backed runtime path 的 deterministic temp write-open denied preserves-existing-truth / recovery
  - store-backed runtime path 的 deterministic backup-promotion rename denied fail-closed / recovery
- 这批 focused fresh run 直接 GREEN，说明当前 provider 已经天然满足这两组 hook-based denial contracts；不需要再打开 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 做额外修法。
- 关键 truth 现在被更明确地锁住了：
  - `.tmp` write-open denied 时，fresh blocked session 会继续 fail closed，canonical main bytes 保持不变，`.tmp` 不残留，`.bak` 也不会被引入
  - existing-main replace fallback 上如果 `main -> .bak` promotion denied，fresh blocked session 也会继续 fail closed，canonical main bytes 保持不变，`.tmp` cleaned，`.bak` 不产生
  - 两组失败形态都会在同一 scripted provider / runtime context 下立即继续 reject original replay truth，证明 canonical main 没被 denial 分支破坏
  - normal provider / installer rebuild 后，previously blocked fresh session 仍会 first accept，并在下一次 replay 继续 reject
- 这批 fresh evidence 也说明当前最小 overrideable file-op seam 已经足够表达更真实的 denial 家族：`OpenWriteFileStream(...)` 与 `RenameFileAt(...)` 的 deterministic hook injection 足以覆盖高价值 permission/write-failure contract，而不需要先走 chmod / ACL / 平台权限位。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in file-backed durability truth，不是默认 durability 升级。
- next queue 因此继续收紧到更重的 provider / durability / persistence 形态；deterministic permission/write-failure family 已不再是默认 future queue。

## 2026-04-15 Findings (Execution / FreePascal early-data `.bak` residue semantics)
- 继 backup restore failure recovery 之后，当前最高 ROI 的下一批确实继续收缩到了 `.bak` residue semantics，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 2 组语义直接锁住了：
  - direct provider 的 `.bak` cleanup failure success + stale `.bak` undeletable next-save fail-closed
  - store-backed runtime path 的同组 `.bak` residue semantics
- 这批 focused fresh run 直接 GREEN，说明当前 provider 已经天然满足这组 delete-failure / residue contracts；不需要再打开 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 做额外修法。
- 关键 truth 现在被更明确地锁住了：
  - success-path 上 first `temp -> main` fail、`main -> .bak` success、second `temp -> main` success、随后 `.bak` cleanup delete fail 时，fresh truth 仍成功落在 canonical main 上
  - 残留 `.bak` 不会遮蔽 canonical main：同一 scripted provider / runtime context 会立即继续 reject old truth 与刚成功落盘的新 truth
  - 下一次 fresh save 遇到无法预删除的 stale `.bak` 时，provider / runtime path 都会 fail closed，并保持 canonical main bytes 不变
  - 后续正常 provider / installer rebuild 仍会先清掉 stale `.bak`，再 accept fresh blocked session，并在下一次 replay 继续 reject
- 这批 fresh evidence 也说明当前最小 seam 已经足够表达 delete-failure 家族：只需要 scripted `DeleteFileAt(<store>.bak)` failure +一次 `RenameFileAt(temp, main)` 首次失败，就能稳定覆盖 success cleanup residue 与 next-save fail-closed 两条最值钱的边界。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in file-backed durability truth，不是默认 durability 升级。
- next queue 因此进一步收紧到更真实的 permission/write-failure / filesystem denial 形态；`.bak` residue semantics 已不再是默认 future queue。

## 2026-04-15 Findings (Execution / FreePascal early-data backup restore failure recovery)
- 继 existing-main replace truth preservation 之后，当前最高 ROI 的下一批确实继续收缩到了 backup-assisted replace 的 restore-failure branch，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 2 组语义直接锁住了：
  - direct provider 的 backup restore-failure recovery
  - store-backed runtime path 的 backup restore-failure recovery
- 这批 fresh RED 暴露的真实 provider gap 是：当 `main -> .bak` 成功、second `temp -> main` 失败、且 `.bak -> main` restore 也失败时，既有 replay truth 虽然还留在 `.bak`，但当前 readable resolution 不会继续消费它，因此同一 store / rebuild 后都看不到旧 truth。
- 最小正确修法继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 内部：`ResolveReadableStoreFileName(...)` 现在改成 bounded fallback 次序：
  - `main`
  - orphan `.tmp`
  - only-if-`main`-missing-and-no-`.tmp`: `.bak`
- 这意味着 `.bak` 不再是永远不可读的工件，而是一个 restore-failure-only 的受限 truth source；它不会覆盖 canonical main，也不会抢 orphan `.tmp` recovery 的优先级。
- fresh focused evidence 证明：restore 失败后，provider 现在仍会 fail closed 拒绝 fresh blocked session，同时继续从 `.bak` 消费原有 replay truth；后续正常 provider / installer rebuild 可以基于 `.bak` 恢复 canonical main 并继续 materialize 新 replay truth。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in file-backed durability truth，不是默认 durability 升级。
- next queue 因此继续收紧到两个更真实、且比当前更重的 residual risk：
  - 更真实的 permission/write-failure / filesystem denial 形态
  - `.bak` residue semantics（例如 success 后 cleanup failure/stale `.bak` 对下一次写入的影响）

## 2026-04-15 Findings (Execution / FreePascal early-data existing-main replace truth preservation)
- 继 `SaveEntries(...)` temp/main-path 边界之后，当前最高 ROI 的下一批确实继续收缩到了 existing-main replace fallback / atomic-replace truth-preservation，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 2 组语义直接锁住了：
  - direct provider 的 existing-main replace fallback truth preservation
  - installer runtime 的 existing-main replace fallback truth preservation
- 这批 fresh RED 先暴露的是 provider 内部缺少可脚本化 file-op seam：focused subclass 试图 override `RenameFileAt(...)` 时直接编译失败，说明 file-backed replay store 还没有最小 overrideable file-op hooks。
- 最小正确修法继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 内部：
  - 为 `TFreePascalFileEarlyDataReplayStore` 增加 protected virtual file-op wrappers（`FileExistsAt`、`DeleteFileAt`、`RenameFileAt`、`OpenReadFileStream`、`OpenWriteFileStream`）
  - 为 `SaveEntries(...)` 增加 canonical `.bak` 路径
  - 把 commit 流程改成 backup-assisted replace：first `temp -> main` fail 后，必要时 `main -> .bak`，second `temp -> main` fail 时恢复 `.bak -> main`
- 这批 fresh evidence 证明现有 persisted replay truth 现在不会再因为 deterministic existing-main replace fallback failure 而退化；`.tmp` 继续由外层 cleanup 收口，`.bak` 不进入 load path，成功新写入后的 `.bak` 清理保持 best-effort。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批只是继续加固 opt-in file-backed durability truth，不是默认 durability 升级。
- next queue 因此继续收紧到两个更真实、但比重更小的 residual risk：
  - backup restore failure branch（`.bak -> main` restore fail closed）
  - 更真实的 permission/write-failure / filesystem denial 形态

## 2026-04-15 Findings (Execution / FreePascal early-data SaveEntries boundaries)
- 继 filesystem failure shapes 之后，当前最高 ROI 的下一批确实进一步收缩到了 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 的 `SaveEntries(...)` temp/main-path 写入边界，而不是重开 seam、builder / factory parity、`TFreePascalContext` 或 `TFreePascalConnection` wiring。
- `tests/test_freepascal_tls13_early_data.pas` 现在已经把这批最值钱的 4 组 truth 直接锁住了：
  - direct provider 的 temp-path write-failure preserves-existing-truth
  - direct provider 的 canonical main-path rename-boundary fail-closed / recovery
  - installer runtime 的 temp-path write-failure preserves-existing-truth
  - installer runtime 的 canonical main-path rename-boundary fail-closed / recovery
- 这批 fresh RED 暴露出一个真实 provider drift：canonical main-path rename-boundary fail-closed 时，`SaveEntries(...)` 会沿 early `Exit` 跳过 `<store>.tmp` 清理，导致失败后遗留 temp file。
- 最小正确修法继续严格收缩在 provider 内部：把 `SaveEntries(...)` 的 temp cleanup 放到外层 `finally`，这样 temp create/write/rename 任一失败 shape 只要返回 `False`，就会统一尝试清理遗留 `<store>.tmp`；无需改 public API、builder / factory / context / connection wiring。
- fresh focused / adjacent / completeness / compile evidence 证明这批只需要这一处最小 `src/` 修复；capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确，因为默认 shipped path 仍是 in-memory single-process ledger。
- 额外的本地探针显示：当前 Linux/FPC 环境里 `RenameFile(temp, existing-main-file)` 会直接成功覆盖已有目标，因此这次 focused batch 不能稳定命中“已有 main file + fallback delete + 第二次 rename 再失败”的分支。
- 因此 next queue 现在更精确地收紧为一个平台条件下仍可能存在的 provider durability gap：如果 first rename fail、fallback 删除旧 main、而 second rename 再 fail，既有 persisted replay truth 仍可能丢失。这个 existing-main replace fallback / atomic-replace truth-preservation gap 比继续重开 wiring 或 capability wording 更值得优先处理。

## 2026-04-15 Findings (Execution / FreePascal early-data filesystem failure shapes fail-closed and recovery)
- 继 store-path identity / cross-process boundary 之后，当前最高 ROI 的下一批确实不是重开 seam 或 public wiring，而是把 file-backed replay-store 的 filesystem failure shapes 直接锁到 provider/runtime truth。
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 的关键写入边界目前天然是 fail-closed 的：
  - `OpenLockFileStream(...)` 依赖 `ForceDirectories(...)` 与 `TFileStream.Create(...)`
  - `SaveEntries(...)` 依赖 temp-file create / rename / delete
  - 这些调用只要遇到路径对象形态冲突，就会返回 `False`，不会静默 accept
- 这批最值钱、最稳定的失败注入不是 Unix-only permission bits，而是三类跨平台 object-shape blocker：
  - `<store>.lock` 被目录占位
  - `<store>.tmp` 被目录占位
  - parent path 被普通文件占位导致 `ForceDirectories(...)` 失败
- fresh focused evidence 证明 direct provider 与 installer runtime path 都已经天然满足这三类合同：
  - blocker 存在时 fail closed
  - canonical main store file 不会 materialize
  - blocker 移除后，同一 session 会重新 accept
  - recovery 后 replay truth 继续 materialize，并在下一次对同一 session reject
- 这批唯一需要的实现改动仍然只在 tests/harness：
  - 新增 `RemoveReplayProviderPathIfExists(...)`
  - `CleanupReplayProviderStoreFiles(...)` 现在可以清理 empty-directory blocker，避免 `.lock` / `.tmp` directory fixture 污染后续 focused run
- 当前没有任何 fresh evidence 需要查看或修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas`、`src/fafafa.ssl.factory.pas` 或 `src/fafafa.ssl.context.builder.pas`。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；这批只是继续加固 opt-in file-backed durability/fail-closed evidence，不是默认 durability 升级。
- next queue 因此进一步收缩为更重的 provider / durability 形态验证，例如更真实的 permission/write-failure、atomic replace 边界或更复杂的 persistence failure shapes；不再值得回头重开当前 wiring。

## 2026-04-15 Findings (Execution / FreePascal early-data store-path identity and cross-process boundary)
- 现在最高 ROI 的下一批不是继续扩 seam / builder / factory wiring，而是给 file-backed replay truth 再补两类低返工合同：same-file path identity convergence，以及 cross-process different-store boundary isolation。
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 当前只对 file name 做 `Trim(...)`，没有 `ExpandFileName(...)` 或 canonicalize；主 store、`.tmp`、`.lock` 都直接基于原始路径字符串拼接，但真实读写仍完全委托给 filesystem，这意味着 same-file relative/absolute alias 大概率会自然收敛到同一物理边界，只是还没有 focused contract 锁住。
- `tests/test_freepascal_tls13_early_data.pas` 已有最关键的复用基元：
  - `BuildReplayProviderStoreFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `BuildAcceptingEarlyDataServerContext(...)`
  - `BuildInstallerFileBackedReplayStoreServerContext(...)`
  - `AssertResumedEarlyDataAcceptedAtRuntime(...)`
  - `AssertResumedEarlyDataRejectedAtRuntime(...)`
  - `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`
  - `TEST_REPLAY_PROVIDER_RUNTIME_CRASH_ACCEPT_MODE`
- 现有 same-process path-swap 合同锁住的是“different store file boundary”，还没有锁住“same physical file 的不同路径表示”；现有 cross-process restart contracts 也都还是同一个字符串路径。
- 本批最稳的实现顺序因此应该是：
  - 先补 installer runtime relative/absolute same-file contract
  - 再补 parent relative -> child absolute same-file contract
  - 最后补 parent file A -> child file B cross-process boundary isolation contract
- 当前默认仍不该碰 `src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`；只有 fresh RED 明确落到 file-backed provider path identity / load-save drift，才值得最小查看 provider 单元。
- fresh RED 最终证明 production 语义没有漂移，真正缺的是 focused child replay probe 的表达力：它把“child 应该看到 parent 已经 materialize 的同一个 store file”与“first resumed attempt 必须 reject”写死了，导致没法直接表达 different-store boundary 上的 `accept_then_reject` truth。
- 最小 GREEN 因此只需要对 `tests/test_freepascal_tls13_early_data.pas` 做 harness 扩展，而不是开新的 child mode 或碰 `src/`：
  - same-file relative/absolute convergence 继续复用原 replay probe 即可
  - different-store boundary 只需要给现有 replay probe 增加一个 `accept_then_reject` expectation 分支
- fresh focused evidence 现在锁住了三件事：
  - installer runtime path 下 relative/absolute 指向同一物理 file 时，replay truth 会收敛到同一 boundary
  - parent relative -> child absolute 的 same-file alias 在跨进程 restart 后继续 reject replay
  - parent file A 与 child file B 在跨进程场景下仍是独立 boundary：child 在 B 上 first accept，随后对同一 session second reject
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；这批只是继续加固 opt-in file-backed runtime durability evidence，而不是升级默认 durability 或 distributed guarantees。

## 2026-04-14 Findings (Execution / FreePascal early-data runtime file-store fail-closed recovery and isolation)
- 当前最高 ROI 的下一批不再是继续扩 builder/factory/installer construction-path 互证，而是把 file-backed provider 已经绿过的 main/orphan/corrupt semantics 提升到真实 runtime early-data `Accept` path。
- 这批最稳的最小路线仍然应该优先保持 tests/harness only：现有 focused suite 已经有 installer runtime、restart child、lock-contention、orphan-temp/provider-level 与 public-path runtime 基元，新的工作更像“把 provider truth抬到真实 handshake path”，而不是再扩新 seam。
- corrupt main store、corrupt orphan `.tmp`、orphan `.tmp` recovery、orphan `.lock` ignore 这四类语义如果能在 runtime path 直接 GREEN，就说明 file-backed anti-replay 的 fail-closed / recovery 行为已经从 provider rebuild 层延伸到了真正 shipped 的 resumed early-data path。
- store-boundary isolation/path-swap 是这批最值钱的 adjacent truth：它能证明 replay truth 绑定的是具体 replay-store file，而不是 context 或 construction path 的隐式内存残留；这能减少后续持久化/切换路径时的返工风险。
- 这批默认不该碰 `src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`；只有 fresh RED 明确说明 file-backed provider/load/save/orphan handling 在 runtime path 漂移，才值得最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批目标只是继续加固 opt-in file-backed runtime durability evidence。
- fresh focused run 最终直接 GREEN，说明这批最高 ROI 的落点确实是 tests/harness only：新增的 installer runtime contracts 没有暴露任何 `src/` 漂移，现有 file-backed provider/main-orphan-lock-path semantics 已经自然贯通到真实 resumed early-data `Accept` path。
- 新增的 `BuildAcceptingEarlyDataServerContext(...)`、`BuildInstallerFileBackedReplayStoreServerContext(...)`、以及 accepted/rejected runtime assertion helpers，把“provider-level truth”直接抬到了真实握手流上，而且没有引入新的 child mode、public surface 或 builder/factory wiring 返工。
- `TestContextFileBackedReplayInstallerFailsClosedOnCorruptStoresAtRuntime` 直接锁住了 corrupt main file 的四类 runtime fail-closed 形态：invalid version、truncated payload、oversize entry count、oversize key length 都会保持 resumed handshake 成功、session reused、early-data rejected、discarded bytes 不泄漏。
- `TestContextFileBackedReplayInstallerFailsClosedOnCorruptOrphanTempStoresAtRuntime` 与 `TestContextFileBackedReplayInstallerRecoversReplayTruthFromOrphanTempStoreAtRuntime` 一起证明了 orphan `.tmp` 语义已经进入真实 runtime path：corrupt orphan temp 继续 fail closed；live orphan temp replay truth 会直接拒绝 replay，而 fresh resumed early-data 仍可重新 accept 并 materialize canonical main file。
- `TestContextFileBackedReplayInstallerIgnoresOrphanLockFileWithoutActiveHolderAtRuntime` 证明 orphan `.lock` sidecar 在没有 active holder 时不会误阻断第一次 resumed early-data accept，也不会阻止 canonical main replay-store materialization。
- `TestContextFileBackedReplayInstallerRuntimeSwitchesReplayTruthBoundaryAcrossStorePathReinstall` 证明 replay truth 的边界已经明确绑定到 store file 本身：相同 session 在 file A 上 accept 后，切到 file B 会重新 accept；再切回 file A 会恢复 reject。这比只看 seam-level `TryAcquire` 更直接地锁住了真实 runtime path-swap / isolation truth。
- 这批 closeout 进一步收紧了 next queue：如果还要继续扩 early-data，最值得开的已经不是现有 installer/public wiring，而是更重的 provider / durability 形态验证；capability wording 继续保持 `experimental` 仍然完全正确。

## 2026-04-14 Findings (Execution / FreePascal early-data installer-to-public mixed cross-process convergence)
- 现在最高 ROI 的下一批不再是 public-path 之间继续互证，而是把 backend-private installer 父路径和已经验证过的 public child 路径接成同一条 persisted truth 证据链。
- 这批最稳的最小路线仍然是 tests/harness only：现有 `RunReplayProviderRuntimeReplayProbeMode(...)` 已经支持 installer / builder / factory selector，新的工作只是让 installer 父路径复用它，而不是再发明一套 child 协议。
- 如果 installer-parent -> builder/factory-child 两条新合同直接 GREEN，就能说明 persisted replay truth 与 construction path 真正解耦到了“store + session serialized truth”层，而不是只在 public-path family 内部自洽。
- 这批默认不该碰 `src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`；只有 fresh RED 明确指出 installer-parent materialized truth 过不了 public child，才值得最小查看 provider/ledger 实现。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批目标只是继续加固 opt-in file-backed durability evidence。
- fresh RED 最终没有落到 provider / ledger / resumed accept 实现漂移，而只落在 child `context_path` marker 证据过弱：marker 只 `TouchFile(...)` 会允许“child 静默回退到 installer path”的假绿空间。
- 最小 GREEN 也因此只需要 tests/harness fix：在 `RunReplayProviderRuntimeReplayProbeMode(...)` 里把 marker 改成写入 normalized `context_path` 文本，继续复用现有 installer / builder / factory selector，不新增 child mode，不触碰任何 `src/` 单元。
- fresh focused run 证明两条新合同都已闭环：
  - `TestContextFileBackedReplayInstallerRuntimeRetainsReplayTruthAcrossProcessRestartThroughBuilderContext`
  - `TestContextFileBackedReplayInstallerRuntimeRetainsReplayTruthAcrossProcessRestartThroughFactoryContext`
  installer-parent materialized 的 persisted replay truth 在 child 走 builder / one-shot factory public path 重建时仍会 reject replay，同时 fresh resumed early-data 继续 accept。
- 这批把证据链进一步收紧到“installer 父路径 -> public child 路径”的 mixed cross-process convergence，说明 persisted replay truth 现在已经和具体 construction path 更彻底解耦，而不是只在 public-path family 内部自洽。
- 本批继续保持 tests/docs/working-memory only：没有任何 fresh RED 需要查看或修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`。
- roadmap 的 next queue 仍应保持为更重 provider / durability 形态验证；不要因为本批 closeout 再重开当前 seam / public wiring / capability wording。

## 2026-04-14 Findings (Execution / FreePascal early-data mixed public-path cross-process durability)
- 刚完成的 mixed public-path same-process closeout 已经证明 builder 与 one-shot factory 在共享 replay-store file 时会消费同一份 persisted truth；下一批最高 ROI 不再是重复 same-process parity，而是确认这份 truth 穿过 restart 之后仍能被另一条 public path 消费。
- 当前最稳的最小路线不是新增 child mode，而是继续复用 `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`，只给现有 child replay probe 加 optional public-path selector。这样不会把 crash / lock / restart harness 再分叉一套。
- 如果新合同直接把 selector 当作“只读附加参数”传给旧 helper，测试会出现假绿风险，因为 child 仍可能走 installer path；因此需要一个最小 marker 证据，证明 child 确实走到了请求的 builder / factory public construction path。
- 这批仍然应该优先保持 tests-only / harness-only。只有 fresh RED 明确指向 persisted replay truth 在 builder-child 或 factory-child restart 后真的漂移，才值得查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确；本批目标只是为 opt-in file-backed 路径补更多 runtime evidence，并不改变默认 shipped truth。
- fresh focused run 最终直接 GREEN：builder-parent -> factory-child 与 factory-parent -> builder-child 两条 cross-process runtime contracts 都证明，父进程通过一条 public path materialize 的 persisted replay truth，在 restart 后可以被另一条 public path 正确消费，继续 reject replay，同时 fresh resumed early-data 仍 accept。
- 新增的 child `context_path` marker 让这批合同不再只是“传了额外参数”；它显式证明 child replay probe 真的按请求走到了 builder / factory public construction path，而不是静默回退到 installer path。
- 这批继续保持了 tests/docs only：没有任何 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`。
- `docs/ROADMAP.md` 现在应把 mixed public-path cross-process durability 也纳入当前真相，同时继续把 next queue 收紧为更重的 provider / durability 形态验证，而不是重开现有 public wiring。

## 2026-04-14 Findings (Execution / FreePascal early-data mixed public-path durability closeout)
- 刚完成的 builder / one-shot factory restart durability 已经证明两条 public path 都能各自 materialize persisted replay truth；下一批最高 ROI 不再是重复 restart，而是确认它们彼此也共享同一份 truth。
- mixed builder/factory public path 如果存在 drift，最可能出现在“同一路径 replay-store file 被不同 public construction path 消费”这层，而不是 `context.pas` / `connection.pas` 的 resumed accept wiring；因此这批应先在 focused runtime tests 上加 mixed contracts。
- expired persisted entry 的 direct provider rebuild 语义已经有合同，但 public-installed ledger 路径还没有同等级证据；补这一条能以很低返工成本锁住 public installation 没有绕开 prune-then-persist 语义。
- roadmap 的下一条 queue 现在应该继续写成“更重 provider / durability 形态验证”，而不是再把 seam、builder/factory parity、public-path runtime wiring 当作当前主线。
- fresh focused run 上，builder->factory 与 factory->builder 两条 mixed runtime contracts 都直接 GREEN：这说明 file-backed replay truth 对 public construction path 是对称的，builder 与 one-shot factory 在共享同一 replay-store file 时不会出现格式、装配或 runtime drift。
- public-installed prune contract 也直接 GREEN：builder-built context 上的 installed ledger 会先 prune matching expired persisted entry，再把 fresh replay truth 写回；随后 factory-built context 仍会正确看到 fresh persisted truth 并 reject replay。这证明 public installation 没有绕开 file-backed store 的 prune-then-persist 语义。
- 本批继续保持 tests/docs only 是正确的；没有任何 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`。
- `docs/ROADMAP.md` 现在已把 mixed public-path durability 与 public-installed prune evidence 纳入当前真相，同时继续把 future queue 收紧为“更重 provider / durability 形态验证”；capability wording 仍保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。

## 2026-04-14 Findings (Execution / FreePascal early-data public opt-in runtime durability)
- 当前 backend-private installer seam 的 runtime durability 已经足够强；下一批最高 ROI 不是再补同一层的更重 stress，而是确认 public opt-in 入口不会把这些 truth 接丢。最值得锁的是 builder 和 one-shot `TSSLFactory.CreateContext(const AConfig)`。
- 这批很可能还能保持 tests-only：现有 child replay helper 只要求父进程写出 replay-store file + serialized session file，因此父进程完全可以改为 builder/config public path，而 child 继续复用 backend-private installer replay probe，不需要新增 production seam。
- `tests/test_factory_config_early_data_isolation.pas` 当前只锁住 direct ledger acquire / config leak truth，还没有锁住真实 resumed early-data runtime durability；这说明 public config path 仍有一条高 ROI、低返工的 runtime 证据值得补。
- fresh focused run 最终证明这批可以完整保持 tests-only：builder 与 one-shot `TSSLFactory.CreateContext(const AConfig)` 两条 public path 都能让父进程真实 accept 第一次 resumed early-data、materialize file-backed replay truth，并在 restart 后由现有 child replay probe 正确 reject replay，同时 fresh resumed session 仍继续 accept。
- 这说明 public builder/config 路径并没有把 file-backed anti-replay opt-in truth 接丢；`ServerEarlyDataReplayStoreFile` 已经正确穿过 public config application 到真实 runtime path，因此本批不需要碰 `src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- 继续复用现有 `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE` 与 `RunReplayProviderRuntimeReplayProbeMode(...)` 是正确的最小路径：父进程只负责走 public path 并落盘 replay/session truth，child 仍使用既有 installer seam 验证 persisted replay truth，不需要新增 child mode、provider 协议或更重的 harness 分叉。
- 这批拿到的是 public opt-in runtime durability 的 fresh evidence，而不是默认 shipped durability 升级；因此 capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确。

## 2026-04-14 Findings (Execution / FreePascal early-data file-backed runtime tiny restart loop and harness cleanup)
- 当前最高 ROI 的下一批仍然应该保持 tests-first、tests-only 优先：process restart、crash-window restart、runtime lock-contention 这三条关键 runtime truth 已有 fresh evidence，剩余最值得补的是一个极小 repeated-restart smoke，用来证明这些 truth 在连续 3 轮 restart/probe 后不漂移，而不是继续扩生产面。
- tiny 3-round restart loop 新合同在 fresh focused run 上直接 GREEN，说明现有 file-backed installer/runtime path 已经天然满足 repeated-restart smoke；这批新增的是更强的回归证据，不是新的生产行为修复。
- `CleanupReplayProviderStoreFiles(...)` 目前扩到了 runtime tests 会稳定产生的 `session.bin` 与 `graceful` sidecar；因为它仍然是 tests-only helper，且所有 sidecar 都以同一个 replay-store temp root 为前缀，这个收敛是低风险且能减少 crash/restart tests 的显式清理重复。
- 本批仍不该动 capability wording：即使 opt-in file-backed runtime evidence 继续增强，默认 shipped truth 仍是 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 这批没有 fresh compile/runtime failure，也没有任何新的生产缺口信号；因此继续坚持“不因为想做更多而碰 `src/`”是正确的。

## 2026-04-14 Findings (Execution / FreePascal early-data file-backed runtime crash, lock, and stress)
- 这批最高 ROI 的 P0 gap 现在已经收口，而且仍然只需要 tests/plan 侧变更：真实 runtime `Accept` path 上的 file-backed anti-replay，在“accepted 后异常退出再重启”与“跨进程 lock contention”两条关键路径上都能维持正确语义；当前不需要改 `src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
- crash-window 合同最终收紧到了比最初 orphan-temp rename 更强的形状：父进程只负责 capture/serialize resumable session，真正的 first resumed early-data accept 发生在 child `--runtime-crash-accept` path；child 在 accept 成功并 materialize replay-store 后立刻 abnormal exit，随后新进程对同一 session 的 replay 仍被 reject，而 fresh resumed session 仍 accept。这样锁住的是“真实 accepted runtime path + restart durability”，不再只是 orphan `.tmp` fallback。
- current FPC `TProcess.ExitCode` 在这台 Linux runner 上对非零退出码不可靠。fresh RED 明确表明：即使 child 走了 `Halt(86)`，父进程里 `LProcess.ExitCode` 仍读成 `0`；同时一个最小本地 probe 也证明 `TProcess` 对 `/bin/sh -c 'exit 86'` 依旧返回 `0`。因此这批更稳的合同证据不是“断言 exit code = 86”，而是“ready marker 已出现，但 child 永远不会回到 `HandleReplayProviderChildMode` 去写 graceful marker”。这个证据更直接地区分了“accepted 后异常终止”和“正常返回”。
- runtime lock-contention 合同现在也从 direct provider 层提升到了真实 resumed `Accept` path：另一进程持有 sidecar advisory lock 时，当前进程仍然 handshake success、session reused，但 early-data 必须 fail closed 为 `sslEarlyDataRejected`，accepted signal 被抑制，discarded bytes 不可读；释放锁后，fresh resumed early-data 又能重新 accept。这比只测 provider acquire 更接近 shipped runtime truth。
- P1 tiny restart stress 在这批被有意 defer 是正确的：当前 P0 closeout 已经通过 fresh runtime evidence 成立，而再引入多轮 child-state / timing 协议，只会给 focused suite 带来更高脆弱性，却不会明显提高当前收益。后续若要继续推进，更值得开的方向应是“是否还要追加独立 stress 批次”，而不是把它强塞进本批。
- capability wording 继续保持不变依然正确：`0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。本批得到的是 file-backed opt-in path 的更强 runtime evidence，而不是默认 shipped durability 或 distributed-ready 承诺。

## 2026-04-14 Findings (Execution / FreePascal early-data file-backed runtime restart durability)
- 当前 file-backed opt-in path 真正还缺的不是新的 seam，而是一条更接近 shipped usage 的 runtime 证据：**进程 A 接受过的 early-data replay truth，到了进程 B / restart 后是否还成立**。这批 focused contract 已经把这个缺口锁住，而且不需要任何 production code 改动。
- 最小高效落点确实就是 installer-based runtime + child self-exec，而不是再扩 builder/factory/public surface。父进程先 accept 一次 resumed early-data 并把 session `Serialize` 落盘；子进程重新安装同一个 file-backed ledger、`Deserialize` 该 session 并写回 resumption cache 后，真实 resumed early-data path 仍然 **reject replay**，同时 fresh resumed session 仍然 **accept**。
- 这说明当前 file-backed provider/store、context installer seam 与 resumed accept wiring 已经足以跨进程保留 replay truth；当前更值钱的事实不是“又需要改 provider”，而是 **restart durability 这条真实 runtime truth 已经天然成立**。
- 子进程里额外验证 fresh resumed session 继续 accept 很关键：它证明 persisted replay truth 只拒绝已消费 ticket，不会把整个 store “毒化”成全局拒绝。这比只证明 replay reject 更完整，也更能支撑后续是否继续做 crash/orphan/stress 批次。
- 本批唯一出现的 fresh RED 只是 Pascal 声明顺序问题：child helper 提前用了 `PrepareServerContextForEarlyData` / `CaptureServerIssuedSession`，补 forward declarations 后即可 GREEN。这个失败进一步说明缺口在测试 harness，而不是 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`context.pas` 或 `connection.pas`。
- capability wording 继续保持正确：`0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。file-backed restart durability 现在只是 **opt-in runtime evidence**，不是默认 shipped durability 承诺。

## 2026-04-14 Findings (Execution / FreePascal early-data runtime parity and doc closeout)
- 这批 intended outcome 继续是 **tests + docs only**，而不是重开 production seam。fresh runtime evidence 最终也支持这一点：真正需要修正的是测试前提，不是 `TFreePascalContext` / `TFreePascalConnection` / public surface。
- fresh focused failure 暴露了一个关键边界：`SetSessionCacheMode(False)` / `SetSessionCacheSize(0)` 不能被当成“只切 local replay gate”的 runtime toggle。它们会把 resumption 本身关掉，导致 client 仍按 resumed transcript 解密、server 却回了非 resumed 路径，于是直接触发 `Failed to decrypt server handshake flight: AES-GCM decryption/authentication failed`。
- 因此，这批 runtime parity 正确的最小锁法不是再动 context/session-cache wiring，而是对安装后的 active ledger 取 `IFreePascalManagedEarlyDataReplayLedger`，只切本地 `SetEnabled(False/True)` 与 `SetCapacity(0/8)` gate。这样 resumption cache 继续有效，真正被测试的只剩 replay gate 行为。
- callback/file-backed opt-in 路径由此获得了更精确的 fresh evidence：它们虽然底层 provider 是 non-managed，但安装到 context 里的 active ledger 仍然暴露 managed local gate；这些 local gate toggle 不会隐式 wipe shared callback truth 或 persisted file truth，恢复后 replay 仍被拒绝，而 fresh resumed session 仍可再次 accepted。
- `docs/ROADMAP.md` 与 `docs/INTEGRATION_GUIDE.md` 现在和上述 truth 对齐：
  - 默认 shipped path 仍是 in-memory single-process anti-replay ledger
  - file-backed replay store 仍是 opt-in，不代表默认 durability
  - callback/file-backed path 上的 local enabled/capacity toggle 只控制当前 ledger gate，不该被描述成 wipe shared/persisted replay truth
  - capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
- 这批没有任何生产代码变更，说明当前 managed seam、installer/helper seam、builder/config/factory opt-in 与 resumed accept wiring 已足以支撑 runtime parity closeout；下一条更值得开的线仍然是更重的 provider/durability 验证，而不是重开当前 seam。

## 2026-04-14 Findings (Execution / FreePascal early-data managed seam contract lock)
- 这批最高 ROI 的 closeout 不是再改 `TFreePascalContext` / `TFreePascalConnection` / public surface，而是把上一批新增的 managed seam 边界写成 focused contracts。fresh evidence 现在已经锁住：shared in-memory managed clear / capacity 语义只作用于默认 shipped seam，non-managed callback/file-backed providers 不会因为 local disable / re-enable / capacity toggle 被隐式 wipe。
- callback/file-backed lifecycle contracts 现在更明确了：`TFreePascalProviderBackedEarlyDataReplayLedger` 的本地 `Enabled` / `Capacity` gate 仍只影响“当前 ledger 是否放行 acquire”，并不会反向篡改 non-managed provider 的共享或持久化 replay truth。这样 future persistence / callback experimentation 不会误以为 local gate 具备 wipe remote state 的语义。
- managed hook exception swallow 也从“代码上看起来如此”升级为“有 focused evidence 的 deliberate boundary”：即使 `IFreePascalManagedReplayProvider.Clear` / `SetCapacity` 抛异常，provider-backed ledger 仍保持 fail-closed 的本地 gate truth，不把异常外抛到 early-data runtime / handshake path。
- `IFreePascalManagedReplayStore` / `IFreePascalManagedReplayProvider` 这批只做了 internal-only 注释，没有尝试强行私有化，是刻意选择：它们当前仍位于 unit `interface` 区，后续若要真正隐藏，需要同时处理 class declaration 可见性与 test-only managed provider shapes。当前先把 contract 边界和 caveat 写清，比贸然挪动可见性更低风险。
- review 发现的一个低风险脆弱点已经顺手收掉：managed-hook swallow test 原本通过 `SetCapacityCalls >= 2` 间接依赖构造器也会触发 setter；现在改成基于调用前计数的增量断言，只锁住“本次 API 调用确实尝试了 managed hook”，不再依赖构造器实现细节。
- 仍保留一个低严重度 caveat：swallowed managed-hook exceptions 对未来“非 shared-in-memory 的 managed provider”仍可能带来 silent state divergence。当前 shipped truth 仍然准确地受 capability wording 描述约束：`0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`，因此这不是本批要扩大承诺的方向。

## 2026-04-14 Findings (Execution / FreePascal early-data default in-memory ledger convergence)
- 当前真正需要收口的不是 `TFreePascalContext` 或 resumed accept wiring，而是默认 in-memory ledger 自己还保留着一套独立 replay-state 实现。把 `TFreePascalInMemoryEarlyDataReplayLedger` 收敛成 shared in-memory store-backed thin wrapper，就能在不改 context 装配面的前提下消掉这段重复逻辑。
- 这批最小正确修法只需要在 `src/fafafa.ssl.freepascal.earlydatareplay.pas` 内新增 backend-private optional managed seam，而不需要扩 `session.pas` / `context.material.pas` 的 shared surface：一个 managed replay-store contract（`Clear` / `SetCapacity`）和一个 managed replay-provider contract（同样只 forward `Clear` / `SetCapacity`）就够了。
- shared in-memory replay store 现在不仅是一个 concrete store prototype，还承担了默认 shipped path 的 retained-state 语义：它会在 `SaveEntries(...)` 后执行 bounded eviction，在 `SetCapacity(0)` 或 `Clear` 时清空 retained entries；默认 capacity 设为“未受限”，避免 direct store-backed tests 在显式同步前被意外禁用。
- `TFreePascalProviderBackedEarlyDataReplayLedger` 的 managed forwarding 是这批的关键收口点：`SetEnabled(False)` 会对 managed provider 执行 clear，`SetCapacity(...)` 会同步 managed capacity，并在 zero-capacity 时清掉 shared in-memory retained state；file-backed / callback-backed / non-managed providers 仍保持 no-op semantics，没有被错误地赋予 wipe persisted state 的新语义。
- default in-memory shipped behavior 现在真正和 store seam 同轨了：`TFreePascalInMemoryEarlyDataReplayLedger` 不再维护独立 `FEntries` / prune / eviction / replay-check，而是委托给 shared in-memory store-backed provider-backed ledger。这让后续若继续推进 local persistence prototype，只需沿 store/provider seam 扩展，而不是再返工默认路径。
- default shipped wrapper 的 lifecycle parity 现在也有直接证据，而不再只靠构造推导：focused test 额外锁住了 default replay ledger 在 `SetSessionCacheMode(False/True)` 与 `SetSessionCacheSize(0/8)` 之间的 disable / re-enable / clear 语义，证明 wrapper 收敛后没有把既有 shipped behavior 带偏。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然是正确的：虽然默认实现已经收敛到底层 seam，但 shipped truth 依旧是 single-process in-memory coordination，而不是 durable-by-default 或 distributed-ready。

## 2026-04-13 Findings (Execution / FreePascal early-data store runtime fail-closed closeout)
- `InstallStoreBackedReplayLedger(...)` 接入的真实 resumed early-data runtime path 实际上早已 fail closed：`TFreePascalConnection.DoAccept` 只是取 active replay ledger 并调用 `TryAcquireEarlyDataSession(...)`；真正的 fail-closed 关键边界仍在 `TFreePascalProviderBackedEarlyDataReplayLedger` 与 `TFreePascalStoreBackedEarlyDataReplayProvider`。后者已经对 guard/load/save 的异常或 `False` 返回统一收口为 `False`，所以 runtime 不需要额外补丁。
- fresh runtime evidence 现在已经把这个结论锁实，而不是继续停留在推断：store guard/load/save 的 exception 与 `False` 返回模式，经过 `InstallStoreBackedReplayLedger(...)`、context installer seam 与真实 resumed `Accept` path，都会变成“握手继续成功 + session reused + early-data rejected + discarded bytes 不可读”。
- 在 runtime closeout 已经天然成立的前提下，当前最高 ROI 的最小生产增量不是硬改 connection/context，而是补一个第二 concrete store implementation。新增的 `TFreePascalSharedInMemoryReplayStore` / guard 证明 store seam 已经能够承载非 file-backed 的真实实现，同时不需要触碰 public API、builder/factory/config surface 或 capability wording。
- 现有 `TSharedReplayEntryStore` 的最佳定位已经进一步收窄：它继续只作为 focused test helper，用于 failure-mode 注入（raise / false），而不再承担“唯一 shared in-memory store shape”的职责。success-path rebuild / cross-context runtime 合同则改由真实 `TFreePascalSharedInMemoryReplayStore` 覆盖，更能证明 seam 边界本身正确。

## 2026-04-13 Findings (Planning / FreePascal early-data internal replay-store shape validation)
- 现有 replaceable seam、provider-backed ledger、custom provider installer、file-backed provider prototype、builder/factory parity 与 resumed accept wiring 都已经够稳；继续改 context / connection / public surface 的收益，已经低于“把 replay acquire 语义从具体持久化实现里抽出来”的收益。
- 当前真正还绑死在单个实现上的不是 provider seam，而是 file-backed provider 仍自己拥有 `lock -> load -> prune -> replay-check -> save` 全套语义。后续如果要扩别的 local/persistent store 形态，直接复刻这段逻辑的返工风险高于抽一个 internal replay-store seam。
- 这批最小正确修法应该落在 provider 之下，而不是 provider 之上：新增 narrow replay-store contract，让 generic store-backed provider 统一承接 replay acquire 语义，再把 file-backed provider 退化成这个 store seam 的一个特化实现。

## 2026-04-13 Findings (Execution / FreePascal early-data internal replay-store shape validation)
- `IFreePascalEarlyDataReplayStore` / `IFreePascalEarlyDataReplayStoreGuard` 这两个 backend-private contract 已经足够表达当前需要的最小 durable seam：store 只负责 update-guard + load/save truth，不负责 replay-check 语义；因此 future local persistence shapes 不必再复制一份 acquire 逻辑。
- `TFreePascalStoreBackedEarlyDataReplayProvider` 是这批真正的抽象落点：它统一承接 `load -> prune -> replay-check -> append -> save` 语义，并对 guard/load/save exception 或 `False` 返回统一 fail close。这样 callback/provider/file/local persistence 的“replay correctness”可以收口到同一段代码。
- file-backed provider 现在只是 file-backed replay-store 的薄特化：sidecar advisory lock、orphan `.tmp` recovery、corruption fail-closed、canonical main-file temp replace 都保留在 file store 里；cross-process / orphan-temp / corruption contracts 没有因为抽 seam 而退化。
- context / resumed accept path 不需要重做 wiring。existing `InstallReplayProviderBackedLedger(...)` seam 已经足够稳定，所以新增 `InstallStoreBackedReplayLedger(...)` helper 直接复用现有 installer 链路就能接进 active replay ledger 与 resumed early-data accept path。
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 仍然正确：虽然 internal replay-store seam 已落地，但默认 shipped behavior 并没有变成 durable-by-default 或 distributed-ready。

## 2026-04-13 Findings (Planning / FreePascal early-data callback provider fail-closed and durable shape validation)
- 在 generic replay-provider installer seam 已经收口后，当前最高 ROI 的剩余风险不是“再做一个更重的持久化实现”，而是 provider exception 仍可能直接打断 early-data acquire 与 resumed handshake。这个风险比继续扩 public/provider abstraction 更值得先收。
- 这批最小正确修法不该放在 callback helper 或 context accept path，而应落在 `TFreePascalProviderBackedEarlyDataReplayLedger` 的 provider 调用边界：这样无论 provider 来自 callback helper、generic installer seam，还是未来更重的 internal provider，只要抛异常都会统一 fail closed。
- shared callback-owned store 的 rebuild-shape 只需要 fresh focused contract，不需要新 abstraction。当前 seam 已经能表达“provider 对象可重建、replay truth 仍由外部 state 持有”，这正是后续更重 provider 形态要复用的边界。

## 2026-04-13 Findings (Execution / FreePascal early-data callback provider fail-closed and durable shape validation)
- 真正需要兜住的不是 `TFreePascalCallbackEarlyDataReplayProvider` 本身，而是 provider-backed ledger 的 acquire 边界：把 `FProvider.TryAcquireReplayKey(...)` 包进 bounded `try/except` 后，direct ledger path、generic installer path、callback helper path 都一起转成 fail closed，而不需要分别在 helper 或 connection 层补重复防护。
- fresh RED 证明了风险是真实的，不是推测：exploding callback provider 之前会直接把异常冒泡到测试与 resumed early-data runtime path；修复后同一路径变成 `False` / `sslEarlyDataRejected`，握手仍成功、session 仍 reused、discarded early bytes 不可读。
- shared callback-owned replay truth 在 provider / ledger 重建后的 focused coverage 继续证明：当前 generic provider seam 已经足够稳定，下一步若真的要做更重的 internal provider/store 形态，没必要重开 context wiring 或 public surface 设计。
- capability wording 继续保持 `experimental` 仍然是正确的：这批只是把 provider exception 语义补硬，并验证 callback-owned replay truth 的形态边界；默认 shipped path 仍是 in-memory single-process anti-replay ledger，没有升级为 durable-by-default 或 distributed-ready。

## 2026-04-13 Findings (Planning / FreePascal early-data custom replay provider installer shape validation)
- 现有 replaceable replay-ledger seam、provider-backed ledger、file-backed installer seam、builder/config/factory parity 与 focused runtime 覆盖已经足够稳定；当前最高 ROI 的下一步不是重开 public surface，也不是直接跳到 distributed persistence，而是证明 context wiring 已经泛化到“安装任意 replay provider”。
- 这批最小正确修法应该继续收缩在 backend-private seam 上，而不是扩 builder / config / connection：在 context material 上新增 generic provider installer interface，再让 `earlydatareplay` 暴露薄 helper / callback helper，就能验证 callback/provider shape 的稳定边界。
- capability wording 这批仍然不该动。即使 custom provider seam 落地，默认 shipped path 依旧是 in-memory single-process anti-replay ledger；当前工作只是把默认实现和可替换实现解耦，为后续更重的持久化原型留出安全扩展点。

## 2026-04-13 Findings (Execution / FreePascal early-data custom replay provider installer shape validation)
- `IFreePascalContextEarlyDataReplayProviderInstaller` 证明当前 context seam 已经足够 generic：`TFreePascalContext` 不需要 public API 变更，就能安装任意 `IFreePascalEarlyDataReplayProvider` 包装出的 active ledger。
- `InstallReplayProviderBackedLedger(...)` 与 `InstallCallbackBackedReplayLedger(...)` 这两个薄 helper 已经把 callback/provider shape 锁到现有 context wiring 上，并对 `nil` context、`nil` provider、缺 seam 场景保持 fail closed；resumed early-data accept path 与 managed-ledger session-cache sync 语义保持不变。
- 现有 `InstallFileBackedReplayLedger(...)` 现在只是 generic installer seam 的一个特化入口，而不再是独立 wiring。这个收口意味着后续如果继续做本地持久化原型或更重的 provider 形态，可以复用同一条 context 安装链路，避免重做 assembler 层。
- capability wording 继续保持 `experimental` 仍然是正确的：这批只验证 callback/provider shape 与 backend-private installer wiring；默认 shipped path 仍是 in-memory single-process ledger，尚未引入 cross-host / distributed durability 或 public provider ergonomics 承诺。

## 2026-04-13 Findings (Planning / FreePascal early-data replay-store cross-process coordination hardening)
- 当前 replaceable seam、provider-backed ledger、file-backed prototype、backend-private installer seam、builder/config/factory parity 与 orphan `.tmp` recovery 都已经足够稳定；继续扩 public surface 或直接跳 distributed persistence 的返工风险都高于收益，当前最高 ROI 剩余缺口只在“多个进程共享同一 replay store file 时仍可能绕过同一份 replay truth”。
- 这批最小正确修法应该继续收缩在 provider 内部，而不是再改 context / connection / builder：给现有 file-backed provider 增加 sidecar `.lock` coordination，把 `load -> prune -> replay check -> append -> save` 包在同一份跨进程可见锁里即可。
- fail-closed 语义需要锁住两端：active lock contention 必须拒绝 acquire；但 orphan / stale `.lock` file 如果没有 active holder，不应退化成“只看 lock file 存在就误拒绝”。

## 2026-04-13 Findings (Execution / FreePascal early-data replay-store cross-process coordination hardening)
- 当前 bounded 修法已经足够覆盖这个并发缺口：`TFreePascalFileEarlyDataReplayProvider` 现在会在状态迁移前获取 `AFileName + '.lock'` 的 advisory lock；只有拿到锁后才进入 `load -> prune -> replay-check -> append -> save`，因此不需要改 file format、ledger abstraction、context 装配或 resumed accept path。
- Unix/Linux 上使用 `Unix.fpFlock(..., LOCK_EX or LOCK_NB)` 是这批最稳的落点：active contention 或加锁异常都会 fail closed 返回 `False`；仅有 orphan `.lock` file 而无 active holder 时，provider 仍可继续 acquire，不会平白拒绝 fresh early-data。
- 这批 RED helper 实现过程中暴露出一个小但关键的测试辅助问题：最初把 child lock-holder 路径写成了 `BaseUnix` 风格，随后修正为 `Unix`，并同步改正计划文件中的 tech-stack wording，保证 focused test 能稳定持有真实 advisory lock。
- capability wording 继续保持不变仍然是正确的：默认 shipped path 仍然是 in-memory single-process anti-replay ledger；本批只 harden 了 Unix/Linux 下 opt-in file-backed provider 的 cross-process coordination，没有引入 cross-host / distributed durability 承诺。

## 2026-04-13 Findings (Planning / FreePascal early-data replay-store orphan temp recovery hardening)
- 现有 replaceable seam、provider-backed ledger、file-backed prototype、installer seam、builder/config/factory parity 已经足够稳定；当前最高 ROI 的剩余 durability 缺口不再是“再做一层 persistence abstraction”，而是 crash/interruption 落在“`.tmp` 写完但 main replace 未完成”时，orphan temp store 里的 replay truth 会被忽略。
- 这批最小正确修法应该严格收缩在 provider 内部恢复逻辑，而不是重开 context / connection / builder：主文件存在时继续以 canonical main store 为准；只有 main store 缺失时才把 `AFileName + '.tmp'` 当作 recovery candidate。
- capability wording 与 public surface 这批都不该动。即使 orphan temp recovery 落地，默认 shipped path 仍然是 in-memory single-process anti-replay ledger；file-backed store 仍是 opt-in path，不该过度承诺默认 durability。

## 2026-04-13 Findings (Execution / FreePascal early-data replay-store orphan temp recovery hardening)
- bounded helper `ResolveReadableStoreFileName` 已经足够覆盖这个 durability gap：它把读取决策限定为“main exists => main；main missing + temp exists => temp；otherwise empty”，因此不需要改 ledger/context/resumed accept path，也不会让 orphan temp store 在 main file 仍存在时覆盖 canonical truth。
- live replay truth 与 fail-closed semantics 现在一起被锁住了：
  - orphan `.tmp` 里有 live replay entry 时，provider rebuild 后仍会拒绝 replay
  - orphan `.tmp` 损坏、版本不匹配、截断时，provider 仍 fail closed，而不是忽略 temp file 后静默 accept
  - fresh acquire 成功后，状态仍通过现有 temp-file replace 流程 materialize 到 canonical main store
- capability wording 继续保持不变仍然是正确的：虽然 file-backed path 的 crash-window recovery 更稳了，但默认 shipped behavior 依旧是 in-memory single-process ledger，且这批没有引入跨进程锁、一致性协议或 distributed durability 承诺。

## 2026-04-13 Findings (Execution / FreePascal early-data replay-store factory parity and error contracts)
- 这批最小正确修法继续证明：当前最稳的落点不是再设计一层新的 public persistence abstraction，而是继续复用已经稳定的 backend-private seam `IFreePascalContextEarlyDataReplayInstaller`，只把 `TSSLConfig` / `TSSLFactory` 薄接到现有安装链路上。
- `ServerEarlyDataReplayStoreFile` 放在 `TSSLConfig` 上、默认空串，是当前最小且低返工的 public surface：
  - 空串仍保留 shipped in-memory ledger
  - one-shot config 只影响返回的 context，不污染 shared default config
  - builder / factory 都能共享同一套错误语义与 opt-in naming
- factory helper 需要保持 server-only、empty-string no-op、fail-closed。fresh tests 证明两条 builder-aligned 错误文案已经锁住：
  - 缺 seam：`Configured server_early_data_replay_store_file requires a backend that implements IFreePascalContextEarlyDataReplayInstaller`
  - 安装失败：`Configured server_early_data_replay_store_file could not install the requested replay store`
- capability wording 继续保持 `experimental` 仍然是正确的：虽然 replay-store file opt-in 现在已经贯通 builder 和 config/factory path，但默认 shipped path 仍然是 in-memory single-process anti-replay ledger，这批没有升级默认 durability 或 cross-process guarantees。
- `docs/ROADMAP.md` 的 stale queue 现在不该再写成“先补 replaceable / persistent seam”。真实状态已经前进到：seam、file-backed prototype、builder opt-in、factory/config parity 都已收口；后续若继续推进，应只在更重的 provider/durability 需求出现时再开新批次。

## 2026-04-13 Findings (Planning / FreePascal early-data anti-replay builder opt-in)
- 现有 `IFreePascalContextEarlyDataReplayInstaller` + `TFreePascalContext.InstallFileBackedReplayLedger(...)` 已经是稳定 seam，因此这批最小正确修法不是再改 context 或 helper，而是在 builder `BuildServer` 上薄接一层 server-only opt-in。
- 新字段沿用已有 `server_ocsp_stapled_response_file` 风格最稳，建议命名为 `server_early_data_replay_store_file`，这样 import/export、clone/reset/merge/override 的接线模式完全可复用。
- 本批仍然不该改 capability wording。即使 builder 能配置 file-backed replay store，默认 shipped path 仍然是 in-memory single-process ledger；opt-in 只是把现有 backend-private seam 暴露给 public builder/config，而不是升级默认语义或一致性承诺。
- 高 ROI 的 fresh RED 应集中在三类合同：
  - builder-built contexts 的跨 context replay rejection 仍成立
  - JSON / INI round-trip 可见新字段
  - clone / reset / merge 不会把新字段漏掉

## 2026-04-13 Findings (Execution / FreePascal early-data anti-replay builder opt-in)
- 这批最值钱的结果不是“又加了一个 provider 入口”，而是 public builder/config 终于和已经稳定的 backend-private installer seam 对齐了：`BuildServer` 只做薄接，context / connection / provider 结构完全不用重改。
- `server_early_data_replay_store_file` 作为命名是成立的：
  - 明确 server-only
  - 明确 file-backed
  - 不过度承诺 distributed/persistent semantics
  - 可以和 `server_ocsp_stapled_response_file` 共用 builder plumbing 模式
- fresh runtime evidence 证明 builder path 没有把 replay truth 带偏：
  - 两个 builder-built FreePascal server contexts 指向同一个 replay store file 时
  - 第一次 resumed early-data 仍被接受
  - 第二次跨 context resumed early-data 仍被 reject
  - resumed handshake 继续成功，discarded early bytes 不会泄漏到 `Read`
- capability wording 继续保持不变仍然是正确的：虽然 public builder 现在能 opt-in file-backed replay store，但默认 shipped behavior 依旧是 in-memory single-process anti-replay ledger；这批没有引入跨进程一致性承诺，也没有把 experimental 升级为 stable。

## 2026-04-12 Findings (Planning / FreePascal early-data anti-replay file installer lifecycle seam)
- 现在最值得收的不是立刻把 file-backed anti-replay 公开到 builder，而是先把 backend-private 安装路径从“free helper 可用”提升到“context 有显式 seam 可装”。这样后续如果要接 public opt-in，只是在稳定 seam 上再加一层，不会重做 provider / ledger / context wiring。
- lifecycle contract 还缺 fresh tests 锁住的点主要有三类：
  - install -> reinstall -> reset 是否真的切换 replay truth source
  - 不同 store file 是否相互隔离
  - helper-installed managed ledger 是否继续响应 `SetSessionCacheMode(...)` / `SetSessionCacheSize(...)`
- `TFreePascalContext` 已经有 `AddCRLFile(...)` / `LoadServerStapledOCSPResponseFile(...)` 这类 backend-private from-file 装配风格，因此给 early-data replay 再加一个 file-backed installer interface 是自然延伸，不需要 public surface 扩面。
- 这批仍然不该动 capability wording。即使 explicit installer seam 落地，默认 shipped behavior 仍然是 in-memory single-process ledger；file-backed path 仍是 backend-private opt-in。

## 2026-04-12 Findings (Execution / FreePascal early-data anti-replay file installer lifecycle seam)
- 这批最小正确修法证明了下一步 builder/config opt-in 的稳定落点已经有了：不需要再把安装逻辑藏在 free helper 里，`TFreePascalContext` 自己就能通过 backend-private seam 暴露 file-backed replay ledger installer，而 helper 只保留为薄封装。
- 最值钱的新合同不是“又多了一个 provider 实现”，而是 lifecycle truth 被 fresh tests 锁住了：
  - reinstall 到新 store file 会切走原 replay truth
  - reset 会回到默认 in-memory ledger
  - reinstall 回原 store file 会重新看到原先持久化的 replay truth
- `SetSessionCacheMode(...)` / `SetSessionCacheSize(...)` 对 helper-installed managed ledger 的同步语义现在也有 focused coverage，说明 context 现有 managed-ledger plumbing 足够支撑后续更正式的 opt-in path，而不需要重做 active-ledger 配置传播。
- capability wording 继续保持不变仍然是正确的：虽然 context 现在有显式 file-backed installer seam，但默认 shipped path 仍然是 in-memory single-process anti-replay ledger；public API、builder 和 config import/export 也都还没有被扩面。

## 2026-04-12 Findings (Planning / FreePascal early-data anti-replay file provider hardening + installer)
- file-backed provider prototype 已经证明 seam 边界成立；下一批最高 ROI 不再是“再发明一个 provider 形态”，而是把 prototype 补齐到更安全、更易装配的状态：
  - fail-closed corruption semantics 有 fresh tests
  - context 可以通过一个 backend-private helper 安装 file-backed replay ledger
- 这批仍然不该动 public API / builder。真正需要复用的 seam 已经在：
  - `IFreePascalEarlyDataReplayLedgerAccess`
  - `TFreePascalProviderBackedEarlyDataReplayLedger`
  - `TFreePascalFileEarlyDataReplayProvider`
- `TFreePascalContext` 已经有 `AddCRLFile(...)` / `LoadServerStapledOCSPResponseFile(...)` 这类 backend-private from-file 安装风格，因此给 file-backed replay ledger 增加一个 internal installer/helper 是自然延伸，不需要再扩 public surface。
- capability wording 仍必须保持不变。即使 helper + hardening 全部落地，默认 shipped path 还是 in-memory single-process ledger；file-backed path 仍只是 explicit opt-in internal prototype。

## 2026-04-12 Findings (Execution / FreePascal early-data anti-replay file provider hardening + installer)
- 这批最小正确修法继续验证了 existing seam 的复用价值：不需要再动 `TFreePascalContext` 或 builder，只要在 file-provider 单元里增加一个小的 `InstallFileBackedReplayLedger(...)` helper，就足以把 file-backed provider-backed ledger 装到真实 context 上，并让 `DoAccept` 继续通过 active-ledger seam 工作。
- file-backed provider 的 failure semantics 现在有 fresh contract 锁住了：
  - invalid version => fail closed
  - truncated store => fail closed
  - oversize entry count / key length => fail closed
  - expired persisted entry 会在 provider rebuild 后被 prune，不污染 fresh acquire
- 最有价值的运行时证据不是“direct provider 可以工作”，而是“helper 安装后的两个独立 context 仍会共享同一 replay truth 并拒绝第二次 early-data”。这证明后续若要继续扩到更正式的 internal installer 或 config-driven seam，当前边界不会推翻重来。
- capability wording 继续锁住仍然是正确的：虽然 file-backed path 现在更安全、更易装配，但默认 shipped behavior 依旧是 in-memory single-process ledger；helper 也仍是 backend-private opt-in，而不是默认或 distributed-ready 行为。

## 2026-04-12 Findings (Planning / FreePascal early-data anti-replay file provider prototype)
- 现有 replaceable seam + provider-backed ledger 已经够承接下一步，因此这批最高 ROI 的实现不该再碰 context / builder / public API；只需要新增一个内部 file-backed provider，就能证明 replay truth 已经可以脱离单对象内存状态。
- file-backed provider 应继续保持 contract 足够窄：
  - ledger 继续负责从 session 解析 `ticket key + expires at`
  - provider 只负责加载持久化 state、prune expired、拒绝 replay、记录 fresh acquire
- 为了减少返工，这批不追求跨进程强一致，只追求：
  - 同进程内 provider / ledger / context 重建后 replay truth 仍存在
  - 存储异常时 fail closed，而不是静默 accept early data
- 本地文件 prototype 最稳的形态是单独新建 internal unit，而不是继续把 file I/O 混进现有 `fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 现有 callback/provider-backed prototype 保持干净
  - file 细节与 ledger 细节不互相污染
  - 后续若要上 callback/provider registry 或更重的存储实现，替换成本更低

## 2026-04-12 Findings (Execution / FreePascal early-data anti-replay file provider prototype)
- 这批最小正确修法继续证明了 existing seam 的边界是对的：不需要再改 context / builder / public API，只要新增内部 `TFreePascalFileEarlyDataReplayProvider`，就能把 replay truth 从“活在某个 provider 对象实例里”推进到“活在 provider 可重建的本地持久化介质里”。
- file-backed provider 继续保持窄 contract 是关键：
  - ledger 仍独占 TLS session 语义与 expiry 真值
  - provider 只处理 `key + expiresAt + now` 的 acquire / prune / persist
  - 这样后续如果换 callback、SQLite、RPC 或分布式实现，不需要复制 TLS session 解析逻辑
- 本地文件 prototype 的最小安全边界已经被验证：
  - provider / ledger 重建后仍会拒绝同一 replay key
  - 两个独立 context 通过同一个 store file 也会拒绝跨 context replay
  - 文件损坏、版本不匹配、读写失败时 fail closed，不会静默 accept early data
- 这批仍然不能升级 capability wording。虽然 prototype 已证明 persistence seam 成立，但实现只用了进程内 `TRTLCriticalSection` 和 best-effort `temp file + RenameFile`，并不提供跨进程强一致；默认 shipped path 也仍然是 in-memory single-process ledger。


## 2026-04-12 Findings (Planning / FreePascal early-data anti-replay provider persistent prototype)
- replaceable seam 上一批已经够用，因此这批不该再扩 context public surface；最高 ROI 的下一步是只补一个 internal provider contract，证明 replay truth 可以跨 context 协调。
- 这个 provider contract 需要足够窄，避免把“session 解析真值”和“持久化介质”耦在一起：
  - ledger 继续负责从 session 导出 `ticket key + expires at`
  - provider 只负责按 key acquire / ignore expired / reject replay
- 先做 callback/provider-backed prototype 比直接做文件型更稳：
  - 更容易先锁定接口边界
  - 更容易在测试里构造 shared state
  - 不会把文件格式、锁、恢复策略等重变量提前引进来
- capability wording 这批仍然不能动。即使 provider prototype 成立，默认 shipped behavior 仍然是 in-memory single-process anti-replay，只有 persistent 默认路径真正落地后才适合升级口径。


## 2026-04-12 Findings (Planning / FreePascal early-data anti-replay replaceable / persistent seam)
- 当前 FreePascal early-data 的 transport、policy、config、ergonomics、focused gate 都已经闭环；真实剩余缺口只在 anti-replay persistence / replaceability。
- 当前 implementation 仍把 replay ledger 直接内嵌在 `TFreePascalContext`：
  - expiry / replay 语义已经正确
  - 但“默认内存实现”和“后续持久化实现”还没有 seam
- 因此这批最高 ROI 的做法不是重开 public API，也不是直接冲分布式，而是：
  - 先抽出 replaceable ledger seam
  - 保留默认 in-memory 行为
  - 让 resumed early-data accept path 走 active ledger
- capability wording 本批先锁住不动：只有当默认实现不再绑定 single-process / in-memory ledger 时，才适合继续调整 `KnownIssues`。


## 2026-04-12 Findings (Execution / FreePascal early-data anti-replay provider persistent prototype)
- 这批最小正确修法仍然不是 public API 扩面，而是在已有 replaceable ledger seam 上再补一个更窄的 internal provider contract：`IFreePascalEarlyDataReplayLedger` 保持 session-aware truth，provider 只负责共享 replay state 的原子 acquire。
- provider-backed ledger 必须继续由 ledger 自己解析 `ticket bytes -> key` 和 session expiry 真值；如果把这些责任提前推给 provider，后续 file/provider/distributed prototype 都会被迫复制 TLS session 语义，返工面反而更大。
- callback/provider-backed prototype 已经证明共享 replay truth 的边界是对的：
  - 两个独立 ledger 实例可以通过 shared provider 协同拒绝 replay
  - 两个独立 context 的 resumed early-data accept path 也能复用同一 provider state 拒绝跨 context replay
  - 现有 `SetEarlyDataReplayLedger(...)` / `ResetEarlyDataReplayLedger(...)` context 装配 seam 已足够承接这条能力
- provider-backed ledger 上的 `Enabled` / `Capacity` 只做本地 gate 是合理的最小设计；真正共享/persistent 的 state 不应被某个单 ledger 的 `Clear` 隐式抹掉，因此 prototype 阶段把 `Clear` 设计成 no-op 更符合后续持久化方向。
- capability wording 继续锁住是正确的：虽然这批已经证明“可替换 provider seam”成立，但默认 shipped behavior 仍然是 in-memory single-process anti-replay，`KnownIssues` 不能因为 prototype 成立就提前上调。


## 2026-04-12 Findings (Execution / FreePascal early-data anti-replay replaceable / persistent seam)
- 这批最小正确修法仍然是 internal seam，而不是 public API 扩面：在 `IFreePascalEarlyDataReplayLedger` acquire contract 不变的前提下，额外引入 context-host access seam，就足以把“默认内存实现”和“可替换实现”解耦。
- `IFreePascalManagedEarlyDataReplayLedger` 这个窄接口是必要的：它让默认 in-memory ledger、以及未来愿意响应配置变化的 custom ledger，都能继续跟随 `SetSessionCacheMode(...)` / `SetSessionCacheSize(...)`；但普通 custom ledger 又不会被强迫实现无关配置 surface。
- 新的 `TFreePascalInMemoryEarlyDataReplayLedger` 必须完整继承旧 truth，而不是只做一个“能通过一次测试”的 stub：
  - key 仍然来自 ticket bytes
  - acquire 前仍会 prune expired entries
  - expiry 仍以 session validity / timeout / ticket lifetime 为真值
  - bounded eviction 仍受 session-cache size 约束
- resumed early-data accept path 的真正解耦点在 `TFreePascalConnection.DoAccept`，而不是 context 自己：server 端必须先取 `IFreePascalEarlyDataReplayLedgerAccess.GetEarlyDataReplayLedger`，只有 seam 不可用时才回退到 plain ledger interface，这样后续持久化实现才不用复刻 context 内部结构。
- capability wording 本批必须继续锁住：虽然 replay ledger 已可替换，但默认 shipped behavior 仍是 single-process / in-memory anti-replay；在 persistent provider 原型真正落地前，`KnownIssues` 继续保持 experimental truth 才是准确口径。

## 2026-04-12 Findings (Execution / Root roadmap truth alignment and early-data next queue)

- 当前 root-level 漂移点主要集中在 `docs/ROADMAP.md`，而不是 `.github/README.md`、`.github/GITHUB_ACTIONS_GUIDE.md` 或 focused gate workflow 本身；后两者已经明确写出了 `ci.yml` 内存在独立的 FreePascal TLS 1.3 focused gate。
- `docs/ROADMAP.md` 继续把“focused gate 进入 CI”“OCSP / CT / validation hardening”写成当前主线，会直接拉偏下一批优先级；这批最高 ROI 是先修正 root queue，而不是重复做已经 closeout 的行为线。
- 当前 FreePascal capability truth 已经足够清楚：`KnownIssues` 只剩 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。因此最可信的下一条实现线，不是笼统“继续推进 0-RTT”，而是把 anti-replay 从单进程内存 ledger 继续收敛成 replaceable / persistent coordination seam。
- 本轮没有 fresh RED 指向 OCSP / CT / validation、也没有 fresh RED 指向 focused gate 缺席于 CI；因此这些线本轮不应重开，实现工作应被冻结到“出现新的 failing evidence 再说”。

## 2026-04-11 Findings (Execution / FreePascal server-side OCSP stapling public closeout)

- 这批真正要收的不是新的 stapling issuance runtime，而是把已经存在的 FreePascal server-side stapling seam 提升成 public truth；最小正确修法因此不是扩核心 `ISSLContext`，而是新增可选 public interface `ISSLServerOCSPStaplingContext`。
- public optional interface 的选择和现有 `ISSLHttpHooksAccess` / `ISSLEarlyDataContext` 风格一致：支持的 backend 显式暴露额外能力，不支持的 backend 不被迫实现空语义。
- backend-private seam 不需要删除；把 `IFreePascalContextServerStaplingMaterial` 改成继承 `ISSLServerOCSPStaplingContext`，既能复用现有 runtime wiring，也避免 public/private 双份方法定义。
- builder 层最小需要的是 file-based config，而不是再扩一套新的 OCSP fetch policy。`WithServerOCSPStapledResponseFile(...)` 刚好把配置入口限制在 caller-provided DER material 上，和当前 bounded design 对齐。
- `BuildServer` 这里不能 silent ignore。只要配置了 `server_ocsp_stapled_response_file`，但 backend 不支持 `ISSLServerOCSPStaplingContext`，就必须直接报错，否则配置表面成功、运行时却没有 stapled response，会制造更隐蔽的 drift。
- capability truth 到这批已经彻底不该再保留任何 OCSP / CT / resumption 剩余 gap。当前 FreePascal `KnownIssues` 的真实内容只剩 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`。
- 文档层真正需要同步的是两件事：
  - FreePascal server-side stapling 已经有 public optional interface 和 builder file config
  - 这条路径仍然只是 caller-provided material，不负责 online fetch、refresh，或 responder 调度
- 原 `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md` 现在已经过期；保留文件本身有价值，但内容必须改成 superseded / closeout note，而不是继续把它当作 future queue。


## 2026-04-11 Findings (Execution / FreePascal server-side OCSP stapling issuance)

- 当前真实 gap 不在 client stapling validation，而在 server accept path 根本没有“caller-provided stapled OCSP material -> `CertificateEntry` `status_request` extension”这段 bounded issuance seam。
- 最小正确修法不该扩公共 `ISSLContext`，而是新增 FreePascal backend 私有 optional interface `IFreePascalContextServerStaplingMaterial` 保存 stapled OCSP DER bytes，并复用现有 backend-private material pattern。
- request gating 不需要改大 parser 面；在 `TFreePascalConnection.DoAccept` 本地判断 `ClientHello` 是否带 `status_request` 已足够稳定，也避免把 extension policy 扩散到更广接口。
- 生产握手 builder 只应给 leaf `CertificateEntry` 挂 `status_request`，body 为 `status_type=ocsp + UInt24(len) + DER bytes`；issuer / intermediate 不应顺手带 stapling extension。
- server 发 stapled response 的边界现在明确为三条件同时满足：full handshake、client requested `status_request`、context configured stapled response bytes。任何一项不满足都保持原 `Certificate` flight 不变。
- gate promotion 完成后，focused completeness gate 现在能同时锁住 client-side stapling validation truth 和 server-side stapling issuance truth；fake `fpc` 调用次数相应提升到 `17`。


## 2026-04-11 Findings (Execution / FreePascal stapling truth and fast revocation regression)

- 前一批把 caller-provided CRL material plumbing 和 CRL parser correctness 收口后，`KnownIssues` 继续保留 `revocation evidence material plumbing` 已经不再是真实 truth；当前唯一剩余边界已经收窄到 `FreePascal server-side OCSP stapling issuance is not implemented.`
- 这批最值钱的新增回归不该再走完整 TLS 握手。直接测试 `TX509CRL.Issuer.ToString` 和 `TSSLCertificateChainVerifier` 的 bounded truth，能更快锁住：
  - CRL issuer DN short-name 不能再丢
  - matching CRL + non-revoked serial => PASS
  - matching CRL + revoked serial => `RevocationStatus = 1`
  - unavailable CRL material => fail-closed 且 `RevocationStatus = 2`
- unavailable case 最终不能用 `revocation_nonmatching_crl.pem` 伪造：
  - `VerifyChain(...)` 会检查整条链
  - nonmatching fixture 会把 root 证书路径一起带进来
  - 结果断言落到错误的 revocation surface
  - 真正稳定、可解释的 unavailable 设计是“不配置 CRL material”
- completeness gate 现在应该先跑这个 fast contract，再跑重型 validation runtime tests。这样一旦 future drift 回到 CRL parser / certchain 基础 truth，会在更低成本的位置尽早爆出。
- 下一阶段不该继续泛化成“broader validation hardening”；基于当前 fresh evidence，唯一值得单独立项推进的是 server-side OCSP stapling issuance。


## 2026-04-11 Findings (Execution / FreePascal revocation evidence material plumbing)

- 当前 closeout 之后真正还剩的 runtime gap 已经收窄到 revocation evidence material：连接层虽然会设置 `cvoCheckRevocation`，但没有 caller-provided CRL material seam，因此 `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 只能稳定落到 unavailable。
- 这批的最小正确修法不是扩公共 `ISSLContext`，而是给 FreePascal backend 增加私有 optional revocation material interface，并让现有 chain verifier 消费 caller-provided CRL PEM。
- fresh RED / GREEN 过程里真正暴露出的最后根因不在 connection 或 chain verifier 映射，而在 `src/fafafa.ssl.crl.pas`：
  - `TX509CRL.ParseName(...)` 之前只填了 `Attr.OID` 和 `Attr.Value`
  - 没有把 `Attr.Name := OIDToName(Attr.OID)` 回填进去
  - 结果 `LCRL.Issuer.ToString` 会变成 `=Test CA, =Test CA, =Beijing, ...`
  - 与证书 issuer 的 `CN=Test CA, O=Test CA, ...` 永远匹配不上，于是 non-revoked CRL 也被误判成 `No applicable caller-provided CRL material found for certificate issuer`
- 因此这批最后的最小正确修法是“修 CRL DN 解析”，不是继续扩 trust path：只补 `Attr.Name` 回填，就能让已有 caller-provided CRL plumbing 正常工作。
- 修复后 bounded truth 达成：
  - 无材料 => unavailable / fail-closed
  - 有匹配且有效的 CRL、serial 不命中 => good / allow
  - 有匹配且有效的 CRL、serial 命中 => revoked / fail-closed，并映射到 `sslErrCertificateRevoked`
- focused runtime、adjacent trust runtime、focused gate 与 `182/182` compile gate 都为绿，说明这次收口停在 revocation evidence material plumbing + CRL parser correctness，没有把 OCSP / CT / broader PKI 语义重新带偏。


## 2026-04-11 Findings (Execution / FreePascal validation closeout and focused gate)

- 当前最值得收的已经不是新的 runtime gap，而是 closeout truth drift：
  - `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues` 仍把 Batch 2 / 4 / 5 当成未完成
  - `tests/test_freepascal_backend_basic.pas` / `tests/test_capability_cache.pas` 也还在要求旧 wording
  - `scripts/run_freepascal_tls13_completeness_gate.sh` focused gate 只覆盖 TLS 1.3 主线与 capability smoke，没有把刚落地的 validation runtime tests 纳进来
- fresh RED 证明 drift 真实存在，而不是测试误写：
  - `tests/test_freepascal_backend_basic.pas` 失败在新的 `REVOCATION` 断言
  - `tests/test_capability_cache.pas` 失败在同一条 `KnownIssues` truth
  - `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 失败在 dry-run 未提到 `tests/test_freepascal_client_chain_trust_runtime.pas`
- 最小正确修法是“closeout truth alignment”，不是再开新实现批次：
  - `KnownIssues` 收窄成 `revocation evidence material plumbing for richer OCSP/CRL-backed certificate validation`
  - focused gate inventory 直接扩成 15 条，纳入 5 个高价值 validation runtime tests
  - roadmap 从 future queue 改成 closeout record
  - OCSP / CT 文档改成完成态，不再保留 Batch 2 / 4 / 5 的未来时态
- 本轮明确没有并入真实 revocation / CRL material plumbing：
  - 这已经超出 closeout truth 范围
  - 当前路线图已经收口；如果继续推进，应单独立新计划
- focused gate、full gate、compile gate 都为绿，说明这次收口控制在 capability/doc/gate truth alignment，没有把现有 runtime 行为重新带偏。

## 2026-04-11 Findings (Execution / FreePascal remaining cert verify flags runtime parity closeout)
- Batch 5 的缺口已经不是“flags 有没有枚举”，而是 runtime truth 还停在 storage / partial wiring：`sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 以前在 FreePascal client full-handshake path 上会被静默吞掉，或者只剩模糊的 store verify 结果。
- strict-chain 在 GREEN 过程里暴露出一个更基础的 root cause，而不只是 wording drift：
  - test fixture 的 leaf 确实带了 `digitalSignature,keyEncipherment`
  - 但 `TFreePascalCertificate.RebuildInfo` 之前没有把 `TX509KeyUsage` 回填进 `TSSLCertificateInfo.KeyUsage`
  - 因此 `CheckCertificateKeyUsage(...)` 看到的 leaf key-usage 位字段恒为 `0`，strict-chain 会在真正的 EKU gate 之前被提前误判
- 最小正确修法因此分成两层：
  - `ValidateClientPeerCertificateTrust(...)` 显式把 strict-chain 映射到 `cvoCheckKeyUsage` + `cvoCheckExtKeyUsage`，把 revocation / CRL 映射到 `cvoCheckRevocation`
  - `TFreePascalCertificate.RebuildInfo` 把解析出的 key-usage set 回填成既有 bitmask 语义，恢复 `CheckCertificateKeyUsage(...)` 的真实输入
- revocation / CRL 这批保持 bounded truth，不冒充完整 browser-grade PKI：
  - `TSSLCertificateChainVerifier` 现在区分 `revoked` 与 `unavailable`
  - `TFreePascalCertificate.VerifyEx(...)` 明确把 `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` surface 成 `RevocationStatus := 2`
  - 连接层对 unavailable 继续 fail-closed，而不是静默通过
- focused、邻近 regressions 与 `182/182` compile gate 都为绿，说明这次改动收口在 remaining verify flags runtime parity，没有把 OCSP / CT 更大范围一起带偏。

## 2026-04-11 Findings (Execution / FreePascal online OCSP broader hardening closeout)
- Batch 4 的真实缺口不是 fetch path 不存在，而是 online OCSP helper 之前把“收到 `good` cert status”与“整个 responder verification 已可信”压平成了同一个整数结果。
- 最小正确修法不是重写 HTTP hooks / issuer fallback，而是把 result truth 拆开：
  - `src/fafafa.ssl.openssl.api.ocsp.pas` 新增 `TOCSPCheckFailureStage` 与 `TOCSPCheckResult`
  - `CheckCertificateStatusDetailed(...)` 返回 cert status、`Verified` 与失败阶段
  - 旧的 `CheckCertificateStatus(...)` 只保留为 wrapper：只有 `Verified=True` 才返回真实 cert status，否则返回 `V_OCSP_CERTSTATUS_ERROR`
- 这样连接层终于能把不同 failure surface 说清楚：
  - response / basic-response 缺失
  - cryptographic verify failure
  - responder verification failure
  - status not found / freshness / nonce failure
- `TFreePascalConnection.ValidateClientOnlineOCSP(...)` 现在消费 richer result 后，`good-but-unverified` 不会再被错表述成通过；cryptographic / responder verification failure 会明确 fail-closed，并保留可读的 verify-result string。
- focused、邻近 regressions 与 `182/182` compile gate 都为绿，说明这次 hardening 收口在 online OCSP broader truth，没有重新打开 fetch parity、CT source 或更大的 delegated responder architecture。


## 2026-04-11 Findings (Execution / FreePascal OCSP stapling cryptographic hardening closeout)
- Batch 3 的缺口是真实的 verified-truth drift，不是 wording 问题：fresh RED 证明一个 `good status`、`CertID` 匹配、freshness 正常、但没有真实签名证明的 stapled OCSP response，会被当前实现直接 surface 成 verified。
- 最小正确修法不需要重写 stapling state machine，也不需要顺手扩 online OCSP：
  - `src/fafafa.ssl.openssl.api.ocsp.pas` 只新增一个 bounded helper，把 raw response DER 与 leaf/issuer DER materialize 成 OpenSSL 对象后复用现有 `VerifyOCSPResponse(...)`
  - helper 内部只额外做一件必要的事：用 issuer cert 建一个临时 `X509_STORE`，让 stapled response 的 cryptographic verification 有明确 trust anchor
  - `src/fafafa.ssl.ocsp.stapling.pas` 只是在 `ocspGood` 之后再加一道 cryptographic verify gate
- 收紧后的 source-of-truth 现在是一致的：
  - parse / freshness / cert status success 只是进入下一步的前提
  - 只有 cryptographic verify 也通过时，才允许 `Result.Status := ossVerified`
  - cryptographic verify 失败时，optional path 继续连接成功，但状态文本明确是 verification failure；required path 则按既有 gate fail-closed
- focused GREEN、CT surface / online OCSP 邻近回归和 `182/182` compile gate 都为绿，说明这次改动收口在 stapled OCSP verified truth，没有把 CT source parity 或 online fetch path 一起带偏。




## 2026-04-11 Findings (Execution / FreePascal OCSP-delivered CT source parity closeout)
- Batch 2 的缺口是真实的 runtime gap，不是文档上的“待补齐”口径：focused RED 在握手保持为绿的前提下，稳定失败在 `OCSP-delivered SCT list should surface CT as enabled`，说明问题确实是 OCSP-delivered SCT 没进当前 CT surface。
- 最小正确修法不需要改 CT policy，也不需要动 stapling / online OCSP cryptographic verification：
  - `src/fafafa.ssl.ocsp.pas` 只新增 `signed_certificate_timestamp` 相关字段与解析
  - `src/fafafa.ssl.ct.sct.pas` 只把 `ValidateFromOCSP(...)` 从空实现补到可复用 `ValidateSCTList(...)`
  - `src/fafafa.ssl.freepascal.connection.pas` 只在 TLS extension 与 embedded X.509 都为空时，才尝试从 `FOCSPResponse` 采用 OCSP-delivered SCT list
- OCSP source precedence 也已经收紧到这批需要的最小粒度：
  - matching single-response SCT list 优先
  - response-level SCT list 作为 fallback
  - 不在这批决定 stapled vs online 的更大 source precedence
- 这次实现还验证了一个边界选择是对的：
  - 连接层只要把 OCSP-delivered raw SCT list 送进现有 `FSignedCertificateTimestampList` 缓存
  - 现有 CT validation bridge 就能继续工作
  - 不需要为了 OCSP source 再引入一套新的 connection-level validation 状态机
- focused GREEN、stapling / online OCSP 邻近回归和 `182/182` compile gate 都为绿，说明这次改动收口在 OCSP-delivered CT source parity，没有把 OCSP 更大的真实性/签名验证问题一起卷进来。



## 2026-04-11 Findings (Execution / FreePascal CT issuer-source evidence hardening closeout)
- 上一批 leaf-only CT surface 为绿，只能证明 availability，没有证明 issuer source 正确；这一批新的 runtime contract 才真正把 CT eval context 里的 issuer 拉出来看。
- 直接替换 `CT_POLICY_EVAL_CTX_set1_issuer` / `SCT_validate` 这类 CT 全局函数指针并不稳定，因为 `RefreshCertificateTransparencyValidationState(...)` 里的 `TryEnsureOpenSSLCTValidationAvailable(...)` 会再次 `LoadCTFunctions`，重绑 CT 函数指针。最终稳定的观测 seam 是在 `BIO_new_mem_buf` 上后置重装 `SCT_validate` stub，再从 `CT_POLICY_EVAL_CTX_get0_issuer(...)` 读取 eval context 里的 issuer。
- fresh RED 给出的证据是明确的：
  - observed subject = leaf `/C=US/ST=California/L=San Francisco/O=fafafa.ssl-tests/OU=Development/CN=ct.example.com`
  - observed issuer = CA `/C=CN/ST=Beijing/L=Beijing/O=Test CA/CN=Test CA`
  - 说明 CT path 在 leaf-only server-chain 场景下确实把 leaf 自己当成 issuer 传进了 OpenSSL CT eval context
- 最小正确修法不需要再造一套 CT 专用 resolver：
  - `RefreshCertificateTransparencyValidationState(...)` 现在直接复用现成的 `TryResolvePeerIssuerCertificate(...)`
  - 这样 CT / trust verification / online OCSP 的 issuer resolution 终于统一
- focused GREEN、3 个邻近回归和 `182/182` compile gate 都为绿，说明这次改动收口在 CT issuer source，不是一次更大的 CT/OCSP 语义漂移。


## 2026-04-11 Findings (Planning / FreePascal validation next-wave roadmap)
- 当前 `KnownIssues` 里的 3 条大类已经被收敛成 5 个更可执行的批次，而不是继续沿用宽泛表述：
  - `broader OCSP validation hardening` => 拆成 stapled OCSP cryptographic hardening + online OCSP broader hardening
  - `OCSP-delivered Certificate Transparency source parity` => 保留为单独一批
  - `broader certificate validation hardening` => 进一步收敛成 remaining cert verify flags runtime parity
- 第 1 批必须先做 CT issuer-source 证据面补强，而不是直接修代码：
  - 上一批已经证明 leaf-only CT surface 在当前 dummy harness 下为绿
  - 但这还不足以证明内部 issuer source 一定正确
  - 因此下一批的最小正确动作是补“能观测 issuer source”的 evidence，而不是继续做对称性修复
- 第 5 批不再写成空泛的 “broader certificate validation hardening”：
  - 当前最具体、最可执行、最明确的剩余缺口其实是：
    - `sslCertVerifyStrictChain`
    - `sslCertVerifyCheckRevocation`
    - `sslCertVerifyCheckCRL`
  - 这些 flag 在 FreePascal client runtime path 还没有像 hostname / expiry / allow-self-signed 那样形成真实验证语义
- OCSP-delivered CT source parity 的关键断点已经明确：
  - `src/fafafa.ssl.ocsp.pas`
    - `ParseResponseData(...)` 只显式处理了 nonce
    - `ParseSingleResponse(...)` 尚未解析 `singleExtensions`
  - `src/fafafa.ssl.ct.sct.pas`
    - `TSCTValidator.ValidateFromOCSP(...)` 仍是空实现
  - `src/fafafa.ssl.freepascal.connection.pas`
    - 当前不会把 `FOCSPResponse` 当成 SCT source
- stapled / online OCSP hardening 必须拆开做：
  - stapled path 已有 raw `FOCSPResponse`
  - online path 还叠加了 HTTP hooks、AIA fetch、richer result mapping
  - 分批能避免一次混入太多 failure surface

## 2026-04-10 Findings (Planning / FreePascal CT trust-store issuer fallback)
- 当前 FreePascal client CT validation 的真正缺口已经不是 “有没有 validation surface”，而是 issuer material 的来源仍停在旧假设：
  - trust verification 已经能通过 context trust store / CAFile / CAPath 找到 issuer
  - online OCSP 也已经改成优先 peer chain、其次 trust store
  - 但 `RefreshCertificateTransparencyValidationState(...)` 仍是 `peer chain[1]`，否则直接回退 leaf
- 这会让 “server 只发 leaf，但 issuer 在本地 trust store” 的路径出现语义分叉：
  - 握手信任校验能过
  - online OCSP 现在也能过
  - 只有 CT validation 可能退回 leaf 自己，最终把结果表述成 `Validation unavailable`
- 这批最小正确动作因此不是再改 CT policy，也不是扩大 OpenSSL CT surface，而是只让 CT path 复用现有 `TryResolvePeerIssuerCertificate(...)`。

## 2026-04-10 Findings (Closeout / FreePascal CT trust-store issuer fallback)
- 这批最重要的结论不是“又要补一段生产代码”，而是当前怀疑的缺口没有被 fresh runtime evidence 证实：
  - 新增的 leaf-only CT contract 在不改生产代码的前提下直接为绿
  - 当前 bounded CT validation surface 没有因为 server 省略 issuer chain 而退化成 `Validation unavailable`
- 因此这批没有去复制 online OCSP 那套 issuer resolver 进 CT path：
  - 没有 runtime 证据时，不应该为了代码形状相似就做“对称性修复”
  - 当前用户可见行为已经满足 contract
- 同时也明确了测试边界：
  - 现有 dummy SCT list 足以证明 leaf-only surface availability
  - 但不足以证明 CT validation 内部究竟走了 peer-chain issuer 还是 trust-store issuer
  - 如果后续真要验证“issuer source parity”，需要更强的证据面，例如 real/stubbed precert-SCT fixture，而不是继续从代码形状外推

## 2026-04-10 Findings (Planning / FreePascal online OCSP trust-store issuer fallback)
- 当前 FreePascal client online OCSP 的真正缺口已经不是 “会不会发 OCSP 请求”，而是 issuer material 的来源过窄：
  - trust verification 已经能通过 context trust store / CAFile / CAPath 找到 issuer
  - 但 online OCSP 仍然只认 `FPeerCertificateChain[1]`
  - 如果 server 只发 leaf，现有实现就会把 leaf 自己当 issuer，和已通过的 trust verification 脱节
- 这批最小正确动作因此不是重写 OCSP verifier，也不是继续扩 CT，而是只收 issuer resolution：
  - peer chain 优先
  - trust store fallback
  - 仅在自签名场景才允许 leaf self-issuer fallback
- 这也正好符合当前 `KnownIssues` 里剩下的更窄边界：不是 online OCSP path 不存在，而是 broader validation hardening 还要继续收口。

## 2026-04-10 Findings (Closeout / FreePascal online OCSP trust-store issuer fallback)
- 这批真正收口的是 FreePascal client online OCSP 和 trust verification 之间的 issuer 一致性：
  - 以前 server 不发 issuer chain 时，online OCSP 会退回 leaf 自己，导致 issuer 取错
  - 现在改成：
    - 先用 `FPeerCertificateChain[1]`
    - 找不到再走 `IFreePascalContextTrustStore.BuildVerificationStore()`
    - 按 `FindBySubject(FPeerCertificate.GetIssuer)` 找 issuer
    - 只有 self-issued 证书才允许继续把 leaf 当 issuer
- focused RED 说明这个缺口是实打实的 runtime contract，不是文字推演：
  - `tests/test_freepascal_client_online_ocsp_runtime.pas` 新增 leaf-only server chain 场景后，旧实现会失败在 `Online OCSP good status should keep working when issuer only exists in trust store`
- 最小修法仍然控制在连接层：
  - `TFreePascalConnection.TryResolvePeerIssuerCertificate(...)`
  - `TryBuildPeerOCSPCertificatePair(...)` 改走新 helper
  - `ValidateClientOnlineOCSP(...)` 的 OpenSSL materialize path 也改走同一 helper
- 邻近 chain trust / OCSP stapling / CertificateVerify 回归与 `182/182` compile gate 都为绿，说明这次改动没有扩散到更大的 TLS/CT 语义。

## 2026-04-10 Findings (Planning / FreePascal online OCSP wording alignment)
- 当前 FreePascal OCSP 相关能力的真实状态已经不再是“online OCSP fetch 还没做”，而是运行时闭环已经落地、文案还停在旧阶段：
  - client `sslCertVerifyCheckOCSP` online AIA fetch 已接入 `verify-peer` 的 non-resumed full-handshake path
  - context-level HTTP hooks access 已存在
  - stapled OCSP `non-good` acceptance semantics 也已经收紧
- 这批的真实缺口因此不是运行时代码，而是 capability / docs drift：
  - `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues` 还把 `online OCSP fetch parity` / `OCSP stapling validation hardening` 写成 pending
  - `docs/guides/OCSP_USAGE_GUIDE.md` 还没有把 FreePascal client online OCSP path 写清楚
  - `docs/guides/security-best-practices.md` 与 `docs/DOCUMENTATION_INDEX.md` 也还没有同步这条能力闭环
- 这批最小正确动作因此是 wording alignment，而不是继续扩实现：
  - 去掉过期 wording
  - 保留真实剩余边界：broader OCSP validation hardening、OCSP-delivered CT source parity、broader certificate validation hardening
  - 不提升 capability 等级，也不把 FreePascal wording 写成完整 revocation parity

## 2026-04-10 Findings (Closeout / FreePascal online OCSP wording alignment)
- `KnownIssues` 现在终于和 runtime truth 对齐了：
  - 不再把 `online OCSP fetch parity` 说成未完成
  - 不再把 `OCSP stapling validation hardening` 说成未完成
  - 仍然明确剩余边界是 broader OCSP validation hardening、OCSP-delivered CT source parity，以及 broader certificate validation hardening
- OCSP guide 的入口也已经从旧的 “OpenSSL 后端指南” 收紧成三条真实路径：
  - FreePascal client runtime OCSP stapling
  - FreePascal client online OCSP check
  - OpenSSL helper workflow
- security best practices 的收口重点不是“什么都支持了”，而是把当前建议写回真实边界：
  - 有 `VerifyPeer + OCSP stapling` 可选/required 路径
  - 有可选的 client online OCSP check
  - 但仍然没有 broader responder-signature / issuer-chain parity，也没有 FreePascal server-side stapling issuance
- 这批 verification 说明 scope 控制住了：
  - wording-focused tests 为绿
  - 旧短语 grep 为零命中，新短语与新入口 grep 命中
  - `182/182` compile gate 继续为绿


## 2026-04-10 Findings (Planning / FreePascal client online OCSP fetch parity)
- 当前 FreePascal client 的 OCSP 相关能力已经不再是“完全没有基础设施”，而是只差最后一段 verify-path 接线：
  - `sslCertVerifyCheckOCSP` flag 已存在
  - `GetOCSPURLFromCertificate(...)` 已能从 leaf AIA 提取 responder URL
  - `fafafa.ssl.openssl.api.ocsp` 已提供 `CreateOCSPRequest / SendOCSPRequest / VerifyOCSPResponse / CheckCertificateStatus`
  - `fafafa.ssl.net.hooks` 已经提供 thread-local HTTP GET/POST 注入点
  - OpenSSL connection / certificate `VerifyEx` 已经把 `sslCertVerifyCheckOCSP` 接成在线 fail-closed 路径
- FreePascal 侧的真实缺口有两段，而且两段都要一起补才算“可用”：
  - `TFreePascalConnection` 在 full-handshake verify 流程里还没有 online OCSP helper
  - `TFreePascalContext` 目前还没有实现 `ISSLHttpHooksAccess`，builder/context 级 HTTP hooks 也进不去
- 这批因此不能只补连接层 helper；否则会出现“逻辑已接线，但没有上下文级 transport 注入路径”的半成品。
- 当前范围继续保持窄边界：
  - 只做 FreePascal client
  - 只做 `verify-peer` + non-resumed full handshake
  - `sslCertVerifyCheckOCSP` 打开时走 AIA online OCSP fetch
  - 不承诺 responder signature / issuer-chain / delegated responder 的 broader parity 收口

## 2026-04-10 Findings (Closeout / FreePascal client online OCSP fetch parity)
- 这批真正补齐的不是新的 OCSP verifier，而是 FreePascal client 对既有 bounded verifier 的接线闭环：
  - `TFreePascalContext` 现在暴露 `ISSLHttpHooksAccess`
  - `TFreePascalConnection` 在 `verify-peer + non-resumed + sslCertVerifyCheckOCSP` 上会提取 leaf AIA URL、materialize OpenSSL `PX509`、并通过 context HTTP hooks 执行 online OCSP POST
  - `good` 放行，`revoked/unknown/error` 继续 fail-closed
- focused RED 先证明了两个真实缺口：
  - context 还不支持 `ISSLHttpHooksAccess`
  - client verify path 还没有执行 online OCSP fetch
- GREEN 过程中出现的运行时崩溃不是生产代码行为漂移，而是测试桩本身遗漏了清理路径：
  - `CheckCertificateStatus(...)` 在第二次 `OCSP_cert_to_id(...)` 后一定会调用 `OCSP_CERTID_free(...)`
  - 测试把 `OCSP_cert_to_id` stub 成假指针，却没有接管 `OCSP_CERTID_free`
  - 结果是 cleanup 路径落回真实 `libcrypto`，对假 `CertID` 解引用后触发 `EAccessViolation`
- 最小正确修法因此是补齐测试桩而不是继续改生产实现：
  - `tests/test_freepascal_client_online_ocsp_runtime.pas` 新增 `OCSP_CERTID_free` 的 stub/保存/恢复
  - 生产改动仍然只收敛在 context hook access 和 client online OCSP helper
- 邻近 stapling / chain-trust / CertificateVerify 回归与 `182/182` compile gate 都为绿，说明这批修改仍然局限在 FreePascal client online OCSP fetch 这一条线上。

## 2026-04-10 Findings (Planning / FreePascal client OCSP stapling validation hardening)
- 当前 FreePascal client 的 OCSP stapling 主路径已经能做到 request/surface/required gate，但 verifier 里还残留一条更细的状态一致性缺口：
  - `TOCSPStaplingClient.ProcessStapledResponse(...)` 在 DER 解析、response-status、CertID、freshness 都通过后，会把结果标成 `ossVerified`
  - `TOCSPStaplingResult.IsValid` 另外又要求 `CertStatus = ocspGood`
  - `ValidateStaplingRequirement(...)` 却只看 `FLastResult.Status = ossVerified`
- 这会带来两个不一致风险：
  - optional surface 可能出现 `verified = False`，但状态文本仍显示为 `Verified`
  - required 模式可能把 `unknown/revoked` 这类 `non-good` stapled response 误当成可接受
- 这批最小正确动作因此不是继续扩 malformed parser、也不是跳去 online OCSP fetch，而是只收紧这一条 acceptance 语义：
  - `CertStatus <> ocspGood` => verification failure
  - optional 模式继续 surface raw bytes，但状态文本必须诚实
  - required 模式对这类响应 fail-closed

## 2026-04-10 Findings (Closeout / FreePascal client OCSP stapling validation hardening)
- 这批真正钉住的不是 “有没有 stapled response”，而是 “response 里真正的证书状态是否可接受”：
  - 之前 `ProcessStapledResponse(...)` 会把 `unknown/revoked` 也归到 `ossVerified`
  - 连接层 surface 因为 `IsValid=False` 会给出 `verified = False`
  - 但状态文本仍可能显示 `Verified`，而 required gate 也可能跟着误放行
- focused RED 已经证明这不是推测：
  - 用当前脚本证书现组的匹配 OCSP response，把 `CertStatus` 设成 `unknown`
  - 旧实现会失败在 `Unknown cert status must not surface as plain Verified`
- 最小正确修法只落在 `src/fafafa.ssl.ocsp.stapling.pas` 两个点：
  - freshness 之后新增 `SingleResp.CertStatus <> ocspGood` guard，统一收敛到 `ossVerificationFailed`
  - `ValidateStaplingRequirement(...)` 改为依赖 `FLastResult.IsValid`，不再只看 `Status = ossVerified`
- 这样 optional/required 语义终于对齐到同一个真实性源：
  - optional 模式仍然 surface raw bytes，但状态文本会诚实反映 `Unknown` / failure
  - required 模式会对 `non-good` stapled response fail-closed
- 邻近 peer-certificate / chain-trust / CertificateVerify 回归与 `182/182` compile gate 都为绿，说明这次 hardening 仍然收敛在 OCSP stapling acceptance 这一条线。

## 2026-04-10 Findings (Planning / FreePascal TLS 1.3 CertificateVerify focused gate promotion)
- 刚完成的 `CertificateVerify` 主线现在已经有完整 focused coverage：
  - `test_tls13_clienthello_parser`
  - `test_tls13_servercertverify`
  - `test_freepascal_client_certificateverify_runtime`
- 但当前 `run_freepascal_tls13_completeness_gate.sh` 还停在更早的 7-test inventory，只覆盖：
  - post-handshake / resumption / early-data
  - backend basic / capability cache
- 这会让一个真实问题继续存在：
  - 最近几批 `CertificateVerify` 主线虽然已经落地并通过 focused regressions
  - 但默认 focused gate 仍然不会守住这条路径
- 因此下一波最小正确动作不是再扩产品功能，而是把刚完成的主线提升进 gate：
  - 先用 contract test 把 dry-run inventory、fake `fpc` invocation count 和 summary row 收紧
  - 再最小修改 gate script 和 inventory 文案
  - 不把 peer-certificate / chain-trust / OCSP / CT 一起塞进这一波

## 2026-04-10 Findings (Closeout / FreePascal TLS 1.3 CertificateVerify focused gate promotion)
- 这批最重要的收口不是“再做一层手工回归”，而是把刚落地的 `CertificateVerify` 主线变成默认 focused verification surface：
  - `test_tls13_clienthello_parser`
  - `test_tls13_servercertverify`
  - `test_freepascal_client_certificateverify_runtime`
- RED 证据非常直接，说明这不是假缺口：
  - gate contract 一收紧就失败在 `dry-run output must mention tests/test_tls13_clienthello_parser.pas`
  - 当前 focused gate 确实还停留在旧的 7-test inventory
- 最小正确修法因此只落在验证面和说明面：
  - `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 先收紧 dry-run inventory、fake `fpc` invocation count 和 summary row
  - `scripts/run_freepascal_tls13_completeness_gate.sh` 把 3 个 `CertificateVerify` 测试组纳入 inventory
  - `.github/README.md` 同步修正 focused gate coverage 文案
- 真实 gate 跑完后的结果也证明这批范围选对了：
  - summary file 现在是 `10` passed / `0` failed
  - 新增的 3 条 `CertificateVerify` 测试都已经进入默认 focused gate
  - 全仓 `182/182` compile gate 继续为绿
- 这让最近几批 `CertificateVerify` 主线不再只停留在 focused regressions，而正式进入了默认主线验证面；后续如果继续推进 peer-certificate / chain-trust / OCSP / CT，可以再单独决定要不要继续扩 gate，而不是在这一波里顺手膨胀范围。

## 2026-04-10 Findings (Planning / FreePascal CertificateVerify suite-aware RSA SHA384 negotiation)
- 上一批补完 RSA SHA384 signer/verify 之后，默认协商面仍然还有两个真实缺口：
  - `ClientHello.signature_algorithms` 默认还只 advertize SHA256 schemes
  - server-side `TrySelectTLS13ServerCertificateVerifySchemeForKeyType(...)` 仍然不看已协商 cipher suite
- 这两个缺口叠在一起的结果是：
  - AES256/SHA384 runtime 虽然已经能处理 RSA SHA384
  - 但默认协商路径仍会继续走 SHA256，RSA SHA384 只能靠 forced test hook 覆盖
- 因此下一批最小正确动作不是单独补 advertisement，也不是单独改 selector，而是两者一起收口：
  - ClientHello 默认 advertize RSA SHA384 schemes
  - selector 按 suite 做 hash-family-aware 选择
- 继续保持窄边界：
  - 不补 `ecdsa_secp384r1_sha384`
  - 不改更大的 TLS 1.3 state machine
  - 只让现有协商路径能默认走到 RSA SHA384

## 2026-04-10 Findings (Closeout / FreePascal CertificateVerify suite-aware RSA SHA384 negotiation)
- 这批真正需要收口的不只是 advertisement，而是“advertisement + selection + real path 接线”三件事一起成立：
  - 只补 `ClientHello.signature_algorithms` 还不够，因为 server-side 默认 selector 仍会优先 SHA256
  - 只补 suite-aware selector 也不够，因为默认 client 之前根本不 advertize RSA SHA384 schemes
- 真实 server path 还有一个容易漏掉的落点：
  - `src/fafafa.ssl.freepascal.connection.pas` 在 selector 之后还有一层 `case LSignatureScheme of`
  - 如果不把 RSA SHA384 schemes 一起放行，就会出现“已经选中 SHA384，但发送路径仍报 unsupported”的假闭环
- 最小正确修法因此落在四个点，而且都保持了窄边界：
  - `ClientHello` 默认 advertize `rsa_pss_rsae_sha384` / `rsa_pkcs1_sha384` / `rsa_pss_pss_sha384`
  - `servercertverify` 新增 suite-aware RSA selector，按 SHA384/SHA256 suite 选择对应 hash family，并在缺项时回退到另一族
  - 真实 FreePascal server `CertificateVerify` path 改走 suite-aware selector
  - 连接层 signer allow-list 同步放行 RSA SHA384 schemes
- 邻近回归和 compile gate 都为绿，说明这批变更仍然收敛在 `CertificateVerify` 默认协商面：
  - peer-certificate surface 没被误伤
  - chain-trust runtime 没被误伤
  - 全仓 `182/182` 核心模块仍可编译

## 2026-04-09 Findings (Planning / FreePascal CertificateVerify RSA SHA384 schemes)
- 经过 SHA384 transcript parity 收口后，`CertificateVerify` 还剩一条更细的真实缺口：
  - transcript input 已经能接受 48-byte hash
  - 但 signature scheme 支持面仍只到 `*_SHA256`
- 这意味着当前 `TLS_AES_256_GCM_SHA384` path 仍依赖一个隐含前提：
  - server/client 都继续使用 RSA SHA256 `CertificateVerify`
  - 一旦对端只提供或使用 RSA `*_SHA384`，当前 pure Pascal path 仍会报 unsupported
- 代码结构也说明这批可以继续保持窄边界：
  - `wire` 层目前连 RSA SHA384 signature-scheme 常量都没公开
  - `servercertverify` 里只有 `SHA256_DIGESTINFO_PREFIX`、`MGF1_SHA256(...)`、RSA SHA256 PSS/PKCS1 helpers
  - ECDSA 仍只支持 P-256 / SHA256，因此这批最小正确动作是只补 RSA `*_SHA384`，不拉进 `secp384r1`

## 2026-04-09 Findings (Closeout / FreePascal CertificateVerify RSA SHA384 schemes)
- RED 证据证明这批缺口确实还停在 `servercertverify` 支持表，而不是更大的 TLS 1.3 状态机：
  - RSA key-type selector 在 SHA384-only offer 下直接失败
  - forced `rsa_pss_rsae_sha384` runtime path 直接报 `Unsupported signature scheme for pure FreePascal signer: 0x0805`
- 最小正确修复仍然只落在两层：
  - `fafafa.ssl.tls13.wire` 公开三个 RSA SHA384 scheme 常量和字符串映射
  - `fafafa.ssl.tls13.servercertverify` 补齐 SHA384 的 RSA-PSS / PKCS#1 v1.5 encode、sign、verify 和 selector/support table
- 连接层最终不需要额外改动，这说明上一批做完 transcript parity 之后，server/client `CertificateVerify` call path 已经足够解耦：
  - 新 scheme 一旦在 `servercertverify` 可建签和验签，runtime 就能直接吃到
  - 这也验证了本批没有必要把 scope 扩到 `freepascal.connection` 的更大状态机
- 邻近 runtime 回归、compile gate 和 diff hygiene 都为绿，说明这次扩展没有把 SHA384 support 误扩成 broader validation drift。

## 2026-04-09 Findings (Planning / FreePascal CertificateVerify SHA384 transcript parity)
- `ValidateServerCertificateVerify(...)` 现在最可疑的点不在 signature scheme parser，而在 transcript-input 构造：
  - 当前连接层把 transcript hash 固定算成 `SHA256`
  - 然后固定走 `BuildTLS13ServerCertificateVerifyInputSHA256(...)`
- 这和仓库当前已经闭环的 SHA384 suite 基础设施不一致：
  - keyschedule / finished / appschedule 都已经有 SHA384 parity
  - runtime tests 里也已经有 `HashTranscriptForSuite(...)`
  - 但 `CertificateVerify` 这条链还只被 `CHACHA20/SHA256` 覆盖
- 因而这批最小正确动作是：
  - 先在 shared helper 层写一个 48-byte transcript-hash RED
  - 再在 client runtime 层写一个 `TLS_AES_256_GCM_SHA384` full-handshake RED
  - 不扩到新的 signature scheme family

## 2026-04-09 Findings (Closeout / FreePascal CertificateVerify SHA384 transcript parity)
- fresh RED 证明根因确实在共享 transcript-input builder，而不是 signature parser：
  - 48-byte transcript hash 会被 `BuildTLS13ServerCertificateVerifyInputSHA256(...)` 以 `TLS13TranscriptHashSHA256` 参数错误拒掉
  - 同一缺口会直接外溢到 `TLS_AES_256_GCM_SHA384` 的真实 runtime 验证路径
- 最小修复保持在两层，并没有扩到新的 signature family：
  - shared builder 只放宽到接受 32/48-byte transcript hash
  - connection 层改为统一走 `HashTLS13TranscriptForSuite(...)`
- GREEN 过程中暴露出的本地实现错误也说明 suite 必须显式沿调用链传递：
  - `ValidateServerCertificateVerify(...)` 一开始引用了未定义的 `ACipherSuite`
  - 正确修法不是再猜一个新状态源，而是让 `ProcessEncryptedServerFlight(...)` 把实际协商的 suite 显式传入
- focused helper、AES256 runtime、peer-certificate surface、chain-trust runtime 和 compile gate 全部为绿，说明这次修复收敛在 `CertificateVerify` transcript parity，没有引入邻近回归。

## 2026-04-09 Findings (Closeout / README OCSP/CT guide entrypoints)
- 文档导航现在有三层高可见度入口已经基本收拢：
  - 仓库首页 `README.md`
  - docs 首页 `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
- 这批 closeout 的价值不在内容新增，而在减少入口分裂：
  - 用户不需要先知道 `CT_IMPLEMENTATION_GUIDE.md` 或 `OCSP_USAGE_GUIDE.md` 的文件名
  - 从首页就能直接发现这两条当前主路径
- `prettier` 也实际整理了两个 README 的表格格式，说明这批不仅补了入口，表格排版也保持了当前风格一致。

## 2026-04-09 Findings (Planning / README OCSP/CT guide entrypoints)
- 在收完 `DOCUMENTATION_INDEX.md` 之后，顶层入口层仍然还有一个明显缺口：
  - `README.md` 的文档表没有 OCSP / CT guide
  - `docs/README.md` 的功能表有 CT，但没有 OCSP
- 这会让用户在仓库首页和 docs 首页看到的导航继续不一致。
- 这批最小正确动作因此是：
  - `README.md` 补 `OCSP 指南` / `CT 指南`
  - `docs/README.md` 补 `OCSP` 行
  - 不改 guide 正文

## 2026-04-09 Findings (Closeout / Documentation index CT guide entry)
- `CT_IMPLEMENTATION_GUIDE.md` 之前最大的入口问题不是内容，而是它在 `DOCUMENTATION_INDEX.md` 里根本没被列出来。
- 这批 closeout 只补了一条导航入口，但价值很直接：
  - 在最常用的 “使用与集成” 列表里，OCSP / CT / TS 现在是并列可发现的
  - 不需要读者先经过其他 README 才能发现 CT guide
- 这类修正的关键不是“多写一个链接”，而是把已经对齐好的 CT runtime 叙事放回用户真正会进入的入口层。

## 2026-04-09 Findings (Planning / Documentation index CT guide entry)
- `DOCUMENTATION_INDEX.md` 当前在“使用与集成”这一组里已经列了：
  - OCSP guide
  - TS guide
- 但已经存在且更贴近当前 runtime truth 的 `CT_IMPLEMENTATION_GUIDE.md` 却没出现在同一入口层。
- 这不是实现缺口，而是导航缺口：
  - CT guide 已存在
  - docs/README 也已经有 CT 行
  - 但 Documentation Index 这一高可见度入口还没把它列出来
- 这批最小正确动作就是只补一条入口，不改 guide 正文。

## 2026-04-09 Findings (Closeout / API documentation resource link repair)
- `API_DOCUMENTATION.md` 的资源区问题是客观断链，不是表述分歧：
  - `docs/OCSP_PERFORMANCE_REPORT.md` 和 `docs/CT_IMPLEMENTATION_GUIDE.md` 都不存在
  - `examples/` 对 `docs/reference/API_DOCUMENTATION.md` 这个位置来说也是错的相对路径
- 这批 closeout 没继续改内容，只把入口修回现有文档树：
  - `OCSP 模块测试报告`
  - `CT 实现指南`
  - `examples/`
- 验证过程中第一次组合 `rg` 正则写错，随后切到固定字符串检查；这类链接修复更适合用简单、可解释的存在性验证，而不是把多个模式拼进一条脆弱正则里。

## 2026-04-09 Findings (Planning / API documentation resource link repair)
- `API_DOCUMENTATION.md` 的正文虽然已经逐步对齐，但资源区还保留了 3 条客观断链：
  - `docs/OCSP_PERFORMANCE_REPORT.md`
  - `docs/CT_IMPLEMENTATION_GUIDE.md`
  - `examples/`
- 这不是 wording 问题，而是实际导航失败：
  - 当前文件位于 `docs/reference/`
  - 旧相对路径会跳到不存在的位置
- 这批最小正确动作因此不是继续改内容，而是只修资源入口：
  - `OCSP 模块测试报告` -> `../test_reports/P2_OCSP_MODULE_REPORT.md`
  - `CT 实现指南` -> `../guides/CT_IMPLEMENTATION_GUIDE.md`
  - `示例代码` -> `../../examples/`

## 2026-04-09 Findings (Closeout / API documentation CT runtime boundary alignment)
- `API_DOCUMENTATION.md` 的 CT section 之前最大的 drift 不在代码缺失，而在叙事入口仍停在旧的低层 validator 路径：
  - 读者会先看到 `TSCTValidator`
  - 却看不到当前更直接的 FreePascal client/runtime surface
  - 也看不到 `required` 的真实生效边界
- 这批 closeout 把 CT 文档入口收回到当前 truth：
  - builder 方法补上 `WithCertificateTransparencyRequired(...)`
  - 连接级 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` surface 补进 API doc
  - `required` boundary 收紧到 `verify-peer` 的 non-resumed full-handshake
  - `verify-none` / resumed path 明确写成 inert
- 同时保持了克制：
  - 没把 CT 写成所有 backend 一致可用
  - 没把 OCSP-delivered SCT source / 自定义连接级 policy enforcement 写成已支持
  - 没碰实现

## 2026-04-09 Findings (Planning / API documentation CT runtime boundary alignment)
- `API_DOCUMENTATION.md` 当前的 CT section 还停在旧的低层 `TSCTValidator` 叙事：
  - 没有 builder 方法 `WithCertificateTransparencyRequired(...)`
  - 没有连接级 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` surface
  - 没有 `verify-peer` / `verify-none` / resumed 的 required boundary
- 这和当前 FreePascal CT runtime truth 已经脱节：
  - `WithCertificateTransparencyRequired(True)` 只在 `verify-peer` 的 non-resumed full-handshake path 上执行 gate
  - `verify-none` / resumed path 不触发 required enforcement
  - fail-closed 条件已经被测试锁定为 missing SCT / validation unavailable / policy failed
- 这批最小正确动作仍然是 docs-only：
  - 不动生产代码
  - 不改其他 guides/reference
  - 只把 `API_DOCUMENTATION.md` 写回当前已验证的 runtime truth

## 2026-04-09 Findings (Closeout / OCSP doc entrypoint and migration wording alignment)
- OCSP docs family 继续往下收时，最容易误导读者的已不再是 runtime contract 本身，而是高可见度入口和迁移示例：
  - `DOCUMENTATION_INDEX.md` 如果继续把 `OCSP_USAGE_GUIDE.md` 写成 OpenSSL-only，会让读者错过已经落地的 FreePascal runtime stapling path
  - `MIGRATION_GUIDE_V1.1.md` 如果继续把 `CertTransparencySupport` 当 deprecated 示例，会让 migration reader 误判当前 capability state
- 这批 closeout 只做了两个很窄但必要的修正：
  - OCSP guide 索引描述改成 “FreePascal stapling + OpenSSL 在线 OCSP”
  - migration capability 示例改成：
    - `OCSPStaplingSupport` 用 usable + non-deprecated 形状
    - `RenegotiationSupport` 作为 deprecated 示例
- 这样处理后，文档继续保持克制：
  - 没有把 OCSP/CT 写成 stable/full-complete
  - 没有碰实现
  - 只把 entrypoint/demo wording 收回当前 capability truth

## 2026-04-09 Findings (Planning / OCSP doc entrypoint and migration wording alignment)
- 继续沿 OCSP docs family 往下扫时，当前最直接的高可见度 drift 已经不在 runtime/reference 语义本身，而在入口描述和迁移示例：
  - `DOCUMENTATION_INDEX.md` 仍把 `OCSP_USAGE_GUIDE.md` 标成 “OpenSSL” 指南
  - `MIGRATION_GUIDE_V1.1.md` 仍把 `CertTransparencySupport` 当 deprecated 示例
- 这两处都已经落后于当前 capability truth：
  - FreePascal `OCSPStaplingSupport = sslSupportExperimental`
  - FreePascal `CertTransparencySupport = sslSupportExperimental`
  - `KnownIssues` 也已经收紧到剩余缺口，而不是 blanket gap / deprecated 叙事
- 因而这批最小正确动作仍然是 docs-only：
  - 不动生产代码
  - 不改更大范围 docs family
  - 只把入口描述和 migration 示例写回当前真相

## 2026-04-09 Findings (Closeout / API documentation OCSP required boundary alignment)
- `API_DOCUMENTATION.md` 现在不再把 `WithOCSPStaplingRequired(True)` 写成无条件 fail-closed contract，而是收紧到了当前 runtime truth：
  - `verify-peer` 的 non-resumed full-handshake path 上，missing/unaccepted stapled response 才会 fail-closed
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 也跳过 required enforcement
- 这批还顺手收掉了同页里的两个残余 drift：
  - builder 条目里的旧 wording
  - troubleshooting 段里的旧 wording
- 其余 reference 叙事保持稳定：
  - `WithOCSPStapling(True)` request path 仍保留
  - server-side 仍写成 backend-specific caveat
  - 没有把 OCSP 写成完整在线 revocation parity

## 2026-04-09 Findings (Planning / API documentation OCSP required boundary alignment)
- `API_DOCUMENTATION.md` 当前也还缺同一层 required boundary truth：
  - `WithOCSPStaplingRequired(True)` 仍写成无条件 fail-closed
  - 没有补上 `verify-none` 和 resumed 两条 guard
- 由于这是 reference 文档，这种 drift 比 guide 更容易被读者当成 contract。
- 这批最小正确动作仍然是只改一处文档：
  - 不动生产代码
  - 不改其他 reference/guides
  - 只把 fresh code/tests 已证明的边界补进去

## 2026-04-09 Findings (Closeout / Security best practices OCSP required boundary alignment)
- `security-best-practices.md` 里的 OCSP 建议现在不再把 `WithOCSPStaplingRequired(True)` 写成一个无条件 fail-closed 建议，而是对齐到了当前 runtime truth：
  - `verify-peer` 的 non-resumed full-handshake path 上，missing/unaccepted stapled response 才会 fail-closed
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 也跳过 required enforcement
- 这批没有重写更大的安全叙事：
  - 继续保留 `WithVerifyPeer`
  - 继续保留 `ISSLOCSPStapling`
  - 继续保留“stapled response path != 完整在线 revocation strategy”

## 2026-04-09 Findings (Planning / Security best practices OCSP required boundary alignment)
- `security-best-practices.md` 当前已经有比最初更好的 OCSP 建议，但 required-mode 的结论仍然偏宽：
  - 文档还没有写出 `verify-none` 不触发 required fail-closed
  - 也没有写出 resumed TLS 1.3 path 会跳过 required enforcement
- 这会让安全建议在高风险配置解释上再次超过 runtime truth。
- 这批最小正确动作仍然是只收一处文档：
  - 不动生产代码
  - 不顺手改 API/reference
  - 只把当前 fresh code/tests 已证明的边界补进去

## 2026-04-09 Findings (Closeout / OCSP usage guide required boundary alignment)
- `OCSP_USAGE_GUIDE.md` 现在不再把 `WithOCSPStaplingRequired(True)` 写成一个无条件 fail-closed 结论，而是收紧到了当前真实 runtime 边界：
  - `verify-peer` 的 non-resumed full-handshake path 上，missing/unaccepted stapled response 才会 fail-closed
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 也不会因为缺少新的 stapled response 被 `required` 阻断
- 这批没有重写更大的 OCSP 叙事：
  - `ISSLOCSPStapling` surface 说明保留
  - online AIA OCSP fetch / responder-signature / server-side issuance 的未覆盖范围保留
  - 没有顺手扩到 security/API 文档

## 2026-04-09 Findings (Planning / OCSP usage guide required boundary alignment)
- 当前 `OCSP_USAGE_GUIDE.md` 的 required-mode 结论还偏宽：
  - 它已经正确写了 missing/unaccepted stapled response 会 fail-closed
  - 但还没有把刚落地的两个 boundary 写出来：
    - `verify-none` 不触发 required fail-closed
    - resumed TLS 1.3 path 不因缺少新的 stapled response 被 `required` 阻断
- 这会让 guide 再次比 runtime truth 更“绝对”，尤其在 `WithOCSPStaplingRequired(True)` 这一段最容易误导。
- 这批最小正确动作是只收 guide wording：
  - 不改 OpenSSL 在线 OCSP 工作流
  - 不顺手改 `security-best-practices` 或 `API_DOCUMENTATION.md`
  - 只写当前已经有 fresh code/tests 支撑的 boundary

## 2026-04-09 Findings (Closeout / FreePascal client OCSP required verify-none boundary)
- fresh RED 证明 `ValidateClientOCSPStapling` 还存在一条独立于 resumed path 的边界缺口：
  - `verify-none + ssoRequireOCSPStapling` 仍会被 required-policy fail-closed
  - 这和同一条 client validation 链上其它 helper 的 `sslVerifyPeer` guard 形状不一致
- 最小正确修复不是改 request trigger，而是只把 enforcement boundary 和其它 validation helper 对齐：
  - `sslVerifyPeer` 未启用时，OCSP helper 直接跳过
  - `ProbeServerHello(...)` 仍按当前 options 发送 `status_request`
- 这次 closeout 也把范围继续锁住了：
  - 没有把 optional OCSP surface 改成 inert
  - 没有回退 verify-peer/full-handshake 的 required fail-closed 语义
  - 没有影响上一批 resumed boundary
- focused runtime contract、session resumption regression、compile gate 和 diff hygiene 都继续为绿，说明这次修复是局部收敛，不是行为漂移。

## 2026-04-09 Findings (Planning / FreePascal client OCSP required verify-none boundary)
- 当前 `ValidateClientOCSPStapling` 是整条 client validation 链里少见的一个例外：
  - trust helper 有 `sslVerifyPeer` guard
  - flags helper 有 `sslVerifyPeer` guard
  - certificate-verify helper 有 `sslVerifyPeer` guard
  - CT helper 也有 `sslVerifyPeer` guard
  - 但 OCSP helper 仍会在 `verify-none` 下继续执行 required-policy
- 同时 `ProbeServerHello(...)` 的 OCSP request plumbing 也和 CT 不同：
  - 当前 `status_request` 仍按 OCSP options 触发
  - 这批不打算顺手改 request trigger，只收 enforcement boundary
- 因而最小正确批次是：
  - 先把 `verify-none + ssoRequireOCSPStapling` 写成 focused contract
  - 如果 RED 成立，只在 `ValidateClientOCSPStapling` 上补一个 verify-mode guard
  - 不顺手改 optional surface/resumed/online fetch/verifier hardening

## 2026-04-09 Findings (Closeout / FreePascal client OCSP required resumption boundary)
- 这批 fresh RED 证明 `required OCSP` 在 resumed client path 上确实有一个真实缺口：
  - `ValidateClientOCSPStapling` 缺少 `FSessionReused` guard
  - resumed TLS 1.3 PSK flight 本来就不会重新发送 certificate / stapled response
  - 因而第二次连接启用 `verify-peer + ssoRequireOCSPStapling` 时会被误伤
- 最小正确修复不是扩 verifier，而是把边界和相邻 helper 对齐：
  - trust / flags / CT 都已经在 resumed path 上直接跳过
  - OCSP 现在也补上相同的 resumed skip
- 这批也继续锁住了范围：
  - 没有把 `required` 扩到 verify-none
  - 没有修改 full-handshake required 失败语义
  - 没有顺手扩 online OCSP fetch / validation hardening
- focused contract、全量 compile gate 和 diff hygiene 都为绿，说明这次修复是局部收敛，不是行为漂移。

## 2026-04-09 Findings (Planning / FreePascal client OCSP required resumption boundary)
- 当前 `ValidateClientOCSPStapling` 和相邻 helper 的 resumed 语义不一致：
  - trust / flags / CT 在 `FSessionReused=True` 时都会直接跳过
  - 但 OCSP helper 还会把“没有 stapled response”当成 required 失败
- 这和当前 TLS 1.3 resumed PSK harness 的 reality 不匹配：
  - resumed server flight 本来就不重发 certificate
  - 也不会重新带 stapled OCSP response
- 因而这批最小正确方案不是扩 OCSP verifier，而是：
  - 先把 resumed + required boundary 写成 focused contract
  - 如果 RED 成立，只补一个 resumed guard
  - 不顺手定义 verify-none 语义

## 2026-04-09 Findings (Closeout / API documentation OCSP runtime alignment)
- `API_DOCUMENTATION.md` 里的 OCSP 条目现在不再把 server-side stapling 写成通用自动行为，而是重新对齐到了当前更可信的 public contract：
  - `WithOCSPStapling(...)`
  - `WithOCSPStaplingRequired(...)`
  - `GetOCSP*` public methods
  - `ISSLOCSPStapling`
- 这批最关键的修正不是 API 名字，而是 contract 边界：
  - 当前最可验证的路径是 client-side stapled-response request/consume
  - required 模式会对缺失或未通过当前有界校验的 response fail-closed
  - server-side stapling issuance 必须写成 backend-specific caveat，不能再写成自动获取
- 同时 reference 也保留了兼容性：
  - 已经直接依赖 `Connection.GetOCSP*` 的用法没有被文档否定
  - 但新增了 `ISSLOCSPStapling` 的 capability-gated 访问示例，能更准确表达“可选 surface”
- 这样 reference 文档不再把 OCSP 范围写大，同时也避免把现有 public API 说窄。

## 2026-04-09 Findings (Planning / API documentation OCSP runtime alignment)
- `docs/reference/API_DOCUMENTATION.md` 当前对 OCSP 的描述比 guide 更容易误导，因为它看起来像 contract：
  - server config 仍写成“自动获取和附加响应”
  - 主节没有把 client-side stapled-response path 和 server/backend-specific caveat 分开
  - 最佳实践、排障和 builder 条目也还停在旧模型
- 当前真实 public surface 更细一些：
  - builder 层公开了 `WithOCSPStapling(...)` / `WithOCSPStaplingRequired(...)`
  - connection 层公开了 `GetOCSP*` 方法
  - 也存在 `ISSLOCSPStapling` 作为 capability-gated 访问面
  - 当前最有 fresh evidence 的路径是 client-side stapled-response request/consume + optional/required semantics
- 因而这批适合继续做一个窄 docs batch：
  - 只改 `API_DOCUMENTATION.md`
  - 去掉过头的 server-side 自动语义
  - 保留已有 public methods，但把说明收紧到真实范围

## 2026-04-09 Findings (Closeout / Security best practices OCSP runtime alignment)
- `security-best-practices` 里的 OCSP 建议现在不再把“开启 stapling”写成一个脱离上下文的 builder 片段，而是重新对齐到了当前真实的 client-side runtime 语义：
  - `WithVerifyPeer`
  - `WithOCSPStapling(...)`
  - `WithOCSPStaplingRequired(...)`
  - `ISSLOCSPStapling`
- 这批补齐的关键不是 API 名字本身，而是安全建议里的边界：
  - 如果只是希望消费 stapled response，可以启用 optional 模式
  - 如果是高风险 client path，可以提升到 required fail-closed
  - 但这仍然只是 stapled response path，不等于完整在线 revocation strategy
- 安全检查清单也同步从笼统的“启用了 OCSP Stapling”收紧成更可执行的建议：
  - 先要求 `VerifyPeer`
  - 再要求按风险决定是否使用 `required`
- 这样最佳实践文档不再隐含夸大 OCSP 覆盖面，也不会把当前 FreePascal runtime 能力写得过于含糊。

## 2026-04-09 Findings (Planning / Security best practices OCSP runtime alignment)
- 当前 `docs/guides/security-best-practices.md` 的 OCSP section 仍然停在一个过于宽泛的启用示例：
  - 只写了 `.WithOCSPStapling`
  - 没写 `WithVerifyPeer`
  - 没写 `WithOCSPStaplingRequired(...)`
  - 没写 `ISSLOCSPStapling`
- 这会把“启用 stapled response path”误导成“已经覆盖完整 OCSP/revocation strategy”，而当前真实能力边界并不是这样：
  - 已有 FreePascal client stapled-response runtime surface
  - 已有 required fail-closed gate
  - 但还没有 online AIA OCSP fetch parity / responder-signature parity
- 这批适合继续做一个窄 docs batch：
  - 只收 security guide
  - 让最佳实践建议和当前运行时 truth 对齐
  - 不扩到更大的 API/reference 文档集合

## 2026-04-09 Findings (Closeout / FreePascal client OCSP guide runtime required)
- OCSP guide 现在不再默认把“怎么用 OCSP”写成 OpenSSL 在线请求/验证视角，而是先给出当前最短的 FreePascal client runtime path：
  - `WithOCSPStapling(...)`
  - `WithOCSPStaplingRequired(...)`
  - `ISSLOCSPStapling`
- 这次补齐的关键信息不是 builder 名字本身，而是当前语义边界：
  - 可选模式下，缺少 stapled response 仍可连接，surface 返回空响应和 `verified = False`
  - required 模式下，缺少或未通过当前有界校验的 stapled response 会 fail-closed
  - 当前不要把这条路径理解成完整 revocation parity
- 同时 guide 也把 OpenSSL 在线 OCSP API 留在文档里，但降级成另一条明确分开的路径：
  - 需要主动发请求时仍用 `SendOCSPRequest` / `VerifyOCSPResponse`
  - 不再让读者误以为 FreePascal client runtime 只能走这条路
- 这样 guide 和最近两批 FreePascal OCSP 代码/测试状态重新对齐，降低了“文档仍停在旧能力模型”的误导风险。

## 2026-04-09 Findings (Planning / FreePascal client OCSP guide runtime required)
- 当前 `docs/guides/OCSP_USAGE_GUIDE.md` 仍然只以 OpenSSL backend 在线 OCSP API 为主视角：
  - `LoadOpenSSLOCSP`
  - `SendOCSPRequest`
  - `VerifyOCSPResponse`
  - `CheckCertificateStatus`
- 这和当前更直接可用的 FreePascal client runtime path 已经脱节：
  - `WithOCSPStapling(...)`
  - `WithOCSPStaplingRequired(...)`
  - `ISSLOCSPStapling`
- 这会让读者误以为 OCSP 只有“手工 OpenSSL OCSP 请求/验证”这一条用法，而看不到握手期 stapled response 的 surface 与 fail-closed 语义。
- 这批适合做一个窄 docs batch：
  - 只更新 OCSP guide
  - 只写已经被代码和测试证明的 runtime 行为
  - 不把 FreePascal OCSP 说成完整 revocation parity，也不扩到其他文档集合

## 2026-04-09 Findings (Closeout / FreePascal capability OCSP truth alignment)
- 这批最终确认：FreePascal backend 的 OCSP stapling 能力已经跨过了“无支持”的描述线，但还没有到 stable/full revocation parity：
  - `SupportsOCSPStapling` 现在应为 `True`
  - `OCSPStaplingSupport` 应为 `sslSupportExperimental`
  - `sslFeatOCSPStapling` 也应随之返回 `True`
- 更重要的是，`KnownIssues` 不能再把 OCSP stapling 整体列成 remaining gap，因为这会和当前运行时事实冲突：
  - client 已能发送 `status_request`
  - 已能通过 `ISSLOCSPStapling` surface raw response / verified bit / status text
  - `ssoRequireOCSPStapling` 已能对缺失或未通过当前有界校验的响应 fail-closed
- 因而新的正确表述是“只保留更窄的 OCSP 剩余缺口”：
  - online OCSP fetch parity
  - broader OCSP stapling validation hardening
  - broader certificate validation hardening
- 这样 capability truth 与前面已落地的 OCSP runtime closeout 重新对齐，同时没有把 OCSP stapling 夸大成完整 revocation parity。

## 2026-04-09 Findings (Planning / FreePascal capability OCSP truth alignment)
- 当前 FreePascal capability matrix 对 OCSP stapling 的 truth 已经落后于实现：
  - `SupportsOCSPStapling = False`
  - `OCSPStaplingSupport = sslSupportNone`
  - `KnownIssues` 仍写 `remaining gaps include OCSP stapling`
- 这与已经落地的 FreePascal client runtime 行为不一致：
  - 可以发送 `status_request`
  - 可以通过 `ISSLOCSPStapling` 暴露 raw stapled response / verified bit / status text
  - `ssoRequireOCSPStapling` 已能对缺失或未通过当前有界校验的响应 fail-closed
- 因而这批更适合做 capability truth 对齐，而不是继续扩实现：
  - capability 应提升到 `usable/experimental`
  - 但不能夸成 stable/full revocation parity
  - `KnownIssues` 也应只保留真实剩余缺口，而不是继续把 OCSP stapling 整体列为 pending

## 2026-04-09 Findings (Planning / FreePascal capability CT truth alignment)
- 当前 FreePascal capability matrix 对 CT 的 truth 已经明显落后于实现：
  - `SupportsCertificateTransparency = False`
  - `CertTransparencySupport = sslSupportNone`
  - `IsFeatureSupported` 也不返回 `sslFeatCertificateTransparency`
- 这与最近几批已经落地的 runtime 行为不一致：
  - client 可以请求并 surface SCT
  - 可以做 embedded SCT fallback
  - 可以 surface validation/policy truth
  - 还可以在 `required` 模式下 fail-closed
- 因而这批更适合做 capability truth 对齐，而不是继续扩实现：
  - capability 应提升到 `usable/experimental`
  - 但不能夸成 stable/full support
  - `KnownIssues` 也应只保留真实剩余缺口，而不是继续把 CT 整体列为 pending

## 2026-04-09 Findings (Closeout / FreePascal capability CT truth alignment)
- 这批最终确认：FreePascal backend 的 CT 能力已经跨过了“无支持”的描述线，但还没有到 stable/full-complete：
  - `SupportsCertificateTransparency` 现在应为 `True`
  - `CertTransparencySupport` 应为 `sslSupportExperimental`
  - `sslFeatCertificateTransparency` 也应随之返回 `True`
- 更重要的是，`KnownIssues` 不能再把 Certificate Transparency 整体列成 remaining gap，因为这会和当前运行时事实冲突：
  - client 已能请求并 surface SCT
  - 已有 embedded SCT fallback
  - 已有 validation surface
  - 已有 required fail-closed gate
- 因而新的正确表述是“只保留更窄的 CT 剩余缺口”：
  - OCSP-delivered SCT source parity
  - broader certificate validation hardening
- 这样 capability truth 与最近几批 CT closeout 重新对齐，同时没有把 CT 夸大成 stable/full support。

## 2026-04-09 Findings (Planning / FreePascal client CT guide runtime required)
- 当前 CT guide 仍然主要描述底层 `TSCTValidator` / `PX509` / `PSSL` 用法，没有覆盖最近几批已经落地的 FreePascal client runtime surface：
  - `ISSLCertificateTransparency`
  - `ISSLCertificateTransparencyValidation`
  - `TSSLContextBuilder.WithCertificateTransparencyRequired(...)`
- 这会让文档和真实使用路径脱节：
  - 使用者看不到连接对象上的 surface API
  - 也不知道 `required` 只在 verify-peer、non-resumed full-handshake 上 fail-closed
  - 更不知道 verify-none / resumed 两条边界是显式 guard，而不是未定义行为
- 这批适合做一个窄 docs batch：
  - 只更新 CT guide
  - 只写已经被代码和测试证明的行为
  - 不把文档扩成新的功能承诺

## 2026-04-09 Findings (Closeout / FreePascal client CT guide runtime required)
- CT guide 现在不再把“如何用 CT”默认写成底层 OpenSSL validator 视角，而是先给出当前最直接的 FreePascal client runtime path：
  - `ISSLCertificateTransparency`
  - `ISSLCertificateTransparencyValidation`
  - `WithCertificateTransparencyRequired(...)`
- 文档层这次补齐的最关键信息不是 API 名字本身，而是边界：
  - `required` 只在 verify-peer、non-resumed full-handshake 上 fail-closed
  - verify-none 时不会请求 SCT，也不会触发 `required`
  - resumed session 不会因为 resumed flight 缺少 certificate / SCT 而被 `required` 阻断
- 同时也把当前 runtime scope 写实了：
  - 支持 TLS `signed_certificate_timestamp` surface
  - 支持 embedded X.509 SCT fallback
  - 不把 OCSP-delivered SCT source / custom runtime policy 说成已支持
- 这让 guide 和最近几批代码/测试状态重新对齐，降低了“文档还停在旧能力模型”的误导风险。

## 2026-04-09 Findings (Planning / FreePascal client CT required boundary contracts)
- 上一批已经把 `CT required` 的 fail-closed 主路径收口，但 guard 本身还没有被独立写成契约：
  - `sslVerifyPeer` 关闭时，当前实现会在 `ValidateClientCertificateTransparency` 里直接 `Exit(True)`
  - `FSessionReused=True` 时，当前实现也会直接 `Exit(True)`
- 这两个 guard 都是重要的边界，因为它们决定了 `required` 不会越界到：
  - verify-none 的“只做 surface / 不做 enforcement”路径
  - TLS 1.3 resumed PSK 的“无证书 / 无 SCT server flight”路径
- 这批范围必须继续压窄：
  - 不扩到新的 CT source
  - 不做正向 policy-pass fixture
  - 不改 `ClientHello` 请求 SCT 的全局策略
- focused RED 可能直接为绿；如果发生这种情况，正确动作是把它记录成 contract-only closeout，而不是硬改生产代码。

## 2026-04-09 Findings (Closeout / FreePascal client CT required boundary contracts)
- 这批 fresh evidence 说明 `CT required` 的两个 runtime guard 已经是正确实现，不存在新的生产缺口：
  - `sslVerifyPeer` 关闭时，即使开启 `ssoRequireCertificateTransparency`，客户端也不会请求 SCT，握手继续成功
  - resumed PSK 路径上，即使第二次连接开启 verify-peer + `ssoRequireCertificateTransparency`，也不会因为 resumed flight 没有 certificate / SCT 而被阻断
- 因而这批正确的收口方式不是再改 `src/`，而是把 guard 补成显式 contract：
  - `tests/test_freepascal_client_ct_sct_surface.pas` 现在锁住 verify-none 的 inert 语义
  - `tests/test_freepascal_client_session_resumption.pas` 现在锁住 resumed-path skip 语义
- 这也进一步确认了上一批的范围判断没有漂移：
  - `required` enforcement 仍只属于 verify-peer、non-resumed full-handshake
  - 没有偷偷扩到 verify-none
  - 也没有误伤 TLS 1.3 session resumption
- 模块级编译门禁和 diff hygiene 继续为绿，说明这次新增契约没有引入额外耦合。

## 2026-04-09 Findings (Planning / FreePascal client CT required policy fail-closed)
- 接上上一批 CT validation surface closeout 后，当前 pure Pascal client 的 CT 路径只停在“可观测”，没有任何 enforcement：
  - 可以读到 raw SCT list
  - 可以读到 bounded validation result / policy truth
  - 但即使 policy failed，握手仍会继续放行
- 下一批最小连续 gap 不是扩 source parity，而是把已有 truth 接到 runtime policy：
  - `TSCTValidator.ValidateFromOCSP(...)` 仍是空实现
  - 继续做 OCSP-delivered SCT source 会把 batch 拉大
  - 反而 required gate 可以直接复用现有 cached SCT + validation state
- 这批配置层也需要一个独立的 context option，而不是把 enforcement 写死在连接里：
  - `TSSLOption` 目前没有 CT required 对应项
  - builder 也没有导入/导出/override/merge/clone 语义
  - 若只补运行时判断，会留下配置层和快照层缺口
- 最小正确方案应为：
  - 在 `TSSLOption` 追加 `ssoRequireCertificateTransparency` 或等价 option
  - builder 只新增一个 `required` 布尔面，不发明单独的 CT enabled flag
  - FreePascal client 在 verify-peer、非 resumed full-handshake 上追加 fail-closed gate
  - gate 条件明确收敛为：
    - missing SCT list
    - validation unavailable
    - default CT policy failed
- 这批继续**不**做：
  - OCSP-delivered SCT source
  - custom log store / custom CT policy 配置
  - 为了做正例去扩真实可通过 policy 的 SCT fixture
  - 修改 CT request 触发条件；它仍由 `sslVerifyPeer` 驱动

## 2026-04-09 Findings (Closeout / FreePascal client CT required policy fail-closed)
- 这批把 CT 从“只可观测”推进到了“可 fail-closed enforce”，但仍然保持了很窄的产品边界：
  - `ssoRequireCertificateTransparency` 是一个单独的 required-only option
  - 没有引入单独的 CT enabled flag
  - 也没有修改 ClientHello 请求 SCT 的触发条件；请求仍跟随 `sslVerifyPeer`
- builder 这次补齐的是配置生命周期一致性，而不是只给 runtime 塞一个裸 option：
  - 新增 `WithCertificateTransparencyRequired(...)`
  - JSON / INI / override / clone / merge / build 都会保留 required state
  - `Merge(...)` 末尾现在显式做 advanced-option sync，因此 source JSON 里的 CT required truth 能稳定映射回 exported option set
- FreePascal runtime gate 的位置和语义都刻意压窄：
  - 执行时机是 trust -> flags -> OCSP -> CT required -> `SendClientFinished`
  - 只在 verify-peer、非 resumed client full-handshake 上生效
  - fail-closed 条件明确只有三个：
    - missing SCT list
    - validation result unavailable
    - CT policy failed
- 这批验证也说明无需再为“required”去扩正向 fixture：
  - `required + missing SCT` 已能稳定 fail-closed
  - `required + dummy SCT list` 在 CT validation 可用时会走到 policy failed
  - 若 validation 不可用，同一路径也会因为 unavailable 而 fail-closed
  - 因而 required 的负向闭环已经完整，不需要本批再引入可通过 policy 的真实 SCT 样本
- focused builder 契约、runtime contract、相邻 FreePascal regressions、模块级编译门禁和 diff hygiene 都为绿，说明这次 enforcement 没把 peer-cert / CertificateVerify / trust / OCSP / resumption / early-data 主线带偏。

## 2026-04-09 Findings (Planning / FreePascal client CT validation surface)
- 接上 embedded SCT fallback closeout 后，当前 FreePascal client 虽然已经能拿到 TLS/embedded 两类 raw SCT list，但仍停在 surface 层：
  - 连接层只缓存 raw SCT bytes / count / source status
  - 没有任何 cryptographic validation result
  - 也没有默认 policy 是否满足的 truth
- 下一批若直接做 `OCSP` delivered SCT source，范围会明显膨胀：
  - `TSCTValidator.ValidateFromOCSP(...)` 目前还是空实现
  - 这意味着 OCSP SCT parity 不是一个更小的 batch
- 相反，当前树上已经具备一条更短的闭环：
  - `TFreePascalConnection` 已缓存 raw `SignedCertificateTimestampList`
  - peer leaf/issuer 证书也已经在连接层可用
  - `fafafa.ssl.ct.sct` 已有 `TSCTValidator.ValidateSCTList(...)` 与 `CheckPolicy(...)`
  - `fafafa.ssl.openssl.api.ct` 已有 `o2i_SCT_LIST` / `SCT_LIST_validate` / 状态常量
- 因而这批最小正确方案是：
  - 新增一个 CT validation optional interface
  - 让 FreePascal connection 在 raw SCT list 存在时，lazy 桥接 OpenSSL CT validator
  - 暴露“是否拿到 validation result / 默认 policy 是否满足 / validation status string”
  - validation helper 本身失败时只 surface `unavailable`，不把握手改成 fail-closed
- 这批继续**不**做：
  - context-level CT policy 配置位
  - validation failure -> handshake failure
  - OCSP-delivered SCT source
  - wider CT log fetching / custom log-store wiring

## 2026-04-09 Findings (Closeout / FreePascal client CT validation surface)
- 这批最终把 CT 从“只有 raw SCT surface”推进到了“有 bounded validation/policy truth”，但仍然没有把 CT 说大成 fail-closed enforcement：
  - `ISSLCertificateTransparencyValidation` 现在暴露 `HasCertificateTransparencyValidationResult`
  - 同时暴露默认 policy 是否满足
  - 以及一条可读的 validation status string
  - 握手是否放行仍不受 CT validation 结果影响
- base / connection 抽象层这次补的是稳定的可选 surface，而不是 FreePascal 私有旁路：
  - `src/fafafa.ssl.base.pas` 新增 optional interface
  - `src/fafafa.ssl.connection.base.pas` 默认回退到 `False / False / Not Supported`
  - 所以后续其他 backend 即使暂不实现，也不会破坏接口兼容性
- 真正的实现拐点不在接口层，而在 OpenSSL CT validator 的接线方式：
  - 最初按计划尝试 `o2i_SCT_LIST(...) + TSCTValidator.ValidateSCTList(...)`
  - 运行期先拿到 `Access violation`
  - 之后即使调整外层长度语义，仍然得到 `Failed to decode SignedCertificateTimestampList with OpenSSL`
  - 说明“整表解码 + validator”在当前 raw SCT surface 上并不稳
- 因此最终实现改成更窄、更可控的 per-SCT 路径：
  - 从已缓存的 raw `SignedCertificateTimestampList` 逐个拆出 serialized SCT
  - 每个 SCT 单独走 `o2i_SCT`
  - 再逐个做 `SCT_validate`
  - 最后用 `SCT_get_validation_status` 聚合成 surface status，并按默认 options 语义本地计算 policy satisfied truth
- 这个切换有两个直接收益：
  - 避开了 `o2i_SCT_LIST` 整体编码语义与当前 raw list framing 的耦合点
  - 保住了本批最关键的契约：即使 validation path 不可用，也只 surface `Validation unavailable: ...`，不把握手打红
- 当前 fresh runtime 也说明这条边界是稳的：
  - no-SCT 场景会给出 `Has...Result = False` 与 `Not Attempted / No SCT`
  - dummy TLS SCT 场景在 OpenSSL CT 模块可用时会给出 validation result，但默认 policy 不满足
  - 当前 dummy 样本下聚合状态是 `Policy failed (0/2 valid SCTs; statuses=Not Set, Not Set)`，测试契约接受这一层“policy failed / unknown-ish”语义
- focused contract、相邻 FreePascal regressions、模块级编译门禁与 diff hygiene 都为绿，说明这次 CT validation surface 没有把 peer-cert / trust / OCSP / resumption / early-data 主线带偏。

## 2026-04-09 Findings (Planning / FreePascal client embedded SCT fallback)
- 上一批 CT/SCT closeout 后，当前纯 Pascal client 的 CT surface 仍然只覆盖 TLS `CertificateEntry.extensions` 里的 `signed_certificate_timestamp` source：
  - `TTLS13ServerCertificateInfo` 会缓存 leaf TLS SCT list
  - `TFreePascalConnection.TryCachePeerCertificatesFromHandshake(...)` 也只消费这一份 TLS parser 输出
  - 因此只要服务端把 SCT 内嵌在 leaf X.509 扩展里，当前 surface 仍会错误表现成 `No SCT List`
- 纯 Pascal 侧其实已经有足够的 fallback building blocks，不必为了这个 batch 扩证书生成 API：
  - `TX509Certificate.Extensions` 会保留未知扩展的原始 `Value: TBytes`
  - OID `1.3.6.1.4.1.11129.2.4.2` 在 OpenSSL 视角下就是 `CT Precertificate SCTs`
  - 用离线生成的静态 CA-signed fixture 校验后，仓库自己的 X.509 parser 读到的扩展值正好是原始 `SignedCertificateTimestampList` bytes
- 因此这批最小正确方案不是改 `TCertificateUtils` / builder，而是：
  - 测试侧引入静态 embedded SCT cert fixture
  - 生产侧在连接层保留 TLS 优先级，只在 TLS SCT 缺失时读取 leaf X.509 embedded SCT
  - 复用同一套 SCT list 边界 parser
- malformed embedded SCT 也应与 malformed TLS SCT 保持同一 fail-closed 原则：
  - 如果 leaf 证书声明了 embedded SCT，但 list framing 无效
  - 这不是“缺少可选信息”，而是“收到损坏的 CT material”
  - 因而应该让连接在 peer-certificate cache / validation 路径失败，而不是静默忽略
- 为了控制 scope，这批继续**不**做：
  - CT policy / cryptographic verification
  - capability wording 更新
  - OCSP-delivered SCT source
  - 通用 arbitrary/custom extension 证书生成 API

## 2026-04-09 Findings (Closeout / FreePascal client embedded SCT fallback)
- FreePascal client 的 CT surface 现在不再把 SCT source 限死在 TLS `CertificateEntry.extensions`：
  - 若 leaf `CertificateEntry` 已带 TLS `signed_certificate_timestamp`，仍然优先使用 TLS source
  - 仅当 TLS SCT 缺失且当前路径确实请求了 CT surface 时，才回退到 leaf X.509 embedded SCT OID
  - 因而 source precedence 保持稳定，没有把上一批 TLS-only 语义改成“谁后解析到就覆盖谁”
- 这批真正复用起来的关键 building block 是上一批已经存在的 SCT list framing parser，而不是再写第二套 embedded 解析逻辑：
  - `TryParseSignedCertificateTimestampList(...)` 现在从 `servercertificate` 暴露成通用 helper
  - TLS SCT 和 embedded SCT 都走同一套 length/item 边界检查
  - 所以 malformed embedded SCT 也会和 malformed TLS SCT 一样 fail-closed
- 连接层的改动刻意收在 cache/materialize 阶段：
  - `TryCachePeerCertificatesFromHandshake(...)` 先照旧缓存 TLS SCT / peer certificate chain
  - peer leaf materialize 完成后，若仍无 SCT list，再读取 `TX509Certificate.Extensions`
  - 找到 OID `1.3.6.1.4.1.11129.2.4.2` 后直接消费原始 `Value: TBytes`
  - 成功时 surface 状态改为 `Received from embedded X.509 extension (%d SCTs)`
- fixture 策略这次也验证了一个重要事实：
  - 不需要扩 `TCertificateUtils` / builder 才能覆盖 embedded SCT fallback
  - 静态 CA-signed PEM fixture + 现有 scripted handshake harness 已足够表达 valid / malformed contract
  - 仓库自己的 X.509 parser 读到的未知扩展值，确实就是我们需要的 raw SCT list bytes
- fresh focused contract、相邻 peer-cert / CertificateVerify / chain-trust / OCSP / resumption / early-data 回归、模块级编译门禁与 targeted diff hygiene 都继续为绿，说明这次 fallback 接线没有把前几批已经收紧的 validation/runtime path 带偏。

## 2026-04-09 Findings (Planning / FreePascal client CT SCT surface)
- 这次把 `Certificate Transparency` backlog 重新落回了当前 pure Pascal client 的真实代码面，而不是照着独立 CT 模块乐观接线：
  - `fafafa.ssl.ct.sct` / `fafafa.ssl.ct.log` / `fafafa.ssl.openssl.api.ct` 全都建立在 OpenSSL `PSSL` / `PX509` / `PSCT_LIST` 上
  - 当前 FreePascal client handshake path 只有 `ISSLCertificate` / `TX509Certificate` / TLS byte parser，没有可直接复用的 OpenSSL CT eval context
  - 所以这批如果硬做 policy verification，会变成越界的大改
- base / connection 抽象层当前也没有任何 CT surface 或开关：
  - `ISSLConnection` / `TBaseSSLConnection` 已有 `ISSLOCSPStapling` 模式
  - 但没有 `ISSLCertificateTransparency`、没有 SCT list getter、也没有 CT required-policy option
  - 这意味着最小正确 batch 更适合先补可观测的 request / parser / surface，而不是发明 fail-closed 语义
- pure Pascal 侧可复用的最直接 building blocks 已经存在：
  - `fafafa.ssl.tls13.clienthello` 已经有 optional extension plumbing 模式（OCSP `status_request`）
  - `fafafa.ssl.tls13.servercertificate` 已经能对 leaf `CertificateEntry.extensions` 做 bounded parse 并缓存 OCSP bytes
  - `TFreePascalConnection` 已经有连接级 surface/cache 模式，可直接平移到 CT/SCT
- 因此这批 scope 收紧为：
  - client `ClientHello` 请求空 `signed_certificate_timestamp` extension
  - TLS `CertificateEntry` 的 SCT list framing parser
  - connection-level raw SCT list / count / status surface
  - malformed SCT list fail-closed
- 这批刻意**不**包含：
  - OpenSSL `TSCTValidator` 接线
  - CT policy / cryptographic verification
  - FreePascal backend capability wording 更新
  - X.509 embedded SCT extension / OCSP SCT source
- 我额外核对了 pure Pascal X.509 parser：
  - `TX509Certificate.Extensions` 已经能拿到原始 extension OID/value
  - 从技术上说后续可以做 embedded SCT extension fallback
  - 但当前树上没有现成 fixture，测试生成也不是零成本，因此不把它塞进这一批，避免无测试实现。

## 2026-04-09 Findings (Closeout / FreePascal client CT SCT surface)
- FreePascal client 现在已经把 TLS SCT list 接到了真实 handshake/runtime surface，但这批仍然保持了刻意收窄的边界：
  - `sslVerifyPeer` client path 会在 `ClientHello` 里主动请求空 `signed_certificate_timestamp`
  - TLS 1.3 `CertificateEntry.extensions` 会对 leaf `signed_certificate_timestamp` 做 bounded framing parse
  - 连接对象会暴露 raw SCT list bytes、count 和 status text
  - malformed SCT list 会在 `Certificate` 解析阶段直接 fail-closed
- 这批最关键的实现点不是“多了一个 getter”，而是把 CT state 与 peer-certificate cache 生命周期绑定：
  - `TFreePascalConnection.ClearPeerCertificateCache` 现在会同步清空 CT state
  - `TryCachePeerCertificatesFromHandshake(...)` 会在缓存证书时一起缓存 SCT list / count / status
  - 因此不会把上一次连接的 SCT 状态泄漏到下一次握手
- `GetCertificateTransparencyEnabled` 的语义也刻意对齐“surface truth”而不是“request truth”：
  - 只有真的收到可解析的 SCT list bytes 才返回 enabled
  - 只是请求了 extension 但服务端没回时，surface 仍表现为 disabled + `No SCT List`
- 当前状态字符串语义已经足够明确，且与测试契约一致：
  - reset/default => `Not Requested`
  - requested but missing => `No SCT List`
  - present => `Received from TLS extension (%d SCTs)`
- 这批没有越界到更大的 CT 叙事：
  - 没有接 OpenSSL `TSCTValidator`
  - 没有做 CT policy / cryptographic verification
  - 没有扩到 X.509 embedded SCT / OCSP SCT source
  - 因此 capability wording 仍应保持“bounded surface / parser”级别，而不是“完整 CT 支持”
- fresh focused contract、相邻 peer-cert / CertificateVerify / chain-trust / OCSP / resumption / early-data 回归、`compile_all_modules.py` 和 diff hygiene 都继续为绿，说明这次 ClientHello + parser + connection surface 接线没有把已有 TLS 1.3 主线带偏。

## 2026-04-09 Findings (Planning / FreePascal client CertificateVerify signature verification)
- 接上 OCSP stapling closeout 后，FreePascal client validation hardening 主线上的下一条 runtime gap 继续集中在“消息存在”与“消息可信”之间的断层：
  - full handshake 现在已经要求必须收到 `CertificateVerify`
  - peer certificate / hostname / expiry / chain trust / stapled OCSP 也都已经有各自的 bounded runtime policy
  - 但 `CertificateVerify` 自身仍只被当作 transcript bytes 追加，没有任何签名验真
- 这条缺口比 CT 更适合作为下一批最小 batch：
  - `fafafa.ssl.tls13.servercertverify` 已有纯 Pascal signer / scheme-selection / RSA-PSS encoding building blocks
  - `fafafa.ssl.tls13.ecdsa` 已有 P-256 点运算与 signer 数学
  - `TX509Certificate.PublicKeyInfo` 已能提供 RSA modulus/exponent 与 ECDSA 公钥点
  - 现有 scripted full-handshake tests 已能稳定构造可控 `CertificateVerify`
- 因此这批应保持窄边界：
  - 只补服务端 `CertificateVerify` parser + public-key verify + client handshake 接线
  - 只覆盖当前已有 SHA-256 / RSA / ECDSA P-256 路径
  - 不顺手扩到 CT / SCT policy
  - 不顺手扩到更完整的 revocation / CT / broader curve family

## 2026-04-09 Findings (Closeout / FreePascal client CertificateVerify signature verification)
- FreePascal client full-handshake path 现在不再把 `CertificateVerify` 只当作“存在即可”的 transcript bytes：
  - `fafafa.ssl.tls13.servercertverify` 新增了 bounded parser
  - 同单元新增了基于 `TX509PublicKeyInfo` 的 RSA public-key verify
  - `fafafa.ssl.tls13.ecdsa` 新增了 P-256 DER signature verify
  - `TFreePascalConnection.ProcessEncryptedServerFlight(...)` 现在会先验签，再把 `CertificateVerify` 追加进 transcript
- 这次真正补齐的不是“更大一层证书验证”，而是一个非常具体的 fail-open 缺口：
  - tampered RSA `CertificateVerify` 先前会被错误接受
  - mismatched signature scheme 先前也只会被当作普通 handshake bytes 吞进 transcript
  - 现在两者都会在 verify-required 的 full handshake client path 上 fail-closed
- 这批 bounded verifier 的支持范围需要明确写清：
  - RSA `rsa_pss_rsae_sha256`
  - RSA `rsa_pss_pss_sha256`
  - RSA `rsa_pkcs1_sha256`
  - ECDSA `ecdsa_secp256r1_sha256`
- 一个关键实现点是 transcript 顺序修正，而不是单纯“补一个 helper”：
  - verify input 必须基于 `CertificateVerify` 之前的 transcript
  - 因此 client 端不能再像之前那样先 append 再继续握手
  - 现在的顺序已经与 TLS 1.3 验签语义对齐
- 这批也刻意没有越界：
  - 不补 CT / SCT policy
  - 不补 OCSP online fetch 或更强 revocation parity
  - 不补更广 curve/hash/signature families
  - resumed PSK path 与 `sslVerifyPeer` 关闭路径继续保持原有边界
- fresh focused contract、相邻 FreePascal regressions、`compile_all_modules.py` 和 diff hygiene 都继续为绿，说明这次新增的 parser / verify 接线没有把前面几批已经收紧的 trust、hostname/expiry、OCSP、resumption、early-data 路径带偏。

## 2026-04-09 Findings (Planning / FreePascal client OCSP stapling surface + required policy)
- 接上 chain trust runtime parity closeout 后，FreePascal client validation hardening 主线上的下一条 runtime gap 已经足够集中，可以拆成一个诚实的小批次，而不是冒进宣称“完整 OCSP parity”：
  - ClientHello 还不会发送 `status_request`
  - TLS 1.3 `Certificate` parser 仍会丢掉 `CertificateEntry.extensions`
  - `ISSLOCSPStapling` 在 FreePascal backend 仍回退到 base stub
- 这批应当刻意收紧 scope：
  - 只补 client-side `status_request` request
  - 只补 TLS 1.3 leaf `CertificateEntry` 上 stapled OCSP response 的 parse/surface
  - 只补 `ssoRequireOCSPStapling` 的 fail-closed policy
  - 不顺手扩到 `sslCertVerifyCheckOCSP` online fetch
  - 不顺手扩到 responder signature / issuer chain cryptographic verification
  - 不顺手改 capability wording，避免把当前 parser-bounded 行为说大
- 纯 Pascal 现有 building blocks 允许这批做成“有界但真实”的 runtime contract：
  - `TOCSPStaplingClient.ProcessStapledResponse(...)` 已经提供 DER parse、response status 检查、CertID 匹配、freshness 检查和 cache write
  - `ValidateStaplingRequirement(...)` 已能表达 required-policy 的最终门
  - 但它目前看起来仍不是 OpenSSL 那种完整 responder-signature / trust-chain cryptographic verification
- 我额外核对了 TLS 1.3 `status_request` 在 `CertificateEntry` 里的 framing，避免实现时靠记忆拍格式：
  - RFC 8446 Section 4.4.2.1 明确：TLS 1.3 server 的 OCSP 信息放在 `CertificateEntry` extension 里，而不是单独 `CertificateStatus` 握手消息
  - 同一节又明确：该 extension body 必须是 RFC 6066 定义的 `CertificateStatus` 结构
  - 这意味着 parser 至少要识别：
    - `status_type = ocsp`
    - `OCSPResponse<1..2^24-1>` 的长度与 payload 边界
- 现有测试模板已经比较清楚：
  - FreePascal scripted full-handshake 模板来自：
    - `tests/test_freepascal_client_chain_trust_runtime.pas`
    - `tests/test_freepascal_client_peer_certificate_surface.pas`
  - required stapling fail-closed 语义模板来自：
    - `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - 纯 Pascal OCSP fixture 的确定性失败样例来自：
    - `tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der`
    - `tests/unit/test_ocsp_stapling.pas`

## 2026-04-09 Findings (Closeout / FreePascal client OCSP stapling surface + required policy)
- FreePascal client 现在终于把 OCSP stapling 从“配置位”接到了真实 runtime surface，但仍然保持了诚实边界：
  - `ProbeServerHello(...)` 会在 `ssoEnableOCSPStapling` 下把 `status_request` 发进 TLS 1.3 `ClientHello`
  - TLS 1.3 `Certificate` parser 不再丢弃 leaf `CertificateEntry.extensions`，而是能提取 stapled OCSP response
  - `TFreePascalConnection` 会缓存 raw response、verified bit、status text，并通过 `ISSLOCSPStapling` 暴露出去
- 这批 required-policy 也已经真正落到 full-handshake client path：
  - `ssoRequireOCSPStapling` + missing stapled response => fail-closed
  - `ssoRequireOCSPStapling` + stapled response present but not accepted by the current bounded verifier => fail-closed
  - 当前错误边界继续收敛在 certificate failure 语义，没有冒进发明新的 public contract
- `DoGetOCSPStaplingEnabled` 的语义这次也刻意对齐“surface truth”而不是“config truth”：
  - 只有当连接上真的拿到了 stapled OCSP response 时才返回 enabled
  - 因而不会把“请求了 stapling 但服务端没回”误报成已启用
- verification 边界必须明确写清楚，避免把这批说大成“完整 revocation parity”：
  - 复用的 `TOCSPStaplingClient` 当前提供的是：
    - DER parse
    - response status 检查
    - CertID matching
    - freshness 检查
  - 本批明确没有补：
    - `sslCertVerifyCheckOCSP` online fetch parity
    - responder signature cryptographic verification
    - responder/issuer trust-chain cryptographic verification
    - server-side stapling issuance
    - capability wording 更新
- fresh focused contract、相邻 FreePascal regressions、模块级编译门禁与 targeted diff hygiene 都继续为绿，说明这次 OCSP stapling 收口没有把前几批已收紧的 peer-certificate / certificate-flight / trust / resumption / early-data 路径带偏：
  - new OCSP stapling runtime contract => PASS
  - peer-certificate surface regression => PASS
  - certificate-flight requirement regression => PASS
  - session resumption regression => PASS
  - early-data regression => PASS
  - chain trust runtime regression => PASS
  - cert verify flags runtime regression => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`
  - `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-stapling-surface-required-policy.md src/fafafa.ssl.tls13.wire.pas src/fafafa.ssl.tls13.clienthello.pas src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-09 Findings (Planning / FreePascal client chain trust runtime parity)
- 接上 `hostname/expiry` parity closeout 后，FreePascal client 在 validation hardening 主线上的下一条 runtime gap 已经非常集中：
  - `SetCertificateStore(...)` / `LoadCAFile(...)` / `LoadCAPath(...)` 都已经存在
  - peer leaf / chain 也已经能从 full handshake materialize 出来
  - 但 `TFreePascalConnection` 从未消费这些 trust material，所以 `sslVerifyPeer` 仍不会对 untrusted roots fail-closed
- 继续沿现有 building blocks 扫描后，最小正确方案不是再造一套链验证：
  - 仓库里已经有 `TSSLCertificateChainVerifier`
  - 它能把 trusted roots 与 peer intermediates 分层处理
  - 这正好适合当前 FreePascal client full-handshake path
- 这批仍然需要刻意收紧 scope：
  - 只补 chain trust runtime parity
  - 只覆盖 non-resumed full-handshake client path
  - 不顺手扩到 OCSP / CT / CRL online fetch
  - 不顺手扩到 resumed PSK path 的 peer-certificate trust persistence
  - 不顺手补 client-side `CertificateVerify` signature verification
- 一个额外实现约束也已经确认：
  - 不能把 peer-provided intermediates 直接混进 trusted roots store
  - 因为当前 store `Contains(...)` 语义会把被加入的证书当成信任锚
  - 所以这批必须保留 trusted roots 与 peer intermediates 的分层，而不是“全塞进一个 store”

## 2026-04-09 Findings (Closeout / FreePascal client chain trust runtime parity)
- FreePascal client full-handshake path 现在终于把 trust material 从 storage-only 接到了 runtime verification：
  - `TFreePascalContext` 通过新的 FreePascal 私有 trust-store accessor 构造 verification store
  - accessor 会先 clone `SetCertificateStore(...)` 提供的 store 内容
  - 再叠加 `LoadCAFile(...)` / `LoadCAPath(...)` 配置出来的 roots
  - 若最终没有任何 trust material，则显式返回空结果，让连接路径 fail-closed
- 连接层复用了现有 `TSSLCertificateChainVerifier`，没有再造第二套链验证逻辑：
  - trusted roots 来自 context trust store
  - peer chain 的 `[1..]` 只进入 intermediate store
  - 这样避免把 peer-provided intermediates 错当成 trust anchors
  - verifier failure 被有界映射为 `sslErrCertificateUntrusted`
- runtime policy 也保持了此前批次建立的层次顺序：
  - 先做 chain trust verification
  - 再继续走已有的 hostname / expiry helper
  - 仍然只覆盖 `sslVerifyPeer` 且 `not FSessionReused` 的 client full-handshake path
- focused contract 证明这批最小 parity 已经落地：
  - CA-signed leaf + no trust material => fail-closed
  - `LoadCAFile(...)` / `LoadCAPath(...)` / `SetCertificateStore(...)` => each can unblock the same handshake
  - self-signed leaf 仍默认失败
  - `sslCertVerifyAllowSelfSigned` 只对 self-signed leaf 做 bounded allow
- 相邻 hostname/expiry contract 也被重新隔离回了自己的语义边界：
  - `tests/test_freepascal_client_cert_verify_flags_runtime.pas` 现在显式加载 `tests/certificate/test_certs/ca_cert.pem`
  - 因而该测试继续只覆盖 hostname / expiry，而不再隐式承担 chain trust semantics
- fresh focused contract、相邻 regressions 与模块级编译门禁都继续为绿，说明这次 trust hardening 没把 peer-certificate surface、certificate-flight floor、resumption、early-data 家族带偏：
  - new chain-trust contract => PASS
  - verify-flags regression => PASS
  - peer-certificate surface regression => PASS
  - certificate-flight requirement regression => PASS
  - session resumption regression => PASS
  - early-data regression => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`
- targeted diff hygiene 也为绿，说明这次最终落盘的 plan/source/test/ledger 集合没有引入格式层噪音：
  - `git diff --check -- docs/plans/2026-04-09-freepascal-client-chain-trust-runtime-parity.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_chain_trust_runtime.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-08 Findings (Planning / FreePascal client cert verify flags hostname/expiry parity)
- 继续沿 validation hardening 主线推进时，当前 FreePascal client 的下一条 runtime gap 已经被压缩到 verify flags parity 本身：
  - `TFreePascalContext.SetCertVerifyFlags(...)` / `GetCertVerifyFlags(...)` 已经可用
  - peer certificate surface 也已经在 full handshake 上可见
  - `ISSLCertificate.VerifyHostname(...)` 与 `IsExpired` 也都已经存在
  - 但 `TFreePascalConnection.DoConnect` 仍没有把这些 pieces 接成 runtime policy
- 对比现有 backend 边界，OpenSSL / WinSSL 已经会在 client verify path 里按 flags 处理 hostname / expiry；FreePascal 现在仍停在 storage-only。
- 这批应当继续保持克制：
  - 只补 hostname / expiry runtime parity
  - 只覆盖 non-resumed full-handshake client path
  - 不顺手扩到 CA store / chain trust verification
  - 不顺手扩到 OCSP / CT
  - 不顺手把 resumed PSK session persistence for peer certificate 一起做掉

## 2026-04-08 Findings (Closeout / FreePascal client cert verify flags hostname/expiry parity)
- FreePascal client full-handshake path 现在终于把 verify flags 从 storage-only 接到了 runtime policy：
  - `TFreePascalConnection` 新增 client-side helper，在 `ProcessEncryptedServerFlight(...)` 成功后、`SendClientFinished(...)` 之前执行
  - 只在 `sslVerifyPeer` 且 `not FSessionReused` 的 client path 上启用
  - 没有 cached peer certificate 会 fail-closed 为 `sslErrCertificate`
  - 未设置 `sslCertVerifyIgnoreHostname` 时，会对规范化后的 `FServerName` 执行 `VerifyHostname(...)`
  - 未设置 `sslCertVerifyIgnoreExpiry` 时，会对 leaf certificate 执行 `IsExpired`
- focused runtime contract 还顺手暴露了一个更下层但真实的生成器 drift：
  - `TCertificateUtils.GenerateSigned(...)` 之前忽略显式 `NotBefore/NotAfter`
  - 这会让“生成过期 leaf certificate”的测试前提失真，表面像是 verify helper 漏判，实际是 leaf 从未被生成为过期
  - 这次只做了一个最小修正，让 CA-signed path 与 self-signed path 一样尊重显式有效期窗口
- 这批边界仍然刻意收紧，没有借机把验证能力说大：
  - 没有补 CA store / chain trust runtime parity
  - 没有补 OCSP stapling / Certificate Transparency
  - 没有补 resumed PSK session persistence 对 peer certificate 的继承
- fresh focused contract、相邻 regressions 与模块级编译门禁都继续为绿，说明这次 runtime verify 收口没有把 peer-certificate surface、certificate-flight floor、resumption、early-data 家族带偏：
  - new verify-flags contract => PASS
  - peer-certificate surface regression => PASS
  - certificate-flight requirement regression => PASS
  - session resumption regression => PASS
  - early-data regression => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`
- targeted diff hygiene 也为绿，说明最终落盘的 plan/source/ledger 集合没有引入格式层噪音：
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-cert-verify-flags-hostname-expiry-parity.md src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.cert.utils.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md` => PASS

## 2026-04-08 Findings (Planning / FreePascal client peer certificate surface)
- 继续沿 validation hardening 主线推进时，下一条更基础的 runtime gap 已经很明确：
  - FreePascal client full handshake 现在终于要求看见 `Certificate`
  - 但 `TFreePascalConnection` 仍然没有把这个消息里的证书 materialize 成 `ISSLCertificate`
  - `DoGetPeerCertificate` 直接返回 `nil`
  - `DoGetPeerCertificateChain` 直接返回空数组
- 这说明当前问题不是“证书消息没到”，而是 connection surface 完全没有 peer-certificate 真值。
- 盘上已有可复用 building blocks 足够支撑一个窄批次：
  - `fafafa.ssl.tls13.servercertificate` 已能从 PEM/DER blob 构造 TLS 1.3 `Certificate` handshake
  - FreePascal backend 已有 `ISSLCertificate.LoadFromDER(...)`
- 因此这一批继续保持克制：
  - 只补 `Certificate` handshake parser + connection-level peer-certificate cache/surface
  - 不夹带 hostname / expiry parity
  - 不夹带完整链验证
  - 不夹带 client-side `CertificateVerify` signature verification
  - 不夹带 resumed PSK session persistence 对 peer certificate 的继承

## 2026-04-08 Findings (Closeout / FreePascal client peer certificate surface)
- FreePascal client full handshake 现在终于把对端证书从“握手里路过的数据”收成了 connection-level truth：
  - `fafafa.ssl.tls13.servercertificate` 新增最小 TLS 1.3 `Certificate` parser
  - `TFreePascalConnection.ProcessEncryptedServerFlight(...)` 在收到 `TLS_HANDSHAKE_TYPE_CERTIFICATE` 时解析 DER 列表
  - 连接层缓存 leaf + chain，并通过 `DoGetPeerCertificate` / `DoGetPeerCertificateChain` 以 clone 形式暴露出去
- 失败语义也保持 fail-closed，而不是继续静默退化成 `nil`：
  - `Certificate` handshake 结构损坏会中止握手
  - 任一 DER 证书加载失败也会中止握手
  - 因此“成功握手但 peer certificate surface 为空”这条路被收掉了
- 这批边界仍然刻意收紧，没有借机夸大验证能力：
  - 没有补 hostname / expiry runtime parity
  - 没有补链信任验证
  - 没有补 client-side `CertificateVerify` signature verification
  - 没有让 resumed PSK session 持久化 peer certificate
- fresh focused test、相邻 regressions 与模块级编译门禁都继续为绿，说明这次 parser/cache 收口没有把上一批 certificate-flight floor、resumption、early-data 家族带偏：
  - new peer-certificate contract => PASS
  - certificate-flight requirement regression => PASS
  - client session resumption regression => PASS
  - early-data regression => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`
- targeted diff hygiene 也继续为绿，说明最终落盘的 plan/source/test/ledger 集合没有引入格式层噪音：
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-peer-certificate-surface.md src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_peer_certificate_surface.pas task_plan.md findings.md progress.md` => PASS
- validation hardening 的下一条正确队列现在更清楚了：
  - hostname / expiry runtime parity for `TSSLCertVerifyFlags`
  - 再决定是否继续扩到 OCSP stapling / Certificate Transparency

## 2026-04-08 Findings (Planning / FreePascal client certificate flight requirements)
- 继续沿 `validation hardening` 主线扫描时，先证实了一个比 flag parity 更高优先级的底线缺口：
  - `TFreePascalContext` 虽然保存了 `FCertVerifyFlags`
  - 但 `TFreePascalConnection.ProcessEncryptedServerFlight(...)` 在 client full handshake 上只显式处理 `EncryptedExtensions` 和 `Finished`
  - `Certificate` / `CertificateVerify` 当前只是被隐式吞进 transcript，没有存在性约束，更没有 runtime 校验
- 这意味着当前 pure Pascal client path 可能接受一个只发：
  - `ServerHello`
  - `EncryptedExtensions`
  - `Finished`
  的非 resumed server flight。
- 相比之下，`sslCertVerifyIgnoreHostname` / `sslCertVerifyIgnoreExpiry` 的 runtime parity 虽然也缺，但它建立在一个更大的前提之上：full handshake 至少要先要求看到证书消息。
- 因此本批决策保持克制：
  - 先补 full-handshake message presence floor
  - 明确保留 resumed PSK path 可以省略 `Certificate` / `CertificateVerify`
  - 不在同一批里夹带完整链验证、hostname/expiry parity、OCSP、CT 或 peer-certificate API

## 2026-04-08 Findings (Closeout / FreePascal client certificate flight requirements)
- 这批先收掉的是一个更基础的 fail-open，而不是直接去做 `TSSLCertVerifyFlags` runtime parity：
  - 当 `sslVerifyPeer` 开启且不是 resumed PSK handshake 时，client 现在必须在 `Finished` 前看到 `Certificate`
  - 同一路径也必须看到 `CertificateVerify`
  - 否则 `ProcessEncryptedServerFlight(...)` 直接 fail-closed
- 行为边界保持克制，没有伪装成“完整证书验证已经做完”：
  - 这批没有补完整链验证
  - 没有补 hostname / expiry runtime parity
  - 没有补 client-side `CertificateVerify` signature verification
  - 没有扩到 OCSP stapling / Certificate Transparency
- resumed/0-RTT 相邻合同被明确从“证书验证语义”里摘出来了：
  - `tests/test_freepascal_client_session_resumption.pas`
  - `tests/test_freepascal_tls13_early_data.pas`
    - 这些 offline scripted tests 现在显式 `SetVerifyMode([])`
    - 目的不是放宽产品默认值，而是把它们的断言范围收窄到 resumption / early-data mechanics，自觉不碰这批新加的 verify floor
- fresh focused regressions 和全模块编译都继续为绿，说明这次收紧没有把已收口的 resumed PSK / early-data 路径带偏：
  - new certificate-flight contract => PASS
  - client session resumption truth test => PASS
  - early-data truth test => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`
- targeted diff hygiene 也为绿，说明这批最终落盘的文件集合没有引入格式层噪音：
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-certificate-flight-requirements.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_certificate_flight_requirements.pas tests/test_freepascal_client_session_resumption.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md` => PASS
- 后续 validation hardening 的正确顺序也更清楚了：
  - 先做 peer-certificate parse/surface
  - 再做 hostname / expiry runtime parity
  - 最后再决定是否继续扩到 OCSP / CT

## 2026-04-08 Findings (Planning / Factory-library logging scope clarification)
- 当前 logging drift 不是“backend 没实现日志”，而是 scope boundary 还不够清楚：
  - backend runtime logging 一直走 `FLogLevel` / `FLogCallback`
  - `SetDefaultConfig(...)` 已经能更新 library-default logging state
  - 但 one-shot `CreateContext(AConfig)` 仍会静默接受 logging 字段
  - `SetLogCallback(...)` 也还没有在 `GetDefaultConfig` 快照里 visibleize
- 这意味着当前真正缺的不是 per-context logging 功能，而是 entrypoint contract：
  - request path 需要 fail-fast
  - library-default path 需要 snapshot/runtime 一致
  - `CreateDefaultConfig(...)` 需要继续扮演 request-safe constructor，而不是回漏 library-scoped defaults
- 因此这一批的正确收口仍然应该保持克制：
  - 不把 `LogLevel` / `LogCallback` 强行下沉到 context
  - 不新增新的 public interface
  - 只把 owner boundary 说清楚并写成 contract

## 2026-04-08 Findings (Closeout / Factory-library logging scope clarification)
- `LogLevel` / `LogCallback` 的 owner boundary 现在终于被写成了可执行 contract：
  - one-shot `TSSLFactory.CreateContext(const AConfig)` 遇到 request `LogLevel` / `LogCallback` 会 fail-fast
  - 错误码保持 `sslErrConfiguration`
  - 没有重新引入“per-context logging 会偷偷改 shared library state”的歧义
- `CreateDefaultConfig(...)` 现在重新回到它该扮演的角色：
  - 继续继承 context/security defaults
  - 但在函数出口显式清理 library-scoped logging 字段
  - 因此它可以被当成 one-shot request config 的正式基线
- backend library-default path 的可见性和 runtime 现在对齐了：
  - `SetDefaultConfig(...)` 继续负责 `LogLevel`
  - `SetLogCallback(...)` 现在也会同步 `GetDefaultConfig.LogCallback`
  - `Log(...)` 继续按 `ALevel <= configured LogLevel` 分发，没有额外改写 runtime gating
- 相邻 one-shot isolation tests 也给出了一个更清晰的 architecture signal：
  - 旧测试里直接从 `Lib.GetDefaultConfig` 复制 one-shot config 的写法，现在不再是好的 request-path baseline
  - 改成 `CreateDefaultConfig(...)` 后，`ServerName` 和 early-data isolation contract 仍然保持绿灯
  - 这说明 request-safe constructor 和 library-default snapshot 现在各归其位
- 结论：
  - 这批收掉的是 config-surface owner drift，不是 backend logging feature gap
  - 如果后续还要继续沿 config surface 收口，就应该继续用“request-safe constructor vs library-default snapshot”这个边界来审查剩余字段
  - 否则就可以回到主线 backlog：`OCSP stapling / CT / validation hardening`

## 2026-04-08 Findings (Planning / Connector client early-data convenience)
- 当前 early-data 真值链已经闭环，但 connector facade 还停在“只管 session + server name + connect”的状态：
  - `ISSLEarlyDataConnection.SetEarlyData(...)` 已存在
  - `TSSLConnector.WithSession(...)` 已存在
  - `TSSLConnector` 仍缺把这两步在握手前串起来的 glue
- 真实缺口不是 backend 行为，也不是新的 public core interface，而是 facade choreography：
  - 用户若走 `TSSLConnector`，仍要退回到底层 `ISSLConnection`
  - 仍要手工 cast `ISSLEarlyDataConnection`
  - 仍要自己保证 `SetEarlyData(...)` 发生在 `Connect` 之前
- 因此下一批继续保持最小：
  - 不改 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` contract
  - 不自动启用 context client early-data
  - 不扩 builder / acceptor
  - 只给 `TSSLConnector` 增加 client early-data convenience API

## 2026-04-08 Findings (Closeout / Connector client early-data convenience)
- 这批收掉的是 connector facade 的编排缺口，不是 early-data 真值或 backend 行为：
  - `TSSLConnector` 现在提供 `WithEarlyData(const AData: TBytes)`
  - client connector path 现在会在 `Connect` 前 queue configured early data
  - connector 调用顺序保持为 `session -> servername -> earlydata -> connect`
- 错误语义保持克制，没有新造一层 connector 特判：
  - connection 不支持 `ISSLEarlyDataConnection` 时，connector 返回 `sslErrUnsupported`
  - `SetEarlyData(...)` 自己返回的 disabled / zero-limit / invalid-session 错误继续原样透传
  - handshake / verify 失败路径没有被改写
- 作用域仍然受控：
  - 没有自动启用 context client early-data
  - 没有扩 `TSSLAcceptor`
  - 没有碰 `src/fafafa.ssl.connection.builder.pas`
  - 没有改 FreePascal/OpenSSL/WinSSL/WolfSSL backend 的 early-data 实现
- contract test 在第一次 GREEN 尝试里还额外暴露了一个“测试观测层自身不稳”的问题：
  - mock early-data probe 没有正确走到子类观测方法
  - mock context 在构造期继承的 `servername` 会污染 connector 顺序日志
  - 这两个问题都收敛在 `tests/test_tls_connector_early_data_contract.pas` 的观测层修正里，没有改变产品行为
- focused contracts、相邻回归和全量模块编译都继续为绿，说明这批 facade glue 没把已收口的 early-data / servername 家族带偏：
  - new connector early-data contract => PASS
  - connector hostname precedence => PASS
  - public early-data API contract => PASS
  - context builder early-data contract => PASS
  - FreePascal TLS 1.3 early-data truth test => PASS
  - `python3 scripts/compile_all_modules.py` => `182/182`

## 2026-04-08 Findings (Planning / Early-data public API ergonomics)
- 当前 early-data 的行为真值已经存在，但 public ergonomics 仍有明确缺口：
  - `ISSLEarlyDataContext`
  - `ISSLEarlyDataConnection`
  - `TSSLEarlyDataStatus`
  - `TSSLEarlyDataServerPolicy`
  这些都还没有经由 `fafafa.ssl` 主入口 re-export。
- 这意味着用户若只 `uses fafafa.ssl;`，仍然不能直接声明 early-data optional interfaces，必须下钻到 `fafafa.ssl.base`。
- 另外，当前用户侧要消费 optional early-data surface，仍然需要重复写 `Supports(...)` / cast 样板。
- 因此下一批继续保持最小：
  - 不改 early-data accept/reject / anti-replay 逻辑
  - 不再扩核心接口
  - 只补主入口 re-export 与最小 helper ergonomic layer

## 2026-04-08 Findings (Closeout / Early-data public API ergonomics)
- 这批收掉的是主入口 contract 与 helper 用法之间的割裂，不是 backend early-data 行为：
  - `fafafa.ssl` 现在直接 re-export `TSSLEarlyDataStatus`
  - `fafafa.ssl` 现在直接 re-export `TSSLEarlyDataServerPolicy`
  - `fafafa.ssl` 现在直接 re-export `ISSLEarlyDataContext`
  - `fafafa.ssl` 现在直接 re-export `ISSLEarlyDataConnection`
  - 主入口也显式带出了 `sslEarlyDataNone / Queued / Accepted / Rejected` 与 `sslEarlyDataServerReject / Accept / IssueOnly`
- 用户侧的 optional-interface 消费样板现在被压缩到了最小 helper 层：
  - `TSSLHelper` 提供 `SupportsEarlyDataContext` / `SupportsEarlyDataConnection`
  - `TSSLHelper` 提供 `TryGetEarlyDataContext` / `TryGetEarlyDataConnection`
  - `TSSLHelper` 提供 `ConfigureClientEarlyData` / `ConfigureServerEarlyData`
  - `TSSLHelper.GetEarlyDataStatus` / `GetEarlyDataLimit` 在 unsupported path 上继续回退 `sslEarlyDataNone` / `0`
- 作用域保持克制，没有把“更顺手”误扩成新行为：
  - 没有改 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` contract
  - 没有新增 queue/send helper
  - 没有改 backend accept/reject / anti-replay / resumption 逻辑
- focused contract 与相邻回归都通过，说明这批 public-surface 收口没有把既有 early-data 家族带偏：
  - new public API contract => PASS
  - builder early-data contract => PASS
  - factory one-shot early-data isolation => PASS
  - FreePascal TLS 1.3 early-data transport contract => PASS
  - adjacent `ServerName` one-shot isolation => PASS
- `python3 scripts/compile_all_modules.py` 继续 `182/182`，说明主入口 re-export 与 helper 扩展没有引入更宽的编译面回归
- 残余 backlog 继续保持诚实：
  - 如果还要继续做 ergonomics，下一条更合理的是 queue/send convenience layer
  - one-shot `LogLevel` / `LogCallback` 语义仍应单独开 batch，而不是夹带在这条 early-data public API 收口里

## 2026-04-08 Findings (Planning / Factory config early-data parity)
- 当前 early-data 的公开真值链只在 builder/context 上完整，factory/config 入口仍存在真实配置漂移：
  - `ISSLEarlyDataContext` 已支持 client enable / server policy / server max-size
  - `TSSLContextBuilder` 已支持这些 fluent/config fields
  - 但 `TSSLConfig` 还不能表达这些值
  - `TSSLFactory.CreateContext(...)` 也不会把这些值应用到 context
- 这意味着当前只要用户走 config-driven path，就会得到和 builder path 不一致的 early-data 默认/显式配置语义。
- 因此下一批应该继续克制：
  - 不重开 backend 行为实现
  - 不引入新的 core interface
  - 只把 `TSSLConfig` / `TSSLFactory` / default-config / diagnostics 拉到和现有 `ISSLEarlyDataContext` 一致

## 2026-04-08 Findings (Closeout / Factory config early-data parity)
- config-driven 路径和 builder/context 路径的 early-data 真值现在已经对齐到同一条可选接口链路：
  - `TSSLConfig` 可表达 `ClientEarlyDataEnabled`
  - `TSSLConfig` 可表达 `ServerEarlyDataPolicy`
  - `TSSLConfig` 可表达 `ServerMaxEarlyDataSize`
  - `TSSLFactory` 只在 context 支持 `ISSLEarlyDataContext` 时才应用这些值
- 这一批真正收掉的不是 backend 行为，而是 factory/config 的漂移点：
  - `CreateDefaultConfig(...)` fallback path 现在显式给出保守 early-data 默认值
  - `DumpSSLConfig(...)` 现在能把 early-data 字段打印出来，诊断面不再落后于配置面
- one-shot 路径的共享默认污染风险也一并收掉了：
  - `TSSLFactory.CreateContext(const AConfig)` 不再通过 `SetDefaultConfig(...)` 改写 shared library defaults
  - focused isolation test 已证明 one-shot context 生效后，后续 default-path context 不会继承 one-shot early-data 值
- 行为边界保持克制：
  - 不支持 `ISSLEarlyDataContext` 的 backend 继续 no-op
  - 没有重开 anti-replay、ticket issuance 或 resumed early-data accept 行为
- focused contracts 与 `compile_all_modules.py` 都通过，说明这批 parity 改动没有把相邻模块编译面带偏
- targeted `git diff --check` 也已通过，最后收口时只清掉了 `src/fafafa.ssl.factory.pas` 里一处尾随空格，没有额外逻辑改动
- reviewer-driven 相邻回归也确认旧的 one-shot isolation family 没被带坏：
  - `tests/test_factory_config_server_name_isolation.pas` fresh rerun => PASS (`6/6`)
  - 说明 one-shot `CreateContext(AConfig)` 仍然不会把 `ServerName` sticky 到后续 default-path context / connection
- 仍有一个未在本批 contract 内展开的残余风险需要诚实保留：
  - `TSSLConfig.LogLevel` / `LogCallback` 本质上仍是 library-global 语义
  - 当前 one-shot factory path 对这两个字段没有显式 contract test
  - 这次 batch 没有扩 public API 或引入临时 library-global mutation 来重新定义它们的隔离语义

## 2026-04-08 Findings (Planning / FreePascal TLS 1.3 server early-data policy and max size)
- 当前 pure Pascal early-data 已有 transport contract 和 bounded anti-replay，下一条真实缺口不再是“能不能跑 0-RTT”，而是服务端公开 policy/max-size surface 还过于粗糙：
  - `TSSLEarlyDataServerPolicy` 只有 `reject/accept`
  - `NewSessionTicket.max_early_data_size` 在连接层仍硬编码为 `8`
  - builder/config round-trip 还没有 `server_max_early_data_size`
- 现有 accept path 边界已经明确：
  - 只有 `max_early_data_size > 0`
  - context policy = `sslEarlyDataServerAccept`
  - replay ledger acquire success
  时服务端才接受 resumed early data
- 因此本批应该继续克制：
  - 只扩 optional early-data context / builder surface
  - `IssueOnly` 负责“发能带 early-data limit 的 ticket，但不接受 resumed early data”
  - `Reject` / `IssueOnly` / `Accept` 三态要把“发 ticket”和“接收 resumed early data”两个责任拆开

## 2026-04-08 Findings (Closeout / FreePascal TLS 1.3 server early-data policy and max size)
- 这批把服务端 early-data contract 从 binary policy 收口到了“发行”和“接受”分离的三态 surface：
  - `TSSLEarlyDataServerPolicy` 现在支持 `Reject / Accept / IssueOnly`
  - `sslEarlyDataServerIssueOnly` 追加在现有枚举尾部，而不是插到中间
  - 这样可保持 JSON / INI 里现有 raw enum ordinal 的兼容性，不会把旧的 `accept = 1` 误映射掉
- optional/public surface 现在已经有完整的 max-size 真值链路：
  - `ISSLEarlyDataContext` 暴露 `Set/GetServerMaxEarlyDataSize`
  - `TSSLContextBuilder` 暴露 `WithServerMaxEarlyDataSize`
  - `TFreePascalContext` 保存 `FServerMaxEarlyDataSize`，默认值是 `0`
- builder/config 这一层有一个容易漏掉的真实 bug 也顺手收口了：
  - `Merge(...)` 之前并不会合并任何 early-data 字段
  - 现在会一起合并 `client_early_data_enabled`、`server_early_data_policy`、`server_max_early_data_size`
- FreePascal backend 的行为边界已经和公开 surface 对齐：
  - `Reject` 发行 `max_early_data_size = 0`
  - `IssueOnly` / `Accept` 发行配置值，不再硬编码 `8`
  - resumed early data 仍然只有在 `sslEarlyDataServerAccept` 且 anti-replay acquire 成功时才会真正被接受
  - 服务端 `DoAccept` 会按配置的累计 early-data size 上限拒绝并丢弃超限 payload
- closeout hygiene 继续保持最小：
  - 在 ticket issuance 的 `case` 里显式写出 `sslEarlyDataServerReject`
  - 这样编译器的枚举覆盖告警不会继续挂在这批新代码上
- focused verification 证明这批扩 surface 没把相邻 resumption / gate 面带偏：
  - builder/config round-trip contracts => PASS
  - early-data end-to-end contract => PASS
  - focused completeness gate => `7` PASS / `0` FAIL
  - targeted diff hygiene => PASS

## 2026-04-08 Findings (Planning / FreePascal TLS 1.3 early-data anti-replay hardening)
- 当前 pure Pascal early-data 已不再缺 transport path；剩余风险收缩到 replay ledger 语义：
  - ledger 现在只按 ticket key 记账
  - acquire 不复用 session timeout / ticket lifetime 真值
  - 过期 replay entry 没有独立 prune 语义
- 当前 server accept path 已经具备最小 anti-replay 挂点：
  - `DoAccept` 只有在 `max_early_data_size > 0`
  - context policy = `sslEarlyDataServerAccept`
  - replay ledger acquire success
  时才接受 early data
- 因此下一批不需要新 public API；只需要把现有内部 replay ledger 收紧成 session-aware。

## 2026-04-08 Findings (Closeout / FreePascal TLS 1.3 early-data anti-replay hardening)
- replay ledger 内部 contract 已从 bare-ticket acquire 收紧为 session-aware：
  - `IFreePascalEarlyDataReplayLedger.TryAcquireEarlyDataSession(ASession: ISSLSession)`
  - `TFreePascalConnection.DoAccept` 现在把 cached resumable session 传给 ledger，而不是只传 ticket 字符串
- `TFreePascalContext` 的 early-data ledger 现在是 bounded 的结构化 entry，而不是纯字符串数组：
  - entry 保存 `ticket key + expires at`
  - acquire 前会先 prune 过期 entry
  - `SetSessionCacheSize(...)` 的 limit enforcement 也会先 prune 再裁剪
- ledger expiry 没有新增独立 policy，而是复用现有 session 真值：
  - expired session 会先被 `ASession.IsValid` 拒绝，不能污染 replay ledger
  - expiry 时间继续遵守 `min(session timeout, ticket lifetime)` 的既有 `TFreePascalSession` contract
- 实际行为边界保持克制：
  - 没有新增 public API
  - `ZeroRTTSupport` / `EarlyDataSupport` 继续保持 `sslSupportExperimental`
  - 现有 `KnownIssues` wording 仍然真实，focused regressions 无需同步改词
- focused verification 证明这批 hardening 没把相邻面带偏：
  - direct replay-ledger validity contract => PASS
  - resumed server replay reject path => PASS
  - focused completeness gate => `7` PASS / `0` FAIL

## 2026-04-08 Findings (Planning / FreePascal TLS 1.3 early-data public transport and policy)
- 2026-03-27 的 primitives batch 已经把 early-data 从“完全空白”推进到了：
  - `ClientHello early_data`
  - `NewSessionTicket.max_early_data_size`
  - `EndOfEarlyData`
  - session metadata persistence
- 但真正的 early-data transport / policy gap 仍然存在，而且根因不是新 public API 缺失，而是连接层收尾语义：
  - stream-backed client `Connect` 没有 opportunistically drain 已到达的 post-handshake `NewSessionTicket`
  - server accept path 在 handshake/application-secret setup 过程中会丢失 accepted early-data buffer
  - clean EOF / no-record path 与 scripted stream fixture 预期不一致
- 这批因此必须继续保持最小实现策略：
  - 不新增 public API
  - 不引入 anti-replay 系统
  - 只把 builder/context policy + offline transport/accept-reject contract 收口
- completeness gate 也存在过一个明确缺口：
  - 在这批开始时，focused gate 仍未覆盖 `tests/test_freepascal_tls13_early_data.pas`
  - 因此 transport contract 当时还没有进入默认 completeness surface

## 2026-04-08 Findings (Closeout / FreePascal TLS 1.3 early-data public transport and policy)
- 这批真正补掉的 transport root cause 都在 `src/fafafa.ssl.freepascal.connection.pas`，而不是 builder/public API 面：
  - client `Connect` 现在会基于 `GetBufferedStreamBytesAvailable` opportunistically drain 立刻到达的 post-handshake records
  - `RecvApplicationDataFragment(..., AAllowNoRecord=True)` 现在在处理完一个 post-handshake handshake fragment 后会停下，不再把 scripted EOF 误当错误
  - `DoRead` 在 clean stream EOF 且没有剩余 application data 时返回 `0`，不再返回 `-1`
  - `DoAccept` 现在会先把 accepted early-data 暂存到局部 `LEarlyDataBuffer`，再在 handshake/application-secret setup 后恢复进 `FApplicationReadBuffer`
- scripted early-data fixture 也暴露出一个测试真值边界：
  - `NewSessionTicket` 必须在 client `Finished` 校验并派生 application secrets 之后再加密/发送
  - 否则 offline scripted transport 会基于错误 secret 模型失败
- builder contract 有一个必要但容易遗漏的接线点：
  - `tests/config/test_context_builder_early_data_contract.pas` 必须引入 `fafafa.ssl.freepascal.lib`
  - 这样 backend registration 才会通过 unit initialization 生效
- completeness gate 现在已与这条 family 对齐：
  - `tests/test_freepascal_tls13_early_data.pas` 已进入 focused gate
  - shell contract 已从 6 个 test groups 升到 7 个
  - 实跑 gate summary 为 `7` PASS / `0` FAIL
- 路线图结论继续保持克制：
  - pure Pascal backend 现在已有 bounded 的 early-data transport / accept-reject contract
  - 但 `0-RTT` 仍未“完全完成”，剩余 backlog 仍包括 anti-replay hardening、policy surface 扩展和更广的 server-side enforcement

## 2026-04-07 Findings (Planning / OpenSSL DER private-key wave 10)
- wave9 收口后，剩余最高价值缺口已经从“通用 DER 接线”收缩到一条更窄的 EC-specific gap：
  - `TOpenSSLContext.LoadPrivateKey(file/stream)` 仍不能消费 EC SEC1 DER 私钥
- 当前 repo 已具备接入 SEC1 EC 所需的大部分表面，但还没被私钥 parser 消费：
  - `d2i_ECPrivateKey`
  - `EC_KEY_free`
  - `EVP_PKEY_new`
  - `EVP_PKEY_set1_EC_KEY`
- 关键实现边界已确认：
  - `LoadPrivateKeyPEM` 仍然必须保持 PEM-only
  - Ed25519 继续沿用现有 PKCS#8 DER path，不新增 raw DER 组装
  - capability 只允许把 `SupportsDERPrivateKey` 纳入 SEC1 EC fallback，不能把 `SupportsPKCS8PrivateKey` / `SupportsPasswordProtectedKeys` 一并错误拉高
- 计划结论：
  - 用 runtime-derived EC / Ed25519 fixtures 继续扩 contract
  - 在 wave9 parser 尾部追加 SEC1 EC parse step
  - capability 继续遵守 strict runtime truth，不扩 WinSSL 或新公开 API

## 2026-04-07 Findings (OpenSSL DER private-key wave 10 closeout)
- wave10 已把 OpenSSL context 的真实 DER 私钥加载控制流继续推进到 4 条路径：
  - 未加密 DER PKCS#8
  - 加密 DER PKCS#8
  - DER PKCS#1 RSA
  - DER SEC1 EC
- `src/fafafa.ssl.openssl.context.pas` 的共享私钥 blob 解析顺序现在固定为：
  - PEM
  - encrypted DER PKCS#8
  - DER PKCS#8
  - DER PKCS#1 RSA
  - DER SEC1 EC
- wave10 只补了最小必需的 OpenSSL 绑定，不扩大 required surface：
  - `src/fafafa.ssl.openssl.api.ec.pas` 增加 `d2i_ECPrivateKey`
  - `src/fafafa.ssl.openssl.api.evp.pas` 增加 `EVP_PKEY_set1_EC_KEY`
- capability truth source 已继续与真实 DER private-key surface 对齐：
  - `SupportsDERPrivateKey` 现在包含 only-EC-SEC1 fallback truth
  - `SupportsPKCS8PrivateKey` / `SupportsPasswordProtectedKeys` 不会因为只剩 SEC1 EC surface 而错误变成 `True`
- 公开 API 语义继续保持收口：
  - 不新增 `LoadPrivateKeyDER` 之类的新接口
  - 不扩到 WinSSL
  - `LoadPrivateKeyPEM` 仍然保持 PEM-only
- fresh verification 证明这轮 EC SEC1 接线没有把相邻 PEM/BIO/证书生成边界带偏：
  - `tests/test_openssl_context_der_private_key_contract.pas` => PASS (`58/58`)
  - `tests/openssl/test_openssl_features.pas` => PASS
    - `Key-format capability matrix EC SEC1 fallback contract verified`
    - `Key-format capability matrix no-DER-surface runtime drift contract verified`
  - `tests/test_helper_utilities.pas` => PASS (`24/24`)
  - `tests/test_openssl_context_bio_contract.pas` => PASS (`20/20`)
  - `tests/test_pem_key_read_symbol_contract.pas` => PASS (`6/6`)
  - `tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas` => PASS (`2/2`)
  - `tests/test_cert_utils_generate_selfsigned_ec_keygen_family_contract.pas` => PASS (`46/46`)
  - `tests/test_cert_utils_ed25519_contract.pas` => PASS (`24/24`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS

## 2026-04-07 Findings (Planning / OpenSSL DER private-key wave 9)
- wave8 收口后，下一条最高价值缺口已经从 capability 声明转成真实加载控制流缺口：
  - `TOpenSSLContext.LoadPrivateKey(file/stream)` 仍然只接了 PEM surface
- 当前 repo 已具备实现 DER 私钥加载所需的大部分 API 绑定，但未消费：
  - 未加密 DER PKCS#8：`d2i_PKCS8_PRIV_KEY_INFO` + `EVP_PKCS82PKEY`
  - 加密 DER PKCS#8：`d2i_X509_SIG` + `PKCS8_decrypt` + `EVP_PKCS82PKEY`
  - DER PKCS#1 RSA：`d2i_RSAPrivateKey` + `EVP_PKEY_new` + `EVP_PKEY_set1_RSA`
- 当前关键实现边界：
  - 文件无密码路径仍然只走 `SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)`
  - stream 路径与 `LoadPrivateKeyPEM` 都只走 `PEM_read_bio_PrivateKey`
  - `LoadPrivateKeyPEM` 语义无需扩到 DER
- 已确认一个额外实现点必须一并处理：
  - `PKCS8_decrypt` 只在 `src/fafafa.ssl.openssl.api.pkcs12.pas` 里绑定
  - `d2i_PKCS8_PRIV_KEY_INFO` / `EVP_PKCS82PKEY` 在 `src/fafafa.ssl.openssl.api.pkcs.pas`
  - 所以 wave9 必须同时覆盖 PKCS 与 PKCS12 的 lazy-load / runtime-drift 语义
- 计划结论：
  - 先用 focused contract 证明 context DER 私钥加载缺口
  - 再以共享 buffer parser 的方式最小接线
  - capability 只在真实 DER surface 存在时再发布支持

## 2026-04-07 Findings (OpenSSL DER private-key wave 9 closeout)
- wave9 已把 OpenSSL context 的真实私钥加载控制流从 PEM-only 推进到 3 条 DER 路径：
  - 未加密 DER PKCS#8
  - 加密 DER PKCS#8
  - DER PKCS#1 RSA
- `src/fafafa.ssl.openssl.context.pas` 现在采用共享私钥 blob 解析流，解析顺序固定为：
  - PEM
  - encrypted DER PKCS#8
  - DER PKCS#8
  - DER PKCS#1 RSA
- 文件与流语义已经收敛到最小且一致的控制流：
  - 文件无密码路径保留 `SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)` 快路径
  - 快路径失败后回退到“读取文件字节 + shared parser”
  - 文件有密码与 stream 路径直接走 shared parser
- 公开 API 语义保持收口：
  - 不新增 `LoadPrivateKeyDER` 之类的新接口
  - `LoadPrivateKeyPEM` 仍然保持 PEM-only
- capability truth source 已与真实 DER private-key surface 对齐：
  - `SupportsDERPrivateKey`
  - `SupportsPKCS8PrivateKey`
  - `SupportsPasswordProtectedKeys`
- `src/fafafa.ssl.openssl.api.pkcs12.pas` 的 loader state 已补齐模块加载标记语义：
  - 仅在 `osmPKCS12` 尚未标记 loaded 时再 lazy-load
  - `UnloadPKCS12Module` 会同步清理 module-loaded state
  - 这避免 runtime-drift tests 中人为置空符号后被无条件偷偷重绑
- fresh verification 证明这轮 DER 接线没有打坏相邻 PEM/BIO/PKCS12 边界：
  - `tests/test_openssl_context_der_private_key_contract.pas` => PASS (`28/28`)
  - `tests/openssl/test_openssl_features.pas` => PASS
  - `tests/test_helper_utilities.pas` => PASS (`24/24`)
  - `tests/test_openssl_context_bio_contract.pas` => PASS (`20/20`)
  - `tests/test_pem_key_read_symbol_contract.pas` => PASS (`6/6`)
  - `tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas` => PASS (`2/2`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS

## 2026-04-07 Findings (Planning / OpenSSL capability wave 8)
- wave7 之后，OpenSSL capability matrix 里下一条仍值得继续处理的 drift 收缩到 4 个 key-format 布尔值：
  - `SupportsPEMPrivateKey := True`
  - `SupportsDERPrivateKey := True`
  - `SupportsPKCS8PrivateKey := True`
  - `SupportsPasswordProtectedKeys := True`
- 当前源码里已经存在两类真实私钥加载表面，但 capability 还没有跟上：
  - 文件快路径：`SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)`
  - 读入路径：`PEM_read_bio_PrivateKey` + `BIO_free` + (`BIO_new_file` or `BIO_new_mem_buf`)
- 关键运行时边界已确认：
  - `Initialize` 默认不会加载 `osmPEM`
  - capability 不能只看“当前是否已预绑定 PEM 指针”，必须允许 lazy-load 到真实读入表面
  - 当前仓库内不存在真实 OpenSSL DER 私钥上下文加载路径
- 计划结论：
  - RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`
  - 实现继续只改 `src/fafafa.ssl.openssl.backed.pas`
  - 本轮不新增 DER 解析、不补 PKCS API 接线、不改 `ISSLContext` 公共接口

## 2026-04-07 Findings (OpenSSL capability wave 8 closeout)
- wave8 已把 OpenSSL capability matrix 中 4 个 key-format 布尔值收紧到当前真实可达的私钥加载表面：
  - `SupportsPEMPrivateKey`
  - `SupportsDERPrivateKey`
  - `SupportsPKCS8PrivateKey`
  - `SupportsPasswordProtectedKeys`
- `src/fafafa.ssl.openssl.backed.pas` 现在新增并使用了本地 readiness helpers：
  - `OpenSSLPrivateKeyFileSurfaceReady`
  - `OpenSSLPrivateKeyReadSurfaceReady`
  - `OpenSSLPasswordProtectedKeySurfaceReady`
- 语义已经锁定为：
  - `Initialize` 默认不加载 PEM module；只有在 `PEM_read_bio_PrivateKey` 尚未绑定且 `osmPEM` 尚未加载时，才在 capability probe 内 lazy-load `LoadOpenSSLPEM(...)`
  - `SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)` 当前可直接加载 PKCS#8 PEM 文件，所以在读入表面缺失时，`SupportsPEMPrivateKey` / `SupportsPKCS8PrivateKey` 仍可由文件快路径维持为 `True`
  - `SupportsPasswordProtectedKeys` 采用更窄语义，只跟随真实私钥读入表面，不再从文件快路径或写出 helper 反推
  - 当前不存在 OpenSSL DER 私钥上下文加载路径，因此 `SupportsDERPrivateKey = False` 直到仓库里出现真实 DER load path
- fresh verification 证明 key-format capability 收紧没有把相邻 PEM/BIO 边界带偏：
  - `tests/openssl/test_openssl_features.pas` => PASS
    - `Key-format capability matrix baseline contract verified`
    - `Key-format capability matrix read-surface runtime drift contract verified`
    - `Key-format capability matrix no-surface runtime drift contract verified`
  - `tests/test_helper_utilities.pas` => PASS (`24/24`)
  - `tests/test_openssl_context_bio_contract.pas` => PASS (`20/20`)
  - `tests/test_pem_key_read_symbol_contract.pas` => PASS (`6/6`)
  - `tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas` => PASS (`2/2`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS

## 2026-04-07 Findings (Planning / OpenSSL capability wave 7)
- wave6 之后，OpenSSL capability matrix 里下一条仍值得继续处理的 drift 是：
  - `SupportsDTLS := True`
- 这条字段当前没有跟随 protocol truth source，对外语义已经落后于其它 C-library backend：
  - `SupportsDTLS = IsProtocolSupported(sslProtocolDTLS10) or IsProtocolSupported(sslProtocolDTLS12)`
- 但 OpenSSL 当前 DTLS protocol probe 本身还不够严格：
  - `sslProtocolDTLS10`, `sslProtocolDTLS12`
  - `=> Assigned(DTLS_method) or (FVersionNumber >= $10000000)`
- 当前源码已经具备补强 DTLS runtime probe 所需的全部表面：
  - `DTLS_method`
  - `DTLS_client_method`
  - `DTLS_server_method`
  - `SSL_CTX_new`
  - `SSL_CTX_free`
  - `SSL_CTX_set_min_proto_version`
  - `SSL_CTX_set_max_proto_version`
  - `DTLS1_VERSION`
  - `DTLS1_2_VERSION`
- 计划结论：
  - 先补强 `IsProtocolSupported(...)` 对 DTLS 的 runtime probe
  - 再把 `SupportsDTLS` 对齐到 `DTLS10 or DTLS12` 的 protocol truth
  - 所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`
  - 实现继续只改 `src/fafafa.ssl.openssl.backed.pas`
  - 本轮不扩到新的 support-level 字段或其它 capability 家族

## 2026-04-07 Findings (OpenSSL capability wave 7 closeout)
- wave7 已把 OpenSSL 的 DTLS protocol truth 和公开 capability 收紧到真正 runtime-aware 的 probe：
  - `IsProtocolSupported(sslProtocolDTLS10)`
  - `IsProtocolSupported(sslProtocolDTLS12)`
  - `SupportsDTLS`
- `src/fafafa.ssl.openssl.backed.pas` 现在不再对 DTLS 走版本号兜底，而是：
  - 先把 `sslProtocolDTLS10` / `sslProtocolDTLS12` 映射到 OpenSSL DTLS 版本常量
  - 再通过 `RuntimeProbeMethodForProtocol(...)` 选择真实 method：
    - `DTLS_method`
    - fallback `DTLS_client_method`
    - fallback `DTLS_server_method`
  - 再复用现有 `SSL_CTX_new` + min/max proto setter probe 流程
- 公开 capability 语义现在与其它 C-library backend 保持一致：
  - `SupportsDTLS = IsProtocolSupported(sslProtocolDTLS10) or IsProtocolSupported(sslProtocolDTLS12)`
- 这批仍保持 strict alignment，没有扩散设计面：
  - 不新增 `DTLSSupport` support-level 字段
  - 不扩 key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段
- fresh verification 证明 DTLS probe hardening 没把 wave2-wave6 的边界带偏：
  - `tests/openssl/test_openssl_features.pas` => PASS
    - `DTLS capability matrix policy-aware contract verified`
  - `tests/test_openssl_ssl_post_handshake_contract.pas` => PASS (`5/5`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS

## 2026-04-06 Findings (Planning / OpenSSL capability wave 6)
- wave5 之后，OpenSSL capability matrix 里最值得继续处理的一条主干 drift 是：
  - `SupportsTLS13`
- 这条静态版本号发布还直接影响：
  - `MaxTLSVersion`
  - `ZeroRTTSupport`
  - `EarlyDataSupport`
  - `PostHandshakeAuthSupport`
- 当前源码已经有可复用的 runtime truth source：
  - `TOpenSSLLibrary.IsProtocolSupported(sslProtocolTLS13)`
  - 其内部通过 `RuntimeProbeProtocolSupport(...)` 走真实 proto-version setter 路径
- 计划结论：
  - 所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`
  - 实现继续只改 `src/fafafa.ssl.openssl.backed.pas`
  - 本轮不扩到 `SupportsDTLS`、key-format 或平台/性能布尔字段

## 2026-04-06 Findings (OpenSSL capability wave 6 closeout)
- wave6 已把 OpenSSL capability matrix 的 TLS 1.3 主干声明收紧到现成 runtime protocol probe：
  - `SupportsTLS13`
  - `MaxTLSVersion`
  - `ZeroRTTSupport`
  - `EarlyDataSupport`
  - `PostHandshakeAuthSupport`
- `src/fafafa.ssl.openssl.backed.pas:GetCapabilities` 现在不再只看版本号，而是先复用：
  - `IsProtocolSupported(sslProtocolTLS13)`
- 这意味着 capability matrix 现在会跟随真实 protocol policy drift：
  - 当 runtime setter policy 拒绝 TLS 1.3 时，`SupportsTLS13 = False`
  - `MaxTLSVersion` 会回落到 `sslProtocolTLS12`
  - `ZeroRTTSupport` / `EarlyDataSupport` 不会继续宣称 `sslSupportStable`
  - `PostHandshakeAuthSupport` 不会继续宣称 `sslSupportStable`
- 这批保持 strict alignment，没有扩散到新的 protocol/helper 设计：
  - 不改 `SupportsDTLS`
  - 不扩到 key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段
- fresh verification 证明 TLS 1.3 capability 收紧没有把 wave2-wave5 的边界带偏：
  - `tests/openssl/test_openssl_features.pas` => PASS
    - `TLS 1.3 capability matrix policy-aware contract verified`
  - `tests/test_openssl_ssl_post_handshake_contract.pas` => PASS (`5/5`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`, host-specific getter skips only)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS

## 2026-04-06 Findings (Planning / OpenSSL capability wave 5)
- wave4 之后，OpenSSL capability matrix 里仍然适合继续 strict-alignment 的字段已经缩到很窄：
  - `SupportsChaChaPoly`
  - `SupportsPKCS12`
- 这两项已有可复用的 runtime truth source：
  - ChaCha20-Poly1305 可复用现有 cipher parser 语义，而不是继续按版本号发布
  - PKCS#12 可复用已绑定的 core API surface，而不是继续硬编码 `True`
- 本轮明确不纳入：
  - `SupportsDTLS`
  - key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段
- 计划结论：
  - 所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`
  - 实现继续只改 `src/fafafa.ssl.openssl.backed.pas`
  - 不为 PKCS#12 新增 module-loaded contract，只直接使用现有 API pointer readiness

## 2026-04-06 Findings (OpenSSL capability wave 5 closeout)
- wave5 已把 capability matrix 中最后两条明显的静态发布字段收紧到现成 runtime truth source：
  - `SupportsChaChaPoly`
  - `SupportsPKCS12`
- `src/fafafa.ssl.openssl.backed.pas` 现在新增并使用了本地 readiness helpers：
  - `OpenSSLChaChaPolySurfaceReady`
  - `OpenSSLPKCS12SurfaceReady`
- capability matrix 不再与现有 runtime truth source 分叉：
  - ChaCha20-Poly1305
    - 语义对齐到 `IsCipherSupported('TLS_CHACHA20_POLY1305_SHA256')` 的真实 parser 路径
    - 需要 `TLS_method` / `SSL_CTX_new`
    - 优先尝试 `SSL_CTX_set_cipher_list`
    - 若前者未成功，再回退到 `SSL_CTX_set_ciphersuites`
    - parser surface 不可用或拒绝该 suite 时，`SupportsChaChaPoly = False`
  - PKCS#12
    - 语义对齐到已绑定的 core API surface
    - 需要 `PKCS12_create`
    - 需要 `PKCS12_parse`
    - 需要 `d2i_PKCS12_bio`
    - 需要 `i2d_PKCS12_bio`
- 这批还顺手收紧了 capability matrix 内部一致性：
  - `SupportedCiphers` 现在只有在 `SupportsChaChaPoly = True` 时才会包含 `sslCipherCHACHA20_POLY1305`
- wave5 仍保持 strict alignment，没有扩散设计面：
  - 不改 `SupportsDTLS`
  - 不扩 key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段
  - 不新增 PKCS#12 module-loaded contract
- fresh verification 证明这批 capability hardening 没把 wave2-wave4 的边界带偏：
  - `tests/openssl/test_openssl_features.pas` => PASS
    - `ChaCha20-Poly1305 capability matrix runtime drift contract verified`
    - `PKCS#12 capability matrix runtime drift contract verified`
  - `tests/test_openssl_ssl_post_handshake_contract.pas` => PASS (`5/5`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`, host-specific getter skips only)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS

## 2026-04-06 Findings (OpenSSL capability wave 4 closeout)
- wave4 已把 capability matrix 中最后一批明显的 SSL runtime drift 收口到真实 readiness 语义：
  - `SupportsSNI` / `SNISupport`
  - `SupportsALPN` / `ALPNSupport`
  - `SupportsOCSPStapling` / `OCSPStaplingSupport`
  - `SessionCacheSupport`
  - `RenegotiationSupport`
  - `SupportsCertificateTransparency` / `CertTransparencySupport`
- `src/fafafa.ssl.openssl.backed.pas` 现在新增并使用了本地 readiness helpers：
  - `OpenSSLSNISurfaceReady`
  - `OpenSSLALPNSurfaceReady`
  - `OpenSSLSessionCacheSurfaceReady`
  - `OpenSSLRenegotiationSurfaceReady`
  - `OpenSSLOCSPStaplingSurfaceReady`
  - `OpenSSLCertificateTransparencySurfaceReady`
- capability matrix 不再与现有 `IsFeatureSupported(...)` 真值源分叉：
  - SNI => `Assigned(SSL_set_tlsext_host_name) or Assigned(SSL_CTX_set_tlsext_servername_callback)`
  - ALPN => `Assigned(SSL_CTX_set_alpn_protos) and Assigned(SSL_get0_alpn_selected)`
  - Session cache => `Assigned(SSL_CTX_set_session_cache_mode) and Assigned(SSL_CTX_get_session_cache_mode)`
  - Renegotiation => `Assigned(SSL_renegotiate)`
  - OCSP stapling => `Assigned(SSL_CTX_set_tlsext_status_type) and Assigned(SSL_CTX_set_tlsext_status_cb)`
  - CT => `version >= 1.1.0` and `TOpenSSLLoader.IsModuleLoaded(osmCT)`
- 版本语义也保留了，但前提变成“runtime ready 才发布”：
  - `RenegotiationSupport`
    - helper ready + OpenSSL `< 3.0` => `sslSupportStable`
    - helper ready + OpenSSL `>= 3.0` => `sslSupportDeprecated`
    - helper missing => `sslSupportNone`
  - `CertTransparencySupport`
    - helper/module ready + OpenSSL `>= 3.0` => `sslSupportStable`
    - helper/module ready + `>= 1.1.0` and `< 3.0` => `sslSupportExperimental`
    - helper/module missing => `sslSupportNone`
- 当前 host 还暴露出一个重要边界，已被写进 contract：
  - session-cache helper surface 在当前 host 上并不完整
  - 因此测试不再简单 skip，而是要求 `SessionCacheSupport = sslSupportNone`
- fresh verification 证明这批 capability hardening 没把前两轮收口带偏：
  - `tests/openssl/test_openssl_features.pas` => PASS
  - `tests/test_openssl_ssl_post_handshake_contract.pas` => PASS (`5/5`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`, host-specific getter skips only)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS

## 2026-04-06 Findings (Planning / OpenSSL capability wave 4)
- wave3 收口后，当前最值得继续处理的不是新 helper family，而是 capability matrix 里仍按常量或版本号发布的公开能力声明：
  - `SupportsSNI` / `SNISupport`
  - `SupportsALPN` / `ALPNSupport`
  - `SupportsOCSPStapling` / `OCSPStaplingSupport`
  - `SessionCacheSupport`
  - `RenegotiationSupport`
  - `SupportsCertificateTransparency` / `CertTransparencySupport`
- 当前源码已经给出了明确真值源：
  - `TOpenSSLLibrary.IsFeatureSupported(...)` 对 SNI / ALPN / session cache / renegotiation / OCSP / CT 都是 runtime-aware
  - 但 `GetCapabilities` 仍把这批字段写成 `True`、`sslSupportStable`，或只按版本号判断
- 计划结论：
  - 本轮不扩 helper family
  - 只把 `GetCapabilities` 收紧到和现有 runtime probe / module-loaded state 一致
  - CT 的测试边界以 `TOpenSSLLoader.SetModuleLoaded(osmCT, False)` 为准，不发散成新的 CT helper-family contract
- `RenegotiationSupport` 的目标语义固定为：
  - helper ready + OpenSSL `< 3.0` => `sslSupportStable`
  - helper ready + OpenSSL `>= 3.0` => `sslSupportDeprecated`
  - helper missing => `sslSupportNone`

## 2026-04-06 Findings (OpenSSL SSL runtime wave 3 closeout)
- wave3 已把 capability matrix 里剩下的两条高价值 drift 收口到 runtime helper readiness：
  - `SupportsSessionTickets` / `SessionTicketsSupport`
  - `PostHandshakeAuthSupport`
- `SessionTickets` 现在不再和 `IsFeatureSupported(sslFeatSessionTickets)` 分叉：
  - `src/fafafa.ssl.openssl.backed.pas`
    - 新增 `OpenSSLSessionTicketSurfaceReady`
    - `SupportsSessionTickets` 不再硬编码 `True`
    - `SessionTicketsSupport` 不再硬编码 `sslSupportStable`
  - 当前语义和 feature probe 对齐为：
    - `Assigned(SSL_CTX_set_tlsext_ticket_key_cb) or Assigned(SSL_set_session_ticket_ext_cb)`
- `PostHandshakeAuth` 不再出现“capability 宣称 stable，但 SSL helper family 甚至没绑定”的状态：
  - `src/fafafa.ssl.openssl.api.ssl.pas`
    - 新增并绑定
      - `SSL_CTX_set_post_handshake_auth`
      - `SSL_set_post_handshake_auth`
      - `SSL_verify_client_post_handshake`
  - `src/fafafa.ssl.openssl.backed.pas`
    - 新增 `OpenSSLPostHandshakeAuthSurfaceReady`
    - `PostHandshakeAuthSupport` 现在只有在 TLS 1.3 可用且上述 helper surface 真正 ready 时才会发布 `sslSupportStable`
- host-aware boundary 进一步明确：
  - 当前 host `libssl.so.3` 上
    - `SSL_CTX_set_tlsext_ticket_key_cb` 不导出
    - `SSL_set_session_ticket_ext_cb` 导出
    - post-handshake-auth 三个 helper 都导出
  - capability 与 load contract 都已经按这条 host 事实收紧，而不是继续扩大 required semantics
- 新增 focused contracts 把 wave3 的边界锁住了：
  - `tests/test_openssl_ssl_post_handshake_contract.pas`
    - 锁住 post-handshake-auth helper family 的 load contract
  - `tests/openssl/test_openssl_features.pas`
    - 新增 session-ticket capability runtime drift contract
    - 新增 post-handshake capability runtime drift contract
- fresh verification 证明这批收紧没有把前两轮结果带偏：
  - `tests/test_openssl_ssl_post_handshake_contract.pas` => PASS (`5/5`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`, host-specific getter skips only)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `tests/openssl/test_openssl_features.pas` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS

## 2026-04-06 Findings (Planning / OpenSSL SSL runtime wave 3)
- wave2 收口后，当前最值得继续处理的已不是新 helper family，而是 capability matrix 中仍未 runtime-aware 的公开能力声明：
  - `SupportsSessionTickets := True`
  - `SessionTicketsSupport := sslSupportStable`
  - `PostHandshakeAuthSupport := sslSupportStable`（仅按 TLS 1.3 判断）
- 当前源码已经显示出一条明确分叉：
  - `TOpenSSLLibrary.IsFeatureSupported(sslFeatSessionTickets)` 是 runtime-aware，当前逻辑为
    `Assigned(SSL_CTX_set_tlsext_ticket_key_cb) or Assigned(SSL_set_session_ticket_ext_cb)`
  - 但 `GetCapabilities` 还没有和这条 probe 对齐，因此 capability matrix 与 feature probe 对同一能力给出不同答案
- post-handshake auth 的问题更直接：
  - 当前 host `libssl.so.3` 导出
    - `SSL_CTX_set_post_handshake_auth`
    - `SSL_set_post_handshake_auth`
    - `SSL_verify_client_post_handshake`
  - 但 `src/fafafa.ssl.openssl.api.ssl.pas` 里还没有声明/绑定这组 helper
  - `GetCapabilities` 却已经把 `PostHandshakeAuthSupport` 按 TLS 1.3 版本号宣称为 stable
- 计划结论：
  - wave3 的第一批先把 session-ticket capability 和已有 runtime probe 对齐
  - 第二批再补齐 post-handshake-auth helper family 的 load contract，并把 capability matrix 收紧到 helper-surface-ready 语义

## 2026-04-06 Findings (OpenSSL SSL runtime wave 2 closeout)
- wave2 已经按批次收口，不再停留在“helper 声明了但 load/capability 没对齐”的中间态：
  - `src/fafafa.ssl.openssl.api.ssl.pas`
    - `LoadOpenSSLSSL` 现在会绑定当前 host 已导出的 `early-data` / `keylog` / `record-padding` / `async` / `QUIC` helper family
    - `SSL` 模块不再对这些公开 surface 出现“声明存在、unload 会清、但 load 后仍为 nil”的语义漂移
  - `src/fafafa.ssl.openssl.backed.pas`
    - `ZeroRTTSupport` / `EarlyDataSupport` 不再只按 TLS 1.3 版本号无条件宣称 `sslSupportStable`
    - 现在只有在 TLS 1.3 可用且 early-data helper surface 真正 ready 时才会发布 stable 支持
- host-aware optional boundary 已被明确锁住，而不是继续扩大 `osmSSL` 的 required 语义：
  - 当前 host `libssl.so.3` 上，`SSL_CTX_get_record_padding_callback` 与 `SSL_get_record_padding_callback` 不导出
  - focused contract 对这两个 getter 显式 `[SKIP]`，其余已导出的 record-padding setter/arg/block-padding helpers 则必须绑定
- 新增 focused contracts 把 wave2 的三个 helper family 批次锁成可回归的边界：
  - `tests/test_openssl_ssl_early_data_contract.pas`
  - `tests/test_openssl_ssl_padding_contract.pas`
  - `tests/test_openssl_ssl_async_quic_contract.pas`
  - `tests/openssl/test_openssl_features.pas`
    - capability-matrix runtime drift contract 现在会验证：清空 representative early-data helper 后，`ZeroRTTSupport` / `EarlyDataSupport` 不能继续宣称 stable
- fresh verification 证明这批收紧没有把仓库基线带偏：
  - `tests/test_openssl_ssl_early_data_contract.pas` => PASS (`7/7`)
  - `tests/test_openssl_ssl_padding_contract.pas` => PASS (`12/12`, only host-specific skips on the two unexported getters)
  - `tests/test_openssl_ssl_async_quic_contract.pas` => PASS (`19/19`)
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/openssl/test_openssl_features.pas` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
  - `git diff --check -- ...` => PASS
- 下一轮优先级已经比 wave2 更窄：
  - 不需要回头重做本轮已经锁住的 helper family
  - 应继续审计 capability matrix 里仍按版本号/硬编码宣称 stable 的字段，尤其是 session-ticket / post-handshake-auth 这一类对外已经暴露的能力声明

## 2026-04-06 Findings (Planning / OpenSSL SSL runtime wave 2)
- 下一轮不再按“哪个 helper 漏绑了就补哪个”的方式推进，而是按公开承诺优先级拆成三个批次：
  - `early-data / 0-RTT`
  - `keylog / record-padding`
  - `async / QUIC`
- 当前 wave2 的 root cause 不是“helper 没声明”，而是“helper 已声明、unload 会清、但 load 和 capability 仍没对齐”：
  - `src/fafafa.ssl.openssl.api.ssl.pas`
    - 剩余 family 都已经声明为函数指针，也会在 `ClearSSLFunctions` 时清空
    - 但 `LoadOpenSSLSSL` 仍未绑定 `early-data` / `keylog` / `record-padding` / `async` / `QUIC`
  - `src/fafafa.ssl.openssl.backed.pas`
    - `ZeroRTTSupport` / `EarlyDataSupport` 仍只按 TLS 1.3 版本号宣称 `sslSupportStable`
    - 这已经构成公开 capability matrix 与 runtime helper readiness 的语义漂移
- 当前 host `libssl.so.3` probe 结果决定了本轮分组方式：
  - `early-data`: `SSL_CTX_set_max_early_data` / `SSL_CTX_get_max_early_data` / `SSL_set_max_early_data` / `SSL_get_max_early_data` / `SSL_get_early_data_status` 全部 exported
  - `keylog`: setter/getter 都 exported
  - `record-padding`: setter/arg/block-padding helpers exported，但 `SSL_CTX_get_record_padding_callback` 和 `SSL_get_record_padding_callback` 不导出
  - `async`: `SSL_poll` / `SSL_set_async_callback` / `SSL_set_async_callback_arg` / `SSL_get_async_status` 全部 exported
  - `QUIC`: 当前声明的 13 个 helper 在这台 host 上全部 exported
- 计划结论：
  - wave2 的第一优先级不是继续泛化 `osmSSL` required semantics，而是先把 capability matrix 已经向外宣称稳定的 `early-data / 0-RTT` 收紧成 runtime-aware
  - `keylog / record-padding` 与 `async / QUIC` 则按 host-aware load contract 补齐，保持“导出就必须绑定，不导出就显式 skip”的边界

## 2026-04-05 Findings (Repo-level hardening wave 1)
- 运行时契约批次已经进一步收口，前一轮 review 剩下的两条高价值 runtime 风险不再只是源码层面的推断：
  - `src/fafafa.ssl.factory.pas`
    - `GetLibrary(...)` / `IsLibraryAvailable(...)` 现在都在同一临界区内完成 create + initialize + cache
    - focused concurrent contract 证明并发调用只会触发一次 `Initialize`
  - `src/fafafa.ssl.openssl.loader.pas` + `src/fafafa.ssl.openssl.api.aes.pas` + `src/fafafa.ssl.openssl.api.sha.pas` + `src/fafafa.ssl.openssl.api.modes.pas`
    - 当前 dirty tree 上，`LoadFunctions(...)` 已经在 required symbol 缺失时 fail-closed，返回 `-1` 并清空本次已绑定的函数指针
    - AES / SHA 模块也已经只在关键符号组就绪时才会发布 loaded state
    - 本轮继续收掉剩余的 `Modes` 漂移：`osmModes` 现在只有在 GCM / CCM / XTS / OCB / key-wrap 这一整套 direct helper surface 全部 ready 时才会发布 loaded state
  - `src/fafafa.ssl.openssl.api.blake2.pas`
    - host `libcrypto.so.3` 上当前没有 direct BLAKE2 符号，但模块之前仍会返回 `True` 并发布 `osmBLAKE2`
    - 本轮把 `BLAKE2` 也收紧到 helper-surface-ready 语义：只有当 hash + MAC 这两组 helper 都真的可用时才发布 loaded state
  - `src/fafafa.ssl.openssl.api.ssl.pas`
    - `UnloadOpenSSLSSL` 之前只清掉极少数 OCSP tlsext 相关指针，`osmSSL` 虽然会被置回 `False`，但 options / cipher-list / SNI / ALPN / cipher-introspection 等 helper surface 仍保持已赋值
    - 本轮把 `SSL` 的 unload-side contract 收紧成真实语义：新增 `ClearSSLFunctions`，让 `UnloadOpenSSLSSL` 清空整组 helper surface，再回落 loaded state
    - `LoadOpenSSLSSL` 之前也存在 load-side 漂移：宿主 `libssl.so.3` 明明导出了 `SSL_CTX_set_info_callback` / `SSL_CTX_get_info_callback` / `SSL_set_info_callback` / `SSL_get_info_callback` / `SSL_state_string` / `SSL_state_string_long`，但模块根本没有绑定这些 helper
    - 本轮把 `SSL` 的 info/state load-side contract 收紧成真实语义：已导出的 info callback / state-string helpers 现在都会在 load 时被绑定，OpenSSL 上层不再把这条能力链误判成 unsupported
    - 本轮继续把 `SSL` 的 session-ticket / PSK load-side contract 收紧成真实语义：宿主当前已导出的
      `SSL_set_session_ticket_ext` / `SSL_set_session_ticket_ext_cb` /
      `SSL_CTX_use_psk_identity_hint` / `SSL_use_psk_identity_hint` /
      `SSL_CTX_set_psk_server_callback` / `SSL_set_psk_server_callback` /
      `SSL_CTX_set_psk_client_callback` / `SSL_set_psk_client_callback`
      现在都会在 load 时被绑定，feature/capability probe 不再把这组路径误判成 unavailable
- 新增 focused Pascal contract 把当前 loader / modes ready 语义锁住了：
  - `tests/test_openssl_loader_ready_contract.pas`
    - 验证 `LoadFunctions(...)` 在 required miss 时返回 `-1`，并保持缺失 binding 为 `nil`
    - 验证 `LoadModesFunctions(...)` 的返回值和 `osmModes` loaded state 必须与实际 direct-helper readiness 一致
  - `tests/test_openssl_blake2_ready_contract.pas`
    - 验证 `LoadBLAKE2Functions(...)` 的返回值和 `osmBLAKE2` loaded state 必须与 BLAKE2 hash/MAC helper readiness 一致
  - `tests/test_openssl_ssl_unload_contract.pas`
    - 验证 `LoadOpenSSLSSL` 成功后，`UnloadOpenSSLSSL` 必须同步清空已发布的关键 helper surface，而不是只改 module flag
  - `tests/test_openssl_ssl_load_contract.pas`
    - 验证 `LoadOpenSSLSSL` 成功后，宿主 `libssl` 已导出的 info callback / state-string helpers 必须被真正绑定
    - 同时验证宿主 `libssl` 已导出的 session-ticket / PSK helpers 也必须被真正绑定；当前 host 不导出的 `SSL_CTX_set_tlsext_ticket_key_cb` 则按 host-specific optional surface 跳过
- fresh verification 说明这批 runtime hardening 没把默认入口带偏：
  - `tests/test_openssl_ssl_load_contract.pas` => PASS (`18/18`)
  - `tests/test_openssl_ssl_unload_contract.pas` => PASS (`13/13`)
  - `tests/test_openssl_loader_ready_contract.pas` => PASS (`4/4`)
  - `tests/test_openssl_blake2_ready_contract.pas` => PASS (`2/2`)
  - `tests/test_factory_concurrent_initialization_contract.pas` => PASS (`13/13`)
  - `tests/openssl/test_openssl_features.pas` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
- review pass on the SSL unload batch found no additional correctness issues:
  - focused diff review over `src/fafafa.ssl.openssl.api.ssl.pas` and `tests/test_openssl_ssl_unload_contract.pas` reported no findings
  - residual risk remains on the load-side `osmSSL` ready contract, not on the unload cleanup that this batch changed
- review pass on the SSL load-side info/state batch also found no additional correctness issues:
  - focused diff review over `src/fafafa.ssl.openssl.api.ssl.pas` and `tests/test_openssl_ssl_load_contract.pas` reported no findings
  - residual risk now收窄到更宽的 declared helper family 边界：early-data / keylog / record-padding / async 等 surface 仍未在 `osmSSL` 里明确 required/optional 语义
- 当前默认入口的两处高优先级语义债已经在本轮脚本批次收口：
  - `scripts/compile_all_modules.py` 不再接受 `98%` 的 fail-open 通过条件，当前阈值已收紧到 `100%`
  - `scripts/run_minimal_ci_gate.sh` 不再依赖 `eval` 执行拼接字符串，改为在项目根目录下以参数数组/显式环境变量调用子脚本
- 这批改动的价值不在“跑起来还是不是绿”，而在“绿的含义更可信”：
  - compile gate 现在对单文件失败 fail-closed
  - minimal gate 不再把 `--modules` 等输入提升为 shell 语法
- 新增 focused contracts 把这两个语义锁住了：
  - `tests/scripts/test_compile_all_modules_fail_closed_contract.sh`
    - 用合成 `100` 个模块、`1` 个失败的场景验证 `99%` 不能再返回成功
  - `tests/scripts/test_minimal_ci_gate_module_argument_injection_contract.sh`
    - 用 shell metacharacters payload 验证 `--modules` 被当作数据而不是被执行
- 既有相关 contracts 也继续保持绿：
  - `test_compile_all_modules_fpc_host_units_override_contract`
  - `test_compile_all_modules_unit_output_isolation_contract`
  - `test_minimal_ci_gate_fpc_host_passthrough_contract`
  - `test_run_minimal_ci_gate_phase2_fast_local_passthrough_contract`
- fresh baseline 继续通过，说明这批 hardening 没把默认入口带偏：
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
    - compile gate => `182/182`
    - module suite => `17/17`
    - phase2 dry-run => PASS
- 下一优先级已经从“把这两处风险收口”切换到“扩展同类 runtime contract 审计面”：
  - 继续看其它仍无条件 `SetModuleLoaded(..., True)` 的 OpenSSL 模块是否也需要改成 helper-surface-ready / required-symbol fail-closed
  - 为更多 `Required: True` 调用点补 focused contracts，避免这次修复只锁住 AES / SHA / Modes / BLAKE2
  - `SSL` 的 unload-side 契约与 info/state load-side 契约已经对齐，下一步应继续定义其它 helper family 的 ready surface，而不是再让 `osmSSL=True` 覆盖过宽的“部分 helper 就绪”状态

## 2026-03-30 Findings (Repo review snapshot / evidence-only)
- fresh baseline 是“能跑通但不等于契约健康”：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - compile gate => `182/182`
  - module suite => `17/17`
  - phase2 dry-run => PASS
- style gate 仍是红的，并且已经和仓库自述 checklist 发生漂移：
  - `docs/AGENTS.md` 要求 review 前执行 `python3 scripts/check_code_style.py src`
  - 实际结果是 `257` 个 style errors，覆盖 `194` 个 Pascal 文件中的多个核心路径
- 当前仍然成立、且优先级最高的 repo-level 风险有四类：
  - `src/fafafa.ssl.factory.pas` 头注释宣称“所有类方法线程安全”，但 `GetLibrary(...)` / `IsLibraryAvailable(...)` 仍在锁外执行 `Initialize`，并且 `GetLibrary(...)` 会在初始化成功前把实例放入 `FLibraries[...]`；这让并发调用者可能拿到同一未完成初始化的实例，线程安全语义被高估
  - `scripts/compile_all_modules.py` 仍把 `98%` 成功率当作 pass 条件；由于多个 active docs 把它当成“默认编译门禁”，这依然属于 fail-open gate 设计
  - `scripts/run_minimal_ci_gate.sh` 仍通过 `eval "$cmd"` 执行拼接命令；`MODULE_SET` 和多组环境变量被拼进 shell 字符串，而不是参数数组
  - OpenSSL loader 的 loaded-state contract 仍偏弱：`TFunctionBinding.Required` 已存在，但 `TOpenSSLLoader.LoadFunctions(...)` 不消费它；AES / SHA / modes 仍可在关键符号未验证齐全时把模块标记为 loaded
- 当前测试保护更偏“脚本还能跑”，而不是锁住这些高风险语义：
  - 已有 contracts 覆盖 `compile_all_modules` / `run_minimal_ci_gate` 的参数透传与 fast-local 行为
  - 但当前没有看到针对 factory 并发初始化、compile gate fail-closed、或去除 `eval` 的 focused contract
- 结论：
  - 当前仓库的推荐门禁在这台机器上确实是绿的
  - 但下一轮更值得优先处理的不是“继续补功能”，而是把 gate / loader / factory 的 fail-open 与 race-prone 语义收紧

## 2026-03-30 Findings (Review follow-up closeout)
- 本批目标不是再做泛化 review，而是把上一轮已经确认的 drift 直接收口：
  - WinSSL `SetCertificateStore(...)` 语义与 `GetCAStoreHandle(...)` 使用路径脱节
  - active docs 仍把 hostname verification 写成不存在的 `.WithVerifyHostname`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md` 仍在讲历史性的“client context 自动加载 system CA”
  - `docs/PLATFORM_SUPPORT.md` 仍把 auto backend selection 过度简化成固定平台映射
- 本批应坚持最小修复：
  - WinSSL 先做 external-store handle fallback，不在这一轮扩展成 collection-store merge 设计
  - docs 改成当前真实入口：`.WithSystemRoots` + `TSSLConnector.Connect*(..., host)` / `ISSLClientConnection.SetServerName(...)`
  - verification 以新增 focused contracts + repo 默认 compile/minimal gate 为准
- 本批 closeout 已完成，并且保持了“最小修复、无 API 扩张”的边界：
  - `TWinSSLContext.GetCAStoreHandle(...)` 现在在 `FCAStore` 为空时，会回退到 `FExternalCertStore` 的 native handle
  - 这让 builder 注入的 system/external certificate store 能被 `TWinSSLConnection.ValidatePeerCertificate(...)` 的 WinSSL 链验证路径实际消费
  - 当前实现仍没有宣称“系统 store + 外部 store 合并”；它只是恢复了 external-store handle reachability
- active docs 已经回到当前 runtime contract：
  - `docs/guides/COMMON_PITFALLS.md` 和 `docs/guides/security-best-practices.md` 不再教授不存在的 `.WithVerifyHostname`
  - hostname/SNI 指南统一改成 connection-level：`TSSLConnector.Connect*(..., host)` 或 `ISSLClientConnection.SetServerName(...)`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md` 已改写为 current-state note，不再把 `Lib.CreateContext(sslCtxClient)` 说成 implicit CA autoload
  - `docs/PLATFORM_SUPPORT.md` 已改成 priority-based auto-detect 说明，并明确当前注册优先级：WinSSL 200、MbedTLS 175、WolfSSL 150、OpenSSL 100
- fresh verification 说明这波收尾没有把 baseline 带偏：
  - `bash tests/scripts/test_active_tls_guidance_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_context_external_store_contract.sh` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS (`17/17`, phase2 dry-run PASS)

## 2026-03-29 Findings (Repo review refresh / current worktree)
- fresh baseline 仍然是绿的：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - compile gate => `182/182`
  - module suite => `17/17`
  - phase2 dry-run => PASS
- style gate 仍然不是绿的：
  - `python3 scripts/check_code_style.py src` => FAIL
  - 当前输出仍是 `257` 个风格错误，覆盖 `194` 个 Pascal 文件中的多个核心路径
- 之前 repo review 里最危险的 `context.builder` system-roots 问题，当前树上已经不再成立：
  - `BuildClient(...)` / `BuildServer(...)` 现在都使用 `ContextBackend` 创建 system store
  - 因此这两条不应再作为“当前缺陷”继续上报
- 当前仍然成立、且更值得优先处理的 repo-level 风险有四类：
  - `TSSLFactory` 对外宣称“所有类方法线程安全”，但 `GetLibrary(...)` / `IsLibraryAvailable(...)` 仍在锁外执行 `Initialize`，并且实例会在完成初始化前被缓存/发布；这会让并发调用者在同一实例上重复进入初始化路径
  - `scripts/compile_all_modules.py` 仍把 `98%` 成功率当作 pass 条件；当前虽然跑到 `100%`，但 gate 语义本身仍然是 fail-open
  - `scripts/run_minimal_ci_gate.sh` 仍通过 `eval "$cmd"` 执行拼接命令；这会把模块列表和若干环境变量提升为 shell 解析输入，而不是参数数组
  - OpenSSL API 模块的 loaded-state contract 仍偏弱：loader 批量绑定函数时不消费 `Required` 元数据，而 AES / SHA / modes 等模块仍会在未确认关键符号全部就绪时标记模块为 loaded
- 结论：
  - 当前仓库的“能编译、能过最小门禁”状态是真实的
  - 但默认 gate 与运行时契约仍有几处 fail-open / race-prone 设计债，适合作为下一轮高优先级收口对象

## 2026-03-29 Findings (Context builder system-roots contract)
- `TSSLContextBuilder.BuildClient` 的 backend/store 错配问题已被 focused contract 证实并修复：
  - RED 时，显式 `sslMbedTLS` client context 实际收到的是 `sslOpenSSL` store
  - GREEN 后，client context 收到与 context 相同 backend 的 store
- `TSSLContextBuilder.BuildServer` 之前确实忽略了 `WithSystemRoots` 的 runtime 语义：
  - RED 时，server build 的 `SetCertificateStore` 调用次数为 `0`
  - GREEN 后，server build 会先 `LoadSystemStore`，再把 store 注入 context
- 本批修复方式保持了最小范围：
  - 没有改变 validation 规则
  - 没有改动 PEM / PKCS#11 / cipher / session 的既有顺序
  - 只把 context 实际 backend 显式化，并让 client/server 共用该 backend 的 store 创建路径
- focused + existing verification 都说明修复没有把 builder 现有行为带偏：
  - 新增 system-roots contract => PASS
  - `test_context_builder_try` => PASS (`42/42`)
  - `test_preset_configurations` => PASS (`35/35`)
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)

## 2026-03-29 Findings (Repo review / config + loader contract scan)
- fresh baseline 仍然是绿的，但 repo-level review checklist 不是全绿：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - `python3 scripts/check_code_style.py src` => FAIL（`257` style errors）
- 当前最值得优先处理的问题集中在 `context.builder` 和 OpenSSL loader contract，而不是 compile baseline：
  - `WithSystemRoots` 的运行时语义在 client/server、auto/explicit backend 路径上并不一致
  - loader 层的 “module loaded” 状态也没有完全等价于“关键符号已就绪”
- `BuildServer` 当前把 `WithSystemRoots` 当作验证配置的一部分，但实际构建时不会加载任何系统根证书：
  - `ValidateCommonBuilderSettings(...)` 会把 `FUseSystemRoots=True` 视为合法 CA 来源，并据此 suppress “no CA certificates configured” warning
  - 但 `BuildServer(...)` 只处理 `CAFile` / `CAPath`，没有对应的 `LoadSystemStore` / `SetCertificateStore` 路径
  - 这意味着服务端 mTLS / client-cert verification 可能“验证通过配置检查”，却在运行时根本没有加载系统根证书
- `BuildClient` 的 system-roots 路径还有 backend mismatch 风险：
  - context 可能由 `SelectedBackend` 或 `FExplicitBackend` 创建
  - 但 system certificate store 却固定通过 `TSSLFactory.CreateCertificateStore(sslAutoDetect)` 获取
  - `TOpenSSLContext.SetCertificateStore` / `TMbedTLSContext.SetCertificateStore` 都直接把 `AStore` 解释为各自 backend 的 native handle
  - 因此在显式 backend 与 auto-detect backend 不一致时，存在把“别的 backend 的证书存储句柄”注入当前 context 的风险
  - 本次 session 未做跨平台 runtime 复现，但这是直接由源码控制流和句柄类型约束推导出来的高风险结论
- OpenSSL module-loaded 合同仍偏弱：
  - `TOpenSSLLoader.LoadFunctions(...)` 虽然接收 `Required` 字段，但实现本身并不使用它来 fail/abort
  - 多个 API 模块仍然在未验证关键符号是否存在时直接 `SetModuleLoaded(..., True)`，例如 AES / SHA / modes
  - 这会让“模块已加载”的布尔状态和“函数指针可安全调用”的真实状态分离
  - 后续如果调用侧只信模块状态、不再判空，就可能把 nil symbol 漏到运行时
- 测试覆盖当前没有锁住最危险的两个 builder 组合：
  - `WithBackend(...) + WithSystemRoots`
  - `BuildServer + WithSystemRoots`
  - 当前已有的 validation / preset / try-build 测试能证明“builder 可构建”，但还没有证明 system roots 真被加载到正确 backend 的 context 上

## 2026-03-28 Findings (Repo scan / architecture review)
- fresh baseline 并不是红的：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - compile gate inside minimal CI = `182/182`
  - module suite = `17/17`
  - phase2 dry-run = PASS
- 但当前仓库仍然有几个值得优先处理的结构性问题，且它们和“当前推荐入口”直接相关：
  - 文档把 `python3 scripts/compile_all_modules.py` 写成默认构建/编译门禁
  - `run_minimal_ci_gate.sh` 也直接调用它
  - 可这个脚本自身却允许 `98%` 成功率即返回成功
- 这意味着默认 compile gate 当前是 fail-open，而不是 fail-closed：
  - 在 182 个核心模块的当前规模下，最多约 3 个模块编译失败仍可能被判定为绿
  - 这与 README / docs/AGENTS / minimal gate 给人的“默认编译门禁”语义不一致
- 工厂层还有一个更隐蔽、但更危险的并发语义问题：
  - `src/fafafa.ssl.factory.pas` 头部声明“所有类方法线程安全”
  - 但 `GetLibrary(...)` 会在持锁区内把 library instance 发布到 `FLibraries[...]`
  - 随后在锁外调用 `Result.Initialize`
  - 这让并发调用者可能在同一个实例上重复进入 `Initialize`
  - 这条结论是基于源码控制流的推断，当前 session 未做多线程复现
- 最小门禁脚本本身还有 shell 安全面的设计债：
  - `run_cmd` 使用 `eval "$cmd"`
  - `module_cmd` / phase2 / TLS13 bench 命令都由参数和环境变量拼接得到
  - 这会把调用方可控值提升为 shell 解析输入，而不是参数数组
- 仓库约定层面，文档要求“评审前执行 `python3 scripts/check_code_style.py src`”，但当前这个 gate 处于红态：
  - 本次实际运行得到 `257` 个 style errors
  - 说明“仓库自述的 review checklist”与当前 worktree 状态并未完全对齐
- OpenSSL loader 还有一个生命周期 residual risk：
  - `UnloadLibraries` 先释放 `libcrypto`，后释放 `libssl`
  - 而依赖方向实际上是 `libssl -> libcrypto`
  - 当前 session 未见直接触发点，但如果后续真把这个 API 用到 teardown 路径，建议按逆依赖顺序释放

## 2026-03-27 Findings (Repo review + targeted fix)
- 当前仓库不是干净基线，而是一个大规模脏 worktree：
  - `git status --short` 显示大量已修改文件与新增合同测试
  - 因此“审查项目”必须建立在 fresh command evidence 上，不能靠 diff 体感或历史结论猜测
- 本批的 working rule:
  - 先跑仓库默认门禁
  - 只抓第一个 stable failure
  - 只改与该 failure 直接相关的最小文件集合
  - 不碰用户已有的无关变更
- fresh baseline 与补充探针都没有给出新的 runtime RED：
  - `python3 scripts/compile_all_modules.py` => PASS（`182/182`）
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - 3 个 `cert.utils` symbol/nil-result contracts 继续全绿
- 因此这批实际最有价值的可复现问题，不是功能失败，而是 `src/fafafa.ssl.tls.pas` 的稳定编译 warning surface：
  - 直接用 `compile_all_modules.py` 同款 FPC 参数编译该单元时，会稳定出现 3 条 file-local warning
  - 这些 warning 都能在一个独立 shell contract 中复现，不依赖更大 gate 的偶然输出
- root cause 收敛到两个局部点：
  - `TSSLStream.Seek` 总是抛 `EStreamError`，但在抛出前没有给函数结果赋值，因此触发 “Function result does not seem to be set”
  - `TSSLConnector.FromContext` / `TSSLAcceptor.FromContext` 对带接口字段的 advanced record 结果使用 `FillChar(Result, SizeOf(Result), 0)`，触发 managed-result 未初始化 warning
- 最小安全修法是不改变 facade 语义，只消掉编译面的不确定性：
  - `Seek` 仍然抛 “TLS stream is not seekable”
  - connector/acceptor 的默认 timeout / blocking / session 初始化语义保持不变
- `src/fafafa.ssl.tls.pas` 在当前树上原本就带有一段未提交的 `ApplyClientOptions` 逻辑改动；
  - 本批没有回滚或改写那段 in-flight diff
  - 只在 3 个 warning 位置叠加了最小修补
- focused verification 证明这批修补没有把 facade 行为带偏：
  - 新增 `tests/scripts/test_tls_facade_warning_contract.sh` 从 RED 变 GREEN
  - `tests/test_tls_connector_hostname_override_precedence.pas` 继续通过 `6/6`
- 当前树上仍然有其它 warning cluster，但不属于本批最小修复范围：
  - 编译 `tests/test_tls_connector_hostname_override_precedence.pas` 时，`src/fafafa.ssl.connection.base.pas` 仍出现 3 条 managed-result warning
  - 如果继续审查下一轮，`connection.base` warning cleanup 是一个合理候选

## Repo-wide Closeout Ledger (2026-03-25)
- 当前最重要的 repo-level 结论不是“还有很多 family 可以继续摸一遍”，而是：
  - `Milestone 1` 已 guarded/frozen
  - `Milestone 2` 当前没有 fresh cert-utils RED
  - `Milestone 3` 已 frozen on current worktree
  - `Milestone 4` 需要把这些状态显式写成 milestone-level ledger
- 现在真正需要防止的是 working memory 自己制造 reopen 噪音：
  - family-level logs 很完整
  - 但如果顶部没有 milestone summary，后续 session 很容易再次把已经 green 的线误当成“下一步待做”
- 因此本轮最小正确动作不是再跑一遍实现测试，而是补齐一个清晰的 reopen contract：
  - closed family stays closed
  - only fresh failing contract or baseline regression can reopen it
- 结论：
  - working memory 现在应优先服务“防止无意义重开”，而不是继续扩张下一队列
  - 后续推进默认应从新 RED 起步，而不是从历史 closeout 文本里找可以继续拆的小块

## 2026-03-25 Findings (SSL/TLS backend completeness roadmap + FreePascal TLS 1.3 AES256/SHA384 parity)
- 当前最需要纠正的认知偏差是：
  - `repo closeout roadmap` 不等于 `SSL/TLS product completeness roadmap`
  - 旧 closeout ledger 只能说明已做 family 的收口状态，不能说明“所有后端都完整了”
- 代码证据继续支持一个更诚实的产品判断：
  - 公共接口面已经很广
  - 各后端完成度并不等价
  - pure Pascal backend 已存在且已注册，但仍是 partial / in-progress，而不是 rustls 级别完成态
- 本批完成后，pure Pascal backend 的一个关键缺口已经收口：
  - 不再只是 `TLS_AES_128_GCM_SHA256 / TLS_CHACHA20_POLY1305_SHA256`
  - `TLS_AES_256_GCM_SHA384` 现在也已打通到 core TLS 1.3 path
  - 对齐面包括：
    - suite-aware Finished key / verify_data
    - ClientHello advertize
    - server-side cipher intersection
    - runtime capability declaration
    - `KnownIssues` / `IsCipherSupported`
- 这个 family 也暴露了一个重要结构性事实：
  - pure Pascal backend 不是“完全没有 SHA384 能力”
  - AEAD、HKDF、key schedule、application schedule 其实早已具备 SHA384 path
  - 真正阻塞的是连接层仍把 Finished 逻辑写死在 SHA256，并且 ClientHello / server selection 没把 AES256 套件接进来
- 这意味着后续推进应该继续沿着“从接口声明漂移切到连接层真实 contract”的方式做，不要只看 capability docs。
- 仍然未完成、因此不能把 pure Pascal backend 说成 rustls-level 的项目包括：
  - PSK / session resumption
  - 0-RTT / early data
  - OCSP stapling
  - Certificate Transparency
  - post-handshake auth
  - 更完整的 certificate validation / enterprise feature parity
- 结论：
  - 当前最合理的下一批不是再做 SHA384 补丁，而是切到 `session resumption / PSK`
  - pure Pascal backend 的“核心 TLS 1.3 modern suite baseline”已经比上一批更接近 rustls，但距离“至少 rustls 水平”仍有明确差距

## 2026-03-25 Findings (FreePascal TLS 1.3 client session resumption / PSK)
- 这一批完成后，pure Pascal backend 不能再被笼统描述为“PSK / session resumption 仍在进行中”：
  - `client-side session resumption / PSK` 已经闭环
  - 闭环路径是：
    - `NewSessionTicket`
    - `ISSLSession`
    - `SetSession(...)`
    - resumed TLS 1.3 handshake
- 当前实现已经覆盖的关键合同包括：
  - 从 `master_secret + transcript_hash + ticket_nonce` 派生 resumption PSK
  - 从 partial ClientHello transcript 计算 binder
  - `ClientHello.pre_shared_key` 正确作为最后一个 extension 输出
  - `ServerHello.pre_shared_key(selected_identity)` 可被 parser 与连接层识别
  - 连接层 `IsSessionReused` 与真实握手结果一致
- 这批也暴露了一个关键正确性点：
  - 之前 `SendClientFinished(...)` 没有先把 client Finished append 到 transcript
  - 这会让后续 application secrets 与服务端不一致
  - 修掉这个问题后，离线 resumed-handshake contract 才真正稳定
- 因此，pure Pascal backend 当前已经具备的 TLS 1.3 基线能力应更新为：
  - modern cipher-suite parity，包含 `TLS_AES_256_GCM_SHA384`
  - client-side `session resumption / PSK`
- 距离 `rustls` 级别完整度仍然明显落后的部分，现在应更精确地描述为：
  - `server-side session resumption / PSK`
  - `0-RTT / Early Data`
  - `OCSP stapling`
  - `Certificate Transparency`
  - 更完整的 certificate validation / enterprise parity
- 结论：
  - pure Pascal backend 已不再只是“有 session API、但恢复路径不完整”
  - 下一条最有价值的 family 应直接转向 `server-side session resumption / PSK`，而不是继续围绕 client-side PSK 做零碎补丁

## 2026-03-26 Findings (FreePascal TLS 1.3 server session resumption / PSK)
- 这一批完成后，pure Pascal backend 不能再把 `server-side session resumption / PSK` 继续列成 remaining gap：
  - 服务端现在已经具备完整的 `issue ticket -> cache session -> accept resumed PSK handshake` 闭环
  - `NewSessionTicket`、context cache、resumed `ServerHello.pre_shared_key(selected_identity=0)`、`IsSessionReused` 已经对齐
- 这条 family 里真正重要的 residual risk 不是“还有没有 cache”，而是 binder gate 是否 fail-closed。
  - 当前已经不再采用“cache hit 即 resume”的宽松门禁
  - `DoAccept(...)` 现在会在 ticket 命中后强制校验 binder
  - binder mismatch 会直接拒绝恢复，而不是悄悄继续走 resumed path
- 严格 gate 打开后暴露了一个真实根因，而不是假阳性：
  - client-side / test-side 构造 PSK `ClientHello` 时，先生成 partial 算 binder，再重新生成 full `ClientHello`
  - 第二次构造会重新生成新的 `random` / `legacy_session_id`
  - 结果是 binder 绑定的 transcript 与最终发送出去的 `ClientHello` 并不是同一份
- 为了把这个 correctness 问题收口，当前树上新增了更安全的构造路径：
  - `BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(...)`
  - 它会复用同一份 `random` / `session_id` 生成 partial/full `ClientHello`
  - 这样正常 resumed handshake 与 fail-closed binder test 才能同时成立
- 因此 pure Pascal backend 当前已经具备的 TLS 1.3 基线能力应更新为：
  - modern cipher-suite parity，包含 `TLS_AES_256_GCM_SHA384`
  - client-side `session resumption / PSK`
  - server-side `session resumption / PSK`
- 距离 `rustls` 级别完整度仍然明确落后的部分，现在收敛为：
  - `0-RTT / Early Data`
  - `OCSP stapling`
  - `Certificate Transparency`
  - 更完整的 certificate validation / enterprise parity
- 结论：
  - pure Pascal backend 已经从“只有 modern TLS 1.3 套件 + 单侧 resumption”推进到“双侧 TLS 1.3 resumption / PSK 可用”
  - 下一条最有价值的 family 应切到 `0-RTT / Early Data`，随后是 `OCSP stapling / CT / validation hardening`

## 2026-03-25 Findings (Milestone 3 context/SNI compatibility drift freeze)
- 当前 worktree 上已经看不到新的 `Milestone 3` 实现缺口；builder/runtime/validation 三个面已经对齐到目标兼容边界：
  - `ValidateClient` 会把 deprecated context-level SNI 指向 `ISSLClientConnection.SetServerName(...)`
  - `ValidateServer` 会明确说明 server-side connections ignore deprecated context-level `ServerName`
  - `BuildClient` / `BuildServer` 都保留了 legacy context `ServerName` state 在 built context 上的可观察性
  - 但 server-side `CreateConnection(...)` 仍按既有 contract 忽略该 legacy state
- fresh focused evidence 证明这不是“源码看起来像对”，而是当前树上的合同面确实全绿：
  - compatibility labels contract 通过
  - `test_config_validation` 通过 `53/53`
  - `test_connection_builder_hostname_precedence` 通过 `9/9`
  - `test_tls_connector_hostname_override_precedence` 通过 `6/6`
  - `test_freepascal_context_server_name_inheritance` 通过 `2/2`
  - `test_context_builder_server_servername_runtime_consistency` 通过 `6/6`
- 相邻 integration / milestone 证据也没有把这条线重新打开：
  - `test_cross_backend_consistency_contract` 在网络 gate 关闭时按预期绿通过
  - `test_cross_backend_errors_contract` 在环境 gate 关闭时按预期 skip 并整体绿通过
  - `python3 scripts/compile_all_modules.py` 继续是 `181/181`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` 继续全绿
- 结论：
  - `Milestone 3` 现在应视为 frozen on current worktree
  - 后续若没有新的 failing contract 或 regression baseline，不应再把 context/SNI compatibility 当作开放实现问题重开
  - 下一批代码工作必须从新的 RED 出发，而不是重复 builder/SNI 审计
  - `Milestone 4` 的价值现在主要是保持这种 closeout evidence 可追溯，而不是再扩一个新的 compatibility family

## 2026-03-25 Findings (context builder merge advanced option snapshot semantics re-closeout)
- 这批不是“新发现一个 merge 语义话题”，而是一个真实 reopen：
  - `progress.md` 里已经记录过 2026-03-09 的 `builder merge advanced option snapshot semantics` closeout
  - 当前磁盘 `src/fafafa.ssl.context.builder.pas` 却仍保留 pre-fix 逻辑：
    - `server_name` / `alpn_protocols` 只有 non-empty 才覆盖
    - `options` 只有 `Count > 0` 才覆盖
    - `ocsp_stapling_enabled` / `ocsp_stapling_required` 根本没从 source snapshot 复制
  - 同时，当时的 focused test 文件当前也不在树上，所以不能假设“历史上绿过一次”就代表现在还是绿
- fresh focused RED 证明这不是 working-memory 噪音，而是当前树上的真实行为缺口：
  - empty `server_name` 没有清掉 target 旧值
  - empty `alpn_protocols` 没有清掉 target 旧值
  - explicit `options=[]` 没有清空 target option set
  - stale OCSP booleans 在 merge 后继续泄漏
  - source `ocsp_stapling_enabled=true` / `required=true` 也不会在 merge 后落到 target
- 最小正确修法仍然只需要限定在 `Merge(...)`：
  - 字段只要在 source JSON 里出现，就复制 `server_name` / `alpn_protocols`，哪怕为空
  - `options=[]` 要被视为 authoritative source snapshot，而不是“没设置”
  - `ocsp_stapling_enabled` / `ocsp_stapling_required` 要和别的 advanced snapshot 字段一样一起复制
- 结论：
  - 当前树上的 `Merge(...)` advanced-option snapshot family 已通过 fresh RED/GREEN 重新收口
  - 这次 closeout 不是扩 scope，而是把曾经存在但当前盘上缺失的 merge semantics 补回

## 2026-03-25 Findings (Milestone 1 doc guardrail contracts)
- 当前最合理的继续方式不是再扩一个新的 docs family，而是把已经收口的 2026-03-25 docs families 变成 shell contract guardrails。
- 现有仓库里已经有足够一致的 docs contract 形状可复用：
  - `tests/scripts/test_canonical_wave_c_entrypoint_convergence_contract.sh`
  - `tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`
  - `tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
- 因此这批的最小实现不是继续手工扫文档，而是补 3 个 focused contracts：
  - support docs legacy script guidance
  - platform support guidance convergence
  - active docs historical reference labels
- fresh verification 暴露的两个失败都来自新 contract 自身，而不是文档再次漂移：
  - historical-label contract 里 `rg` 没有在 pattern 前加 `--`，导致以 `-` 开头的 README bullet 被误当成命令行参数
  - platform-support contract 把 Windows PowerShell 脚本路径写死成 `tests/run_core_tests.ps1` / `tests/run_winssl_tests.ps1`，但当前文档是在 `cd tests` 后使用 `.\run_core_tests.ps1` / `.\run_winssl_tests.ps1`
- 最小安全修补因此只需要收窄在 contract 本身：
  - 给 `rg` pattern 匹配补 `--`
  - 把 Windows 断言从 repo-relative 路径放宽到真实脚本名
- 进一步按 best practice 做一致性复核后，`test_support_docs_legacy_script_guidance_contract.sh` 也补上了同样的 `rg --` 防护，避免未来 pattern 以 `-` 开头时再次出现同类误报。
- 结论：
  - 这批完成后，Milestone 1 才能从“已编辑文档”升级成“有回归防线的 frozen milestone”
  - 当前结果是 3 个新 contract + 2 个既有 docs contracts 全绿，Milestone 1 已达到 guarded/frozen 状态

## 2026-03-25 Findings (platform support guidance convergence)
- `docs/PLATFORM_SUPPORT.md` 剩下的是一个独立的 platform-semantics family，而不是前面 supporting docs 的延长线：
  - Linux section 仍把 `build_linux.sh` / `run_core_tests.sh` 当默认 build/test path
  - macOS section 仍把 `build_macos.sh` 当默认入口
  - 但 Windows section 用的 `tests/run_core_tests.ps1` / `tests/run_winssl_tests.ps1` 仍是实际存在的脚本，不应混进同一修复方式
- 这页文档的最小一致修复不是把所有平台都强行改成同一条命令，而是：
  - Linux 对齐当前 canonical chain
  - macOS 改成当前 focused smoke 起点
  - Windows 保持现有真实可用 PowerShell 入口
- 结论：
  - `platform support guidance convergence` 适合作为单文件 docs-only family 收口
  - 关键不是“删掉旧脚本名”，而是停止把它们写成当前默认动作

## 2026-03-25 Findings (support docs legacy script guidance convergence)
- 在主入口和次级 quickstart guidance 已收口后，supporting docs 里仍残留一条更窄但一致的 drift：
  - `docs/FCL_DEPENDENCIES.md` 继续把 `build_linux.sh` / `run_tests_linux.sh` 当成默认验证动作
  - `docs/testing/TEST_COVERAGE_ASSESSMENT.md` 的 automated-suite 描述仍停留在同一组旧脚本
- 这批适合单独成一个 family，因为它们都不是“当前总入口”，但都会把读者重新带回历史脚本路径。
- 最小一致修复是：
  - 保留这些文档原本的主题
  - 只把当前验证入口统一到 canonical chain
  - 对历史 assessment 文档，补一层“这是历史快照，但当前验证请走新入口”的说明
- 结论：
  - `support docs legacy script guidance convergence` 可以作为单独 docs-only family 收口
  - `docs/PLATFORM_SUPPORT.md` 仍然是候选，但因平台语义更重，应在下一批单独处理

## 2026-03-25 Findings (secondary build/test guidance convergence)
- 当前更值得推进的不是重开已经冻结的 `context.builder` / `cert-utils` family，而是 roadmap Milestone 1 里还残留的次级 active guidance drift。
- 这批文档漂移具有同一个 family 形状：
  - `docs/AGENTS.md` 仍把 `build_linux.sh` 写成默认 build 入口
  - `docs/guides/LINUX_QUICKSTART.md` 不仅继续推荐 `build_linux.sh`，还把仓库里并不存在的 `run_tests_linux.sh` 当成默认测试命令
  - `docs/guides/QUICKSTART_30SEC.md` 在 local-first 异常处理里仍把历史 `B127` 页面放在 first-stop 位置
- 最小一致修复不是删除历史入口，而是把它们重新分层：
  - `python3 scripts/compile_all_modules.py` 作为默认编译门禁
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` 作为默认本地最小门禁
  - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local` 作为当前 Phase 2 入口探测
  - `build_linux.sh` / `B127` 继续保留，但只作为历史兼容或历史 troubleshooting 参考
- 结论：
  - `secondary build/test guidance convergence` 可以作为一个独立 docs-only family 正式收口
  - 本批不需要触碰 `src/` 或脚本实现；关键是停止让次级文档把人重新带回旧入口

## 2026-03-25 Findings (context builder backend selection serialization/merge closeout rerun)
- 这条 family 在当前工作树里已经不再是开放 bug；本轮缺的是 fresh evidence 和 closeout writeback。
- explicit backend pin 的 serialization/merge 合同仍然保持完整：
  - JSON round-trip 继续保留 `.WithBackend(...)` 设下的 explicit backend
  - INI round-trip 继续保留同一 explicit backend pin
  - `Merge(...)` 继续把 source builder 的 explicit backend pin 带到 destination
- 相邻 runtime 面也没有出现“状态面绿了但 build path 回退了”的反向漂移：
  - `TryBuildClient` 仍会在 explicit unavailable backend 下稳定失败
  - preset 默认行为没有被 explicit backend state surface 误伤
- 结论：
  - `backend selection serialization/merge` 这条 explicit-backend family 现在可视为已正式收口
  - 后续若没有新的 failing contract，不应再把这条 state-surface family 当成待修代码问题重开

## 2026-03-25 Findings (context builder backend selection state contract closeout rerun)
- `Clone/Reset` 这条 backend-selection state family 在当前工作树里同样没有 fresh RED。
- 当前 snapshot/reset 语义仍与原合同一致：
  - `Clone` 继续复制 backend-selection state，而不是只复制证书/协议/session 等字段
  - `Reset` 继续回到 constructor defaults，不再泄漏旧 explicit backend 或 auto-selection mode
- 这说明 `backend-selection state` 已经不只是“历史上修过一次”，而是当前磁盘源码仍然维持着正确的 snapshot contract。
- 结论：
  - `backend selection state contract` 现在可视为已正式收口
  - 没有新的 failing contract 时，不要再为了“继续”重开 `Clone/Reset` 这条线

## 2026-03-25 Findings (non-SNI context.builder cluster freeze and cert-utils bounded discovery)
- 在补齐剩余两条 backend-selection family 的 fresh closeout 后，non-SNI `context.builder` cluster 的核心证据继续一致：
  - transformation/export-state 仍是 `45/45`
  - validation 仍是 `53/53`
  - selector minimum-score filtering 仍是 `1/1`
  - 全量 `tests/config/*.pas` 审查继续全绿
  - `compile_all_modules.py` 继续是 `181/181`
  - milestone-level `run_minimal_ci_gate.sh --fast-local` 也继续全绿
- 这说明当前工作树上的 non-SNI builder drift 已经从“可能还有隐藏 gap”收敛为“没有 fresh RED 就不应再重开”的稳定基线。
- bounded cert-utils discovery 也没有给出新的 family-level reopen 信号：
  - 2026-03-24/25 的 cert-utils plan 没有未写回的 orphan family
  - 当前 `GenerateSelfSigned(...)` / `GenerateSigned(...)` 成功路径里，真正会影响“结果已 materialized 后还能否保住成功”的边界，已经在近期 closeout family 中被单独覆盖
  - 本轮只读审查没有暴露出新的稳定 successful-path RED
- 结论：
  - non-SNI `context.builder` cluster 现在可视为 frozen on current worktree
  - Milestone 2 当前也没有新的稳定 cert-utils RED；后续只有拿到新的 failing contract，才值得创建新的 cert-utils family 并进入 TDD

## 2026-03-25 Findings (context builder Override explicit backend parity closeout rerun)
- 这条 family 在当前工作树里已经不再是开放 bug；本轮缺的是 fresh evidence 和 working-memory convergence。
- 当前 `Override(...)` 的 explicit-backend 边界仍和原计划一致：
  - 识别 `explicit_backend`
  - 通过 `TryParseLibraryTypeValue(...)` 接受 case-insensitive symbolic names 与 numeric ordinals
  - 赋值时设置：
    - `FExplicitBackend`
    - `FExplicitBackendSet := True`
    - `FAutoSelectBackend := False`
- 这说明最初的 root cause 仍被正确封住：
  - 问题不只是少支持一个 override 字段
  - 更关键的是 stale auto-backend state 不应继续压住调用方新的 explicit backend intent
- 本轮 fresh focused / adjacent evidence 继续证明这不是局部偶然通过：
  - transformation/export state 仍是 `45/45`
  - runtime auto-backend regression 仍是 `42/42`
  - import/export 与 snapshot/clone 相邻回归继续全绿
  - compile gate 继续是 `181/181`
- 结论：
  - `Override explicit backend parity` 现在可视为已正式收口
  - 后续若没有新的 failing contract，不应再把这条 runtime-significant explicit-backend family 当成待修代码问题重开

## 2026-03-25 Findings (context builder Override OCSP stapling parity closeout rerun)
- 这条 family 在当前工作树里已经不再是开放 bug；本轮缺的是 fresh evidence 和 working-memory convergence。
- 当前 OCSP state 仍保持计划要求的统一状态机：
  - `Override('ocsp_stapling_enabled', ...)` / `Override('ocsp_stapling_required', ...)` 继续通过 fluent path 改状态
  - `WithOCSPStapling(False)` 会先清掉：
    - `ssoEnableOCSPStapling`
    - `ssoRequireOCSPStapling`
    - `FOCSPStaplingRequired`
  - 然后才执行 `SyncOCSPStaplingOptions`
- 这说明 2026-03-20 暴露出的核心 root cause 仍被正确封住：
  - 问题不只是少识别两个 override 字段
  - 更关键的是 option-coupled state pair 不能再被 stale `FOptions` 反向污染
- 本轮 fresh focused / adjacent evidence 继续证明这不是局部偶然通过：
  - transformation/export state 仍是 `45/45`
  - runtime context-options 仍是 `42/42`
  - import/export 与 snapshot/clone 相邻回归继续全绿
  - standalone `test_ocsp_stapling_integration.lpr` 也继续可执行
- 结论：
  - `Override OCSP stapling parity` 现在可视为已正式收口
  - 后续若没有新的 failing contract，不应再把这条 option-coupled OCSP family 当成待修代码问题重开

## 2026-03-25 Findings (context builder PKCS#11 PIN order sensitivity closeout rerun)
- 这条 family 的原始 RED 在当前工作树里同样已经不存在；本轮价值在于把“setter ordering 已闭环”正式写回 working memory。
- 当前源码已经把顺序敏感性的根因明确压住：
  - `HasExplicitNonValuePKCS11PINMethod(...)` 作为独立 guard 存在
  - `WithPKCS11PIN(...)` 只有在未显式选择非 value method 时才默认把 method 设为 `pmValue`
  - `Override('pkcs11_pin', ...)` 也保持同样语义
- 因而当前 builder state 合同已经和计划目标一致：
  - 调用方先选 `pmEnvironment` / `pmFile`，再赋 env name / file path，不会再被静默降级成 direct PIN
  - override path 也不再因为后续 `pkcs11_pin` 赋值而丢掉已选 method
  - 只有 `pkcs11_pin` 单独出现时，才继续保留 direct-PIN 默认兼容语义
- 本轮 fresh evidence 也说明这不是“只在状态面上看起来对”：
  - `TryBuildServer` 仍通过 environment-variable source-resolution failure 暴露 ordered fluent path 的真实 runtime 语义
  - `ExportToJSON` 仍让 ordered override path 的 env/file method export-visible
  - 相邻 import/export regression 继续兜底 direct-PIN default 与 named method import 语义
- 结论：
  - `PKCS#11 PIN order sensitivity` 现在可视为已正式收口
  - setter 保序、runtime support 边界、secret serialization 仍是三层不同合同，后续不要再把它们混成一个 batch


## 2026-03-25 Findings (GenerateSigned private-key export post-success cleanup family closeout)
- 这批不是重新打开 signed outer cleanup family，而是继续 roadmap Milestone 2 的更窄 successful-path discovery：
  - 聚焦 `GenerateSigned(...)` private-key PEM export `BIO_free(LBIO)` cleanup
  - 只覆盖 successful second `BIO_read(...)` 之后才发生的 delayed helper loss
- 新 family contract 证明当前实现还存在一个真实但很窄的 false-failure gap：
  - certificate PEM 与 private-key PEM 都已经 materialized
  - direct `GenerateSigned(...)` 仍会在 private-key export `finally BIO_free(LBIO)` 处抛 `ESSLCertError`
  - `TryGenerateSigned(...)` 不抛，但会把已经 materialized 的输出清空并返回 `False`
- 这个 family 的关键边界和前面的 self-signed sibling line 一样，不是“helper missing 一律吞掉”，而是保留两种语义：
  - entry-missing `BIO_free`：继续维持旧的 controlled failure
  - delayed-loss after successful PEM materialization：必须保住 `Result=True` 和 PEM 输出
- 最小安全修复只需要限定在 `GenerateSigned(...)` private-key PEM export cleanup boundary：
  - 进入函数后记录 `LHadBIOFreeAtEntry`
  - 在 private-key export `finally` 中，`BIO_free` 仍 assigned 时正常 cleanup
  - `BIO_free` 已丢失时，只有“不是 post-success delayed-loss”才继续抛 `Required private key PEM export BIO cleanup helper is unavailable`
- 这批还澄清了一个 contract 分类问题：
  - broad `tests/test_cert_utils_generate_signed_bio_contract.pas` 仍然是 entry-missing baseline，不该被改成 delayed-loss success 语义
  - `tests/test_cert_utils_generate_signed_private_key_bio_free_symbol_contract.pas` 虽然名字像 symbol contract，但它实际上是 wrapper-based delayed-loss scenario
  - 因此它应该与新 family 一样对齐到 preserved-success semantics，而不是继续期待 failure
- 结论：
  - `GenerateSigned(...)` private-key export post-success cleanup family 已闭环
  - true entry-missing `BIO_free` failure contract 仍被保留
  - overlap 的 legacy private-key `BIO_free` test 已对齐到其真实 delayed-loss success 语义
  - 下一批应继续 fresh discovery，而不是回头重开这条 signed private-key export cleanup 线

## 2026-03-25 Findings (GenerateSigned post-success cleanup family closeout)
- `GenerateSigned(...)` 的 remaining outer cleanup line 和刚收掉的 `GenerateSelfSigned(...)` family 对称，但范围更完整：
  - `X509_free(LCert)`
  - `EVP_PKEY_free(LKey)`
  - `EVP_PKEY_free(LCAKey)`
  - `X509_free(LCACert)`
- 新 family contract 证明当前实现确实还存在系统性的 post-success gap：
  - helper 在 entry 时仍可用
  - certificate/key PEM 已经 materialized
  - 只要 outer cleanup helper 在之后延迟丢失，direct `GenerateSigned(...)` 就会在 unwind 中抛 `ESSLCertError`
  - `TryGenerateSigned(...)` 则把已经生成的输出清掉并返回 `False`
- 这批的关键语义和 self-signed family 相同：
  - entry-missing helper 仍然应该保持 old controlled failure
  - delayed-loss after successful materialization 则必须保住 `Result=True` 和输出
- 最小安全修复仍然只需要限定在 `GenerateSigned(...)` 的 outer cleanup sites：
  - 进入函数后记录 `LHadX509FreeAtEntry` / `LHadEVPPKeyFreeAtEntry`
  - outer `finally` 中 helper 仍 assigned 时正常 cleanup
  - helper 已丢失时，只有“不是 post-success delayed-loss”才继续抛 controlled `ESSLCertError`
- 这批还暴露出一个重要的 historical-contract distinction：
  - `tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas` 和 `tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas` 仍然是 entry-missing contracts
  - `tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas` 则一直是 wrapper-based delayed-loss scenario，不是 entry-missing
  - 因此它在本批不应继续期待 failure，而应升级成 preserved-success coverage
- 结论：
  - `GenerateSigned(...)` outer post-success cleanup family 已闭环
  - 真正的 entry-missing contracts 被保留
  - legacy CA `X509_free` focused contract 已对齐到新的 delayed-loss success contract
  - 下一批应继续 fresh discovery，而不是回头重开这组 signed outer cleanup sites

## 2026-03-24 Findings (GenerateSelfSigned post-success cleanup family closeout)
- 先前已经 green 的 `GenerateSelfSigned(...)` outer `X509_free` / `EVP_PKEY_free` symbol-guard family，只覆盖了“helper 在进入函数前就缺失”的 controlled-failure contract；它们并没有证明“成功结果已经形成后，late cleanup helper loss 是否还会把成功翻掉”。
- 新 family contract 证明当前实现确实还存在更窄的 gap：
  - `BIO_read(...)` wrapper 可以在第二次成功 PEM 读取后才清掉 `X509_free`
  - `X509_free(...)` wrapper 可以在证书 cleanup 成功后才清掉 `EVP_PKEY_free`
  - 结果是 direct `GenerateSelfSigned(...)` 会在 PEM 已 materialized 后仍抛 `ESSLCertError`
  - `TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` 会把已经生成的输出清掉并返回 `False`
- 这批的关键边界不是“cleanup helper missing 就一律吞掉”，而是保留两种语义：
  - helper 在 entry 就缺失：继续走旧的 controlled failure
  - helper 只在 `Result=True` 且输出已 materialized 后延迟丢失：成功和 PEM 输出必须保住
- 最小安全修复只需要限定在 `GenerateSelfSigned(...)` 的两个 outer cleanup sites：
  - 进入函数后立刻记录 `LHadX509FreeAtEntry` / `LHadEVPPKeyFreeAtEntry`
  - 在 outer `finally` 中，如果 helper 仍 assigned 就正常 cleanup
  - 如果 helper 已丢失，只有在“不是 post-success delayed-loss”时才继续抛 controlled `ESSLCertError`
- 这次修复不需要重新设计：
  - key generation
  - X509 construction/signing
  - PEM export read/write
  - earlier cleanup families
- 结论：
  - `GenerateSelfSigned(...)` post-success cleanup family 已闭环
  - entry-missing contract 被保留
  - 下一批应继续看 `GenerateSigned(...)` 或其它 remaining post-success cleanup families，而不是重开已经 green 的 self-signed outer cleanup line

## 2026-03-24 Findings (GenerateSigned PEM export BIO_new nil-result family closeout)
- 这批重新回到 roadmap 的 Milestone 2，但没有重放已绿的 `BIO_new symbol becomes unavailable` 线路，而是专门验证一个更窄的新差异：
  - `BIO_new` 仍然 assigned
  - helper gate 仍然通过
  - 但 local constructor `BIO_new(...)` 返回 `nil`
- fresh source review 显示 `GenerateSelfSigned(...)` 已经在两个 export constructor 后做了本地 `LBIO=nil` 检查，而 `GenerateSigned(...)` 没有：
  - certificate export path lacks `Failed to create BIO for certificate export`
  - private-key export path lacks `Failed to create BIO for key export`
- 新 family contract 证明当前 signed path 的问题不是 raw crash，但仍然越过了正确的边界：
  - certificate PEM export nil-result scenario currently raises `Failed to write certificate to PEM`
  - private-key PEM export nil-result scenario currently raises `Failed to write private key to PEM`
  - 也就是说，当前实现把 `nil` BIO 继续传给了后续 PEM write helper，而不是在 constructor boundary 直接停下
- 这个 RED 比单纯“还会抛 `ESSLCertError`”更有价值，因为它说明：
  - public contract 还算 controlled
  - 但 local failure attribution 不准确
  - signed/selfsigned 两条路径在同一类 constructor failure 上不一致
- 最小安全修复只需要两处本地 guard：
  - after signed certificate export `LBIO := BIO_new(LBIOMethod)`
  - after signed private-key export `LBIO := BIO_new(LBIOMethod)`
  - 复用已有 selfsigned path 的 message：
    - `Failed to create BIO for certificate export`
    - `Failed to create BIO for key export`
- family GREEN 结果确认：
  - direct `GenerateSigned(...)` 现在在两个 nil-result 场景都停在 constructor boundary
  - `TryGenerateSigned(...)` 继续 non-throwing、返回 `False`、并清空输出
  - 相邻的 signed export `BIO_new symbol-unavailable` contracts 仍然 green
  - full module compile 仍然 green (`181/181`)
- 结论：
  - `GenerateSigned(...)` PEM export constructor nil-result parity family 已闭环
  - 这条线不应再被重开，除非出现新的 failing contract 或新的 constructor/result-shape drift

## 2026-03-24 Findings (Canonical Wave C entrypoint convergence family closeout)
- 这批不是又一轮 connection-level SNI docs drift，而是 roadmap Milestone 1 的主入口收敛 family：
  - 目标不是补 API 细节
  - 而是让默认导航与默认命令真正把人带到当前 Wave C closeout/current-chain
- fresh discovery 表明这 5 个入口文件的问题并不相同，但可以构成一个 coherent family：
  - `README.md` 已经包含 canonical/current-chain 与 B121/B127 链接，但还缺少明确的“默认导航顺序”与“历史仅作参考”语句
  - `docs/README.md` 作为文档中心，仍然缺当前工程入口与当前 build/test commands
  - `docs/DOCUMENTATION_INDEX.md` 虽然已有 current/historical 分区，但顶部推荐列表仍未把 canonical Wave C 页面放在默认入口位置
  - `docs/guides/GETTING_STARTED.md` 与 `docs/guides/QUICKSTART.md` 仍把 `bash build_linux.sh` 放在“下一步 / 构建与测试”里，和当前真实工程入口不一致
- 这批的最小一致方案是：
  - README 明确写出“默认导航先看 closeout，再看 current-chain”
  - README 明确把 B121/B127 压到单行“历史手册仅作参考”
  - `docs/README.md` 新增 `当前工程入口（Wave C canonical chain）`
  - `docs/DOCUMENTATION_INDEX.md` 在顶部新增 canonical current-chain section，并把历史区标题明确标成“仅归档参考”
  - `GETTING_STARTED.md` / `QUICKSTART.md` 的 active build/test guidance 统一为：
    - `python3 scripts/compile_all_modules.py`
    - `bash scripts/run_minimal_ci_gate.sh --fast-local`
    - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
- 新 contract 在 GREEN 复跑时暴露了一个 harness gap：
  - `require_fixed ... "..."` 里的 backtick 被 shell 当成 command substitution
  - 这不是文档回归，而是 contract 自己的 quoting 问题
  - 最小修正是把带 backtick 的 expected literals 改成单引号包裹
- 本批仍然是 docs-only；没有修改 `src/`，也没有重新打开 runtime / validation 线路。

## 2026-03-24 Findings (WinSSL performance tuning connection-level SNI guidance family closeout)
- `docs/reference/WINSSL_PERFORMANCE_TUNING.md` 是一个新的 coherent family，因为它不是单一 snippet 漏洞，而是一组围绕性能/会话复用的 active reference examples 同时发生 drift。
- 这批和前面的 guide/reference omission family 不同的地方在于，它混合了三种语境：
  - 直接在 snippet 内 `CreateConnection(...)` 的 client flows
  - `MeasureConnection(AConn)` 这类 helper，握手发生在 helper 内，但 helper 自己不知道 host
  - session cache / connection pool 这类 usage snippet，连接和 host 责任落在外部 acquire/create path
- 因此这一批不能只做统一的 `SetServerName(...)` 插入：
  - direct `CreateConnection(...)` snippets 需要显式 `ISSLClientConnection.SetServerName(...)`
  - helper/pool/cache snippets 更适合补“连接级 SNI 已由调用方/Acquire 路径完成”的责任说明
- basic session reuse 与 benchmark resumed-session path 仍需维持已有顺序约束：
  - `CreateConnection(...)`
  - `SetSession(...)` when resuming
  - `SetServerName(...)`
  - `Connect` / `MeasureConnection(...)`
- memory-leak examples 即使主旨不是 SNI，也不能继续示范一个缺少 hostname 的 client handshake；因此这批把“错误/正确”两个 loop 都补成带 SNI 的真实客户端握手，再保留原本的 leak-management 对比。
- benchmark snippet 的 hostname source 最小一致方案是引入 `LHost := 'example.com';`，这样：
  - 首次连接
  - 无复用批量测试
  - 有复用批量测试
  都能复用同一个 host source，并保持 `MeasureConnection(AConn)` 由调用方负责 host/SNI 的边界清晰。
- 本批仍然是 docs-only；没有修改 `src/`，也没有重新打开 WinSSL runtime/performance behavior 本身。

## 2026-03-24 Findings (Error-handling best practices URL-driven SNI guidance family closeout)
- `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md` 的 remaining drift 不是 direct-setter API shape，也不是完全没有 hostname 语境的 generic socket snippet，而是一个 URL-driven client example：
  - 参数已经给出 `AUrl`
  - 请求发送也继续使用 `AUrl`
  - 但握手前缺少 hostname source，因此也缺少 connection-level SNI
- fresh repo scan 没有发现足够多仍未修复、且同样属于 “URL/host source 间接派生” 的 active docs，可以与它自然并成更大的 family；因此这一批合理边界就是单文档收口。
- repo 里已有可复用的文档约定来自 `WINSSL_QUICKSTART.md`：
  - URL 解析可以在示例层通过注释说明“实际应使用 URL 解析库”
  - 不需要为这批发明新的正式 API
- 因此本批的最小一致方案是：
  - 在示例里显式引入 `LHost: string;`
  - 用 `LHost := 'api.example.com';` 作为示例 hostname source
  - 紧接着注明“实际项目中应先从 `AUrl` 解析主机名，再据此建立或复用 `LSocket`”
  - 然后在握手前调用 `(LConnection as ISSLClientConnection).SetServerName(LHost);`
- 这个设计比只补一行 `SetServerName(...)` 更完整，因为它把“hostname 从哪里来”也写清了，避免文档又退回到半截 guidance。
- 本批仍然是 docs-only；没有修改 `src/`，也没有重开 runtime compatibility 线。

## 2026-03-24 Findings (API reference connection-level SNI omissions family closeout)
- `docs/reference/API_REFERENCE.md` 里 remaining 的 drift 不是 direct-setter API shape，而是一整组 reference-style client examples 缺少 canonical connection-level SNI step。
- 这批和前几批 guide/matrix 文档不同的地方在于，generic API examples 大量只给出了 `MySocket` 占位，没有现成 hostname 变量可直接复用。
- 为了把这一个 family 真正收口，而不是继续切碎，最小一致方案是：
  - 在 generic client snippets 中显式加入 `LServerName := 'example.com';`
  - 保持 `ISSLConnection` 变量不变
  - 在 `Connect` 前加入 `(LConn as ISSLClientConnection).SetServerName(LServerName);`
- WinSSL session examples 仍需维持已有顺序约束：
  - 第一次连接先设置 SNI 再握手
  - resumed connection 继续保持 `SetSession(...)` before `SetServerName(...)`
  - multi-host cache 从 `LHost` 派生 SNI
- 末尾完整 OpenSSL walkthrough 也需要把 hostname source 统一：
  - 同一个 `LServerName` 同时用于 `SetServerName(...)`
  - 以及后续 `VerifyHostname(...)`
- 新 contract 初版暴露了一个 harness gap：
  - 在 `set -euo pipefail` 下，零命中的 `rg` 被放进 command substitution 计数会提前退出，导致 RED 没有可读失败信息
  - 解决方式是在计数分支上为 `rg` 加 `|| true`，让 RED 结果显式打印 expected/actual count
- 本批仍然是 docs-only；没有修改 `src/`，也没有重开 runtime compatibility 线。

## 2026-03-24 Findings (Migration and troubleshooting connection-level SNI omissions family closeout)
- 在 direct-setter docs/API drift 连续收口之后，remaining 的下一类 active-doc drift 是 omission 而不是 wrong-API shape：
  - selected snippets in `docs/guides/MIGRATION_GUIDE.md`
  - selected snippets in `docs/guides/TROUBLESHOOTING.md`
- 这组片段的问题不是还在教 deprecated context-level setter，也不是把 setter 放在 `ISSLConnection` 上，而是完全漏掉了 canonical 的 connection-level SNI step。
- 这一批只选了有明确 hostname context 的 client snippets：
  - migration examples pointing at `example.com`
  - troubleshooting session-resumption snippet that caches `example.com`
- 这类 omission 不能沿用之前“抓 direct setter drift”的 contract，因此需要单独的 presence contract，直接要求 selected snippets 出现：
  - `(LConn as ISSLClientConnection).SetServerName('example.com');`
  - `(LConn1 as ISSLClientConnection).SetServerName('example.com');`
  - `(LConn2 as ISSLClientConnection).SetServerName('example.com');`
- `ERROR_HANDLING_BEST_PRACTICES.md` 没有并入这批，因为当前片段只给出 `LSocket` / `AUrl`，缺少同样明确的 hostname context；它更适合单独评估，而不是为凑批次硬塞进来。
- 本批仍然是 docs-only；不涉及 runtime compatibility 或 backend behavior。

## 2026-03-24 Findings (Capability matrix connection-level SNI API-drift family closeout)
- 在 secondary guides family 收口之后，remaining 的相关 drift 延伸到 capability / backend matrix 文档：
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- 这组文件和前一批不同的地方在于语境分成两类：
  - backend/client example：普通客户端连接流程
  - generic capability snippet：`ConfigureConnection(Conn: ISSLConnection)`
- 因此这批不能只做统一的 inline cast 替换：
  - backend/client example 可直接转 `ISSLClientConnection`
  - generic snippet 必须保留 `ISSLConnection` 参数，并在 SNI 分支上用 `Supports(..., ISSLClientConnection, ...)`
- 新 contract 在 GREEN 后暴露了一个 harness gap：
  - 旧的 fixed-string stale check 会把 `ClientConn.SetServerName(...)` 误判为 still matching `Conn.SetServerName(...)`
  - 解决方式是把 stale check 收紧为带边界的 regex pattern
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 的 session-reuse snippet 也顺手对齐到已有顺序：
  - `SetSession(...)` before `SetServerName(...)`
  - both before `Connect`
- 本批仍然是 docs-only；没有 runtime 变更，也没有重新打开 capability behavior 本身。

## 2026-03-24 Findings (Secondary guides connection-level SNI API-drift family closeout)
- 在 landing docs 收口之后，remaining 的 Milestone 3 drift 进一步收敛成一类 docs/API drift：
  - 文档不再教 context-level SNI
  - 但仍把 `SetServerName(...)` 直接调用到 `ISSLConnection` 变量上
- 这次选择的 family 是 normal client-flow secondary guides：
  - `docs/guides/QUICKSTART.md`
  - `docs/guides/COMMON_PITFALLS.md`
  - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
- 现有 contract（如 `test_active_docs_no_context_level_sni_guidance_contract.sh`）只能抓 deprecated context-level setter，抓不到这种“已经转向 connection path，但 API 形态仍然错”的 drift，因此需要新增 focused contract。
- 这组 drift 的最小修复模式很统一：
  - 保留 `ISSLConnection` 变量
  - 在需要 SNI 的地方显式转成 `ISSLClientConnection`
  - 再调用 `SetServerName(...)`
- `docs/guides/QUICKSTART.md` 的 resumed/session path 还顺手对齐到已有 guidance：
  - `SetSession(...)` before `SetServerName(...)`
  - both before `Connect`
- `docs/CAPABILITY_MATRIX_GUIDE.md` 也有 related hit，但它处在 generic `ConfigureConnection(Conn: ISSLConnection)` 语境，scope 不同；本批刻意不混进去，避免把 normal client-flow family 扩成更宽的 generic capability batch。
- 本批仍然是 docs-only；不需要修改 `src/`，也不需要重新论证 runtime compatibility。

## 2026-03-24 Findings (Landing docs connection-level SNI guidance family closeout)
- 在当前工作树里，remaining 的 `context/SNI` drift 更像 landing-docs drift，而不是 runtime / validation bug：
  - `docs/README.md`
  - `docs/guides/INTEGRATION_GUIDE.md`
  - `docs/guides/USER_GUIDE.md`
- 这三个文档都展示了 normal client flow，但遗漏了 canonical 的 connection-level SNI 步骤：
  - `CreateConnection(...)`
  - cast to `ISSLClientConnection`
  - `SetServerName(...)` before `Connect`
- 同仓已经有可直接复用的 canonical 参考，因此这一批不需要重新设计 guidance，只需要收敛到现有模式：
  - `README.md`
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/guides/GETTING_STARTED.md`
- 现有 `test_active_docs_no_context_level_sni_guidance_contract.sh` 只会抓“仍然在教 context-level setter”的文档，不足以覆盖这类“完全漏掉 canonical step”的 drift，所以需要新增 focused contract。
- 这个 family 的稳定 contract 不是检查 deprecated API 是否消失，而是要求 targeted docs 明确展示：
  - `ISSLClientConnection`
  - 从 `CreateConnection(...)` 结果做 client cast
  - `SetServerName(...)` before `Connect`
- 这是 docs-only batch；本批不需要修改 `src/`，也不需要为了“继续而继续”去重开已经 green 的 runtime SNI 线路。

## 2026-03-24 Findings (Certificate utils DERToPEM export delayed-loss family closeout)
- 在当前工作树里，`context/SNI` compatibility line 的 fresh rerun 已经是 green baseline，所以这批没有重开那条线，而是回到 cert-utils 做 fresh discovery
- fresh source review 显示 `TCertificateUtils.DERToPEM(...)` 仍有一个边界清晰的 export delayed-loss family，前提是：
  - `d2i_X509(...)` 已成功解码
  - helper gate 已通过
  - 随后 export block 内 helper 才消失
- 这个 family 保持得很窄，只覆盖三个真正仍可触发的 local dereference 点：
  - `BIO_s_mem()`
  - `BIO_new(...)` after `BIO_s_mem()` succeeds
  - `PEM_write_bio_X509(...)` after export BIO constructor succeeds
- fresh RED 证明 direct `DERToPEM(...)` 的实际问题不是错误返回，而是 raw `EAccessViolation`：
  - 三个 delayed-loss scenarios 都在 direct call 上触发 access violation
  - `TryDERToPEM(...)` 之所以仍返回 `False`，只是因为它兜底捕获了 direct exception，而不是因为 direct contract 本身安全降级
  - RED summary: `Passed: 12, Failed: 3, Skipped: 0`
- 这个 family 的最小安全修复只需要硬化 `DERToPEM(...)` 自己的 export block：
  - 在 `BIO_s_mem()` 调用前要求 `Assigned(BIO_s_mem)`
  - 把 `BIO_new(BIO_s_mem())` 拆成 `LBIOMethod := BIO_s_mem();` 后，在 `BIO_new(LBIOMethod)` 前重新检查 `Assigned(BIO_new)`
  - 在 `PEM_write_bio_X509(LBIO, LCert)` 前做本地 `Assigned(PEM_write_bio_X509)` guard
- rerun 结果确认修复后的 direct/public contract 回到预期：
  - direct `DERToPEM(...)` 不再抛异常，并对三种 delayed-loss 都返回空字符串
  - `TryDERToPEM(...)` 继续 non-throwing，返回 `False`，并清空输出
  - focused GREEN summary: `Passed: 15, Failed: 0, Skipped: 0`
- 旁路回归也确认这次 fix 没有破坏已经闭环的相邻 families：
  - conversion BIO contract 仍 PASS (`35/35`)
  - post-success cleanup family 仍 PASS (`20/20`)
  - earlier `d2i_X509` symbol contract 仍 PASS (`5/5`)
  - full module compile 仍 PASS (`181/181`)
- 这批 family 现在可以视为局部收口完成：
  - `DERToPEM(...)` export delayed-loss helper set 已闭环
  - 下一批应按 roadmap 继续 fresh discovery，而不是重新切这组已关掉的 export helpers 或重放当前 green 的 `context/SNI` line

## 2026-03-24 Findings (Repo-wide closeout roadmap + canonical docs entrypoint convergence)
- the repo-wide closeout decision is now materialized as a real roadmap document:
  - `docs/plans/2026-03-24-repo-wide-closeout-roadmap.md`
  - it locks the chosen cleanup policy to “主入口收敛”, not aggressive deletion
- fresh verification against the current working tree showed the previously suspected cert-utils follow-up batches are already green baselines rather than open gaps:
  - `GenerateSigned(...)` outer cleanup focused contracts pass for:
    - leaf `X509_free(...)`
    - outer `EVP_PKEY_free(...)`
    - CA `X509_free(...)`
  - `GenerateSelfSigned(...)` outer cleanup and RSA keygen focused contracts also pass for:
    - outer `X509_free(...)`
    - outer `EVP_PKEY_free(...)`
    - `RSA_new(...)`
    - `RSA_generate_key_ex(...)`
    - `BN_free(...)`
- conclusion from that fresh discovery:
  - these cert-utils families should not be re-opened without a new failing contract
  - the highest-value open work moved from replaying green helper batches to converging repo navigation and milestone tracking
- default documentation entrypoints were still drifting toward historical Wave C pages in active user-facing docs:
  - `README.md` and `docs/README.md` still promoted `WAVE_C_B121_*` / `WAVE_C_B127_*` directly
  - `docs/DOCUMENTATION_INDEX.md` already had a current-chain section, but the top recommended entry list still did not include the canonical Wave C stop-here pages
- the canonical entrypoint policy for this repo is now explicit:
  - default Wave C navigation starts from `WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`
  - then flows into `WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
  - historical B121/B127 pages remain valid references only
- next milestone should therefore move to `context/SNI` compatibility drift rather than continuing to symbol-slice cert-utils families that are already closed in this worktree

## 2026-03-24 Findings (Certificate utils VerifyChain bundled intermediate cleanup family closeout)
- after the earlier bundled-intermediate BIO guard batch, `TCertificateUtils.VerifyChain(...)` still had one narrow delayed-loss cleanup family inside the same `leaf + intermediate` success path:
  - skip-leaf cleanup `X509_free(LX509)` after the first successful `PEM_read_bio_X509(...)`
  - intermediate export constructor `BIO_new(...)` after `BIO_s_mem()` succeeds
  - loop cleanup `X509_free(LX509)` after the tracked intermediate export `BIO_free(...)`
- this family stayed intentionally narrow:
  - bundled `leaf + intermediate` verification path only
  - one focused family-level contract test
  - one local production edit inside `VerifyChain(...)`
  - no changes to `fafafa.ssl.certchain`, CA store loading, `GetInfo(...)`, or broader verification semantics
- the verify-chain public contract remained the target for the whole family:
  - direct `TCertificateUtils.VerifyChain(...)` is a boolean-returning API and should degrade to `False` when late helpers disappear
  - `TCertificateUtils.TryVerifyChain(...)` should stay non-throwing, return `True`, and report the degraded direct result through `AIsValid=False`
- fresh RED confirmed the three delayed-loss crash shapes exactly inside the bundled intermediate extraction/export loop:
  - a `PEM_read_bio_X509(...)` wrapper let the first skip-read succeed, then cleared `X509_free`
  - a `BIO_s_mem()` wrapper let the export method allocation succeed, then cleared `BIO_new`
  - a `BIO_new(...)` wrapper tracked the intermediate export BIO, and a `BIO_free(...)` wrapper cleared `X509_free` only when that tracked export BIO was freed
  - in all three scenarios direct `VerifyChain(...)` raised raw `EAccessViolation`
  - `TryVerifyChain(...)` stayed non-throwing but returned `False`, because the direct exception path was caught instead of surfacing the intended degraded boolean result
  - summary: `Passed: 6, Failed: 9, Skipped: 0`
- fresh GREEN showed the minimal safe fix is to harden only the actual delayed-loss dereference points inside `VerifyChain(...)`:
  - require `Assigned(X509_free)` before the skip-leaf cleanup dereference
  - split `BIO_new(BIO_s_mem())` into `LBIOMethod := BIO_s_mem();` plus a local `Assigned(BIO_new)` re-check immediately before `BIO_new(LBIOMethod)`
  - guard the loop cleanup `X509_free(LX509)` and abort the remaining intermediate loop with a normal `False` result if that cleanup helper disappears after export cleanup
- rerunning the focused family regression and the earlier bundled-intermediate BIO contract confirmed:
  - all three cleanup-family scenarios now degrade without `EAccessViolation`
  - direct `VerifyChain(...)` returns `False`
  - `TryVerifyChain(...)` remains non-throwing, returns `True`, and sets `AIsValid=False`
  - the earlier verify-chain BIO contract still passes unchanged
- this family is now locally closed for the reachable delayed-loss cleanup set:
  - skip-leaf cleanup `X509_free(...)`
  - intermediate export constructor `BIO_new(...)` after `BIO_s_mem()`
  - intermediate loop cleanup `X509_free(...)` after export `BIO_free(...)`
- next work should start from fresh discovery outside this closed `VerifyChain(...)` family rather than continue slicing the same bundled intermediate cleanup loop

## 2026-03-24 Findings (Certificate utils GenerateSelfSigned EC keygen family closeout)
- after the already-closed RSA self-signed batches and the later Ed25519 batch, the ECDSA self-signed path still had one cohesive local family inside `GenerateECKey(...)` used by `GenerateSelfSigned(...)`
- this family stayed intentionally narrow:
  - default self-signed ECDSA path only
  - one focused family-level contract test
  - one local production edit inside `GenerateECKey(...)`
  - no mixed changes to RSA/Ed25519 generation, `GenerateSigned(...)`, PEM export, or `TryGenerateSelfSignedSimple(...)`
- the self-signed public contract remained stable across the family closeout:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` fails through controlled `ESSLCertError` when EC keygen helpers disappear after the entry gate or a prior local step
  - `TCertificateUtils.TryGenerateSelfSigned(...)` stays non-throwing, returns `False`, and clears `ACertPEM` / `AKeyPEM`
  - `TryGenerateSelfSignedSimple(...)` is intentionally out of scope because it exercises the default RSA path rather than ECDSA
- fresh RED confirmed seven failure shapes exactly inside `GenerateECKey(...)`:
  - direct entry helper missing: `OBJ_txt2nid(...)`
  - delayed loss after curve lookup: `EC_KEY_new_by_curve_name(...)`
  - delayed loss after EC key allocation: `EC_KEY_generate_key(...)`
  - delayed loss after EC key generation: `EVP_PKEY_new(...)`
  - delayed loss after EVP container allocation: `EVP_PKEY_assign(...)`
  - delayed loss on failed assign cleanup: `EVP_PKEY_free(...)`
  - delayed loss on except cleanup after failed assign: `EC_KEY_free(...)`
  - in all seven scenarios direct `GenerateSelfSigned(...)` raised raw `EAccessViolation`
  - `TryGenerateSelfSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 39, Failed: 7, Skipped: 0`
- fresh GREEN showed the minimal safe fix is to harden `GenerateECKey(...)` locally at the actual dereference and cleanup boundaries:
  - require `Assigned(OBJ_txt2nid)` before curve lookup
  - require `Assigned(EC_KEY_new_by_curve_name)` before EC key allocation
  - require `Assigned(EC_KEY_generate_key)` before EC key generation
  - require `Assigned(EVP_PKEY_new)` before EVP container allocation
  - require `Assigned(EVP_PKEY_assign)` before ownership transfer, and if it disappears after container allocation, free the temporary container through `EVP_PKEY_free`, clear `Result`, and raise controlled `ESSLCertError`
  - if `EVP_PKEY_assign(...)` fails, free the temporary container through `EVP_PKEY_free`, clear `Result`, and preserve controlled failure
  - transfer ownership with `LKey := nil` after successful `EVP_PKEY_assign(...)` so the except path cannot double-free the EC key
  - guard `EC_KEY_free(...)` in the except path before freeing an unassigned EC key
- rerunning the focused family regression confirmed:
  - all seven EC family scenarios now raise controlled `ESSLCertError` instead of `EAccessViolation`
  - `TryGenerateSelfSigned(...)` still returns `False` and clears outputs
  - normal ECDSA self-signed generation still succeeds
- this family is now locally closed for the reachable helper set:
  - `OBJ_txt2nid(...)`
  - `EC_KEY_new_by_curve_name(...)`
  - `EC_KEY_generate_key(...)`
  - `EVP_PKEY_new(...)`
  - `EVP_PKEY_assign(...)`
  - `EVP_PKEY_free(...)`
  - `EC_KEY_free(...)`
- next work should start from fresh discovery outside this closed family rather than continue symbol slicing inside `GenerateECKey(...)`

## 2026-03-24 Findings (Certificate utils GenerateSelfSigned Ed25519 keygen family closeout)
- after leaving the already-closed RSA self-signed keygen/export batches and doing fresh discovery on the Ed25519 self-signed path, the next meaningful family was the local `GenerateEd25519Key(...)` helper sequence used by `GenerateSelfSigned(...)`
- this family stayed intentionally narrow:
  - default self-signed Ed25519 path only
  - one focused family-level contract test
  - one local production edit inside `GenerateEd25519Key(...)`
  - no mixed changes to RSA/ECDSA generation, `GenerateSigned(...)`, PEM export, or `TryGenerateSelfSignedSimple(...)`
- the self-signed public contract remained stable across the family closeout:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` fails through controlled `ESSLCertError` when Ed25519 keygen helpers disappear after the entry gate
  - `TCertificateUtils.TryGenerateSelfSigned(...)` stays non-throwing, returns `False`, and clears `ACertPEM` / `AKeyPEM`
  - `TryGenerateSelfSignedSimple(...)` is intentionally out of scope because it exercises the default RSA path rather than Ed25519
- fresh RED confirmed the three delayed-loss failure shapes exactly inside `GenerateEd25519Key(...)`:
  - a wrapper on `EVP_PKEY_CTX_new_id(...)` let the context constructor succeed, then cleared `EVP_PKEY_keygen_init`
  - a wrapper on `EVP_PKEY_keygen_init(...)` let init succeed, then cleared `EVP_PKEY_keygen`
  - a wrapper on `EVP_PKEY_keygen(...)` let key generation succeed, then cleared `EVP_PKEY_CTX_free`
  - in all three scenarios direct `GenerateSelfSigned(...)` raised raw `EAccessViolation`
  - `TryGenerateSelfSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 15, Failed: 3, Skipped: 0`
- fresh GREEN showed the minimal safe fix is to harden `GenerateEd25519Key(...)` locally at the actual dereference points:
  - require `Assigned(EVP_PKEY_keygen_init)` immediately before `EVP_PKEY_keygen_init(LCtx)`
  - require `Assigned(EVP_PKEY_keygen)` immediately before `EVP_PKEY_keygen(LCtx, LKey)`
  - guard the final `EVP_PKEY_CTX_free(LCtx)` dereference
  - transfer local ownership with `Result := LKey; LKey := nil` so cleanup logic can release the generated key if context cleanup disappears after successful keygen
  - when `EVP_PKEY_CTX_free` is missing after success, free the generated key through `EVP_PKEY_free` when available, clear `Result`, and raise controlled `ESSLCertError`
- rerunning the focused family regression and the existing Ed25519 baseline contract confirmed:
  - all three delayed-loss scenarios now raise controlled `ESSLCertError` instead of `EAccessViolation`
  - `TryGenerateSelfSigned(...)` still returns `False` and clears outputs
  - normal Ed25519 self-signed generation still succeeds
  - normal RSA-CA-signed Ed25519 leaf generation still succeeds
- this family is now locally closed for reachable delayed-loss helpers:
  - `EVP_PKEY_keygen_init(...)`
  - `EVP_PKEY_keygen(...)`
  - `EVP_PKEY_CTX_free(...)`
- `EVP_PKEY_CTX_new_id(...)` remains a direct family-entry helper with no useful post-gate delayed-loss hook point, so the next batch should start from fresh discovery outside this closed family

## 2026-03-24 Findings (Certificate utils GenerateSigned private-key PEM export family closeout)
- after finishing the delayed constructor and delayed write batches in the signed private-key PEM export block, the `GenerateSigned(...)` private-key PEM export family is now locally closed for reachable delayed-loss helpers:
  - `BIO_new(...)`
  - `PEM_write_bio_PrivateKey(...)`
  - `BIO_read(...)`
  - `BIO_free(...)`
- this family stayed intentionally narrow:
  - default CA-signed `GenerateSigned(...)` path only
  - one focused constructor contract test and one focused write-call contract test
  - local guards only in the signed private-key PEM export block inside `src/fafafa.ssl.cert.utils.pas`
  - no mixed changes to self-signed export logic, earlier certificate export helpers, or outer cleanup helpers
- the signed-generation public contract remained stable across the family closeout:
  - direct `TCertificateUtils.GenerateSigned(...)` fails through controlled `ESSLCertError` when private-key PEM export helpers disappear after the entry helper gate
  - `TCertificateUtils.TryGenerateSigned(...)` stays non-throwing, returns `False`, and clears `ACertPEM` / `AKeyPEM`
- fresh RED for the remaining write-call gap confirmed the delayed-loss shape exactly at the private-key PEM export write call:
  - the wrapper-driven test kept `PEM_write_bio_PrivateKey` assigned long enough to pass `HasPrivateKeyPEMWriteBIOHelpers`
  - then a `BIO_new` wrapper let the certificate export constructor and private-key export constructor succeed
  - immediately after the second successful constructor returned, the wrapper cleared `PEM_write_bio_PrivateKey`
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the missing local write guard was the last open reachable gap inside this family:
  - adding `Assigned(PEM_write_bio_PrivateKey)` immediately before `PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil)` converts that delayed-loss path back to controlled `ESSLCertError`
  - the earlier `BIO_new` hardening remains valid and focused
  - successful CA-signed generation remains unchanged when the private-key PEM export helpers stay available
- after rerunning the focused constructor contract and full module compilation, the family closeout evidence is:
  - constructor batch focused test passes (`6 passed / 0 failed`)
  - write batch focused test passes after its observed RED (`6 passed / 0 failed`)
  - full compile passes (`181/181`)
  - family diff hygiene passes for the touched family files and working-memory files
- this closeout should stop further symbol slicing inside the same private-key export block; the next batch should be chosen only after fresh discovery across the remaining signed successful path, because any surviving gap is now outside this closed family

## 2026-03-24 Findings (Certificate utils GenerateSigned certificate PEM export PEM_write_bio_X509 symbol guard)
- after closing the signed certificate PEM export constructor batch and doing fresh discovery inside the same export family, the next earliest reachable helper gap moved to the certificate PEM write call inside `GenerateSigned(...)`:
  - `GenerateSigned(...)` -> `PEM_write_bio_X509(LBIO, LCert)`
- this batch stayed intentionally narrow:
  - default CA-signed `GenerateSigned(...)` path only
  - one focused symbol-contract test
  - one local write-call guard in the certificate PEM export block
  - no mixed changes to later `BIO_read(...)`, `BIO_free(...)`, private-key export, or self-signed paths
- the signed-generation public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when the certificate PEM export write helper becomes unavailable after the helper gate and constructor succeed
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape exactly at the delayed write call site:
  - the wrapper-driven test kept `PEM_write_bio_X509` assigned long enough to pass `HasCertificatePEMWriteBIOHelpers`
  - then a `BIO_new` wrapper let the certificate export constructor succeed and cleared `PEM_write_bio_X509` immediately after that success
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local write guard is sufficient:
  - adding `Assigned(PEM_write_bio_X509)` immediately before `PEM_write_bio_X509(LBIO, LCert)` converts that delayed-loss path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `PEM_write_bio_X509` stays available
- fresh discovery after GREEN indicates the next earliest remaining helper inside the same certificate PEM export family now moves forward to:
  - `GenerateSigned(...)` -> `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
  - later `BIO_free(...)` and private-key export helpers remain separate isolated batches

## 2026-03-24 Findings (Certificate utils GenerateSigned certificate PEM export BIO_new symbol guard)
- after closing the signed CA private-key load family and doing fresh discovery across the surviving signed export path, the next earliest reachable helper gap moved to the certificate PEM export constructor inside `GenerateSigned(...)`:
  - `GenerateSigned(...)` -> `LBIO := BIO_new(BIO_s_mem())`
- this batch stayed intentionally narrow:
  - default CA-signed `GenerateSigned(...)` path only
  - one focused symbol-contract test
  - one local constructor hardening in the certificate PEM export block
  - no mixed changes to later `PEM_write_bio_X509(...)`, `BIO_read(...)`, `BIO_free(...)`, private-key export, or self-signed paths
- the signed-generation public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when the certificate PEM export constructor helper becomes unavailable after the helper gate
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- the first RED attempt failed in the test harness rather than at runtime:
  - the new contract test initially omitted `fafafa.ssl.openssl.base`
  - `PBIO_METHOD` therefore failed to resolve at compile time
  - fixing the test-only `uses` list and rerunning reached the intended runtime RED without touching production code
- fresh RED confirmed the failure shape exactly at the delayed constructor call site:
  - the wrapper-driven test kept `BIO_new` assigned long enough to pass `HasCertificatePEMWriteBIOHelpers`
  - then a `BIO_s_mem` wrapper let the helper gate remain satisfied and cleared `BIO_new` immediately before the local `BIO_new(...)` dereference
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the correct minimal fix mirrors the earlier self-signed constructor hardening:
  - require `Assigned(BIO_new)` before entering the certificate export constructor sequence
  - resolve `LBIOMethod := BIO_s_mem()` first
  - re-check `Assigned(BIO_new)` immediately before `BIO_new(LBIOMethod)`
  - this converts the delayed constructor-loss path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_new` stays available
- within the same constructor sequence, `BIO_s_mem()` remains a direct helper but sits at the start of the local sequence with no intermediate hook point for a post-gate delayed-loss RED
- fresh discovery after GREEN indicates the next earliest remaining helper inside the same certificate PEM export family now moves forward to:
  - `GenerateSigned(...)` -> `PEM_write_bio_X509(LBIO, LCert)`
  - later `BIO_read(...)`, `BIO_free(...)`, and private-key export helpers remain separate isolated batches

## 2026-03-24 Findings (Certificate utils GenerateSigned CA private-key PEM_read_bio_PrivateKey symbol guard)
- after closing the CA certificate parse block and doing fresh discovery across surviving signed paths, the next earliest reachable helper gap moved to the CA private-key parse call inside `GenerateSigned(...)`:
  - `GenerateSigned(...)` -> `PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)`
- this batch stayed intentionally narrow:
  - default CA-signed `GenerateSigned(...)` path only
  - one focused symbol-contract test
  - one local parse-call guard in the CA private-key load block
  - no mixed changes to CA cleanup, leaf generation, PEM export, or self-signed paths
- the signed-generation public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when the CA private-key parse helper becomes unavailable after the helper gate and BIO constructor succeed
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape exactly at the delayed parse call site:
  - the wrapper-driven test kept `PEM_read_bio_PrivateKey` assigned long enough to pass `HasPrivateKeyPEMReadBIOHelpers`
  - then a `BIO_new_mem_buf` wrapper let the CA private-key constructor succeed and cleared `PEM_read_bio_PrivateKey` immediately after that success
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local parse guard is sufficient:
  - adding `Assigned(PEM_read_bio_PrivateKey)` immediately before `PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)` converts that delayed-loss path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `PEM_read_bio_PrivateKey` stays available
- fresh discovery after GREEN indicates the CA private-key load subfamily is now locally closed for reachable delayed-loss helpers:
  - `PEM_read_bio_PrivateKey(...)`
  - `BIO_free(...)`
- within the same block, `BIO_new_mem_buf(...)` remains a direct helper but sits immediately after the entry helper gate with no intermediate hook point for a post-gate delayed-loss RED
- the next likely reachable remaining helper on the signed successful path now moves forward to:
  - `GenerateSigned(...)` -> certificate PEM export constructor `LBIO := BIO_new(BIO_s_mem())`
  - this should be re-confirmed by fresh discovery before the next isolated batch

## 2026-03-24 Findings (Certificate utils GenerateSigned CA certificate PEM_read_bio_X509 symbol guard)
- after closing the latest self-signed block and doing fresh discovery across surviving signed paths, the next earliest reachable helper gap moved to the CA certificate parse call inside `GenerateSigned(...)`:
  - `GenerateSigned(...)` -> `PEM_read_bio_X509(LBIO, nil, nil, nil)`
- this batch stayed intentionally narrow:
  - default CA-signed `GenerateSigned(...)` path only
  - one focused symbol-contract test
  - one local parse-call guard in the CA certificate load block
  - no mixed changes to CA/private-key cleanup, leaf generation, PEM export, or self-signed paths
- the signed-generation public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when the CA certificate parse helper becomes unavailable after the helper gate and BIO constructor succeed
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape exactly at the delayed parse call site:
  - the wrapper-driven test kept `PEM_read_bio_X509` assigned long enough to pass `HasCertificatePEMReadBIOHelpers`
  - then a `BIO_new_mem_buf` wrapper let the CA certificate constructor succeed and cleared `PEM_read_bio_X509` immediately after that success
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local parse guard is sufficient:
  - adding `Assigned(PEM_read_bio_X509)` immediately before `PEM_read_bio_X509(LBIO, nil, nil, nil)` converts that delayed-loss path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `PEM_read_bio_X509` stays available
- fresh discovery after GREEN indicates the CA certificate load subfamily is now locally closed for reachable delayed-loss helpers:
  - `PEM_read_bio_X509(...)`
  - `BIO_free(...)`
- within the same block, `BIO_new_mem_buf(...)` remains a direct helper but sits immediately after the entry helper gate with no intermediate hook point for a post-gate delayed-loss RED
- the next likely reachable remaining helper on the signed successful path now moves forward to:
  - `GenerateSigned(...)` -> `PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)`
  - this should be re-confirmed by fresh discovery before the next isolated batch

## 2026-03-24 Findings (Certificate utils GenerateSelfSigned private-key PEM_write_bio_PrivateKey symbol guard)
- after closing the private-key PEM export constructor batch and doing fresh discovery on the default self-signed successful path, the next earliest reachable helper gap moved to the private-key PEM write call:
  - `GenerateSelfSigned(...)` -> `PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil)`
- this batch stayed intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local write-call guard in the private-key PEM export block
  - no mixed changes to private-key `BIO_read(...)` / `BIO_free(...)`, `GenerateSigned(...)`, or broader PEM writer helpers elsewhere in the unit
- the self-signed public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when the private-key PEM export write helper becomes unavailable after the helper gate and constructor succeed
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape exactly at the delayed write call site:
  - the wrapper-driven test kept `PEM_write_bio_PrivateKey` assigned long enough to pass `HasPrivateKeyPEMWriteBIOHelpers`
  - then a `BIO_new` wrapper let the certificate export constructor and the private-key export constructor succeed
  - immediately after the second successful constructor returned, the wrapper cleared `PEM_write_bio_PrivateKey`
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local write guard is sufficient:
  - adding `Assigned(PEM_write_bio_PrivateKey)` immediately before `PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil)` converts that delayed-loss path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `PEM_write_bio_PrivateKey` stays available
- fresh discovery after GREEN indicates the self-signed private-key PEM export subfamily is now locally closed:
  - `BIO_new(...)`
  - `PEM_write_bio_PrivateKey(...)`
  - `BIO_read(...)`
  - `BIO_free(...)`
- outer cleanup on the same self-signed path remains guarded:
  - `X509_free(...)`
  - `EVP_PKEY_free(...)`
- the next batch should not assume the next call site from this block alone:
  - remaining direct PEM export helpers still exist elsewhere in `fafafa.ssl.cert.utils.pas`
  - fresh discovery should now pivot across those surviving paths before choosing the next isolated batch

## 2026-03-24 Findings (Certificate utils GenerateSelfSigned private-key BIO_new symbol guard)
- after closing the certificate PEM export subfamily and doing fresh discovery on the default self-signed successful path, the next earliest reachable helper gap moved into private-key PEM export construction:
  - `GenerateSelfSigned(...)` -> `LBIO := BIO_new(BIO_s_mem())`
- this batch stayed intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local constructor-call-site hardening in the private-key PEM export block
  - no mixed changes to later `PEM_write_bio_PrivateKey(...)`, `BIO_read(...)`, `BIO_free(...)`, or outer cleanup helpers
- the self-signed public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when the private-key PEM export BIO constructor helper becomes unavailable after the helper gate
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the delayed constructor failure shape exactly at the private-key export call site:
  - the wrapper-driven test kept `BIO_new` assigned long enough to pass `HasPrivateKeyPEMWriteBIOHelpers`
  - then the second `BIO_s_mem()` call cleared the global `BIO_new` symbol immediately before the private-key export constructor dereference
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the correct minimal fix mirrors the earlier certificate constructor hardening:
  - require `Assigned(BIO_new)` before entering the private-key constructor sequence
  - resolve `LBIOMethod := BIO_s_mem()` first
  - then re-check `Assigned(BIO_new)` immediately before `BIO_new(LBIOMethod)`
  - this converts the delayed constructor-loss path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `BIO_new` stays available
- fresh discovery after GREEN indicates the next earliest remaining helper inside the same private-key PEM export family now moves forward to:
  - `GenerateSelfSigned(...)` -> `PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil)`
  - this is now the first unguarded direct helper left in that private-key export block before existing readback and cleanup guards

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned certificate PEM export PEM_write_bio_X509 symbol guard)
- after closing the certificate PEM export constructor batch and doing fresh discovery on the surviving self-signed path, the next earliest reachable helper gap moved to the certificate PEM write call:
  - `GenerateSelfSigned(...)` -> `PEM_write_bio_X509(LBIO, LCert)`
- this batch stayed intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local write-call guard in the certificate PEM export block
  - no mixed changes to certificate `BIO_read(...)` / `BIO_free(...)` or private-key export helpers
- the self-signed public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when the certificate PEM export write helper becomes unavailable after the helper gate and constructor succeed
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape exactly at the delayed write call site:
  - the wrapper-driven test kept `PEM_write_bio_X509` assigned long enough to pass `HasCertificatePEMWriteBIOHelpers`
  - then a `BIO_new` wrapper let the certificate export constructor succeed and cleared `PEM_write_bio_X509` immediately after that success
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local write guard is sufficient:
  - adding `Assigned(PEM_write_bio_X509)` immediately before `PEM_write_bio_X509(LBIO, LCert)` converts that delayed-loss path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `PEM_write_bio_X509` stays available
- fresh discovery after GREEN indicates the certificate PEM export subfamily is now locally closed on the self-signed path:
  - `BIO_new(...)`
  - `PEM_write_bio_X509(...)`
  - `BIO_read(...)`
  - `BIO_free(...)`
- the next earliest remaining direct helper on the default self-signed successful path now moves forward to:
  - private-key PEM export constructor `LBIO := BIO_new(BIO_s_mem())`
  - this should be the next narrow batch before private-key write/read/cleanup helpers

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned certificate PEM export BIO_new symbol guard)
- after closing the default RSA `GenerateRSAKey(...)` family and doing fresh discovery on the surviving self-signed path, the next earliest reachable helper gap moved into certificate PEM export construction:
  - `GenerateSelfSigned(...)` -> `LBIO := BIO_new(BIO_s_mem())`
- this batch stayed intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local constructor-call-site hardening in the certificate PEM export block
  - no mixed changes to later `PEM_write_bio_X509(...)`, `BIO_read(...)`, `BIO_free(...)`, or private-key export helpers
- the self-signed public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when the certificate PEM export BIO constructor helper becomes unavailable after the helper gate
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the failure shape at the delayed constructor call site:
  - the wrapper-driven test kept `BIO_new` assigned long enough to pass `HasCertificatePEMWriteBIOHelpers`
  - then `BIO_s_mem()` cleared the global `BIO_new` symbol immediately before the local constructor dereference
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- the first GREEN hypothesis was too weak:
  - adding only `Assigned(BIO_new)` before the whole `BIO_new(BIO_s_mem())` expression did not fix the AV
  - root cause: `BIO_s_mem()` is evaluated first, and the wrapper clears `BIO_new` during that parameter-evaluation step, so the later dereference in the same expression still crashes
- fresh GREEN confirmed the correct minimal fix is to split the constructor input and re-check the symbol at the actual dereference boundary:
  - resolve `LBIOMethod := BIO_s_mem()` first
  - then require `Assigned(BIO_new)` immediately before `BIO_new(LBIOMethod)`
  - this converts the delayed constructor-loss path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `BIO_new` stays available
- fresh discovery after GREEN indicates the next earliest remaining helper inside the same certificate PEM export family now moves forward to:
  - `GenerateSelfSigned(...)` -> `PEM_write_bio_X509(LBIO, LCert)`
  - this is now the first unguarded direct helper left in that certificate export block after local constructor, readback, and cleanup hardening

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned BN_free symbol guard)
- after closing the self-signed EVP ownership-transfer batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `finally BN_free(LExp)`
- this batch stayed intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local cleanup guard around `BN_free(...)`
  - no mixed changes to earlier RSA/BIGNUM/EVP guard intent or broader X509 / PEM flows
- the self-signed public contract remained the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when RSA exponent-cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the final BIGNUM cleanup call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `BN_free` was cleared before `GenerateRSAKey(...)` exited
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local BIGNUM cleanup guard is sufficient, but it needed ownership-state normalization inside `GenerateRSAKey(...)`:
  - adding `Assigned(BN_free)` around the final `BN_free(LExp)` converts that path back to controlled `ESSLCertError`
  - because this cleanup gap fires after earlier branches may already have freed `LKey` / `Result`, and after the success path may already have transferred `LKey` ownership into `Result`, the minimal safe implementation must normalize local nil-state after manual frees and after successful `EVP_PKEY_assign(...)`
  - with that local nil-state cleanup in place, the new `BN_free`-missing branch can release `Result` exactly once before raising instead of leaking or double-freeing the generated key container
  - successful RSA self-signed generation remains unchanged when `BN_free` stays available
- fresh discovery after GREEN indicates the default RSA successful-path helper family inside `GenerateRSAKey(...)` is now closed for this one-symbol campaign:
  - `RSA_new`
  - `BN_new`
  - `BN_set_word`
  - `RSA_generate_key_ex`
  - `EVP_PKEY_new`
  - `EVP_PKEY_assign`
  - `BN_free`
- the next batch should therefore start with fresh discovery outside `GenerateRSAKey(...)` successful-path assumptions rather than precommitting to another call site

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned EVP_PKEY_assign symbol guard)
- after closing the self-signed EVP container allocation batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `EVP_PKEY_assign(Result, EVP_PKEY_RSA, LKey)`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `EVP_PKEY_assign(...)`
  - no mixed fixes for later `BN_free`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when EVP key ownership-transfer helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the EVP ownership-transfer call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `EVP_PKEY_assign` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local EVP ownership-transfer guard is sufficient:
  - adding `Assigned(EVP_PKEY_assign)` immediately before `EVP_PKEY_assign(Result, EVP_PKEY_RSA, LKey)` converts that path back to controlled `ESSLCertError`
  - because this guard sits after both `Result` and `LKey` are allocated but before ownership transfer succeeds, the minimal safe implementation must free both `Result` and `LKey` before raising so the AV fix does not orphan either resource on the new failure path
  - successful RSA self-signed generation remains unchanged when `EVP_PKEY_assign` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `finally BN_free(LExp)`
- this is the current last known remaining direct helper inside `GenerateRSAKey(...)`, so the next batch should finish this family before any fresh pivot

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned EVP_PKEY_new symbol guard)
- after closing the self-signed RSA key-generation batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `EVP_PKEY_new()`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `EVP_PKEY_new()`
  - no mixed fixes for later `EVP_PKEY_assign` or `BN_free`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when EVP key-container allocation helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the EVP container allocation call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `EVP_PKEY_new` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local EVP container allocation guard is sufficient:
  - adding `Assigned(EVP_PKEY_new)` immediately before `Result := EVP_PKEY_new()` converts that path back to controlled `ESSLCertError`
  - because this guard sits after RSA generation but before `EVP_PKEY_assign(...)`, the minimal safe implementation must free `LKey` before raising so the AV fix does not leave the generated RSA structure orphaned on the new failure path
  - successful RSA self-signed generation remains unchanged when `EVP_PKEY_new` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `EVP_PKEY_assign(Result, EVP_PKEY_RSA, LKey)`
  - later direct helpers remain after that:
    - `BN_free`
- the next batch should therefore stay on `GenerateRSAKey(...)` rather than pivot away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned RSA_generate_key_ex symbol guard)
- after closing the self-signed exponent-initialization batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `RSA_generate_key_ex(LKey, ABits, LExp, nil)`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `RSA_generate_key_ex(...)`
  - no mixed fixes for later `EVP_PKEY_new`, `EVP_PKEY_assign`, or `BN_free`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when RSA key-generation helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the RSA key-generation call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `RSA_generate_key_ex` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local RSA key-generation guard is sufficient:
  - adding `Assigned(RSA_generate_key_ex)` immediately before `RSA_generate_key_ex(LKey, ABits, LExp, nil)` converts that path back to controlled `ESSLCertError`
  - because this guard sits after RSA/BIGNUM setup but before `EVP_PKEY` ownership transfer, the minimal safe implementation must free `LKey` before raising so the AV fix does not leave the generated RSA structure orphaned on the new failure path
  - successful RSA self-signed generation remains unchanged when `RSA_generate_key_ex` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `EVP_PKEY_new()`
  - later direct helpers remain after that:
    - `EVP_PKEY_assign`
    - `BN_free`
- the next batch should therefore stay on `GenerateRSAKey(...)` rather than pivot away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned BN_set_word symbol guard)
- after closing the self-signed BIGNUM allocation batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `BN_set_word(LExp, RSA_EXPONENT_F4)`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `BN_set_word(...)`
  - no mixed fixes for later `RSA_generate_key_ex`, `EVP_PKEY_new`, `EVP_PKEY_assign`, or `BN_free`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when RSA exponent-initialization helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the exponent-init call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `BN_set_word` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local exponent-init guard is sufficient:
  - adding `Assigned(BN_set_word)` immediately before `BN_set_word(LExp, RSA_EXPONENT_F4)` converts that path back to controlled `ESSLCertError`
  - because this guard sits after both `LKey` and `LExp` are already allocated, the minimal safe implementation must free `LKey` before raising so the AV fix does not introduce a leaked RSA structure on the new failure path
  - successful RSA self-signed generation remains unchanged when `BN_set_word` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `RSA_generate_key_ex(LKey, ABits, LExp, nil)`
  - later direct helpers remain after that:
    - `EVP_PKEY_new`
    - `EVP_PKEY_assign`
    - `BN_free`
- the next batch should therefore stay on `GenerateRSAKey(...)` rather than pivot away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned BN_new symbol guard)
- after closing the self-signed RSA allocation batch, the next earliest remaining default-path helper gap within `GenerateRSAKey(...)` moved to:
  - `BN_new()`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `BN_new()`
  - no mixed fixes for later `BN_set_word`, `RSA_generate_key_ex`, `EVP_PKEY_new`, `EVP_PKEY_assign`, or `BN_free`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when RSA exponent-allocation helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the BIGNUM allocation call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `BN_new` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local BIGNUM allocation guard is sufficient:
  - adding `Assigned(BN_new)` immediately before `LExp := BN_new()` converts that path back to controlled `ESSLCertError`
  - successful RSA self-signed generation remains unchanged when `BN_new` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `BN_set_word(LExp, RSA_EXPONENT_F4)`
  - later direct helpers remain after that:
    - `RSA_generate_key_ex`
    - `EVP_PKEY_new`
    - `EVP_PKEY_assign`
    - `BN_free`
- the next batch should therefore stay on `GenerateRSAKey(...)` rather than pivot away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned RSA_new symbol guard)
- fresh single-symbol discovery after closing the self-signed private-key PEM export cleanup tail shows the earliest remaining default-path helper gap moved back to RSA key generation:
  - `GenerateRSAKey(...)` -> `RSA_new()`
- this batch stays intentionally narrow:
  - default RSA `GenerateSelfSigned(...)` path only
  - one focused symbol-contract test
  - one local guard before `RSA_new()`
  - no mixed fixes for later `BN_new`, `BN_set_word`, `RSA_generate_key_ex`, `EVP_PKEY_new`, or `EVP_PKEY_assign`
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when RSA key-allocation helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the RSA allocation call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when `RSA_new` was cleared before `GenerateRSAKey(...)`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local RSA allocation guard is sufficient:
  - adding `Assigned(RSA_new)` immediately before `LKey := RSA_new()` converts that path back to controlled `ESSLCertError`
  - successful RSA self-signed generation remains unchanged when `RSA_new` stays available
- fresh discovery after GREEN moves the next earliest default-path helper gap forward within the same RSA keygen family:
  - `GenerateRSAKey(...)` -> `BN_new()`
  - later direct helpers remain after that:
    - `BN_set_word`
    - `RSA_generate_key_ex`
    - `EVP_PKEY_new`
    - `EVP_PKEY_assign`
- the next batch should therefore stay on `GenerateRSAKey(...)` rather than pivot away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned private-key BIO_free symbol guard)
- after closing the self-signed private-key PEM export `BIO_read` batch, the next earliest remaining direct symbol on the successful `GenerateSelfSigned(...)` path moved to:
  - private-key PEM export cleanup `BIO_free(LBIO)`
- this batch needs a delayed-disable RED pattern because `HasPrivateKeyPEMWriteBIOHelpers` checks `BIO_free` at entry:
  - keep `BIO_free` assigned long enough to pass the helper gate
  - install a temporary `BIO_read` wrapper
  - let both PEM readback calls succeed
  - clear the global `BIO_free` symbol immediately after the second private-key export read returns
  - expose only the following private-key export cleanup dereference
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when private-key PEM export cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the private-key PEM export cleanup call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when the private-key PEM export `finally BIO_free(LBIO)` executed after the wrapper disabled `BIO_free`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local private-key PEM export cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the private-key PEM export cleanup `BIO_free(LBIO)` converts that path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN shows the late PEM export and outer-cleanup tail on the self-signed path is now closed:
  - certificate PEM export `BIO_read(...)`
  - certificate PEM export cleanup `BIO_free(LBIO)`
  - private-key PEM export `BIO_read(...)`
  - private-key PEM export cleanup `BIO_free(LBIO)`
  - outer `X509_free(LCert)` / `EVP_PKEY_free(LKey)`
- do not overclaim overall closure from that narrower result:
  - on the default RSA successful path, the earliest remaining direct helper gap now moves back to key generation
  - the first unguarded direct helper is `GenerateRSAKey(...)` -> `RSA_new()`
  - there is no focused self-signed symbol-contract coverage yet for the surrounding RSA keygen helper family:
    - `RSA_new`
    - `BN_new`
    - `BN_set_word`
    - `RSA_generate_key_ex`
    - `EVP_PKEY_new`
    - `EVP_PKEY_assign`
- the next batch should therefore restart at the earliest RSA key-generation helper instead of pivoting away from `GenerateSelfSigned(...)`

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned private-key BIO_read symbol guard)
- after closing the self-signed certificate PEM export cleanup `BIO_free` batch, the next earliest remaining direct symbol on the successful `GenerateSelfSigned(...)` path moved to:
  - private-key PEM export `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
- this batch needs a wrapper-based RED because the same `BIO_read` symbol is already guarded at the earlier certificate PEM export call site:
  - install a temporary `BIO_read` wrapper
  - let the first certificate PEM export read succeed
  - clear the global `BIO_read` symbol immediately before the second private-key export readback
  - expose only that later private-key export dereference
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when private-key PEM export read helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the private-key PEM export readback call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when the wrapper cleared `BIO_read` before the second readback
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local private-key export read guard is sufficient:
  - adding `Assigned(BIO_read)` immediately before the private-key PEM export `BIO_read(...)` converts that path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `BIO_read` stays available
- fresh discovery after GREEN moves the next earliest remaining direct symbol forward to:
  - private-key PEM export cleanup `BIO_free(LBIO)`
  - outer `X509_free(LCert)` / `EVP_PKEY_free(LKey)` are later and already guarded
- no broader helper redesign was needed:
  - the earlier certificate PEM export `BIO_read` guard remains unchanged
  - the next batch should stay on the later private-key PEM export cleanup `BIO_free` instead of jumping to outer cleanup

## 2026-03-23 Findings (Certificate utils GenerateSelfSigned certificate PEM export BIO_free symbol guard)
- after re-checking the successful `GenerateSelfSigned(...)` path beyond the earlier certificate PEM export `BIO_read` batch, the earliest remaining direct cleanup call site was:
  - certificate PEM export cleanup immediately after the first certificate PEM `BIO_read(...)`
- this batch needs a delayed-disable RED pattern because `HasCertificatePEMWriteBIOHelpers` checks `BIO_free` at entry:
  - keep `BIO_free` assigned long enough to pass the helper gate
  - install a temporary `BIO_read` wrapper
  - let the first certificate PEM export read succeed
  - clear the global `BIO_free` symbol immediately after that successful read returns
  - expose only the following certificate PEM export cleanup dereference
- the self-signed public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` should raise controlled `ESSLCertError` when certificate PEM export cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the certificate PEM export cleanup call site:
  - direct `TCertificateUtils.GenerateSelfSigned(...)` raised `EAccessViolation` when the certificate PEM export `finally BIO_free(LBIO)` executed after the wrapper disabled `BIO_free`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` remained non-throwing, returned `False`, and cleared outputs
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local certificate PEM export cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the certificate PEM export cleanup `BIO_free(LBIO)` converts that path back to controlled `ESSLCertError`
  - successful self-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN corrects the remaining queue on the self-signed path:
  - the next earliest remaining direct symbol is private-key PEM export `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
  - the later private-key PEM export cleanup `BIO_free(LBIO)` remains after that
  - outer `X509_free(LCert)` / `EVP_PKEY_free(LKey)` are later and already guarded
- do not jump directly from this batch to outer cleanup work:
  - the next batch should stay inside the self-signed private-key PEM export segment first
  - continue single-symbol isolation from that earlier remaining `BIO_read` gap

## 2026-03-23 Findings (Certificate utils GenerateSigned certificate PEM export BIO_free symbol guard)
- after closing the CA private-key load cleanup `BIO_free` batch, the next earliest remaining direct `BIO_free(LBIO)` call site on the successful `GenerateSigned(...)` path moved to:
  - certificate PEM export cleanup immediately after the first certificate PEM `BIO_read(...)`
- this batch needs a delayed-disable RED pattern because `HasCertificatePEMWriteBIOHelpers` checks `BIO_free` at entry:
  - keep `BIO_free` assigned long enough to pass the helper gate
  - install a temporary `BIO_read` wrapper
  - let the first certificate PEM export read succeed
  - clear the global `BIO_free` symbol immediately after that successful read returns
  - expose only the following certificate PEM export cleanup dereference
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when certificate PEM export cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the certificate PEM export cleanup call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when the certificate PEM export `finally BIO_free(LBIO)` executed after the wrapper disabled `BIO_free`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local certificate PEM export cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the certificate PEM export cleanup `BIO_free(LBIO)` converts that path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN indicates the known direct `BIO_free` cleanup family on the current successful `GenerateSigned(...)` path is now closed:
  - CA certificate load cleanup `BIO_free(LBIO)`
  - CA private-key load cleanup `BIO_free(LBIO)`
  - certificate PEM export cleanup `BIO_free(LBIO)`
  - private-key PEM export cleanup `BIO_free(LBIO)`
- do not overclaim completion from that narrower result:
  - the next batch should begin with fresh single-symbol discovery over the remaining direct helper calls on the signed-generation path
  - only that fresh scan should decide whether work stays in `GenerateSigned(...)` or pivots elsewhere

## 2026-03-23 Findings (Certificate utils GenerateSigned CA private-key BIO_free symbol guard)
- after closing the CA certificate load cleanup `BIO_free` batch, the next earliest remaining direct `BIO_free(LBIO)` call site on the successful `GenerateSigned(...)` path moved to:
  - CA private-key load cleanup immediately after `PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)`
- this batch needs the same delayed-disable RED pattern because `HasPrivateKeyPEMReadBIOHelpers` checks `BIO_free` at entry:
  - keep `BIO_free` assigned long enough to pass the helper gate
  - install a temporary `PEM_read_bio_PrivateKey` wrapper
  - let the CA private-key parse succeed
  - clear the global `BIO_free` symbol immediately after that parse returns
  - expose only the following CA private-key load cleanup dereference
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when CA private-key load cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the CA private-key load cleanup call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when the CA private-key load `finally BIO_free(LBIO)` executed after the wrapper disabled `BIO_free`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local CA private-key load cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the CA private-key load cleanup `BIO_free(LBIO)` converts that path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN moves the next earliest direct `BIO_free` cleanup call forward to:
  - certificate PEM export cleanup `BIO_free(LBIO)`
  - the later private-key PEM export cleanup `BIO_free(LBIO)` was already closed in the earlier batch
- no broader helper redesign was needed:
  - `HasPrivateKeyPEMReadBIOHelpers` remains unchanged
  - later `BIO_free`, `X509_free`, and `EVP_PKEY_free` cleanup batches stay isolated

## 2026-03-23 Findings (Certificate utils GenerateSigned CA certificate BIO_free symbol guard)
- after closing the outer cleanup tail batches, fresh discovery returned to the earlier remaining `BIO_free` call-family on the successful `GenerateSigned(...)` path
- the earliest still-unguarded direct `BIO_free(LBIO)` call site was:
  - CA certificate load cleanup immediately after `PEM_read_bio_X509(LBIO, nil, nil, nil)`
- this batch needs a wrapper-based RED strategy because `HasCertificatePEMReadBIOHelpers` checks `BIO_free` at entry:
  - keep `BIO_free` assigned long enough to pass the helper gate
  - install a temporary `PEM_read_bio_X509` wrapper
  - let the CA certificate parse succeed
  - clear the global `BIO_free` symbol immediately after that parse returns
  - expose only the following CA certificate load cleanup dereference
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when CA certificate load cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the CA certificate load cleanup call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when the CA certificate load `finally BIO_free(LBIO)` executed after the wrapper disabled `BIO_free`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local CA certificate load cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the CA certificate load cleanup `BIO_free(LBIO)` converts that path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN moves the next earliest direct `BIO_free` cleanup call forward to:
  - CA private-key load cleanup `BIO_free(LBIO)`
  - later certificate PEM export cleanup remains after that
- no broader helper redesign was needed:
  - `HasCertificatePEMReadBIOHelpers` remains unchanged
  - later `BIO_free`, `X509_free`, and `EVP_PKEY_free` cleanup batches stay isolated

## 2026-03-23 Findings (Certificate utils GenerateSigned CA X509_free symbol guard)
- after closing the signed-path outer `EVP_PKEY_free` batch, the next earliest remaining cleanup symbol at the end of the successful `GenerateSigned(...)` path was:
  - outer CA certificate cleanup `X509_free(LCACert)`
- the earlier leaf cleanup for the same symbol is already guarded:
  - `X509_free(LCert)`
- that means the later CA cleanup call site can be isolated with a wrapper-based RED strategy:
  - install a temporary `X509_free` wrapper
  - let the first leaf cleanup succeed
  - immediately clear the global `X509_free` symbol after that first call
  - expose only the later `X509_free(LCACert)` dereference
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when CA certificate cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the outer CA cleanup call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when the later `X509_free(LCACert)` call was reached with the symbol cleared
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local CA cleanup guard is sufficient:
  - adding `Assigned(X509_free)` immediately before `X509_free(LCACert)` converts the tail cleanup path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `X509_free` stays available
- fresh discovery after GREEN shows the outer cleanup tail is now closed, but the whole successful path is not yet fully guarded:
  - earlier direct `BIO_free(LBIO)` call sites still remain on the `GenerateSigned(...)` path:
    - CA certificate load cleanup
    - CA private-key load cleanup
    - certificate PEM export cleanup
  - the next batch should return to fresh single-symbol discovery over that earlier `BIO_free` call-family instead of claiming cleanup completion
- a small test-harness correction was needed during RED setup:
  - the wrapper signature required directly importing `fafafa.ssl.openssl.api.types` so `PX509` is in scope
  - this did not change the targeted behavior or the batch boundary

## 2026-03-23 Findings (Certificate utils GenerateSigned EVP_PKEY_free symbol guard)
- after closing the signed-path outer leaf `X509_free(LCert)` batch, the next earliest remaining cleanup symbol on the successful `GenerateSigned(...)` path is:
  - `EVP_PKEY_free`
  - it appears twice in the contiguous outer cleanup chain:
    - `EVP_PKEY_free(LKey)`
    - `EVP_PKEY_free(LCAKey)`
- fresh root-cause analysis showed the first `EVP_PKEY_free(LKey)` call cannot be isolated into its own independently correct call-site batch:
  - when the first call is reached with `EVP_PKEY_free = nil`, control immediately unwinds into the second `EVP_PKEY_free(LCAKey)` finally block
  - there is no intervening cleanup site where the original symbol can be restored before the second duplicate call
  - guarding only the first call site would therefore not preserve the public contract for the whole `GenerateSigned(...)` path when `EVP_PKEY_free` is unavailable
- the minimal defensible batch boundary is therefore:
  - one single-symbol batch for `EVP_PKEY_free`
  - covering both outer cleanup call sites for that same symbol
  - without mixing in `X509_free(LCACert)` or any other symbol
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when private-key cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape for the missing cleanup symbol:
  - temporarily clearing `EVP_PKEY_free` caused direct `TCertificateUtils.GenerateSigned(...)` to raise `EAccessViolation`
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the minimal symbol-level fix is sufficient:
  - adding `Assigned(EVP_PKEY_free)` before both `EVP_PKEY_free(LKey)` and `EVP_PKEY_free(LCAKey)` converts the outer cleanup chain back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `EVP_PKEY_free` stays available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_free(LCACert)`
  - it should remain isolated into the next later batch

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_free symbol guard)
- after closing the signed-path private-key PEM export `BIO_free` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves to:
  - outer leaf certificate cleanup `X509_free(LCert)`
  - it occurs after certificate and private-key PEM export have both already succeeded
- this batch needs a wrapper-based RED mechanism because the same `X509_free` symbol appears again later on the same unwind path:
  - target outer leaf cleanup `X509_free(LCert)`
  - later outer CA cleanup `X509_free(LCACert)`
- the focused RED strategy is:
  - clear the global `X509_free` symbol before the target outer leaf cleanup
  - install an `EVP_PKEY_free` wrapper that restores the original `X509_free` during unwind before the later CA cleanup executes
  - keep the batch limited to the target `X509_free(LCert)` call site
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test for the outer leaf `X509_free` gap
  - one local `Assigned(X509_free)` guard around the target outer leaf cleanup path
  - no mixed fixes for `EVP_PKEY_free(LKey)` / `EVP_PKEY_free(LCAKey)` / `X509_free(LCACert)`
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf certificate cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the outer leaf cleanup call site:
  - the wrapper-based test can keep the target `X509_free(LCert)` exposed while preventing the later `X509_free(LCACert)` from overriding the result
  - direct `TCertificateUtils.GenerateSigned(...)` then raised `EAccessViolation` when `X509_free(LCert)` was reached
  - `TCertificateUtils.TryGenerateSigned(...)` remained non-throwing, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local cleanup guard is sufficient:
  - adding `Assigned(X509_free)` immediately before `X509_free(LCert)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `X509_free` stays available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - outer private-key cleanup `EVP_PKEY_free(LKey)`
  - later `EVP_PKEY_free(LCAKey)` / `X509_free(LCACert)` should remain isolated into separate later batches

## 2026-03-22 Findings (Certificate utils GenerateSigned private-key BIO_free symbol guard)
- after closing the signed-path private-key PEM export `BIO_read` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves to:
  - `BIO_free(LBIO)` in the private-key PEM export `finally` block
  - it occurs after both certificate and private-key PEM readback calls have already succeeded
- this batch needs a wrapper-based RED mechanism because `BIO_free` has several earlier successful call sites on the same `GenerateSigned(...)` path:
  - CA certificate load cleanup
  - CA private-key load cleanup
  - certificate PEM export cleanup
  - the target private-key PEM export cleanup
- the focused RED strategy is:
  - install a `BIO_read` wrapper that allows both PEM readback calls to succeed
  - after the second private-key PEM export `BIO_read(...)`, clear the global `BIO_free` symbol
  - keep the batch limited to the target `finally BIO_free(LBIO)` call site
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test for the late `BIO_free` gap
  - one local `Assigned(BIO_free)` guard around the private-key PEM export cleanup path
  - no mixed fixes for outer `X509_free` / `EVP_PKEY_free`
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when private-key PEM export cleanup helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the private-key PEM export cleanup call site:
  - a wrapper-based test can let both `BIO_read` calls succeed and then clear `BIO_free` before the key-export `finally`
  - direct `TCertificateUtils.GenerateSigned(...)` then raised `EAccessViolation` when `BIO_free(LBIO)` was reached
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local cleanup guard is sufficient:
  - adding `Assigned(BIO_free)` immediately before the private-key PEM export `BIO_free(LBIO)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_free` stays available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - outer certificate cleanup `X509_free(LCert)`
  - later `EVP_PKEY_free(LKey)` / `EVP_PKEY_free(LCAKey)` / `X509_free(LCACert)` should remain isolated into separate later batches

## 2026-03-22 Findings (Certificate utils GenerateSigned private-key BIO_read symbol guard)
- after closing the signed-path certificate PEM export `BIO_read` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves to:
  - the second `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))` during private-key PEM export
  - it occurs after certificate PEM export has already succeeded, so this batch must isolate the second occurrence of the same symbol
- this batch needs a wrapper-based RED mechanism because the same `BIO_read` symbol is already guarded at the earlier certificate PEM export call site:
  - install a focused wrapper that lets the first certificate PEM export `BIO_read` succeed
  - then clear the global `BIO_read` symbol before the private-key PEM export readback
  - keep the batch limited to the second call site only
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test for the second `BIO_read` gap
  - one local `Assigned(BIO_read)` guard before the private-key PEM export `BIO_read(...)`
  - no mixed fixes for `BIO_free`, `X509_free`, or `EVP_PKEY_free`
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when private-key PEM export read helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- the first RED compile attempt showed the wrapper needed the same type-narrowing pattern used earlier for duplicate-call symbol tests:
  - using `PBIO` in the test-local wrapper caused FPC symbol-resolution errors
  - narrowing the wrapper signature to `Pointer` preserved the test intent and let RED reach runtime behavior
- fresh RED confirmed the current failure shape exactly at the private-key PEM export readback call site:
  - a wrapper-based test can let the first certificate PEM export `BIO_read` call succeed and then clear the global symbol before the second call
  - direct `TCertificateUtils.GenerateSigned(...)` then raised `EAccessViolation` when the private-key PEM export `BIO_read(...)` was reached
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local private-key export guard is sufficient:
  - adding `Assigned(BIO_read)` immediately before the private-key PEM export `BIO_read(...)` converts the second direct call back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the helper stays available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `BIO_free(LBIO)` in the private-key PEM export `finally` block
  - later outer `X509_free` / `EVP_PKEY_free` cleanup should remain isolated into separate later batches

## 2026-03-22 Findings (Certificate utils GenerateSigned BIO_read symbol guard)
- after closing the signed-path issuer-name setter `X509_set_issuer_name` batch, fresh discovery at the first `AddExtension(LCert, LCACert, ...)` boundary showed the shared helper is already fully guarded for the previously isolated extension symbols:
  - `X509V3_set_ctx`
  - `X509V3_EXT_conf_nid`
  - `X509_add_ext`
  - `X509_EXTENSION_free`
- with the extension helper chain already guarded, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves to:
  - the first certificate PEM export readback `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
  - this occurs after CA load, leaf generation, subject/issuer assignment, extension setup, and CA signing have already succeeded
- this batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `BIO_read`
  - one local `Assigned(BIO_read)` guard before the first certificate PEM export `BIO_read(...)`
  - no mixed fixes for private-key export `BIO_read` or outer cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when certificate PEM export read helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the certificate PEM export readback call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `BIO_read` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(BIO_read)` immediately before the first certificate PEM export `BIO_read(...)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when `BIO_read` is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - private-key PEM export `BIO_read`
  - outer `X509_free` / `EVP_PKEY_free` cleanup should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_set_issuer_name symbol guard)
- after closing the CA subject-name `X509_get_subject_name(LCACert)` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_set_issuer_name(LCert, LCAName)`
  - both leaf and CA subject-name retrieval are already complete at that point, while the extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_set_issuer_name`
  - one local `Assigned(X509_set_issuer_name)` guard in `GenerateSigned(...)`
  - no mixed fixes for later extension, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when issuer-name setter helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the issuer-name setter call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_set_issuer_name` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_set_issuer_name)` immediately before `X509_set_issuer_name(LCert, LCAName)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next execution boundary forward to:
  - the first `AddExtension(LCert, LCACert, ...)` helper call
  - the next batch should re-enter fresh single-symbol discovery inside the extension helper chain before touching export or cleanup helpers

## 2026-03-22 Findings (Certificate utils GenerateSigned CA X509_get_subject_name symbol guard)
- after closing the leaf subject-name `X509_get_subject_name(LCert)` batch, the next earliest direct symbol that still remains independently exposable on the successful `GenerateSigned(...)` path is:
  - CA subject-name retrieval `X509_get_subject_name(LCACert)`
  - it occurs after leaf subject construction succeeds and immediately before `X509_set_issuer_name(LCert, LCAName)`
- this batch needs a slightly different RED mechanism because the same symbol is already guarded at the leaf call site:
  - use a focused wrapper/stub that lets the first `X509_get_subject_name` call succeed
  - then clear the global symbol so the second call at `LCACert` becomes a direct nil-call
  - keep the batch limited to the second call site only
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test for the CA retrieval gap
  - one local `Assigned(X509_get_subject_name)` guard before `LCAName := X509_get_subject_name(LCACert)`
  - no mixed fixes for `AddNameEntry(...)`, `X509_set_issuer_name`, extension chain, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when the CA subject-name helper is unavailable at the issuer-setup stage
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the CA subject-name call site:
  - a wrapper-based test can let the first leaf `X509_get_subject_name` call succeed and then clear the global symbol before the CA call
  - direct `TCertificateUtils.GenerateSigned(...)` then raised `EAccessViolation` when `X509_get_subject_name(LCACert)` was reached
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local CA guard is sufficient:
  - adding `Assigned(X509_get_subject_name)` immediately before `LCAName := X509_get_subject_name(LCACert)` converts the second direct call back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the CA helper remains available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_set_issuer_name(LCert, LCAName)`
  - later extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_get_subject_name symbol guard)
- after closing the signed leaf public-key attach `X509_set_pubkey` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_get_subject_name(LCert)`
  - leaf allocation/versioning/serial/validity/public-key attach are already complete at that point, while issuer-name assignment, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_get_subject_name`
  - one local `Assigned(X509_get_subject_name)` guard in `GenerateSigned(...)`
  - no mixed fixes for `AddNameEntry(...)`, issuer-name helpers, extension chain, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf subject-name helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the leaf subject-name call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_get_subject_name` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_get_subject_name)` immediately before `LName := X509_get_subject_name(LCert)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - CA subject-name retrieval `X509_get_subject_name(LCACert)`
  - `AddNameEntry(...)` already guards its internal `X509_NAME_add_entry_by_txt`, while later `X509_set_issuer_name`, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_set_pubkey symbol guard)
- after closing the signed leaf validity-adjustment `X509_gmtime_adj` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_set_pubkey(LCert, LKey)`
  - leaf allocation/versioning/serial/validity setup are already complete at that point, while subject-name retrieval, issuer-name assignment, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_set_pubkey`
  - one local `Assigned(X509_set_pubkey)` guard in `GenerateSigned(...)`
  - no mixed fixes for later name, extension, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf public-key attach helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the public-key attach call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_set_pubkey` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_set_pubkey)` immediately before `X509_set_pubkey(LCert, LKey)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_get_subject_name(LCert)`
  - `AddNameEntry(...)` already guards its internal `X509_NAME_add_entry_by_txt`, while later issuer-name helpers, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_gmtime_adj symbol guard)
- after closing the signed leaf validity-end `X509_get_notAfter` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - the first `X509_gmtime_adj(...)` call, currently `X509_gmtime_adj(LNotBefore, 0)`
  - both validity getters are already complete at that point, while the second `X509_gmtime_adj` call, `X509_set_pubkey`, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_gmtime_adj`
  - one local `Assigned(X509_gmtime_adj)` guard in `GenerateSigned(...)`
  - no mixed fixes for later public-key attach, name, extension, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf validity-adjustment helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the first validity-adjustment call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_gmtime_adj` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_gmtime_adj)` immediately before the first `X509_gmtime_adj(...)` call converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_set_pubkey(LCert, LKey)`
  - later subject/issuer helpers, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_get_notAfter symbol guard)
- after closing the signed leaf validity-start `X509_get_notBefore` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_get_notAfter(LCert)`
  - leaf allocation/versioning/serial setup/validity-start lookup are already complete at that point, while `X509_gmtime_adj`, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_get_notAfter`
  - one local `Assigned(X509_get_notAfter)` guard in `GenerateSigned(...)`
  - no mixed fixes for later validity adjustment, name, extension, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf validity-end helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the validity-end call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_get_notAfter` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_get_notAfter)` immediately before `LNotAfter := X509_get_notAfter(LCert)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_gmtime_adj(...)`
  - later subject/issuer helpers, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_get_notBefore symbol guard)
- after closing the signed leaf serial-setter `ASN1_INTEGER_set` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_get_notBefore(LCert)`
  - leaf allocation/versioning/serial setup are already complete at that point, while `X509_get_notAfter`, `X509_gmtime_adj`, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_get_notBefore`
  - one local `Assigned(X509_get_notBefore)` guard in `GenerateSigned(...)`
  - no mixed fixes for later validity-end adjustment, name, extension, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf validity-start helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the validity-start call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_get_notBefore` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_get_notBefore)` immediately before `LNotBefore := X509_get_notBefore(LCert)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_get_notAfter(LCert)`
  - later `X509_gmtime_adj`, subject/issuer helpers, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned ASN1_INTEGER_set symbol guard)
- after closing the signed leaf serial-lookup `X509_get_serialNumber` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `ASN1_INTEGER_set(LSerial, ...)`
  - leaf allocation/versioning/serial lookup are already complete at that point, while validity helpers, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `ASN1_INTEGER_set`
  - one local `Assigned(ASN1_INTEGER_set)` guard in `GenerateSigned(...)`
  - no mixed fixes for later validity, name, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf serial setters are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the serial-setter call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `ASN1_INTEGER_set` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(ASN1_INTEGER_set)` immediately before the leaf `ASN1_INTEGER_set(...)` calls converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_get_notBefore(LCert)`
  - later `X509_get_notAfter`, `X509_gmtime_adj`, subject/issuer helpers, extension chain, PEM export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_get_serialNumber symbol guard)
- after closing the signed leaf versioning `X509_set_version` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_get_serialNumber(LCert)`
  - leaf allocation/versioning are already complete at that point, while `ASN1_INTEGER_set`, validity helpers, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_get_serialNumber`
  - one local `Assigned(X509_get_serialNumber)` guard in `GenerateSigned(...)`
  - no mixed fixes for later ASN.1 serial setting, validity, name, export, or cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf serial helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the serial-helper call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_get_serialNumber` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_get_serialNumber)` immediately before `LSerial := X509_get_serialNumber(LCert)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `ASN1_INTEGER_set(LSerial, ...)`
  - later validity, name, extension, export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_set_version symbol guard)
- after closing the signed leaf allocation `X509_new` batch, the next earliest direct symbol on the successful `GenerateSigned(...)` path moves immediately to:
  - `X509_set_version(LCert, X509_VERSION_3)`
  - leaf allocation is already complete at that point, while serial setters, validity helpers, subject/issuer helpers, extension chain, PEM export, and cleanup all remain downstream
- this next batch should stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_set_version`
  - one local `Assigned(X509_set_version)` guard in `GenerateSigned(...)`
  - no mixed fixes for later serial/validity/name/export/cleanup helpers
- the signed-generation public contract remains the same for this batch:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf version helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear outputs
- fresh RED confirmed the current failure shape exactly at the versioning call site:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_set_version` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_set_version)` immediately before `X509_set_version(LCert, X509_VERSION_3)` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_get_serialNumber(LCert)`
  - later ASN.1 serial setting, validity, name, extension, export, and cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSigned X509_new symbol guard)
- fresh discovery shows `GenerateSigned(...)` should start a new single-symbol queue instead of reusing the old broad BIO guard batch:
  - CA certificate PEM load is already front-stopped by `HasCertificatePEMReadBIOHelpers`
  - CA private-key PEM load is already front-stopped by `HasPrivateKeyPEMReadBIOHelpers`
- after those helper predicates and successful CA material parsing, the next earliest direct symbol on the successful signed-generation path is:
  - leaf certificate allocation `X509_new()`
  - leaf key generation happens earlier through dedicated helper functions, while later X509 setters/signing/export/cleanup all remain downstream of this allocation point
- the next batch should therefore stay narrow:
  - one focused `GenerateSigned(...)` / `TryGenerateSigned(...)` contract test that nils `X509_new`
  - one local `Assigned(X509_new)` guard in `GenerateSigned(...)`
  - no mixed fixes for later mutation, export, or cleanup helpers
- this batch keeps the signed-generation public contract unchanged:
  - direct `TCertificateUtils.GenerateSigned(...)` should raise controlled `ESSLCertError` when leaf allocation helpers are unavailable
  - `TCertificateUtils.TryGenerateSigned(...)` should stay non-throwing, return `False`, and clear `ACertPEM` / `AKeyPEM`
- fresh RED confirmed the current failure shape exactly at the allocation point:
  - direct `TCertificateUtils.GenerateSigned(...)` raised `EAccessViolation` when `X509_new` was nil
  - `TCertificateUtils.TryGenerateSigned(...)` continued to swallow the exception, returned `False`, and cleared both outputs
  - summary: `Passed: 5, Failed: 1, Skipped: 0`
- fresh GREEN confirmed the local guard is sufficient:
  - adding `Assigned(X509_new)` immediately before `LCert := X509_new()` converts the direct path back to controlled `ESSLCertError`
  - successful CA-signed generation remains unchanged when the symbol is available
- fresh discovery after GREEN moves the next earliest direct symbol forward to:
  - `X509_set_version(LCert, X509_VERSION_3)`
  - later serial/validity/name/extension/export/cleanup helpers should remain isolated into later single-symbol batches

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned EVP_PKEY_free symbol guard)
- 在收完 outer certificate cleanup `X509_free` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - outer private-key cleanup path 上的 `EVP_PKEY_free(LKey)`
  - 当前函数在证书 handle cleanup 完成后才会进入最外层 private-key cleanup，这正好是 self-signed successful path 上最后一条未收口的 direct cleanup call
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批不需要扩大到其他 helper：
  - contract test 只需在 warmup 后暂时置空 `EVP_PKEY_free`，就能稳定把 failure 隔离到 outer private-key cleanup
  - `GenerateSelfSigned(...)` 当前 direct helper/cleanup chain 在它之后已经没有更早可拆的 distinct symbol
- 最小正确修法只需要留在当前 cleanup call site：
  - 在 `GenerateSelfSigned(...)` 的最外层 key cleanup 前增加 `Assigned(EVP_PKEY_free)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不顺手改动 `GenerateSigned(...)` 或其他 generation paths
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 outer cleanup `EVP_PKEY_free(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 cleanup guard 就足够收口：
  - 在 outer private-key cleanup 前直接 guard `EVP_PKEY_free`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明 `GenerateSelfSigned(...)` 当前成功执行路径上的 direct helper/cleanup chain 已基本收口：
  - 当前更合适的下一批入口转向 `TCertificateUtils.GenerateSigned(...)`
  - 仍应保持 single-symbol scope，不直接复用旧的 broad BIO guard 计划

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_free symbol guard)
- 在收完 certificate PEM export `BIO_read` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - outer certificate cleanup path 上的 `X509_free(LCert)`
  - 当前函数在证书与私钥 PEM 都导出完成后，首先进入 certificate handle cleanup，然后才会继续到 outer key cleanup
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批不需要扩大到 key cleanup：
  - contract test 只需在 warmup 后暂时置空 `X509_free`，就能稳定把 failure 隔离到 outer certificate cleanup
  - `EVP_PKEY_free` 明确还在其后，应该保留给下一批独立收口
- 最小正确修法只需要留在当前 cleanup call site：
  - 在 `GenerateSelfSigned(...)` 的 outer certificate cleanup 前增加 `Assigned(X509_free)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `EVP_PKEY_free` 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 outer cleanup `X509_free(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 cleanup guard 就足够收口：
  - 在 outer certificate cleanup 前直接 guard `X509_free`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - outer private-key cleanup path 上的 `EVP_PKEY_free(LKey)`
  - 没有更早的 direct helper 留在 `X509_free` 与函数退出之间

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned BIO_read symbol guard)
- 在收完 `AddExtension(...)` helper chain 与 signing path 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - certificate PEM export path 上的 `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
  - 当前路径在成功写入 `PEM_write_bio_X509(...)` 后会立刻进入 BIO memory readback，这就是 PEM export 段上第一条未收口的 direct call
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批不需要更宽的 BIO helper 改造：
  - `HasCertificatePEMWriteBIOHelpers` 已经覆盖 `BIO_new`、`BIO_s_mem`、`PEM_write_bio_X509`、`BIO_free`
  - contract test 只需在 warmup 后暂时置空 `BIO_read`，就能稳定把 failure 隔离到证书 PEM export readback
- 最小正确修法只需要留在当前 call site：
  - 在证书 PEM export 的第一次 `BIO_read(...)` 前增加 `Assigned(BIO_read)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 key export 或 outer cleanup 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为证书 PEM export `BIO_read(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 call-site guard 就足够收口：
  - 在 certificate PEM export readback 前直接 guard `BIO_read`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - outer certificate cleanup path 上的 `X509_free(LCert)`
  - `EVP_PKEY_free` 与更晚的 cleanup helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned AddExtension X509_EXTENSION_free symbol guard)
- 在收完 `AddExtension(...)` 里的 `X509_add_ext` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `X509_EXTENSION_free(LExt)`
  - extension attach 完成后，局部 helper 会立即进入 extension object cleanup，这就是 AddExtension helper chain 上最后一条未收口的 direct call
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 已经装载 X509 helpers
  - contract test 可以直接暂时置空 `X509_EXTENSION_free`，稳定把 failure 隔离到 `AddExtension(...)`
- 最小正确修法只需要留在局部 helper：
  - 在 `AddExtension(...)` 内、进入 extension cleanup 前增加 `Assigned(X509_EXTENSION_free)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 signing 或 PEM export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_EXTENSION_free(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `AddExtension(...)` 的 extension-cleanup helper 调用前直接 guard `X509_EXTENSION_free`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - certificate PEM export path 上的 `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
  - key PEM export、`X509_free`、`EVP_PKEY_free` 与更晚的 helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned AddExtension X509_add_ext symbol guard)
- 在收完 `AddExtension(...)` 里的 `X509V3_EXT_conf_nid` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `X509_add_ext(ACert, LExt, -1)`
  - extension object 构造成功后，self-signed path 立刻进入扩展附加，这就是 cleanup 之前的最后一步 attach call
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 已经装载 X509v3 / X509 helpers
  - contract test 可以直接暂时置空 `X509_add_ext`，稳定把 failure 隔离到 `AddExtension(...)`
- 最小正确修法只需要留在局部 helper：
  - 在 `AddExtension(...)` 内、`X509_add_ext(...)` 前增加 `Assigned(X509_add_ext)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_EXTENSION_free`、signing 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_add_ext(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `AddExtension(...)` 的 extension-attach helper 调用前直接 guard `X509_add_ext`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `AddExtension(...)` 里的 `X509_EXTENSION_free(LExt)`
  - 更晚的 signing / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned AddExtension X509V3_EXT_conf_nid symbol guard)
- 在收完 `AddExtension(...)` 里的 `X509V3_set_ctx` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `X509V3_EXT_conf_nid(nil, @LCtx, ANID, PAnsiChar(AnsiString(AValue)))`
  - X509v3 context 准备完成后，当前 helper 立刻进入扩展对象构造，这是 extension path 上第一条新的 direct call
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 已经装载 X509v3 helpers
  - contract test 可以直接暂时置空 `X509V3_EXT_conf_nid`，稳定把 failure 隔离到 `AddExtension(...)`
- 最小正确修法只需要留在局部 helper：
  - 在 `AddExtension(...)` 内、`X509V3_EXT_conf_nid(...)` 前增加 `Assigned(X509V3_EXT_conf_nid)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_add_ext`、`X509_EXTENSION_free`、signing 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509V3_EXT_conf_nid(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `AddExtension(...)` 的 extension-construction helper 调用前直接 guard `X509V3_EXT_conf_nid`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `AddExtension(...)` 里的 `X509_add_ext(ACert, LExt, -1)`
  - `X509_EXTENSION_free` 和更晚的 signing / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned AddExtension X509V3_set_ctx symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_set_issuer_name` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `AddExtension(...)` 里的 `X509V3_set_ctx(@LCtx, AIssuer, ACert, nil, nil, 0)`
  - issuer 赋值完成后，self-signed 路径马上进入 Basic Constraints 扩展构造，而 `AddExtension(...)` 入口第一条 direct OpenSSL 调用就是 X509v3 context 初始化
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 已经装载 X509v3 helpers
  - contract test 可以直接暂时置空 `X509V3_set_ctx`，稳定把 failure 隔离到 `AddExtension(...)`
- 最小正确修法只需要留在局部 helper：
  - 在 `AddExtension(...)` 内、`X509V3_set_ctx(...)` 前增加 `Assigned(X509V3_set_ctx)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509V3_EXT_conf_nid`、`X509_add_ext`、`X509_EXTENSION_free`、signing 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509V3_set_ctx(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `AddExtension(...)` 的 extension-context helper 调用前直接 guard `X509V3_set_ctx`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `AddExtension(...)` 里的 `X509V3_EXT_conf_nid(nil, @LCtx, ANID, PAnsiChar(AnsiString(AValue)))`
  - `X509_add_ext`、`X509_EXTENSION_free` 和更晚的 signing / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_set_issuer_name symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `AddNameEntry(...)` / `X509_NAME_add_entry_by_txt` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `X509_set_issuer_name(LCert, LName)`
  - 当前函数完成 subject fields 写入后，紧接着就进入 issuer=subject 的自签名赋值
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_set_issuer_name`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`X509_set_issuer_name(LCert, LName)` 前增加 `Assigned(X509_set_issuer_name)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `AddExtension(...)`、signing 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_set_issuer_name(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 issuer-name setter 调用前直接 guard `X509_set_issuer_name`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- fresh discovery 说明当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `AddExtension(...)` 里的 `X509V3_set_ctx(@LCtx, AIssuer, ACert, nil, nil, 0)`
  - `X509V3_EXT_conf_nid`、`X509_add_ext`、`X509_EXTENSION_free` 和更晚的 signing / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned AddNameEntry X509_NAME_add_entry_by_txt symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_get_subject_name` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `AddNameEntry(...)` 里的 `X509_NAME_add_entry_by_txt(...)`
  - 当前函数拿到 `LName` 后，第一条真正写入 subject fields 的 OpenSSL 调用就落在这个局部 helper 里
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_NAME_add_entry_by_txt`，稳定把 failure 隔离到 `AddNameEntry(...)`
- 最小正确修法只需要留在局部 helper：
  - 在 `AddNameEntry(...)` 内、`X509_NAME_add_entry_by_txt(...)` 前增加 `Assigned(X509_NAME_add_entry_by_txt)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_set_issuer_name`、extension helpers 或 export 路径
- 为了让 RED 聚焦到 symbol gap，本批还修正了一个无关测试夹具问题：
  - 初版 test fixture 的 `CommonName` 过长，baseline warmup 先因为正常 `AddNameEntry(...)` 失败而偏离目标
  - 缩短 `CommonName` 后，failure 重新稳定落回 nil helper contract
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_NAME_add_entry_by_txt(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `AddNameEntry(...)` 的 subject-name entry 调用前直接 guard `X509_NAME_add_entry_by_txt`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `X509_set_issuer_name(LCert, LName)`
  - 后续 extension helpers 与 export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_get_subject_name symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_set_pubkey` 之后，当前执行路径上的下一条最早还能独立拆批的 direct symbol 是：
  - `X509_get_subject_name(LCert)`
  - 当前函数在把公钥挂到证书后，紧接着就读取主题名称对象以填充 `C/ST/L/O/OU/CN`
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_get_subject_name`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LName := X509_get_subject_name(LCert)` 前增加 `Assigned(X509_get_subject_name)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `AddNameEntry(...)`、`X509_set_issuer_name` 或 extension / export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_get_subject_name(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 subject-name getter 调用前直接 guard `X509_get_subject_name`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，当前执行路径上的下一条最早 direct symbol 已经前移到：
  - `AddNameEntry(...)` 里的 `X509_NAME_add_entry_by_txt(...)`
  - `X509_set_issuer_name(LCert, LName)` 和更晚的 extension / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_set_pubkey symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_gmtime_adj` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_set_pubkey(LCert, LKey)`
  - 当前函数在完成有效期设置后，紧接着就把生成好的公钥挂到证书对象上
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_set_pubkey`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`X509_set_pubkey(LCert, LKey)` 前增加 `Assigned(X509_set_pubkey)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_get_subject_name`、`X509_set_issuer_name` 或 extension / export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_set_pubkey(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 public-key attach 调用前直接 guard `X509_set_pubkey`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_get_subject_name(LCert)`
  - `X509_set_issuer_name(LCert, LName)` 和更晚的 extension / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_gmtime_adj symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_get_notAfter` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_gmtime_adj(LNotBefore, ...)`
  - 当前函数在拿到 `LNotBefore` / `LNotAfter` 指针后，紧接着就开始做证书有效期调整
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_gmtime_adj`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、第一处 `X509_gmtime_adj(...)` 调用之前增加 `Assigned(X509_gmtime_adj)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_set_pubkey`、subject-name 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_gmtime_adj(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 validity-adjustment 调用前直接 guard `X509_gmtime_adj`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_set_pubkey(LCert, LKey)`
  - `X509_get_subject_name(LCert)` 和更晚的 issuer-name / extension / export helpers 仍然在它之后，应该继续拆成独立批次

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_get_notAfter symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_get_notBefore` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_get_notAfter(LCert)`
  - 当前函数在拿到 validity-start 指针后，紧接着就读取证书有效期结束时间
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_get_notAfter`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LNotAfter := X509_get_notAfter(LCert)` 前增加 `Assigned(X509_get_notAfter)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_gmtime_adj`、subject-name 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_get_notAfter(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 validity-end 调用前直接 guard `X509_get_notAfter`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_gmtime_adj(LNotBefore, ...)`
  - 后续对 `LNotAfter` 的 `X509_gmtime_adj(...)` 调用和更晚的 `X509_set_pubkey(...)` 仍然在它之后，应该继续拆成独立批次
## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_get_notBefore symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `ASN1_INTEGER_set` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_get_notBefore(LCert)`
  - 当前函数在 serial value 设置完成后，紧接着就开始读取证书有效期起始时间
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_get_notBefore`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LNotBefore := X509_get_notBefore(LCert)` 前增加 `Assigned(X509_get_notBefore)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_get_notAfter`、`X509_gmtime_adj`、subject-name 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_get_notBefore(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 validity-start 调用前直接 guard `X509_get_notBefore`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_get_notAfter(LCert)`
  - `X509_gmtime_adj(...)` 仍然在两个 time helper 之后，应该继续留给后续独立批次
## 2026-03-22 Findings (Certificate utils GenerateSelfSigned ASN1_INTEGER_set symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_get_serialNumber` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `ASN1_INTEGER_set(LSerial, ...)`
  - 当前函数在拿到 `LSerial` 后，立即进入 serial value 设置分支
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载 ASN.1 helper
  - 只要在测试里显式加载 ASN.1 支持后再暂时置空 `ASN1_INTEGER_set`，就能稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LSerial := X509_get_serialNumber(LCert)` 之后、任一 `ASN1_INTEGER_set(...)` 调用之前增加 `Assigned(ASN1_INTEGER_set)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `X509_get_notBefore` / `X509_get_notAfter`、`X509_gmtime_adj` 或 export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `ASN1_INTEGER_set(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 serial setter 调用前直接 guard `ASN1_INTEGER_set`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_get_notBefore(LCert)`
  - `X509_get_notAfter(LCert)` 和 `X509_gmtime_adj(...)` 仍然在它之后，应该继续分开做单符号批次
## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_get_serialNumber symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_set_version` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_get_serialNumber(LCert)`
  - 当前函数在 certificate version 设置成功后，紧接着就是 serial helper 读取
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_get_serialNumber`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LSerial := X509_get_serialNumber(LCert)` 前增加 `Assigned(X509_get_serialNumber)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 `ASN1_INTEGER_set`、validity 或 PEM export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_get_serialNumber(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 serial helper 调用前直接 guard `X509_get_serialNumber`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的最早 direct symbol 已经继续前移：
  - `X509_get_serialNumber(LCert)` 已收口
  - 下一批必须继续按单符号批次做 fresh discovery，不能直接假定后续是 `ASN1_INTEGER_set` 或 validity helper
- 基于当前调用顺序，一个合理候选是：
  - `ASN1_INTEGER_set(LSerial, 1)`
  - 这是代码顺序推断，不是已验证结论；开新批前仍需重新确认
## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_set_version symbol guard)
- 在收完 `GenerateSelfSigned(...)` 里的 `X509_new` 之后，同一函数里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_set_version(LCert, X509_VERSION_3)`
  - 当前函数在 certificate allocation 成功后，紧接着就是版本设置调用
- 这条线继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批也不需要晚期失效 stub：
  - warmup 后 `EnsureInitialized` 不会重新装载该 helper
  - contract test 可以直接暂时置空 `X509_set_version`，稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`X509_set_version(LCert, X509_VERSION_3)` 前增加 `Assigned(X509_set_version)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改 serial/validity/export 路径
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_set_version(...)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的版本设置调用前直接 guard `X509_set_version`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_get_serialNumber(LCert)`
  - 下一批应继续按单符号批次推进，而不是混入 validity 或 export 路径

## 2026-03-22 Findings (Certificate utils GenerateSelfSigned X509_new symbol guard)
- 在 generation 线里复扫 `src/fafafa.ssl.cert.utils.pas` 后，`TCertificateUtils.GenerateSelfSigned(...)` 当前最早还能独立拆批的 direct symbol 是：
  - `X509_new()`
  - 当前函数在 key generation 成功后，第一条 certificate-side direct OpenSSL call 就是 `LCert := X509_new()`
- 这条线应该继续沿用现有 self-signed generation contract：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 在 helper 缺失时应抛受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 应保持 non-throwing、返回 `False`，并清空输出
- 这批不需要晚期失效 stub：
  - `EnsureInitialized` 在 warmup 后会因为 `osmInitCert` 已加载而直接返回
  - 因此 contract test 只要在 warmup 后直接暂时置空 `X509_new`，就能稳定把 failure 隔离到 `GenerateSelfSigned(...)`
- 最小正确修法只需要留在局部函数：
  - 在 `GenerateSelfSigned(...)` 内、`LCert := X509_new()` 前增加 `Assigned(X509_new)` guard
  - helper 缺失时抛受控 `ESSLCertError`，不改 Try wrappers 语义，也不提前修改后续 `X509_set_version`、serial、validity、export 等 generation helpers
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GenerateSelfSigned(...)` 会因为 `X509_new()` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` 与 `TCertificateUtils.TryGenerateSelfSignedSimple(...)` 会继续走异常兜底，清空输出并返回 `False`
  - summary: `Passed: 9, Failed: 1, Skipped: 0`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `GenerateSelfSigned(...)` 的 allocation 调用前直接 guard `X509_new`，就能把 direct path 稳定转回受控 `ESSLCertError`
  - 两个 Try wrapper 继续 non-throwing、返回 `False`，并清空 `ACertPEM` / `AKeyPEM`
- 到这一步，同一函数里的下一条最早 direct symbol 已经前移到：
  - `X509_set_version(LCert, X509_VERSION_3)`
  - 下一批应继续按单符号批次推进，而不是混入 serial/validity/export 路径

## 2026-03-22 Findings (Certificate utils GetInfo X509NameToString BIO_read symbol guard)
- 在收完 `X509NameToString(...)` 里的 `X509_NAME_print_ex` 之后，同一 helper 里最后一条还能独立拆批的 direct symbol 是：
  - `BIO_read(...)`
  - 当前 helper 在成功分配 memory BIO 并写入 X509 name 后，直接执行 `LLen := BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))`
- 这条线继续沿用前几批的 partial/full info contract：
  - `X509_get_subject_name` / `X509_get_issuer_name` 仍然可用，`BIO_s_mem`、`BIO_new`、`X509_NAME_print_ex` 也已经有本地 guard
  - 因此最小正确语义仍然是“`Subject` / `Issuer` 置空，但后续 metadata 与 SAN decoding 继续保留”
- 这批也不需要晚期失效 stub：
  - `GetInfo(...)` 入口不会主动重装或前置检查 `BIO_read`
  - 因此 contract test 可以直接暂时置空 `BIO_read`，稳定把 failure 隔离到 `X509NameToString(...)`
- 最小正确修法应该继续留在局部 helper：
  - 在 `X509NameToString(...)` 内、`BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))` 前增加 `Assigned(BIO_read)` guard
  - helper 缺失时直接返回空字符串，让 `GetInfo(...)` 继续跑后续 metadata extraction
  - 不改 `TryGetInfo(...)` 返回值语义，也不修改更广的 `GetInfo(...)` 流程
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GetInfo(...)` 会因为 `BIO_read(LBIO, @LBuffer[0], SizeOf(LBuffer))` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空后续 metadata 和 `SubjectAltNames`，并返回 `False`
  - summary: `Passed: 7, Failed: 12, Skipped: 0`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `X509NameToString(...)` 的读取调用前直接 guard `BIO_read`，就能把 `Subject` / `Issuer` 稳定降级为空字符串
  - `TCertificateUtils.GetInfo(...)` 继续保留后续 metadata 与 decoded `SubjectAltNames`
  - `TCertificateUtils.TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`
- 到这一步，`X509NameToString(...)` 内原先还能独立拆批的 direct helper chain 已全部本地 guard：
  - `BIO_s_mem`
  - `BIO_new`
  - `X509_NAME_print_ex`
  - `BIO_read`
  - 下一批应该回到 `cert.utils` 做 helper 外的 fresh single-symbol discovery，而不是继续在这个 formatter 里扩批

## 2026-03-22 Findings (Certificate utils GetInfo X509NameToString X509_NAME_print_ex symbol guard)
- 在收完 `X509NameToString(...)` 里的 `BIO_new` 之后，同一 helper 里下一条最早还能独立拆批的 direct symbol 是：
  - `X509_NAME_print_ex(...)`
  - 当前 helper 在拿到 `LBIO` 后直接执行 `X509_NAME_print_ex(LBIO, AName, 0, 0)`
  - 这比后面的 `BIO_read` 更早触发
- 这条线继续沿用上一批的 partial/full info contract：
  - `X509_get_subject_name` / `X509_get_issuer_name` 仍然可用，`BIO_s_mem` 和 `BIO_new` 也已经有本地 guard
  - 因此最小正确语义仍然是“`Subject` / `Issuer` 置空，但后续 metadata 与 SAN decoding 继续保留”
- 这批不需要晚期失效 stub：
  - `GetInfo(...)` 入口不会主动重装或前置检查 `X509_NAME_print_ex`
  - 因此 contract test 可以直接暂时置空 `X509_NAME_print_ex`，稳定把 failure 隔离到 `X509NameToString(...)`
- 最小正确修法应该继续留在局部 helper：
  - 在 `X509NameToString(...)` 内、`X509_NAME_print_ex(LBIO, AName, 0, 0)` 前增加 `Assigned(X509_NAME_print_ex)` guard
  - helper 缺失时直接返回空字符串，让 `GetInfo(...)` 继续跑后续 metadata extraction
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `BIO_read` 或更广的 `GetInfo(...)` 流程
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GetInfo(...)` 会因为 `X509_NAME_print_ex(LBIO, AName, 0, 0)` 的 direct nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空后续 metadata 和 `SubjectAltNames`，并返回 `False`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `X509NameToString(...)` 的打印调用前直接 guard `X509_NAME_print_ex`，就能把 `Subject` / `Issuer` 稳定降级为空字符串
  - `TCertificateUtils.GetInfo(...)` 继续保留后续 metadata 与 decoded `SubjectAltNames`
  - `TCertificateUtils.TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo X509NameToString BIO_new symbol guard)
- 在收完 `X509NameToString(...)` 里的 `BIO_s_mem` 之后，同一 helper 里下一条最早还能独立拆批的 direct symbol 是：
  - `BIO_new(...)`
  - 当前 helper 直接执行 `LBIO := BIO_new(BIO_s_mem())`
  - 在 `BIO_s_mem` 已经有本地 guard 的前提下，这就是下一条最早会触发的 direct nil-call
- 这条线继续沿用上一批的 partial/full info contract：
  - `X509_get_subject_name` / `X509_get_issuer_name` 仍然可用，缺的是名字字符串化时的 BIO allocator
  - 因此最小正确语义仍然是“`Subject` / `Issuer` 置空，但后续 metadata 与 SAN decoding 继续保留”
- 这批不能直接在入口前置空 `BIO_new`：
  - `GetInfo(...)` 入口会在 `if not Assigned(BIO_new) then LoadOpenSSLBIO()` 里尝试重装 helper
  - 因此 contract test 需要用“晚一点失效”的 stub，在 `X509_get_subject_name(...)` 成功后再禁用 `BIO_new`，这样才能稳定隔离到 `X509NameToString(...)`
- 最小正确修法应该继续留在局部 helper：
  - 在 `X509NameToString(...)` 内、`BIO_new(BIO_s_mem())` 前增加 `Assigned(BIO_new)` guard
  - helper 缺失时直接返回空字符串，让 `GetInfo(...)` 继续跑后续 metadata extraction
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_NAME_print_ex`、`BIO_read` 或更广的 `GetInfo(...)` 流程
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - 用 `X509_get_subject_name(...)` late-loss stub 可以稳定绕过入口重载，并把 `BIO_new` 缺失定位到 `X509NameToString(...)`
  - direct `TCertificateUtils.GetInfo(...)` 会因为 `BIO_new(BIO_s_mem())` 的 allocator nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空后续 metadata 和 `SubjectAltNames`，并返回 `False`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `X509NameToString(...)` 开头直接 guard `BIO_new`，就能把 `Subject` / `Issuer` 稳定降级为空字符串
  - `TCertificateUtils.GetInfo(...)` 继续保留后续 metadata 与 decoded `SubjectAltNames`
  - `TCertificateUtils.TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo X509NameToString BIO_s_mem symbol guard)
- 在收完 outer `BIO_free` cleanup 之后，`GetInfo(...)` 相邻局部 helper 里下一条最早还能独立拆批的 direct symbol 是 `TCertificateUtils.X509NameToString(...)` 里的：
  - `BIO_s_mem()`
  - 当前 helper 直接执行 `LBIO := BIO_new(BIO_s_mem())`
  - 这比同 helper 里的 `BIO_new`、`X509_NAME_print_ex`、`BIO_read` 更早触发
- 这条线不该退回到“整份 empty info”语义：
  - `X509_get_subject_name` / `X509_get_issuer_name` 仍然可用，缺的是名字字符串化时的内存 BIO helper
  - 因此最小正确语义是“`Subject` / `Issuer` 置空，但后续 metadata 与 SAN decoding 继续保留”
  - 这更符合 `GetInfo(...)` 的 best-effort partial/full info contract
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合验证名字字符串 helper 缺失时，后续 SAN decode 仍然可观测
- 最小正确修法应该留在局部 helper：
  - 在 `X509NameToString(...)` 内、`BIO_new(BIO_s_mem())` 前增加 `Assigned(BIO_s_mem)` guard
  - helper 缺失时直接返回空字符串，让 `GetInfo(...)` 继续跑后续 metadata extraction
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_NAME_print_ex`、`BIO_read`、`BIO_new` 或更广的 `GetInfo(...)` 流程
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GetInfo(...)` 会因为 `BIO_new(BIO_s_mem())` 里的 `BIO_s_mem()` nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空后续 metadata 和 `SubjectAltNames`，并返回 `False`
- fresh GREEN 说明本地 helper guard 就足够收口：
  - 在 `X509NameToString(...)` 开头直接 guard `BIO_s_mem`，就能把 `Subject` / `Issuer` 稳定降级为空字符串
  - `TCertificateUtils.GetInfo(...)` 继续保留后续 metadata 与 decoded `SubjectAltNames`
  - `TCertificateUtils.TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo BIO_free cleanup symbol guard)
- 在收完 `X509_free` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct cleanup symbol 是：
  - outer `finally` 里的 `BIO_free(LBIO)`
- 这条线和早先的 `GetInfo BIO guard` 不是同一个 contract：
  - 旧批次验证的是“入口缺少 `BIO_free` 时通过 `HasCertificatePEMReadBIOHelpers` silent-degrade 到 empty info”
  - 新批次要验证的是“入口先通过，再在执行过程中丢掉 `BIO_free` 时，outer cleanup 不得把已经成功提取的完整 info 变成 crash”
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合稳定命中 full-info path + outer BIO cleanup
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - outer `BIO_free` cleanup 缺口发生在所有可观察输出都已经赋值之后
  - 因此最小正确语义是“保留已提取 metadata 与已解码 `SubjectAltNames`；cleanup helper 在执行中丢失不应导致 helper 崩溃或丢失这些字段”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 这批的 first RED attempt 暴露了两个重要约束：
  - 测试程序需要显式引入 `fafafa.ssl.openssl.base` 才能拿到 `PBIO`
  - 用 `BIO_new_mem_buf` stub 在首个成功调用后立刻置空 `BIO_free` 太早，会把同一调用里的更前路径也暴露在 helper-loss 下，不能稳定隔离到 outer cleanup
- 因此 contract test 最终改成了“晚期失效”模型：
  - 通过 `X509_free` stub 在 certificate cleanup 完成后再置空 `BIO_free`
  - 并在 direct `GetInfo(...)` 与 `TryGetInfo(...)` 两次调用前分别 re-arm stub，避免第一次调用留下的 nil helper 污染第二次入口
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 outer BIO cleanup `finally` 里为 `BIO_free` 增加 `Assigned(...)` guard
  - 通过 `X509_free` late-loss stub 在 certificate cleanup 之后禁用 `BIO_free`，让测试稳定命中真正的 outer cleanup gap
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `HasCertificatePEMReadBIOHelpers`、`VerifyChain(...)`、generation、conversion 或 fingerprint
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - direct `TCertificateUtils.GetInfo(...)` 会因为 outer `BIO_free(LBIO)` 的 cleanup nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空输出并返回 `False`
- fresh GREEN 说明本地 outer cleanup guard 就足够收口：
  - direct `TCertificateUtils.GetInfo(...)` 不再抛异常，并保留全部已提取 metadata 与 decoded `SubjectAltNames`
  - `TCertificateUtils.TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo X509_free symbol guard)
- 在收完 `GENERAL_NAMES_free` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct cleanup symbol 是：
  - `finally` 里的 `X509_free(LCert)`
  - 这时 subject/public-key metadata、`IsCA`、`KeyUsage` 与 `SubjectAltNames` 都已经填充完成
  - 因此这批 contract 不能再沿用 partial-info 语义，而必须保留已经解码好的 SAN entries 与所有已提取 metadata
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合稳定命中 full-info path + certificate cleanup
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_free` 缺口发生在所有可观察输出都已经赋值之后
  - 因此最小正确语义是“保留已提取 metadata 与已解码 `SubjectAltNames`；cleanup helper 缺失不应导致 helper 崩溃或丢失这些字段”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 certificate cleanup `finally` 里为 `X509_free` 增加 `Assigned(...)` guard
  - 缺 symbol 时直接跳过 cleanup call，不改已经填充好的 metadata 与 SAN 输出
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `BIO_free`、`VerifyChain(...)`、generation、conversion 或 fingerprint
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - 直接 `TCertificateUtils.GetInfo(...)` 会因为 `X509_free` 的 cleanup nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空输出并返回 `False`
- fresh GREEN 说明本地 cleanup guard 就足够收口：
  - 在 certificate cleanup `finally` 里直接 guard `X509_free`，就能保留全部已提取 metadata 与 SAN entries
  - `TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo GENERAL_NAMES_free symbol guard)
- 在收完 `GENERAL_NAME_get0_value` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct SAN cleanup symbol 是：
  - `finally` 里的 `GENERAL_NAMES_free(LExtNames)`
  - 这时 SAN loop 已经走完，subject/public-key metadata、`IsCA`、`KeyUsage` 与 `SubjectAltNames` 都已经填充完成
  - 因此这批 contract 不能再沿用“empty SAN list”语义，而必须保留已经解码好的 SAN entries
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合稳定命中 SAN decode + cleanup path
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `GENERAL_NAMES_free` 缺口发生在所有可观察输出都已经赋值之后
  - 因此最小正确语义是“保留已提取 metadata 与已解码 `SubjectAltNames`；cleanup helper 缺失不应导致 helper 崩溃或丢失这些字段”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 SAN cleanup `finally` 里为 `GENERAL_NAMES_free` 增加 `Assigned(...)` guard
  - 缺 symbol 时直接跳过 cleanup call，不改已经填充好的 metadata 与 SAN 输出
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `VerifyChain(...)`、generation、conversion 或 fingerprint
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - 直接 `TCertificateUtils.GetInfo(...)` 会因为 `GENERAL_NAMES_free` 的 cleanup nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空输出并返回 `False`
- fresh GREEN 说明本地 cleanup guard 就足够收口：
  - 在 SAN cleanup `finally` 里直接 guard `GENERAL_NAMES_free`，就能保留全部已提取 metadata 与 SAN entries
  - `TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo GENERAL_NAME_get0_value symbol guard)
- 在收完 `OPENSSL_sk_value` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct SAN symbol 是 general-name value helper：
  - `LGenName := OPENSSL_sk_value(LExtNames, I)` 现在已经有本地 guard
  - `LGenName <> nil` 后，下一条直接调用就是 `LVal := GENERAL_NAME_get0_value(LGenName, @LType)`
  - 这里没有本地 `Assigned(GENERAL_NAME_get0_value)` guard
  - `GENERAL_NAMES_free` 在 finally cleanup 里，应该留给后续独立批次
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合稳定命中 `GENERAL_NAME_get0_value`
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `GENERAL_NAME_get0_value` 缺口发生在 subject/public-key metadata、`IsCA`、`KeyUsage` 都已经成功赋值之后
  - 因此最小正确语义是“保留这些已提取字段；当 SAN general-name value helper 缺失时，`SubjectAltNames` 保持 allocated 但 empty”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `LGenName <> nil` 后、`GENERAL_NAME_get0_value(LGenName, @LType)` 前增加 `Assigned(GENERAL_NAME_get0_value)` guard
  - 缺 symbol 时直接跳过 SAN decoding，不改已提取字段，也不提前修改 cleanup path
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `GENERAL_NAMES_free`、`VerifyChain(...)`、generation、conversion 或 fingerprint
- fresh RED 进一步确认了这批 contract 的 failure shape：
  - 直接 `TCertificateUtils.GetInfo(...)` 会因为 `GENERAL_NAME_get0_value` 的 nil-call 抛出 `EAccessViolation`
  - `TCertificateUtils.TryGetInfo(...)` 会走异常兜底，清空输出并返回 `False`
- fresh GREEN 说明本地 guard 就足够收口：
  - 在 `LGenName <> nil` 后直接 guard `GENERAL_NAME_get0_value`，就能保留已提取 metadata
  - `SubjectAltNames` 继续保持 allocated 且 empty
  - `TryGetInfo(...)` 重新回到 non-throwing 且返回 `True`

## 2026-03-22 Findings (Certificate utils GetInfo OPENSSL_sk_value symbol guard)
- 在收完 `OPENSSL_sk_num` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct SAN symbol 是 stack-element helper：
  - `LCount := OPENSSL_sk_num(LExtNames)` 现在已经有本地 guard
  - 进入 `for I := 0 to LCount - 1 do` 后，第一条直接调用就是 `LGenName := OPENSSL_sk_value(LExtNames, I)`
  - 这里没有本地 `Assigned(OPENSSL_sk_value)` guard
  - `GENERAL_NAME_get0_value` 与 `GENERAL_NAMES_free` 都在它之后，应该留给后续独立批次
- 这批继续使用真正带 SAN extension 的证书：
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合稳定命中 `OPENSSL_sk_value`
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `OPENSSL_sk_value` 缺口发生在 subject/public-key metadata、`IsCA`、`KeyUsage` 都已经成功赋值之后
  - 因此最小正确语义是“保留这些已提取字段；当 SAN stack element helper 缺失时，`SubjectAltNames` 保持 allocated 但 empty”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 SAN loop 里、`OPENSSL_sk_value(LExtNames, I)` 前增加 `Assigned(OPENSSL_sk_value)` guard
  - 缺 symbol 时直接跳过 SAN traversal，不改已提取字段，也不提前修改后续 SAN helpers
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `GENERAL_NAME_get0_value`、`GENERAL_NAMES_free`、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo OPENSSL_sk_num symbol guard)
- 在收完 `EVP_PKEY_free` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct SAN symbol 是 stack-count helper：
  - SAN block 入口当前只检查了 `Assigned(X509_get_ext_d2i)` 和 `TOpenSSLLoader.IsModuleLoaded(osmStack)`
  - `LExtNames := POPENSSL_STACK(X509_get_ext_d2i(...))` 成功后，第一条直接调用就是 `LCount := OPENSSL_sk_num(LExtNames)`
  - 这里没有本地 `Assigned(OPENSSL_sk_num)` guard
  - `OPENSSL_sk_value`、`GENERAL_NAME_get0_value` 与 `GENERAL_NAMES_free` 都在它之后，应该留给后续独立批次
- 这批的 fixture 需要改成真正带 SAN extension 的证书：
  - `tests/certificate/test_certs/signer_cert.pem` 不带 SAN，不能命中 stack traversal
  - `tests/certs/san-test.pem` 带多个 SAN（DNS + IP），适合用来稳定命中 `OPENSSL_sk_num`
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `OPENSSL_sk_num` 缺口发生在 subject/public-key metadata、`IsCA` 与 `KeyUsage` 都已经成功赋值之后
  - 因此最小正确语义是“保留这些已提取字段；当 SAN stack count helper 缺失时，`SubjectAltNames` 保持 allocated 但 empty”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `LExtNames <> nil` 后、`OPENSSL_sk_num(LExtNames)` 前增加 `Assigned(OPENSSL_sk_num)` guard
  - 缺 symbol 时直接跳过 SAN traversal，不改已提取字段，也不提前修改后续 SAN helpers
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `OPENSSL_sk_value`、`GENERAL_NAME_get0_value`、`GENERAL_NAMES_free`、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo EVP_PKEY_free symbol guard)
- 在收完 `X509_get_pubkey` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 public-key cleanup：
  - `LPubKey := X509_get_pubkey(LCert)` 现在已经有本地 guard
  - 但成功拿到 `LPubKey` 后，`finally` 里仍然直接调用 `EVP_PKEY_free(LPubKey)`
  - 这里没有本地 `Assigned(EVP_PKEY_free)` guard
  - SAN 路径与更后的 metadata helpers 都在它之后，应该留给后续独立批次
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `EVP_PKEY_free` 缺口发生在 `Result.Subject`、`Result.Issuer`、`Result.Version`、`Result.NotBefore`、`Result.NotAfter`、`Result.SerialNumber`、`Result.SignatureAlgorithm`、`Result.PublicKeyType` 与 `Result.PublicKeyBits` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject / Issuer / Version / NotBefore / NotAfter / SerialNumber / SignatureAlgorithm / PublicKeyType / PublicKeyBits；cleanup helper 缺失不应导致 helper 崩溃或丢失这些字段”
  - cleanup helper 缺失本身并不要求 `GetInfo(...)` 截断后续 metadata 提取；这个批次只锁定“前面已经提取好的字段不能丢”和 “helper 不崩溃”，把更后字段继续留给后续 focused contract
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `LPubKey <> nil` cleanup 路径里为 `EVP_PKEY_free` 增加 `Assigned(...)` guard
  - 缺 symbol 时直接跳过 cleanup call，不改已经提取好的字段，也不额外改写后续 metadata flow
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_check_ca`、`X509_get_key_usage`、SAN helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_pubkey symbol guard)
- 在收完 `X509_get_serialNumber` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_pubkey`：
  - `SignatureAlgorithm` 提取完成后，下一条 metadata extraction 就是 `LPubKey := X509_get_pubkey(LCert)`
  - 这里没有本地 `Assigned(X509_get_pubkey)` guard
  - `X509_check_ca` / `X509_get_key_usage` 与更后的 metadata helpers 都在它之后，应该留给后续独立批次
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_get_pubkey` 缺口发生在 `Result.Subject`、`Result.Issuer`、`Result.Version`、`Result.NotBefore`、`Result.NotAfter`、`Result.SerialNumber` 与 `Result.SignatureAlgorithm` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject / Issuer / Version / NotBefore / NotAfter / SerialNumber / SignatureAlgorithm，PublicKey 与更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.SignatureAlgorithm` 提取完成后、`LPubKey := X509_get_pubkey(LCert)` 之前增加 `Assigned(X509_get_pubkey)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_check_ca`、`X509_get_key_usage`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_serialNumber symbol guard)
- 在收完 `X509_get_notAfter` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_serialNumber`：
  - `NotAfter` 提取成功后，下一条 metadata extraction 就是 `LSerialAsn1 := X509_get_serialNumber(LCert)`
  - 这里没有本地 `Assigned(X509_get_serialNumber)` guard
  - `X509_get_pubkey` 与更后的 metadata helpers 都在它之后，应该留给后续独立批次
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_get_serialNumber` 缺口发生在 `Result.Subject`、`Result.Issuer`、`Result.Version`、`Result.NotBefore` 与 `Result.NotAfter` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject / Issuer / Version / NotBefore / NotAfter，SerialNumber 与更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.SerialNumber := ''` 之后、`LSerialAsn1 := X509_get_serialNumber(LCert)` 之前增加 `Assigned(X509_get_serialNumber)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_get_pubkey`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_notAfter symbol guard)
- 在收完 `X509_get_notBefore` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_notAfter`：
  - `NotBefore` 提取成功后，下一条 metadata extraction 就是 `Result.NotAfter := ASN1TimeToDateTime(X509_get_notAfter(LCert))`
  - 这里没有本地 `Assigned(X509_get_notAfter)` guard
  - `X509_get_serialNumber` / `X509_get_pubkey` 都在它之后，应该留给后续独立批次
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_get_notAfter` 缺口发生在 `Result.Subject`、`Result.Issuer`、`Result.Version` 与 `Result.NotBefore` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject / Issuer / Version / NotBefore，NotAfter 与更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.NotBefore := ...` 之后、`Result.NotAfter := ...` 之前增加 `Assigned(X509_get_notAfter)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_get_serialNumber`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_notBefore symbol guard)
- 在收完 `X509_get_version` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_notBefore`：
  - `Version` 提取成功后，下一条 metadata extraction 就是 `Result.NotBefore := ASN1TimeToDateTime(X509_get_notBefore(LCert))`
  - 这里没有本地 `Assigned(X509_get_notBefore)` guard
  - `X509_get_notAfter` / `X509_get_serialNumber` / `X509_get_pubkey` 都在它之后，应该留给后续独立批次
- 这批继续沿用 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_get_notBefore` 缺口发生在 `Result.Subject`、`Result.Issuer`、`Result.Version` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject / Issuer / Version，NotBefore 与更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.Version := ...` 之后、`Result.NotBefore := ...` 之前增加 `Assigned(X509_get_notBefore)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_get_notAfter`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_version symbol guard)
- 在收完 `X509_get_issuer_name` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_version`：
  - `Issuer` 提取成功后，下一条 metadata extraction 就是 `Result.Version := X509_get_version(LCert) + 1`
  - 这里没有本地 `Assigned(X509_get_version)` guard
  - `X509_get_notBefore` / `X509_get_notAfter` / `X509_get_serialNumber` / `X509_get_pubkey` 都在它之后，应该留给后续独立批次
- 这批延续 `GetInfo(...)` 的 best-effort partial-info contract：
  - `X509_get_version` 缺口发生在 `Result.Subject` 和 `Result.Issuer` 都已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject 和 Issuer，Version 与更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.Issuer := ...` 之后、`Result.Version := ...` 之前增加 `Assigned(X509_get_version)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_get_notBefore`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_issuer_name symbol guard)
- 在收完 `X509_get_subject_name` 之后，`TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_issuer_name`：
  - `Subject` 提取成功后，下一条 metadata extraction 就是 `Result.Issuer := X509NameToString(X509_get_issuer_name(LCert))`
  - 这里没有本地 `Assigned(X509_get_issuer_name)` guard
  - `X509_get_version` / `X509_get_notBefore` / `X509_get_notAfter` / `X509_get_serialNumber` / `X509_get_pubkey` 都在它之后，应该留给后续独立批次
- 这批和 `X509_get_subject_name` 的 contract 细节不同，不能照搬“整份 empty info”假设：
  - `X509_get_issuer_name` 缺口发生在 `Result.Subject` 已经成功赋值之后
  - 因此最小正确语义是“保留已提取的 Subject，Issuer 和更后字段保持默认值”
  - `TryGetInfo(...)` 只要 `GetInfo(...)` 不再抛异常，就会继续返回 `True`
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 `Result.Subject := ...` 之后、`Result.Issuer := ...` 之前增加 `Assigned(X509_get_issuer_name)` guard
  - 缺 symbol 时直接返回当前已部分填充的 `Result`
  - 不改 `TryGetInfo(...)` 返回值语义，不提前修改 `X509_get_version`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-22 Findings (Certificate utils GetInfo X509_get_subject_name symbol guard)
- 先做了现状验证而不是盲修：
  - 现有 `tests/test_cert_utils_getinfo_bio_contract.pas` 已经在当前源码上通过
  - 说明旧的 GetInfo BIO batch 已经被 `HasCertificatePEMReadBIOHelpers` 入口 guard 收口，不应重复改动
- `TCertificateUtils.GetInfo(...)` 里下一条真正还裸露的 direct symbol 是 `X509_get_subject_name`：
  - PEM parse 成功后第一条 metadata extraction 就是 `Result.Subject := X509NameToString(X509_get_subject_name(LCert))`
  - 这里没有本地 `Assigned(X509_get_subject_name)` guard
  - `X509_get_issuer_name` / `X509_get_version` / `X509_get_notBefore` / `X509_get_notAfter` 都在它之后，应该留给后续独立批次
- 这条线的 public contract 与 generation/fingerprint 不同，必须单独处理：
  - `GetInfo(...)` 不是 exception-style helper
  - symbol unavailability 时当前正确方向是 silent-degrade 到 empty `TCertInfo`
  - `SubjectAltNames` 需要保持已分配，避免把“安全降级”变成调用方二次崩溃
  - `TryGetInfo(...)` 只需要继续 non-throwing 并输出 sanitized info，不需要在这批扩大返回值语义
- 最小正确修法只需要留在 `GetInfo(...)` 本地：
  - 在 PEM parse 成功后、第一条 metadata extraction 前增加 `Assigned(X509_get_subject_name)` guard
  - 缺 symbol 时直接返回当前已初始化的 `Result`
  - 不改 `X509_get_issuer_name`、其他 metadata helpers、`VerifyChain(...)`、generation、conversion 或 fingerprint

## 2026-03-21 Findings (Certificate utils GenerateSelfSigned EVP_sha256 symbol guard)
- 在收完 `SignCertificateWithKey(...)` 的 `X509_sign` 入口 guard 后，同一 helper 里还保留着下一条独立的 digest-helper gap：
  - helper 现在已经在任何签名入口前 guard 了 `X509_sign`
  - 但 RSA/ECDSA 分支仍然直接调用 `EVP_sha256()`
  - 这里没有本地 `Assigned(EVP_sha256)` guard
- 这批必须继续和刚收口的 `X509_sign` 分开：
  - `X509_sign` 是签名入口
  - `EVP_sha256` 只是传给该入口的 digest helper
  - 把两者混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.GenerateSelfSigned(...)` 是 exception-style helper
  - `EVP_sha256` 缺失时应继续抛出受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 self-signed generation helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `SignCertificateWithKey(...)` 本地：
  - 在调用 `EVP_sha256()` 之前增加 `Assigned(EVP_sha256)` guard
  - 对 RSA/ECDSA 分支，缺 helper 时直接返回 `False` 即可，让现有 public call sites 继续转成 `ESSLCertError`
  - 保留已有 nil-digest fallback，但只在 `EVP_sha256` 可用且首选签名调用失败时才继续尝试
  - 保持 `X509_sign` guard、Ed25519 path、PEM export path 与其他 helper families 不变

## 2026-03-21 Findings (Certificate utils GenerateSelfSigned X509_sign symbol guard)
- 在收完 conversion/fingerprint helper 之后，`cert.utils` 的 self-signed generation path 里还保留着一个更早的 signing-entry gap：
  - `GenerateSelfSigned(...)` 在构建完证书字段和扩展后，调用 `SignCertificateWithKey(LCert, LKey)`
  - `SignCertificateWithKey(...)` 会直接调用 `X509_sign`
  - 这里没有本地 `Assigned(X509_sign)` guard
- 这批必须先于同 helper 里的 `EVP_sha256`：
  - `X509_sign` 是签名入口
  - `EVP_sha256` 只是 RSA/ECDSA 分支里传给 `X509_sign` 的 digest helper
  - 把两者混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.GenerateSelfSigned(...)` 是 exception-style helper
  - `X509_sign` 缺失时应继续抛出受控 `ESSLCertError`
  - `TCertificateUtils.TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 self-signed generation helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `SignCertificateWithKey(...)` 本地：
  - 在任何 `X509_sign(...)` 调用之前增加 `Assigned(X509_sign)` guard
  - guard 直接返回 `False` 就够了，因为 `GenerateSelfSigned(...)` / `GenerateSigned(...)` 的 public call site 已经把 signing failure 转成 `ESSLCertError`
  - 保持 Ed25519 nil-digest path、RSA/ECDSA SHA-256 首选路径、fallback path 与 PEM export path 不变
  - 不改 `GenerateSigned(...)`、`EVP_sha256`、PEM export 或其他 helper families

## 2026-03-21 Findings (Certificate utils fingerprint EVP_sha256 symbol guard)
- 在收完 `TCertificateUtils.GetFingerprint(...)` 的 `X509_digest` guard 后，同一 helper 里还保留着下一条独立的 digest-algorithm gap：
  - helper 现在已经在 digest 入口前 guard 了 `X509_digest`
  - 但在真正调用 digest 之前，仍然直接调用 `EVP_sha256()`
  - 这里没有本地 `Assigned(EVP_sha256)` guard
- 这批必须继续和上一批 `X509_digest` 分开：
  - `X509_digest` 是 digest 入口
  - `EVP_sha256` 是传给 digest 的摘要算法 helper
  - 把两个符号混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.GetFingerprint(...)` 是 exception-style helper
  - `EVP_sha256` 缺失时应继续抛出受控 `ESSLCertError`
  - `TCertificateUtils.TryGetFingerprint(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 fingerprint helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `TCertificateUtils.GetFingerprint(...)` 本地：
  - 在调用 `EVP_sha256()` 之前增加 `Assigned(EVP_sha256)` guard
  - 保持已有 PEM/BIO read guard、`X509_digest` guard、证书 cleanup 与成功指纹路径不变
  - 不改 conversion helpers、certificate generation 或其他 helper families

## 2026-03-21 Findings (Certificate utils fingerprint X509_digest symbol guard)
- 在收完 `TCertificateUtils.PEMToDER(...)` / `DERToPEM(...)` 的单符号转换缺口后，同一 helper family 里 `TCertificateUtils.GetFingerprint(...)` 还保留着一个更窄的 digest-stage symbol gap：
  - helper 已经通过 `HasCertificatePEMReadBIOHelpers` guard 了 `BIO_new_mem_buf` / `PEM_read_bio_X509` / `BIO_free`
  - PEM parse 成功拿到 `PX509` 后，仍然直接调用 `X509_digest(LCert, EVP_sha256(), ...)`
  - 这里没有本地 `Assigned(X509_digest)` guard
- 这批必须继续和 `EVP_sha256` 分开：
  - `X509_digest` 是真正的 digest 入口
  - `EVP_sha256` 是同一调用里的摘要算法选择 helper
  - 把两个符号混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.GetFingerprint(...)` 是 exception-style helper
  - `X509_digest` 缺失时应继续抛出受控 `ESSLCertError`
  - `TCertificateUtils.TryGetFingerprint(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 fingerprint helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `TCertificateUtils.GetFingerprint(...)` 本地：
  - 在 digest 调用之前增加 `Assigned(X509_digest)` guard
  - 保持已有 PEM/BIO read guard、证书 cleanup 与成功指纹路径不变
  - 不改 `EVP_sha256`、conversion helpers、certificate generation 或其他 helper families

## 2026-03-21 Findings (Certificate utils DERToPEM d2i symbol guard)
- 在收完 `TCertificateUtils.PEMToDER(...)` 的 `i2d_X509` guard 后，同一 helper family 里 `TCertificateUtils.DERToPEM(...)` 还保留着一个更早的 decode-stage symbol gap：
  - helper 已经通过 `HasCertificatePEMWriteBIOHelpers` guard 了 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_X509` / `BIO_free`
  - 但在进入 PEM write path 之前，仍然直接调用 `d2i_X509`
  - 这里没有本地 `Assigned(d2i_X509)` guard
- 这批必须和上一批 `PEMToDER/i2d_X509` 分开：
  - `d2i_X509` 是 DER decode 入口
  - `PEM_write_bio_X509` / `BIO_get_mem_data` 只有在 decode 成功拿到 `PX509` 后才会触发
  - 把 decode 和 write-stage symbols 混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.DERToPEM(...)` 是 non-throwing helper
  - `d2i_X509` 缺失时应继续返回空字符串
  - `TCertificateUtils.TryDERToPEM(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 direct conversion helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `TCertificateUtils.DERToPEM(...)` 本地：
  - 在 DER decode 调用之前增加 `Assigned(d2i_X509)` 早退
  - 保持已有 PEM/BIO write guard、certificate cleanup 与成功转换路径不变
  - 不改 `PEMToDER(...)`、fingerprint、certificate generation 或其他 helper families

## 2026-03-21 Findings (Certificate utils PEMToDER i2d symbol guard)
- 在 `cert.utils` 的 conversion BIO guard 收口后，`TCertificateUtils.PEMToDER(...)` 里还保留着一个更窄的 encode-stage symbol gap：
  - helper 已经通过 `HasCertificatePEMReadBIOHelpers` guard 了 `BIO_new_mem_buf` / `PEM_read_bio_X509` / `BIO_free`
  - 但 PEM parse 成功拿到 `PX509` 后，仍然直接调用 `i2d_X509`
  - 这里没有本地 `Assigned(i2d_X509)` guard
- 这批必须和更大的 conversion/fingerprint 线分开：
  - 旧的 conversion BIO batch只覆盖了 PEM/BIO 入口依赖
  - `i2d_X509` 是 parse 成功后的 encode-stage symbol
  - 把它和 `DERToPEM(...)` 的 `d2i_X509`、`BIO_get_mem_data` 或 fingerprint 的 `X509_digest` 混批会让 public contract 与失败点交叉
- 这条线的正确 public contract 很清楚：
  - `TCertificateUtils.PEMToDER(...)` 是 non-throwing helper
  - `i2d_X509` 缺失时应继续返回 empty bytes
  - `TCertificateUtils.TryPEMToDER(...)` 应继续返回 `False` 并清空输出
  - helper loss 不应把 direct conversion helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `TCertificateUtils.PEMToDER(...)` 本地：
  - 在 PEM parse 成功之后、进入 DER encode 之前增加 `Assigned(i2d_X509)` 早退
  - 保持已有 PEM/BIO read guard、certificate cleanup 与成功转换路径不变
  - 不改 `DERToPEM(...)`、fingerprint、certificate generation 或其他 helper families

## 2026-03-21 Findings (PKCS12 i2d symbol guard)
- 在收完 `SavePKCS12ToFile(...)` 的 `PKCS12_create` guard 后，同一 helper 里还保留着下一阶段的独立 serialize symbol gap：
  - helper 现在已经 guard 了 `BIO_new_file` / `PKCS12_create` / `BIO_free`
  - `PKCS12_create` 成功返回 `P12` 后，helper 仍然直接调用 `i2d_PKCS12_bio`
  - 这里没有本地 `Assigned(i2d_PKCS12_bio)` guard
- 这批必须和上一批 `PKCS12_create` 分开：
  - create 阶段已经单独收口
  - `i2d_PKCS12_bio` 只有在成功拿到有效 `P12` 后才会触发
  - 把 create 与 serialize 混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `SavePKCS12ToFile(...)` 是 non-throwing helper
  - `i2d_PKCS12_bio` 缺失时应继续返回 `False`
  - helper loss 不应把 save helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `SavePKCS12ToFile(...)` 本地：
  - 增加 `Assigned(i2d_PKCS12_bio)` 早退
  - 保持已收口的 `PKCS12_create` guard、BIO guard 与 cleanup 路径不变
  - 不提前修改 PKCS12 load helper、PKCS7 或 CMS 行为

## 2026-03-21 Findings (PKCS12 create symbol guard)
- 在收完 `LoadPKCS12FromFile(...)` 的 `d2i_PKCS12_bio` / `PKCS12_parse` guards 后，同文件里 `SavePKCS12ToFile(...)` 还留着一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_file` / `BIO_free`
  - 但仍然直接调用 `PKCS12_create`
  - 没有本地 `Assigned(PKCS12_create)` guard
- 这批必须和后续 save-path symbols 分开：
  - `PKCS12_create` 是导出入口
  - `i2d_PKCS12_bio` 只有在成功拿到 `P12` 后才会触发
  - 把 create 与 serialize 混批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `SavePKCS12ToFile(...)` 是 non-throwing helper
  - `PKCS12_create` 缺失时应继续返回 `False`
  - helper loss 不应把 save helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `SavePKCS12ToFile(...)` 本地：
  - 增加 `Assigned(PKCS12_create)` 早退
  - 保持已有 BIO guard 与后续 serialize 路径不变
  - 不提前修改 `i2d_PKCS12_bio`、PKCS12 load helper、PKCS7 或 CMS 行为

## 2026-03-21 Findings (CMS decrypt symbol guard)
- 在收完 `CMSEncryptData(...)` 的 `CMS_encrypt` guard 后，同文件里 `CMSDecryptData(...)` 还留着另一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new` / `BIO_s_null` / `BIO_s_mem` / `BIO_free` / `BIO_read`
  - 但仍然直接调用 `CMS_decrypt`
  - 没有本地 `Assigned(CMS_decrypt)` guard
- 这批必须继续和其他 CMS helpers 分开：
  - sign / verify / encrypt 三条路径已经各自单独收口
  - decrypt helper 的 contract 是 empty bytes，而不是 `nil` 或 `False`
  - 把 decrypt 与其他 CMS symbols 混批会让 contract 与失败面交叉
- 这条线的正确 public contract 很清楚：
  - `CMSDecryptData(...)` 是 non-throwing helper
  - `CMS_decrypt` 缺失时应继续返回 empty bytes
  - helper loss 不应把 decrypt helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `CMSDecryptData(...)` 本地：
  - 增加 `Assigned(CMS_decrypt)` 早退
  - 保持已有 BIO guard、`BIO_read` 提取逻辑与成功解密路径不变
  - 不改 sign/verify/encrypt 行为

## 2026-03-21 Findings (CMS encrypt symbol guard)
- 在收完 `CMSVerifySignature(...)` 的 `CMS_verify` guard 后，同文件里 `CMSEncryptData(...)` 还留着另一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_mem_buf` / `BIO_free`
  - 测试通过显式传入非 nil cipher，避开默认 `EVP_aes_256_cbc()` 分支
  - 但 helper 仍然直接调用 `CMS_encrypt`
  - 没有本地 `Assigned(CMS_encrypt)` guard
- 这批必须继续和其他 CMS helpers 分开：
  - sign path 与 verify path 已经各自单独收口
  - `CMSDecryptData(...)` 依赖的是另一条 `CMS_decrypt` 解密路径
  - 把 encrypt 与 decrypt 混批会让 contract 与触发点交叉
- 这条线的正确 public contract 很清楚：
  - `CMSEncryptData(...)` 是 non-throwing helper
  - `CMS_encrypt` 缺失时应继续返回 `nil`
  - helper loss 不应把 encrypt helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `CMSEncryptData(...)` 本地：
  - 增加 `Assigned(CMS_encrypt)` 早退
  - 保持已有 BIO guard 与成功加密路径不变
  - 不改 sign/verify/decrypt 行为

## 2026-03-21 Findings (CMS verify symbol guard)
- 在收完 `CMSSignData(...)` 的 `CMS_sign` guard 后，同文件里 `CMSVerifySignature(...)` 还留着另一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_mem_buf` / `BIO_new` / `BIO_s_null` / `BIO_free`
  - 但仍然直接调用 `CMS_verify`
  - 没有本地 `Assigned(CMS_verify)` guard
- 这批必须继续和其他 CMS helpers 分开：
  - sign path 已经单独收口
  - `CMSEncryptData(...)` / `CMSDecryptData(...)` 依赖的是不同 symbols
  - 把 verify 和 encrypt/decrypt 混批会让 contract 与失败面交叉
- 这条线的正确 public contract 很清楚：
  - `CMSVerifySignature(...)` 是 non-throwing helper
  - `CMS_verify` 缺失时应继续返回 `False`
  - helper loss 不应把 verify helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `CMSVerifySignature(...)` 本地：
  - 增加 `Assigned(CMS_verify)` 早退
  - 保持已有 BIO guard 与成功验签路径不变
  - 不改 sign/encrypt/decrypt 行为

## 2026-03-21 Findings (CMS sign symbol guard)
- 从 PKCS family 切到 fresh CMS discovery 后，`src/fafafa.ssl.openssl.api.cms.pas` 里 `CMSSignData(...)` 还留着一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_mem_buf` / `BIO_free`
  - 但仍然直接调用 `CMS_sign`
  - 没有本地 `Assigned(CMS_sign)` guard
- 这批必须和同文件里的其他 CMS helpers 分开：
  - `CMSVerifySignature(...)` 是另一条 verify 语义
  - `CMSEncryptData(...)` / `CMSDecryptData(...)` 依赖的是不同 symbols
  - 把多个 CMS symbols 混批会让 public contract 与失败触发点交叉
- 这条线的正确 public contract 很清楚：
  - `CMSSignData(...)` 是 non-throwing helper
  - `CMS_sign` 缺失时应继续返回 `nil`
  - helper loss 不应把 sign helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `CMSSignData(...)` 本地：
  - 增加 `Assigned(CMS_sign)` 早退
  - 保持已有 BIO guard 与成功签名路径不变
  - 不改 verify/encrypt/decrypt 行为

## 2026-03-21 Findings (PKCS12 parse symbol guard)
- 在收完 `d2i_PKCS12_bio` 入口 guard 后，`LoadPKCS12FromFile(...)` 里还保留着下一阶段的独立 parse symbol gap：
  - `d2i_PKCS12_bio` 成功返回 `P12` 后，helper 直接调用 `PKCS12_parse`
  - 这里没有本地 `Assigned(PKCS12_parse)` guard
- 这批必须和上一批 `d2i_PKCS12_bio` 分开：
  - decode 阶段已经单独收口
  - `PKCS12_parse` 只有在有效 `P12` 场景下才会触发
  - 把 decode 与 parse 混批会让失败触发点不再最小
- 这条线的正确 public contract 仍然很清楚：
  - `LoadPKCS12FromFile(...)` 是 non-throwing helper
  - `PKCS12_parse` 缺失时应继续返回 `False`
  - `AKey` / `ACert` / `ACAs` 应保持 `nil`
  - helper loss 不应把 parse-stage helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `LoadPKCS12FromFile(...)` 本地：
  - 增加 `Assigned(PKCS12_parse)` 早退
  - 保持已收口的 `d2i_PKCS12_bio` guard、file/BIO guard 与成功 parse 路径不变
  - 不改 PKCS7 或 CMS 行为

## 2026-03-21 Findings (PKCS12 d2i symbol guard)
- 在收完 PKCS7 sign/verify 两条线后，同文件 `LoadPKCS12FromFile(...)` 还留着更早的一处独立 load-path symbol gap：
  - helper 已经 guard 了 `BIO_new_file` / `BIO_free`
  - 但仍然直接调用 `d2i_PKCS12_bio`
  - 没有本地 `Assigned(d2i_PKCS12_bio)` guard
- 这批必须和 `PKCS12_parse` 分开：
  - `d2i_PKCS12_bio` 是更早的 decode 入口
  - `PKCS12_parse` 只有在 decode 成功拿到 `P12` 后才会触发
  - 把 decode 和 parse 混在一批会让失败触发点不再最小
- 这条线的正确 public contract 很清楚：
  - `LoadPKCS12FromFile(...)` 是 non-throwing helper
  - `d2i_PKCS12_bio` 缺失时应继续返回 `False`
  - `AKey` / `ACert` / `ACAs` 应保持 `nil`
  - helper loss 不应把 load helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `LoadPKCS12FromFile(...)` 本地：
  - 增加 `Assigned(d2i_PKCS12_bio)` 早退
  - 保持已有 file/BIO guard 和后续 parse 路径不变
  - 不提前修改 `PKCS12_parse`、PKCS7 或 CMS 行为

## 2026-03-21 Findings (PKCS7 verify symbol guard)
- 在收完 `CreatePKCS7SignedData(...)` 的 `PKCS7_sign` guard 后，同文件里 `VerifyPKCS7SignedData(...)` 还留着另一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_mem_buf` / `BIO_new` / `BIO_s_null` / `BIO_free`
  - 但仍然直接调用 `PKCS7_verify`
  - 没有本地 `Assigned(PKCS7_verify)` guard
- 这批必须继续和其他 helper 分开：
  - sign path 已经单独收口
  - `LoadPKCS12FromFile(...)` 依赖的是 `d2i_PKCS12_bio` / `PKCS12_parse`
  - 把 verify 和 PKCS12 混批会让 contract 与失败面交叉
- 这条线的正确 public contract 很清楚：
  - `VerifyPKCS7SignedData(...)` 是 non-throwing helper
  - `PKCS7_verify` 缺失时应继续返回 `False`
  - helper loss 不应把 verify helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `VerifyPKCS7SignedData(...)` 本地：
  - 增加 `Assigned(PKCS7_verify)` 早退
  - 保持已有 BIO guard 与成功验签路径不变
  - 不改 sign、PKCS12 或 CMS 行为

## 2026-03-21 Findings (PKCS7 sign symbol guard)
- 从 PEM family 切到 fresh PKCS discovery 后，`src/fafafa.ssl.openssl.api.pkcs.pas` 里 `CreatePKCS7SignedData(...)` 还留着一处独立的 local symbol gap：
  - helper 已经 guard 了 `BIO_new_mem_buf` / `BIO_free`
  - 但仍然直接调用 `PKCS7_sign`
  - 没有本地 `Assigned(PKCS7_sign)` guard
- 这批必须和同文件里的其他 helper 分开：
  - `VerifyPKCS7SignedData(...)` 是另一条 verify 语义
  - `LoadPKCS12FromFile(...)` 涉及 `d2i_PKCS12_bio` / `PKCS12_parse`
  - 把多个 symbols 混批会让失败触发点和 public contract 变脏
- 这条线的正确 public contract 很清楚：
  - `CreatePKCS7SignedData(...)` 是 non-throwing helper
  - `PKCS7_sign` 缺失时应继续返回 `nil`
  - helper loss 不应把 sign helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `CreatePKCS7SignedData(...)` 本地：
  - 增加 `Assigned(PKCS7_sign)` 早退
  - 保持已有 BIO guard 和成功签名路径不变
  - 不改 verify、PKCS12 或 CMS 行为

## 2026-03-21 Findings (PEM encrypted privatekey cipher symbol guard)
- 在收完 PEM key-save write symbols 后，`SavePrivateKeyToPEM(...)` 的 password branch 还保留一个更窄的 local cipher gap：
  - `APassword <> ''` 时直接调用 `EVP_aes_256_cbc()`
  - 这里没有本地 `Assigned(EVP_aes_256_cbc)` guard
- 这批必须和前一批 key-save write symbols 分开：
  - 前一批锁的是 `PEM_write_bio_PrivateKey` / `PEM_write_bio_PUBKEY`
  - 这一批锁的是 encrypted private-key branch 选择 cipher 时的 EVP helper
  - 两者的最小修法位置和失败触发点不同
- 这条线的正确 public contract 仍然很清楚：
  - encrypted `SavePrivateKeyToPEM(..., 'password')` 是 non-throwing helper
  - helper 缺失时应继续返回 `False`
  - 不应把 helper loss 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `SavePrivateKeyToPEM(...)` 的 password branch：
  - `APassword <> ''` 时 require `Assigned(EVP_aes_256_cbc)`
  - helper 缺失则直接保留 `False`
  - 不改 unencrypted branch 或 broader EVP loading semantics

## 2026-03-21 Findings (PEM key save symbol guard)
- 在收完 PEM certificate helpers 和 key-read helpers 之后，`fafafa.ssl.openssl.api.pem` 里还剩一组独立的 key-save symbol gap：
  - `SavePrivateKeyToPEM(...)` 直接调用 `PEM_write_bio_PrivateKey`
  - `SavePublicKeyToPEM(...)` 直接调用 `PEM_write_bio_PUBKEY`
  - 两处都只 guard 了 `BIO_new_file` / `BIO_free`，没有本地 guard 对应的 PEM write symbol
- 这批必须继续和 password branch 分开：
  - `SavePrivateKeyToPEM(...)` 在 `APassword <> ''` 时还会依赖 `EVP_aes_256_cbc()`
  - 但 unencrypted save path 的崩溃面已经足够形成一个更窄、更干净的批次
- 这条线的正确 public contract 也很明确：
  - `SavePrivateKeyToPEM(..., '')` helper 缺失时应继续返回 `False`
  - `SavePublicKeyToPEM(...)` helper 缺失时应继续返回 `False`
  - helper loss 不应把 non-throwing save helper 放大成 raw `EAccessViolation`
- 最小正确修法只需要留在 `fafafa.ssl.openssl.api.pem` 本地：
  - private-key unencrypted save helper requires `Assigned(PEM_write_bio_PrivateKey)`
  - public-key save helper requires `Assigned(PEM_write_bio_PUBKEY)`
  - 不改 key-read helpers、certificate helpers 或 encrypted branch semantics

## 2026-03-21 Findings (PEM key read symbol guard)
- 在修完 PEM certificate-only symbol gap 后，同一个模块里 key read helpers 仍保留独立的 local symbol gap：
  - `LoadPrivateKeyFromPEM(...)` 直接调用 `PEM_read_bio_PrivateKey`
  - `LoadPrivateKeyFromMemory(...)` 直接调用 `PEM_read_bio_PrivateKey`
  - `LoadPublicKeyFromPEM(...)` 直接调用 `PEM_read_bio_PUBKEY`
  - 三处都只 guard 了 `BIO_*`，没有本地 guard 对应的 PEM key read symbol
- 这批应该和 PEM certificate batch 分开：
  - certificate helpers 已经单独收口
  - key save helpers 是另一组 symbols，仍应另拆批次
- 这条线的正确 public contract 也很清楚：
  - 三个 key read helper 都是 non-throwing entrypoint
  - helper 缺失时应继续返回 `nil`
  - 不应把 helper loss 放大成 raw `EAccessViolation`
- 最小正确修法仍然只需要留在 `fafafa.ssl.openssl.api.pem` 本地：
  - private-key file/memory helpers require `Assigned(PEM_read_bio_PrivateKey)`
  - public-key file helper requires `Assigned(PEM_read_bio_PUBKEY)`
  - 不改 certificate helpers、save helpers 或 broader loader logic

## 2026-03-21 Findings (PEM certificate symbol guard)
- fresh discovery 证明旧的 `pem_helper_bio_contract` 只能说明 BIO constructor/cleanup 侧已经收口：
  - `tests/test_pem_helper_bio_contract.pas` 已经是绿的
  - 但这不代表 PEM symbol-level helper loss 已经安全
- `src/fafafa.ssl.openssl.api.pem.pas` 的 certificate-only helper 还留着一个独立的 symbol gap：
  - `LoadCertificateFromPEM(...)` 直接调用 `PEM_read_bio_X509`
  - `LoadCertificateFromMemory(...)` 直接调用 `PEM_read_bio_X509`
  - `SaveCertificateToPEM(...)` 直接调用 `PEM_write_bio_X509`
  - 三处都只 guard 了 `BIO_*`，没有本地 guard 对应的 PEM certificate symbol
- 这批应该和更大的 PEM helper family 分开：
  - public/private key helpers 是另一组 symbols
  - 当前最窄、最清晰的 public contract 只落在 certificate-only helpers
- 这条线的正确 public contract 很明确：
  - `LoadCertificateFromPEM(...)` / `LoadCertificateFromMemory(...)` 缺 helper 时应继续返回 `nil`
  - `SaveCertificateToPEM(...)` 缺 helper 时应继续返回 `False`
  - helper loss 不应把 non-throwing helper 放大成 raw `EAccessViolation`
- 最小正确修法必须留在 `fafafa.ssl.openssl.api.pem` 本地：
  - certificate read helpers require `Assigned(PEM_read_bio_X509)`
  - certificate save helper requires `Assigned(PEM_write_bio_X509)`
  - 不改 key helpers、BIO guards 或 loader semantics

## 2026-03-21 Findings (Post-destroy backlog discovery scan)
- destructor `SSL_free` batch收口后，旧 backlog 里的很多 focused contracts 已经不再提供新的 red surface：
  - `openssl.connection` 抽查通过：
    - `GetConnectionInfo`
    - `IsSessionReused`
    - `GetPeerCertificateChain`
    - `GetProtocolVersion`
    - OCSP storectx issuer ownership 两条 contract
  - broader BIO helper families 抽查通过：
    - `openssl.context`
    - `openssl.session`
    - `openssl.certificate`
    - `cert.builder`
    - `cert.utils.GenerateSigned`
    - `cert.utils.VerifyChain`
- 这意味着现有未入进度记录的 plan/test backlog 里，很多条目已经被先前修复顺带打绿：
  - 再继续按旧计划名逐个执行的收益很低
  - 下一批更应该从 fresh discovery 出发，而不是把“已有绿测试”误当成待修缺口
- 快速本地门禁也支持这个判断：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` 通过
  - compile-all `181/181`
  - module tests `17/17`
- 当前高置信结论：
  - `openssl.connection` 这一波窄 helper guard surface 已经显著收敛
  - 下一阶段需要新的高置信 red signal 来决定具体批次

## 2026-03-21 Findings (OpenSSL connection destroy SSL_free guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `TOpenSSLConnection.Destroy` 里还留着一处独立 cleanup helper gap：
  - `if FSSL <> nil then SSL_free(FSSL);`
  - 这里没有本地 `Assigned(SSL_free)` guard
- 这批不该和 constructor cleanup 的 `SSL_free` 批次混在一起：
  - 那一批发生在 stream constructor partial-init `finally`
  - 这一批发生在对象正常析构 release path
  - 两者的 public contract 不同
- 这条线的正确 contract 也不是 function-not-found 语义：
  - 析构没有返回值，也没有 wrapper 帮你重写异常
  - helper 缺失时，用户直接看到 raw `EAccessViolation`
  - 所以真正要锁住的是 release path 的 no-raise contract
- 为了把批次保持最窄，socket-style constructor 是更好的夹具：
  - 只依赖 `SSL_new` / `SSL_set_fd`
  - 不需要把 BIO setup、stream ownership 或 handshake 引进来
- 最小正确修法必须留在 destructor 本地：
  - 仅在 `Assigned(SSL_free)` 时调用 `SSL_free(FSSL)`
  - 无论 helper 是否存在，都把 `FSSL := nil`
  - 保留已连接实例先走 `DoShutdown` 的现有 cleanup 顺序

## 2026-03-21 Findings (OpenSSL connection stream cleanup SSL_free guard)
- 在修完 stream constructor 的 `SSL_new` 入口后，stream partial-init cleanup 里还剩一处独立 helper gap：
  - 第二个 `BIO_new(BIO_s_mem())` 失败后会进入 `finally`
  - 当前 cleanup 分支直接调用 `SSL_free(FSSL)`
  - 这里没有本地 `Assigned(SSL_free)` guard
- 这条线不能和刚修完的 `SSL_new` 批次混在一起：
  - `SSL_new` 缺口发生在 constructor 入口
  - `SSL_free` 缺口发生在 partial-init cleanup
  - 它们的正确 public contract 也不同
- 这批真正要锁住的是“保留原始异常”，不是 function-not-found 语义：
  - 第二个 BIO 分配失败本来就会 `RaiseMemoryError('create write BIO')`
  - 这个异常是受控 `ESSLOutOfMemoryException`
  - helper `SSL_free` 缺失不应把它冲成 wrapped `ESSLConnectionException: ... Access violation`
- public wrapper 行为再次证明问题出在 constructor cleanup 本地：
  - `TOpenSSLContext.CreateConnection(AStream)` 对 generic `Exception` 会 wrap 成 `ESSLConnectionException`
  - 所以一旦 cleanup nil-call 发生，用户看到的就不再是原始内存错误
  - 但只要 cleanup 本地不再 nil-call，原始 `ESSLOutOfMemoryException` 就会被 wrapper 按 `ESSLException` 原样 re-raise
- 最小正确修法也必须留在 stream constructor cleanup 分支：
  - `if FSSL <> nil then` 分支里只在 `Assigned(SSL_free)` 时调用 `SSL_free`
  - 无论 helper 是否存在，都把 `FSSL := nil`
  - 保留现有的 `BIO_free` cleanup guard、第二个 BIO 分配失败语义，以及成功构造路径

## 2026-03-21 Findings (OpenSSL connection stream constructor SSL_new guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 stream constructor 在进入 BIO setup 之前仍有一处 direct helper gap：
  - `SSL_new(Ctx)`
  - 这一点和刚修完的 socket constructor 是同型问题，只是 public 入口换成了 `TOpenSSLContext.CreateConnection(AStream)`
- 这批和旧的 stream BIO cleanup batch 不是同一个问题：
- 之前的 `tests/test_openssl_connection_stream_bio_contract.pas` 锁的是 partial-init cleanup 时 `BIO_free` 缺失
- 这一批命中的更早，发生在 `FSSL` 尚未构造前
- 所以最小修法不应该混进 `SSL_free` cleanup 或 BIO failure path
- public surface 的真实问题也和 socket constructor 一样，不是“会不会 raise”，而是 raise 的语义不够精确：
- helper 缺失会先触发 constructor 本地 nil-call
- 再被 `TOpenSSLContext.CreateConnection(AStream)` 包成 `ESSLConnectionException`
- error code 退化成 `sslErrConnection`
- message 退化成 `Failed to create SSL connection: Access violation`
- 因此这批必须把 contract 收紧到：
- raise 受控 `ESSLException`
- error code 为 `sslErrFunctionNotFound`
- message 明确指向 `SSL_new`
- 不再暴露 raw `Access violation`
- 最小正确修法仍然只能留在 stream constructor 本地：
- 在 `SSL_new(Ctx)` 前要求 `Assigned(SSL_new)`
- 直接使用现有 `RaiseFunctionNotAvailable('SSL_new')`
- 保留 helper 存在但 allocation 失败时现有的 `RaiseSSLInitError(...)`
- 不改 BIO setup、partial cleanup 或 context wrapper

## 2026-03-21 Findings (OpenSSL connection socket constructor guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 socket constructor 仍保留两处 direct helper gap：
  - `SSL_new(Ctx)`
  - `SSL_set_fd(FSSL, ASocket)`
  - 两者都没有本地 `Assigned(...)` guard
- 这批和前几批 read/write/connect/accept 的关键差异在 public surface：
  - `TOpenSSLContext.CreateConnection(...)` 已经会 catch generic `Exception`
  - 因此 helper 缺失不会直接冒成 public `EAccessViolation`
  - 但它会把 constructor 里的 nil-call 包成 `ESSLConnectionException`
  - error code 变成 `sslErrConnection`
  - message 退化成 `Failed to create SSL connection: Access violation`
- 所以这批真正要锁住的 contract 不是“有没有 raise”，而是更精确的 public error semantics：
  - 仍然 raise
  - 但必须是 constructor 本地抛出的受控 `ESSLException`
  - error code 必须是 `sslErrFunctionNotFound`
  - message 必须指向缺失 helper，而不是 raw `Access violation`
- 最小正确修法必须留在 socket constructor 本地：
  - 在 `SSL_new(Ctx)` 前要求 `Assigned(SSL_new)`
  - 在 `SSL_set_fd(FSSL, ASocket)` 前要求 `Assigned(SSL_set_fd)`
  - 用现有 `RaiseFunctionNotAvailable(...)` 直接建立精确异常
  - 保留 helper 存在但 `SSL_new` 返回 `nil` 时的 `RaiseSSLInitError(...)` 行为
  - 不改 stream constructor、context wrapper 或 broader constructor semantics

## 2026-03-21 Findings (OpenSSL connection stream write guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoWrite(...)` 在完成 socket-side 与 stream-side `Read` 收口后，stream transport 分支仍保留同型 helper gap：
  - 进入 `HasStreamTransport` 分支并跳过握手后，直接调用 `SSL_write(FSSL, @ABuffer, ACount)`
  - 失败路径又直接调用 `SSL_get_error(FSSL, LRet)`
  - 两处都没有本地 `Assigned(...)` guard
- 这批的 public contract 仍然很清楚：
  - direct `Write` 是整数返回 API
  - stream-side 失败时当前实现同样退化到 `-1`
  - helper 缺失不应把这个 contract 放大成 `EAccessViolation`
- 这条线和 socket `Write` / stream `Read` 一样，需要一个很窄的入口夹具：
  - 用 `TMemoryStream` 构造 stream-based 连接
  - 再强制 `FConnected := True` / `FHandshakeComplete := True`
  - 这样 public `Write` 才会稳定命中 stream-side `DoWrite(...)`，而不是先回到 handshake path
- focused contract 证明了两个 stream-side 崩溃面都真实存在：
  - `SSL_write := nil` 时，stream-based `Write` 直接 AV
  - `SSL_write` 返回失败且 `SSL_get_error := nil` 时，stream-based `Write` 也会在 failure path AV
- 最小正确修法必须留在 `DoWrite(...)` 的 stream 分支本地：
  - 进入循环前要求 `Assigned(SSL_write)`
  - 失败路径在 `SSL_get_error` 不可用时直接返回 `-1`
  - 保留现有的 handshake gate、`WANT_READ` / `WANT_WRITE` BIO pump loop 与整数退化 contract

## 2026-03-21 Findings (OpenSSL connection stream read guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoRead(...)` 在完成 socket-side 收口后，stream transport 分支仍保留同型 helper gap：
  - 进入 `HasStreamTransport` 分支并跳过握手后，直接调用 `SSL_read(FSSL, @ABuffer, ACount)`
  - 失败路径又直接调用 `SSL_get_error(FSSL, LRet)`
  - 两处都没有本地 `Assigned(...)` guard
- 这批的 public contract 仍然很清楚：
  - direct `Read` 是整数返回 API
  - stream-side 失败时当前实现同样退化到 `-1`
  - helper 缺失不应把这个 contract 放大成 `EAccessViolation`
- 这条线和前一批 socket `Read` 的关键差异是入口夹具：
  - 需要用 `TMemoryStream` 构造 stream-based 连接
  - 但又要强制 `FConnected := True` / `FHandshakeComplete := True`
  - 这样 public `Read` 才会稳定命中 stream-side `DoRead(...)`，而不是先被 `InternalHandshake(...)` 抢走
- focused contract 证明了两个 stream-side 崩溃面都真实存在：
  - `SSL_read := nil` 时，stream-based `Read` 直接 AV
  - `SSL_read` 返回失败且 `SSL_get_error := nil` 时，stream-based `Read` 也会在 failure path AV
- 最小正确修法必须留在 `DoRead(...)` 的 stream 分支本地：
  - 进入循环前要求 `Assigned(SSL_read)`
  - 失败路径在 `SSL_get_error` 不可用时直接返回 `-1`
  - 保留现有的 handshake gate、`WANT_READ` / `WANT_WRITE` BIO pump loop 与整数退化 contract

## 2026-03-21 Findings (OpenSSL connection write guard)
- `src/fafafa.ssl.openssl.connection.pas` 里的 `DoWrite(...)` 在非 stream 路径上保留了和 `DoRead(...)` 同型的 direct helper gap：
  - 连接已建立后直接调用 `SSL_write(FSSL, @ABuffer, ACount)`
  - 失败路径直接调用 `SSL_get_error(FSSL, Result)`
  - 两处都没有本地 `Assigned(...)` guard
- 这批的 public contract 很清楚：
  - socket-style `Write` 是整数返回 API
  - 当前失败约定本来就是返回 `-1`
  - helper 缺失不应把这个 contract 放大成 `EAccessViolation`
- focused contract 需要绕开 stream handshake/BIO pump，最稳的夹具是：
  - 先 warm up socket-style constructor
  - 用一个薄测试子类把 `FConnected` / `FHandshakeComplete` 强制设为 `True`
  - 这样 public `Write` 会直接命中 socket-style `DoWrite(...)`
- regression 证明了两个独立崩溃面都真实存在：
  - `SSL_write := nil` 时，`Write` 直接 AV
  - `SSL_write` 返回失败且 `SSL_get_error := nil` 时，`Write` 也会在 failure path AV
- 最小正确修法必须留在 `DoWrite(...)` 本地：
  - 进入非 stream path 前要求 `Assigned(SSL_write)`
  - 失败分支只在 `Assigned(SSL_get_error)` 时记录 `FLastSSLError`
  - 保留 stream-based write loop、BIO pump 与现有 `-1` 退化 contract

## 2026-03-21 Findings (OpenSSL connection read guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoRead(...)` 在非 stream 路径上也保留了一个很窄的 direct helper gap：
  - 连接已建立后直接调用 `SSL_read(FSSL, @ABuffer, ACount)`
  - 读取失败时直接调用 `SSL_get_error(FSSL, Result)`
  - 两处都缺少本地 `Assigned(...)` guard
- 这批的 public contract 同样明确：
  - socket-style `Read` 是整数返回 API
  - 当前失败约定就是返回 `-1`
  - helper 缺失不应把原本的 safe degrade 变成 `EAccessViolation`
- 为了把问题精确锁在 socket-style `DoRead(...)`，测试必须避开 stream path：
  - 先 warm up socket-style constructor
  - 再用薄测试子类把连接强制置为 connected / handshake complete
  - 这样 public `Read` 不会被 `InternalHandshake(...)` 或 BIO loop 抢先拦截
- focused contract 证明了两个失败模式都真实存在：
  - `SSL_read := nil` 时，`Read` 直接 AV
  - `SSL_read` 返回失败且 `SSL_get_error := nil` 时，`Read` 也会在 failure path AV
- 最小正确修法也必须落在 `DoRead(...)` 本地：
  - 进入非 stream path 前要求 `Assigned(SSL_read)`
  - 失败分支只在 `Assigned(SSL_get_error)` 时记录 `FLastSSLError`
  - 保留 stream-based read loop 与现有 `-1` 退化 contract

## 2026-03-21 Findings (OpenSSL connection accept guard)
- `src/fafafa.ssl.openssl.connection.pas` 里的 `DoAccept(...)` 与刚完成的 `DoConnect(...)` 是几乎完全镜像的 helper gap：
  - 非 stream 路径直接调用 `SSL_accept(FSSL)`
  - 握手失败时直接调用 `SSL_get_error(FSSL, Ret)`
  - 两者都缺少本地 `Assigned(...)` guard
- 这批的 public contract 同样很明确：
  - `Accept` 是布尔型 server-side 入口
  - socket-style 握手失败时，本来就通过 `Result := FConnected` 退化为 `False`
  - helper 缺失不应该放大成 `EAccessViolation`
- 这里不能依赖前一批 `DoConnect(...)` 的修法自动覆盖：
  - `DoConnect(...)` 和 `DoAccept(...)` 各自单独解引用不同的 OpenSSL handshake entry point
  - stream 路径虽然都走 `InternalHandshake(...)`，但 socket-style server path 仍然独立停在 `DoAccept(...)`
  - 所以必须单独写 focused regression，并把 guard 放在 `DoAccept(...)` 本地
- focused contract 证明了两种 server-side 失败模式都真实存在：
  - `SSL_accept := nil` 时，`Accept` 直接 AV
  - `SSL_accept` 返回失败且 `SSL_get_error := nil` 时，`Accept` 也会在 failure path AV
- 最小正确修法与 `Connect` 同型：
  - 进入 socket-style `DoAccept(...)` 前要求 `Assigned(SSL_accept)`
  - 失败分支只在 `Assigned(SSL_get_error)` 时记录 `FLastSSLError`
  - 这样保留了成功握手后继续执行 `ValidatePostHandshake(False)` 的行为，也恢复了既有的 `False` 退化 contract

## 2026-03-21 Findings (OpenSSL connection connect guard)
- 在完成 `ValidatePostHandshake(...)` / `DoIsOCSPResponseVerified(...)` 的 OCSP ownership 收口后，`src/fafafa.ssl.openssl.connection.pas` 里下一处最窄的 public nil-call 面回到了 socket-style `Connect`：
  - `DoConnect(...)` 在非 stream 路径上直接调用 `SSL_connect(FSSL)`
  - 当握手失败时，又直接调用 `SSL_get_error(FSSL, Ret)`
  - 两个 helper 都没有本地 `Assigned(...)` guard
- 这批的 public contract 很清楚：
  - `Connect` 是布尔型入口
  - socket-style 握手失败时，本来就通过 `Result := FConnected` 退化为 `False`
  - helper 缺失不应把这个失败 contract 放大成 `EAccessViolation`
- 这条线和之前的 stream-handshake 批次相邻但不是同一个问题：
  - stream 路径已经在 `InternalHandshake(...)` 收口了 `SSL_do_handshake` / `SSL_get_error`
  - 当前缺口只存在于 socket-style `DoConnect(...)`
  - 所以最小正确修法必须留在 `DoConnect(...)` 本地，不能去改更高层 `Connect` 包装或共享错误映射
- focused contract 证明了两种独立失败模式都存在：
  - `SSL_connect := nil` 时，`Connect` 直接 AV
  - `SSL_connect` 返回失败且 `SSL_get_error := nil` 时，`Connect` 也会在 failure path AV
- 最小正确修法也很窄：
  - 进入 socket-style `DoConnect(...)` 前要求 `Assigned(SSL_connect)`
  - 失败分支只在 `Assigned(SSL_get_error)` 时记录 `FLastSSLError`
  - 这样既保留了成功握手后继续执行 `ValidatePostHandshake(True)` 的行为，也恢复了既有的 `False` 退化 contract

## 2026-03-21 Findings (OpenSSL connection post-handshake OCSP storectx issuer ownership guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `ValidatePostHandshake(...)` 暴露的是和上一批 `DoIsOCSPResponseVerified(...)` 同型、但 sink 不同的生命周期错误：
  - issuer 先尝试从 peer chain / handshake verified chain 找
  - 找不到才退到临时 `X509_STORE_CTX` 的 `X509_STORE_CTX_get0_chain(...)`
  - 在这个 fallback 分支里，`FindIssuerX509InChain(...)` 取到的 issuer 只受 `StoreCtx` 生命周期保护
  - 但当前实现只有在 `Assigned(X509_up_ref)` 时才接管所有权；如果 `X509_up_ref` 缺失，它仍会把 borrowed issuer 带到后面的 `CheckCertificateStatus(...)`
- 这条线和上一批的关键差异不是来源，而是下游消费点：
  - `DoIsOCSPResponseVerified(...)` 的 borrowed issuer 会流向 `VerifyOCSPResponse(...)`
  - `ValidatePostHandshake(...)` 的 borrowed issuer 会流向 `CheckCertificateStatus(...)`
  - 两者都必须在 storectx 作用域内收口，不能让 borrowed handle 越过 `X509_STORE_CTX_free`
- 这批最小正确修法也不应该碰 shared OCSP helper：
  - 如果 storectx fallback 找到了 issuer，但没有 `X509_up_ref`
  - 直接把 `IssuerX509` 清成 `nil`
  - 让现有 `IssuerX509=nil` 的 fail-closed 路径继续设置 `X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT` 并返回 `False`
- 这批的 contract test 还暴露出一个执行层面事实：
  - 为了稳定打中 ownership 边界，最好把非目标 request/nonce helper 一并 stub 掉，并显式加载 stack helpers
  - 否则很容易先被环境上的 `OPENSSL_sk_*` / typed stack wrapper 缺口拦成 capability skip，而不是命中真正的 post-handshake bug

## 2026-03-21 Findings (OpenSSL connection OCSP storectx issuer ownership guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoIsOCSPResponseVerified(...)` 这次暴露的不是单纯 nil-call，而是更隐蔽的生命周期错误：
  - 当 issuer 无法从 peer chain / handshake verified chain 获得时，代码会回退到临时 `X509_STORE_CTX` 的 `X509_STORE_CTX_get0_chain(...)`
  - `FindIssuerX509InChain(...)` 在这里拿到的是被 `StoreCtx` 拥有的 borrowed `PX509`
  - 当前实现只有在 `Assigned(X509_up_ref)` 时才接管所有权，但即使 `X509_up_ref` 缺失，也仍会把 `IssuerX509` 留给后续 `VerifyOCSPResponse(...)`
  - 一旦 `X509_STORE_CTX_free(StoreCtx)` 先执行，这个 borrowed issuer 就已经越过了自己的生命周期边界
- 这里不能沿用 peer-chain / handshake verified-chain 的思路：
  - `SSL_get_peer_cert_chain` / `SSL_get0_verified_chain` 返回的对象寿命至少绑定在 `FSSL`
  - `X509_STORE_CTX_get0_chain(...)` 返回的对象寿命只绑定在临时 `StoreCtx`
  - 所以只有 storectx fallback 路径必须强制要求 `X509_up_ref`，否则就必须 fail closed
- 最小正确修法不是“继续带着 nil issuer 去试一遍”，也不是放宽验证，而是在 storectx 作用域内直接收口：
  - 如果 fallback 找到了 issuer 但没有 `X509_up_ref`
  - 立即清掉 `IssuerX509` 并 `Exit(False)`
  - 从而保证不会把 released borrowed issuer 传给共享 `VerifyOCSPResponse(...)`
- 这批的 focused contract test 采用真实最小 `PX509` leaf/issuer 和真实 stack，再用 stub 的 `X509_STORE_CTX_*` / `OCSP_BASICRESP_verify` 把问题精确锁在 ownership 边界，而不是依赖随机内存损坏。
- 同一个 ownership 模式在 `VerifyCertificateOCSP(...)` / `ValidatePostHandshake(...)` 分支里高度疑似重复存在，但这批先不顺手修：
  - 需要单独的 focused regression
  - 否则会把当前批次从单点 ownership 收口扩散成 broader post-handshake policy 改动

## 2026-03-21 Findings (OpenSSL connection peer certificate chain guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoGetPeerCertificateChain(...)` 虽然一开始就把 `Result` 设为空数组，但真正的 contract 仍然会被四个连续 helper 缺口破坏：
  - `SSL_get_peer_cert_chain`
  - `sk_X509_num`
  - `sk_X509_value`
  - `X509_up_ref`
  任何一个 helper 缺失，当前实现都会把“空数组退化”破坏成 `EAccessViolation`。
- 这批和前几轮不同，不再是单 helper query，而是一个很小的组合路径：
  - 先拿 peer chain
  - 再拿 count
  - 再取每个 `PX509`
  - 最后做 `X509_up_ref` 以接管 borrowed handle 的生命周期
  所以最小收口点不是只补一条 guard，而是把这四个 capability 一次性并入入口 guard。
- `X509_up_ref` 缺失时尤其不能“勉强返回部分结果”：
  - `sk_X509_value` 返回的是 borrowed `PX509`
  - 当前实现依赖 `X509_up_ref` 后再把句柄交给 `TOpenSSLCertificate.Create(..., True)`
  - 如果没有 `X509_up_ref` 还继续构造 owned wrapper，会把所有权语义做坏
  - 因此最安全且最小的 contract 保持方式就是直接退化为空数组
- 这批的 RED 不需要真实 TLS 对端或真实 `STACK_OF(X509)`：
  - `SSL_get_peer_cert_chain` 缺口可直接置空
  - `sk_X509_num` / `sk_X509_value` 缺口可用 fake chain 指针 + stub count/value 精确触发
  - `X509_up_ref` 缺口只需要让 `sk_X509_value` 返回一个真实临时 `PX509`
  这样可以把崩溃严格锁在 `DoGetPeerCertificateChain(...)`，不把问题扩散到别的路径。
- 完成 peer-chain getter 后，`openssl.connection` 里下一段最值得继续压缩的面已经从 direct getter 转向更宽的 OCSP issuer-resolution / verified-chain 组合路径：
  - `DoIsOCSPResponseVerified(...)`
  - `VerifyCertificateOCSP(...)`
  它们重复依赖 peer-chain / verified-chain / `X509_up_ref` 组合，是当前最自然的下一批。

## 2026-03-21 Findings (OpenSSL connection verify result guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoGetVerifyResult(...)` 与前几批一样，是一个非常直接的单 helper query 缺口：
  - 函数已经在 `FSSL = nil` 时明确退化为 `-1`
  - 这意味着 public contract 已经把“当前不可查询”编码成了 `-1`
  - 但当前实现仍直接调用 `SSL_get_verify_result(FSSL)`，所以 helper 缺失时会把这个整数 contract 破坏成 `EAccessViolation`
- 这批的 regression 不需要去碰 `GetVerifyResultString(...)`：
  - `GetVerifyResultString(...)` 依赖 `DoGetVerifyResult(...)`
  - 先把 direct integer contract 收口，能让后续 string path 至少建立在稳定的底座上
  - 这是更小也更可审计的切法
- 最小正确修法同样是把 helper 缺失并入已有 `-1` 退化路径：
  - `if (FSSL = nil) or (not Assigned(SSL_get_verify_result)) then Exit(-1);`
  - 不改错误码映射
  - 不触碰 `X509_verify_cert_error_string(...)` 的更高层字符串路径
- 当前 `openssl.connection` 的高置信 public crash surface已经被持续压缩：
  - `GetConnectionInfo(...)`
  - `GetPeerCertificate(...)`
  - `IsSessionReused(...)`
  - `GetVerifyResult(...)`
  这几条都已收口到既有 contract
- 下一段更有价值的队列将从“单 helper query”逐步转向“多 helper 组合面”：
  - `DoGetPeerCertificateChain(...)`
  - 视结果再看 session/diagnostics 组合路径

## 2026-03-21 Findings (OpenSSL connection session reused guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoIsSessionReused(...)` 是另一个很干净的单 helper query 缺口：
  - 函数已经在 `FSSL = nil` 时返回 `False`
  - 说明它的 public contract 本来就是“无法判断或未连接时安全退化为 False”
  - 但当前实现仍直接调用 `SSL_session_reused(FSSL)`，所以 helper 缺失时会把这个布尔 contract 破坏成 `EAccessViolation`
- 这批同样不需要真实 TLS 会话即可稳定复现：
  - 用 `TMemoryStream` 走 stream-based `TOpenSSLConnection` 构造器，拿到真实 `FSSL`
  - 直接把 `SSL_session_reused := nil`
  - 调 direct `IsSessionReused`
  - 即可把故障精确锁在 `DoIsSessionReused(...)`
- 最小正确修法和 peer-certificate 批次同型：
  - `if (FSSL = nil) or (not Assigned(SSL_session_reused)) then Exit(False);`
  - 不改 `DoGetSession(...)`
  - 不碰 `SSL_set_session` / session ticket / session resumption broader logic
- 下一刀最自然的是同类整数 query：
  - `DoGetVerifyResult(...)`
  - 然后再决定是否继续扫 `DoGetPeerCertificateChain(...)` 这种多 helper 组合面

## 2026-03-21 Findings (OpenSSL connection peer certificate guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `DoGetPeerCertificate(...)` 是一个非常干净的单 helper query 缺口：
  - 函数开头已经把 `Result := nil`
  - `FSSL = nil` 时也直接 `Exit`
  - 这说明它的 public contract 很明确：拿不到对端证书时就返回 `nil`
  - 但当前实现仍直接调用 `SSL_get_peer_certificate(FSSL)`，所以 helper 缺失时会把这个 nil contract 破坏成 `EAccessViolation`
- 这批不需要真实 TLS 对端即可稳定复现：
  - 用 `TMemoryStream` 走 stream-based `TOpenSSLConnection` 构造器，拿到真实 `FSSL`
  - 直接把 `SSL_get_peer_certificate := nil`
  - 再调用 direct `GetPeerCertificate`
  - 就能把故障精确锁在 `DoGetPeerCertificate(...)`
- 最小正确修法非常直接：
  - 在 `DoGetPeerCertificate(...)` 开头把 guard 收口成
    - `if (FSSL = nil) or (not Assigned(SSL_get_peer_certificate)) then Exit;`
  - 不改 `TOpenSSLCertificate.Create(...)`
  - 不碰 `DoGetPeerCertificateChain(...)` 的更宽 helper 组合面
- `openssl.connection` 现在仍适合继续按单 helper query 批次推进：
  - `DoIsSessionReused(...)`
  - `DoGetVerifyResult(...)`
  - 然后再回到 peer-chain / session 组合路径

## 2026-03-21 Findings (OpenSSL connection info cipher guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 `GetConnectionInfo(...)` 在这轮审查里暴露的不是新语义问题，而是 override 聚合阶段重复解引用 cipher helper：
  - `Result := inherited GetConnectionInfo` 已经先拿到了基线值
  - 但随后又直接调用 `SSL_get_current_cipher(FSSL)`
  - 如果拿到非空 cipher，又直接调用 `SSL_CIPHER_get_name(Cipher)`
  - 所以 helper 缺失时，public API 会在“补充 OpenSSL 细节”这一步退化成 `EAccessViolation`
- 这里最重要的 contract 不是“未握手 fresh 连接一定报告 TLS1.2”，而是：
  - override 不得破坏 inherited 已经给出的安全基线
  - 因此正确的 regression 写法是先捕获 fresh-connection baseline，再比较 helper-loss 场景是否仍与该 baseline 一致
  - 这比把 `ProtocolVersion` 硬编码成某个枚举值更稳，也更贴近真实 public contract
- 为了让 `SSL_CIPHER_get_name` 缺口可稳定复现，测试不能只给出 fake cipher 指针：
  - 需要 stub `SSL_get_current_cipher` 返回 non-nil fake cipher
  - 同时把 `SSL_CIPHER_get_bits` stub 成安全返回，避免真实 OpenSSL 对 fake pointer 做无关解引用
  - 这样故障才能精确锁在 `SSL_CIPHER_get_name`
- 最小正确修法不是 `Exit` 整个 `GetConnectionInfo(...)`，否则会把无关的 servername path 一并短路。
  最小收口应是：
  - 仅在 cipher aggregation 外层检查 `Assigned(SSL_get_current_cipher)`
  - 仅在读取 cipher name 前检查 `Assigned(SSL_CIPHER_get_name)`
  - 保持 `SSL_CIPHER_get_bits` 与 `SSL_get_servername` 的既有 guard 行为不变
- `openssl.connection` 的下一批最好继续走单 helper query 面，而不要马上跳到 peer-chain 组合路径：
  - `DoGetPeerCertificate(...)`
  - `DoIsSessionReused(...)`
  - `DoGetVerifyResult(...)`

## 2026-03-21 Findings (OpenSSL connection cipher name guard)
- 在 `DoGetProtocolVersion(...)` 之后，`src/fafafa.ssl.openssl.connection.pas` 里下一处同风格 query helper nil-call 入口是 `DoGetCipherName(...)`：
  - 函数开头已经把 `Result` 设为 `''`
  - 但随后在 `FSSL <> nil` 时直接调用 `SSL_get_current_cipher(FSSL)`
  - 若拿到非空 cipher，再直接调用 `SSL_CIPHER_get_name(Cipher)`
  - 因此任一 helper 丢失时，原本已经编码进函数体的 empty-string contract 会被破坏成 `EAccessViolation`
- 这里要让 `SSL_CIPHER_get_name` 的缺口稳定可见，不能依赖真实未握手连接，因为正常情况下 `SSL_get_current_cipher(FSSL)` 会先返回 nil，掩盖后续 nil-call。
  正确的 RED 夹具是：
  - 场景 1：直接把 `SSL_get_current_cipher := nil`
  - 场景 2：把 `SSL_get_current_cipher` stub 成返回非空 fake cipher，再把 `SSL_CIPHER_get_name := nil`
  这样才能把两处崩溃面都精确锁在 `DoGetCipherName(...)`
- 这批的 public contract 很明确：
  - `GetCipherName` 是 value-returning query API；当前设计就是拿不到 cipher 时返回空字符串
  - helper 缺失时应继续安全退化为 `''`
  - 不应该把 helper 缺失暴露成异常，更不应该是 `EAccessViolation`
- 最小正确修法因此只需要在 `DoGetCipherName(...)` 本地补两条 capability guard：
  - 进入函数后要求 `Assigned(SSL_get_current_cipher)`
  - 拿到非空 cipher 后要求 `Assigned(SSL_CIPHER_get_name)`
  - 不改 handshake、`GetConnectionInfo(...)` 或 broader query design
- `openssl.connection` 继续可做，但已经从单一 helper 入口转向组合查询面：
  - `GetConnectionInfo(...)` 现在是最自然的下一批
  - 其余 query helper 继续按单批策略推进

## 2026-03-21 Findings (OpenSSL connection protocol version guard)
- 在 `Shutdown` / `Renegotiate` 批次之后，`src/fafafa.ssl.openssl.connection.pas` 里最干净的 query helper nil-call 入口是 `DoGetProtocolVersion(...)`：
  - 函数一开始就把 `Result` 设为 `sslProtocolTLS12`
  - 但随后在 `FSSL <> nil` 时直接调用 `SSL_version(FSSL)`
  - 因此 helper 缺失时，会把原本已经明确写在代码里的 safe-default contract 破坏成 `EAccessViolation`
- 这批同样不需要真实 TLS 会话即可稳定复现：
  - 用 `TMemoryStream` 走 stream-based `TOpenSSLConnection` 构造器，拿到真实 `FSSL`
  - 直接把 `SSL_version := nil`
  - direct `GetProtocolVersion` 就会立即崩溃
- 这批的 contract 非常明确，源码本身已经说明了目标行为：
  - `FSSL=nil` 时默认返回 `sslProtocolTLS12`
  - 未知版本值时也默认返回 `sslProtocolTLS12`
  - helper 缺失时应继续沿用同一 safe-default，而不是抛异常
- 最小正确修法因此只需要在 `DoGetProtocolVersion(...)` 的 helper 调用前补一条 guard：
  - `if (FSSL = nil) or (not Assigned(SSL_version)) then Exit;`
  - 不改版本映射表，不改 `GetConnectionInfo(...)` 或其他 query API
- `openssl.connection` 的下一批高置信 query 队列开始分散，但仍可继续单刀推进：
  - `DoGetCipherName(...)`
  - `GetConnectionInfo(...)` 中的 cipher/state helper 组合

## 2026-03-21 Findings (OpenSSL connection renegotiate handshake guard)
- `src/fafafa.ssl.openssl.connection.pas` 在完成 `Shutdown` 批次后，下一处同型高置信 helper gap 是 `DoRenegotiate(...)`：
  - 入口已经 guard 了 `SSL_renegotiate`
  - 但当 `SSL_renegotiate(FSSL)` 返回成功后，会直接调用 `SSL_do_handshake(FSSL)`
  - 因此 helper 丢失时 direct `Renegotiate` 会直接退化成 `EAccessViolation`
- 这批不需要真实 TLS 对端即可稳定复现：
  - 用 `TMemoryStream` 走 stream-based `TOpenSSLConnection` 构造器
  - 在测试子类里强制 `FConnected := True` 与 `FHandshakeComplete := True`
  - stub `SSL_renegotiate` 返回成功
  - baseline 下把 `SSL_do_handshake` stub 成成功可得到 `Renegotiate=True`
  - 然后仅把 `SSL_do_handshake := nil`，即可把崩溃精确锁在 `DoRenegotiate(...)`
- 这批的 public contract 很清晰：
  - `Renegotiate` 是布尔型 API；helper 缺失时应安全退化为 `False`
  - 不应该把 helper 缺失暴露成异常，更不应该是 `EAccessViolation`
- 最小正确修法因此只需要在 `DoRenegotiate(...)` 的握手调用点前补一条 capability guard：
  - `if not Assigned(SSL_do_handshake) then Exit;`
  - 不改 `SSL_renegotiate` 入口、stream pump、full handshake loop 或 broader renegotiation design
- `openssl.connection` 的连续队列仍然存在，但剩余面已不如前几批直接：
  - 继续优先看 close-adjacent / state-query helper 路径
  - 如果没有新的高置信 public nil-call，再切回其他 helper 家族

## 2026-03-21 Findings (OpenSSL connection shutdown guard)
- `src/fafafa.ssl.openssl.connection.pas` 当前在关闭路径上的最窄 helper-availability 缺口是 `DoShutdown(...)`：
  - 代码直接在 `FSSL <> nil` 时调用 `SSL_shutdown(FSSL)`
  - 没有本地 `Assigned(SSL_shutdown)` guard
  - 因为 `DoClose` 直接委托 `DoShutdown`，所以这个缺口同时影响 `Shutdown` 和 `Close` 族入口
- 这批不需要建立真实 TLS 会话即可稳定复现：
  - 用 `TMemoryStream` 走 stream-based `TOpenSSLConnection` 构造器，拿到真实 `FSSL`
  - 不做握手，直接把 `SSL_shutdown := nil`
  - direct `Shutdown` 立刻退化成 `EAccessViolation`
  这说明问题是纯粹的 shutdown helper nil-call，而不是网络、BIO pump 或 post-handshake 状态问题。
- 这批的 public contract 很明确：
  - `Shutdown` 当前是布尔型 API，并且 OpenSSL 实现无论 `SSL_shutdown` 返回值如何都写死 `Result := True`
  - helper 缺失时应继续保持“不抛异常、返回 `True`、清掉连接状态”的既有 contract
  - 不应把 helper 缺失暴露成异常，更不应是 `EAccessViolation`
- 最小正确修法因此只需要在 `DoShutdown(...)` 的单点调用前加 capability guard：
  - `if (FSSL <> nil) and Assigned(SSL_shutdown) then SSL_shutdown(FSSL);`
  - 保留 `FConnected := False` 和 `Result := True`
  - 不改 destructor、`SSL_free`、renegotiate 或 broader close design
- 连续队列仍然留在 `openssl.connection`：
  - `DoRenegotiate(...)` 仍直接依赖 `SSL_do_handshake`，很可能是下一处高置信非构造 helper gap
  - 其余 close-adjacent 路径继续按单批策略审计

## 2026-03-21 Findings (OpenSSL connection stream handshake guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 stream-based `TOpenSSLConnection.Connect` 当前最窄可达缺口，在 `InternalHandshake(...)` 的握手 helper 前置检查缺失：
  - `SSL_do_handshake`
  - `SSL_get_error`
  这两个符号在 stream-handshake loop 中都被直接解引用，缺失时 direct `Connect` 会直接退化成 `EAccessViolation`。
- 这批不需要真实网络或完整 TLS 对端即可稳定复现：
  - 用 `TMemoryStream` 触发 stream transport 路径
  - 把 `SSL_do_handshake` stub 成固定失败，`SSL_get_error` stub 成 `SSL_ERROR_WANT_READ`
  - baseline 就能稳定得到 “不抛异常、返回 `False`” 的空流握手失败
  - 然后分别抽掉 `SSL_do_handshake` / `SSL_get_error`，就能把崩溃精确锁在 `InternalHandshake(...)`
- 这批的 public contract 很清晰：
  - `Connect` 是布尔型 API；helper 缺失时应安全退化为 `False`
  - 不应该把 helper 缺失暴露成异常，更不应该是 `EAccessViolation`
- 最小正确修法因此只需要在 `InternalHandshake(...)` 开头补两条 capability guard：
  - 缺 `SSL_do_handshake` -> 直接 `False`
  - 缺 `SSL_get_error` -> 直接 `False`
  - 不改 stream pump loop、OCSP、constructor、shutdown 或 renegotiate 逻辑
- 连续队列仍然留在 `openssl.connection`：
  - `Shutdown` / `Renegotiate` 等其余非-constructor helper 入口
  - 如无更高置信问题，再切回其他 public helper 家族

## 2026-03-21 Findings (Certificate utils VerifyChain BIO guard)
- `src/fafafa.ssl.cert.utils.pas` 的 `TCertificateUtils.VerifyChain(...)` 当前真正可达的问题面，不是 leaf load 本身，而是 leaf 成功加载后对 bundled intermediate 的第二阶段解析：
  - second-stage read path 仍直接依赖 `BIO_new_mem_buf` / `PEM_read_bio_X509` / `BIO_free`
  - intermediate export path 仍直接依赖 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_X509` / `BIO_free`
  这些 helper 在内部阶段缺失时，原实现会直接退化成 `EAccessViolation`。
- 这里不能只把 helper 直接置空做测试，因为 leaf 证书的前置加载会先命中更底层 guard，掩盖 `VerifyChain(...)` 自己的缺口。
  正确的 RED 夹具需要用 call-count stub：
  - 先让 leaf load 成功
  - 再在 `VerifyChain(...)` 的内部 bundled parse 阶段把 helper 置空
  这样才能把故障稳定锁在目标入口。
- 这批的 direct/try contract 与布尔型 API 一致：
  - `VerifyChain(...)` 是 value-returning API，helper 缺失时应安全退化为 `False`
  - `TryVerifyChain(...)` 的语义是“调用是否成功且不抛异常”；因此 direct 退化为 `False` 后，`TryVerifyChain(...)` 仍应返回 `True`，同时设置 `AIsValid=False`
- 最小正确修法不是重写证书链验证器，而是在 `VerifyChain(...)` 内给 bundled-intermediate 阶段补局部 capability guard：
  - 进入第二阶段前检查 read helper 集
  - 首次 skip-read 之后、循环每次读取之前再次检查 read helper 集
  - intermediate PEM export 前后检查 write helper 集
  - `BIO_free` cleanup 改成 guarded cleanup，helper 已缺失时允许小范围泄漏而不是崩溃
  - 不改 `fafafa.ssl.certchain`、trust store 装载、链语义或 public API
- 下一轮连续队列保持在剩余高置信 crash surface：
  - `src/fafafa.ssl.openssl.connection.pas` 内其余 stream pump / handshake 非-constructor 入口
  - 如出现新的 public helper nil-call 家族，再单开一批

## 2026-03-21 Findings (Certificate utils signed-generation BIO guard)
- `src/fafafa.ssl.cert.utils.pas` 的 `TCertificateUtils.GenerateSigned(...)` 暴露的是两个独立但连续的 helper contract gap，而不是签发逻辑错误：
  - CA PEM read path 直接依赖 `BIO_new_mem_buf` / `PEM_read_bio_X509` / `PEM_read_bio_PrivateKey` / `BIO_free`
  - leaf PEM export path 直接依赖 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_X509` / `PEM_write_bio_PrivateKey` / `BIO_free`
  这些 helper 缺失时，原实现会在 direct API 上直接退化成 `EAccessViolation`。
- 这批的 public contract 分层和 self-signed 批次一致：
  - `GenerateSigned(...)` 是异常型 API，helper 缺失时应抛受控 `ESSLCertError`
  - `TryGenerateSigned(...)` 应保持不抛异常，返回 `False` 并清空输出
- 当前单元里已有的局部 capability predicate 足够支撑最小修复：
  - `HasCertificatePEMReadBIOHelpers`
  - `HasCertificatePEMWriteBIOHelpers`
  但 `GenerateSigned(...)` 的 CA key read 仍需要单独的 `PEM_read_bio_PrivateKey` guard，因此新增 `HasPrivateKeyPEMReadBIOHelpers` 是最小正确收口。
- 这批最小正确修法因此不是重构签发流程，而是在 `GenerateSigned(...)` 内做局部前置 guard：
  - CA cert read 前检查 `HasCertificatePEMReadBIOHelpers`
  - CA key read 前检查 `HasPrivateKeyPEMReadBIOHelpers`
  - leaf cert/key export 前分别检查现有的 certificate/private-key write predicates
  - helper 缺失时直接抛受控 `ESSLCertError`
  - 不改密钥生成、扩展设置、`Try*` wrapper 结构或 broader generation design
- 下一轮连续队列继续优先处理剩余高置信入口：
  - `src/fafafa.ssl.cert.utils.pas` 的 `VerifyChain(...)`
  - `src/fafafa.ssl.openssl.connection.pas` 内其余 stream pump / handshake 非-constructor 入口
  - 只在需要时再回头处理新的 `cert.utils` 边角 helper 路径

## 2026-03-20 Findings (Certificate utils self-signed export BIO guard)
- `src/fafafa.ssl.cert.utils.pas` 的 `TCertificateUtils.GenerateSelfSigned(...)` 在证书/私钥导出阶段暴露的是纯粹的 nil-call contract gap，而不是生成逻辑错误：
  - 证书导出直接依赖 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_X509` / `BIO_free`
  - 私钥导出直接依赖 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_PrivateKey` / `BIO_free`
  这些 helper 缺失时，原实现会直接退化成 `EAccessViolation`。
- 这一批的 direct/try contract 分层很清晰：
  - `GenerateSelfSigned(...)` 是异常型 API，helper 缺失时应该抛受控 `ESSLCertError`
  - `TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` 应继续保持不抛异常、返回 `False` 并清空输出
- 现有单元里已经有可复用的证书 PEM 导出 helper 判定：
  - `HasCertificatePEMWriteBIOHelpers`
  但它只覆盖证书导出，不覆盖 `PEM_write_bio_PrivateKey`；因此私钥导出仍需要单独的 capability predicate。
- 这批的最小正确修法因此是双 guard，而不是重构整个生成流程：
  - 在证书导出前检查 `HasCertificatePEMWriteBIOHelpers`
  - 新增私钥导出 predicate，要求 `BIO_new` / `BIO_s_mem` / `PEM_write_bio_PrivateKey` / `BIO_free`
  - helper 缺失时直接抛受控 `ESSLCertError`
  - 不改签名、扩展写入、密钥生成和 `Try*` wrapper 结构
- 下一轮连续队列继续保持高置信单批推进：
  - `src/fafafa.ssl.cert.utils.pas` 的 `GenerateSigned(...)`
  - `src/fafafa.ssl.cert.utils.pas` 的 `VerifyChain(...)`
  - `src/fafafa.ssl.openssl.connection.pas` 的 stream pump / handshake 非-constructor 入口

## 2026-03-20 Findings (Certificate utils GetInfo BIO guard)
- `src/fafafa.ssl.cert.utils.pas` 的 `TCertificateUtils.GetInfo(...)` 已经有明确的 silent-degrade 失败语义：
  - 开头先 `FillChar(Result, ...)`
  - 立即分配 `Result.SubjectAltNames := TStringList.Create`
  - PEM parse 失败时直接 `Exit`
  这意味着它的正确 contract 不是抛异常，而是返回空 `TCertInfo` 且保留可释放的 `SubjectAltNames`。
- 真实缺口出在这层 contract 之后：
  - `BIO_new_mem_buf(...)` 没有 guard
  - `finally` 里的 `BIO_free(LBIO)` 也没有 guard
  因此 helper 缺失时，`GetInfo(...)` 会直接暴露 `EAccessViolation`，破坏上层“空结果而非崩溃”的约定。
- `TryGetInfo(...)` 已经把这个崩溃吞掉了，所以问题面比 `PEMToDER(...)` 那批更窄：
  - 直接调用 `GetInfo(...)` 会崩
  - `TryGetInfo(...)` 通常不会崩，但只是靠 `except` 兜底，不是因为底层 contract 正确
- 这批最小正确修法因此不是重写 `TryGetInfo(...)`，而是让 `GetInfo(...)` 自己在 helper 缺失前就走已有退化路径：
  - 复用本单元现成的 `HasCertificatePEMReadBIOHelpers`
  - 缺 `BIO_new_mem_buf` / `PEM_read_bio_X509` / `BIO_free` 时直接 `Exit`
  - 保持成功路径、字段提取和 `SubjectAltNames` ownership 不变
- 下一轮高价值队列继续保持单批次推进：
  - `src/fafafa.ssl.cert.utils.pas` 的生成导出路径
  - `src/fafafa.ssl.cert.utils.pas` 的 `VerifyChain(...)`
  - `src/fafafa.ssl.openssl.connection.pas` 的 stream pump / handshake 非-constructor 入口

## 2026-03-20 Findings (Advanced certificate CRL BIO guard)
- `src/fafafa.ssl.cert.advanced.pas` 的 `TCRLManagerImpl.ParseCRL(...)` 暴露了两层问题，而且都集中在同一个窄入口：
  - 公开 contract gap：缺 `BIO_new_mem_buf` / `PEM_read_bio_X509_CRL` / `BIO_free` 时，`LoadFromPEM(...)` 直接退化成 `EAccessViolation`
  - baseline success-path bug：即使 helper 都在，`ParseCRL(...)` 里本地 `ASN1_TIME_to_tm` 桥接也会在成功解析后把调用栈打坏，表现为 `LoadFromPEM(...)` 返回前发生 `EAccessViolation`
- systematic debugging 把 warmup fatal AV 缩到了 `nextUpdate` 提取：
  - valid CRL PEM 夹具本身是好的，`openssl crl -text` 可正常解析
  - `PEM_read_bio_X509_CRL(...)` 成功返回，`BIO_free(LBio)` 也完成
  - 只有在继续走本地 `ASN1_TIME_to_tm` 路径时，调用返回栈才被破坏
- 仓库里已经有更安全的现成模式：
  - `src/fafafa.ssl.openssl.api.asn1.pas` 提供字符串解析版 `ASN1TimeToDateTime(...)`
  - `src/fafafa.ssl.cert.utils.pas` 也已经委托给该 shared helper
  - 因此 `cert.advanced` 的本地 `TM`-based helper 是重复且风险更高的实现
- 这批的最小正确修法因此是双收口，而不是只给 helper 缺失打补丁：
  - `ParseCRL(...)` 前置要求 `BIO_new_mem_buf` / `PEM_read_bio_X509_CRL` / `BIO_free`
  - `nextUpdate` 转换改为委托共享的 `fafafa.ssl.openssl.api.asn1.ASN1TimeToDateTime(...)`
  - 保持 `LoadFromPEM(...)` 的异常型 contract，不改 CRL 语义、不扩散到 OCSP/PKCS#12
- 下一轮高价值队列继续保持：
  - `src/fafafa.ssl.cert.utils.pas` 的 `GetInfo(...)` / 生成导出路径 / `VerifyChain(...)`
  - `src/fafafa.ssl.openssl.connection.pas` 的 stream pump / handshake 非-constructor 入口

## 2026-03-20 Findings (OpenSSL connection stream BIO cleanup guard)
- `src/fafafa.ssl.openssl.connection.pas` 的 stream-based `TOpenSSLConnection.Create(AContext, AStream)` 在 BIO 部分初始化失败时有两个紧邻的失败窗口：
  - 第二个 `BIO_new(BIO_s_mem())` 失败时，cleanup 分支直接调用 `BIO_free(FBioRead)`，在 `BIO_free=nil` 时会立刻崩成 `EAccessViolation`
  - 同一个构造器在 `FSSL := SSL_new(Ctx)` 成功后，到 `SSL_set_bio(...)` 接管 BIO 之前，缺少统一失败回收；任何此区间异常都会留下未附着的 `FSSL`/BIO 资源
- focused RED 先暴露了一个测试夹具问题：
  - 仅引用 `fafafa.ssl.factory` 时，`TSSLFactory.GetLibraryInstance(sslOpenSSL)` 会报 “backend OpenSSL is not registered”
  - 测试需像先前 OpenSSL contract 批次一样引入 `fafafa.ssl` 来完成后端注册，之后才能稳定命中目标构造器缺口
- 在夹具收敛后，目标故障被稳定锁定：
  - 第一次 `BIO_new` 成功
  - 第二次 `BIO_new` 失败
  - `BIO_free := nil`
  - 当前源码对外暴露为 `EAccessViolation`，而不是受控 `ESSLOutOfMemoryException`
- 这批的最小正确修法不是扩大 BIO capability 检查，而是给构造器补上局部失败回收：
  - 用 guarded cleanup 处理“尚未交给 `SSL_set_bio` 接管”的 BIO 指针
  - 用 `finally` 在未完成构造时回收 `FSSL`
  - 保持成功路径、BIO ownership 以及后续 stream pump 行为不变
- 这批做完后，`openssl.connection` 主题仍值得继续审查，但应单开批次：
  - stream pump / handshake 路径对 `BIO_read` / `BIO_write` / `BIO_pending` 的 helper 依赖
  - 其余关闭/错误路径是否还有类似 “helper 丢失时 cleanup nil-call” 的 contract gap

## 2026-03-20 Findings (Certificate utils conversion BIO guard)
- `src/fafafa.ssl.cert.utils.pas` 里的 conversion/fingerprint 公开 helper 是一组很干净的高层入口，且 contract 明确分裂成两类：
  - value-returning degrade contract:
    - `PEMToDER(...)` -> 空 `TBytes`
    - `DERToPEM(...)` -> 空字符串
  - exception contract:
    - `GetFingerprint(...)` -> 受控 `ESSLCertError`
  - 对应 Try wrapper 则都应返回 `False` 且不抛异常
- focused RED 证实当前 crash 面集中在真实的 memory BIO constructor/cleanup 依赖，而不是转换算法本身：
  - read path:
    - `BIO_new_mem_buf`
    - `BIO_free`
  - write path:
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
  - `BIO_get_mem_data` 在仓库里是安全 macro-wrapper，底层依赖是 `BIO_ctrl`，不是这批 nil-call 的直接 crash source。
- 这批的最小正确修法因此是 helper-level capability guard，而不是 broader redesign：
  - `PEMToDER(...)` 缺 read helper -> 直接空 `TBytes`
  - `DERToPEM(...)` 缺 write helper -> 直接空字符串
  - `GetFingerprint(...)` 缺 read helper -> 抛受控 `ESSLCertError`
  - `TryPEMToDER(...)` / `TryDERToPEM(...)` / `TryGetFingerprint(...)` 自动沿用原 contract 返回 `False`
- `cert.utils` 里仍有后续可做的同型面，但应保持单独批次：
  - `GetInfo(...)`
  - `GenerateSelfSigned(...)` / `GenerateSigned(...)` 的 PEM export path
  - `VerifyChain(...)` 的中间证书 memory BIO path
- 连续队列优先级目前保持为：
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.cert.utils.pas` 的剩余入口
  - `src/fafafa.ssl.cert.advanced.pas` 中的 `TCRLManagerImpl.ParseCRL(...)`

## 2026-03-20 Findings (Certificate pinning BIO guard)
- `src/fafafa.ssl.cert.pinning.pas` 这批和前面的 BIO guard 问题不是同一种根因：
  - `TPinValidator.ExtractPublicKeyHash(...)` 会分配 `BIO_new(BIO_s_mem())`
  - 但真正的 SPKI DER 编码实际走的是 `i2d_PUBKEY(PubKey, nil/@P)` 直接缓冲区路径
  - 也就是说 `BIO_new` / `BIO_s_mem` / `BIO_free` 在这里是死依赖，不参与任何有效输出
- focused RED 证实三个公开入口都被同一个死依赖拖崩：
  - `ValidateCertificate(...)`
  - `ValidateCertificateChain(...)`
  - `ValidateCertificateEx(...)`
  把上述任一 helper 置空后，都会直接触发 `EAccessViolation`，即使传入的是正确的 public-key pin。
- 因此这批的最小正确修法不是“能力缺失就退化为 False”，而是删除错误依赖，保持原语义：
  - valid pin 继续匹配成功
  - `ValidateCertificateEx(...)` 继续填充 fingerprint 与 matched pin 信息
  - 不把一个无关 helper 的缺失扩散成 pinning false negative
- 这批之后，BIO nil-call 连续队列仍然集中在真正需要 memory/file BIO 的入口：
  - `src/fafafa.ssl.cert.utils.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.cert.advanced.pas` 中的 `TCRLManagerImpl.ParseCRL(...)`

## 2026-03-20 Findings (Advanced certificate PKCS12 BIO guard)
- `src/fafafa.ssl.cert.advanced.pas` 里的 `TPKCS12Manager` 证明这条问题族已经进入 advanced certificate facade 的公开 PKCS#12 入口：
  - `CreatePKCS12(...)`
  - `LoadFromPKCS12(...)`
  export path 是异常型 contract，import path 是 `Boolean` degrade contract，但 helper 缺失时两者都会直接退化成 `EAccessViolation`。
- 这批的依赖面非常集中，正适合做窄修：
  - export 依赖 `BIO_new` / `BIO_s_mem` / `BIO_free`
  - import 依赖 `BIO_new_mem_buf` / `BIO_free`
  - `BIO_get_mem_data` 仍然走 wrapper，本身不是这批的主要 crash source
- focused RED 说明 PKCS#12 场景的夹具也必须先预热：
  - 先创建真实 keypair/certificate
  - 再生成一份真实 PKCS#12 bytes
  - 然后才把 `BIO_*` 指针置空
  否则容易把证书生成或 fixture 构造误报成目标失败。
- 最小正确修法继续保持 contract-preserving：
  - `CreatePKCS12(...)` 缺 export BIO helper 时抛受控 `ESSLException`
  - `LoadFromPKCS12(...)` 缺 import BIO helper 时直接返回 `False`
  - 不改 PKCS#12 结构创建/解析流程，不扩散到 CRL manager
- 这一批做完后，advanced certificate 主题下还剩一个较窄同型点：
  - `TCRLManagerImpl.ParseCRL(...)`
  但更高价值的连续队列仍是：
  - `src/fafafa.ssl.cert.utils.pas`
  - `src/fafafa.ssl.cert.pinning.pas`
  - `src/fafafa.ssl.openssl.connection.pas`

## 2026-03-20 Findings (OpenSSL context BIO guard)
- `src/fafafa.ssl.openssl.context.pas` 证明这条 `BIO_*` nil-call 问题族已经进入最常用的上下文装载入口，而不是只停留在 helper / certificate object 层：
  - `LoadCertificate(AStream)`
  - `LoadCertificatePEM(...)`
  - `LoadPrivateKey(const AFileName, const APassword)` 的 password/BIO file path
  - `LoadPrivateKey(AStream, ...)`
  - `LoadPrivateKeyPEM(...)`
  这些接口对外本来都是异常型 contract，但 helper 缺失时会直接退化成 `EAccessViolation`。
- 这批把风险面进一步坐实为三条具体 BIO 依赖拓扑：
  - certificate memory load 依赖 `BIO_new_mem_buf` / `BIO_free`
  - private-key memory load 依赖 `BIO_new_mem_buf` / `BIO_free`
  - encrypted private-key file load 依赖 `BIO_new_file` / `BIO_free`
- focused RED 说明这类 context 测试不能只靠 `CreateSSLLibrary(...).Initialize`：
  - 还需要显式拉起 `Core/BIO/X509/PEM` 低层模块
  - 否则会被 baseline capability skip 掩盖掉真正的 context crash path
  - 在夹具收敛后，五个入口在 `BIO_new_mem_buf` / `BIO_new_file` / `BIO_free` 缺失时都稳定复现 `EAccessViolation`
- 最小正确修法继续保持 contract-preserving guard，而不是扩大 loader 职责：
  - certificate memory path 缺 BIO helper 时走受控 certificate exception
  - private-key memory/file path 缺 BIO helper 时走受控 key exception
  - 不改 context 初始化，不改 PKCS#11 flow，不改 PEM parse/use 成功路径
- 经过前一轮对 `comp` / `pkcs12` 的复核后，这批完成后更高置信度的连续队列已经转向更上层 convenience/bridge 单元：
  - `src/fafafa.ssl.cert.utils.pas`
  - `src/fafafa.ssl.cert.advanced.pas`
  - `src/fafafa.ssl.cert.pinning.pas`
  - `src/fafafa.ssl.openssl.connection.pas`

## 2026-03-20 Findings (Certificate builder BIO guard)
- `src/fafafa.ssl.cert.builder.impl.pas` 证明这条问题族不只存在于 public OpenSSL wrapper，也存在于更上层的 certificate/private-key convenience implementation：
  - `TCertificateImpl.EnsureHandle`
  - `TCertificateImpl.EnsurePEM`
  - `TPrivateKeyImpl.EnsureHandle`
  - `TPrivateKeyImpl.EnsurePEM`
  它们对上层暴露的是异常型 contract，但在 helper 缺失时仍会直接退化成 `EAccessViolation`。
- 这批的证据点比前一批更贴近最终高层 facade：
  - `TCertificate.ParsePEM(...)` 最终落到 `TCertificateImpl`
  - builder/advanced certificate 流程也会走 `CreateFromHandle(...)`
  因此它虽然位于 implementation unit，仍然是实际的 convenience crash path。
- focused RED 说明测试夹具必须区分两种入口：
  - `GetX509Handle` / `GetEVP_PKEYHandle` 需要先在 helper 可用时创建 wrapper object，再在 nil-state 下触发 `EnsureHandle`
  - `CreateFromHandle(...)` 需要先准备原生 handle，再在 nil-state 下触发 `EnsurePEM`
  否则会把 `LoadInfo` 或 fixture 准备路径误判成目标失败。
- 最小正确修法继续保持 contract-preserving guard：
  - PEM -> handle 路径缺 `BIO_new_mem_buf` / `BIO_free` / read helper 时，直接抛受控 `ESSLException`
  - handle -> PEM 路径缺 `BIO_new` / `BIO_s_mem` / `BIO_free` / write helper 时，也直接抛受控 `ESSLException`
  - 不改 builder 设计，不改 `TCertificateUtils`
- 连续审查队列现在更适合收窄到：
  - `src/fafafa.ssl.openssl.api.comp.pas`
  - `src/fafafa.ssl.openssl.api.pkcs12.pas`
  因为 certificate 主题下最明显的 convenience BIO nil-call 路径已经被连续收口。

## 2026-03-20 Findings (OpenSSL certificate BIO guard)
- `src/fafafa.ssl.openssl.certificate.pas` 证明这条问题族已经进入 OpenSSL 后端最直接的 public certificate convenience API：
  - `LoadFromFile(...)`
  - `LoadFromPEM(...)`
  - `LoadFromMemory(...)`
  - `SaveToFile(...)`
  - `SaveToPEM`
  - `SaveToDER`
  这些接口原本都是非异常型 contract，但在 helper 缺失时会直接退化成 `EAccessViolation`。
- 这批暴露出的依赖面已经覆盖 certificate 常见的 file/memory/save 三条 BIO pipeline：
  - file path 直接依赖 `BIO_new_file` / `BIO_free`
  - memory load 直接依赖 `BIO_new_mem_buf` / `BIO_free`
  - memory save 直接依赖 `BIO_new` / `BIO_s_mem` / `BIO_free`
- focused RED 还顺手暴露了一个测试设计点：
  - 当 `BIO_free := nil` 时，任何在 nil-state 之后临时构造 fixture 的代码都会把“夹具构造”误报成目标失败
  - 先预热并预创建 save-path certificate fixture，才能把 contract 真正锁在 helper 自身
- 最小正确修法继续保持 helper-local degrade：
  - file/memory/save helper 各自检查自己会解引用的 BIO capability
  - `LoadFromStream(...)` 也一起收口，因为它和 `LoadFromMemory(...)` 是同型 memory BIO pipeline
  - 对 PEM/DER 读写符号改为按调用点条件分支，避免在符号缺失时继续落到 nil call
  - 成功路径保持不变
- 横向扫描后，下一条高置信度目标已经转到 `src/fafafa.ssl.cert.builder.impl.pas`：
  - `TCertificateImpl.EnsureHandle` / `EnsurePEM`
  - `TPrivateKeyImpl.EnsureHandle` / `EnsurePEM`
  这些实现层虽然不直接面向最终 API 文档，但实际被 `fafafa.ssl.cert.pas` / builder 输出对象调用，仍然属于高价值 convenience crash path。

## 2026-03-20 Findings (OpenSSL session BIO guard)
- `src/fafafa.ssl.openssl.session.pas` 证明这条问题族已经扩展到 OpenSSL 后端的 session convenience API：
  - `TOpenSSLSession.Serialize`
  - `TOpenSSLSession.Deserialize`
  这两个公开接口原本都是“非异常型”合同，但在 helper 缺失时会直接退化成 `EAccessViolation`。
- 这批不只覆盖跨单元 `BIO_*` 依赖，也覆盖 session-specific helper：
  - serialize 直接依赖 `i2d_SSL_SESSION_bio`、`BIO_new`、`BIO_s_mem`、`BIO_free`
  - deserialize 直接依赖 `d2i_SSL_SESSION_bio`、`BIO_new_mem_buf`、`BIO_free`
- focused RED 的价值在于它不需要真实握手或已有 session fixture：
  - 直接用 `SSL_SESSION_new` 构造最小 handle
  - 然后把 representative helper 置空
  - 当前源码上 `Serialize` / `Deserialize` 都会在 helper 入口直接崩成 `EAccessViolation`
- 最小正确修法继续保持 helper-local degrade：
  - serialize 缺依赖时直接返回空 `TBytes`
  - deserialize 缺依赖时直接返回 `False`
  - 不引入额外初始化，不改 session 生命周期设计
- 横向扫描后，下一条高置信度目标已经很明确：
  - `src/fafafa.ssl.openssl.certificate.pas` 的 load/save helper 也在 public API 层直接复用 BIO pipeline
  - 这比内部的 `cert.builder.impl` 更值得优先收口

## 2026-03-20 Findings (Encoding Base64 BIO guard)
- `src/fafafa.ssl.encoding.pas` 证明这条问题族不只存在于 `fafafa.ssl.openssl.api.*` helper，也存在于更高层的异常型 convenience API：
  - `Base64Encode(const AInput: TBytes)`
  - `Base64Decode(const AInput: string)`
  - `Base64EncodeView(const AInputView: TBytesView)`
  这些入口会在 helper 已初始化后，继续无保护地解引用跨单元 `BIO_*` 依赖。
- 这批的依赖面比前几批更完整，因为编码路径不只碰裸函数指针：
  - encode/encode-view 直接依赖 `BIO_new` / `BIO_s_mem` / `BIO_f_base64` / `BIO_push` / `BIO_write` / `BIO_free` / `BIO_free_all`
  - decode 直接依赖 `BIO_new_mem_buf` / `BIO_new` / `BIO_f_base64` / `BIO_push` / `BIO_read` / `BIO_free` / `BIO_free_all`
  - `BIO_flush` / `BIO_get_mem_data` 虽然是 helper 包装，但底层仍依赖 `BIO_ctrl`
- focused RED 也说明 `EnsureInitialized` 只检查 `BIO_new` / `BIO_f_base64` 还不够：
  - warmup 之后再把 `BIO_push` / `BIO_free_all` / `BIO_new_mem_buf` / `BIO_read` 置空
  - exception-based helper 会直接退化成 `EAccessViolation`
  - `TryBase64Encode` / `TryBase64Decode` 本身的 contract 反而是正确的，只是被下层异常型 helper 拖垮
- 最小正确修法继续保持 helper-local contract 收敛，而不是扩大初始化职责：
  - 在 Base64 encode/decode 入口加前置依赖检查
  - 缺 capability 时抛出受控 `ESSLCryptoError`
  - `Try*` wrapper 因为原本已经 catch-all，所以自然继续退化为 `False` + 清空输出
- 下一轮横向审查不该只盯低层 API unit：
  - 高层 convenience API 只要直接拼接 `BIO` pipeline，也可能有同类 crash path
  - `comp` 仍是一个窄入口，但还需要先做一轮快速重新扫描以确认最高置信度目标

## 2026-03-20 Findings (PKCS7 helper BIO guard)
- `src/fafafa.ssl.openssl.api.pkcs7.pas` 证明这条 `BIO_*` 跨单元 nil-call 风险不只存在于 pointer-returning helper，也存在于“出错时会抛异常”的 bytes-returning helper：
  - `SignData(...)`
  - `EncryptData(...)`
  在模块已加载但 `BIO_*` 依赖缺失时，本来会从 helper 内部直接崩成 `EAccessViolation`，而不是回到它们自己的 `nil`/异常合同边界。
- 这批还把问题族扩到了更完整的 BIO pipeline：
  - input constructor：`BIO_new_mem_buf`
  - output constructor：`BIO_new` + `BIO_s_mem`
  - output reader：`BIO_read`
  - cleanup：`BIO_free`
- 和前几批相比，`pkcs7` 给了一个更强的证据点：
  - 不只是用 dummy pointer
  - 而是用仓库里的真实签名/接收者证书以及 `pkcs7_signed_attached_v1.der` fixture
  - 把 sign / verify / encrypt / decrypt 的真实成功路径准备出来之后，再把相应 `BIO_*` 依赖置空
  - 当前源码上四条 helper 都会直接抛 `EAccessViolation`
- 最小正确修法因此继续保持 self-contained helper contract：
  - helper 先检查自己实际会解引用的 `BIO_*`
  - 缺 capability 时直接 `Exit`
  - `SignData(...)` / `EncryptData(...)` 退回空 `TBytes`
  - `VerifySignedData(...)` / `DecryptData(...)` 退回 `False`
  - 不改 PKCS7 loader，不改 PEM/EVP/stack 的装载语义
- 横向审计之后，剩余高置信度目标需要重新排序：
  - `src/fafafa.ssl.openssl.api.comp.pas` 仍有直接 `BIO_new(...)` helper 入口
  - `src/fafafa.ssl.openssl.api.pkcs12.pas` 的 `BIO_new_*` 层面已经基本有 guard，后续更值得看的是 `BIO_get_mem_data` / macro-wrapper 依赖是否还留 contract gap

## 2026-03-20 Findings (CMS helper BIO guard)
- `src/fafafa.ssl.openssl.api.cms.pas` 进一步确认这条问题族已经从 file helper 扩展到了更复杂的 memory pipeline helper：
  - `CMSSignData(...)` / `CMSEncryptData(...)` 直接依赖 `BIO_new_mem_buf` / `BIO_free`
  - `CMSVerifySignature(...)` 还依赖 `BIO_new` / `BIO_s_null`
  - `CMSDecryptData(...)` 更进一步依赖 `BIO_s_mem` 与 `BIO_read`
- 这说明“跨单元 function pointer 未 guard”风险不只是 simple constructor 缺失，而是整条 helper pipeline 都可能被 capability gap 撕开：
  - 输入 BIO 构造
  - 输出/黑洞 BIO 构造
  - 读取输出 BIO
  任何一步默认当作总是可用，helper 都会退化成 `EAccessViolation`。
- focused RED 用最小 dummy input 已经证明崩溃发生在 helper 入口依赖面，而不是 CMS 密码学逻辑本身：
  - `CMSSignData(...)`
  - `CMSEncryptData(...)`
  - `CMSVerifySignature(...)`
  - `CMSDecryptData(...)`
  在对应 `BIO_*` 依赖置空时都会直接抛 `EAccessViolation`
- 最小正确修法因此继续保持 capability-aware early exit：
  - sign/encrypt 缺 `BIO_new_mem_buf` / `BIO_free` -> 直接 `nil`
  - verify 缺 `BIO_new_mem_buf` / `BIO_new` / `BIO_s_null` / `BIO_free` -> 直接 `False`
  - decrypt 缺 `BIO_new` / `BIO_s_null` / `BIO_s_mem` / `BIO_free` / `BIO_read` -> 直接空 `TBytes`
  - 不改 CMS loader，也不强制 helper 隐式加载 BIO
- 横向扫描后的下一批次建议已经很清楚：
  - `src/fafafa.ssl.openssl.api.pkcs7.pas`
  - `src/fafafa.ssl.openssl.api.pkcs12.pas`
  它们仍有与当前 CMS/PKCS 几乎同构的 memory/file BIO helper 依赖形状。

## 2026-03-20 Findings (PKCS helper BIO guard)
- `src/fafafa.ssl.openssl.api.pkcs.pas` 证实这条“跨单元 `BIO_*` function pointer 未 guard”风险不是个别 helper，而是一整组辅助入口的共同形态：
  - `LoadPKCS12FromFile(...)` 直接依赖 `BIO_new_file` / `BIO_free`
  - `CreatePKCS7SignedData(...)` 直接依赖 `BIO_new_mem_buf` / `BIO_free`
  - `VerifyPKCS7SignedData(...)` 除了 `BIO_new_mem_buf` / `BIO_free`，还依赖 `BIO_new` / `BIO_s_null`
- 这批比 PEM 更进一步，说明风险不止发生在“单一 constructor 缺失”：
  - 既可能是 file BIO 缺失
  - 也可能是 memory BIO sink/helper 缺失
  - helper 一旦把这些跨单元函数指针当成总是可用，就会直接退化成 `EAccessViolation`
- focused RED 也验证了这不是 PKCS 解析/签名算法本身的问题：
  - 把对应 `BIO_*` 指针置空后
  - `LoadPKCS12FromFile(...)`
  - `CreatePKCS7SignedData(...)`
  - `VerifyPKCS7SignedData(...)`
  都在 helper 入口直接抛出 `EAccessViolation`
- 最小正确修法仍然是对 helper 自己的 capability contract 做收敛：
  - file helper 缺 `BIO_new_file` / `BIO_free` -> 直接 `False`
  - sign helper 缺 `BIO_new_mem_buf` / `BIO_free` -> 直接 `nil`
  - verify helper 缺 `BIO_new_mem_buf` / `BIO_new` / `BIO_s_null` / `BIO_free` -> 直接 `False`
  - 顺手把同型 `SavePKCS12ToFile(...)` 一并补 guard，但不改变其原有创建/序列化语义
- 下一条连续队列已经很明确：
  - `src/fafafa.ssl.openssl.api.cms.pas`
  - 它和 PKCS 的 memory helper 拓扑几乎同构，只是 verify/decrypt 路径把 `BIO_s_mem` 也引入了依赖面。

## 2026-03-20 Findings (PEM helper BIO guard)
- `src/fafafa.ssl.openssl.api.pem.pas` 暴露了和 `TXT_DB` 同一类、但覆盖面更广的跨单元 helper 依赖问题：
  - file helpers 直接调用 `BIO_new_file` / `BIO_free`
  - memory helpers 直接调用 `BIO_new_mem_buf` / `BIO_free`
  - helper 自身没有 guard，也没有保证 BIO 模块先加载
- 这条风险比前一批更高，因为 PEM helper 是当前代码路径里的公共入口：
  - 证书/私钥文件加载
  - 证书导出
  - 内存 PEM 解析
  一旦调用方只加载了 PEM 模块但没准备 BIO，helper 会直接退化成 `EAccessViolation`。
- 这批 focused RED 用更窄的方式把根因固定住了：
  - 临时把 `BIO_new_file` / `BIO_new_mem_buf` / `BIO_free` 置空
  - `LoadCertificateFromPEM(...)`
  - `SaveCertificateToPEM(...)`
  - `LoadCertificateFromMemory(...)`
  都会在当前源码上直接抛 `EAccessViolation`
  这说明崩溃发生在 helper 入口的跨单元依赖解引用，而不是 PEM 解析逻辑本身。
- 最小正确修法仍然是 capability-aware degrade，而不是隐式补加载：
  - file helpers 缺 `BIO_new_file` 或 `BIO_free` -> 直接返回 `nil` / `False`
  - memory helpers 缺 `BIO_new_mem_buf` 或 `BIO_free` -> 直接返回 `nil`
  - 保持现有 PEM 行为和 loader 结构不变
- 连续审查的下一队列也更清楚了：
  - `src/fafafa.ssl.openssl.api.pkcs.pas`
  - `src/fafafa.ssl.openssl.api.cms.pas`
  两者同样直接复用了 `BIO_*` helper，但还没有前置 guard。

## 2026-03-20 Findings (TXT_DB helper BIO guard)
- optional-symbol 审查继续往前推后，发现下一类高价值风险不一定来自“同单元 optional binding”，也可能来自 helper 的跨单元依赖：
  - `src/fafafa.ssl.openssl.api.txt_db.pas` 的 `TXTDBReadFromFile(...)` / `TXTDBWriteToFile(...)`
  - 直接调用来自 `fafafa.ssl.openssl.api.bio` 的 `BIO_new_file` / `BIO_free`
  - 但 helper 里既没有 guard，也没有保证 BIO 模块先加载
- 这类问题和 SRP helper 是同一个根因族，只是形态不同：
  - SRP 是“同单元 optional symbol 被无条件调用”
  - TXT_DB 是“跨单元 function pointer 被无条件调用”
  两者都会把 capability gap 退化成 `EAccessViolation`。
- 这批 focused RED 没有依赖 TXT_DB 模块本身是否在当前 runtime 可用，而是直接把 `BIO_new_file` / `BIO_free` 置空：
  - `TXTDBReadFromFile(...)` 立即触发 AccessViolation
  - `TXTDBWriteToFile(...)` 也同样触发 AccessViolation
  这说明崩溃发生在 helper 自身入口，而不是更深的 TXT_DB path。
- 最小正确修法也保持对称：
  - read helper 缺 `BIO_new_file` 或 `BIO_free` -> 直接 `nil`
  - write helper 缺 `BIO_new_file` 或 `BIO_free` -> 直接 `False`
  - 不改 TXT_DB loader，也不强制 helper 隐式加载 BIO
- 下一条值得连续追的审查方向因此更清楚：
  - 不只扫“Required: False”
  - 还要扫 helper 是否跨单元直接依赖别的动态函数指针却没有前置 guard
  - 尤其是 `BIO_*` / `BN_*` / `ERR_*` 这类基础模块

## 2026-03-20 Findings (SRP helper optional user-pwd guard)
- standalone `test_p2_srp.lpr` 的 contract drift 不只是测试层问题，它还暴露出一条真实 runtime bug：
  - `src/fafafa.ssl.openssl.api.srp.pas` 把 `SRP_user_pwd_set_salt` / `SRP_user_pwd_set_verifier` 标记为 `Required: False`
  - 但 `SRPCreateUser(...)` 仍然会无条件调用这两个函数
  - 在当前 OpenSSL 3.x 环境里，这直接表现为 `EAccessViolation`
- 这类问题的本质不是“OpenSSL 3.x 少函数”本身，而是 helper contract 自相矛盾：
  - loader 说 optional
  - helper 却把它们当 required
  结果就是调用方只要碰到 helper，就会从 graceful capability downgrade 退化成崩溃。
- 最小正确修法不是扩展 SRP 支持面，而是把 helper 语义收敛到 capability-aware contract：
  - `SRPCreateUser(...)` 先要求清理路径必需的 `SRP_user_pwd_free`
  - 若 deprecated setter 缺失，则释放部分状态并返回 `nil`
  - 有 setter 的老 runtime 路径保持不变
- 这也说明后续审查应从单个 SRP helper 扩展到更通用的动态加载风险模式：
  - 任何把 function pointer 标成 optional 的 API unit
  - 只要 helper 里仍无保护调用这些符号，就存在同类 nil-call 风险

## 2026-03-20 Findings (Standalone LPR unit/API refresh and OpenSSL 3.x contract alignment)
- standalone `.lpr` smoke 程序会独立于 sibling reference program 漂移：
  - `tests/test_p2_ct.lpr` 和 `tests/certificate/test_p2_ct.pas`
  - `tests/test_p2_srp.lpr` 和 `tests/crypto/test_p2_srp.pas`
  不能只看模块本体或 compile-all，独立入口自身也会老化。
- `test_p2_ct.lpr` 的 source drift 不是单点：
  - 旧 unit 名 `fafafa.ssl.openssl.types`
  - 旧 CT serialization symbol 假设：`i2d/d2i_SCT(_LIST)`
  - 旧 X509 extension availability 假设：直接检查 `X509_get_SCT_LIST`
  当前源码/参考实现已经收口到：
  - `fafafa.ssl.openssl.api.types`
  - `i2o/o2i_SCT(_LIST)`
  - `X509_get_ext_d2i` 作为 `X509_get_SCT_LIST` helper 的底层依赖
- `test_p2_srp.lpr` 的运行失败也不是 loader defect，而是 test contract 老化：
  - `src/fafafa.ssl.openssl.api.srp.pas` 明确把 `SRP_user_pwd_set_salt` / `SRP_user_pwd_set_verifier` / `SRP_user_pwd_get0_*` 标成 `Required: False`
  - `tests/crypto/test_p2_srp.pas` 与 `tests/crypto/test_p2_srp_comprehensive.pas` 也已经接受它们在 OpenSSL 3.x 下缺失
  - 只有 standalone `.lpr` 还在把这组符号当成必需项，制造假阴性失败
- `test_mock.lpr` 失败被确认是 compile-contract issue，不是 source drift：
  - 补上 `-Fu./tests/mocks` 后即可编译
  - 这说明问题在 test entrypoint/search-path contract，而不是 mock 源码 API 老化
- 基于源码检查的下一条高价值风险点：
  - `src/fafafa.ssl.openssl.api.srp.pas` 里的 `SRPCreateUser(...)` 仍会无保护地调用 `SRP_user_pwd_set_salt` / `SRP_user_pwd_set_verifier`
  - 由于这两个符号已被 loader 视为 optional，这里很可能仍有 OpenSSL 3.x nil-function-call 风险
  - 这是来自源码检查的推断，下一批应先补 focused regression 再改 runtime contract

## 2026-03-20 Findings (Context builder OCSP disable normalization and integration smoke refresh)
- 这批的真实根因比“`Override(...)` 少映射两个字段”更深一层：
  - `Override('ocsp_stapling_enabled', 'false')` 最开始当然无效
  - 但就算改成复用 `.WithOCSPStapling(False)`，也仍然会失败
  - 因为 `WithOCSPStapling(False)` 本身没有先清 `ssoRequireOCSPStapling`，`SyncOCSPStaplingOptions` 会把 stale required state 重新拉回 enabled。
- 这说明 OCSP 这组字段不是普通 boolean，而是一个有依赖关系的 option-coupled state pair：
  - `required=true` 必然带起 `enabled=true`
  - `enabled=false` 也必须反向清掉 `required`
  否则 builder state 会继续受历史 `FOptions` 污染。
- 最小正确修法因此不是只在 `Override(...)` 做特判，而是统一到同一条状态机上：
  - `Override(...)` 走 fluent OCSP path
  - `WithOCSPStapling(False)` 在 sync 前显式清掉 stale required state
- 相邻验证还暴露了一条独立但高价值的测试老化：
  - `tests/test_ocsp_stapling_integration.lpr` 仍在使用过时 API：`Count`、`ToJSON`、`FromJSON`
  - 还夹带当前编译配置不接受的块内 `var`
  - 这类独立 smoke/integration 程序如果不及时刷新，会让辅助覆盖面逐步失效，即使核心 test suites 仍然是绿的。
- 下一批最值当的连续工作不该只盯更多 override 字段，而是：
  - 继续审其他 option-coupled disable path 是否也会被 stale option/state 反向污染
  - 顺手清理独立 smoke/integration tests 的 API 老化，恢复它们作为廉价回归探针的价值

## 2026-03-20 Findings (Context builder override explicit backend parity)
- `Override(...)` 在 cert/key / PKCS#11 / validation 相关字段逐步补齐之后，仍留着一条更高层的 runtime-significant drift：
  - builder/export/import/merge/clone 已经把 explicit backend pin 当作正式 state
  - override/transform surface 却还不能表达同一合同
- 这条 drift 的危险点不只是“少支持一个字段”，而是会让 stale auto-backend state 继续压住调用方的新意图：
  - 调用方先通过 `RequirePKCS11Support` 一类 API 进入 auto-select mode
  - 再用 override pipeline 显式指定 backend
  - 结果 builder 仍继续按旧 auto-selection 失败，形成 silent no-op
- 最小正确修法不需要改 import/export/merge，也不需要引入 structured requirements parser：
  - `Override('explicit_backend', ...)` 识别 backend 名和值
  - 赋值时做和 `.WithBackend(...)` 一样的状态切换：
    - set `FExplicitBackend`
    - set `FExplicitBackendSet := True`
    - clear `FAutoSelectBackend`
- 这说明 backend selection 也属于同一个“external mutation surface 必须是 self-contained contract”的问题族，而不只是 cert/key source selection 的特例。
- 下一批更值得审的是剩余简单 scalar/boolean override gap，而不是马上去扩结构化 `backend_requirements` string parser：
  - `ocsp_stapling_enabled`
  - `ocsp_stapling_required`
  - `auto_select_backend` / `backend_requirements` 则应先做设计边界判断

## 2026-03-20 Findings (Context builder override PEM parity)
- `Override(...)` 在经过前两批修复后，cert/key source selection 还留着一个明显但更窄的 API-surface 缺口：
  - 已支持 `certificate_file` / `private_key_file`
  - 但仍然忽略 `certificate_pem` / `private_key_pem`
- 这条缺口不是单纯“少支持两个字段”这么简单，因为它会重新引入 history dependence：
  - 调用方先有 stale missing-file state
  - 再通过 override pipeline 提供有效 PEM
  - 结果 builder 仍继续按旧 file path 失败，因为 PEM override 根本没有生效
- 最小正确修法不需要再碰 build precedence，也不需要扩密码/secret 面：
  - `Override('certificate_pem', ...)` 赋值并清 `certificate_file`
  - `Override('private_key_pem', ...)` 赋值并清 `private_key_file`
- 修完后，certificate/private-key source selection 在当前主要 external mutation surface 上终于基本对齐：
  - fluent setter
  - import
  - override
  - merge
  - apply-preset
- 下一批不该再围绕同一组 file/PEM precedence 反复补边，而应转去更高层或更窄的 external-config 合同审计。

## 2026-03-20 Findings (Context builder merge/apply-preset file clears stale PEM state)
- `Merge(...)` 之前的 cert/key 行为本质上是“把 source 的 non-empty field 累加到 destination”，而不是“把 source 当成 self-contained source-selection state”：
  - merged `certificate_file` 只会覆盖 `FCertificateFile`
  - merged `certificate_pem` 只会覆盖 `FCertificatePEM`
  - `private_key_*` 同理
  结果是 destination 上旧的对侧 state 会继续残留。
- 这类残留不只是导出状态脏：
  - certificate 仍然是 `PEM > file`
  - private key 仍然是 `PKCS#11 > PEM > file`
  所以 merge file 到一个已有 PEM 的 builder 时，runtime 仍可能继续吃旧 PEM，形成 history-dependent build 结果。
- `ApplyPreset(...)` 没有独立逻辑，只是直接调用 `Merge(...)`，因此它不是另一条独立 bug，而是 merge drift 的继承面。
- 最小正确修法因此只需要落在 `Merge(...)`：
  - source 非空 `certificate_file` -> 赋值并清 `certificate_pem`
  - source 非空 `certificate_pem` -> 赋值并清 `certificate_file`
  - source 非空 `private_key_file` -> 赋值并清 `private_key_pem`
  - source 非空 `private_key_pem` -> 赋值并清 `private_key_file`
- 这样做之后，file/PEM mutual-exclusion contract 终于在所有已支持的主 mutation surface 上对齐了：
  - fluent setter
  - import
  - override
  - merge
  - apply-preset
- 下一条队列不该再回到这组已闭环的 file/PEM precedence，而应转向更窄的 API-surface gap 审计，例如：
  - `Override(...)` 是否应单独支持 `certificate_pem` / `private_key_pem`
  - 或是否还存在别的 merge-like surface 仍把 mutually exclusive state 当 additive field 处理

## 2026-03-20 Findings (Context builder override file clears stale PEM state)
- `Override(...)` 在这条线上暴露的不是 parser 缺字段，而是和 import 同一类的 state-mutation drift：
  - `Override('certificate_file', ...)`
  - `Override('private_key_file', ...)`
  之前都只改 file field，本身不会清掉对应的 stale PEM state。
- 由于当前 runtime precedence 仍然是：
  - certificate: `PEM > file`
  - private key: `PKCS#11 > PEM > file`
  所以“已经 override 到 file”在旧 PEM 还残留时其实并不会真正生效，build 结果依然取决于历史状态。
- 最小正确修法不是再碰 build precedence，也不是在这一批扩 `certificate_pem` / `private_key_pem` 的 override 面：
  - override `certificate_file` 时清掉 `certificate_pem`
  - override `private_key_file` 时清掉 `private_key_pem`
- 这说明 import / override / merge 并不是三条独立小 bug，而是同一个 builder-state 合同在不同 mutation surface 上逐步漂移：
  - fluent setter 已经保持 file/PEM 互斥
  - import 和 override 现在已对齐
  - 剩下最高价值缺口自然落在 `Merge(...)`
  - `ApplyPreset(...)` 又直接复用 `Merge(...)`，因此会继承同一问题
- 下一批应继续保持最小范围：
  - 只补 `Merge(...)` / `ApplyPreset(...)` 的 file-vs-PEM mutual-exclusion parity
  - 不顺手扩 `Override('certificate_pem' / 'private_key_pem')`
  - 不改当前 build precedence

## 2026-03-20 Findings (Context builder import file clears stale PEM state)
- 这批确认的真实缺陷不是 build precedence 又退回去了，而是 import assignment 语义落后于 fluent setter：
  - `WithCertificate(...)` / `WithPrivateKey(...)` 本来就会清理对应 PEM state
  - 但 `ImportFromJSON` / `ImportFromINI` 之前只是把 file field 覆盖进去，不会清理 stale PEM
- 因为当前 runtime precedence 已经是：
  - certificate: `PEM > file`
  - private key: `PKCS#11 > PEM > file`
  所以“导入了 file”这件事在 stale PEM 存在时并不会真正生效，结果仍然取决于 builder 历史状态。
- 最小正确修法不是再改 build precedence，而是让 import 路径和 fluent setter 对齐：
  - imported `certificate_file` 必须清掉 `certificate_pem`
  - imported `private_key_file` 必须清掉 `private_key_pem`
- JSON 侧还需要同步收口另一半互斥合同：
  - imported `certificate_pem` 清掉 stale `certificate_file`
  - imported `private_key_pem` 清掉 stale `private_key_file`
  虽然 runtime 本来就偏向 PEM，但如果不清，导出状态仍然自相矛盾，后续 merge / transform 也会继续带着脏状态传播。
- 修完之后，manual import 的 source selection 重新变成 self-contained contract，而不是 merge-like 累加语义。
- 同时，这批源码复核也给出了下一条最明确的连续队列：
  - `Override(...)` 对 `certificate_file` / `private_key_file` 仍然不会清 stale PEM
  - `Merge(...)` 仍然把 file/PEM 当作独立 non-empty fields 合并
  这两处和本批 root cause 是同一类 drift，优先级应高于更泛的序列化面讨论。

## 2026-03-20 Findings (Context builder PKCS#11 direct PIN import default)
- 在 fluent / override 路径已经把 `pkcs11_pin` 默认绑定到 `pmValue` 之后，manual import surface 还留着一条语义裂缝：
  - `ImportFromJSON`
  - `ImportFromINI`
  当 `pkcs11_pin_method` 缺席时，只导入 `pkcs11_pin` 值，却不会把 method 归一到 `pmValue`
- 这使 import 结果带上了不该有的 history dependence：
  - fresh builder 上它可能保持 `pmNone`
  - 带 stale `pmEnvironment` / `pmFile` state 的 builder 上，它会继续沿用旧 method
  - 同一个输入 payload 因 builder 旧状态不同而跑出不同 PIN-source 语义
- 最小正确修法不是改 export，也不是改 runtime load：
  - 只要 import payload 含 `pkcs11_pin`，且这次输入没有显式 `pkcs11_pin_method`
  - 就把导入结果归一到 direct PIN 的 `pmValue`
- 这样能把 manual import 重新拉回与 fluent / override 一致的合同：
  - `pkcs11_pin`-only 表达 direct PIN
  - env/file 仍然只有在 method 明确给出时才成立
- 后续 probe 也排除了一个看起来像新缺口的方向：
  - `pkcs11_pin_method=pmBogus` 现在不会单独落成 `pmNone`
  - 因为只要同一 payload 里有 `pkcs11_pin`，它就和 `pkcs11_pin`-only import 一样回到 `pmValue`
  - 所以下一条高价值队列不再是专门给 `pmBogus` 补 guard，而是继续审计更广义的 import history dependence

## 2026-03-20 Findings (Context builder PKCS#11 PIN order sensitivity contract)
- 在 named import / override parity 已经补齐之后，真正剩下的高价值缺口不是 parser，而是 setter ordering：
  - `WithPKCS11PIN(...)`
  - `Override('pkcs11_pin', ...)`
  之前都会无条件把 `FPKCS11PINMethod` 改回 `pmValue`
- 这会造成新的 silent misconfiguration：
  - 调用方先显式选择 `pmEnvironment` / `pmFile`
  - 再设置 env name / file path 作为 source value
  - builder state 却悄悄退化成 direct PIN，直到 runtime/export 行为才暴露漂移
- 最小正确修法不是新增更多 parser，也不是改 validation wording：
  - 只在“调用方从未显式选过非 direct-PIN method”时，PIN setter 才默认回落到 `pmValue`
  - 一旦 method 已被显式设成非 `pmValue`，后续 source value 赋值只能更新 value，不能重写 method
- 当前把 `pmCallback` / `pmInteractive` 也纳入“显式非 value method”保留集合是合理的：
  - runtime support 与 state preservation 是两层不同合同
  - setter 顺序不应把 unsupported method 偷偷伪装成 supported direct-PIN state
- 因而下一条值得锁定的不是再修 parser，而是决定是否补一条更直接的 regression：
  - 明确显式 unsupported method 也应该保序
  - 然后继续扫剩余 builder-state precedence / merge drift

## 2026-03-20 Findings (Context builder PKCS#11 PIN method named import values)
- 当前 builder state contract 其实已经支持 `pmEnvironment` / `pmFile`，而且 round-trip 也已经闭环，但 human-authored import surface 还存在一条明显脆弱点：
  - JSON import 对 `pkcs11_pin_method: "pmEnvironment"` 直接抛 invalid integer
  - INI import 对 `pkcs11_pin_method=pmFile` 会静默退回 `pmNone`
- 这类 drift 的危险点仍然是“文本配置看起来合理，但语义没有真正生效”：
  - JSON path 直接崩
  - INI path 更糟，是 silent fallback
- 最小正确修法不是改 export 格式，也不是新增文档格式分支：
  - 让 `ImportFromJSON` / `ImportFromINI` 复用同一个 tolerant PKCS#11 PIN-method parser
  - 这样 named value 和 ordinal 都能走到同一条 builder state 语义
- 这批也明确了下一条真正需要单独决策的边界：
  - `pkcs11_pin` 与 `pkcs11_pin_method` 的调用顺序敏感性
  - 这和 import parser 不同，涉及 direct-PIN 兼容语义，不应在这一批里顺手悄悄改变

## 2026-03-20 Findings (Context builder Override PKCS#11 PIN method parity)
- `Override(...)` 之前虽然已经补上了 `pkcs11_uri` 和 `pkcs11_pin`，但 external-config parity 仍然少最后一块关键状态：
  - `pkcs11_pin_method`
- 这会造成一条新的 silent downgrade 路径：
  - 调用方通过 override/transform pipeline 设置 `pkcs11_pin`
  - 但无法把 method 从默认 direct PIN 语义切到 env/file source
  - 结果 builder runtime 会把 source 名称当 direct PIN 使用，直到更后面的 backend/key-load 路径才暴露错误
- 最小正确修法不是改 validation wording，也不是重做 transform API：
  - 只让 `Override(...)` 识别 `pkcs11_pin_method`
  - 复用一个宽松 parser，接受 case-insensitive enum 名和 ordinal
  - invalid value 继续 no-op，保持 override 的 defensive contract
- 这批刻意没有顺手改变另一条独立语义：
  - `Override('pkcs11_pin', ...)` 仍然会把 method 设回 `pmValue`
  - 因而 override/fluent path 的“先 method 后 pin”顺序敏感性仍然存在
  - 这应该作为下一条单独合同审计，而不是在本批里静默改变 direct-PIN 兼容语义
- 下一条高价值队列因此也更清楚：
  - 评估 `pkcs11_pin` / `pkcs11_pin_method` 的顺序敏感性是否仍值得修
  - 然后再看 human-authored text/INI surfaces 是否需要接收命名值而不是只吃 ordinal

## 2026-03-20 Findings (PKCS#11 docs builder guidance contract)
- 这条 docs drift 已经不只是一次性的改文案问题，而是出现了两种重复风险：
  - builder PIN source guidance 曾经落后于 runtime
  - architecture reference 里的 backend interface/class 名字也曾回到旧 API
- 对这种问题，轻量 pattern-based shell contract 是合适的：
  - 这里要锁的是稳定的 API 名字和 guidance marker
  - 没必要把它升级成更重的 Markdown lint 或 snippet compile 流程
- 测试需要同时覆盖 positive/negative space：
  - 正向约束：env/file builder 示例、lower-level callback guidance、当前 backend interface/class 名仍在
  - 反向约束：`.WithPKCS11Key(...)`、`.ForServer`、`GetBackendType`、`GetLastError` 等旧名不能再回来
- 因而这批的价值不在新增功能，而在把刚修完的文档合同变成可执行 guard。
- 下一条高价值工作仍应回到 builder state/runtime 审计，而不是继续停留在 docs polishing：
  - `pkcs11_pin_method` export/import/merge
  - callback/interactive builder design decision

## 2026-03-20 Findings (Context builder PKCS#11 docs contract alignment)
- 前两批 builder 修复已经改变了真实用户合同，但 PKCS#11 文档没有同步收口：
  - guide 只展示 direct PIN builder path，没有告诉读者 env/file source 已经可用
  - guide 把 callback 示例放在同一条叙述线上，却没有明确 callback/interactive 仍是 lower-level backend path
- callback 示例还有第二个准确性问题：
  - 当前 `TPKCS11PINCallback` 是 `function(...) of object`
  - 旧示例却用 free function，和真实类型面不一致
- 最小正确修法仍然是 docs-only，而不是继续扩实现：
  - 明确 builder 支持 `pmValue` / `pmEnvironment` / `pmFile`
  - 明确 `pmCallback` / `pmInteractive` 属于 `TPKCS11Config` + backend
  - 把 callback 示例改成 object-bound callback + backend factory
- `docs/reference/PKCS11_ARCHITECTURE.md` 也需要同步加一句 boundary note：
  - interface 暴露 `WithPKCS11PINMethod(...)` 并不意味着所有 `TPKCS11PINMethod` 都能经由 builder runtime 执行
- 同一份 architecture reference 里还混着更老的一层 API 漂移：
  - `IPKCS11Backend` 片段仍写着已不存在的 `GetBackendType` / `GetLastError`
  - backend 类名也还停留在 `TPKCS11ProviderBackend` / `TPKCS11EngineBackend`
  - 如果不一起修，新的 builder boundary 说明会挂在旧 backend API 示例旁边，继续误导读者
- 下一条高价值队列因此更清楚：
  - 如果担心再次漂移，可以补轻量 docs contract check
  - 但 callback/interactive builder support 与 `pkcs11_pin_method` serialization 仍应保持为独立设计问题

## 2026-03-20 Findings (Context builder PKCS#11 PIN source support)
- 上一批 runtime guard 收口后，下一条真实高价值问题变得更清楚：
  - builder 把 `pmEnvironment` / `pmFile` 和 `pmCallback` / `pmInteractive` 一起拒绝
  - 但底层 `TPKCS11PINManager` 其实早就支持 env/file source
  - 这让 builder API 支持面比现有底层能力更窄，属于不必要的 contract gap
- 最小正确修法不是新增 URI 变异层，也不是立刻设计 callback 注入：
  - 直接在 builder 里复用 `TPKCS11PINManager.GetPIN(...)`
  - 把 env/file source 解析成 direct PIN
  - 然后继续走现有 PKCS#11 key-load path
- 这样做的收益是确定且可验证的：
  - validation 可以放行非空 env/file source
  - `TryBuildServer` 能在缺失 env var / file 时给出 deterministic source-resolution error
  - 不依赖真实 token 或完整 PKCS#11 runtime 才能看见 builder 已经走到正确分支
- 同时也保住了边界：
  - `pmCallback` / `pmInteractive` 仍然 unsupported
  - callback 生命周期和线程模型没有被偷偷塞进 builder
- 因而下一条高价值队列也更明确：
  - 如果要支持 callback/interactive，必须作为显式设计工作来做
  - 文档也需要同步，把 builder 已支持的 env/file path 与仍然 lower-level 的 callback path 分开讲清楚

## 2026-03-20 Findings (Context builder PKCS#11 PIN method runtime guard)
- `TSSLContextBuilder` 之前暴露了一个比 runtime 真正能力更大的 PKCS#11 PIN API 面：
  - `FPKCS11PINMethod` 会被 builder 保存、clone、reset
  - 但 `BuildClient` / `BuildServer` 之前始终只把 `FPKCS11PIN` 当 direct PIN 传给 `LoadPrivateKey(...)`
  - `pmEnvironment` / `pmFile` / `pmCallback` / `pmInteractive` 因而都只是“看起来支持”
- 这条 drift 的危险点同样是 silent misconfiguration：
  - 调用方以为 builder 会按指定 method 获取 PIN
  - 实际 runtime 要么把 source 名称当明文 PIN 用，要么落到更晚的 backend/file failure
  - validation 之前还会把这类配置当作有效 PKCS#11 state 放过去
- 最小正确修法不是在这批里半实现 callback/file/env plumbing：
  - 先把 builder 支持面收紧到当前真实可执行的 `pmNone` / `pmValue`
  - validation 对 unsupported methods 直接报错
  - build path 也在进入 backend load 前抛 configuration error
- 同时把 direct PIN override 语义收口：
  - 只有 `pmValue` 才会把 `FPKCS11PIN` 传给 `LoadPrivateKey(...)`
  - `pmNone` 不再隐式携带 stale direct PIN
- 下一条高价值工作因此更清楚：
  - 如果要支持 env/file/callback，应做成显式 builder/runtime 设计
  - 例如 URI `pin-source` 组合或 callback 注入
  - 在此之前，不应先扩 `pkcs11_pin_method` 的导出/序列化面

## 2026-03-20 Findings (Context builder PKCS#11 URI serialization and merge contract)
- `TSSLContextBuilder` 的 PKCS#11 state surface 之前是不一致的：
  - `Clone` 直接复制 `FPKCS11URI`
  - `BuildServer` / `ValidateServer` 也会消费它
  - 但 JSON/INI export/import 和 `Merge(...)` 都不会保留它
- 因为 `Merge(...)` 本身就是基于 `ExportToJSON` 做 field-surface merge，这个遗漏会一次性打断三条路径：
  - JSON round-trip
  - INI round-trip
  - merge from source builder
- 这类缺陷的危险点仍然是 silent state loss：
  - 源 builder 是有效 server config
  - round-trip / merge 后没有崩溃
  - 但 validation contract 已经退化成“缺私钥”
- 最小正确修法是只扩 non-secret state：
  - `pkcs11_uri` 加入 JSON / INI export/import
  - `Merge(...)` 恢复它
  - 明确不把 `pkcs11_pin` 拉进导出面，继续保持与 `private_key_password` 类似的敏感信息边界
- 这批还暴露了一个需要单独决策的旁支：
  - `certificate_pem` 当前并不在 INI 面 round-trip
  - 但那是“INI 是否应该承载 PEM blob”的合同问题，不应和 `pkcs11_uri` 非敏感状态丢失混为一谈
- 因而下一条高价值工作应当是安全边界明确的 builder-state 审计：
  - 判断 `pkcs11_pin_method` 是否属于可安全序列化的非敏感状态
  - 分开评估 INI/JSON 是否需要继续扩展 PEM / secret-adjacent surfaces

## 2026-03-20 Findings (Context builder Override validation parity)
- `TSSLContextBuilder.Override(...)` 的真实支持面落后于 builder/validation 合同：
  - `ValidateClient` 会读取 `FUseSystemRoots`
  - `ValidateServer` / `BuildServer` 会读取 `FPKCS11URI`
  - 但 `Override(...)` 之前只支持一小组 legacy 字段，导致 override-based 配置被静默丢弃
- 这个漂移的危险点不在崩溃，而在 silent misconfiguration：
  - 调用方以为自己通过 transform/override pipeline 打开了系统根证书或 PKCS#11 私钥
  - validation/runtime 却继续按旧状态执行
- 最小正确修法不是改 validation，也不是重做 transform API：
  - 只把 `Override(...)` 的字段分发补齐到当前有直接合同证据的字段
  - 让它和 `.WithSystemRoots` / `.UsePKCS11(...)` 对齐
- 这批顺手把 `pkcs11_pin` 也纳入同一路径，并在 override 赋值时同步 `FPKCS11PINMethod := pmValue`，以保持与 `.WithPKCS11PIN(...)` 一致。
- 但更大的 builder state drift 还没有完全收口：
  - `FPKCS11URI` / `FPKCS11PIN` / `FPKCS11PINMethod` 仍未出现在当前 `ExportToJSON` / `ImportFromJSON` / `ExportToText` / `ImportFromText` 面上
  - 因而下一批高价值工作不该停在 override，而应转去 builder serialization / copy contract
  - 特别是 `pkcs11_pin_method` 目前仍缺少外部可观测面，不能假设它已经真正闭环

## 2026-03-20 Findings (Context builder server ServerName runtime consistency)
- 用隔离 `-FU/-FE` 目录重跑后，原本 lingering 的 `ServerName` precedence / validation 队列已经可以明确收口：
  - `test_connection_builder_hostname_precedence` => PASS
  - `test_tls_connector_hostname_override_precedence` => PASS
  - `test_freepascal_context_server_name_inheritance` => PASS
  - `test_config_validation` => PASS
- 这说明此前 builder precedence 那条“还没彻底完”的信号，已经不是产品行为缺陷，而是旧共享 `tmp/` 产物污染导致的验证配方债务。
- 在此基础上暴露出的下一条真实语义裂缝更具体：
  - `TSSLContextBuilder.BuildClient` 会把 `.WithSNI(...)` 落到 `Result.SetServerName(...)`
  - `TSSLFactory.CreateContext(const AConfig)` 也会应用 `ServerName`
  - 但 `TSSLContextBuilder.BuildServer` 之前完全跳过 `FServerName`
- 这会造成一个不一致的 runtime surface：
  - builder 记录了 `FServerName`
  - validation 也会因为 `.WithSNI(...)` 给出 deprecated warning
  - 但 server build path 却把这个 legacy context field 静默丢掉
- 最小正确修法不是改 warning，也不是扩大到 API slimming：
  - 只在 `BuildServer` 补上 `Result.SetServerName(FServerName)`
  - 让它和现有 `BuildClient` / factory config path 对齐
  - 保持证书、私钥、ALPN 与 validation 文案不变
- 修完后，这条线的下一批高价值工作更清楚了：
  - 继续审计 `context/default-validation` 里其他“配置面接受，但某一条 runtime build 路径静默丢弃”的字段
  - 而不是继续围绕旧 precedence 失败记录反复打转

## 2026-03-20 Findings (Post-SNI verification sweep)
- 在完成 WinSSL/MbedTLS 的 normal-flow 清理和 framework/API-surface 标注之后：
  - `python3 scripts/compile_all_modules.py` => 181/181 成功
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
- 这说明当前这条连续修复没有把问题从测试层带回核心模块或最小 CI 门禁。
- 同时也给出一个清晰的队列切换信号：
  - 继续按字符串扫 `SetServerName(...)` 的收益已经明显下降
  - 下一条高价值工作更适合回到 `ServerName` precedence / default-validation 等更高层语义合同

## 2026-03-20 Findings (Backend framework context SNI labeling)
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
  这两个文件都属于 backend framework/context configuration coverage，而不是 live client-flow guidance。
- 它们的共同点是：
  - 在 context 配置测试里直接 `SetServerName(...)` / `GetServerName(...)`
  - 重点是后端 context API 是否工作
  - 不是在表达“推荐怎样写握手代码”
- 因此这批正确动作不是迁移到 connection-level SNI，而是显式标注：
  - `INTENTIONAL_API_SURFACE: context-level SNI setter coverage`

## 2026-03-20 Findings (MbedTLS server accept SNI cleanup)
- `tests/mbedtls/test_mbedtls_server_accept.pas`
- `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
  整体主题虽然是 server-side accept/handshake，但旧的 `SetServerName(...)` 命中都出现在本地 client half 上。
- 这类场景的分类要点是：
  - 测试目标是 server accept
  - 但 client half 仍然是一次真实 TLS client handshake
  - 因而 context-level hostname 仍然属于 stale connection-flow guidance
- 最小正确修法是：
  - 保留 client context 的 `SetVerifyMode([])`
  - 只把 hostname 设置点挪到 `CreateConnection(...)` 之后的 `ISSLClientConnection.SetServerName(...)`
- 这批验证还顺手确认了一个通用配方细节：
  - 这些 server-accept tests 编译时需要 `-Fu./examples`
  - 若复用已有产物目录，可能吃到跨目标污染；隔离 `-FU/-FE` 目录更稳

## 2026-03-20 Findings (WinSSL comprehensive test compile drift)
- `test_winssl_context_comprehensive` 暴露的是单点接口迁移：
  - `ISSLContext.GetNativeHandle` 已经从核心接口移除
  - 当前正确入口是可选接口 `ISSLNativeHandleAccess`
- `tests/unit/test_winssl_comprehensive.pas` 则是一组更老的 API 漂移叠加：
  - `IsAvailable` 已不存在
  - `GetCertificateCount` 已收敛为 `GetCount`
  - `TSSLVerifyCallback` / `TSSLPasswordCallback` 现在是 `of object` 且签名已变化
  - `TSSLOptions` 与 option flags 来自 `fafafa.ssl.base`
- 这些问题的共同点是：
  - 都属于 test-only API alignment
  - 不需要改变 WinSSL 行为或测试意图
  - 最小修法就是把测试重新对齐到当前公共接口

## 2026-03-20 Findings (WinSSL comprehensive context SNI labeling)
- `tests/winssl/test_winssl_context_comprehensive.pas`
- `tests/winssl/test_winssl_unit_comprehensive.pas`
- `tests/unit/test_winssl_comprehensive.pas`
  这 3 个文件都不是 live socket/handshake guidance，而是 WinSSL context API 的综合 setter/getter coverage。
- 它们的共同点很明确：
  - `SetServerName(...)` 后立刻读 `GetServerName(...)`
  - 不围绕该调用建立真实 TCP/TLS 连接
  - 重点是 `ISSLContext` 配置面是否可用，而不是推荐客户端握手写法
- 因此这批正确动作不是迁移到 connection-level SNI，而是显式标注：
  - `INTENTIONAL_API_SURFACE: context-level SNI setter coverage`
- 这样后续审查时，WinSSL comprehensive 文件就不会再被误当成 stale normal-flow。

## 2026-03-20 Findings (WinSSL integration multi SNI cleanup)
- `tests/winssl/test_winssl_integration_multi.pas` 属于典型多主机真实 client/integration flow：
  - 连接 Google / GitHub / Cloudflare / Microsoft
  - 协议协商
  - 数据传输大小
  - 错误端口/过时协议失败
  - 多次连续连接稳定性
- 即使其中包含负向握手，它依然不是 context fallback coverage，而是在跑真实 TCP/TLS 流程。
- 因此最小正确修法是：
  - 保留 context 上的协议/验证模式配置
  - 仅把 hostname 设置点挪到 `CreateConnection(...)` 之后的 `ISSLClientConnection.SetServerName(...)`
- 做完这批后，WinSSL 侧剩余未分类命中明显收敛到两类：
  - 已显式标注的 compatibility/API-surface coverage
  - 仍待分类的 framework/server-side 文件

## 2026-03-20 Findings (WinSSL performance and backend comparison SNI cleanup)
- `tests/winssl/test_winssl_performance.pas`
- `tests/winssl/test_winssl_session_reuse_benchmark.pas`
- `tests/integration/test_backend_comparison.pas`
  这 3 个文件都属于“真实连接流 + 性能/对比观测”，而不是 compatibility coverage。
- 即使其中包含负向握手或性能观测，分类仍然成立：
  - `test_winssl_performance` 在做真实握手、传输和连续连接吞吐统计
  - `test_winssl_session_reuse_benchmark` 在做会话复用前后对比
  - `test_backend_comparison` 在比较 WinSSL/OpenSSL 的握手、传输、证书和错误路径
  它们都不是在锁定 context-level SNI fallback 本身。
- 这批还有两个验证层面的细节值得记录：
  - session reuse 路径的正确顺序应保持为 `CreateConnection -> SetSession -> SetServerName -> Connect`
  - `backend_comparison` 的 Win64 验证如果复用 Linux 产物目录，会吃到错误目标的 `ppu`，需要用独立 `-FU/-FE` 目录隔离
- focused verification 还顺手暴露了一个旧 API 漂移：
  - `LConn.IsSessionResumed`
  - 当前接口真实存在的是 `IsSessionReused`
  这属于 pre-existing drift，不是本轮 SNI 迁移引入的问题。
- 当前剩余命中里，下一批最高价值的 normal-flow 候选已经明显收敛到 `tests/winssl/test_winssl_integration_multi.pas`；而 `test_winssl_context_comprehensive` / `test_winssl_unit_comprehensive` 更像应当被标注保留的 API-surface coverage。

## 2026-03-20 Findings (WinSSL cross-target compile drift)
- `fafafa.ssl.secure.compare` shim 补上后，Win64 编译链继续暴露出一串共享 WinSSL 实现层错误，但它们本质上是同一类“接口/命名漂移”，不是新的架构问题。
- 这批高价值 blocker 包括：
  - `GetVerifyCallback` 被当成带参数的方法直接调用，而不是先取到回调值再执行
  - `AcceptSecurityContext` 与 `AcceptSecurityContextW` 命名不一致
  - `TWinSSLLibrary` 在 `winssl.connection` implementation 中不可见
  - `TryGetNativeHandle(..., nil)` 违反 `out` 参数规则
  - `winssl.lib` 构造函数体在 `end;` 后还残留初始化语句
  - `RaiseSSLInitError` helper 名字从 OpenSSL 路径漂进了 WinSSL lib
- 这些问题的共同点是：
  - 都能通过极小的语义保持型修复解决
  - 不需要改变握手逻辑、验证逻辑或统计逻辑
- 修完后，前一批改过的三个 WinSSL 在线测试都能稳定通过 Win64 cross-compile，说明这条共享编译链已经恢复，可以继续支撑后续 WinSSL guidance sweep。

## 2026-03-20 Findings (WinSSL secure compare shim)
- Win64 cross-compile 暴露的第一条真实阻塞不是本轮 SNI 改动，而是一个更基础的源代码缺口：
  - `src/fafafa.ssl.winssl.certstore.pas` 明确 `uses fafafa.ssl.secure.compare`
  - 但仓库里根本不存在这个单元
- 这条缺口的意图其实已经写在注释里了：
  - 想要一个“独立的常量时间比较模块”
  - 避免把 WinSSL certstore 重新绑回 OpenSSL-backed `fafafa.ssl.secure`
- 当前唯一实际消费者就是 WinSSL certstore 的指纹查找路径：
  - `SecureCompareStrings(FP_SHA256, SearchFP)`
  - `SecureCompareStrings(FP_SHA1, SearchFP)`
  所以最小正确修法不是大搬运 `fafafa.ssl.secure`，而是补一个小 shim：
  - `SecureCompare`
  - `SecureCompareStrings`
  - 都委托到 `fafafa.ssl.crypto.constant_time.TConstantTime`
- 修完之后，Win64 编译链确实继续往后推进到了 `src/fafafa.ssl.winssl.connection.pas` 的另一组共享错误，说明这个 shim 不是“装样子”，而是切实移除了第一道 compile blocker。
- 因而下一批不该再回头碰 shim，而应直扑 `winssl.connection` 的 shared compile drift。

## 2026-03-20 Findings (WinSSL online-flow SNI guidance cleanup)
- `tests/winssl/test_winssl_hostname_mismatch_online.pas`
- `tests/winssl/test_winssl_alpn_sni.pas`
- `tests/winssl/test_winssl_session_resumption.pas`
  这 3 个文件重新分类后都属于“真实在线 client flow”，而不是 intentional compatibility coverage。
- `Hostname Mismatch` 和 `Session Resumption Baseline` 虽然分别观察负向结果与计时结果，但它们的测试主题仍然是：
  - 用真实 socket 建立 TLS 连接
  - 握手后看验证/协商/耗时表现
  而不是锁定 context-level hostname fallback 本身。
- 这批 RED 还暴露了一个容易漏掉的审查细节：
  - 只抓 `Ctx.SetServerName(...)` 的正则不够
  - `Ctx1` / `Ctx2` 这种编号变量同样会藏住旧指导
- 迁移后，3 个文件都统一回到：
  - `CreateConnection(...)`
  - `(Conn as ISSLClientConnection).SetServerName(...)`
  - `Connect`
- 验证层面的重要结论是：
  - contract/grep 已经证明这些文件本身不再残留 context-level SNI
  - native Linux compile 失败是 WinSSL 平台边界，不是本轮修改问题
  - Win64 cross-compile 的失败也一致收敛到 `src/fafafa.ssl.winssl.connection.pas` 的共享错误，而不是这 3 个测试文件各自的新错误
- 因此这批可以判定为“指导清理完成”，但 WinSSL 编译面还需要单独根修复。

## 2026-03-20 Findings (MbedTLS online verification SNI guidance cleanup)
- `tests/mbedtls/test_mbedtls_cert_chain.pas`
- `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
- `tests/mbedtls/test_mbedtls_cert_errors.pas`
- `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
  这 4 个文件重新分类后都属于“普通在线 client verification flow”，而不是 intentional compatibility coverage。
- 即使其中包含负向场景，这个分类仍然成立：
  - `Hostname Mismatch` 依赖的是“故意给错 hostname”
  - `Expired Certificate` / `No CA Verification` 依赖的是验证失败条件
  - `OCSP capability` 依赖的是一次真实握手后的能力观测
  它们都不需要借助 shared-context SNI 才能表达测试意图。
- 因此最小正确修法不是改断言，也不是删 context API：
  - 保持原有网络目标、验证模式、错误判断与 OCSP 检查
  - 只把 hostname 设置点统一挪到 `CreateConnection(...)` 之后的 `ISSLClientConnection.SetServerName(...)`
- 这批之后，MbedTLS 剩余活跃命中更清楚地分裂成两类：
  - 已经迁好的普通 client flow
  - 需要单独判断的 server-side / framework / backend-comparison / WinSSL online tests
- 下一轮最值得优先碰的，不再是继续深挖 MbedTLS，而是 WinSSL 在线/性能/会话类文件和 `tests/integration/test_backend_comparison.pas` 这种仍在“真实对比流”里使用 context-level SNI 的测试。

## 2026-03-20 Findings (OpenSSL CA autoload SNI guidance cleanup)
- `tests/openssl/test_openssl_ca_autoload.pas` 一开始看起来像“只是测试文件里的小残留”，但分类后更接近 stale guidance，而不是 intentional compatibility coverage：
  - 这个文件的主题是 CA auto-loading
  - 不是在验证 context-level SNI fallback
  - 其中两处 `LCtx.SetServerName(...)` 只是顺手沿用了旧 client setup 路径
- active 文档摘要里也还留着同一条旧口径：
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `8. ✅ SNI hostname properly set on context`
- 这类文件比 API-surface setter coverage 更应该清，因为它们在表达“正常客户端配置流程”，而不是“保留 deprecated API 行为”。
- 最小正确修法不需要真实网络：
  - 用 dummy socket 创建 connection
  - 在 `ISSLClientConnection` 上设置 `ServerName`
  - 通过 connection-level getter 断言
- 这样既保留了 CA auto-loading test 的无网络性质，又把 SNI 路径收回到当前项目口径。

## 2026-03-20 Findings (Core functionality Base64 API drift)
- 在 API-surface label batch 的 focused verification 里，`tests/examples/test_lib_core_functionality.pas` 暴露出一个与注释改动无关的真实失败：
  - `TCryptoUtils.Base64Encode`
  - `TCryptoUtils.Base64DecodeString`
  当前都不存在
- 这是典型的 API drift，不是新引入问题：
  - 代码库当前 Base64 helper 已经集中到 `TEncodingUtils`
  - `tests/crypto/benchmark_base64_performance.pas` 也保留了同样的旧调用
- 同一个 smoke test 在修完编译后还暴露出第二层旧夹具问题：
  - 它通过工厂拿 `sslOpenSSL`
  - 但 `uses` 里没导入 OpenSSL backend registration 单元
  - 所以运行时会报 `SSL backend OpenSSL is not registered`
- 最小正确修法因此分两步：
  - Base64 调用迁到 `TEncodingUtils`
  - `test_lib_core_functionality` 补 `fafafa.ssl.openssl.backed`
- 这批的价值不只是“顺手修一个例子”：
  - 它证明当前 examples/tests 里还存在 helper API 漂移与 backend registration drift 的组合问题
  - 后续扫 examples/benchmarks 时应优先复核这两类模式

## 2026-03-20 Findings (API-surface tests context-level SNI labeling)
- 当前剩余的一部分 active `SetServerName(...)` 命中并不属于 live connection flow，也不属于 compatibility contract，而是更窄的 API-surface / validation coverage：
  - `tests/examples/test_lib_core_functionality.pas`
  - `tests/diagnostic/test_error_handling.pas`
  - `tests/diagnostic/test_error_handling_comprehensive.pas`
  - `tests/security/test_memory_safety.pas`
  - `tests/security/test_input_validation.pas`
- 这些文件的共同点是：
  - 不创建真实 client connection 去握手
  - 只验证 context API 的 setter acceptance、no-error behavior、resource cleanup
  - 因而迁到 per-connection SNI 并不会更“正确”，反而会改变测试对象
- 但如果不显式标注，它们又会继续被误看成旧 guidance。
- 因此这批的正确动作不是清理旧 API，而是补说明：
  - 这里是 `ISSLContext.SetServerName(...)` 的 API-surface coverage
  - 不是推荐的新代码路径
- 这样一来，active 命中就不再只靠“有没有字符串”做判断，而是开始按测试意图分类。

## 2026-03-20 Findings (Compatibility tests context-level SNI labeling)
- 在上一批把 selected connection-flow tests 迁到 per-connection SNI 后，剩余 active 命中开始更集中地落在“故意保留”的合同测试上。
- 这些文件如果没有显式标识，后续审查很容易再次踩到同一个分类问题：
  - 看到 `Ctx.SetServerName(...)`
  - 误以为又是 stale guidance
  - 但实际上它们是在锁定 legacy fallback / precedence / cross-backend compatibility
- 这批高置信 intentional coverage 包括：
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
- 它们的共同点不是“用户应该这样写新代码”，而是：
  - 需要把 deprecated context-level SNI path 保持可观测
  - 用来验证 fallback/override/inheritance/error normalization 等兼容边界
- 因此这批最小正确修法不是继续迁移，也不是删 API，而是补元信息：
  - 统一加 `INTENTIONAL_COMPAT: legacy context-level SNI coverage`
  - 明确说明这里保留旧路径是出于 contract/compatibility 目的
- 这样做的价值在于把后续审查规则从“按字符串命中”升级为“按意图分类”：
  - stale guidance => 清理
  - intentional compatibility coverage => 标注并保留
- 剩余更模糊的命中现在值得优先复核的是：
  - example/demo/API-smoke 边界不够清楚的测试文件
  - 而不是重复碰已经被显式标注的兼容合同

## 2026-03-20 Findings (Connection-flow tests SNI guidance cleanup)
- 在 active docs 和高可见 examples 收口后，剩余 `SetServerName(...)` 命中大多集中在 tests。
- 这批继续分类后，发现不能把这些命中一刀切处理：
  - 有些文件是明确的 compatibility/precedence contracts，保留旧路径是有意的
  - 但也有一类文件只是“真实连接流测试”，并没有在验证 context fallback 本身
- 这批选中的 4 个文件都属于后者：
  - `tests/mbedtls/test_mbedtls_connection.pas`
  - `tests/mbedtls/test_mbedtls_simple_connection.pas`
  - `tests/integration/test_e2e_scenarios.pas`
  - `tests/integration/test_real_https_connection.pas`
- 它们共同特征是：
  - 主要目的在于验证真实 handshake / ALPN / session reuse / certificate / transfer
  - 并不是在锁定 `Ctx.SetServerName(...)` 的兼容语义
  - 却仍然把 context-level SNI 写在 live connection flow 里
- 这会带来一个新的 drift：
  - 项目主文档和高可见 examples 已经统一到 connection-level SNI
  - 但维护者继续阅读这些网络测试时，仍会看到过时的共享 context hostname 路径
- 最小正确修法不是改 runtime，也不是删掉 context API：
  - 只把 selected tests 的 SNI 步骤挪到 `CreateConnection(...)` 之后
  - 统一成 `(Conn as ISSLClientConnection).SetServerName(...)`
  - 保留原本的协议、ALPN、session、证书与传输断言
- 这批之后，剩余命中更清楚地分成两类待办：
  - intentional compatibility/API-surface coverage，需要显式标注其保留旧路径的原因
  - 仍然在测试/演示层误导人的 stale guidance，需要后续继续清理

## 2026-03-20 Findings (FreePascal context ServerName inheritance)
- 这轮继续审查 `BuildClient` 的 legacy context-level SNI compatibility boundary 时，发现它并不只是“要不要删兼容 API”的架构问题，而是有一条真实的跨后端语义漂移：
  - OpenSSL 连接构造器会把 `AContext.GetServerName` 继承到连接
  - MbedTLS 连接构造器也会继承
  - WolfSSL 连接构造器同样会继承
  - WinSSL 当前源码也已经继承
  - 只有 FreePascal 两个 connection 构造器把 `FServerName` 初始化为空，完全没有复制 context 默认值
- 这让同样的 legacy compatibility path 在不同 backend 上表现不一致：
  - `.WithSNI(...)` / `Ctx.SetServerName(...)` 在多数 backend 上还能通过“context default -> connection”继续生效
  - 但到 FreePascal backend 就会静默丢失
- focused RED 直接证明了这不是理论推断：
  - builder path：`WithBackend(sslFreePascal).WithSNI('ctx.example.com').BuildClient` 后创建 socket connection，`GetServerName=''`
  - direct context path：`Ctx.SetServerName('stream.example.com')` 后创建 stream connection，`GetServerName=''`
- 最小正确修法不需要重新设计 builder / factory / connector contract：
  - 只要在 `TFreePascalConnection.Create(...)` 两个 overload 里复制 `AContext.GetServerName`
  - 就能把 FreePascal 拉回与其他 backend 一致的 legacy compatibility 语义
- 这批之后，关于 context-level SNI 的结论更清楚了：
  - 是否最终移除这条兼容路径，仍然是产品/架构决策
  - 但在保留兼容的前提下，后端之间至少要先一致

## 2026-03-20 Findings (Active docs context-level SNI cleanup)
- 我先复核了原本下一条低风险候选：`WithSNI(...)` validation alignment。
  - fresh 运行 `tests/config/test_config_validation.pas` 已经通过
  - `ValidateClient` 当前也已经对 `.WithSNI(...)` 发出 deprecated warning
  - 因此这条并不是当前 worktree 里最值得重复动手的缺口
- 真正还在误导用户的是 active 非 archive 文档里的共享 context SNI 指导：
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- 这类漂移和 archive 报告、兼容性测试不同：
  - 它们属于当前有效文档
  - 用户更可能直接复制这些片段进入新代码
- 这批最稳的做法依然不是扩大到全仓库，而是只锁定已经确认残留的 active docs：
  - 新增 focused shell contract
  - 仅修这两份文档里的上下文级 SNI 片段
- 修法统一为：
  - 先 `CreateConnection(...)`
  - 再在连接上设置 `SetServerName(...)`
  - 然后 `Connect`
- 当前连续审查队列回到两个层次：
  - 代码/API 层：`BuildClient` 仍保留的 legacy context-level SNI compatibility boundary
  - guidance 层：剩余 non-archive examples / integration files 里哪些是 intentional compatibility coverage，哪些还是过期示例

## 2026-03-20 Findings (High-visibility SNI example cleanup)
- 我先复核了原本排在下一位的 connector precedence 问题，当前磁盘证据显示它已经是绿色：
  - `tests/test_tls_connector_hostname_override_precedence.pas` 存在
  - `src/fafafa.ssl.tls.pas` 的 `ApplyClientOptions(...)` 也已经是“支持时总是下发 `SetServerName`，非空 hostname 才在 unsupported backend 上报错”
  - fresh 运行 connector / connection-builder precedence tests 都通过
- 因此当前更值得修的不是重复碰已经绿掉的 runtime 分支，而是高可见度 examples 的指导漂移：
  - targeted docs 已经清理过
  - 但多个 examples 和 example-style tests 仍在直接示范 `Context.SetServerName(...)`
- 这类漂移比 dormant compatibility API 更有害，因为用户最容易复制的就是这些文件，而不是底层接口定义。
- 这批最稳的做法不是扫整个仓库，而是只锁定高可见度入口：
  - `examples/example_factory_usage.pas`
  - `examples/winssl_health_checker.pas`
  - `examples/winssl_https_downloader.pas`
  - `examples/winssl_rest_client.pas`
  - 以及 6 个 `tests/examples/*.pas`
- 修法分两种：
  - 对真实连接流：先 `CreateConnection(...)`，再 cast 到 `ISSLClientConnection`，然后 `SetServerName(...)`
  - 对仅展示 context 配置的 demo：直接去掉 context-level SNI 步骤，改成短提示，明确 SNI/hostname 是连接级配置
- 这批还补了一个 grep contract，避免这些入口再静默漂回旧指导。
- 下一轮最值得回到代码层的是：
  - `BuildClient` 仍保留的 legacy context-level SNI compatibility boundary
  - 以及 examples / integration 层哪些文件应继续保留“显式标注的 deprecated compatibility coverage”

## 2026-03-20 Findings (Auto backend state serialization and merge contract)
- `TSSLContextBuilder` 在前两批修完 explicit backend 和 selector gate 后，剩下的 builder state drift 集中在 auto-selection state surface：
  - `ExportToJSON / ImportFromJSON`
  - `ExportToINI / ImportFromINI`
  - `Merge`
- 根因和 explicit backend 那批一样直接：
  - `FAutoSelectBackend` 与 `FBackendRequirements` 没有进入导出格式
  - `Merge(...)` 又依赖 source `ExportToJSON`，所以 source 的 auto mode 也会被静默丢失
- 这次用 `CreateDefaultRequirements(optBalanced)` + `MinSecurityScore := 95` 作为稳定观测信号是正确的：
  - 当前 config harness 只注册 `sslFreePascal`
  - 保留 auto-state 时，`TryBuildClient` 应报 `No suitable SSL backend found for requirements`
  - 丢失 auto-state 时，builder 会回退到默认选择并成功
- 最小正确修法不是改 `WithAutoBackendSelection(...)` 的 runtime 行为，而是收口 active-mode 序列化语义：
  - auto mode 导出 `auto_select_backend=true`
  - auto mode 导出完整 `backend_requirements`
  - 仅当 builder 当前不在 auto mode 时才导出 `explicit_backend`
- import/merge 端需要处理一个重要细节：
  - `WithAutoBackendSelection(...)` 目前不会清掉 `FExplicitBackendSet`
  - 因此导入或合并 auto mode 时，必须显式清除 stale explicit pin，避免 round-trip 后错误复活
- 这批修复后：
  - focused auto-state regressions 通过
  - 前两批 explicit backend / requirement gate / minimum-score filtering regressions 继续通过
  - full `tests/config` audit 与 `compile_all_modules.py` 保持绿色
- 当前连续审查队列里，代码层面最高价值的问题已经回到：
  - legacy context-level SNI runtime contract
  - 以及两条高确定性的文档/API 漂移：PKCS#11 guide 的过时 builder API、WinSSL 文档中的 context-level SNI 示例

## 2026-03-20 Findings (Backend selector minimum score filtering)
- 在上一批修完 `SelectBestBackend(...)` 的 requirement gate 后，selector surface 还剩一条语义不一致：
  - `SelectBestBackends(...)` 在 minimum-score failure 分支上仍会泄漏 zero-score backend
  - 当前 harness 下用 `MinSecurityScore := 95` 可以稳定重现：
    - OpenSSL 安全分数是 `90`
    - FreePascal Native 安全分数是 `60`
    - 但 `SelectBestBackends(...)` 仍返回两条结果，且都带着 `MatchScore = 0`、`MeetsMinimumRequirements = True`
- 根因落在 `CalculateTotalMatchScore(...)` 的状态语义，而不是排序逻辑：
  - required-features check 失败时会把 `MeetsMinimumRequirements := False`
  - 但 `MinSecurityScore / MinPerformanceScore / MinCompatibilityLevel` 失败时只返回 `0`
  - 没有同步把 `MeetsMinimumRequirements` 置回 `False`
- 这导致同一个字段在不同失败路径上的含义不一致，也让 `SelectBestBackends(...)` 很难仅凭它正确过滤。
- 最小正确修法是在 `CalculateTotalMatchScore(...)` 的三个 minimum-score failure 分支都补上：
  - `ADetails.MeetsMinimumRequirements := False`
  - 再返回 `0`
- 这样做比单独在 `SelectBestBackends(...)` 加 `Score <= 0` 过滤更稳，因为它修的是根语义，不是末端补漏。
- 修复后：
  - focused selector regression 通过
  - `test_backend_selector_basic` 现在只返回真正合格的 backend 列表
  - 前一批 `RequirePKCS11Support` 的 builder-level regression 继续通过
  - full config audit 和全量编译门继续保持绿色
- 当前 backend-selection 审查的下一个高价值问题已经重新收敛为：
  - `FAutoSelectBackend / FBackendRequirements` 的 `Export/Import/Merge` 合同仍未锁定
  - 这次可以直接复用稳定的 minimum-score requirement 作为 round-trip 观测信号

## 2026-03-20 Findings (Auto backend requirements enforcement)
- 继续审查 backend-selection surface 时，发现了一条比“state 是否可序列化”更基础的行为 bug：
  - `TSSLContextBuilder.Create.RequirePKCS11Support.TryBuildClient(...)` 在当前只注册 `sslFreePascal` 的 config harness 中居然仍然成功
  - 但 `sslFreePascal` 的能力矩阵明确声明 `SupportsPKCS11 = False`
- 根因不在 builder 链式 API，而在 selector gate：
  - `CalculateTotalMatchScore(...)` 已经能把“不满足 required features”的后端打成不合格
  - `SelectBestBackends(...)` 也会基于 `MeetsMinimumRequirements` 过滤
  - 但 `SelectBestBackend(...)` 之前只是取“最高分的可用后端”，没有先过滤不合格候选
- 这会让 auto-select requirement 在单后端/少后端环境里退化成“尽量选一个”，而不是“必须满足要求”。
- focused RED 选 `RequirePKCS11Support` 很合适，因为它在当前 harness 下是稳定不可满足条件：
  - 修前：`TryBuildClient` 成功
  - 修后：`TryBuildClient` 返回 `No suitable SSL backend found for requirements`
- 最小正确修法就是把 qualification gate 补回 `SelectBestBackend(...)`：
  - 只有 `MeetsMinimumRequirements = True` 且分数有效的 backend 才参与最终 best-score 竞争
  - 不动现有排序/权重算法
  - 不顺手扩展到 auto-selection state serialization
- 这批修复后的 focused、adjacent、full config audit 和全量编译门都继续通过，说明默认 builder、显式 backend、preset runtime 没被这次 gate 收紧误伤。
- 剩余高价值问题现在更具体了：
  - `SelectBestBackends(...)` 与 `MeetsMinimumRequirements` 在 minimum-score 分支是否完全一致
  - `FAutoSelectBackend / FBackendRequirements` 的 import/export/merge 合同仍未锁定

## 2026-03-20 Findings (Context builder backend selection serialization and merge contract)
- `TSSLContextBuilder` 的 backend-selection drift 在上一批 `Clone/Reset` 修完后，剩下的真实缺口集中在三个 surface：
  - `ExportToJSON / ImportFromJSON`
  - `ExportToINI / ImportFromINI`
  - `Merge`
- 根因很直接：
  - 这三条路径都基于普通配置字段工作
  - 但 `FExplicitBackend / FExplicitBackendSet` 完全没有进入导出格式
  - `Merge` 又是基于 source `ExportToJSON` 做覆盖，所以也必然丢失 explicit backend pin
- focused RED 用 `sslWinSSL` 这类当前 Linux harness 下不可用 backend 后，问题变得可直接观察：
  - 原 builder 会失败
  - JSON round-trip 后却成功，说明 pin 丢了
  - INI round-trip 后却成功，说明 pin 丢了
  - Merge 后 destination 却成功，说明 source 的 pin 没被带过去
- 这批最小正确修法只扩展 explicit backend state，不碰 auto-select requirements：
  - JSON 仅在 `FExplicitBackendSet` 为真时导出 `explicit_backend`
  - JSON import 看到该字段时恢复 `FExplicitBackend`，并关闭 `FAutoSelectBackend`
  - INI 同样增加 `explicit_backend`
  - `Merge` 只在 source 导出了 `explicit_backend` 时覆盖到 destination
- 这样做保留了当前批次的窄语义：
  - 默认 builder 行为不变
  - 未显式 pin backend 的现有 round-trip 不受影响
  - `FAutoSelectBackend` / `FBackendRequirements` 继续保持未序列化状态，作为下一批单独决策
- full config audit 和 `compile_all_modules.py` 都继续通过，说明这次 state-surface 扩展没有把现有 config/runtime 边界打回去。

## 2026-03-20 Findings (Context builder backend selection state contract closeout)
- `tests/config` 的 backend-registration 噪音收口后，又暴露出一条真正的 builder state drift：
  - `Clone` 文义上应保留 builder 的完整行为语义，但源码只复制了证书/协议/session 等字段，没有复制：
    - `FAutoSelectBackend`
    - `FBackendRequirements`
    - `FExplicitBackend`
    - `FExplicitBackendSet`
  - `Reset` 文义上应回到 constructor defaults，但源码也没有恢复这些 backend-selection 字段。
- 这不是抽象担忧，而是 focused RED 已经直接证明：
  - 显式不可用 backend 的原 builder 会失败
  - `Clone` 后却不再失败，说明 explicit backend pin 被丢了
  - `Reset` 后仍继续失败，说明旧 backend pin 泄漏进“默认配置”
- 最小正确修法就是只改 state copy/reset，不碰 build path：
  - `Clone` 复制全部 backend-selection 字段
  - `Reset` 恢复与 constructor 一致的 backend defaults
- 这批修复后，`test_config_snapshot_clone` 新增合同测试和相邻 config/runtime regressions、全量 config 审查、全量模块编译都保持绿色。
- 但 builder state surface 还没有完全锁定：
  - `Merge`
  - `ImportFromJSON / ExportToJSON`
  - `ImportFromINI / ExportToINI`
  这些路径对 backend-selection state 仍然没有明确合同，应作为下一批单独决策。

## 2026-03-20 Findings (Config suite backend-registration audit closeout)
- 这轮 `tests/config` 审查里剩余的失败点并不都来自产线实现，实际上分成两类：
  - harness drift：
    - `test_batch_config`
    - `test_conditional_config`
    - `test_config_snapshot_clone`
    - `test_preset_configurations`
    这些文件都在真实调用 `TryBuildClient/TryBuildServer`，却没有导入任何 backend registration 单元，因此在当前环境里落到 `No SSL library available. Please register a library first.`
  - API drift：
    - `test_context_cert_loading` 仍直接调用 `ISSLContext.GetNativeHandle`
    - 但 native handle 在当前 API 中已经迁到可选接口/辅助函数 `fafafa.ssl.native_handle`
- 最小正确修法保持在 test-side：
  - 为 runtime config tests 导入 `fafafa.ssl.freepascal.lib`
  - 只在真实 build path 上固定 `.WithBackend(sslFreePascal)`，不污染纯配置/导出/链式 API 测试
  - `test_context_cert_loading` 改成 `TryGetNativeHandle(Context, NativeHandle)`
- 复跑全量 `tests/config` 后已无残留失败，说明这批主要是测试夹具和 API 迁移滞后，而不是当前 builder/runtime 还有同类未收口缺陷。

## 2026-03-20 Findings (Context builder SNI deprecation warning alignment)
- `WithSNI(...)` 当前不是简单“没人用了”的死接口，而是仍然参与真实兼容链：
  - `BuildClient` 会把 `FServerName` 写进 context-level `SetServerName`
  - OpenSSL / MbedTLS / WinSSL connection 构造器又会从 context 默认值继承到连接
  - 多个现有测试仍依赖这条 context fallback 语义
- 因此，“直接删掉/停用 context-level SNI” 不是本批能安全做的局部修复，而是兼容性决策。
- 但 validation 层此前存在一条明确的低风险漂移：
  - 文档已经说 context-level SNI deprecated
  - `ValidateClient` 却对 `.WithSNI(...)` 完全静默
- 最小正确修法就是只补 warning，不动 runtime：
  - `WithSNI(...)` 仍然 valid
  - validation 明确提示它在配置 deprecated 的 context-level SNI
  - warning 直接指向 `ISSLClientConnection.SetServerName(...)`
- 这批相邻验证还顺手暴露并收口了一条老的测试夹具问题：
  - `tests/config/test_config_import_export.pas` 的 preset-import runtime case 仍依赖 ambient backend registration
  - 通过固定到 `sslFreePascal`，验证重新回到 builder/import-export 逻辑本身
- 到这里，真正剩下的 SNI 问题已经不是 drift，而是产品级兼容边界：
  - 要不要继续保留 `WithSNI` 作为 legacy fallback
  - 若保留，哪些后端与测试需要被正式锁定为 contract

## 2026-03-20 Findings (PKCS#11 and WinSSL docs API drift cleanup)
- 当前文档层存在两类已确认且会误导使用者的 API 漂移：
  - `docs/guides/PKCS11_USER_GUIDE.md` 仍示例不存在的 builder 链：`.ForServer` / `.WithPKCS11Key(...)` / `.Build`
  - `docs/guides/WINSSL_USER_GUIDE.md` 与 `docs/guides/WINSSL_QUICKSTART.md` 仍多处指导 `Ctx.SetServerName(...)`
- 第一类不是“旧写法仍可用”，而是明确的不存在 API：
  - 当前 builder 对应的公开方法是 `UsePKCS11(...)`
  - server/client 模式通过 `BuildServer` / `BuildClient` 区分
- 第二类不是单纯文风问题，而是和当前文档主口径冲突：
  - `GETTING_STARTED` / `INTEGRATION_GUIDE` 已明确 hostname/SNI 是连接级配置
  - WinSSL 文档若继续写 context-level `SetServerName(...)`，会把用户带回 deprecated 路径
- 这批修法保持 docs-only：
  - PKCS#11 示例改成 `WithCertificate(...) + UsePKCS11(...) + WithPKCS11PIN(...) + BuildServer`
  - WinSSL 示例和 FAQ 统一改成在 `Conn.Connect` 前调用 `(Conn as ISSLClientConnection).SetServerName(...)`
- 文档批次收口后，真正剩下的高价值问题已不在 docs，而在源码契约边界：
  - `src/fafafa.ssl.context.builder.pas` 的 `BuildClient` 仍保留 context-level `SetServerName`
  - 这条行为应该作为下一批代码/API 审查对象

## 2026-03-20 Findings (Context builder PKCS#11 server validation drift closeout)
- 当前磁盘上的 `TSSLContextBuilder` 存在一个明确的 validation/runtime 漂移：
  - `BuildServer` 已把 `FPKCS11URI` 视为合法私钥来源
  - `ValidateServer` 仍只接受 `private_key_file/private_key_pem`
  - 因而 `certificate + UsePKCS11(...)` 会被误判为 invalid
- 最有效的 RED 不需要真的连 HSM：
  - 只需生成真实证书 PEM
  - 组合 `WithCertificatePEM(...) + UsePKCS11('pkcs11:...')`
  - 然后直接断言 `ValidateServer` 的 `IsValid/HasErrors`
- 最小正确修法就是只改 `ValidateServer`：
  - required-key check 改成 `file or PEM or PKCS#11`
  - 错误文案补上 `UsePKCS11`
  - 不触碰 `BuildServer` 的 runtime 加载路径
- 这次 focused 修复后，相邻 builder regressions 与全量编译门都保持绿色，说明补丁没有把前一批 PEM precedence 收口打回去。
- 这轮项目审查还顺手暴露了两条明确的后续漂移：
  - `docs/guides/PKCS11_USER_GUIDE.md` 仍示例不存在的 builder API：`.ForServer` / `.WithPKCS11Key(...)` / `.Build`
  - 多份 WinSSL 文档仍示例 `Ctx.SetServerName(...)`，与当前“连接级别 SNI 优先”的文档口径不一致

## 2026-03-20 Findings (Context builder PEM precedence regression re-closeout)
- 当前磁盘上的 `TSSLContextBuilder` 存在一个真实的 build/validation 漂移：
  - `ValidateServer` 已经对外警告 “Both certificate/private key file and PEM are set - PEM will be used”
  - 但 `BuildClient` 仍会先打 `certificate_file`，并且私钥走 `file > PEM`
  - `BuildServer` 更明显：证书和私钥 build path 都没有兑现 `PEM > file`
- 这次最有效的 RED 不是只喂 PEM，而是构造 imported dual-state：
  - 先用 `WithCertificate/WithPrivateKey` 放入不存在的 file path
  - 再用 `ImportFromJSON(...)` 叠加真实 `certificate_pem/private_key_pem`
  - 这样可以直接证明 builder 仍然先踩到缺失文件
- 聚焦测试第一次运行时还暴露了一个测试夹具噪音：
  - 旧 runtime tests 依赖 ambient backend registration
  - 在当前环境里会先报 `No SSL library available`
  - 这不是本次想验证的行为，因此相关 runtime tests 现在都固定到 `sslFreePascal`
- 最小正确修法就是只改 build path 的材料选择顺序：
  - `BuildClient`: certificate `PEM > file`, private key `PKCS#11 > PEM > file`
  - `BuildServer`: certificate `PEM > file`, private key `PKCS#11 > PEM > file`
- 这批之后仍剩下一条明确的新缺口：
  - `BuildServer` 接受 `PKCS#11` 私钥
  - `ValidateServer` 仍没有把 `PKCS#11-only` 视为 valid
  - 这是下一批最值得处理的 default-validation drift

## 2026-03-19 Findings (TLS connector hostname override precedence regression re-closeout)
- 当前 worktree 里真正的 connector 缺口不是抽象讨论，而是一个已回退的具体行为：
  - `src/fafafa.ssl.tls.pas` 仍然保留 `if AServerName <> '' then ... SetServerName(...)`
  - `tests/test_tls_connector_hostname_override_precedence.pas` 这个 focused test 文件却不在树里
- 这会让 `TSSLConnector.ConnectStream(..., '')` / `ConnectSocket(..., '')` 再次退回旧语义：
  - non-empty override 正常
  - empty override 被当成“未设置”
  - inherited context `ServerName` 因而泄漏到本连接
- RED 已直接证明这一点：在 mock context 先注入 `ctx.example.com` 的前提下，empty override case 实际结果仍是 `ctx.example.com`。
- 最小正确修法仍然和 2026-03-09 的结论一致：
  - 对支持 `ISSLClientConnection` 的连接，总是下发 `SetServerName(AServerName)`，包括空字符串
  - 只在 non-empty hostname 且 backend 不支持 per-connection server name 时保留原来的 unsupported error
- 这样 builder 与 connector 的 precedence 再次统一为：
  - `connection override > context default > empty`
  - 其中 empty override 仍然是 override，不是 unset

## 2026-03-19 Findings (Wave C cert verify cache baseline re-verification)
- 这轮不是新修功能，而是对当前 worktree 的真实复核，因为 `tests/benchmarks/run_all_benchmarks.sh` 与 `tests/benchmarks/benchmark_cert_verify_cache.pas` 仍处于在改动面上，不能只信 2026-03-15 的旧证据。
- 复跑结果说明 `benchmark_cert_verify_cache` 没有退回旁路：
  - `run_all_benchmarks` 集成 contract 仍然通过
  - `benchmark_cert_verify_cache` smoke 编译通过
  - `run_phase2_performance_baseline.sh --fast-local --iterations 1 --tls-iterations 1 --skip-tls` 仍能在 `tmp/phase2_bench_results_*` 下完成 `3/3 PASS`
  - `run_minimal_ci_gate.sh --fast-local --skip-compile --skip-modules` 仍把同一条 Phase 2 baseline dry-run 配置透传到 `tmp`
- active docs 入口契约也仍为 PASS，说明这条 baseline 链在“脚本入口 + 文档入口”两层都没有回退到旧 `ci_pipeline`/旁路口径。
- 到这里可以认为 `cert verify cache baseline integration` 已在当前工作树上重新收口；下一步更值得转去新的行为模块，而不是继续在这条基线链上做重复验证。

## 2026-03-19 Findings (Wave C baseline and readiness manifest module)
- 在 closeout hub、live manifest、approval manifest 都齐了之后，Wave C 文档结构里最后缺的一块是上游 baseline/readiness 模块。
- 原因不是找不到文档，而是这些上游页仍按“时间顺序逐页展开”：
  - `WAVE_C_UNIFIED_BASELINE_STATUS`
  - `WAVE_C_READINESS_REFRESH`
  - `WAVE_C_UNIFIED_THRESHOLD_REFRESH`
  - `B107/B108/B109/B110`
  - `B113/B115/B116`
- 如果不补一页 manifest，这条上游链就仍然缺少一个模块级总览，无法和另外两块形成对称结构。
- 最合适的拆法是三段：
  - `Unified Fast-local Inputs`
  - `Readiness and Canary Gates`
  - `Approval Bridge Inputs`
- 这样当前 Wave C 文档结构已经稳定成三大模块 manifest：
  - baseline/readiness
  - live evidence
  - approval/submission
- 到这里，Wave C 导航层已经基本封板，再继续改入口页的收益会明显下降。

## 2026-03-19 Findings (Wave C closeout hub refresh)
- 在 live manifest 和 approval/submission manifest 都落地之后，`WAVE_C_CLOSEOUT_STATUS_2026-03-18.md` 如果还只指向 chain status，就会少掉真正的“总控页”角色。
- 因此 closeout 页现在更适合承担 hub 职责：
  - `Current Decision` 里直接列出两份 manifest
  - `Recommended Next Step` 先引导到 manifest，再引导到审批 brief / submission bundle
- 这样当前 Wave C 文档导航层次就稳定成：
  - `closeout status` 负责冻结边界与导航总控
  - `live evidence manifest` 负责运行证据模块
  - `approval / submission manifest` 负责审批材料模块
- 到这里，文档侧已经不是“能找到入口”，而是“入口层次已经固定下来”。

## 2026-03-19 Findings (Wave C approval and submission manifest module)
- live evidence manifest 已经把运行证据按模块理顺，但审批材料仍然是散落在：
  - `B113/B115/B116`
  - `B146/B147/B148/B149`
  - `closeout status` 与 `chain status` 只做了跳转，没有单独的审批模块页
- 这会让阅读路径还差最后一步：想看“可提交审批，但不可直接启用”的完整边界时，仍要手动在多页之间跳。
- 最合适的收口方式是再补一页 `approval / submission manifest`：
  - `Module 1`: signoff + enablement inputs
  - `Module 2`: submission materials
  - 再给出一个 `Decision Summary`
- 这样当前 Wave C 文档结构就从两层进一步稳定成三类角色页：
  - closeout / chain status hub
  - live evidence manifest
  - approval / submission manifest
- 这页的核心结论没有变化：
  - `signoff_state = READY_FOR_APPROVAL`
  - `enable_state = HOLD`
  - `submission_state = READY_TO_SUBMIT`
  - 当前剩余动作仍是人工审批，不是继续补 enable 实现

## 2026-03-19 Findings (Wave C live evidence manifest module)
- 当前总入口页虽然已经更新到 `20260319_consistent_b144` 批次，但它本质上仍是一页“导航页”，不是一页“证据分层页”。
- 对继续按模块推进的人来说，缺的不是更多历史入口，而是一份能直接回答下面问题的 manifest：
  - local-first 模块当前看哪几份证据
  - pre-CI 模块当前看哪几份证据
  - ops/export 模块当前看哪几份证据
  - approval/submission 当前默认页是哪几份文档
- `20260319_consistent_b144` 是当前最适合做 manifest 的批次，因为它一次覆盖：
  - `B123/B124/B125/B126`
  - `B129/B132`
  - `B137/B138`
  - `B139/B140/B142/B143/B144`
- 最小且可复用的做法不是再改一堆旧页，而是新增一页 `WAVE_C_LIVE_EVIDENCE_MANIFEST_2026-03-19.md`：
  - 按模块列当前状态与证据文件
  - 再从总入口页和索引回链到这页
- 这样当前 Wave C 文档结构就形成了清晰的三级导航：
  - closeout / chain status
  - live evidence manifest
  - 单个 B12x~B14x 证据文件

## 2026-03-19 Findings (Wave C chain status page latest evidence refresh)
- `WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md` 之前虽然已经是当前总入口，但它内部的 live chain table 仍停留在旧样例：
  - `B123/B124` 指向 `20260316_live`
  - `B129/B132/B137/B138` 指向 `20260316_guard2`
  - `B142/B143/B144` 还没有进入总入口页
- 现在更合适的入口批次已经存在于 `tmp/test-reports/`：
  - `run_id = 20260319_consistent_b144`
  - 同批次覆盖 `B123 -> B144`
  - 能同时表达当前 local-first、pre-CI、status export、alert、ops pack 全链健康状态
- 最小修法不是新建一页，而是直接刷新当前总入口页的证据索引：
  - 用 `20260319_consistent_b144` 替换旧 evidence table
  - 补上 `B125/B126/B139/B140/B142/B143/B144`
  - 把 `B146~B149` 另列为当前 approval/submission docs，避免和 `tmp/test-reports` 实链混淆
- 这样总入口页就不再只是“跳转到当前链路”，而是能直接看到最新一批实链证据。

## 2026-03-19 Findings (Wave C historical docs navigation leftovers closeout)
- 上一轮历史结果页导航刷新后，真正剩下的缺口只剩 4 处：
  - `B120` 还没有 `Current Wave C Chain`
  - `B133` 还没有 `Current Wave C Chain`
  - `B135` 还没有 `Current Wave C Chain`
  - `docs/DOCUMENTATION_INDEX.md` 的历史区还没列出 `B120`
- 这意味着大部分历史页虽然已经被导回 2026-03-15/03-16 新链路，但用户从 `B120` 或索引历史区进入时仍可能停在旧 trigger-oriented 语境。
- 这次修法保持最小化：
  - 不重写历史页正文
  - 只在页首增加当前主链入口和历史定位说明
  - `B120` 额外同时指向 `WAVE_C_UNIFIED_BASELINE_STATUS_2026-03-15.md` 与 `WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
  - `B135` 额外指向 `WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15.md`
- focused 扫描已经证明：2026-02-08/02-09 的 `B12x~B14x` 历史页现在都具备统一的 `Current Wave C Chain` 入口。

## 2026-03-19 Findings (Wave C B142 snapshot-bound local-guard status export)
- `B142` 原先虽然已经优先从 `tmp/test-reports` 取最新报告，但默认仍有 mixed-run 风险：
  - `B132` snapshot 单独取 latest
  - `B129` oncall 也单独取 latest
  - `B138/B140` 再分别独立取 latest
- 在 local-guard 持续巡检时，这意味着：
  - 最新稳定 snapshot 仍然是 `GREEN`
  - 但更新中的 `B129` 可能更“新”且暂时 `FAIL`
  - `B142` JSON 就会被误导出 `overall_state=ATTENTION`
- 最小修法和 `B137` 保持一致：
  - 默认先选 snapshot
  - 再从 snapshot 的 `Latest Evidence` 中提取 `- B129: ...`
  - 只有提取失败或文件不存在时，才回退到原来的 latest `B129` lookup
- 这样 `B142` 导出的看板/告警状态就重新绑定到同一批 local-guard 证据，而不是把 in-flight `B129` 混进稳定 snapshot。
- 实链验证结果表明这次收口已经延伸到下游：
  - `B142 = HEALTHY`
  - `B143 = NONE`
  - `B144 = PASS`

## 2026-03-19 Findings (Wave C B137 consistent snapshot-bound input selection)
- `B137` 原先虽然已经是 `tmp/test-reports` 优先，但默认仍存在 mixed-input 风险：
  - `SNAPSHOT_REPORT` 取最新 `B132`
  - `ONCALL_REPORT` 单独取最新 `B129`
- 在 local-guard 并行执行时，这意味着：
  - 旧 snapshot 仍是健康的 `GREEN`
  - 但更新中的 `B129` 可能更“新”且尚未配套
  - `B137` 就会被误拖成 `HOLD`
- 最小修法不是加新参数，而是利用现有 `B132` 报告已经写出的 `Latest Evidence`：
  - 默认先选 snapshot
  - 再从 snapshot 的 `- B129: ...` 行中提取配套 oncall 报告
  - 只有提取失败或文件不存在时，才回退到原来的 latest `B129` lookup
- 这样 `B137` 的默认输入语义就和 `B132` 对齐了：先保证证据批次一致，再谈最新。

## 2026-03-19 Findings (Wave C B132 consistent-run snapshot selection)
- `B132` 之前的问题不是单纯“取到了旧文件”，而是它对 `B123/B124/B125/B126/B129` 分别独立做 latest lookup。
- 在 local-guard 并行生成场景下，这会产生 mixed-run 视图：
  - 新 run 的 `B123/B124/B125` 已经落盘
  - 但 `B126/B129` 还没写完
  - `B132` 就会把新旧两批证据拼在一起，导致 snapshot 短暂出现不一致的 `ATTENTION`
- 最小且可辩护的修法不是新增参数，而是把默认读取语义改成：
  - 优先使用 `tmp/test-reports` / `test-reports` / `docs/test_reports` 中“最新完整的 local-guard run”
  - 完整的定义是：同一 run suffix 同时存在 `B123/B124/B125/B126/B129`
  - 只有在找不到完整批次时，才回退到原来的逐项 latest lookup
- 新契约证明了这一点：
  - 当新 run 只有 `B123/B124/B125`，而旧 run 已完整时，`B132` 现在会继续使用旧的完整 run
  - 从而避免把 in-flight 文件误当成当前稳定快照

## 2026-03-19 Findings (Wave C local-guard recovery semantics and weekly cadence alignment)
- 2026-03-19 首次继续复跑 local guard 链时，`B129` 的失败已不再是 `tmp/test-reports` 根目录问题，而是两层恢复语义缺口：
  - `B124` 断档后第一次复检仍会因为 `previous_b124_gap_hours` 而输出 `HOLD`
  - `B126` 只要窗口里见过 FAIL，就会持续 `DEGRADED`
- 这与现有运维文档口径不一致：
  - `B127` 明确写了“修复后重跑 B125 strict”
  - 但旧实现实际上会把历史 FAIL 粘住，无法通过一次修复恢复
- 进一步复盘真实 `B129` 失败后，又定位到第二个独立问题：
  - `B124` 默认 `MAX_BUNDLE_AGE_HOURS = 72`
  - 但 `B124/B130` 的文档节奏一直是“每周复核 latest bundle”
  - 这会导致 4 天左右的正常周内窗口就触发 `bundle_age_hours` HOLD
- 处置后，local guard 链的恢复逻辑变成：
  - `B124` 当前证据健康时，可在第一次复检恢复 `LOCAL_STABLE`，同时继续把旧 gap 作为证据行展示
  - `B126` 以 `latest_state` 决定 `trend_state`，历史 `fail_count` 继续保留用于观察
  - `B124` 默认 quick-bundle age window 调整到 `168h`，与“每周复核”一致
- 真实结果已经回到当前主链健康状态：
  - `B129 = PASS`（2026-03-19，run_id=`20260319_recovery_b129_v2`）
  - `B132 = GREEN`（2026-03-19，run_id=`20260319_recovery_b132_v2`）
- 并行触发 `B129` 与 `B132` 时，`B132` 可能短暂抓到尚未更新完成的旧 `B126/B129` 文件并显示 `ATTENTION`；顺序重跑后恢复 `GREEN`。这更像读取一致性问题，而不是当前守护逻辑回归。

## 2026-03-18 Findings (Wave C B125/B126 tmp-root alignment closeout)
- 在 `B129/B132/B137/B138` 已切回 `tmp/test-reports` 之后，local-first 守护链里最后两处默认仍写 legacy 根目录的是：
  - `scripts/run_wave_c_local_first_guard_bundle.sh` (`B125`)
  - `scripts/summarize_wave_c_local_guard_history.sh` (`B126`)
- 这会造成链路默认行为不一致：
  - `B129` 显式把 `B125/B126` 输出写到 `tmp/test-reports`
  - 但用户直接跑 `B125/B126` 时仍会落到 `test-reports`
- 处置后：
  - `B125` 默认 `REPORTS_DIR = tmp/test-reports`
  - `B126` 默认输出落到 `tmp/test-reports`
  - `B126` 历史读取改为 `tmp/test-reports` 优先，缺失时回退到 legacy `test-reports`
- 新增 focused contracts 覆盖了两件事：
  - 默认输出根目录已经切到 `tmp/test-reports`
  - `B126` 的历史摘要在 tmp 有数据时不会被 legacy 抢走，tmp 为空时仍能读 legacy
- 2026-03-18 再跑一次 `B129` 实链时结果是 `FAIL`，但失败点不是路径回归：
  - `B125` 中的 `B124` 返回 `HOLD`
  - 根因是 `previous_b124_gap_hours = 55`，超过阈值 `<= 24`
  - 因此这是当前时效/节奏门槛触发，而不是这次 `tmp/test-reports` 迁移导致的新回归

## 2026-03-18 Findings (Wave C approval-pending closeout status)
- 现在最缺的不是更多 Wave C 页面，而是一份“可以引用的结论页”：
  - 当前 landing page 说明了链路结构
  - B113/B148/B149 分别说明了 signoff、approval brief、submission bundle
  - 但还缺一个把“工程已收口，等待人审”的决策边界写成正式状态的文件
- 现有最新状态已经足够支撑 closeout status：
  - `B113 signoff_state = READY_FOR_APPROVAL`
  - `B115 enable_state = HOLD`
  - `B116 suggested_action = 保持禁用，等待人工签核完成`
  - `B148 brief_state = READY_FOR_APPROVAL`
  - `B149 submission_state = READY_TO_SUBMIT`
- 因此这轮最合适的动作是新增 closeout status 文档，而不是继续补说明页或脚本。
- 已新增 `docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`，并明确当前结论：
  - `engineering_state = CLOSED_OUT_PENDING_APPROVAL`
  - `workflow_state = DISABLED`
  - 剩余工作是 human approval decision，而不是 implementation batch
- `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md` 现在把 closeout status 放在第一入口。
- `docs/DOCUMENTATION_INDEX.md` 的 `Current Wave C Chain` 也已把 closeout status 提到最前。

## 2026-03-18 Findings (Wave C historical guidance docs close-out)
- 剩余未迁导航口径的页面主要是说明/模板页，而不是结果页：
  - `B121` 一页式 runbook
  - `B122` CI deferred / local mode
  - `B127` troubleshooting
  - `B130` oncall rhythm template
  - `B131` handoff checklist
- 这些页面的问题与前一批不同：
  - 不是“样例报告还停在旧日期”
  - 而是“说明文本仍可能把人带回旧执行语义或旧路径”
- 已确认两个最值得直接修的误导点：
  - `B127` 里查失败 bundle 的例子仍只看 `test-reports/`
  - `B130` 的 cron 示例仍把 quick/bundle/weekly 日志写到 `test-reports/`
- `B121` 本身是历史 runbook，但当前真实策略仍是 `workflow disabled`，所以只适合加前导说明并回指当前链路，不适合把整页重写成新流程。
- 已完成的文档收口动作：
  - `B121/B122/B127/B130/B131` 顶部都已增加 `Current Wave C Chain`
  - landing page 新增 `Historical Guidance Pages Still Useful`
  - 索引标题已改成 `Wave C Historical Guidance and Result Pages`
- 当前这波只对 guidance 层做了两处有意的内容更新：
  - `B127` 失败 bundle 排查优先看 `tmp/test-reports`
  - `B130` cron 日志推荐写到 `tmp/test-reports`，并显式 `mkdir -p`
- 这意味着 Wave C 文档已经形成三层稳定结构：
  - current chain 入口页
  - current active result docs
  - historical guidance/result docs
- 再继续推进文档的边际收益已经很低；下一步自然转回审批决策，而不是继续修页面。

## 2026-03-18 Findings (Wave C historical result navigation refresh)
- `docs/DOCUMENTATION_INDEX.md` 当前同时列出新旧两套 Wave C 文档，但旧 `B123~B149` 页面仍没有“当前入口”提示，读者很容易顺手点进 2026-02-09 历史页并误把旧样例当默认执行入口。
- 当前最新链路已经分成两段：
  - 2026-03-15：`WAVE_C_UNIFIED_BASELINE_STATUS`、`WAVE_C_READINESS_REFRESH`、`B107/B108/B109/B110`、`B113/B115/B116`
  - 2026-03-16：`B146/B147/B149`，外加 2026-03-15 的 `B148`
- `B123/B124/B125/B126/B128/B129/B132/B134/B136/B137/B138/B140/B141/B142/B143/B144/B145` 目前没有新的 docs 结果页，因此最稳妥的收口方式不是伪造新 run 文档，而是新增一个当前链路 landing page，并从旧页显式回指到它。
- `B146/B147/B148/B149` 已经有新的结果页，因此旧 2026-02-09 结果页适合直接标成 historical 并链接到新的 2026-03-15/2026-03-16 版本。
- 过程里发现 `planning-with-files` 的脚本路径最初引用错到了 `/home/dtamade/.codex/skills/superpowers/skills/planning-with-files/...`；修正为 `/home/dtamade/.codex/skills/planning-with-files/...` 后已恢复正常。
- 已新增 `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md` 作为 current-chain 总入口，并把当前 live 状态集中到同一页：
  - `B123 = LOCAL_READY`
  - `B124 = LOCAL_STABLE`
  - `B129 = PASS`
  - `B132 = GREEN`
  - `B137 = READY_FOR_APPROVAL`
  - `B138 = PASS`
- `docs/DOCUMENTATION_INDEX.md` 现在明确拆成：
  - `Current Wave C Chain`
  - `Other Reports`
  - `Wave C Historical Result Pages`
- 当前收口策略保持“历史页不改历史内容，只补当前入口提示”，因此既避免伪造 2026-03-16 历史页，又能把默认导航稳定切到新链路。
- 格式化阶段发现本机没有 `yarn`；改用 `npx prettier` 成功完成 docs 格式化。

## 2026-03-16 Findings (Wave C B129/B132/B137/B138 tmp-chain alignment)
- `B142/B144` 虽然已经能优先读 `tmp/test-reports`，但上游 `B129/B132/B137/B138` 仍默认写旧 `test-reports`，会导致整条 local-guard/pre-CI 链持续混用两套输出根目录。
- 处置后，这条上游链已经切回当前主口径：
  - `B129` 输出 `tmp/test-reports`
  - `B132` 优先读取 `tmp/test-reports`
  - `B137` 优先读取并输出 `tmp/test-reports`
  - `B138` report/logs 全部写 `tmp/test-reports`
  - `B139` cleanup 也已纳入 tmp 模式下的新样式
- 当前真实结果：
  - `B129 = PASS`
  - `B132 = GREEN`
  - `B137 = READY_FOR_APPROVAL`
  - `B138 = PASS`
- 这说明当前 Wave C 的 local-first / pre-CI 守护链已经和新的主证据链统一，不再只是下游脚本做兼容。

## 2026-03-16 Findings (Wave C B148 default lookup refresh)
- `generate_wave_c_ci_reenable_approval_brief.sh` 虽然已经支持 B116 packet 输入，但默认查找顺序仍停留在旧 `test-reports/wave_c_b146...`。
- 这会导致不带 `--input` 的调用在当前链路下回退到旧证据，和新的 B113-B149 链不一致。
- 处置后，默认行为已经改为优先当前链路：
  1. `docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_*.md`
  2. `docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_*.md`
  3. legacy `test-reports/wave_c_b146_ci_reenable_submission_pack_*.md`

## 2026-03-16 Findings (Wave C local observability path refresh)
- 多个 Wave C 守护/预检脚本仍只在旧 `test-reports/` 查找 quick bundle 或相关产物，看不到当前真实落在 `tmp/test-reports/` 的证据链：
  - `check_wave_c_first_run_preflight.sh`
  - `check_wave_c_local_first_continuity.sh`
  - `check_wave_c_local_drift_watch.sh`
  - `check_wave_c_post_trigger_observability.sh`
- 这会导致两类问题：
  - 看不到当前真实证据，产生假 FAIL/HOLD
  - `B124` 会被旧 `test-reports` 里的历史 drift 报告拖成假 HOLD
- 处置后：
  - 当前证据优先从 `tmp/test-reports` 解析
  - `B124 previous_b124_gap` 只看当前 tmp 链
- 直接结果：
  - `B123 = LOCAL_READY`
  - `B120 = READY`
  - `B124 = LOCAL_STABLE`
- 这说明当前 local-first / observability 支线已经和新的 Wave C 主证据链重新对齐。

## 2026-03-16 Findings (Wave C B146/B147/B149 submission chain refresh)
- 原来的 B146/B147/B149 仍建立在旧的 B137/B138/local-guard/CI-reenable 证据链上，和当前 2026-03-15 的审批链已经脱节。
- 处置后，这条链路已经重新定义为：
  - B146：由 B113 signoff + B115 prereq + B116 packet 生成“可提交审批”的 submission pack
  - B147：校验新的 B146 schema 与状态
  - B149：打包 B146/B147/B148，形成完整审批提交材料
- 当前状态：
  - B146 = `READY_TO_SUBMIT`
  - B147 = `PASS`
  - B148 = `READY_FOR_APPROVAL`
  - B149 = `PASS`
- 这说明审批材料链已经完整，且边界仍正确：
  - 它只支持“提交审批”
  - 不会越过人工批准去执行 enable

## 2026-03-15 Findings (Wave C B148 approval brief on unified entrypoint)
- 现有 `scripts/generate_wave_c_ci_reenable_approval_brief.sh` 只会读取 B146 submission pack，而我们当前安全推进到的是 B116 enablement packet。
- 处置后，B148 简报脚本可以同时消费：
  - 旧 B146 submission pack
  - 当前 B116 enablement packet
- 这让审批沟通材料可以继续推进，而不需要先回头硬补整条旧 B146 支线。
- 当前 B148 结论与边界：
  - `brief_state = READY_FOR_APPROVAL`
  - `signoff_state = READY_FOR_APPROVAL`
  - `enable_state = HOLD`
  - 结论仍是“保持 workflow disabled，等待人工签核”

## 2026-03-15 Findings (Wave C B115/B116 refresh on unified entrypoint)
- `scripts/check_wave_c_workflow_enable_prereq.sh` 和 `scripts/prepare_wave_c_b116_enablement_packet.sh` 都还绑着 2026-02-08 的固定 signoff/prereq 路径，不适合继续承接新的 2026-03-15 证据链。
- 处置：
  - B115 默认读取最新 B113 signoff record
  - B115 可直接消费 raw quick sprint bundle 报告，不再要求旧 B114 汇总页
  - B116 默认读取最新 B113 signoff record 与最新 B115 prereq 报告
  - B116 输出路径改成 run-specific，而不是固定 2026-02-08 文件名
- 新链路结果：
  - B115 = `HOLD`
  - B116 = packet generated, suggested action = keep disabled
- 这个结果是正确的，不是倒退：
  - 技术证据已经完整
  - 但 signoff 仍是 `READY_FOR_APPROVAL`，尚未人工批准

## 2026-03-15 Findings (Wave C B113 signoff pack on unified entrypoint)
- B113 之前没有 builder 脚本，只有 template + 手写 record。
- 处置：
  - 新增 `scripts/prepare_wave_c_b113_release_signoff.sh`
  - 输入显式绑定：
    - B107 threshold
    - B108 readiness
    - B109 canary
    - B110 rollback
    - quick sprint bundle
- 重要边界：
  - 当前 record 只会自动落到 `READY_FOR_APPROVAL`
  - 不会擅自写成 `APPROVED`
- 当前状态：
  - `signoff_state = READY_FOR_APPROVAL`
  - `allow_canary_execution = YES`
  - `allow_default_on_switch = NO`
- 这意味着技术 evidence chain 已经可以提交审批，但审批动作本身仍需用户/人工明确给出。

## 2026-03-15 Findings (Wave C B110 refresh on unified entrypoint)
- `scripts/run_wave_c_b110_rollback_drill.sh` 原先在 `recovery_recheck` 阶段仍使用“目录里最新 threshold/validation 报告”的隐式选择。
- 在统一证据链已经迁到 `tmp/test-reports/` 之后，这种隐式选择会带来将来捡错文件的风险。
- 处置后，B110 recheck 明确复用当前传入的：
  - `threshold_report`
  - `validation_report`
- 当前链路状态进一步延长为：
  - B107 = `PASS`
  - B108 = `READY`
  - B109 = `CANARY_READY`
  - B110 = `PASS`
- 这意味着下一步的重点已转移到 signoff / approval packet，而不是继续补 rollback 证据。

## 2026-03-15 Findings (Wave C B109 refresh on unified entrypoint)
- `scripts/prepare_wave_c_b109_canary_rollout.sh` 本身可复用，但它输出的 operator commands 仍停留在旧口径，没有体现：
  - `--reports-dir`
  - `--report-glob`
  - `--require-full-gate`
  - 显式 threshold/validation 报告路径
- 处置后，B109 canary 计划已经能直接引用 2026-03-15 的统一证据链。
- 当前新链路状态：
  - B107 = `PASS`
  - B108 = `READY`
  - B109 = `CANARY_READY`
- 这意味着下一步的主线已经不是“继续补证据”，而是进入后续 rollout governance 文档，例如 rollback / signoff / controlled approval。

## 2026-03-15 Findings (Wave C B107/B108 refresh on unified entrypoint)
- `scripts/evaluate_wave_c_b101_thresholds.sh` 原先会把目录内所有 `wave_c_b101_validation_*.md` 都吞进去；在 `tmp/test-reports/` 模式下，这会把 contract run 或 partial run 一并计入阈值判断。
- 处置：
  - 增加 `--report-glob`
  - 增加 `--require-full-gate`
- 新的 B107/B108 结果现在只基于 3 个真实的 2026-03-15 full-gate 样本：
  - `20260315_172046`
  - `20260315_180632`
  - `20260315_180735`
- 结果：
  - B107 = `PASS`
  - B108 = `READY`
- 这意味着下一步已经可以从“证明技术前置条件”切换到“设计受控 canary/default-on 策略”。

## 2026-03-15 Findings (Wave C unified threshold refresh)
- 现在已经有 3 次连续的新入口样本：
  - `20260315_172046`
  - `20260315_180632`
  - `20260315_180735`
- 这 3 次都来自同一入口：
  - `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate`
- 结果一致：
  - `overall = PASS`
  - `hit_rate_percent = 99.9`
  - `speedup_factor_x = 5.2 / 7.7 / 5.9`
- 因此旧 B106 阈值已经在新的 fast-local 入口下被重新证明；当前真正剩下的是产品/发布策略决策，而不是技术证据缺口。

## 2026-03-15 Findings (Wave C readiness refresh)
- 2026-03-15 的新事实是“执行入口已统一且 clean-worktree”，不是“旧阈值已经被新入口重新证明”。
- 因此需要显式区分两类结论：
  - 流程 readiness：现在是 `READY`
  - 默认开启 / 阈值 readiness：还需要基于新入口补连续样本
- 新增 `docs/test_reports/WAVE_C_READINESS_REFRESH_2026-03-15.md`，把这个边界写清楚，避免后续把一次低迭代 smoke 误当成新的阈值证据。
- 补充强证据：`bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate` 已通过，说明 compile/modules/benchmark 三段在新的 fast-local 口径下可以一起闭环。

## 2026-03-15 Findings (Wave C current status report)
- Wave C 的当前真实入口已经从历史单点 probe 演化为两条统一命令：
  - `scripts/run_phase2_performance_baseline.sh --fast-local`
  - `scripts/run_wave_c_b101_validation_playbook.sh --fast-local`
- 现有 active docs 虽然已经更新了入口命令，但仍缺一个“当前状态页”把最新命令、验证结果和输出路径放在同一处；新增 `docs/test_reports/WAVE_C_UNIFIED_BASELINE_STATUS_2026-03-15.md` 作为这个入口，并写入 `docs/DOCUMENTATION_INDEX.md`。

## 2026-03-15 Findings (Wave C B101 fast-local alignment)
- `scripts/run_wave_c_b101_validation_playbook.sh` 原先仍停留在旧路径口径：
  - 自己的 report/log 默认写到 `test-reports/`
  - benchmark compile 固定写 `tests/benchmarks/bin`
  - `--full-gate` 时也没有把 compile/modules 子步骤切到独立 tmp 目录
- 这意味着即使 Wave C baseline 主入口已经 clean-worktree，B101 playbook 仍会成为旁路污染源。
- 处置：
  - 增加 `--fast-local`、`--reports-dir`、`--bench-bin-dir`
  - dry-run 输出所有关键路径
  - `--full-gate --fast-local` 时显式指定：
    - `compile_all_modules.py --unit-output-dir`
    - `run_all_module_tests.sh` 的 module reports/bin/unit dirs
    - benchmark bin dir
  - cleanup helper 同步纳入 `wave_c_b101_*`
- 结果：
  - dry-run contract 与真实执行 contract 都确认 B101 playbook 可在 `tmp/` 下完成闭环，且 `git status --porcelain` 不变

## 2026-03-15 Findings (Wave B cross-platform summary stable states)
- `scripts/generate_wave_b_cross_platform_summary.sh` 之前仍用字面量 `TODO` 作为内部 sentinel：
  - 当 macOS/Windows summary 未回填时，最终 checklist 会把 `TODO` 直接写进输出文档
  - 这会让“待回填”与“脚本未实现/语义未定义”混在一起，降低审查清晰度
- 处置：
  - 新增 `parse_check_state` / `stable_check_state` 两层归一化
  - step 级未知状态稳定收敛到 `PENDING`
  - Linux 保持缺 step 时回退到 overall
  - macOS/Windows overall 直接继承 `PASS/FAIL/DRY_RUN/PROBE_ONLY/PROBE_OK/PENDING/READY`
- 结果：
  - summary 输出不再包含 `TODO`
  - 现有 Wave B checklist PASS/FAIL/SKIP 合同保持不回归

## 2026-03-15 Findings (Wave C cert verify cache baseline integration)
- `benchmark_cert_verify_cache` 原本还是一条旁路：
  - 不在 `tests/benchmarks/run_all_benchmarks.sh` 内
  - 依赖当前工作目录是项目根（fixture 路径相对）
  - 在低迭代 / `0ms` 时间窗口下会触发浮点异常
- 根因不是“少一个命令入口”，而是 benchmark runner 与 cert-cache benchmark 的运行契约没有统一：
  - runner 没有 `--bin-dir`
  - runner 默认在调用目录执行 benchmark，可导致生成物与 fixture 路径语义漂移
  - cert-cache benchmark 自身没有对 cwd 与低迭代做 fail-safe
- 处置：
  - `run_all_benchmarks.sh` 增加 `--bin-dir`，并在 `--output` 目录下执行 benchmark
  - 默认 benchmark 集新增 `benchmark_cert_verify_cache`
  - `benchmark_cert_verify_cache.pas` 支持命令行 iterations，fixture 优先读取 `FAFAFA_PROJECT_ROOT`，否则向上搜索项目根
  - 修复 `WithoutCache=0ms` 时的百分比除零
  - `run_phase2_performance_baseline.sh --fast-local` 新增 `tmp/phase2_bench_bin_<run_id>`
- 结果：
  - `run_phase2_performance_baseline.sh --fast-local --iterations 1 --skip-tls` 现在得到 `3/3 PASS`
  - cert verify cache 已经从“单独 probe”变成统一 baseline 流程的一部分

## 2026-03-15 Findings (Wave C baseline fast-local + active docs entrypoints)
- `scripts/run_phase2_performance_baseline.sh` 原先即使在 `--dry-run` 下也会先创建 `tests/benchmarks/results` / `docs/test_reports` 目录，且没有 clean-worktree 输出选项；这不利于“先审查、后执行”的本地节奏。
- 修复策略：
  - 增加 `--run-id`、`--fast-local`、`--doc-reports-dir`
  - `--dry-run` 只输出解析后的配置与命令，不创建目录
  - 所有输出路径限制在仓库根目录内，避免路径散落
- `scripts/run_minimal_ci_gate.sh --fast-local` 之前没有把 fast-local 语义透传给 Phase 2 baseline dry-run，虽然 dry-run 本身不落盘，但会造成门禁输出口径与真实 fast-local 路径不一致；现已统一到同一 `run_id` 与 `./tmp` 路径。
- `scripts/cleanup_fast_local_outputs.sh` 之前没有纳入 `tmp/phase2_bench_results_*`，导致新的 baseline fast-local 输出无法通过统一清理入口回收；现已补齐。
- 通过 MCP + 本地复核发现，active docs 仍有一批高可见入口漂移：
  - `README.md`
  - `docs/guides/GETTING_STARTED.md`
  - `docs/guides/QUICKSTART.md`
  - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  它们仍引用已不存在的 `ci_pipeline.sh`，会让新用户直接执行失败；本批已切回当前真实入口命令并新增契约锁定。
- 下一批高优先级候选：
  - `scripts/generate_wave_b_cross_platform_summary.sh` 仍保留多处 `TODO` 占位状态值，摘要语义存在歧义风险。
  - Wave C 的 `benchmark_cert_verify_cache` 仍是单独入口，尚未完全收敛到统一 clean-worktree baseline 流程。

## Session 2026-03-14 (run_all_module_tests clean-worktree defaults)
- `scripts/run_all_module_tests.sh` 默认将可执行输出写入 `bin/`，会覆盖仓库已跟踪的二进制文件，导致本地执行后 `git status` 变脏且难以审查。
- 处置：默认 `BIN_DIR` 改为 `./tmp` 下的 run_id 隔离目录，并新增 `--dry-run`（不要求 `fpc`、不创建目录/文件），用于低成本契约测试与审查。
- 回归防护：新增契约 `tests/scripts/test_run_all_module_tests_dry_run_paths_contract.sh`，断言 dry-run 默认路径在 `./tmp` 且不改变 `git status --porcelain`。

## Session 2026-03-14 (Wave B/B2 cross-platform gate hardening)
- macOS：`scripts/run_all_module_tests.sh` 原先依赖 GNU `timeout`，在 macOS runner 上全量退出 `127`；修复为 `timeout/gtimeout` 优先，缺失时回退 `python3 subprocess.run(timeout=...)`，并把失败解析从 `grep -P` 改为 `awk`（BSD grep 兼容）。
- Linux：B2 manual workflow 使用 setup job 的 `run_id` 命名 summary 文件，但 `scripts/run_wave_b_ci_gate.sh` 内部自生成 run_id，导致证据一致性脚本误报 `run_id mismatch`；新增 `--run-id` 并在 workflow 与 `scripts/run_tls13_signer_gate_ci.sh` 统一透传。
- Windows：`run_openssl_tests.ps1` / `run_winssl_tests.ps1` 同时使用 `[CmdletBinding()]` 与自定义 `[switch]$Verbose` 会与 common `-Verbose` 冲突；改为移除自定义参数并用 `$PSBoundParameters.ContainsKey('Verbose')` 控制回显。`scripts/run_wave_b_windows_gate.ps1` 优先 `pwsh`，并强制 UTF-8 日志输出，提升 artifact 可读性。
- Windows modules：`scripts/validate_all_modules.ps1` 旧硬编码模块清单与当前 `fafafa.ssl.openssl.api.*` 命名漂移，存在“编译极少数模块也 PASS”的假阳性；改为动态扫描 `src/fafafa.ssl.openssl*.pas` 并加入 `MinModuleCount` 阈值门禁（默认 50），同时将编译产物隔离到 `test-reports/` 下。

## Session 2026-03-14 (CI workflows consolidation + fast-local cleanup)
- 问题：`.github/workflows/` 中存在多个 draft/历史 workflow 仍处于启用状态，且 `.github/*.md` 文档长期与实际不一致；导致 CI 成本不可控、维护口径漂移、审查困难。
- 处置：
  - 仅保留三条“真实可复现”的启用链路：
    - `ci.yml`：Linux minimal gate（对齐 `scripts/run_minimal_ci_gate.sh --fast-local`）
    - `tls13-signer-gate.yml`：TLS13 signer 专项门禁（path filter + 手动）
    - `wave-b-b2-manual.yml`：B2 三平台证据回填（手动）
  - 其余 workflow 统一改为 `.yml.disabled` 模板，避免误触发（需要时可按需启用）。
  - 新增 `scripts/cleanup_fast_local_outputs.sh`，专门清理 `./tmp` 下的 fast-local 产物；默认 dry-run 且拒绝清理项目根目录之外路径。
- 回归防护：新增契约 `tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`，约束默认 dry-run 不删除，`--apply --all` 只删除候选目录/文件不误删其它目录。

## Session 2026-03-13 (builder WithHTTPHooks)
- 为接入方减少样板代码：在 `TSSLContextBuilder` 链上新增 `WithHTTPHooks`，把 HTTP hooks 写入实现了 `ISSLHttpHooksAccess` 的 context（当前主要是 OpenSSL 后端）。
- 回归：新增 `tests/config/test_context_builder_http_hooks.pas`，断言 builder 注入的 hooks 可被取回并可调用。

## Session 2026-03-13 (denetworkize HTTP transport via hooks)
- 目标边界：`fafafa.ssl` 不是网络通讯框架；任何 HTTP 传输必须由上层实现并注入。
- 实现策略：
  - 新增线程局部 hooks：`src/fafafa.ssl.net.hooks.pas`（`SSLHTTPGet/SSLHTTPPost` + scope guard）。
  - `src/fafafa.ssl.base.pas` 增加 `TSSLHTTPGetCallback/TSSLHTTPPostCallback` 与可选接口 `ISSLHttpHooksAccess`。
  - OpenSSL connection 在执行 OCSP 在线检查前 push hooks（来源：context 的 `ISSLHttpHooksAccess`，否则依赖线程默认 hooks）。
- 去网络化落点：
  - `src/fafafa.ssl.ct.log.pas` 不再依赖 `fphttpclient/ssockets`。
  - `src/fafafa.ssl.openssl.api.ocsp.pas` 不再实现内部 BIO connect 传输（改走 hooks）。
  - `src/fafafa.ssl.http.client.pas` 仅保留为兼容/桥接层（不再 sockets）。
- 回归防护：新增 `tests/scripts/test_denetworkize_*.sh` 三个契约脚本，分别约束 CT log / HTTP client / OpenSSL OCSP 不得重新引入网络依赖。

## Session 2026-03-13 (Wave B gate fast-local clean worktree)
- 问题：`scripts/run_wave_b_ci_gate.sh` 默认会把 logs/summary/examples report 写入 `test-reports/`（仓库内可见/可跟踪目录），本地执行会污染 `git status`，不利于小步审查与持续门禁回归。
- 处置：
  - 新增 `--fast-local`：默认把 reports 根目录切到 `tmp/wave_b_ci_gate_reports_<run_id>/`（并透传 `--fast-local` 给 `run_all_module_tests.sh`，避免覆盖 `bin/` 中的跟踪产物）。
  - 新增 `--reports-dir DIR`：显式指定 reports 根目录（相对项目根目录），默认产物（summary/logs/examples json）落在该目录下。
  - 路径治理：`--reports-dir/--summary-out/--examples-report/--tls13-sign-bench-json-out` 仅允许项目根目录下的相对路径，避免产物散落到项目外部。
- 证据：新增合同 `tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`，要求 `--fast-local` 后 `git status --porcelain` 不变。

## Session 2026-03-13 (connection builder hostname precedence)
- 问题：`TSSLConnectionBuilder.WithHostname('')` 无法表达“显式清空”，因为 builder 仅在 `FHostname <> ''` 时才调用 `ISSLClientConnection.SetServerName`，导致有 context fallback 时无法清掉继承值。
- 决策：增加显式配置标记 `FHostnameSet`，并在 `TryBuildClient` 中以该标记决定是否调用 `SetServerName`（允许空字符串）。
  - 未调用 `WithHostname`：不触碰 per-connection 字段，保留 context fallback（兼容旧行为）。
  - 调用 `WithHostname('')`：显式清空，覆盖 context fallback。
- 回归：新增 `tests/test_connection_builder_hostname_precedence.pas`（mock context/connection），覆盖 3 条 precedence 路径。

## Session 2026-03-13 (split branch dirty overlay triage)
- 当前分支：`split/pre-onboarding-snapshot-2026-03-13`。
- 工作区“脏”的来源明确：`git diff` 的 **419 个已跟踪文件改动** 与 `git ls-files --others` 的 **1029 个未跟踪新增文件**，内容均与 `archive/pre-onboarding-snapshot-2026-03-13` 的提交 `a3101da` 一致（未发现任何“只存在于工作区”的独立改动）。
- 这意味着：若目标只是“清理工作区”，可以安全执行 `git restore .` + `git clean -fd`，不会造成信息丢失（因为内容已在 `a3101da` 保存）。
- 发现 1 个明显构建产物：`examples/pkcs7_sign_verify_simple_contract`（ELF 可执行文件，无需入库，建议删除并补 `.gitignore` 规则避免再次出现）。

## Session 2026-03-13 (certificate SAN normalization + hostname verification truth)
- 证书生成侧的 `SubjectAltName` 曾存在“沉默失败”的真相漂移：`TCertificateBuilderImpl.AddSubjectAltName('alt.example.com')` 会把裸值原样交给 OpenSSL `subjectAltName` 扩展解析器，但 OpenSSL 期望 typed 形式（如 `DNS:alt.example.com`），导致扩展添加失败被静默吞掉，最终证书并没有 SAN。
- 这会让一批依赖 `TCertificate.CreateServerCert(..., ['alt.example.com'])` 的用例表面在测 SAN/hostname，但实际走的是 CN fallback（SAN 为空）。
- 修复策略：在 builder 层把不带 `:` 的 SAN 输入自动视为 DNS，并前置成 `DNS:<value>`，从源头保证生成的证书确实包含 SAN。

## Session 2026-03-10 (pure Pascal client ALPN negotiation foundation)
- `ALPN` 是 pure Pascal client M1 checklist 里最适合率先推进的一条，因为它已经有字段、clienthello builder 和连接状态入口，只缺真正的协商闭环。
- 这波最小正确修复不是直接上重型网络端到端，而是先把 4 个关键点接起来：
  - clienthello offer parse
  - server-side selection
  - EncryptedExtensions parse
  - negotiated protocol observability
- 做完后，`ALPN` 还不能算“已满足”，但已经足够从“缺失”升级成“部分满足”，这正符合当前 M1 checklist 的保守策略。

## Session 2026-03-10 (API canon Wave 6 capability strategy)
- 在 Phase A 里，capability 语义如果不单独固定，后面很容易再次退回到“把 capability 当 marketing 表”或者“让业务代码先查 capability 再写逻辑”的坏方向。
- 最关键的设计结论是：`TSSLBackendCapabilities` 表达的是 runtime truth，而不是愿景；并且它主要服务 Advanced API 与框架作者，而不是 Core API 的普通调用路径。
- 一旦这层文档固定下来，pure Pascal 后端的 M1 checklist、backend selection、fallback policy 就都有了统一解释框架。

## Session 2026-03-10 (API canon Wave 5 error model)
- `ARCHITECTURE.md` 里已经写了“错误与观测模型”原则，但还不够具体；实现期一开始就需要一份可以直接指向 `Result / exception / warning` 分工的单页文档。
- 这份文档的关键不是罗列所有异常类，而是先把层次钉住：
  - Core API 用什么
  - Advanced API 可以暴露什么
  - warning 在 contract 里承担什么角色
- 只要这层明确下来，后续再做 pure Pascal client M1 时，错误语义就不必反复从零讨论。

## Session 2026-03-10 (builder material-loading helper extraction)
- `BuildClient` / `BuildServer` 的共性已经明显大于差异：真正不同的只剩 context type 和“server 是否强制要求 identity”。
- 在 cert/key/PKCS11 precedence、system roots、ServerName、session config 都已合同化后，再让两份长分支并行存在，只会继续制造 drift 窗口。
- 这波抽 helper 的价值不在“代码更好看”，而在于把后续 contract 风险压缩到一处；之后再改 cert/key/PKCS11/material 行为，不需要 client/server 各修一遍。

## Session 2026-03-10 (API canon Wave 4 entrypoint governance)
- 仅靠 `ARCHITECTURE.md` 里的入口治理章节还不够；实现期一开始就会反复遇到“这个入口是不是 canon”的问题，所以值得单独抽成治理文档。
- 最重要的不是罗列入口，而是明确每个入口的治理级别：
  - 推荐主入口
  - 兼容/底层入口
  - deprecated/bridge surface
- 一旦这三层写清楚，后续 helper 抽取、兼容策略和文档整改就都不必再从零解释。

## Session 2026-03-10 (API canon Wave 3 pure Pascal client M1 checklist)
- 在路线图里，“pure Pascal client M1” 如果没有 checklist，就会很容易演变成口号：大家都知道方向，但没人知道哪些条目已经够了、哪些还只是局部实现。
- 这波最重要的不是把条目刷成绿色，而是诚实地区分：哪些只是“部分满足”，哪些是真的“已满足”。
- 目前最保守、也最有价值的判断是：pure Pascal client 不是从零开始，但也还没到可以宣称生产可用的程度；先把证据面收拢成 checklist，后续实现期才不会失焦。

## Session 2026-03-10 (API canon Wave 2 contract index)
- 仅有 `ARCHITECTURE.md` 还不够；进入实现期前还需要一份“contract 当前入口”，否则调用方仍然要在几十个 plan 文件里盲找。
- 这份 index 最有价值的地方不是列出所有历史文件，而是按 `Core / Advanced / Backend-Specific` 三层重新组织当前真相。
- 只要后续持续维护这份 index，仓库的 contract 面就能保持“可导航”，不会再次退回到“信息都在，但没人知道从哪开始读”。

## Session 2026-03-10 (API canon Wave 1 architecture rewrite)
- 旧 `docs/reference/ARCHITECTURE.md` 虽然积累了很多真实 contract 片段，但整体结构仍是“旧架构说明 + 里程碑遗留 + 历史周计划”混合物，不适合作为当前真相源。
- Wave 1 最有价值的动作不是继续往旧结构上补 patch，而是直接把它重写成 API canon：先明确 Core / Advanced / Backend-Specific 三层，再把 builder/factory/config/backend 的已验证 contract 安放到正确位置。
- `docs contract` 对这类大文档重写很有价值：它能避免文档再次退回“什么都提了一点，但没有主设计结论”的状态。

## Session 2026-03-10 (roadmap mode decision)
- 后续执行方式已从“边修边问”切到“路线图驱动 + 非必要不停”。
- 用户确认的高层方向：
  - 第一优先是 SSL/TLS 接口设计的全面、合理、优雅
  - 第二优先才是实现完整度
  - 纯 Pascal 后端是重点投资方向
- 已明确的路线图约束：
  - 最高设计原则：API 易用与一致性
  - 主用户：普通业务开发者；次用户：框架作者
  - 主入口：`TSSLContextBuilder`
  - 抽象策略：统一核心 API + 分层暴露高级能力
  - 设计阶段：先把接口设计全面梳理完，再进入实现期
  - 兼容策略：允许受控 breaking changes
  - 一等平台：Linux + Windows
  - 纯 Pascal 后端优先覆盖 Linux
  - pure Pascal 第一个实现里程碑：HTTPS/TLS 客户端生产可用
- pure Pascal client M1 验收标准已明确成 10 条：TLS1.2/1.3、证书链校验、hostname verification、system roots、自定义 CA、SNI、ALPN、timeout/cancel/error、稳定流式 IO、观测性。

## Session 2026-03-06
- Initialized review session.
- Need to inspect repository health, architecture, and verification scripts before making recommendations.
- Worktree is heavily dirty before review (`git status --short` shows many modified and untracked files), so any review conclusions should distinguish baseline repo issues from in-flight local changes.
- Repository is doc/test heavy (`docs`: 1046 files, `tests`: 1202 files, `src`: 192 files, `scripts`: 135 files), suggesting process and maintenance overhead are material concerns.
- Several CI workflow files are disabled (`*.yml.disabled`) while README advertises strong passing status; this may create trust drift between documentation and actual automation state.
- `README.md` currently contains duplicate “30 秒示例” sections, indicating documentation drift.
- `python3 scripts/compile_all_modules.py` completed successfully: 179/179 core modules compiled on Linux.
- `bash scripts/run_minimal_ci_gate.sh --fast-local` passed, but this preset only exercises warning-noise governance contracts and does not prove the broader README claims about overall test pass rate.
- `docs/testing/TESTING_README.md` reports an older validation snapshot (20/72 modules, 95.5% pass) that conflicts with README marketing claims and current repo scale.
- `src/fafafa.ssl.context.builder.pas` builds a selected backend context, but system roots are always loaded through `TSSLFactory.CreateCertificateStore(sslAutoDetect)`, which may diverge from the chosen backend contract.
- `src/fafafa.ssl.openssl.backed.pas` appears to expose a public unit name typo (`backed` vs `backend`), increasing cognitive load and making the API surface feel less intentional.
- `.gitignore` excludes `bin/`, yet many binaries remain tracked in Git history, indicating repository hygiene drift.
- Batch 1 remediation implemented on 2026-03-06: README and testing docs now describe current verification entry points instead of stale fixed success metrics.
- CI workflow naming/comments now better communicate scope: active Linux OpenSSL workflow is Linux-specific, and the Phase C workflow is targeted rather than a full repo health signal.
- New regression test `tests/test_context_builder_backend_store_consistency.pas` proves `WithBackend(...).WithSystemRoots.BuildClient` uses the same backend for context and certificate store creation.
- Batch 2 remediation removed root `bin/*` from Git tracking while preserving `.gitignore` as the source of truth for generated artifacts.
- Added repo hygiene and workflow convergence contract tests under `tests/scripts/` to keep these policies from drifting back.
- Active workflow surface is now narrower: `ci.yml` remains the main always-on workflow, `tls13-signer-gate.yml` remains path-scoped, `test-all-platforms.yml` is nightly/manual, and draft/phase-specific workflows are manual only.
- Batch 3 remediation aligned `.github/workflows/ci.yml` with commands already validated locally: `python3 scripts/compile_all_modules.py` plus `bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal`.
- Added `tests/scripts/test_main_ci_workflow_local_verified_commands_contract.sh` so the main Linux CI workflow stays aligned with the documented local verification path.
- Batch 4 remediation introduced `src/fafafa.ssl.openssl.lib.pas` as the canonical OpenSSL library-management import unit, matching the `*.lib` naming pattern used by other backends.
- The shim delegates to legacy `src/fafafa.ssl.openssl.backed.pas`, so existing imports keep working while new code can use the cleaner unit name.
- `src/fafafa.ssl.pas` now points at the canonical OpenSSL lib unit, and high-visibility docs describe the new unit as the preferred import surface.
- Batch 5 remediation migrated active Pascal imports to `fafafa.ssl.openssl.lib`; remaining `backed` references are now limited to the legacy implementation itself, the shim, and docs that explicitly describe the compatibility relationship.
- A representative example compile (`examples/test_ssl_context.lpr`) still fails because it imports missing unit `fafafa.ssl.openssl.types`; this is pre-existing naming debt unrelated to the `backed` → `lib` migration.
- Batch 6 remediation fixed the root cause of the older OpenSSL naming drift: active code now imports canonical `fafafa.ssl.openssl.api.*` units directly, while newly added `fafafa.ssl.openssl.*` shims remain only as compatibility bridges.
- The first shim-only hypothesis was insufficient because Pascal units do not re-export API identifiers the way these old examples expect; direct import migration was the necessary root-cause fix.
- `examples/test_ssl_context.lpr` now compiles after canonical import migration, confirming the missing-unit class of failures is addressed.
- Some legacy examples such as `examples/test_openssl_rsa.lpr` and `examples/test_pem.lpr` still expose separate API/signature drift beyond naming (e.g. missing old helper loaders or changed type expectations). Those are follow-up modernization tasks, not evidence the naming migration failed.
- Batch 7 remediation modernized two representative legacy examples without changing library code.
- `examples/test_openssl_rsa.lpr` failed due to an example-local bug (`SetLength` on a static array), not because of the OpenSSL API rename itself.
- `examples/test_pem.lpr` failed because it still called removed helper loaders (`LoadRSAFunctions`, `LoadBIOFunctions`, parameterless `LoadEVP`) and lacked the unit that defines `PRSA`/`PBIO` aliases.
- Updated current high-visibility OpenSSL reference docs to prefer canonical `fafafa.ssl.openssl.api.*` unit names in module lists and code examples.
- Historical validation reports and time-stamped archival docs were intentionally left unchanged so they keep their original context.
- Added a consistent `Historical snapshot` banner to selected non-archive test/validation pages that contain older fixed metrics or dated environments.
- Left historical content intact and only added routing guidance toward `docs/testing/TESTING_README.md` as the current entry point.
- Added a rollup page `docs/test_reports/REPO_HYGIENE_REMEDIATION_SUMMARY_2026-03-06.md` that explains the repo hygiene cleanup in one place and points readers to the current verification path.
- Linked the summary from `docs/README.md` and `docs/test_reports/README.md` so it is discoverable from both the docs homepage and the historical reports directory.
- Added `tests/scripts/test_repo_hygiene_contract_batch.sh` as a single entry point for the repo-hygiene contracts introduced during the cleanup work.
- Added `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh` so the batch cannot silently lose expected component contracts over time.

## Session 2026-03-06 (review refresh)
- Refreshed the review against the current workspace state instead of relying on older notes.
- `python3 scripts/compile_all_modules.py` now passes with `229/229` core modules compiled on Linux, so the core Pascal surface is currently buildable.
- `bash scripts/run_minimal_ci_gate.sh --fast-local` now fails in the focused compile contract because `tests/openssl/test_openssl_chain_issuer_selection.pas` imports `fafafa.ssl.openssl.api.x509.chain`, while the live unit on disk is `src/fafafa.ssl.openssl.x509.chain.pas`.
- `bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal` passes, but it validates gate-contract semantics rather than the failing focused compile path; main CI therefore does not currently exercise the broken `--fast-local` smoke route.
- `README.md` still references `./ci_pipeline.sh`, which does not exist in the repository root, so the public testing story still has active command drift.
- `docs/README.md` still shows `TSSLFactory.CreateContext(sslClient)`, and active comments in `src/fafafa.ssl.factory.pas` still reference `sslClient`, while the real enum in `src/fafafa.ssl.base.pas` is `sslCtxClient`.
- The repository still has significant maintenance surface area (`src`: 242 files, `tests`: 1136 files, `scripts`: 135 files, `docs`: 1057 files), plus `9` disabled workflows, `156` script-contract files, `124` March plan files, and `472` archived docs; this is manageable, but it increases drift risk unless current-entrypoint docs stay very explicit.
- Strength signals remain strong: the builder path now routes system roots through the selected backend, the core compile gate is green, and the test framework provides both `fpcunit` helpers and a lighter OpenSSL-specific runner.

## Session 2026-03-06 (remediation execution)
- Added `src/fafafa.ssl.openssl.api.x509.chain.pas` as a compatibility shim that forwards `FindIssuerX509InChain` to the live implementation in `src/fafafa.ssl.openssl.x509.chain.pas`.
- The root cause of the broken `--fast-local` path was naming drift between a focused compile contract and the active X509 chain unit name; fixing the compatibility edge restored the local smoke path without changing certificate-chain behavior.
- Active docs now point to commands that exist today: `README.md` no longer references `./ci_pipeline.sh`, `docs/README.md` uses `sslCtxClient`, and `src/fafafa.ssl.factory.pas` comment examples now match the current enum names.
- Main Linux CI now mirrors the real local smoke path by running `bash scripts/run_minimal_ci_gate.sh --fast-local` after the core compile gate.
- Added `docs/testing/CURRENT_HEALTH.md` as the current status entry point and linked it from `docs/README.md`; `docs/testing/TESTING_README.md` now points readers there first and describes the updated CI scope.
- Fresh verification after remediation is green: core compile gate, `--fast-local`, `--pre-commit-minimal`, focused compile contract, docs drift contract, CI workflow contracts, and current-health doc contract all pass.

## Session 2026-03-06 (sha3 compatibility follow-up)
- Added `tests/scripts/test_active_openssl_api_imports_resolve_contract.sh` to scan active `uses` clauses under `src/`, `tests/`, and `examples/` and fail if any referenced `fafafa.ssl.openssl.api.*` unit has no corresponding source file.
- That contract exposed one remaining active import drift: `fafafa.ssl.openssl.api.sha3` was still referenced by active tests, but no source unit existed.
- Added `src/fafafa.ssl.openssl.api.sha3.pas` as a compatibility layer that restores the legacy SHA3 procvar-style API (`LoadSHA3Functions`, `LoadSHA3Module`, `SHA3_256_Init`, `SHA3_256`, `SHAKE128`, etc.) on top of EVP digest operations.
- Updated `examples/test_sha3.lpr` and `examples/test_openssl_sha3.lpr` to import `fafafa.ssl.openssl.api.sha3` instead of the EVP-only helper unit, because those examples exercise the old low-level SHA3 interface rather than the newer `*_Hash_EVP` helpers.
- Fixed `tests/examples/test_sha3_diagnostic.pas` to match the current `LoadOpenSSLCore` procedure-style API and validate success via `GetCryptoLibHandle`.
- The restored SHA3 compatibility layer now compiles and runs representative entry points successfully on Linux with OpenSSL 3.5.4.

## Session 2026-03-06 (legacy inventory follow-up)
- Audited the next suggested legacy surfaces: `cmac` is already aligned to `fafafa.ssl.openssl.api.cmac.evp`, while `rand_old` no longer exists as a source unit and only survived as a stale string in the active module inventory.
- Added `tests/scripts/test_active_module_inventory_units_resolve_contract.sh` to ensure active inventory metadata does not point at nonexistent `fafafa.ssl.openssl.api.*` units.
- Removed the stale `rand_old` entry from `tests/test_all_modules_comprehensive.pas` and left a note that the modern `rand` API supersedes it.
- A focused runtime check on `tests/test_all_modules_comprehensive.pas` exposed a separate real bug: `AddModule` used parameter names identical to record fields inside a `with` block, so inventory rows self-assigned and printed blank names/categories.
- Fixed that root cause by renaming `AddModule` parameters and assigning fields explicitly via an index instead of `with`-shadowed names.
- Added `tests/scripts/test_module_inventory_runtime_contract.sh` so the inventory program must keep printing populated module names and priorities.

## Session 2026-03-06 (core validation loader follow-up)
- Continued the active-string audit into two representative runtime validation programs: `tests/test_core_modules_only.pas` and `tests/test_headers_validation.pas`.
- Both programs compiled but failed at runtime because they only called `LoadOpenSSLLibrary` and then asserted module-level function pointers without calling the corresponding module loaders.
- Added `tests/scripts/test_core_openssl_validation_runtime_contract.sh` to require both programs to compile, run, and emit their success markers.
- Fixed the root cause by adding a small `LoadValidationModules` helper in both programs that loads the specific modules whose pointers they assert (`ERR`, `BIO`, `RAND`, `BUFFER`, `EVP`, `SHA`, `BLAKE2`, `AES`, `DES`, `ChaCha`, `HMAC`, `BN`, `RSA`, `DSA`, `DH`, `EC`, `ECDH`, `ECDSA`, `ASN1`, `PEM`, `X509`, `X509V3`, `KDF`).
- That runtime contract then exposed a deeper implementation gap in `src/fafafa.ssl.openssl.api.evp.pas`: SHA3/SHAKE EVP procvar types existed, but the corresponding global variables and batch loader bindings were missing, so `EVP_sha3_256` always stayed nil.
- Fixed `src/fafafa.ssl.openssl.api.evp.pas` by exporting `EVP_sha3_224/256/384/512` and `EVP_shake128/256` procvars and loading them in `EVP_BINDINGS`.
- Added `tests/scripts/test_active_validation_summaries_no_rand_old_contract.sh` and removed the stale `rand_old` wording from active validation summaries so they no longer describe a removed active inventory item.

## Session 2026-03-06 (zero-noise follow-up)
- Fresh `--fast-local` after the loader/EVP fixes exposed a zero-noise regression that had previously been hidden by cached builds.
- The focused zero-noise contract first failed on real warning/note sources in `src/fafafa.ssl.cert.pinning.pas`, `src/fafafa.ssl.openssl.api.store.pas`, `src/fafafa.ssl.pkcs11.provider.pas`, and `src/fafafa.ssl.pkcs11.backend.pas`.
- Fixed those sources with minimal semantics-preserving changes:
  - explicit `Result := nil` for managed `TBytes` returns in `cert.pinning`
  - removed or localized unused variables in store/provider code
  - removed unreachable `else` branch in `TPKCS11BackendFactory.IsBackendAvailable`
- The runtime validation contract also exposed that `src/fafafa.ssl.openssl.api.evp.pas` defined SHA3/SHAKE EVP function types but did not export/load matching procvars; adding those bindings made `EVP_sha3_256` available to active validation programs.
- After those fixes, focused zero-noise, `--fast-local`, compile gate, SHA3 compatibility, inventory contracts, and runtime validation contracts are all green again.

## Session 2026-03-06 (version-output modernization follow-up)
- Audited remaining `GetOpenSSLVersion` / deprecated loader-state calls in active tests/examples and separated intentional enum-version uses from stale string-output sites.
- Modernized the remaining string-output sites to `GetOpenSSLVersionString`, while preserving the two intentional enum-version call sites in `examples/test_version_detection.lpr` and `tests/openssl/test_openssl_1_1_compatibility.pas`.
- For programs that previously used only `api.pas`-style loading, switching to `GetOpenSSLVersionString` required aligning the loader path as well: `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)` are now used where the current version-string API is expected.
- Brought `tests/test_module_headers_quick.pas` into the same modernized validation lane: added `cthreads` on Unix, added explicit module loading, switched to current loader checks, and kept the runtime success marker under contract.
- After this pass, the remaining raw `GetOpenSSLVersion` hits in active tests/examples are intentional enum-version checks or a local helper procedure name, not stale string-version output sites.

## Session 2026-03-06 (resolved-version-string follow-up)
- A focused runtime contract showed that `tests/test_core_modules_only.pas` and `tests/test_headers_validation.pas` still printed `Unknown` even after earlier loader modernization.
- Root cause: both programs still entered through `LoadOpenSSLLibrary`, while their version output had been switched to the `api.core` version-string helper. That helper only reports a concrete version once the core loader path has established `LoadedOpenSSLVersion`.
- Fixed both programs by switching their library-loading section to `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)` while keeping the rest of their validation logic unchanged.
- The active validation programs now print resolved OpenSSL version strings rather than `Unknown`.

## Session 2026-03-06 (openssl 1.1 compatibility semantics follow-up)
- `tests/openssl/test_openssl_load.pas` is confirmed to be a Windows-only historical harness under the current Linux workflow because it imports the `Windows` unit and cannot compile on Linux.
- Added `tests/scripts/test_getopensslversion_allowlist_contract.sh` to freeze the current allowlist of `GetOpenSSLVersion` uses in active tests/examples.
- The remaining active compatibility target was `tests/openssl/test_openssl_1_1_compatibility.pas`.
- First remediation step fixed its compile drift by replacing outdated helper names (`LoadHMAC`/`LoadRAND`/`LoadBN`), correcting `EVP_PKEY_keygen` var-parameter usage, modernizing loader state checks, and patching the EC paramgen control constant usage.
- Second remediation step fixed its runtime semantics: when OpenSSL 1.1.x is unavailable in the current environment, the program now reports an explicit `[SKIP] OpenSSL 1.1.x runtime not available` outcome instead of misreporting `[FAIL] 1.1.x Core Loading`.
- This keeps the test meaningful on hosts that do have 1.1.x, while making current Linux/OpenSSL 3-only environments report an environment limitation rather than a product regression.

## Session 2026-03-06 (openssl load harness modernization)
- `tests/openssl/test_openssl_load.pas` was no longer just Windows-only: after removing the `Windows` unit dependency, it still depended on a large set of obsolete `Load*Module` helper names and a manual DLL-loading skeleton.
- Modernized the program in-place as a current cross-platform loader smoke test:
  - replaced manual DLL probing with `LoadOpenSSLCore` + `GetCryptoLibHandle`/`GetSSLLibHandle`
  - added local compatibility wrappers that map the historical module table to current loader entry points
  - renamed the local `GetOpenSSLVersion` procedure to `PrintOpenSSLVersionInfo`
  - removed the interactive `ReadLn` footer so the program can run non-interactively in contracts
  - replaced `for var` loops and brittle symbol probes with current-FPC-compatible constructs
  - added an ASCII completion marker for stable contract checking across terminals
- The modernized `test_openssl_load.pas` now compiles and runs successfully on the active Linux workflow and prints version information plus a summary block.

## Session 2026-03-06 (crypto basics harness modernization)
- Modernized `tests/crypto/test_crypto_basics.pas` from a Windows-only legacy loader skeleton into a Linux-compatible current-loader smoke test.
- Removed the `Windows` dependency, switched library initialization to `LoadOpenSSLCore`, replaced obsolete `Load*Module` calls with the current exported loader entry points, fixed current API shape mismatches in HMAC and EVP signing, and removed manual `FreeLibrary` cleanup in favor of `UnloadOpenSSLCore`.
- Kept the original test intent intact: basic crypto smoke over RAND, SHA256, HMAC-SHA256, AES-CBC, and RSA sign/verify.
- The program now compiles and runs successfully on the active Linux workflow.

## Session 2026-03-07 (quick module validation modernization)
- Modernized `tests/test_modules_quick_validation.pas` and `tests/test_priority1_modules.pas` to the current loader model.
- Both programs had the same stale assumption as earlier validation harnesses: `LoadOpenSSLLibrary` + a partial manual load step, while expecting BN/BIO/RAND/ERR and other module functions to already be available.
- Fixed `tests/test_modules_quick_validation.pas` by adding a focused `LoadValidationModules` helper and a stable completion marker.
- Fixed `tests/test_priority1_modules.pas` by removing `with`-shadowed `AddTest` field assignment, adding a focused `LoadValidationModules` helper, and emitting a stable completion marker.
- Both programs now compile and run successfully on the active Linux workflow.

## Session 2026-03-07 (backend and algorithm batch modernization)
- Modernized `tests/test_backend_capabilities.pas` and `tests/test_algorithms_batch.pas` to the current loader entrypoints.
- `test_backend_capabilities.pas` was functionally green already; it now uses `LoadOpenSSLCore` + current loader-state checks and emits a stable completion marker.
- `test_algorithms_batch.pas` is an availability-report harness rather than an all-green functional test; it now uses `LoadOpenSSLCore` + `LoadEVP(GetCryptoLibHandle)` and emits a stable completion marker while preserving its current summary semantics.
- Both programs now compile and run successfully as active Linux smoke tests under contract.

## Session 2026-03-07 (algorithm and benchmark entrypoint modernization)
- Modernized `tests/test_algorithm_availability.pas` to use `LoadOpenSSLCore` + current loader-state checks and added a stable completion marker.
- Modernized `tests/performance/benchmark_crypto.pas` to use `LoadOpenSSLCore` + `LoadEVP(GetCryptoLibHandle)` before running EVP-based benchmarks, which fixes the runtime nil-pointer crash in the SHA/AES benchmark path.
- Both programs now compile and run successfully as active Linux smoke/benchmark entrypoints.

## Session 2026-03-07 (pkcs7 and self-contained example modernization)
- Modernized the PKCS#7 example family (`pkcs7_basic_example`, `pkcs7_data_example`, `pkcs7_encrypt_decrypt_example`, `pkcs7_sign_verify_example`, `pkcs7_sign_verify_simple`) to the current loader model.
- These examples previously mixed `LoadOpenSSLLibrary` with module-local symbol expectations, so they printed unresolved version strings and reported key PKCS7 functions as unavailable even though the modules were present.
- Updated them to use `LoadOpenSSLCore`, explicit current module loaders (`LoadPKCS7Functions`, `LoadOpenSSLBIO`, `LoadEVP`, etc.), and stable completion markers.
- Also updated `examples/02_generate_certificate.pas` to the current loader path and verified it runs successfully in an isolated output directory.

## Session 2026-03-07 (tool example modernization batch)
- Modernized the remaining CLI-style example tools in one batch: `03_file_encryption`, `file_encrypt`, `password_hash`, `digital_signature`, and `hmac_tool`.
- Common fix pattern:
  - replace `LoadOpenSSLLibrary` with `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)`
  - keep explicit module loads (`LoadEVP`, `LoadOpenSSLRAND`, `LoadOpenSSLPEM`, `LoadKDFFunctions`, etc.) where the tool actually depends on them
  - add stable completion markers for contract testing
- Special-case fixes:
  - `03_file_encryption` now binds PBKDF2 explicitly through `fafafa.ssl.openssl.api.kdf.PKCS5_PBKDF2_HMAC`
  - `file_encrypt` no longer re-loads `libcrypto` manually and instead reuses `GetCryptoLibHandle`
  - `hmac_tool` now uses the current `EVP_DigestSignInit` call shape and current loader imports
- The full tool/example group now compiles and runs successfully with real roundtrip/runtime contracts.

## Session 2026-03-07 (tool examples runtime batch)
- Completed the remaining tool/example modernization batch: `03_file_encryption`, `file_encrypt`, `password_hash`, `digital_signature`, and `hmac_tool` now run under the current loader stack.
- The batch used real runtime contracts instead of help-only checks:
  - file encryption roundtrip
  - password hash generation + verification
  - digital signature generate/sign/verify roundtrip
  - HMAC generate + verify roundtrip
- This confirms the current loader path is not only compile-compatible but functionally valid for these CLI tools.

## Session 2026-03-07 (crypto family A modernization)
- Modernized the remaining active crypto smoke family A programs: `test_blowfish`, `test_blake2`, `test_chacha20`, `test_camellia`, `test_sha3_simple`, `test_ripemd`, `test_sm3`, and `test_sm4`.
- Common fix pattern:
  - move to `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)`
  - keep explicit `LoadEVP(GetCryptoLibHandle)` when needed
  - add stable `[PASS] ... completed` markers
- For algorithms that may legitimately be unavailable in the current OpenSSL build (`blowfish`, `chacha20`, `sm4`, `ripemd`, `sm3`), the programs now keep nonfatal semantics when the entire set is unavailable instead of turning environment capability into process failure.
- The whole family now compiles and runs successfully under one grouped runtime contract.

## Session 2026-03-07 (crypto family B completion)
- Completed the remaining `crypto family B` modernization batch locally while the worker batch was still converging.
- These active crypto programs now use the current core loader path and their required explicit module loads.
- After the final cleanup, active stale loader hits are reduced to the two intentional enum-version detection sites:
  - `examples/test_version_detection.lpr`
  - `tests/openssl/test_openssl_1_1_compatibility.pas`

## Session 2026-03-07 (parallel worker consolidation)
- Completed the parallel modernization wave across active smoke/validation programs.
- `crypto family A` is green under `tests/scripts/test_crypto_family_a_runtime_contract.sh`.
- `crypto family B` is green under `tests/scripts/test_crypto_family_b_runtime_contract.sh`.
- `cert/diag` is green under `tests/scripts/test_cert_and_diag_runtime_contract.sh`.
- `integration/pkcs11` is green under `tests/scripts/test_integration_pkcs11_runtime_contract.sh`.
- After consolidating local and worker changes, stale loader-state hits are fully eliminated from active tests/examples; the remaining raw `GetOpenSSLVersion` uses are only the two intentional enum-version detection sites.

## 2026-03-07 (remaining contract gap scan)
- Green baseline confirmed:
  - `bash tests/scripts/test_old_loader_hit_allowlist_contract.sh` => PASS
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`231/231` core modules compiled)
- Remaining high-signal uncontracted areas were rescanned.
- `tests/integration/` has many active program-style tests, but most simple compile attempts fail unless test support paths are included (`test_openssl_base` not found). This suggests integration contracts should reuse a dedicated compile helper or include explicit `-Fu./tests -Fu./tests/framework` style paths rather than the minimal `-Fu./src` pattern.
- `tests/certificate/` contains a substantial set of compileable OpenSSL smoke/workflow programs not yet covered by grouped shell contracts; a focused batch appears feasible.
- `examples/production/` is currently uncontracted and mostly network-sensitive. `https_server_simple.pas` compiles with `-Fu./src`, but the HTTPS client examples fail under plain `-Fu./src` because they depend on `fafafa.examples.tcp`; these examples are good candidates for a compile-only contract with the correct example helper search path, not a local-CI runtime contract.

## 2026-03-07 (integration runtime completion markers batch)
- Selected `tests/integration/` as the next high-signal batch because multiple programs already compile and run with `-Fu./tests/framework` but were not under a grouped runtime contract and had no stable final completion markers.
- Added `tests/scripts/test_integration_runtime_contract.sh` to compile/run five integration smoke programs with the correct framework path and assert explicit ASCII success markers.
- Observed RED immediately: `test_bn_simple.pas` (and by implication its peers) completed successfully but only emitted `RESULT: ALL TESTS PASSED`, which was less specific than the newer contract pattern.
- Added final `[PASS] ... completed` markers to:
  - `tests/integration/test_bn_simple.pas`
  - `tests/integration/test_asn1_simple.pas`
  - `tests/integration/test_bio_simple.pas`
  - `tests/integration/test_e2e_scenarios.pas`
  - `tests/integration/test_integration_tls_end_to_end.pas`
- Tightened `tests/integration/test_e2e_scenarios.pas` so its outer exception handler records a failed check instead of only printing `FATAL`, preventing a false-green completion marker on unexpected batch exceptions.
- A parallel `--fast-local` run hit `ld.bfd` error code `-7` while `compile_all_modules.py` was linking concurrently; a serial retry passed unchanged, so this was treated as transient linker/resource contention rather than a code regression.


## 2026-03-07 (certificate smoke + production example contracts)
- Added `tests/scripts/test_certificate_smoke_runtime_contract.sh` for self-contained certificate/OpenSSL smoke programs.
- RED:
  - initial contract failed because `tests/certificate/test_p2_pkcs7_data.pas` had no stable ASCII completion marker.
- GREEN:
  - updated `tests/certificate/test_p2_pkcs7_data.pas` to emit `[PASS] p2 pkcs7 data completed` on success.
  - adjusted the grouped certificate runtime contract to execute from the repository root because some certificate workflow programs depend on repo-relative fixture paths (for example `tests/certificate/test_pkcs7_workflow.pas` loading `tests/certificate/test_certs/`).
  - `bash tests/scripts/test_certificate_smoke_runtime_contract.sh` => PASS
- Added `tests/scripts/test_production_examples_compile_contract.sh` for `examples/production/*.pas`.
- Key finding:
  - production HTTPS client examples are suitable for compile-only validation locally and require `-Fu./examples` so `fafafa.examples.tcp` can be resolved.
  - runtime validation is intentionally deferred because these programs depend on network endpoints or external certificate material.
- Regression:
  - `bash tests/scripts/test_production_examples_compile_contract.sh` => PASS
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`231/231` core modules compiled)

## 2026-03-07 (integration simple runtime contract batch)
- Added `tests/scripts/test_integration_simple_runtime_contract.sh` to cover the self-contained simple integration programs:
  - `tests/integration/test_asn1_simple.pas`
  - `tests/integration/test_bn_simple.pas`
  - `tests/integration/test_bio_simple.pas`
  - `tests/integration/test_hmac_simple.pas`
  - `tests/integration/test_rand_simple.pas`
  - `tests/integration/test_rsa_simple.pas`
  - `tests/integration/test_x509_simple.pas`
- Used the existing `TSimpleTestRunner` summary token `RESULT: ALL TESTS PASSED` as the runtime contract marker for this batch.
- RED:
  - the new grouped contract failed only at `tests/integration/test_x509_simple.pas`
  - failure symptom: `X509_dup` returned `nil` for the minimal certificate built by `TestX509Dup`
- Root cause:
  - `X509_dup` on the active OpenSSL 3 runtime does not duplicate the overly minimal certificate object used by the old test path
  - duplication succeeds once the test constructs a more complete certificate with subject/issuer, validity window, public key, and signature
- GREEN:
  - updated `tests/integration/test_x509_simple.pas` to load `X509_get_notBefore` / `X509_get_notAfter` / `X509_gmtime_adj`
  - rebuilt `TestX509Dup` to create a duplicate-ready certificate before calling `X509_dup`
- Regression:
  - `bash -n tests/scripts/test_integration_simple_runtime_contract.sh` => PASS
  - `bash tests/scripts/test_integration_simple_runtime_contract.sh` => PASS
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`231/231` core modules compiled)

## 2026-03-07 (ocsp validation current-loader contract)
- Classified `tests/certificate/test_ocsp_simple.pas` as manual/interactive because it blocks on `ReadLn`; it should not be added to local runtime CI without an explicit non-interactive mode.
- Extended `tests/scripts/test_certificate_smoke_runtime_contract.sh` with `tests/certificate/test_ocsp_validation.pas` to create a RED check for stale loader assumptions in the OCSP smoke path.
- RED symptom: `test_ocsp_validation.pas(42,26) Error: Type mismatch` because the program still treated `LoadOpenSSLCore` as a boolean-returning function.
- GREEN fix in `tests/certificate/test_ocsp_validation.pas`:
  - use `LoadOpenSSLCore` as a procedure and verify core state via `TOpenSSLLoader.IsModuleLoaded(osmCore)`
  - explicitly call `LoadOpenSSLOCSP(GetCryptoLibHandle)` before checking OCSP procvars
  - emit `[PASS] ocsp validation completed` only on full success
- Regression remained green:
  - `bash tests/scripts/test_certificate_smoke_runtime_contract.sh` => PASS
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## 2026-03-07 (ocsp simple current-loader contract)
- `tests/certificate/test_ocsp_simple.pas` looked interactive because it ended with `ReadLn`, but redirected stdin in local CI exits cleanly; the real CI gap was stale loader usage, not blocking input.
- RED contract: `tests/scripts/test_ocsp_simple_runtime_contract.sh` failed because the program emitted no stable completion marker and reported `0/8` available OCSP functions.
- GREEN fix in `tests/certificate/test_ocsp_simple.pas`:
  - added `fafafa.ssl.openssl.loader`
  - switched to `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)`
  - explicitly loaded OCSP procvars with `LoadOpenSSLOCSP(GetCryptoLibHandle)`
  - emitted `[PASS] ocsp simple completed` on success
- After the fix, the simple OCSP smoke path reports `7/8` available functions on the local OpenSSL runtime and is safe to run under redirected stdin.

## 2026-03-07 (ocsp simple manual-only classification)
- `tests/certificate/test_ocsp_simple.pas` has a single hard interactive boundary: terminal prompt `按 Enter 键退出...` followed by `ReadLn;`.
- The program also does not explicitly load OCSP procvars and only prints advisory availability counts, so forcing it into runtime CI would duplicate/weaken the stronger `test_ocsp_validation.pas` contract.
- Minimal CI-safe strategy is therefore compile-only/manual-only, not a new non-interactive runtime mode.
- Added `tests/scripts/test_ocsp_simple_manual_only_contract.sh` to lock three expectations:
  - explicit source-level manual-only marker
  - interactive `ReadLn` remains present
  - the entrypoint still compiles while staying out of runtime contract coverage
- Retired the stale `tests/scripts/test_ocsp_simple_runtime_contract.sh` to a `[SKIP]` sentinel so future runs do not treat `ocsp_simple` as runtime-CI coverage.

## 2026-03-07 (ocsp simple conflict resolution)
- A parallel branch added a manual-only contract after the runtime-safe branch, leaving the workspace in a contradictory state: `test_ocsp_simple.pas` had current-loader runtime fixes, while `test_ocsp_simple_runtime_contract.sh` was retired to a skip sentinel.
- Re-checked the program behavior under redirected stdin: it exits cleanly in CI and now reports `7/8` OCSP functions once the module is explicitly loaded.
- Final decision: keep `test_ocsp_simple.pas` as a focused runtime-safe smoke program, and retire the manual-only branch.

## 2026-03-07 (integration primitives + certificate utilities focused batches)
- Added integration primitive coverage (later consolidated into `tests/scripts/test_integration_simple_runtime_contract.sh`) for:
  - `tests/integration/test_buffer_simple.pas`
  - `tests/integration/test_dsa_simple.pas`
  - `tests/integration/test_ec_simple.pas`
  - `tests/integration/test_ecdsa_simple.pas`
- These integration primitive programs already share the `TSimpleTestRunner` summary marker `RESULT: ALL TESTS PASSED`, so this batch required no Pascal source changes.
- Added `tests/scripts/test_certificate_utilities_runtime_contract.sh` for:
  - `tests/certificate/test_cert_utils_simple.pas`
  - `tests/certificate/test_cert_utils.pas`
  - `tests/certificate/test_certificate_chain_methods.pas`
  - `tests/certificate/test_cert_verification_failures.pas`
- Redirected stdin is sufficient for `test_cert_utils_simple.pas` and `test_cert_utils.pas`; their trailing `ReadLn` does not block CI when stdout/stderr are redirected.
- `tests/certificate/test_cert_store.pas` remains out of scope for this batch because it currently fails to compile on `GetNativeHandle`.
- `tests/integration/test_x509_basic.pas` remains out of scope for this batch because current runtime probing shows genuine test failures (name handling and version expectation), not a contract gap.

## 2026-03-07 (x509 basic + cert_store follow-up)
- `tests/integration/test_x509_basic.pas` had two real test-side semantic issues:
  - name-entry creation was more reliable with UTF-8/auto-length input in the exercised OpenSSL 3.x path
  - `TestX509BasicFields` asserted version retrieval without first setting the certificate version on that cert instance
- `tests/certificate/test_cert_store.pas` was still using stale member-style native-handle access; the canonical path is `fafafa.ssl.native_handle` helper APIs such as `IsNativeHandleAvailable`.
- Both files are now folded into existing grouped runtime contracts instead of remaining one-off probes.
- Final regression should stay serial on this machine because parallel heavy FPC links can intermittently hit `ld.bfd` error code `-7`.

## 2026-03-07 (integration extended runtime batch)
- Probed the remaining uncovered integration entrypoints with `-Fu./src -Fu./tests/framework` and local runtime execution before adding any new contract.
- `tests/integration/test_asn1_module.pas`, `tests/integration/test_ec_comprehensive.pas`, `tests/integration/test_error_recovery.pas`, `tests/integration/test_rsa_comprehensive.pas`, and `tests/integration/test_rsa_integration.pas` are all CI-safe on this machine:
  - compile successfully
  - run successfully
  - emit the stable summary token `RESULT: ALL TESTS PASSED`
- No Pascal source change was needed for this batch; the gap was coverage, not behavior.
- Added integration extended coverage (later consolidated into `tests/scripts/test_integration_simple_runtime_contract.sh`) for the next uncovered integration cluster.
- After this batch, the remaining uncovered integration entrypoints are concentrated in broader/higher-cost cases rather than simple self-contained smoke programs.

## 2026-03-07 (active program runtime batch aggregation)
- Active Pascal program entrypoints under `tests/integration` + `tests/certificate` are now fully covered by runtime/compile-only contract scripts (remaining uncovered count: `0`).
- Added `tests/scripts/test_active_program_runtime_contract_batch.sh` as the manual super-batch runner for all current active program runtime contracts.
- Added `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh` to pin membership and prevent silent drift/duplication.
- Kept `scripts/run_minimal_ci_gate.sh --fast-local` unchanged to preserve a cheap default gate; the new batch is intended for broader local sweeps.

## 2026-03-07 (runtime contract consolidation)
- Reduced runtime-contract script count by consolidating overlapping integration and certificate non-P2 runtime scripts.
- Integration consolidation folded the previously separate primitive and extended integration coverage into `tests/scripts/test_integration_simple_runtime_contract.sh`.
- Certificate consolidation folded old focused non-P2 runtime scripts into `tests/scripts/test_certificate_utilities_runtime_contract.sh` and kept `tests/scripts/test_cert_load_debug_contract.sh` as the sole compile-only environment probe.
- Updated the active runtime super-batch + coverage contract to reference only the new consolidated script set.
# 2026-03-08 P0 factory wave

- `TSSLFactory.CreateContext(const AConfig)` currently mutates backend-shared default config via `LLib.SetDefaultConfig(LConfig)` before creating a context; this makes one request's config observable by later requests.
- `TSSLFactory.GetLibrary` currently stores/returns a shared backend instance under lock but calls `Initialize` outside the lock; when `IsInitialized` stays `False` during slow startup, multiple threads can run `Initialize` on the same instance.
- Existing fake-library tests in `tests/test_context_builder_backend_store_consistency.pas` provide a good pattern for contract-only factory tests without real TLS dependencies.
- The minimal safe fix is entirely factory-local: no backend API change is required to stop config bleed and duplicate init.
- `IsLibraryAvailable` used the same create-then-init-outside-lock pattern, so tightening only `GetLibrary` would leave the same race pattern in the availability path.

# 2026-03-08 P1 repo hygiene mini-wave

- The repo already has a `tests/scripts/test_repo_hygiene_contract_batch.sh` entry point and coverage contract; adding one more focused script is lower risk than introducing a new batch.
- `python3 scripts/check_code_style.py src` currently emits exactly two WinSSL `CODEPAGE UTF8` warnings:
  - `src/fafafa.ssl.winssl.session.pas`
  - `src/fafafa.ssl.winssl.native_handle.pas`
- This batch can close visible style warnings without touching the larger 114 indentation errors.
- The focused contract can safely ignore the broader style-check failure by asserting only the specific WinSSL warning lines; this keeps the batch small and non-disruptive.
- After the fix, `check_code_style.py` still reports 114 indentation errors, but the warning count drops from `2` to `0`.

## 2026-03-08 (focused style batch: openssl context + backed)
- The next smallest high-signal style slice after `factory` + `backend.selector` was `src/fafafa.ssl.openssl.context.pas` (2 findings) plus `src/fafafa.ssl.openssl.backed.pas` (1 finding).
- All three findings were odd indentation on continuation lines; no runtime behavior or API contract change was required.
- A focused style contract remains the safest way to make progress while the repo still carries large unrelated style debt.
- Wiring the new focused contract into repo hygiene coverage prevents the cleaned files from silently regressing.
- Repo hygiene super-batch is green after direct rerun; the earlier nightly-schedule miss did not reproduce and appears transient rather than a source-file regression.
- After this batch, the full style checker count moved from `106` errors to `103`, with `0` warnings unchanged.

## 2026-03-08 (focused style batch: winssl context + connection)
- The next low-count backend-coherent style slice was `src/fafafa.ssl.winssl.context.pas` (2 findings) plus `src/fafafa.ssl.winssl.connection.pas` (5 findings).
- All seven findings were continuation-line indentation only; no runtime behavior or public API semantics changed.
- Grouping `WinSSLContext` and `WinSSLConnection` in one contract gave a cleaner backend-themed batch than mixing unrelated single-file fixes.
- After this batch, the full style checker count moved from `103` errors to `96`, with `0` warnings unchanged.
- Repo hygiene batch remains green after adding the new focused contract, so the focused-contract pattern still scales without destabilizing the batch entrypoint.

## 2026-03-08 (focused style batch: openssl certificate + ocsp stapling)
- The next backend-coherent low-count OpenSSL slice was `src/fafafa.ssl.openssl.certificate.pas` (1 finding) plus `src/fafafa.ssl.ocsp.stapling.pas` (2 findings).
- All three findings were continuation-line indentation only; no runtime behavior or public API semantics changed.
- After this batch, the full style checker count moved from `96` errors to `93`, with `0` warnings unchanged.
- Remaining style debt is now concentrated in fewer, denser files rather than many scattered one-liners; the top clusters are `tls13.servercertverify` (13), `capability.serializer` (13), `openssl.api.x509v3` (12), and `openssl.api.ts` (8).
- The repository still has a large non-code hygiene surface in `git status`, especially tracked root `bin/*` deletions, so future review noise is now more likely to come from workspace hygiene than from the focused style contracts.
- Governance noise remains high at the history layer: `.github/workflows` has `5` top-level workflows and `docs/plans` has `342` markdown records, so the next review round should separate active truth from archive more aggressively.

## 2026-03-08 (focused style wave: backend contexts + cert utils)
- The next low-risk wave combined two backend-context files (`wolfssl.context`, `mbedtls.context`) with one utility file (`cert.utils`) for a total of 8 continuation-line indentation findings.
- All eight findings were indentation-only; no runtime behavior or public API semantics changed.
- After this wave, the full style checker count moved from `93` errors to `85`, with `0` warnings unchanged.
- Remaining style debt is now concentrated even more strongly in dense files: `tls13.servercertverify` (`13`), `capability.serializer` (`13`), `openssl.api.x509v3` (`12`), and `openssl.api.ts` (`8`).
- The backend contexts still duplicate protocol-version validation entrypoints across four units (`OpenSSL`, `WinSSL`, `WolfSSL`, `MbedTLS`), which raises long-term semantic-drift risk even though this wave only touched formatting.
- Repository hygiene noise remains material: `git status` still shows a wide tracked `bin/*` deletion surface and several unrelated modified docs/workflows, so review bandwidth is increasingly constrained by workspace cleanliness rather than code correctness.
- Governance noise also remains high: `docs/plans` now contains `343` markdown records while `.github/workflows` has `5` top-level workflows.

## 2026-03-08 (focused style wave: x509v3 + helper APIs)
- This wave combined one dense OpenSSL API file (`openssl.api.x509v3`) with two smaller helper files (`capability.diff`, `openssl.api.sha3`) for a total of 20 continuation-line indentation findings.
- All twenty findings were indentation-only; no runtime behavior or public API semantics changed.
- After this wave, the full style checker count moved from `85` errors to `65`, with `0` warnings unchanged.
- Remaining style debt is now concentrated almost entirely in a small set of dense files: `tls13.servercertverify` (`13`), `capability.serializer` (`13`), `openssl.api.ts` (`8`), `cert.rotation` (`7`), then several `4`-line clusters.
- Repository hygiene noise remains material: `git status` still shows a wide tracked `bin/*` deletion surface and unrelated modified docs/workflows, so workspace cleanliness is now a more significant review problem than style checker breadth.
- Governance/history noise continues to grow: `docs/plans` now contains `344` markdown records.

## 2026-03-08 (focused style wave: x509v3 + helper APIs)
- This wave combined one dense OpenSSL API file (`openssl.api.x509v3`) with two smaller helper files (`capability.diff`, `openssl.api.sha3`) for a total of 20 continuation-line indentation findings.
- All twenty findings were indentation-only; no runtime behavior or public API semantics changed.
- After this wave, the full style checker count moved from `85` errors to `65`, with `0` warnings unchanged.
- Remaining style debt is now concentrated almost entirely in a small set of dense files: `tls13.servercertverify` (`13`), `capability.serializer` (`13`), `openssl.api.ts` (`8`), `cert.rotation` (`7`), then several `4`-line clusters.
- Repository hygiene noise remains material: `git status` still shows a wide tracked `bin/*` deletion surface and unrelated modified docs/workflows, so workspace cleanliness is now a more significant review problem than style checker breadth.
- Governance/history noise continues to grow: `docs/plans` now contains `344` markdown records.

## 2026-03-08 (focused style wave: capability serializer + cert rotation)
- This wave combined one dense serializer file (`capability.serializer`) with one medium certificate-maintenance file (`cert.rotation`) for a total of 20 continuation-line indentation findings.
- All twenty findings were indentation-only; no runtime behavior or public API semantics changed.
- After this wave, the full style checker count moved from `65` errors to `45`, with `0` warnings unchanged.
- Remaining style debt is now concentrated in just `11` files, led by `tls13.servercertverify` (`13`) and `openssl.api.ts` (`8`).
- The workspace cleanliness issue is now more prominent than the remaining style debt: `git status` still shows a broad tracked `bin/*` deletion surface and several unrelated modified workflow/doc files.
- Governance/history noise continues to rise: `docs/plans` now contains `345` markdown records.

## 2026-03-08 Findings (TLS13 style/warning wave + TS API wave)
- `src/fafafa.ssl.tls13.servercertverify.pas` had 13 odd-indentation hits concentrated in continuation lines; these were style-only and safe to fix surgically.
- The repo's zero-noise compile contract exposed an adjacent quality issue in the same TLS13 unit: several `TBytes`-returning helpers performed `SetLength(Result, ...)` before `Result := nil;`, which Free Pascal warns about as "managed type result variable does not seem to be initialized".
- Minimal fix for that warning class is explicit `Result := nil;` at function entry; this preserves semantics and satisfies the gate.
- `tests/scripts/test_workflow_trigger_convergence_contract.sh` had a false-negative bug under `set -euo pipefail`: `tr -d '\r' | grep -q` can fail because `grep -q` exits early and `tr` receives `SIGPIPE`.
- Rewriting `has_key()` to normalize file content into a variable and then run `grep` via here-string removes the `SIGPIPE`/`pipefail` hazard and makes the contract deterministic again.
- After the two style waves, style debt is down to `24` errors / `0` warnings, with remaining clusters now concentrated in 9 files.

## 2026-03-08 Findings (style debt to zero + PKCS11 zero-noise follow-up)
- Themed style batching remains efficient: a single contract covering `cert.advanced` + `cert.pinning` + `dns.ldns` removed 12 findings with no semantic changes.
- The final tail batch (`aesgcm.pool`, `freepascal.connection`, `freepascal.context`, `native_handle`, `pkcs11.types`, `http.client`) brought `python3 scripts/check_code_style.py src` to `0` errors / `0` warnings.
- `fast-local` then surfaced an adjacent quality issue in `pkcs11.types` / `pkcs11.uri`: zero-noise compile governance still had latent managed-result warnings that had not been exercised in earlier waves.
- Warning root causes matched earlier repo patterns:
  - `Result := nil;` needed before `SetLength(Result, ...)` on `TBytes` return values
  - `Default(TRecordType)` is safer than `FillChar(Result, ...)` for records containing managed fields
  - enum `case` statements should explicitly cover sentinel/none variants to avoid exhaustiveness warnings
- `tests/scripts/test_focused_compile_zero_noise_contract.sh` can spuriously fail with `Text file busy` if run concurrently with another command that compiles the same focused binary; this is a validation orchestration hazard, not a repo logic bug. Sequential verification avoids it.
- With style debt now cleared, the next highest-value review targets shift from formatting to repo governance: tracked `bin/*` deletion noise and `docs/plans` archive/index sprawl.

## 2026-03-08 Findings (plans current index docs governance)
- The repo already had docs-noise and docs-index-dedup governance, but it lacked a small stable entry for “what to read first” when `docs/plans` grows large.
- A root-level active index is lower risk than reorganizing hundreds of historical plan files: it improves discoverability without rewriting archive history.
- The right boundary remains:
  - `docs/PLANS_CURRENT_INDEX.md` = live entrypoint
  - `docs/plans/*.md` = historical execution records
  - `docs/archive/` = older archive scope
- The docs-governance strict batch was already green before this wave; the value added here is making that governance easier for humans to navigate, not changing the scanner logic.
- `docs/plans/*.md` count is now `353`, which strengthens the case for keeping the active index intentionally short.

## 2026-03-08 Findings (git status noise summary)
- A read-only `git status` summary layer is a safer first step than a failing cleanliness gate: it improves reviewer navigation without breaking large in-flight worktrees.
- The current worktree is dominated by review noise rather than runtime risk: `tmp/git_status_noise_summary_current.md` reports `1806` entries, including `docs_drift` `236`, `tests_drift` `342`, and `other` `903`.
- `generated_artifacts_root_bin` remains visible in the report (`134` entries), but `git ls-files -- bin` returns `0`; the current `bin/*` surface is cleanup/index drift, not an actively tracked generated-artifact source that needs a new blocking contract.
- The higher-value hygiene target is `test-reports/`: `git ls-files -- test-reports` returns `294`, and those tracked outputs currently collapse into the generic `other` bucket.
- `docs/plans` remains a meaningful governance hotspot: the live status summary shows `209` entries under `docs/plans`, reinforcing the need for a monthly rollup rather than a longer root index.

## 2026-03-08 Findings (test-reports noise bucket split)
- The previous `git status` summary proved too coarse for action: `test-reports/*` was being buried inside `other`, which made the dominant review-noise source look incidental.
- After splitting `test_reports_drift`, the live report shows `test_reports_drift = 902` and `other = 1`; this confirms test-report artifacts, not miscellaneous root drift, are the main hygiene problem now.
- `git ls-files -- test-reports` still returns `294`, so the issue is not just untracked scratch files; the repository still carries a large tracked historical-report surface.
- High-churn scripts still default into `test-reports/`, including `scripts/run_all_module_tests.sh` and several Wave B/C report generators, so the next meaningful fix is output-policy cleanup rather than another reporting tweak.
- This wave stays intentionally read-only: it improves prioritization signal without risking broad behavior changes across legacy report scripts.

## 2026-03-08 Findings (test-reports output policy + March summary)
- The highest-value low-risk hygiene move is default-path policy, not immediate historical cleanup: changing where new reports land stops the noise from growing without rewriting old evidence.
- Three high-churn scripts now stop contributing new default noise to `test-reports/`:
  - `scripts/run_all_module_tests.sh`
  - `scripts/run_wave_c_b101_validation_playbook.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
- The live drift surface does **not** drop immediately after this wave: `test_reports_drift` is still `944` and `git ls-files -- test-reports` is still `294` because the old tracked/untracked report inventory remains in place.
- This confirms the next repo-hygiene step should be historical-surface reduction (archive/ignore policy), not more observability work.
- A monthly rollup page is now justified, not optional: `docs/plans/*.md` has reached `356`, and the root current index should stay short rather than absorbing more historical links.

## 2026-03-08 Findings (Wave C local-guard report dir policy)
- The next coherent hygiene unit after the first output-policy wave was not “three isolated scripts”, but the whole Wave C local-guard report chain: B123/B124 outputs feed B125/B129/B132/B142/B144 readers and wrappers.
- A stable shared directory works better here than per-run directories: `tmp/wave_c_local_guard_reports/` preserves latest-report glob behavior while still moving default outputs out of `test-reports/`.
- This wave now prevents a broad class of new default noise, not just one script family: continuity, drift watch, guard bundle, history summary, oncall check, status snapshot, full gate, cleanup plan, consistency check, status export, and ops pack all align on the same default dir policy.
- The live `test_reports_drift` count remains `944` and tracked historical files remain `294` because the old report inventory still exists; this confirms the next step must be historical cleanup policy, not more path-by-path default tweaks alone.
- `docs/plans/*.md` is now `357`, so the monthly rollup is already paying off: this wave belongs in the month summary, not in a longer root current index.


## 2026-03-08 Findings (Wave C CI re-enable report dir policy)
- The local-guard output-policy wave exposed a real downstream drift, not just unfinished cleanup: B137/B143/B146/B147/B148/B149 still defaulted to `test-reports/`, so the “latest report” chain no longer matched current local-guard defaults.
- A second stable shared directory works well for this downstream chain: `tmp/wave_c_ci_reenable_reports/` preserves latest-report glob behavior for B146/B147/B148/B149 while keeping new default outputs out of the tracked report surface.
- B146 is the key bridge point and needs split source-of-truth inputs:
  - B137 + B143 should come from the CI re-enable shared dir
  - B138 + B142 + B144 should come from the local-guard shared dir
- The value of runtime contracts here is operational, not only static: a lightweight default-output contract caught that B149 subreports/logs would otherwise keep silently repopulating `test-reports/`.
- The live repo-noise picture barely changes immediately after another default-path wave: `test_reports_drift` is still `945` and `git ls-files -- test-reports` is still `294`, confirming the next step must include historical-surface cleanup rather than only more default rewiring.
- `docs/plans/*.md` is now `358`, so the monthly summary remains the right governance boundary; new waves should continue linking there instead of expanding the root active index.


## 2026-03-08 Findings (Wave C quick/enablement + Wave B/TLS13 report dir policy)
- Wave C quick-sprint and enablement want two stable shared dirs, not one mixed surface: `tmp/wave_c_quick_sprint_reports/` for B107/B108/B109/B110/B120, and `tmp/wave_c_enablement_reports/` for B115/B116/B119.
- Moving Wave C downstream reports to `tmp/` exposed a latent operational bug: several shell scripts assumed the output directory already existed. Adding `mkdir -p "$(dirname "$OUTPUT_FILE")"` is the safer default than relying on callers.
- The new Wave C runtime contract caught a real production bug, not only policy drift: `scripts/evaluate_wave_c_b101_thresholds.sh` had a broken embedded Python string literal, which caused B107 to fail and the bundle overall state to flip to `FAIL`.
- Wave B and TLS13 signer gate should stay in separate shared dirs: `tmp/wave_b_reports/` for generic B/B2 artifacts, `tmp/tls13_signer_gate_reports/` for signer-specific outputs. This keeps downstream latest-report scans coherent.
- Default-path governance is now ahead of historical cleanup: tracked `test-reports` files are still `294`, and `docs/plans/*.md` is now `360`, so the next high-value move is historical-surface reduction plus a smaller active queue, not more generic observability.
- The next remaining active `test-reports` default cluster is small and coherent: `scripts/run_wave_b_macos_gate.sh`, `scripts/run_wave_b_windows_gate.ps1`, and related archive/default-surface handoff in `scripts/archive_ci_artifacts_draft.sh`.


## 2026-03-08 Findings (Wave B platform gates + active report surface)
- The remaining active script-level `test-reports` default cluster was not random drift; it was one coherent Wave B surface: macOS gate, Windows gate, active archive scanning, and continuous monitor. Handling them together keeps the shared-dir semantics aligned.
- `scripts/run_wave_b_windows_gate.ps1` needs env-aware fallback instead of a hard-coded param default so the script can join the same shared `FAFAFA_WAVE_B_REPORTS_DIR` contract as the Bash Wave B scripts.
- `scripts/archive_ci_artifacts_draft.sh` should distinguish active paths from historical compatibility: Wave B/TLS13 active patterns now follow `tmp/wave_b_reports` and `tmp/tls13_signer_gate_reports`, while a single `LEGACY_REPORTS_DIR` variable preserves old generic `test-reports` scanning until the historical cleanup wave lands.
- Archive alignment is incomplete if callers do not propagate the actual report dir. `scripts/run_tls13_signer_gate_ci.sh` and `scripts/run_tls13_signer_gate_bundle.sh` both needed explicit env passthrough so archive collection still works when reports are written somewhere other than the default.
- `scripts/continuous_test_monitor.sh` was a separate noise source, not part of the Wave B chain, but still active enough to justify fixing in the same report-surface wave because it continuously writes fresh files during local monitoring.
- After this wave, the next active `test-reports` surface is mainly workflow-level explicit overrides, not script defaults: `.github/workflows/tls13-signer-gate.yml` and `.github/workflows/wave-b-b2-manual.yml.disabled` still point uploads and handoff steps at `test-reports/`.

## 2026-03-08 Findings (workflow-level Wave B/TLS13 report-dir policy)
- 脚本层默认路径已经收口后，剩余活动 `test-reports/` 噪音主要来自 workflow 层显式覆盖；继续在脚本层打补丁收益已经很低。
- `.github/workflows/tls13-signer-gate.yml` 应直接对齐 `tmp/tls13_signer_gate_reports`，否则 upload-artifact 和 step summary 会重新把活动面拉回旧目录。
- `.github/workflows/wave-b-b2-manual.yml.disabled` 的真实漂移点不只是在 gate 调用，还包括 summary job 里的 artifact 回收与 downstream closure/consistency 组装；这部分必须一起迁移到同一个 `REPORTS_DIR`。
- workflow 层与脚本层共享目录一致后，repo-hygiene 可以用一个静态合同同时冻结 run step、upload-artifact 和 summary-stage 的路径约束。
- 这波之后，`scripts/archive_ci_artifacts_draft.sh` 中的 `LEGACY_REPORTS_DIR` 兼容引用就明确属于历史面治理，不再和活动路径治理混在一起。

## 2026-03-08 Findings (Wave C quick + ci-matrix workflow report-dir policy)
- 剩余 workflow-level `test-reports/` 漂移分成两类：Wave C quick manual workflow 只需要复用已有共享目录，而 `ci-matrix-draft` 需要 workflow 自己定义一个独立 `tmp/` 目录。
- `ci-matrix-draft` 不需要再扩脚本能力：Linux 分支已经有 `FAFAFA_TEST_REPORTS_DIR` 透传口，直接复用 `scripts/run_all_module_tests.sh` 的现成参数化能力即可。
- macOS/Windows 的 draft 步骤此前几乎没有持久化报告，只是上传旧目录；把示例编译/运行日志显式写到 `tmp/ci_matrix_draft_reports` 更符合 artifact 语义，也能避免继续依赖空的 legacy 目录。
- 到这一步，当前 active/manual workflow 层显式 `test-reports/` 覆盖面已经基本清空；剩下的主要是历史跟踪面，而不是新噪音入口。

## 2026-03-08 Findings (historical test-report bucket cleanup)
- `test_report_*.txt` 是最适合先下手的历史桶：它由 `scripts/run_all_module_tests.sh` 单一来源生成，语义清晰，可重放，而且规模最大。
- 这一桶不能粗暴硬删，因为仍有 28 份被 `docs/test_reports/`、`docs/reference/` 和若干 2026-02 计划页引用；更稳的做法是“保留引用的迁档，未引用的退跟踪”。
- 迁到 `docs/archive/reports/test-report-history/` 后，历史证据与当前运行输出面终于分开：`test-reports/` 继续缩成活动/遗留过渡面，archive 才是历史凭证位。
- 第一桶落地后，`test-reports/` tracked surface 从 294 降到 188，说明“按家族清理”比零散删文件更有效。
- 这类历史清理在未提交前会暂时放大 `git status` 的 `test_reports_drift`，因为大量 delete/move 本身也算 drift；真正代表仓库收口的是 `git ls-files -- test-reports` 和目标家族是否还被跟踪。
- 下一优先桶应继续选 replayable 且规模成团的家族，例如 `test_p2_*` 或 `wave_b_*` 历史报告。

## 2026-03-08 Findings (historical test_p2 bucket cleanup)
- `test_p2_*` 是第二个高性价比历史桶：文件数只有 18，且活动文档只引用其中 3 份 comprehensive 结果，迁移决策非常清晰。
- 对这类模块结果桶，继续沿用“保留引用的迁档，未引用的退跟踪”比单纯删除更稳，因为 `docs/test_reports/` 仍承担历史说明文档角色。
- 第二桶落地后，`test-reports/` tracked surface 进一步从 188 降到 170；剩余大桶已经更集中在 `wave_c_*` 和 `wave_b_*`。
- 现在最合适的下一桶是 `wave_b_*`：规模适中、家族边界清晰，而且和已经完成的 `tmp/wave_b_reports` 活动目录天然对应。

## 2026-03-08 Findings (historical wave_b bucket cleanup)
- `wave_b_*` 历史桶比 `wave_c_*` 更适合先做第三桶：规模只有 24，且 concrete 引用文件可以精确锁到 11 份，不需要先拆更复杂的大桶。
- Wave B 家族同时覆盖 gate summary、cross-summary、B2 closure/consistency/handoff、macOS probe/summary 和 TLS13 本地 smoke 产物；做成一个桶能把相关历史证据一次性归到同一个 archive 位。
- 第三桶落地后，`test-reports/` tracked surface 进一步从 170 降到 146，剩余历史面已经收敛到 `wave_c_*`、`examples_compile_*`、`tls13_signer_*` 这些更明确的家族。
- 接下来不宜立刻整桶硬啃 `wave_c_*`；更稳的顺序是先收小桶 `examples_compile_*` 与 `tls13_signer_*`，再把 `wave_c_*` 按 local-guard / ci-reenable / quick 三个子家族拆开。


## 2026-03-08 Findings (historical examples_compile + tls13_signer bucket cleanup)
- The small-bucket strategy continues to pay off: `examples_compile*` and `tls13_signer_*` were coherent enough to clear in one wave without mixing in the much larger `wave_c_*` surface.
- `examples_compile*` split cleanly into “referenced historical evidence” vs “replayable noise”: 9 concrete files were still cited by docs/plans and therefore belonged in `docs/archive/reports/examples-compile-history/`, while 1 text check file had no current reference and was safe to drop.
- The tracked `tls13_signer_*` local-smoke artifacts had no concrete references outside `test-reports/`; this confirms they were residual replay outputs, not archive-worthy evidence.
- Updating concrete example-report references inside both active docs and already-archived Wave B summaries keeps archive navigation truthful: historical pages now point at stable archive paths instead of deleted `test-reports/` locations.
- After this wave, tracked `test-reports/` dropped from `146` to `128`; the remaining historical surface is now overwhelmingly concentrated in `wave_c_*` (`125` files), which makes the next split strategy clearer.


## 2026-03-08 Findings (historical Wave C quick / enablement bucket cleanup)
- Wave C quick / enablement required a stronger retention rule than earlier buckets: “externally referenced” alone was insufficient because archived bundle/observability documents would otherwise point at deleted peer artifacts.
- Using a dependency-closure keep-set avoided archive breakage: 19 files were directly referenced by docs, and 7 more had to be retained so the archived quick/bundle/rollback chain remained navigable.
- The remaining 4 files were genuinely replayable noise: one rolling latest B101 snapshot, one superseded threshold run, and two superseded B115 prereq runs.
- After this wave, tracked `test-reports/` dropped from `128` to `98`; the remaining historical surface is now almost entirely two Wave C sub-buckets plus 3 singleton files.
- The next highest-value cleanup is now the smaller pre-ci-reenable / submission / approval-brief bucket before returning to the much larger local-first / local-guard surface.


## 2026-03-08 Findings (historical Wave C pre-ci / submission bucket cleanup)
- The pre-ci / submission / approval chain was smaller and cleaner than the quick bucket: only 6 files were directly referenced by docs, and just 3 extra files were needed to preserve archive navigability.
- The retained closure reflects the actual approval path: B149 depends on B146/B147/B148, B146 depends on the latest B137/B138 pair, and the earlier B138 retained by docs depends on its matching B137 packet.
- The remaining 7 files were clearly superseded replay outputs rather than durable evidence, so deleting them shrank noise without weakening the archived approval narrative.
- After this wave, tracked `test-reports/` dropped from `98` to `82`; remaining historical noise is now dominated by a single local-first / local-guard Wave C bucket (`79`) plus 3 singletons.
- This makes the final historical cleanup path straightforward: one last coherent Wave C bucket, then the 3 singleton tails.


## 2026-03-08 Findings (historical Wave C local-first / local-guard bucket cleanup)
- The local-first / local-guard surface was the largest remaining Wave C bucket, and dependency closure dominated retention: only 11 files were directly referenced by docs, but 45 more had to be preserved so archive-internal continuity, drift, bundle, history, cleanup-plan, consistency, status, alert, and ops-pack links stayed navigable.
- After this wave, tracked `wave_c_*` historical artifacts dropped to zero; the historical `test-reports/` surface is now reduced to just 3 singleton files.
- A generic “all known archive histories” rewrite pass is now paying off: newly archived local-first / local-guard files also pick up correct links to previously archived quick / enablement and pre-ci / submission evidence.
- Remaining cleanup is no longer family-based governance work; it is just a final singleton tail plus any follow-up architecture review the project wants to resume.

## 2026-03-08 Findings (historical singleton tail cleanup)
- The final 3 tracked `test-reports/` files were not archive-worthy evidence; they were legacy singleton outputs with no current concrete doc references.
- `test_provider_result.txt` and `test_p4_engine_result.txt` are clearly replayable because `scripts/run_all_module_tests.sh` already emits run-id scoped result files for those modules, so retaining fixed-name historical leftovers only adds governance noise.
- `mbedtls_test_suite_20260209.md` is likewise replayable and already semantically covered by `tests/test_mbedtls_framework.pas` plus `docs/test_reports/MBEDTLS_BACKEND_STATUS_REPORT.md`; a separate archive copy would preserve bytes but not add navigational value.
- The right final guard is broader than the three filenames themselves: locking `git ls-files -- test-reports` to `0` makes it harder for any future fixed-name historical artifact to silently re-enter the tracked repo surface.
- With tracked historical `test-reports/` now at zero, the next meaningful review topic is no longer report-surface governance but the deferred backend context/default-validation architecture overlap.

## 2026-03-08 Findings (context builder server build/validation alignment)
- The most actionable `context/default-validation` drift was in the server builder path, not the factory anymore: validation, build, and backend-selection state had diverged in three concrete places.
- `ValidateServer` previously rejected PKCS#11-only key setups even though `BuildServer` explicitly accepted `FPKCS11URI`; this was a pure policy/runtime mismatch.
- `BuildServer` had fallen behind `BuildClient`: it skipped certificate/private-key PEM loading and ignored `WithSystemRoots`, while validation and API surface implied both were valid server-side inputs.
- The remaining higher-level architecture issue is now clearer: library-level defaults are still applied in different layers depending on backend, so direct `ISSLLibrary.CreateContext(...)` is not yet a cross-backend-equivalent API.


## 2026-03-08 Findings (library CreateContext default-config consistency)
- The right split is now clearer: library-level defaults belong to `ISSLLibrary.CreateContext(...)`, while per-request config belongs to `TSSLFactory.CreateContext(const AConfig)` and must stay context-scoped.
- A shared `TSSLFactory.ApplyConfigToContext(...)` helper removes backend drift at the application layer; without it, FreePascal / MbedTLS / WolfSSL were already diverging from OpenSSL / WinSSL.
- The failed compile after the first refactor exposed an important boundary: `TSSLConfig` does not own `CertificatePEM` / `PrivateKeyPEM`; those inputs are builder-only material and should not leak into factory-level config handling.
- After this wave, direct `ISSLLibrary.CreateContext(...)` is much closer to being a real cross-backend-equivalent API, and the remaining review work shifts from “defaults applied in different places” to “do config surfaces cover the same fields everywhere”.
- The next highest-value architecture check is parity across `SetDefaultConfig/GetDefaultConfig`, builder material fields, validation rules, and runtime application, especially around fields that intentionally exist only in builder state versus fields that are meant to round-trip through `TSSLConfig`.


## 2026-03-08 Findings (config surface parity audit)
- `TSSLConfig` is no longer just a context-creation record; it currently mixes at least three scopes: context/runtime fields, library-level logging fields, and dead/undeclared ownership fields like `BufferSize` / `HandshakeTimeout`.
- The highest-confidence dead-field pair is `BufferSize` and `HandshakeTimeout`: they are present in defaults and debug output, but the actual runtime control surfaces are elsewhere (`ISSLConnection.SetTimeout`) or absent entirely.
- `LogLevel` / `LogCallback` are not broken in the same way; they are live fields, but only on the library-default path. The real issue is scope mismatch, not missing implementation.
- `TSSLContextBuilder` has already evolved into a separate DSL with PEM / PKCS#11 / validation ergonomics. Treating it as a superset or mirror of `TSSLConfig` will keep causing boundary bugs.
- The safest next remediation is not to force-apply these fields somewhere arbitrary, but to make unsupported/scope-mismatched fields visible first, then split the surfaces intentionally.


## 2026-03-09 Findings (request config dead-field visibleization)
- The compatibility-preserving cut is to reject only **non-default** `BufferSize` / `HandshakeTimeout` values on the factory request path; that surfaces misleading configuration without breaking `CreateDefaultConfig(...)` callers that inherit the historical defaults.
- `HandshakeTimeout` now has a clearer ownership story: request-scoped context creation is the wrong layer, while `ISSLConnection.SetTimeout` and connection builder timeout remain the live runtime controls.
- `BufferSize` is even more clearly a dead field than `HandshakeTimeout`: there is still no context/runtime apply surface behind it, so fail-fast is safer than pretending it is honored.
- This wave fixes the highest-frequency misuse path (`TSSLFactory.CreateContext(const AConfig)`), but not the whole config surface; `ISSLLibrary.SetDefaultConfig` still needs the same visibility treatment if the project wants full consistency.
- The next architecture step should therefore mirror the same rule onto library-default config, and only then tackle `LogLevel` / `LogCallback` scope splitting.


## 2026-03-09 Findings (library default dead-field visibleization)
- The right convergence point was a shared factory helper, not five backend-local copies; once `TSSLFactory.ValidateRuntimeConfigFields(...)` became public, request-path and library-default-path could share the same contract.
- This confirms the dead-field problem was never backend-specific. It was a config-surface ownership issue, so fixing it once in the factory layer yields much better drift resistance.
- After this wave, `BufferSize` / `HandshakeTimeout` no longer silently pass through either of the two main config entry points: `TSSLFactory.CreateContext(const AConfig)` and `ISSLLibrary.SetDefaultConfig(...)`.
- The remaining ambiguity in `TSSLConfig` is now more isolated: `LogLevel` / `LogCallback` are the most visible scope-mismatch fields still left in the record.
- The next highest-value change is therefore not more dead-field work, but splitting or fencing off library-scope logging configuration from request-scoped context configuration.


## 2026-03-09 Findings (request config logging scope visibleization)
- The request/library split for logging is now explicit: `LogLevel` / `LogCallback` are legitimate fields only on the library-default path, not on per-request context creation.
- This was a scope mismatch, not a missing backend feature. The correct fix was to fail fast on the wrong entry point instead of trying to make per-context logging mutate shared library state.
- With this wave, the highest-noise `TSSLConfig` ownership ambiguities have been reduced to a smaller set: the record still mixes context-creation fields with builder-only concerns in the broader API story, but its main misleading runtime no-ops are now fenced off.
- The transient `ld.bfd: file truncated` failure during parallel verification was environmental, not semantic; a serial rerun of the same command passed unchanged.
- The next high-value step is API surface slimming: decide which fields stay in `TSSLConfig`, which remain builder-only, and which should move to an explicit library-config surface.



## 2026-03-09 Findings (CreateDefaultConfig request-safe logging compatibility)
- `CreateDefaultConfig(...)` is logically a request-config constructor, so inheriting library-scope logging defaults from `ISSLLibrary.SetDefaultConfig(...)` is a scope leak even when the backend intentionally stores those values.
- The earlier green compatibility test was not a sufficient proof of contract. A stronger RED case appears as soon as the backend default library is explicitly configured with custom `LogLevel` / `LogCallback`; before this fix, `CreateDefaultConfig(...)` leaked them back out unchanged.
- The safest boundary is the function exit itself: keep inheriting context-level security defaults from the backend, but always clear request-path-illegal logging fields before returning the record.
- Probes exposed a deeper visibility discrepancy worth a later architecture pass: direct `CreateMbedTLSLibrary` / `CreateOpenSSLLibrary` preserve raw backend logging defaults, while `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig` on the initialized cached path in this environment surfaced request-safe values (`sslLogNone`, `nil`, `LibraryType=sslAutoDetect`).
- This wave intentionally does not resolve that deeper backend/default-config visibility question; it makes the public `CreateDefaultConfig(...)` contract deterministic first, so callers no longer depend on backend- or environment-specific leakage.


## 2026-03-09 Findings (factory library registration constructor fix)
- 根因不在 backend `GetDefaultConfig(...)`，而在 factory registration / instantiation：`LClass.Create as ISSLLibrary` 没有可靠保留 backend constructor 里建立的默认状态。
- 一旦改成显式 `LibraryFactory`，`OpenSSL` / `MbedTLS` / `WolfSSL` / `FreePascal` 的 factory cached 实例立刻恢复与 direct `Create*Library` 相同的 constructor baseline，说明真正的问题是注册入口而不是后端默认值实现。
- `RegisterLibrary(..., ALibraryClass)` 可以继续作为兼容 fallback，但它不该再承担“保留 constructor 语义”的契约；对任何 constructor 里建默认配置/能力缓存/统计初值的 backend，都应该优先传 `ALibraryFactory`。
- 仓库内依赖 constructor 基线的 fake library 也要一起收口，否则测试很容易在 class-only path 上“偶然通过”，掩盖未来同类问题。
- 剩余 class-only 注册主要在无状态 fake backend；下一波更适合做 API/文档治理，而不是再做一轮大范围代码改动。


## 2026-03-09 Findings (legacy class-only registration governance)
- 最稳的治理方式不是立刻删除 class-only API，而是“内部全部迁走 + 外部兼容入口标记 deprecated”。这样既不给第三方代码突然断路，也明确了推荐路径。
- repo-local call site 全部显式化之后，新的 focused contract 才有意义：它不再只是检测一个历史问题，而是把“仓库内不能再新增 class-only registration”固化成规则。
- `tests/test_factory.pas` 和 `tests/test_context_builder_backend_store_consistency.pas` 里的 fake backend 虽然大多是无状态的，但继续保留 class-only 调用会把错误示范留在仓库里，长期会误导后续新增测试。
- 现在的 class-only overload 更适合作为遗留兼容层，而不是推荐 API；真正的“推荐范式”已经很清楚：`RegisterLibrary(..., ALibraryClass, ..., @CreateYourLibrary)`。
- 这波做完后，工厂架构面上与 registration/default-config 相关的高风险分叉已基本收敛，下一层更值得投入的是 `TSSLConfig` 与 builder DSL 的边界瘦身。


## 2026-03-09 Findings (builder override PEM parity)
- `Override(...)` 原本只覆盖了一部分 file/path-style 字段，导致 builder DSL 出现“`With*PEM` / `ImportFromJSON` 可以、`Override` 却不行”的隐式断层。
- 这类问题比单纯文档漂移更危险：用户看到 `Override` 是通用变换入口，很自然会认为它支持所有 export/import 可见字段。
- 对 PEM/file 成对字段，最稳的语义仍然是 builder 现有的 last-write-wins，而不是允许内部长期同时挂两份材料来源。
- 本波没有把 `Override(...)` 一次性扩成完全镜像 `ImportFromJSON/INI`；只先补了 PEM 材料字段，避免面过大。
- 下一波最自然的延伸是继续补剩余 builder-only 字段 parity，例如 `use_system_roots`、PKCS#11 URI/PIN 与相关密码字段。

## Session 2026-03-09 (builder override validation parity)
- `TSSLContextBuilder.Override(...)` 之前仍跳过 `use_system_roots` / `pkcs11_uri`，导致 validation DSL 与 `WithSystemRoots` / `UsePKCS11` 存在可观测语义漂移。
- `Override('pkcs11_uri', ...)` 是硬错误缺口：修复前 `ValidateServer` 仍会报 missing private key。
- `Override('use_system_roots', 'true')` 是 warning 级缺口：修复前配置仍然有效，但会错误保留 `no CA certificates configured` warning。
- 这波最小根因修复就在 `TSSLContextBuilderImpl.Override(...)` 分发层，不需要继续扩大到 builder build-path 或 `TSSLConfig` 结构重整。
- `pkcs11_pin` / `pkcs11_pin_method` 仍是下一层 parity 候选，但当前 builder export surface 还没有把这两个字段做成稳定可观测面。

## Session 2026-03-09 (builder override PKCS11 JSON parity)
- `Override('pkcs11_pin', ...)` 的正确 parity 不是只保存 PIN 字符串，还应与 `WithPKCS11PIN(...)` 一样把 PIN method 推成 `pmValue`。
- PKCS#11 字段之前的根因不是单点漏接，而是整个 JSON surface 缺口：`ExportToJSON` / `ImportFromJSON` / `Merge(...)` 都没有带上 `pkcs11_uri` / `pkcs11_pin` / `pkcs11_pin_method`。
- `pkcs11_pin_method` 适合用字符串而不是整数导出，便于 `Override(...)`、JSON、文档和人工调试共用一套可读值。
- 相邻 `tests/config/test_config_snapshot_clone.pas` 的 build-path 失败仍是旧问题；该测试文件没有导入 backend registration unit，这与本波 JSON/parity 改动无关。

## Session 2026-03-09 (builder override PKCS11 INI parity)
- PKCS#11 的真正 config-surface 收口不是只补 JSON；只要 INI 仍丢字段，builder 配置面就还存在双真相。
- 继续复用 PIN method 的文本 helper 比重新发明 INI 专用枚举编码更稳，能保证 JSON / INI / Override 共用同一组可读值。
- 补 `[PKCS11]` section 比把字段散落到 `[Certificates]` 更清晰，也更便于后续继续扩展 PKCS#11 相关配置。

## Session 2026-03-09 (builder override OCSP parity)
- OCSP 这条链的根因不只是少了字段映射，而是 `Override(...)` 如果只改布尔字段、不改 `FOptions`，会被 `SyncOCSPStaplingOptions` 的旧 option-set 反向污染。
- `Override(...)` 在这类 option-backed 字段上，必须与 builder methods 一样同时更新 flag + option set，不能只做一半。
- 这波确认了 builder 既有语义：`ocsp_stapling_required=false` 会清 `required`，但不会自动清掉 `enabled`；override parity 应复制这个行为，而不是重定义它。


## Session 2026-03-09 (builder override cert verify cache parity)
- `cert_verify_cache` / `cert_verify_cache_skip_valid_hit_refresh` 这组字段的根因非常集中：JSON / INI / import 面本来就通过 `options` set 工作，真正缺口只在 `Override(...)` 没把名字级输入同步进 `FOptions`。
- 这波不需要新增额外 builder 状态或 sync helper；最小且正确的修复就是在 `Override(...)` 里直接 `Include/Exclude` 对应 option。
- focused contract 用“override 导出结果必须与 builder methods 完全相同”来定义 parity，比重新手写 option ordinals 断言更稳，也更能防止后续默认值漂移。
- 新一层高概率缺口已经浮出：`server_name` / `alpn_protocols` / `session_cache_enabled` 的 builder methods 不只改数据字段，也会改 `FOptions`；而 `Override(...)` 当前看起来还没有完全对齐。


## Session 2026-03-09 (builder override advanced option parity)
- `server_name` / `alpn_protocols` / `session_cache_enabled` 的表面上看只是普通字段，但对应 builder methods 实际都会同步 `FOptions`，所以它们本质上是 option-coupled 字段。
- 初版 focused contract 如果直接从默认 builder 起跑，会被默认 `ssoEnableSNI` 与默认 session-cache 状态掩盖；用 `WithoutOption(...)` 和显式 `WithSessionCache(False)` 先剥离基线，才能看到真实 parity 缺口。
- 最小根因修复仍然在 `Override(...)` 分发层，不需要额外引入新的 sync helper。
- 下一层高概率缺口已很明确：`ImportFromJSON(...)` / `ImportFromINI(...)` 对这组 advanced fields 仍主要写字段本身；当外部输入不带 `options` 面时，field-set 与 option-set 仍可能漂移。


## Session 2026-03-09 (builder import advanced option parity)
- advanced option-coupled fields 的 import 缺口与 override 缺口本质相同：并不是字段值没进来，而是缺 `FOptions` 同步。
- `ImportFromJSON(...)` / `ImportFromINI(...)` 的正确边界不是“无条件重写 options”，而是“只有在输入缺失显式 `options` 面时，才用 field-only 输入补最小同步”。这样不会踩掉调用者明确给出的 options 集。
- 这波之后 advanced option-coupled 语义已经横跨三条入口对齐：override、JSON import、INI import；但重复实现也更明显了。
- 下一层最值当的工作不一定再是补功能，而是把这三处重复 sync 逻辑收拢成一个 helper，防止未来再出现单点漏改。


## Session 2026-03-09 (advanced option sync helper consolidation)
- advanced option sync 的真正风险点已经从“少某个字段映射”转成“同一语义分散在三处入口，各自手写一遍”，这类重复最容易在后续小改动时再次漂移。
- 抽 helper 时最重要的不是减少行数，而是保留两个不同入口语义：override 对 `server_name` / `alpn_protocols` 是无条件启用 option，而 field-only import 仍只在非空时启用。
- 现有 focused contracts 已经足够覆盖 helper refactor；这类收拢波次不一定需要再发明新行为测试，关键是用现有已绿合同守住外部语义。
- 下一层真正值得显式合同化的是空值边界：`server_name=''` / `alpn_protocols=''` 在 override 与 import 上仍不是完全同一语义，这是既有行为，不应在无合同的情况下被顺手重写。


## Session 2026-03-09 (builder advanced option empty value contract)
- `server_name` / `alpn_protocols` 的真正风险已经不是实现缺口，而是“override 空值启用 option”与“field-only import 空值不启用 option”这对既有分叉在没有显式合同时很容易被 helper refactor 抹平。
- 这波 focused contract 证明现有语义已经稳定存在，属于 contract codification wave，不是 bugfix wave；因此没有生产代码变更才是正确结果。
- 显式 `options` 仍是 import 路径上的最高优先级输入；即使 advanced fields 为空，显式 options 也不能被 field-only fallback 反向覆盖。
- 继续 builder 审查时，`Merge(...)` 是最值得单独合同化的下一站：它当前对 `server_name` / `alpn_protocols` 只在非空时赋值，因此 empty-value 更像“忽略”而不是“显式清空”。


## Session 2026-03-09 (builder merge advanced option snapshot semantics)
- `Merge(...)` 之前最危险的点不是 option set 本身，而是 source snapshot 已经显式关闭了 SNI / ALPN，目标 `FOptions` 也确实被替换了，但 `server_name` / `alpn_protocols` 因“仅非空才覆盖”而残留旧值，形成 field/option 漂移。
- `options=[]` 之前在 merge 路径上是不可表达的：source 明明导出了空数组，目标却因为 `Count > 0` 守卫而保留旧 option set；这属于明确的 snapshot 语义缺口。
- `Merge(...)` 还遗漏了 `ocsp_stapling_enabled` / `ocsp_stapling_required` 的布尔面复制；当 source options 清空时，这两个布尔值如果不一起覆盖，就会继续泄漏旧状态。
- 本波修复后，`Merge(...)` 在 advanced fields / empty option-set 上已经更接近真正的 source snapshot；下一层仍未收口的是其他字符串字段的 empty-value clearing 语义。


## Session 2026-03-09 (builder merge string-field empty-value snapshot semantics)
- `Merge(...)` 在 advanced fields 上已经转向 source snapshot 后，剩余最高风险点就是其他字符串字段仍保留“非空才覆盖”的旧语义，这会让同一个 merge surface 内部再分裂成两套规则。
- focused RED 证明了这一点：default source snapshot 明明导出了空字符串，目标上的 certificate / key / CA / PKCS#11 / cipher 字段却继续残留旧值。
- 最小且一致的修复不是再加特判，而是把这些字段统一改成“只要 source snapshot 明确提供，就覆盖；哪怕值为空字符串也一样”。
- 下一层真正更值钱的残余已很清楚：`FPrivateKeyPassword` 以及 backend-selection 状态还没有进入 snapshot surface，所以 merge 仍可能携带路径但丢失密码/后端约束。


## Session 2026-03-09 (builder private-key-password snapshot semantics)
- `private_key_password` 之前处在一个很危险的半接线状态：builder methods 与 `Override(...)` 都能设置它，build 路径也会消费它，但 snapshot surface 完全不导出、不导入、不 merge。
- 由于仓库已经接受 `pkcs11_pin` 进入 JSON / INI surface，`private_key_password` 继续留在 snapshot 外并不是“安全边界”，而是语义缺口。
- focused RED 证明了三层缺口同时存在：JSON/INI round-trip 丢密码，`Merge(...)` 既不能覆盖也不能显式清空目标 password。
- 下一层更清晰的缺口已经从 password 转到 backend-selection state：它不仅不在 snapshot surface，上层 `Clone(...)` 里也还没复制这些字段。


## 2026-03-09 Findings (builder backend-selection snapshot semantics)
- 根因不在 build path，而在 builder state surface：`FAutoSelectBackend` / `FBackendRequirements` / `FExplicitBackend*` 之前只存在于运行时内存里，`Clone` / JSON / INI / `Merge(...)` / `Reset` 都看不见它们。
- 最小安全修复是把完整 backend-selection state 一起带进 snapshot surface，并让 `Reset` 回到和 constructor 一致的默认值；否则一旦 snapshot 开始表达这组状态，`Reset` 会立刻暴露漂移。
- 这波修的是“状态可见性”，不是“状态归一化”：现在 snapshot 会忠实保留 `FAutoSelectBackend=True` 与 `FExplicitBackendSet=True` 共存的状态，而 build path 仍以 auto-selection 优先；这已经足够稳定，但仍有 API 语义噪音。
- `tests/config/test_config_snapshot_clone.pas` 里的 4 个 build-path 失败仍是既有测试夹具问题：它没有引入 backend registration fixture，所以和本波 snapshot/parity 变更解耦。


## 2026-03-09 Findings (builder backend-selection mode normalization)
- 仅仅把 backend-selection state 接进 snapshot surface 还不够；如果 `WithAutoBackendSelection` / `Require*` / `WithBackend` 彼此切换后仍残留 inactive state，JSON/INI round-trip 只会把噪音稳定地保存下来。
- 这波最稳的 contract 是 precedence normalization，而不是更多 ad-hoc 清字段：`auto` 优先于 `explicit`，`explicit` 再优先于“孤立 requirements”，最后才回到全默认空态。
- 这样做还能兼容上一波已落地的 snapshot surface：旧的 stale explicit/requirements 组合在 import/merge 时会被显式归一化，不再把 build 实际不会使用的字段继续传播下去。
- `Require*` 系列方法现在真正成为“切到 auto-selection mode”的 DSL，而不只是偷偷往 `FBackendRequirements` 上叠字段。

## 2026-03-09 Findings (config build-path fake backend fixture)
- `tests/config/test_config_snapshot_clone.pas` 与 `tests/config/test_config_import_export.pas` 的旧失败根因都不在 snapshot/import logic，而在测试进程没有预注册默认 backend；`TryBuildServer` 走到 `TSSLFactory.CreateContext(..., sslAutoDetect)` 时直接报 `No SSL library available. Please register a library first.`。
- 把 fake default backend 夹具限定在具体 build-path 测试块里，比给整个程序全局注册更稳：纯 JSON/INI/snapshot 用例不会被额外 backend state 污染。
- 将夹具抽成 `tests/config/test_fake_default_backend_fixture.inc` 能同时服务多个 standalone test program，而不需要改现有 `fpc -Fu./src ...` 编译命令。
- 新的 builder 审查点：`BuildClient` 会先把 `SelectedBackend := sslAutoDetect`，但 `BuildServer` 没有对应初始化；一旦 `FUseSystemRoots=True` 且未显式/auto 选后端，`CreateCertificateStore(SelectedBackend)` 就可能消费未定义值，见 `src/fafafa.ssl.context.builder.pas:1402`。

## 2026-03-09 Findings (builder server default-backend store consistency)
- 这不是只靠静态审查猜出来的理论问题；focused RED 已在当前环境里稳定复现 `EAccessViolation`，栈落在 `TSSLFactory.CreateCertificateStore(SelectedBackend)`，因为 `BuildServer` 的 implicit-default 路径没有先给 `SelectedBackend` 赋值。
- `BuildClient` 与 `BuildServer` 在 system-roots 路径上的差异之前是不必要的实现分叉：client 先做 `SelectedBackend := sslAutoDetect`，server 没做，同一 contract 被拆成了两套实现。
- 最小安全修复就是把 server 初始化对齐到 client；这波不需要动 factory，也不需要重构 builder 流程。
- 下一层更值得单开的不是 nil/崩溃问题，而是 implicit-default 仍会分两次解析 backend：一次给 `CreateContext(..., sslAutoDetect)`，一次给 `CreateCertificateStore(sslAutoDetect)`；若后续想进一步收紧语义，应该先 resolve concrete backend 再复用。

## 2026-03-09 Findings (builder implicit-default backend resolution consistency)
- 上一波把 `BuildServer` 的未初始化值修掉后，implicit-default 路径仍有更深一层 contract 缺口：context 与 store 会各自对 `sslAutoDetect` 发一次 factory 调用，这意味着 default backend 只要在两次调用之间变化，就会发生跨 backend 漂移。
- focused RED 用一个在 `CreateContext(...)` 里切换 default backend 的 fake library，把这个窗口稳定复现成可见 fail；因此这不是“理论上可能”，而是当前 contract 未被约束。
- 最小安全修复不是改 factory，而是在 builder implicit-default 分支先拿到 concrete `GetDefaultLibrary` 结果，再把同一个 `SelectedBackend` 复用给 `CreateContext` 和 `CreateCertificateStore`。
- 下一层更有价值的是抽共享 helper：现在 client/server 仍复制同一套 auto/explicit/default 分支，只是恰好再次对齐了；如果不抽 helper，未来还会再分叉。

## 2026-03-09 Findings (milestone 1 kickoff)
- 用户反馈的核心问题不是某个具体 bug，而是推进方式太碎；因此从这条消息开始，开发节奏切换为 milestone-driven，而不再是零散波次拼接。
- Milestone 1 选择 `builder` / `factory` / backend 语义统一作为主线，是因为它同时连接了近期绝大多数真实问题：默认 backend、system roots、default-config 边界、测试夹具噪音。
- 这类主线任务的关键不是一次大改，而是按 contract 连续收口：先 backend resolution，再 helper 抽取，再 default-config boundary，最后测试平台化与文档同步。


## 2026-03-09 Findings (builder backend-resolution helper consolidation)
- 上一波 implicit-default 收口后，`BuildClient` / `BuildServer` 里仍各自维护一份 auto / explicit / default 分支；真正剩余的语义缝是 `WithBackend(sslAutoDetect)` 仍落在 explicit 分支，却把未解析的 `sslAutoDetect` 继续传给 `CreateContext` / `CreateCertificateStore`。
- drifting fake backend 的 focused RED 证明这不是边缘理论：只要 default backend 在 `CreateContext(...)` 期间被切换，explicit-`sslAutoDetect` 的 context 与 system-roots store 就会漂移到不同 backend。
- 这类问题的根治点不是在 client/server 各自再补一个条件，而是把 backend 解析收口成单一 helper，让 build path 先拿到一次性 resolved concrete backend，再交给 context/store 复用。
- 对 explicit `sslAutoDetect` 与 implicit default，`TSSLFactory.GetDefaultLibrary` 是正确的 concrete resolver：factory 已负责把默认 auto-detect 收敛成 concrete backend 并缓存，builder 不需要重复复制 factory 的检测细节。
- Milestone 1 的下一主线应转向 default-config / request-scope / context-scope boundary；backend-resolution duplication 已降下来，再继续停留在 builder 会边际递减。


## 2026-03-09 Findings (factory default-config material boundary)
- `ISSLLibrary.SetDefaultConfig` 之前只有 runtime dead-field 校验，没有区分 request-only material fields；结果是 `CertificateFile` / `PrivateKeyFile` / `PrivateKeyPassword` / `CAFile` / `CAPath` 可以写进 backend defaults，但 backend `CreateContext(...)` 实际完全不会消费它们。
- 这类问题比“静默忽略”更糟，因为 `GetDefaultConfig` 会把这组字段原样 visibleize 出去，让调用方误以为 default-config 会驱动证书/CA 装载。
- 最小正确修复是新增 library-default scoped validator，而不是把规则塞进 request validator：request path 仍然需要支持这些 material fields，真正要收紧的是 `SetDefaultConfig(...)` 这条库级入口。
- 这波后 default-config / request-scope 边界已经更清晰，但 `LibraryType` / `ContextType` 仍属于可见但语义含混的字段：backend defaults 当前会保存它们，实际建 context 时又会覆盖 `ContextType`，下一波应该把这条 contract 明文化。


## 2026-03-09 Findings (factory default-config owner fields contract)
- `LibraryType` / `ContextType` 在 library default config 上并不是普通业务配置字段，而是 owner fields：backend instance 自己拥有真实 `LibraryType`，`CreateContext(AType)` 也总会在运行时覆盖 `ContextType`。
- 之前 `SetDefaultConfig(...)` 直接持久化这两个字段，会让 `GetDefaultConfig` visibleize 出“错 backend / server context”这类看起来可配置、实际不生效的噪音状态。
- 相比直接 reject，更稳的 contract 是 normalize：允许调用方继续复用完整 `TSSLConfig`，但 library default path 会把 owner fields 收回 backend-owned baseline，同时保留其余真正可配置字段。
- 这样既不打断现有 default-config 调用习惯，又能让 `GetDefaultConfig` 成为可信的“当前真相”；下一阶段就更适合转向测试夹具平台化，而不是继续围着 default-config 噪音打转。


## 2026-03-09 Findings (test fixture platformization)
- `tests/config/test_fake_default_backend_fixture.inc` 提升到 `tests/helpers/` 后，config build-path suites 与 builder/store consistency suites 可以共享同一份 fake backend 基座，而不必复制类型定义。
- 旧路径保留薄包装 include 是这波的关键降风险点：既完成了平台化，又没有强迫现有 standalone `fpc -Fu./src ...` 命令一次性重写 include 路径。
- `tests/test_context_builder_backend_store_consistency.pas` 把 drifting/default fake backend 收走后，后续继续补 backend-resolution 合同时，注意力终于可以回到 contract 本身，而不是重复维护一大段夹具代码。
- 这类 fixture 平台化应该坚持“最小共享面”原则：只提炼已经跨 suite 复用的 fake backend，不要为了整齐感过早做大规模测试框架化。

## 2026-03-09 Findings (docs contract truth sync)
- 近期 builder/default-config 波次已经把源码行为收口，但公共文档仍停留在泛化描述；这会让后续 review 又回到“读代码猜 contract”的高成本路径。
- 对当前仓库最有价值的公共真相不是再写一份大而全手册，而是把四条稳定规则写在入口上：builder 单次 resolve backend、material fields 属于 request/context scope、logging 属于 library scope、owner fields 在 library defaults 上 normalize。
- `README.md`、`docs/README.md`、`docs/reference/ARCHITECTURE.md` 这三个层级已经足够构成“短入口 + 导航入口 + 设计细节”三段式说明；在 contract 发生下一次实质变化前，不应再新增平行说明文档。
- Milestone 1 下一波最值得回到的仍是 backend context/default-validation 架构复审，而不是继续围绕文档表述做表层修补。


## 2026-03-09 Findings (context builder server-name context parity)
- `BuildClient` 与 `TSSLFactory.CreateContext(const AConfig)` 都会把 `ServerName` 写入 context，但 `BuildServer` 之前只应用了 `ALPNProtocols`，把同一配置面在 server 路径上静默降级成了 no-op。
- 这波最小正确修复不是重定义 `ServerName` 的大架构，而是先让 builder server/client 与 factory request path 保持同一份 context-level contract；否则调用方会在不同入口上得到不同结果。
- 当前接口层仍把 `ServerName` 暴露在 `ISSLContext` 上，因此 `BuildServer` 保留它比静默丢弃更一致；真正“是否应下沉到 connection-only”是下一层 DSL 设计题，不该在这波无合同地顺手改语义。
- 这也说明下一波 backend context/default-validation 复审应把“context-level 字段是否其实属于 connection-level”单独拎出来，而不是继续把它们和 library-default scope 混在一起讨论。

## 2026-03-09 Findings (connection context server-name inheritance parity)
- OpenSSL / WinSSL / WolfSSL / MbedTLS 在 `CreateConnection(...)` 构造阶段都已经继承了 context 默认 `ServerName`，FreePascal 单独把 `FServerName` 重置为空，导致同一 context 配置在不同 backend 上表现不一致。
- 这个缺口位于 connection 构造层，而不是 builder/context 写入层；因此即使上一波已经让 `BuildServer` 保留了 `ServerName`，FreePascal 连接路径仍会把它静默吃掉。
- 这波最小安全修复是让 FreePascal 构造函数复用现有 `SetServerName(...)` 路径继承 context 默认值，而不是中途发明新的 per-connection 配置 API。
- 下一波应把优先级顺序显式合同化：context default → connection override → backend native setup；同时明确 server-mode 连接是否应忽略 client SNI。

## 2026-03-09 Findings (connection server-mode SNI isolation)
- 各 backend 此前都把 context 默认 `ServerName` 无条件注入新连接；这对 client backward compatibility 有帮助，但会把 client-only SNI 语义泄漏到 server-side connection。
- `ISSLClientConnection` 的接口说明已经把 `ServerName` 定义为 per-connection、client-specific；因此 server context 不自动继承该默认值，比继续在 server path 上携带它更符合接口边界。
- 这波修复不是否认 context 仍可保留 `ServerName` 状态，而是明确“保留在 context ≠ server-side connection 必须继承”；这与上一波 `BuildServer` 保留 context-level 值并不冲突。
- 下一波最值得合同化的是 precedence：context default 只是 backward-compatible fallback，真正优先级应是 connection override > context default > empty。

## 2026-03-09 Findings (openssl server-name clear override parity)
- OpenSSL client connection 在 context 默认值注入后，`SetServerName('')` 之前只清空了 `FServerName` 字段，没有清掉底层 `SSL` 句柄里的 SNI，因此 `GetConnectionInfo.ServerName` 仍泄漏旧值。
- 这说明 precedence 不能只看对象字段，还要看 native-handle state；否则调用方以为空 override 生效，实际握手仍可能带着旧 hostname。
- 这波最小修复是让 `TOpenSSLConnection.SetServerName` 在空字符串时走 native clear 路径，而不是在 builder/context 层额外塞特殊分支。
- 下一波值得扩展的方向是核对其他 native backend 是否也存在“field cleared but native state retained”的同类缺口，尤其 WolfSSL / MbedTLS。


## 2026-03-09 Findings (connection builder hostname override precedence)
- builder 之前只在 `FHostname <> ''` 时才调用 `ISSLClientConnection.SetServerName(...)`，因此 non-empty override 可以覆盖 context fallback，但 empty override 无法表达，语义上把“未设置 override”和“显式清空 override”混成了一种状态。
- 在 connection 已经从 context 继承了默认 `ServerName` 的前提下，这个歧义会让调用方以为 `WithHostname('')` 清空了 SNI，实际连接对象仍保留旧 hostname。
- 这波最小安全修复不是改 context/connection 的 owner 关系，而是在 builder 内部补一个显式 `FHasHostnameOverride`，只解决三态表达能力缺口。
- 明确后的 precedence 是：`connection override > context default > empty`；其中 empty override 也是 override，不应再回退到 inherited fallback。
- 下一波值得继续核对 `TSSLConnector.ApplyClientOptions(...)`：它也仍然只在 `AServerName <> ''` 时才下发 hostname，可能存在同类空值歧义。


## 2026-03-09 Findings (tls connector hostname override precedence)
- `TSSLConnector.ApplyClientOptions(...)` 此前与 builder 有同类歧义：只在 `AServerName <> ''` 时才调用 `ISSLClientConnection.SetServerName(...)`，导致调用方无法用 `ConnectStream(..., '')` 显式清空 inherited context fallback。
- 由于 connector API 的 hostname 参数本身就是 connection-level 输入，空字符串不应被解释成“未提供”；它表示“本连接明确不带 SNI”。
- 这波最小修复是让 connector 对 supported client connections 始终下发 `SetServerName(AServerName)`，并仅在 non-empty hostname 且 backend 不支持 per-connection server name 时保留原来的 unsupported error。
- 代码审查还显示 WolfSSL / MbedTLS 目前只有 one-way 的 `wolfSSL_UseSNI(...)` / `mbedtls_ssl_set_hostname(...)` 设置路径，仓内暂未看到与 OpenSSL `GetConnectionInfo.ServerName` 对应的可观测 native SNI 读取面；下一波若要做 clear-path parity，最好先补可观测合同或 handshake-based 证据。


## 2026-03-09 Findings (wolfssl / mbedtls server-name observability parity)
- WolfSSL 真实运行路径并不走 `src/fafafa.ssl.wolfssl.connection.pas`，而是走 `src/fafafa.ssl.wolfssl.context.pas` 里的内嵌 `TWolfSSLConnection`；这类“双轨连接实现”非常容易让修复落在死路径上。
- focused RED 证明 WolfSSL / MbedTLS 的真实连接类此前虽然有 `SetServerName/GetServerName` 方法，但未声明实现 `ISSLClientConnection`，导致 builder / connector 的 per-connection hostname contract 在这两个 backend 上并不成立。
- 仅靠对象字段不足以证明 parity；必须把 `GetConnectionInfo.ServerName` 的原生证据面补上，否则无法区分“字段被清空了”和“native handle 仍保留旧 hostname”。
- MbedTLS 的 clear-path 可通过 `mbedtls_ssl_set_hostname(nil)` 成立，且 `mbedtls_ssl_get_hostname_pointer` 能给出可观测证据。
- WolfSSL 的 clear-path 更微妙：`wolfSSL_UseSNI(..., nil, 0)` 不能清掉旧值，`wolfSSL_set_tlsext_host_name(nil)` 还会触发 access violation；当前可工作的路径是 `wolfSSL_set_tlsext_host_name('')`。
- 下一波高价值项不是继续补更多边角测试，而是先收口 WolfSSL 的双轨连接实现，避免后续修复再次打到未使用单元上。


## 2026-03-09 Findings (wolfssl standalone connection compatibility contract)
- `src/fafafa.ssl.wolfssl.connection.pas` 虽然不走当前 runtime 工厂主链路，但它会被 compile-all 编译，也可能被外部用户直接 `uses`；因此它不是“可以完全忽略的死代码”，而是“会继续积累漂移风险的兼容入口”。
- 首轮 compatibility RED 暴露的是 `ISSLClientConnection` / `ServerName` 漂移；后续 native handle RED 进一步证明，兼容面不只包含 SNI，还包含 `ISSLNativeHandleAccess` 这类外部调用方可能依赖的 runtime contract。
- 这波真正的 root fix 不是继续给 standalone 单元补洞，而是把它改成委托 `AContext.CreateConnection(...)` 的 thin shim，让 `ServerName`、connection info、native handle 直接复用 runtime path 的单一真相源。
- 追加结构审计显示，当前“context 内再藏一份连接实现 + 独立 connection 单元”这一类双轨结构主要集中在 WolfSSL；OpenSSL / WinSSL / FreePascal / MbedTLS 当前仍以 context -> connection unit 为主链。
- 下一波更高价值的收口点已经变清楚：OpenSSL / WinSSL / FreePascal 构造函数里还分别复制着 `AContext.GetServerName` 的 backward-compatible fallback，这条 deprecated 路径是新的漂移热点，适合抽成共享 helper 或统一入口。


## 2026-03-09 Findings (connection context server-name fallback helper centralization)
- OpenSSL / FreePascal / WinSSL / MbedTLS 原本都各自复制了 deprecated context-default `ServerName` fallback；其中 MbedTLS 的复制点甚至不在构造函数，而在 `AllocateSSLContext`，说明这类漂移不能只靠“看 constructor”发现。
- 这波更稳的 root fix 不是继续逐文件 suppress deprecated warning，而是把“读取 legacy fallback”的动作统一收口到 `TBaseSSLConnection.GetLegacyContextDefaultServerName`。
- 共享 helper 只统一读取时机与来源，不统一 backend-specific 应用路径；这保持了 OpenSSL native SNI 设置、WinSSL 字段缓存、MbedTLS runtime setup 的各自边界。
- 现在 runtime 连接层已经不再直接读 `AContext.GetServerName`；后续若继续治理，应把焦点转向 deprecated API 的迁移策略，而不是再做散点去重。


## 2026-03-09 Findings (server-name migration policy)
- 仅靠 `deprecated 'Use per-connection SNI...'` 这类编译提示，不足以让调用方理解 `ISSLContext.ServerName` 现在到底还承担什么语义；README 和架构文档之前都缺少这层明确说明。
- 更稳的治理方式不是立刻移除 context-level API，而是先把它降格为“兼容桥接层”并写清边界：只影响后续 client connection 的默认 fallback，server connection 不继承。
- 这波之后，`ISSLClientConnection.SetServerName(...)` 已经在示例、接口注释、架构文档三层形成同一条推荐路径；未来若继续收缩 deprecated surface，会更容易评估影响面。
- 下一波的价值重点不在继续写更多 ServerName 文档，而在 WolfSSL shim policy 和 warning/noise 这些更高风险的实现层问题。


## 2026-03-09 Findings (wolfssl standalone shim policy)
- 仅靠当前实现“看起来像 shim”并不够；如果源码头和架构文档不写清楚，后续维护者仍可能把 standalone 单元重新当成第二套运行时实现去扩展。
- 把 runtime 真相源明确钉在 `TWolfSSLContext.CreateConnection(...)` 上之后，WolfSSL 的维护边界就更清楚了：行为修复优先动 runtime path，standalone 单元只做兼容桥接。
- 这一波的价值主要是防回退，不是新增功能；它把未来 drift 的入口再缩小了一层。
- 当前更值得投入的是 WolfSSL / MbedTLS 的 warning/noise，而不是继续追加 shim prose。


## 2026-03-09 Findings (wolfssl/mbedtls focused warning noise cleanup)
- focused contract 最终只剩两个 backend-local 残点：`TMbedTLSCertificateStore.BuildCertificateChain` 仍对未初始化的 managed result 调 `SetLength(Result, 0)`，以及 `TWolfSSLLibrary.IsProtocolSupported` 缺少 catch-all `else`。
- 对 dynamic array / bytes 这类 managed result，`Result := nil` 才是稳定零值初始化；继续使用 `SetLength(Result, 0)` 只会反复触发 FPC 的“managed type does not seem to be initialized” warning。
- 这波也验证了“合同边界要窄”的价值：runtime regressions 仍会看到 `src/fafafa.ssl.openssl.api.sha.pas` 的 unrelated warning，但 focused contract 不再让它污染 WolfSSL / MbedTLS 的 backend-local 进度。
- `python3 -u scripts/compile_all_modules.py` 维持 `231/231`，说明这是一波纯 warning-noise 收口，而不是用行为回退换安静日志。
- 当前最高价值的下一波已从 backend-local Pascal warning 清理转回脚本链治理：优先做 Wave C quick-sprint / enablement，再做 Wave B / TLS13 gate，然后单开 test-reports 历史面清理。


## 2026-03-09 Findings (Wave C quick-sprint spaced-path safety)
- `scripts/run_wave_c_quick_sprint_bundle.sh` 的字符串 `eval` 与 B108/B109/B110 的裸 `ls -1t $VALIDATION_GLOB` 是同一类路径安全缺口的两半；只修 bundle 会让 B107 先绿，但 B108/B109/B110 仍会因含空格 glob 解析失败而报 `Missing input reports`。
- quick-sprint reports (`tmp/wave_c_quick_sprint_reports`) 与 enablement reports (`tmp/wave_c_enablement_reports`) 的双目录策略本身没有问题；现有 default reports 合同保持绿色，说明真正的问题在执行/解析路径，而不是目录划分。
- 这波说明脚本链治理的高价值切口不是再改默认目录名，而是优先消灭 `eval` 与裸 glob 解析这种可复用的脆弱模式。
- 同类热点已经在 `scripts/run_tls13_signer_gate_bundle.sh:92` 复现出来；Wave B / TLS13 下一波可以直接复用“spaces contract + 参数式执行”模板。


## 2026-03-09 Findings (TLS13 signer gate bundle quoted-path safety)
- `scripts/run_tls13_signer_gate_bundle.sh` 的 `eval` 并不是抽象味道问题，而是可复现的真实故障：`--reports-dir` 含单引号时，CI/snapshot/status 三个步骤都会在 shell 解析层失真。
- 这波最小正确修复是在 bundle 层去掉字符串命令拼接，改成参数式执行与 `env` 传参；snapshot/status 子脚本本身不需要改。
- fake `bash` stub 证明 bundle 的高价值合同应聚焦“参数传递正确性”，而不是把重型 CI 编译过程拖进合同里。
- `scripts/run_wave_b_ci_gate.sh` 目前仍保留同类 shell-string 执行面；Wave B 下一波最该收的不是文档，而是这条主入口的同型路径安全风险。


## 2026-03-09 Findings (Wave B CI gate quoted-path safety)
- `run_wave_b_ci_gate.sh` 的真正价值点不是把字符串命令“格式化得更漂亮”，而是把执行层从 `-lc` shell-string 中解耦出来；quoted reports-dir 失败只是最容易复现的症状。
- dry-run / summary / contract 仍需要旧 shell 风格字符串，因此这波最小正确修复不是删掉 display string，而是把“展示给人看的命令”和“真正执行的参数数组”拆成两层。
- 这也解释了为什么旧的 FPC host passthrough / isolation passthrough contract 会在第一次重构后失败：它们验证的是 display surface，不是执行 surface。
- 当前 Wave B/TLS13 主线最接近的剩余风险已从 bundle/主入口下沉到发现层：`generate_tls13_signer_gate_snapshot.sh`、`export_tls13_signer_gate_status_json.sh`、`check_wave_b_b2_closure_readiness.sh`、`check_wave_b_b2_evidence_consistency.sh`。


## 2026-03-09 Findings (Wave B B2 closure run-id scoped default)
- `check_wave_b_b2_closure_readiness.sh` 的缺口不在判定规则，而在默认发现：只给 `--run-id` 时，Linux summary 仍会捡“最新任意 run”的文件，导致当前 run 明明证据齐全却被 newer distractor run 拉成 `IN_PROGRESS`。
- 这波最小正确修复不是把 closure checker 扩成全自动平台发现器，而是仅把 Linux 默认 summary 收到 run-id scoped + latest fallback 两段式。
- 既有 dryrun/skipped contract 与 windows blocker linkage contract 都继续绿色，说明这波确实只修了默认发现，不是改闭环语义。
- 当前更值得继续下沉的是 `check_wave_b_b2_evidence_consistency.sh` 的默认发现与 linked evidence 细边界，而不是再回头扩 closure 自动发现面。


## 2026-03-09 Findings (Wave B B2 evidence examples run-id consistency)
- `check_wave_b_b2_evidence_consistency.sh` 之前对 examples JSON 的严格模式只检查“是不是合法 JSON”，没有检查它是否属于当前目标 run，因此显式 run-id 漂移会被静默放过。
- 最小正确修复不是把 evidence checker 升级成重型 schema validator，而是只在 JSON 明确暴露 `run_id` 时做一致性判定；top-level、`metadata.run_id`、`summary.run_id` 已足够覆盖当前报告形态。
- 对 legacy JSON 维持“无 `run_id` 仍可接受”的 backward-compatible 语义很关键，否则会把旧历史报告一次性打成噪音，阻断当前脚本链继续收口。
- windows blocker linkage、closure run-id scoped default、Wave B/TLS13 default reports 回归都继续绿色，说明这波确实只收紧 examples 证据一致性，没有把默认发现或 blocker 语义带偏。

## 2026-03-09 Findings (Wave B B2 handoff run-id scoped Linux summary)
- `prepare_wave_b_b2_handoff_bundle.sh` 之前的默认发现与 closure/evidence 的新语义不在一个时代：下游 checker 已经按 run-id scoped 在收紧，但 handoff 入口仍用 `ls -1t` 捡“最新任意 Linux summary”，会把整条链一上来就带偏。
- 这波最小正确修复不是重写 handoff bundle 参数面，而是只把 `LINUX_SUMMARY` 默认发现收口成 `run_id exact -> latest fallback` 两段式。
- 保持 `examples_compile_ci_gate.json` 继续用静态默认名是合理的范围控制；当前真实问题是 summary 漂移，不是 examples artifact 命名策略。
- handoff consistency snapshot、closure semantics snapshot、windows blocker artifact visibility、Wave B/TLS13 default reports 回归都继续绿色，说明这波只修了默认发现，没有改 handoff 报告语义。

## 2026-03-09 Findings (Wave B cross-summary run-id scoped Linux default)
- `generate_wave_b_cross_platform_summary.sh` 的默认发现此前仍停留在“最新文件即当前真相”的假设上；一旦 reports 目录里出现 newer distractor run，平台状态和 checklist 会整体漂移。
- 这波最小正确修复不是扩展 cross-summary 的参数面，而是仅把 `LINUX_SUMMARY` 默认解析收口成 `run_id exact -> latest fallback` 两段式。
- `examples_compile_ci_gate.json` 继续保持静态默认名是合理的范围控制；当前真实问题是 Linux summary 漂移，而不是 examples artifact 命名策略。
- cross-summary 既有 checklist/default-output/android/windows-blocker 回归都继续绿色，说明这波只修了默认发现，没有改摘要语义。

## 2026-03-09 Findings (TLS13 status stale snapshot fallback)
- `export_tls13_signer_gate_status_json.sh` 的 `snapshot` 与 `summary` / `bench_json` 不是同一类工件：前者是派生摘要，跨 run fallback 会直接把目标 run 的状态污染成别人的结果。
- 这波最小正确修复不是让 status export 自动生成 snapshot，而是把默认 snapshot 解析收紧成 exact-only；缺失时显式落到 `MISSING/ATTENTION` 更安全。
- `summary` / `bench_json` 继续保留 `run_id exact -> latest fallback` 是合理的，因为它们仍然是 source evidence，且既有 default selection contract 依赖这条便利语义。
- stale snapshot focused contract、run-id scoped default selection、Wave B/TLS13 default reports 回归都继续绿色，说明这波只修了 snapshot fallback 的工件边界，没有改 TLS13 正常绿路。

## 2026-03-09 Findings (TLS13 snapshot stale history fallback)
- `generate_tls13_signer_gate_snapshot.sh` 的 `history` 与 `summary` / `bench_json` 不是同一类工件：前两者是当前 run 的 source evidence，`history` 更像派生上下文，跨 run fallback 只会制造误导。
- 这波最小正确修复不是自动重建 history，而是把 default history 解析收紧成 exact-only；缺失时直接显示 `<none>` 更符合 snapshot 的职责边界。
- 这和上一波 `status stale snapshot` 的结论一致：派生工件默认不应跨 run 借用，源工件才保留更宽松的 fallback。
- stale history focused contract、run-id scoped default selection、status stale snapshot fallback、Wave B/TLS13 default reports 回归都继续绿色，说明 TLS13 这条链的 fallback 语义正在变得更清晰而不是更脆。

## 2026-03-09 Findings (Wave B CI gate examples report run-id)
- `examples_compile_ci_gate.json` 之前是静态路径 + 无 `run_id` 的组合，这意味着 evidence checker 即便已经具备 run-id 严格判定能力，也只能在“JSON 恰好带 run_id”的幸运场景下生效。
- 这波最小正确修复不是改默认文件名，而是先让生产侧把 `run_id` 明确写出来；这样既不打破现有静态路径契约，也能显著提升 stale report 可观测性。
- 在 gate 层导出 `FAFAFA_WAVE_B_CI_GATE_RUN_ID`，比在调用命令行上追加更多 display-string 更稳，且不会打破现有 quote/isolation/FPC passthrough 合同。
- 新合同、Wave B CI gate 既有 contracts、B2 evidence examples run-id、Wave B/TLS13 default reports 全部继续绿色，说明这波是在补 observability，而不是改 gate 业务语义。

## 2026-03-09 Findings (Wave B examples run-scoped default chain)
- 仅仅给静态 `examples_compile_ci_gate.json` 增加 `run_id` 还不够；只要 consumer 默认仍直接吃静态路径，stale 报告仍然会污染 cross-summary / handoff / evidence 的默认链路。
- 这波最小正确修复不是废弃静态默认文件，而是在 producer 侧额外生成 `examples_compile_ci_gate_<run_id>.json`，再让 consumer 默认优先读这个 run-scoped 副本。
- 保留静态 alias 非常重要：现有文档、历史引用和默认入口仍依赖 `examples_compile_ci_gate.json`，贸然移除会制造新的治理噪音。
- 三条 focused contracts 与一组既有 gate/cross-summary/evidence/runtime 回归全部继续绿色，说明这波是在稳态地加“current run truth surface”，不是大改接口。

## 2026-03-09 Findings (wave b examples static fallback run-id guard)
- dual-surface examples default consumer 不能只看“静态 alias 是否存在”，还必须看它是否真的属于当前 `run_id`；否则 stale static 仍会污染默认链路。
- `check_wave_b_b2_evidence_consistency.sh` 之前对 stale static 的“防串味”是偶然行为：`resolve_preferred_examples_artifact` 在 helper 未定义前就调用了 `resolve_path`，因此 same-run static fallback 也一起失效了。
- 这波收敛后的安全规则是：`explicit override -> run-scoped exact -> same-run static fallback -> missing`；这样既阻止 stale alias 串味，又保留当前 run 的兼容兜底。
- 下一波最值得 contract 化的是 static alias 的治理边界：谁负责 current alias、何时清理 stale、是否保留 archive copy，以及显式覆盖输入是否要在 strict/default 报告里留下 warning note。


## 2026-03-09 Findings (wave b examples current alias / archive governance)
- 把 stale-static fallback 收紧之后，producer 侧仍需要把“谁是 current alias、谁是 history”明确写出来；否则 reviewer 仍要从文件名和目录习惯里猜语义。
- 最低成本的治理边界是三层：top-level static alias 表示 current、top-level run-scoped copy 表示当前 run 精确证据、history bucket 只保留 run-scoped archive copy。
- history bucket 中的 generic static alias 是高噪音对象：它既不是 current alias，也不属于稳定历史命名，因此最稳的策略是 producer 每次写 history 时主动清掉它。
- 下一波最值钱的是 contract 化显式 override 的 owner/warning 语义；因为显式覆盖会绕过 default discovery，如果不留下 note，review 时仍会失去“这个报告是不是当前真相”的可见性。


## 2026-03-09 Findings (wave b examples selection observability)
- 仅仅把 explicit override 路径写进产物还不够；reviewer 仍然不知道这是“手工覆盖”还是“默认发现”。
- 给 consumer 增加统一的 `linux_examples_selection` 字段，是最小但高价值的治理动作：它不改变行为，却让 strict/default 审查有了可读的来源上下文。
- 一旦 selection source 可见，后续 warning contract 就可以非常窄：只针对 `explicit_override` 加 note，而不需要重新推断 default resolver 走了哪条分支。
- 下一波最值得做的是 explicit override warning note；其次是 history retention 的轻量命名约束，避免新噪音继续长出来。

## 2026-03-09 Findings (wave b examples explicit override warning)
- `linux_examples_warning` 最稳的实现方式是完全依赖既有 `linux_examples_selection`：只有 `explicit_override` 产出 warning，其余统一 `none`，不需要再引入新的 strict 分支。
- `resolve_preferred_examples_artifact` 不能再放进命令替换里调用；否则函数内部对 `LINUX_EXAMPLES_SELECTION` / resolved-path 变量的赋值会丢在 subshell，外层 consumer 看到的仍是旧默认值。
- 仅修 resolver 还不够：`prepare_wave_b_b2_handoff_bundle.sh` 如果把 default-resolved path 重新用 `--linux-examples` 传给子脚本，会把 child consumer 的来源语义折叠成 `explicit_override`。
- 所以 handoff fan-out 需要区分“真实显式 CLI override”和“本地默认解析结果”：只有前者才向子脚本转发 `--linux-examples`；env override 则继续依赖继承环境。
- 这波完成后，Wave B examples consumer 的语义稳定为：default path 保持 `run_scoped_exact` / `static_same_run_fallback` / `run_scoped_missing`，只有真实显式覆盖才显示 warning。

## 2026-03-09 Findings (wave c enablement cli reports-dir passthrough)
- Wave C quick-sprint bundle 早已具备 `--reports-dir`，但 B115/B116/B119 仍只支持 env，导致 caller 一旦要切目录，就不得不把 orchestration 分裂成“bundle 用 CLI、enablement 用 env”的混合面。
- 对 B115/B116 而言，`--reports-dir` 的真正价值不只是控制输出路径，还要驱动 B116 默认 latest B115 lookup；否则 CLI override 只能写 output，不能带动默认发现闭环。
- B119 和 B115/B116 不同，它天然依赖两类目录：自己的 enablement 输出目录，以及 quick bundle 的发现目录，所以需要拆成 `--reports-dir` 与 `--quick-reports-dir` 两条显式 surface。
- 这波完成后，Wave C quick / enablement 链的目录控制已经从“默认靠 env、局部靠 CLI”收敛到一致的 caller-facing 模型；下一波更值得下沉到 Wave B / TLS13 gate 的发现与 passthrough 一致性。

## 2026-03-09 Findings (tls13 snapshot status cli reports-dir passthrough)
- TLS13 signer gate 的目录控制面此前是裂开的：bundle 有 `--reports-dir`，但 snapshot/status 直连脚本仍是 env-only；这会让 direct caller 与 wrapper caller 的 orchestration 模式不一致。
- 对这两支脚本来说，补 `--reports-dir` 已经足够，因为它们的显式 evidence 覆盖参数（`--summary` / `--bench-json` / `--history` / `--snapshot`）本来就存在；真正缺的只是 default discovery 所依赖的 base dir 入口。
- 新合同证明这波修复没有碰坏之前的 stale-fallback 收口：CLI 切目录后，run-id scoped selection 和 exact-only stale guard 仍然按原来的语义工作。
- 这也意味着 Wave B/TLS13 主线上剩余高价值尾项，已经从“目录控制面缺口”继续收敛到更细的 linked evidence / history hygiene 边界。

## 2026-03-09 Findings (wave b b2 closure/evidence cli reports-dir passthrough)
- Wave B B2 direct caller 的目录控制面此前也是裂开的：closure/evidence 都有 `REPORTS_DIR` 概念，但只有 env surface，没有 CLI surface。
- 因为两个脚本的默认发现与默认输出都已经统一依赖 `REPORTS_DIR`，所以补 `--reports-dir` 是真正的最小修复；不需要再碰 linked evidence 判定逻辑。
- 新合同证明这波修复没有打破前面收口过的 examples selection / warning / run-id consistency：caller-facing surface 扩展与 evidence 语义本身可以独立演进。
- 这也说明 Wave B/TLS13 尾项已经越来越集中到 wrapper 层的 orchestration surface，而不是底层判定规则本身。

## 2026-03-09 Findings (wave b handoff cli reports-dir passthrough)
- handoff wrapper 的缺口不是缺一个参数那么简单，而是 input/output 两个目录面的时序绑定：`OUTPUT_DIR` 如果在 parse 前就吃掉旧默认值，单补 `--reports-dir` 也会把产物写错地方。
- wrapper 层修目录面时，还必须同步考虑 child consumer 的默认发现来源；否则 parent 在新目录里找到了 Linux summary/examples，child 却仍会回到旧默认目录继续找 cross/evidence 依赖。
- 把 `FAFAFA_WAVE_B_REPORTS_DIR` 在 fan-out 时显式透传给 child consumer，是这类 wrapper 修复里最稳的做法；它不需要重新暴露更多 display 参数，也不改变 child 脚本的显式覆盖规则。
- 到这一步，Wave B/TLS13 主线上 caller-facing 目录控制面已经基本闭环，剩余更值得做的是历史面清理和少量 linked-evidence 细边界。

## 2026-03-09 Findings (Wave B CI gate CLI reports-dir passthrough)
- `run_wave_b_ci_gate.sh` 的默认 reports 派生链其实早就完整：examples JSON、summary、TLS13 bench JSON 和各 step log 都是从 `REPORTS_DIR` 展开的；缺的只是 direct caller CLI 输入面。
- 这波最小正确修复不是改 artifact 结构或 fallback 规则，而是仅补 `--reports-dir` 参数解析与 usage 文案，让 CI gate 与后续 Wave B / TLS13 consumer 的 caller-facing 目录控制面一致。
- 显式 `--examples-report` / `--summary-out` / `--tls13-sign-bench-json-out` 继续保留更高优先级，因此这波不会改变现有 mixed-output 自定义能力。
- fresh check 显示 `git ls-files 'test-reports/*' | wc -l` 已是 `0`；因此“历史 tracked test-reports 清理波次”不再是当前优先队列，下一波更值得做 mixed override 与 linked evidence 边界审计。

## 2026-03-09 Findings (Wave B CI gate mixed output priority)
- `run_wave_b_ci_gate.sh` 的路径优先级解析本身没有错；真正的问题是输出目录创建策略不一致：它只创建了 `REPORTS_DIR`，却没有保证显式 `--summary-out` 的父目录存在。
- `verify_examples_compile.sh` 的 `-o/--output` 也存在同类根因：不会主动创建目标父目录，导致 direct caller 或 wrapper 传入嵌套路径时，examples JSON 可能静默丢失却仍返回 0。
- 这波最小正确修复是分别在真正的 writer 侧补 `dirname mkdir`：`verify_examples_compile.sh` 负责 examples JSON，`run_wave_b_ci_gate.sh` 负责 summary，并顺手为显式 bench/output 路径补统一目录准备。
- 横向比对后确认这不是整条 Wave B/TLS13 链的普遍问题：`generate_wave_b_cross_platform_summary.sh`、closure/evidence、handoff、TLS13 snapshot/status` 已经都有显式输出目录创建保护。

## 2026-03-09 Findings (Wave B evidence cross-summary linked examples sync)
- `check_wave_b_b2_evidence_consistency.sh` 之前只校验 artifact 自身的 `run_id`，却不会校验 `cross_summary` 内嵌的 Linux examples 指针是否仍然和当前 evidence 选择结果一致。
- 这使得一类危险场景被放过：`cross_summary` 文件本身属于当前 run，但它里面写着错误的 `linux_examples_json` / `linux_examples_selection`，最终仍会被误判成 `CONSISTENT`。
- 最小正确修复不是强制所有 `cross_summary` fixture 都补全字段，而是仅在字段存在时做 linked-evidence 比对；这样既能抓住真实串味，也不会打爆已有 minimal fixture。
- 这波没有发现同类的普遍目录创建缺口；当前 Wave B/TLS13 主链剩余更值得做的是历史治理/retention 与更高层的架构复审。

## 2026-03-09 Findings (Wave B examples history retention naming)
- 之前的治理只保证了“history bucket 遗留 generic alias 会被删除”，但没有限制 archive copy 自己的文件名；一旦内部 env 被误配，archive copy 仍可能落成 `examples_compile_latest.json` 之类的 generic 名称。
- 这会让 history bucket 的职责边界重新模糊：目录还在，但文件名已经失去 run-scoped 可追踪性。
- 最小正确修复不是扩 retention 清理策略，而是只在 producer 侧把 archive copy basename 规范化成 `examples_compile_ci_gate_<run_id>.json`；目录仍允许自定义。
- 到这一步，Wave B examples 这条线已经把 current alias / run-scoped / archive history 三层语义都补成了有合同的边界。

## 2026-03-09 Findings (Wave B handoff linked-evidence alert sync)
- evidence checker 新增 `linked_evidence_mismatch` 之后，handoff 的顶部 `Consistency Alert Summary` 仍只看 `required_missing` 与 `runid_mismatch_or_parse_issue`，导致一种新的假阴性：整体已 `INCONSISTENT`，但 `alert_state` 还显示 `CLEAR`。
- 这类问题不需要改 handoff 的 artifact row 逻辑；row 已经会展示 `cross_summary` 的 mismatch note。真正缺的是顶部聚合计数面板。
- 最小正确修复是让 handoff 读取并展示 `linked_evidence_mismatch`，并把它纳入 `WARN` 判定，而不是扩展更多状态机。
- 到这一步，Wave B examples 主链的 producer、consumer、evidence checker、handoff 计数面板已经对齐到同一套信号面。

## 2026-03-09 Findings (library log callback roundtrip visibleization)
- request path 已经拒绝 `TSSLConfig.LogCallback`，`CreateDefaultConfig(...)` 也会主动清掉它；因此 library 级 logging 的可见性现在主要取决于 `ISSLLibrary` 自己的 round-trip surface。
- 这轮 backend 审查确认所有主后端都有同一处缝：`SetDefaultConfig` 会同时更新 `FDefaultConfig.LogCallback` 与 `FLogCallback`，但 `SetLogCallback` 只改 runtime 字段，不会同步默认快照。
- 这会制造一个稳定的表面不一致：library logging 已经生效，但 `GetDefaultConfig.LogCallback` 仍回显旧值或 `nil`，从而让默认配置快照失真。
- 最小正确修复是在各 backend 的 `SetLogCallback` 中同步 `FDefaultConfig.LogCallback`；这样只修 visibleization / round-trip，不改变 context 创建路径，也不重新把 logging 放回 request path。

## 2026-03-09 Findings (fake backend log callback snapshot parity)
- 真实 backend 已经收口到 `SetLogCallback` 会同步 `GetDefaultConfig.LogCallback`，但 `test_factory_shared_config_and_init_race.pas` 内部 fake backend 仍然保留了旧的 no-op 语义。
- 这会让一条关键测试主线和生产语义脱节：shared-config / init-race 合同虽然覆盖工厂并发与默认配置隔离，却无法继续承接 logging-scope 的 round-trip 断言。
- 最小正确修复不是再开一个新的 helper abstraction，而是在当前 fake backend 上同步 `FDefaultConfig.LogCallback`；这样能把测试替身拉回与生产一致的 contract surface。
- 下一层更值得做的是把 `tests/helpers/*.inc` 里的其他 fake library 也按同样原则做 contract parity，避免不同测试夹具之间再出现语义分叉。

## 2026-03-10 Findings (helper fake log callback snapshot parity)
- `tests/helpers/test_fake_default_backend_fixture.inc` 与 `tests/helpers/test_backend_store_fake_fixture.inc` 原先都没有保存任何默认配置快照，因此 `SetLogCallback` 即使被调用，`GetDefaultConfig` 仍然只能回显空记录。
- 这让一批 builder/config 测试夹具天然落后于真实 backend：它们可以继续验证 build path，却没法承接 logging-scope / round-trip 合同。
- 最小正确修复不是把 helper 做得和真实 backend 一样复杂，而是只补一份 `FDefaultConfig` 快照，并让 `SetDefaultConfig` / `GetDefaultConfig` / `SetLogCallback` 对齐到这份状态。
- 这样既不会影响现有 builder/store 行为，又能让依赖 helper fixture 的测试主线逐步接上生产语义。

## 2026-03-10 Findings (helper create-context default-config consistency)
- helper fixture 前一波已经开始保存 `FDefaultConfig`，但这还只修到“快照可见性”；`CreateContext` 仍然直接 new 一个 `TFreePascalContext`，不会把默认配置应用进去。
- 这制造了新的半吊子状态：`GetDefaultConfig` 看起来像是对的，但通过 helper library 直接创建出的 context 仍回到 `TFreePascalContext` 自己的内部默认值（例如 session timeout 变成 300）。
- 最小正确修复是两层：给 helper fixture 一个稳定 baseline snapshot，避免未配置时把零值写进 context；然后在 `CreateContext` 中复制 snapshot、覆盖 `ContextType`、调用 `TSSLFactory.ApplyConfigToContext`。
- 到这一步，helper fixture 不只是在 getter surface 上像真实 backend，而是连 default-config -> context 的主语义链也对齐了。

## 2026-03-10 Findings (helper library default config validation parity)
- helper fixture 之前已经补上了 default snapshot 与 `CreateContext` 应用，但 `SetDefaultConfig` 仍然是“无条件吞入”模式，这和真实 backend 差了一整层关键语义。
- 这种差异会让 helper fixture 接受生产代码本应拒绝的非法 library default config，例如 request-only 的 `CertificateFile` / `CAFile`，以及非默认的 `HandshakeTimeout` / `BufferSize`。
- 最小正确修复不是在测试里手写一套平行规则，而是直接让 helper fixture 复用 `TSSLFactory.NormalizeLibraryDefaultOwnerFields`、`ValidateLibraryDefaultConfigFields` 和 `NormalizeConfig`。
- 到这一步，helper fixture 的 library-default surface 已经基本对齐真实 backend：owner 字段会归一化，request-only 字段会被拒绝，accepted defaults 会被统一规范化。

## 2026-03-10 Findings (helper log dispatch parity)
- helper fixture 前几波已经补上了 logging snapshot 可见性，但这只是“配置面”对齐；`Log(...)` 本身仍然是 no-op，因此 runtime logging surface 依然比真实 backend 少一层语义。
- 这意味着一类测试仍会被误导：`SetDefaultConfig(LogLevel=Info)` + `SetLogCallback(...)` 看起来准备好了，但真正调用 `Log(...)` 时不会收到任何回调。
- 最小正确修复不是再引入独立的运行时字段，而是直接复用 `FDefaultConfig.LogCallback` 与 `FDefaultConfig.LogLevel` 做 dispatch gating：`Assigned(callback) and (ALevel <= LogLevel)`。
- 这样 helper fixture 的 logging surface 终于同时覆盖了 snapshot、setter 和 runtime dispatch 三层语义。

## 2026-03-10 Findings (fake backend log dispatch parity)
- `test_factory_shared_config_and_init_race.pas` 内部 fake backend 前面几波已经补上了 logging snapshot visibleization，但 runtime `Log(...)` 仍然停在 no-op，这让这条并发/默认配置主线和 helper fixture、真实 backend 之间再次分叉。
- 结果就是一类 contract 仍然会被漏掉：`SetDefaultConfig(LogLevel=Info)` 与 `SetLogCallback(...)` 都看似正确，但实际发日志时 callback 永远不会被触发。
- 最小正确修复同样是复用 snapshot：让 `Log(...)` 在 `Assigned(FDefaultConfig.LogCallback)` 且 `ALevel <= FDefaultConfig.LogLevel` 时直接分发。
- 到这一步，shared-config/init-race 这条 fake backend 主线的 logging surface 也接上了 snapshot、setter、runtime dispatch 三层语义。

## 2026-03-10 Findings (fake backend library default config validation parity)
- `tests/test_factory_shared_config_and_init_race` 内部 fake backend 之前仍直接保存 `AConfig`，这意味着即使 helper fixture 已收口，shared-config/init-race 主线仍会误接受非法 library-default 字段并放大测试夹具分叉。
- 复用 `TSSLFactory.NormalizeLibraryDefaultOwnerFields -> ValidateLibraryDefaultConfigFields -> NormalizeConfig` 是最小稳定修复：它只收口显式 `SetDefaultConfig` 路径，不改这条 fake backend 自己的 baseline 初始化语义。
- 到这一步，同一条 fake-backend 主线已经同时覆盖 per-request isolation、library-default validation、logging snapshot、logging dispatch 与 init race，不再只测“看起来能跑”的半 contract 表面。


## 2026-03-10 Findings (helper/fake CreateContext snapshot immutability)
- helper fixture 与 shared-config fake backend 现在已经覆盖了 default snapshot、validation、log callback visibleization、log dispatch；补一条 `CreateContext(...)` 不回写默认快照的合同后，这条测试替身主线更接近“可信替身”而不是“功能上差不多”。
- 新合同没有暴露生产缺口，说明当前 helper/fake backend 在 `CreateContext(server)` 上已经遵守“复制默认配置 -> 覆盖临时 `ContextType` -> 应用到 context”这条语义。
- 既然夹具线已经够稳，下一波更值钱的就不是继续修夹具，而是切回主 backend/API 复审。


## 2026-03-10 Findings (FreePascal connection ServerName observability parity)
- 真正的缺口不在 FreePascal 的 `SetServerName/GetServerName` 字段本身，而在 `GetConnectionInfo` 证据面：字段已经有值，但默认 connection info 里完全看不见。
- 这类问题更适合修在 shared base，而不是再给 FreePascal 单独补一份 backend-local override；否则未来任何未覆盖 `GetConnectionInfo` 的 client backend 都可能再掉进同一坑。
- 把默认 `GetConnectionInfo` 接到 `ISSLClientConnection.GetServerName` 后，FreePascal 立即补齐了 `create -> override -> clear` 三段 observability，而且不会影响已有覆盖更细 native 细节的 backend-local overrides。


## 2026-03-10 Findings (WinSSL network examples per-connection SNI)
- `ServerName` 迁移策略如果只停在 README / 架构文档层，示例层仍继续示范 `LCtx.SetServerName(...)`，调用方最容易学到的仍然会是旧路径。
- 这三份 WinSSL 联网示例都有真实连接对象，因此它们是最适合优先迁移的高可见入口：改动小、收益高、不会牵扯“只做 context 配置演示”的特殊场景。
- 用 shell contract 锁“不得再出现 context-level 旧用法 + 必须存在 per-connection 设置路径”比单纯 grep 一个 API 名更稳，能防止以后只删旧调用却忘了补新路径。


## 2026-03-10 Findings (example_factory_usage per-connection SNI)
- `ServerName` migration policy 如果只写在 README / 架构文档里，但最直观的 `example_factory_usage` 还继续打印 `Ctx.SetServerName(...)`，用户最容易复制的仍会是旧路径。
- 这类入口的收益比普通示例更高：它们不是兼容性测试，而是默认会被当成“推荐写法”。
- 用 focused shell contract 锁住“不能再打印 context-level 旧用法 + 必须展示 per-connection 路径”，比只改一行示例文字更抗回退。

## 2026-03-10 Findings (WinSSL example-style tests per-connection SNI)
- `tests/examples/test_winssl*.pas` 再加上 `test_performance.pas` / `test_certchain.pas`，虽然挂在测试目录下，但从内容和输出形态看更像“可阅读的连接示例”而不是纯兼容性断言，因此继续示范 context-level `SetServerName(...)` 会放大迁移噪音。
- 这五个文件都是真实建连接再握手的路径，迁到 per-connection SNI 的改动非常小，不需要去碰它们真正关心的 WinSSL 握手/读写逻辑。
- 因为它们是 Windows-only 入口，本波最合理的验证就是 focused shell contract；在 Linux 环境下强做编译并不能带来更高置信度。

## 2026-03-10 Findings (example tests context ServerName compat coverage)
- `tests/examples/test_basic.pas` 与 `tests/examples/test_lib_core_functionality.pas` 和前面那些真实联网示例不同：它们更像“基础 API 冒烟/兼容覆盖”而不是推荐的连接写法示例。
- 直接把它们也迁到 per-connection 路径会削弱 deprecated context-level `SetServerName(...)` 的覆盖面，因此更稳的治理方式是显式标记其角色，而不是把所有旧调用一律删掉。
- 这轮顺手暴露出 `test_lib_core_functionality.pas` 还有一条旧的编译漂移（`TCryptoUtils.Base64Encode/Base64DecodeString`），但那是独立债务，不应和 `ServerName` 迁移混批修。

## 2026-03-10 Findings (test_lib_core_functionality encoding API modernization)
- 这条 drift 的根因非常直接：文件停留在旧的 `TCryptoUtils.Base64Encode/Base64DecodeString` API，而仓库早已把 Base64 主入口收敛到 `TEncodingUtils`。
- focused runtime 还暴露出第二层旧问题：这个文件虽然显式点名 `sslOpenSSL`，但没导入 `fafafa.ssl.openssl.lib`，导致在 focused 运行里 backend 甚至没注册。
- 两个问题都属于“示例测试没跟上当前 API 面”的同一类历史债务，因此适合合并成一个小波次一起收口。

## 2026-03-10 Findings (test_basic deprecated warning noise)
- `test_basic.pas` 已经被定性为 compatibility coverage，所以这波的正确目标不是删掉旧 API 覆盖，而是把 deprecated warning 局部化，避免高可见入口持续制造无价值噪音。
- 最稳的做法是沿用仓库既有模式，在极小的调用点周围加 `{$PUSH}{$WARN SYMBOL_DEPRECATED OFF}` / `{$POP}`，而不是扩大 suppress 范围到整个文件。
- 再补一个最小变量清理后，`test_basic.pas` 的 focused 编译输出已经不再包含 file-local warning/note，说明这个入口的噪音面也已经基本闭环。


## 2026-03-10 Findings (Wave B cross-summary examples warning observability)
- `cross-summary` 之前已经带了 `linux_examples_selection`，但 warning 面还停留在 evidence / handoff，导致同一条 Linux examples 链上字段集合并不一致。
- 这种缝最容易让后续消费者再各自发明一套“显式 override 是否需要提醒”的判断；因此比继续扩更多 run-id/fallback 合同更值钱的是先把 producer 输出字段补齐。
- 这波修复非常小：不动 resolver 语义，只让 `cross-summary` 把已经存在于相邻 consumer 的 `linux_examples_warning` 显式写出来。


## 2026-03-10 Findings (Wave B CI gate examples warning observability)
- cross-summary 已经补上 `linux_examples_warning` 之后，producer summary 反而成了新的最前端缺口：链路中间层都能看见 warning，最前面的人类入口却看不见。
- 这类 producer observability 修复很便宜：resolver 语义完全不变，只是把已经隐含存在的 explicit/default warning 状态显式写进 summary。
- 做完这波后，Linux examples warning 面终于在 producer / cross-summary / evidence / handoff 四层统一，不再需要读者在不同报告之间脑补。


## 2026-03-10 Findings (Wave B CI gate examples selection observability)
- producer summary 之前只给了 alias/run-scoped/archive path，已经足够让机器串链，但对人类读者来说仍需要自己推断“这到底是 explicit override 还是默认 alias”。
- 这波和 warning observability 一样，最有价值的不是改 resolver，而是把已经隐含在参数/默认路径里的状态显式输出成 `Selection` 字段。
- 做完后，producer summary 终于也具备了 selection + warning 双字段，Linux examples 这条链的最前端和后面的 consumer 报告不再割裂。


## 2026-03-10 Findings (Wave B CI gate dry-run examples observability)
- `dry-run` 如果只打印 step 命令而不打印 examples selection/warning，调用者在排查配置组合时仍得去脑补 summary 最终会落什么 metadata，这会削弱 dry-run 的价值。
- 这波和前面的 summary observability 一样，不改任何 resolver/override 语义，只把已有状态显式打印出来。
- 做完后，CI gate 的 Linux examples metadata 在 dry-run / summary 两条面都对齐，观察成本明显更低。


## 2026-03-10 Findings (Wave B cross/evidence dry-run examples path observability)
- `selection + warning` 已经补上后，dry-run 里最自然的下一层缺口就是 path：没有 `linux_examples_json`，调用者仍然要猜当前到底选中了哪份 artifact。
- 这波依旧是纯 observability 修复：不动 resolver，不动 strict，只把已经解析出来的 path 显式打印出来。
- 做完后，cross-summary / evidence 的 dry-run 都具备 `path + selection + warning` 三件套，诊断成本明显下降。


## 2026-03-10 Findings (Wave B handoff dry-run examples observability)
- handoff dry-run 其实已经带了 Linux examples 的 path/selection/warning 三件套，但 path 字段名还停在 `linux_examples`，和 CI gate / cross-summary / evidence 已经收口出来的 `linux_examples_json` 不一致。
- 这种问题不影响语义，却会拉高观察成本：读者需要记住同一概念在不同脚本里叫不同名字。
- 这波最小修复就是统一字段名，不碰 resolver/strict/handoff fan-out 逻辑。


## 2026-03-10 Findings (Wave B CI gate dry-run examples path observability)
- 把 selection/warning 补上之后，CI gate dry-run 里最自然的下一层缺口就是 path：调用者仍然不知道 report/current_alias/run-scoped/archive 最终会落到哪里。
- 这波依然不改 resolver/落盘语义，只是把已经确定的路径显式打印出来。
- 做完后，CI gate producer dry-run 终于具备完整的 path + selection + warning 观察面。


## 2026-03-10 Findings (Wave B closure dry-run path observability)
- `closure` 这头虽然不消费 Linux examples artifact，但 dry-run 仍然应该把三平台 summary 输入路径打出来；否则调用者只能看到状态，却不知道状态是从哪份 summary 算出来的。
- 这波修复同样是纯 observability：不碰 closure 判定，只把已传入/已默认解析到的输入路径显式输出。
- 做完后，Wave B closure dry-run 终于也补齐了“输入路径可见”这一层。


## 2026-03-10 Findings (Wave B handoff dry-run report paths)
- handoff dry-run 之前已经有 `output_dir`，但这对调用者仍然不够：cross/closure/consistency/bundle 4 份报告的实际命名规则仍要靠人脑拼接。
- 这波依旧只补 observability，不动 fan-out 或路径规则；把最终 report paths 明确打印出来就够了。
- 做完后，handoff dry-run 在输出层也不再留盲区。

## 2026-03-10 Findings (Wave B handoff dry-run platform paths)
- `handoff` dry-run 之前把 macOS / Windows 输入只折叠成了 `macos_args/windows_args`，这对调试 fan-out CLI 来说太间接了。
- 对比 closure / evidence / cross-summary，平台 summary 路径本身应该直接可见，而不是藏在 args 字符串里。
- 这波最小修复就是直接输出 `macos_summary` / `windows_summary`，不动参数拼装逻辑。


## 2026-03-10 Findings (Wave B evidence dry-run path observability)
- `evidence` dry-run 之前虽然已经有 `linux_examples_json/selection/warning`，但真正驱动 consistency 的 summary/cross/closure 输入路径仍然不可见，这会让定位来源变慢。
- 这波修复依然是纯 observability，不动 consistency 判定本身，只把输入路径显式打出来。
- 做完后，evidence dry-run 在 input side 也不再是半黑盒。


## 2026-03-10 Findings (Wave B cross-summary dry-run platform paths)
- cross-summary dry-run 之前已经能看见 Linux examples 元数据，但 macOS/Windows/Android 仍只剩 `state + note`，输入路径本身不可见。
- 这波同样是纯 observability 修复：不动状态机，只把实际传入/解析到的平台路径显式打印出来。
- 做完后，cross-summary dry-run 在平台输入层不再留盲区。


## 2026-03-10 Findings (Wave B CI gate dry-run summary observability)
- `CI gate` dry-run 之前虽然已经有 examples path/selection/warning，但 summary 层的关键元数据还停在非结构化 stderr 行里，不利于稳定消费。
- 这波最小修复是把 `run_id` / `summary_out` 也补成结构化 `[DRY-RUN]` 字段，而不是去删掉原有 stderr 输出。
- 做完后，CI gate dry-run 的 summary 层终于也和其它 dry-run 输出风格统一了。


## 2026-03-10 Findings (Wave B macOS gate dry-run observability)
- `run_wave_b_macos_gate.sh` 之前的 dry-run 更像“命令预览”，但还没有把关键 metadata（run_id / summary / probe/examples 输出）结构化吐出来。
- 这波依旧不改 gate 语义，只把已经确定的输出路径显式打印成 dry-run metadata。
- 做完后，macOS gate 也进入了和主链脚本一致的结构化 dry-run 观察面。

## 2026-03-10 Findings (Wave B Windows gate dry-run observability)
- `run_wave_b_windows_gate.ps1` 是平台 gate producer 里剩下的最后一个明显缺口：dry-run 仍只有 step-command 预览和尾部 summary 行，缺少稳定的结构化 metadata。
- active Linux 环境没有 `pwsh` / `powershell`，所以这条链最稳的合同方式仍然是源码级静态约束，冻结 `Write-Host "[DRY-RUN] key=value"` 输出面，而不是引入依赖宿主的动态执行。
- 最小正确修复是先把相对路径统一收口成可复用变量，再显式输出 `run_id` / `output_dir` / `summary` / 各 step log 路径；这样能补 observability，又不碰现有执行语义。

## 2026-03-10 Findings (Wave B platform archive debug log surface)
- `archive_ci_artifacts_draft.sh` 之前只归档 Wave B 的 summary/bundle 级 markdown，但 Linux/macOS/Windows gate 的活动日志面没有进入 archive；这会让后续排查还得回工作目录翻日志。
- 这波最小正确修复不是把日志塞进 `core-reports`，而是补到现有 `debug-logs` 类，保持 summary/evidence 和 debug signal 分层。
- 邻近的 repo-hygiene 静态合同还暴露出一个旧模式漂移：`run_tls13_signer_gate_bundle.sh` 的 env passthrough 早已是 `env "FAFAFA_*=..."` 形式，合同应对齐真实实现而不是继续盯历史 quoting 细节。

## 2026-03-10 Findings (continuous test monitor output governance)
- `continuous_test_monitor.sh` 之前虽然已经把 `history/summary` 收进 `REPORTS_DIR/monitor`，但 `trend_report` 还是函数内局部路径，run-scoped `unit/bin` 则继续散落到项目根 `tmp/`，输出层不完整。
- 这波最小正确修复不是重写 monitor，而是把输出层显式拆成 `REPORTS_DIR/monitor` 与 `REPORTS_DIR/runs` 两块：前者存长期观察面，后者存每轮隔离产物。
- 用“复制脚本到临时项目根 + fake runner”做 runtime 合同，比单纯 grep 更稳：它直接证明 monitor 在真实运行时会把 `FAFAFA_FPC_UNIT_OUTPUT_DIR` / `FAFAFA_TEST_BIN_DIR` 指向收口后的 run-scoped 路径。

## 2026-03-10 Findings (builder private-key PEM precedence alignment)
- `private_key_file` / `private_key_pem` 的“双态并存”不是理论边角：`ImportFromJSON(...)` 和 `Merge(...)` 都会直接写字段，不会像 `WithPrivateKeyPEM(...)` / `Override('private_key_pem', ...)` 那样自动清掉 file。
- 现状里 `ValidateServer` 已经承诺 “PEM will be used”，但 `BuildClient` / `BuildServer` 实际还是 file-first；这会把 validation 合同和 build 真行为撕开。
- 最小正确修复是只调 build 分支顺序到 `PKCS#11 > PEM > file`，不碰 validation 文案和其余配置流；这样既收口真实风险，也避免把范围扩大成新的 builder 重构。

## 2026-03-10 Findings (builder certificate PEM precedence alignment)
- `certificate_file` / `certificate_pem` 也存在和私钥完全同类的双态风险：builder DSL 会清互斥字段，但 `ImportFromJSON(...)` / `Merge(...)` 不会。
- 原 build 路径是“先 file、再 PEM”，所以当 file 路径无效而 PEM 实际可用时，构建会在进入 PEM 分支前就失败；这和 validation 中“PEM will be used”的约定相冲突。
- 最小正确修复仍然是只调 build 分支顺序，不动 validation 文案和 import/merge 语义；这样能在保持 snapshot surface 不变的前提下收口真实执行顺序。

## 2026-03-10 Findings (builder PKCS11 mixed key warning alignment)
- `pkcs11_uri + private_key_file/private_key_pem` 的 precedence 之前只存在于 build 分支实现里；validation 只看“有没有私钥来源”，没有把这个 precedence 显式说出来。
- 对调用方来说，这会制造一种假象：mixed-input 是“完全无歧义”的配置；但实际 build 已经固定成 `PKCS#11` 优先。
- 这波最小正确修复不去改 build，而是给 `ValidateServer` 补 user-facing warning，把 precedence 变成显式合同。

## 2026-03-10 Findings (builder PKCS11 certificate-required doc truth)
- 行为层面，`UsePKCS11(...)` 一直只替代私钥来源；server 证书要求从没取消过。
- 真正漂移的是用户面：`docs/guides/PKCS11_USER_GUIDE.md` 还在写旧 API `ForServer` / `WithPKCS11Key` / `.Build`，而且没有把“证书仍必需”说透。
- 这波用一条 docs contract + 一条 validation focused test 同时锁住“当前 API 名称”和“PKCS11 不替代证书”两层真相。

## 2026-03-10 Findings (pure Pascal client peer certificate foundation)
- pure Pascal 客户端真正缺的不是又一层 parser，而是“真实客户端握手路径把 `Certificate` 消息留下来”。没有这层 capture，后续 `hostname verification` / `system roots` / `custom CA` 都只能停留在旁路能力证明。
- 最小正确修复是把 `ProcessEncryptedServerFlight(...)` 接上 `TLS_HANDSHAKE_TYPE_CERTIFICATE` 解析，并在 `DoGetPeerCertificate` / `DoGetPeerCertificateChain` 做 DER -> `ISSLCertificate` 重建；不要在这一步抢先把完整验证逻辑耦进去。
- `tests/test_freepascal_client_peer_certificate_foundation.pas` 采用按真实 `ClientHello` 动态生成 server flight 的 scripted harness，比静态录制握手更稳，因为它不会再被 ephemeral keyshare 漂移击穿。

## 2026-03-10 Findings (pure Pascal client hostname verification path)
- peer certificate capture 建好之后，最自然的下一层缺口就是：`Connect` 仍只做握手，不做 trust-store / hostname 判定。这会让 `sslVerifyPeer` 在 pure Pascal 客户端上只是“配置存在”，不是“真实执行”。
- 这波最小正确修复不是直接把完整 CA-source 矩阵都做完，而是先把 context trust store 接到客户端后握手验证，并让 client-only hostname mismatch 在 `Connect` 层显式失败。
- 老的 peer-certificate foundation 测试必须显式提供 trust material并忽略 hostname；否则在更严格的默认验证语义下，它会因为“测试目标不是 host verify”而被误伤。

## 2026-03-10 Findings (pure Pascal client custom CA sources path)
- hostname verification 接上之后，下一层真实缺口是：`LoadCAFile(...)` / `LoadCAPath(...)` 仍只停留在 context 字段，不会进入客户端握手验证主线。
- 最小正确修复不是新增第二套验证分支，而是扩展现有 post-handshake validation，把 effective trust source 收口为：`context store + CAFile + CAPath`。
- 对 pure Pascal 后端而言，这类工作应当遵守同一条原则：**现有 public interface 不能只“有 setter”，还要在真实运行路径里有行为证据。**

## 2026-03-10 Findings (pure Pascal client verify callback path)
- completeness audit 里最贴近当前主线的高信号缺口是 `SetVerifyCallback(...)`：接口已经公开，但 pure Pascal runtime 之前只存 `FVerifyCallback`，不会在真实验证路径中触发。
- 最小正确修复不是造第三套 callback 流程，而是把 callback 接在现有 post-handshake validation 的最终判定点上，让它既能 veto success，也能 override failure。
- 这波把 verify callback 接上之后，pure Pascal 后端的下一批 completeness 高信号缺口更清晰了：`certificate pinning`、`info callback`、`session/session reuse`。

## 2026-03-10 Findings (pure Pascal client info callback path)
- `SetInfoCallback(...)` 是另一条典型 completeness gap：上下文已经保存 `FInfoCallback`，但 pure Pascal 连接此前完全不发状态事件。
- 对齐 WinSSL 的事件名最稳：`handshake_start` / `verify_failed` / `handshake_failed` / `handshake_done`。这样不会再引入一套只有 pure Pascal 自己懂的状态词。
- 这波完成后，剩下更重的 public interface gap 更集中到 `certificate pinning`、`session/session reuse` 和 `password callback`。

## 2026-03-10 Findings (pure Pascal client chain verification path)
- `WithSystemRoots` 外网探针失败后，真正暴露出来的根因不是“system roots 没接线”，而是 pure Pascal 之前只拿 leaf 去看 trust store，没有把 peer chain intermediates 纳入验证。
- 最小正确修复是两步一起做：
  - 客户端 effective validation store 合并 peer chain intermediates
  - `VerifyCertificate(...)` 沿 issuer 链继续走到 trusted self-signed root
- 这仍不是完整 PKIX，但已经把真实 HTTPS client 最关键的 intermediate-chain 缺口补上了。

## 2026-03-10 Findings (pure Pascal client system roots runtime)
- 链验证缺口补上后，`WithSystemRoots` 的 builder/runtime 路径在真实外网握手里是可用的；之前卡住它的不是 builder wiring，而是 chain verification 根能力不够。
- 最稳的 system-roots 证据形式是 network-gated integration，而不是试图向系统证书目录里塞临时测试 CA。
- 这波也说明 pure Pascal M1 后续应该优先“补真实运行路径缺口”，不要把时间先花在只会增加文档字样、不改变运行能力的表层工作上。

## 2026-03-10 Findings (pure Pascal session surface foundation)
- `GetSession` / `SetSession` / `IsSessionReused` 是一组典型 public-interface completeness gap：pure Pascal 已经公开了接口，还能解析 `NewSessionTicket`，但 surface 仍是 `nil/False` 空壳。
- 最小正确修复不是假装做完 TLS 1.3 resumption，而是先把 session snapshot surface 做真：可取、可序列化、可克隆、可重新传回 `SetSession(...)`。
- 这波完成后，session API 至少不再是“看起来支持、运行时完全空白”；真正的 resumption/PSK 则可以作为下一层增强单独收口。

## 2026-03-10 Findings (pure Pascal client certificate pinning path)
- `certificate pinning` 也是一条典型 completeness gap：context 已经能收 pin、能开关 pinning，但 pure Pascal runtime 之前完全不消费它。
- 最稳的实现方式不是试图复用 OpenSSL-only `TPinValidator`，而是直接在 pure Pascal 验证路径上补两种哈希：
  - 证书 pin：`SHA256(cert DER)`
  - 公钥 pin：`SHA256(SPKI DER)`
- 这波做完后，pure Pascal client 的安全相关 public interface 已经从“仅配置面存在”推进到“真实握手里生效”的阶段。

## 2026-03-10 Findings (pure Pascal password-protected private key truth)
- 这条缺口的根因不是“忘了调用 callback”，而是更底层：pure Pascal TLS13 signer 目前根本不支持 encrypted PEM private key。
- 在这种前提下，最小正确修复不是继续静默接受 password/callback，而是：
  - capability truth 改回 `SupportsPasswordProtectedKeys = False`
  - 加载加密 PEM 私钥时显式抛 `unsupported`
- 这样至少先消掉“接口看起来支持、运行时悄悄忽略”的危险语义；真正的 encrypted-key runtime 支持可在后续单独实现。

## 2026-03-10 Findings (pure Pascal TLS13 session resumption foundation)
- 真正的 resumption 根能力并不在 `ISSLSession` surface，而在三条 wire/runtime 主线上：
  - ClientHello `pre_shared_key`
  - PSK-aware handshake key schedule
  - ServerHello `selected_identity`
- 这一波先故意把范围收在 pure Pascal self-interoperability foundation：
  - single identity
  - `psk_dhe_ke`
  - binder 自洽校验
- 这已经足以让 pure Pascal 自己的第二次握手走 reused path，但还不能宣称对外部 TLS 栈互操作完成。

## 2026-03-10 Findings (pure Pascal session ticket runtime)
- 真实外网验证表明：pure Pascal 客户端已经能在 TLS 1.3 真实站点上提取 `NewSessionTicket` 并形成 `IsResumable=True` 的 session snapshot。
- 但把这个真实提取出的 session 立即用于第二次对外 resumed handshake，当前仍会收到对端 alert。这说明：
  - `session ticket extraction` 已经是真的
  - `external resumption interoperability` 还没有闭环
- 这两个结论必须分开写，不能因为第一条成立就过早宣称第二条也完成。

## 2026-03-10 Findings (pure Pascal local session resumption roundtrip)
- 本地 roundtrip 暴露的不是单点 bug，而是三层耦合漂移：
  - 测试层：固定端口 + `Sleep(...)` 让本地 socket case 容易被 bind/startup 抖动污染。
  - 配置层：FreePascal backend 的 `FDefaultConfig.Options` 虽然包含 `ssoEnableSessionTickets`，但 `EnableSessionTickets` 没同步为 `True`，`NormalizeConfigOptions(...)` 会在 direct-library/factory apply 时把 ticket 选项删掉。
  - 协议层：应用流量密钥派生和 resumption PSK 派生不能共用同一份 transcript 状态；前者要与应用数据密钥时点对齐，后者则要与 `client Finished` 后的 resumption transcript 对齐。
- 这条链修完后，当前纯 Pascal 的能力边界更清楚：
  - 本地 end-to-end `ticket -> resumable session -> resumed handshake` 已闭环。
  - 真实外网站点 `ticket extraction` 仍是绿色。
  - 但对外 resumed handshake 互操作仍未闭环，说明剩余问题已经收缩到更窄的 wire/protocol 对齐层。

## 2026-03-10 Findings (pure Pascal external session resumption interop)
- 外部 resumed handshake 互操作的真正根因分成两层：
  - binder 线不符合 TLS 1.3 规范：
    - binder transcript 截断到了 binder bytes 起点，而不是 binders list 起点
    - `res binder` 的 `Derive-Secret(..., "")` 上下文错误地用了空字节，而不是 `Hash("")`
  - resumed validation 线不完整：
    - resumed handshake 不会重新发证书链
    - pure Pascal session snapshot 之前只保存 leaf certificate，没有保存 intermediates
    - 所以 `verify peer + system roots` 的 resumed path 会在真实站点上失败
- 这两层修完后，外部互操作状态发生了质变：
  - 本地 OpenSSL 标准栈第二次握手 `reused=True`
  - `www.google.com:443` 的第二次握手也能 `reused=True`
- 这说明 pure Pascal 的 session resumption 已经不再只是 self-interoperability foundation，而是进入了真实外部 TLS 栈互操作阶段。

## 2026-03-10 Findings (pure Pascal stream semantics)
- `close_notify/EOF` 与 `WantRead/WantWrite` 是 pure Pascal 当前最直接暴露给业务代码的流式语义缺口：
  - `TSSLStream.Read` 之前会把优雅关闭也抬成异常
  - `SetBlocking(False)` 之前只改基类布尔位，不会真正改 socket 模式
- 这类问题的正确修复点在连接层本身，而不是 `TSSLStream` 包装层：
  - 连接层必须区分 `retryable would-block`、`graceful EOF`、`real TLS/IO error`
  - 只有这样 `TSSLStream.Read` 才能自然继承正确行为
- 这波修完后仍要保持克制：
  - 当前 nonblocking 语义已经足以覆盖“空闲读 -> WantRead”
  - 但对“部分 record 读取后跨多次 nonblocking continuation”的 buffering 还没有单独合同，属于下一层更深的流式能力

## 2026-03-10 Findings (pure Pascal password protected private key runtime)
- 这条能力真正的修复点不在 signer，而在 context 装载阶段：
  - signer 已经能稳定消费 clear PKCS#8 / PKCS#1 / SEC1
  - 所以最小正确方案是“先解密，再把 clear material 交给既有 signer”
- 当前仓库没有现成的 pure Pascal encrypted-key helper，因此这波补的是刻意收窄的纯 Pascal 支持面：
  - `PBES2`
  - `PBKDF2-HMAC-SHA256`
  - `AES-CBC`
- 这种切法的价值是高的：
  - 已有 `LoadPrivateKey(..., APassword)` / `LoadPrivateKeyPEM(..., APassword)` / `SetPasswordCallback(...)` 都开始在真实运行路径中生效
  - builder/factory 的 password-string 路径也自然跟着变真
- 但边界也要保持诚实：
  - legacy encrypted PEM 现在已经覆盖 AES-CBC 分支，但非 AES 的 legacy cipher family 仍未覆盖
  - 如果后面要进一步扩展，这一层应该继续在 context-side decrypt helper 上长，而不是把复杂度塞进握手/signer主线

## 2026-03-11 Findings (pure Pascal shutdown close notify)
- `Shutdown` 的正确修复点同样在连接层，而不是 `TSSLStream`：
  - `TSSLStream` 只是消费 `ISSLConnection.Shutdown`
  - 真正决定是否发 alert record 的，是连接层是否持有 application secrets、seq、nonce、AEAD 发送能力
- 这条线说明 pure Pascal 现在在“被动 graceful EOF”和“主动 graceful close”上已经基本对齐：
  - 先前流式语义波次解决了对端 `close_notify`
  - 这波补齐了本端 `Shutdown -> close_notify`
- 仍要保持范围克制：
  - 当前 `Shutdown` 已适用于 blocking happy path
  - 若后面继续深化，应优先补 nonblocking shutdown retry contract，而不是先做更复杂的抽象重构

## 2026-03-11 Findings (pure Pascal nonblocking partial-record buffering)
- 这个缺口的真正根因不是 `WantRead` 本身，而是“在 `WantRead` 前已经读到的 ciphertext 没有落到连接级持久状态”：
  - 旧实现里 header / payload 都走局部 `TBytes`
  - 一旦第二次 `recv` 返回 `would block`，前半段 record 就随局部变量一起丢失
- 最小正确修法是把 raw transport buffering 放到 `RecvTLSRecord(...)` 边界，而不是只补 `RecvExact(...)`：
  - 如果先消费掉 5-byte header，再在 payload 阶段 `WantRead`，问题仍然存在
  - 必须等完整 record 已经在连接缓冲里拼齐后，才真正从缓冲中消费它
- 这条线也顺手暴露了一个对称的下一优先级 gap：
  - 当前 read-side continuation 已闭环
  - 但 write-side `SendAll(...)` / application record send 仍没有 pending-record staging；nonblocking `WantWrite` 下仍可能出现“局部发送后无法无损继续”的对称问题

## 2026-03-11 Findings (pure Pascal nonblocking write WantWrite)
- 写侧最先暴露出来的 bug 甚至比“partial send continuation”更表层：
  - `SendData(...)` 已经能正确标记 `sslErrWantWrite`
  - 但 `SendApplicationDataFragment(...)` / `SendTLS13AlertRecord(...)` 又把它覆盖回 `sslErrIO`
- 所以最小正确修法必须包含两层：
  - 上层发送路径不能再抹掉 retryable `WantWrite`
  - 连接层要持有 pending TLS record，确保 retry 的仍是同一条记录，而不是重新生成一个“看起来一样但语义上是新操作”的发送
- 当前这套 pending-send 设计还带来一个清晰的后续动作：
  - `application data` 和 `close_notify` 已经共享同一套 pending-send
  - 下一波最自然的是把 `Shutdown` nonblocking retry 也合同化，然后再评估是否把 `KeyUpdate` / post-handshake ticket send 一并收口

## 2026-03-11 Findings (pure Pascal shutdown nonblocking retry)
- 当前 `Shutdown` 的 nonblocking retry 真相已经比实现前预期更清晰：
  - 当 pending 的是上一条 application-data record 时，`Shutdown` 不会越过它直接发 `close_notify`
  - 而是显式返回 `WantWrite`，要求调用方先续完同一条 pending write
- 这说明现有 pending-send foundation 已经足以覆盖：
  - `application data`
  - `close_notify`
- 但同一个审计也暴露了下一条更深的设计缺口：
  - `SendPostHandshakeKeyUpdate(...)` 和 `SendInitialSessionTicket(...)` 仍走 `SendAll(...)`
  - 它们还没有并入同一套 pending-send contract
  - 如果后面要把 post-handshake write model 做完整，这两条线必须统一，不然 pure Pascal 会有两套不同的 nonblocking post-handshake 语义

## 2026-03-11 Findings (pure Pascal post-handshake send pending unification)
- `Renegotiate` 的红测证明：只修 `application data` / `close_notify` 还不够。
  - 只要 `KeyUpdate` 还绕开 pending-send，它就会在 pending write 场景下重新退回 generic IO
- `NewSessionTicket` 比 `KeyUpdate` 多一个额外陷阱：
  - 它的 payload 含随机 ticket / nonce / age_add
  - 所以 retry 不能“重算一个等价新记录”，只能续发第一次已经生成的那条 pending record
- 这也暴露了一个新的用户面噪点：
  - 虽然 retry 成功了，但部分成功路径上的 `detail` 仍会残留前一次错误文案
  - 这不会破坏 wire correctness，但会污染可观测性；适合作为下一波 focused cleanup

## 2026-03-11 Findings (pure Pascal retry success clears stale error)
- 这是一个纯 observability / ergonomics 问题，不是 wire correctness 问题：
  - retry 已经成功
  - 但 `GetVerifyResultString` 仍显示上一次 `would block`
- 正确修法不是改 `GetVerifyResultString` 的展示层，而是成功完成 send/flush 后就把当前错误状态清空。
- 这波做完后，pure Pascal stream IO 的用户面比之前更接近“生产可调试”：
  - blocked
  - retry
  - success
  三个阶段的可观测性现在终于能对上真实状态。

## 2026-03-11 Findings (pure Pascal SHA384 suite support)
- SHA384 套件这波真正卡住的不是 `keyschedule` / `appschedule` / `AEAD` 本身，而是连接层的三处“默认仍按 SHA256 思维写死”的代码：
  - Finished key / verify_data
  - server `CertificateVerify` transcript input
  - client/server cipher-suite negotiation与 resumption gate
- 另外还有一个很隐蔽的线程问题：
  - `TLS13AEADIsSupported(TLS_AES_256_GCM_SHA384)` 在主线程首次触发时可用
  - 但如果第一次 AES-GCM lazy init 发生在 worker thread，pure Pascal server thread 会把 SHA384 套件误判成 unsupported
  - 单元初始化阶段预热 AES-GCM 支持后，这条线程差异被收口
- 默认 resumption 在 SHA384 下还暴露了一条尾巴：
  - binder / resumption PSK 前半段逻辑仍停在 SHA256 gate
  - 所以要让默认 cipher 顺序切到 AES256 而不打坏 local resumption，binder + `res master/resumption` 也必须一起补

## 2026-03-11 Findings (pure Pascal blocking read timeout)
- timeout 这条线的最小正确切口是 blocking read，不是先打 cancel：
  - Linux `SO_RCVTIMEO` 下常见 errno 仍是 `EAGAIN/EWOULDBLOCK`
  - 如果不结合 `FBlocking + FTimeout`，就会把 blocking timeout 误判成 nonblocking `WantRead`
- transport 层修完还不够：
  - 应用读路径之前会把任何 `RecvTLSRecord` 失败折叠成 generic IO
  - 所以还必须在 `RecvApplicationDataFragment(...)` 保留 `sslErrTimeout`
- 这波之后，pure Pascal 的 timeout/error model 比之前更接近 API error model 文档里的预期：
  - blocking timeout -> `sslErrTimeout`
  - nonblocking would-block -> `sslErrWantRead/sslErrWantWrite`
  - 这两类恢复动作终于分开了

## 2026-03-11 Findings (pure Pascal write and handshake timeout)
- send timeout 与 handshake timeout 的根因都不是底层 `SendData/RecvData` 本身，而是上层路径的“错误再包装”：
  - application write path之前会把 timeout 覆写成 `Failed to send TLS application record`
  - `ProbeServerHello` / encrypted-flight 等握手读路径之前会把 timeout 覆写成 generic handshake IO
- 所以这波的关键不是再加更多 errno mapping，而是：
  - 对 send path：把 timeout 视为 deferred state，不能清 pending write，也不能覆写成 generic IO
  - 对 handshake read path：看到 timeout 就直接返回，让已有 timeout detail 往外冒
- 做完后，pure Pascal 的 timeout/error model 已经形成一个更稳定的最小矩阵：
  - blocking read timeout -> `sslErrTimeout`
  - blocking write timeout -> `sslErrTimeout`
  - client handshake timeout -> `sslErrTimeout`
  - nonblocking retry -> `sslErrWantRead/sslErrWantWrite`

## 2026-03-11 Findings (API cancellation model)
- 当前仓库最重要的 cancel truth 不是“怎么实现取消”，而是“现在根本没有独立 cancel API”。
- 在这个前提下，最容易漂移的三件事必须先分开写清：
  - `Close` = force abort
  - `Shutdown` = graceful close
  - `SetTimeout(...)` = budget / deadline
- 这层文档固定下来后，后面如果真的要加 `Cancel`，就不会再和 `Close` / `Shutdown` / timeout 混成一锅。

## 2026-03-11 Findings (pure Pascal observability state and verify result)
- 成功握手后返回 `Not verified`，失败握手后返回 `Disconnected`，这两种默认文案都太粗糙：
  - 前者掩盖了“验证通过”与“验证关闭”的区别
  - 后者掩盖了“从未连接”和“握手失败”的区别
- 最小正确修法是直接在 pure Pascal 连接层把状态说清楚：
  - verified success -> `Verification passed`
  - verification off -> `Verification disabled`
  - failed handshake -> `HANDSHAKE_FAILED / Handshake failed`
- 这条线也顺手暴露了 `GetConnectionInfo` 的一个实用缺口：
  - 之前 `CipherSuiteId` / `KeySize` 没填
  - `IsResumed` 虽然理论上由 base 填，但没有 pure Pascal 端的 focused evidence

## 2026-03-11 Findings (pure Pascal info callback timeout observability)
- timeout 既然已经在错误模型里独立成 `sslErrTimeout`，info callback 继续把它归到 `handshake_failed` 就太粗。
- 最小正确修法不是到处散着写 `if timeout then ...`，而是集中成一个 handshake-failure state mapper：
  - timeout -> `timeout`
  - verify failures -> `verify_failed`
  - 其它 -> `handshake_failed`
- 这样后面如果再新增例如 `cancelled`，也有清晰落点，不会继续污染主握手路径。

## 2026-03-11 Findings (pure Pascal diagnostics timeout observability)
- diagnostics 这条线目前比最初预期更好：在握手超时后，`GetHealthStatus` / `GetDiagnosticInfo.ErrorHistory` 已经能保留真实 timeout 信息。
- 这说明前面几波在 `RecordError` / `GetHealthStatus` / state-string 上的收口已经开始形成组合价值：
  - timeout 不只是 error code
  - 它也进入了 health / diagnostic / state 三层输出
- 所以下一条更值得做的，不是再补一条 diagnostics-only 修复，而是继续把这些真相补到 `GetConnectionInfo` / info callback 的其余字段和 contract 里。

## 2026-03-11 Findings (pure Pascal connection info richness)
- `GetConnectionInfo` 对框架层的价值，不在“有这个 record”，而在“里面的字段是不是够真”。
- 之前 pure Pascal 的几个关键缺口是：
  - `CipherSuiteId` 空
  - `KeySize / MacSize` 空
  - `PeerCertificate` 虽能单独拿到，但 connection-info 快照里没填
  - `SessionId / IsResumed` 缺 focused evidence
- 这波补完后，`GetConnectionInfo` 至少已经从“占位 record”推进到“可以直接拿来打日志/指标/框架桥接”的程度。

## 2026-03-11 Findings (pure Pascal client trust source runtime matrix)
- `custom CA / trust store` 之前的核心缺口不是语义错误，而是证据层级偏弱：
  - 只有 scripted memory-stream contract
  - 还没有本地真实 socket runtime 证据
- 这波本地 runtime matrix 通过后，trust-source 这条线至少已经具备：
  - negative no-trust failure
  - `SetCertificateStore`
  - `LoadCAFile`
  - `LoadCAPath`
  四种真实 socket 行为证据
- 因此 custom CA 这条 M1 项已经比 system-roots 更接近“生产级本地证据闭环”；system-roots 反而是更需要继续扩矩阵的一侧。

## 2026-03-11 Findings (pure Pascal system roots runtime matrix)
- system-roots 这条线当前最弱的点已经不是 builder wiring 或验证主线，而是运行时证据太窄：
  - 原先只有 `www.google.com:443` 单主机探针
  - 这不足以支撑 “production usable” 的保守判断
- network-gated 外网矩阵如果没有“显式 override 真正覆盖默认值”的语义，就会重新引入偶发外部噪音：
  - 调用方以为只测 2 个站点
  - 实际却还会偷偷附带默认 host
- 这波之后，system-roots runtime 已经有一个更稳的最小矩阵：
  - 默认保守 host set：`www.google.com` / `www.cloudflare.com` / `www.github.com`
  - 显式 override：`FAFAFA_SYSTEM_ROOTS_HOSTS`
  - 兼容单主机 override：`FAFAFA_SYSTEM_ROOTS_HOST`
  但它仍然是 Linux-first、network-gated 的证据，而不是完整生产矩阵。

## 2026-03-11 Findings (pure Pascal protocol support truth)
- pure Pascal 在 `Connect/Accept` 层早就已经是 TLS 1.3-only，但 library/capability/default-config 之前还残留着 “TLS 1.2 似乎支持” 的假象：
  - `IsProtocolSupported(sslProtocolTLS12)` 返回 `True`
  - `MinTLSVersion` 还是 `sslProtocolTLS12`
  - default config / context 也默认带 `TLS12`
- 这会直接破坏 capability strategy 里“runtime truth”的原则，也让普通业务开发者更难理解 pure Pascal 当前的真实边界。
- 更一致的语义是：
  - 默认真相：`TLS1.3-only`
  - 显式 opt-in：调用方若主动把 `ProtocolVersions` 收窄到 `TLS12`，连接阶段仍得到 unsupported contract
- 这样做之后，下一波真正实现 TLS 1.2 时也会更干净：届时只需把 truth 从 `TLS1.3-only` 提升为 `TLS12+TLS13`，而不是继续清历史漂移。

## 2026-03-11 Findings (pure Pascal TLS1.2 client minimum slice planning)
- 当前 pure Pascal backend 并不是“缺最后一点 TLS1.2 glue”，而是完整缺少一条 TLS 1.2 handshake 轨道：
  - 没有 `tls12.*` 单元
  - 现有 Hello / key schedule / Finished / record protection 都是 TLS 1.3 形状
- 但它也不是从零开始：
  - transport buffering / retry / timeout / stream semantics 已经可复用
  - trust / hostname / pinning / verify callback 已经可复用
  - X25519 / BigInt / X.509 RSA key extraction 已经可复用
- 因此最合理的路线不是“大而全 TLS1.2”，而是：
  - client-only
  - RSA leaf only
  - X25519-backed `ECDHE_RSA`
  - 单 cipher 先打通
- 另外还暴露出一个架构事实：当前 pure Pascal 的 AES-GCM 路径对“无原生依赖”目标并不完全干净；TLS1.2 首切片要么接受过渡依赖，要么优先考虑 ChaCha20-Poly1305。

## 2026-03-11 Findings (pure Pascal TLS1.2 Phase A foundation)
- Phase A 最有价值的结论是：TLS1.2 并不需要一开始就碰 `freepascal.connection` 主线。
- 先把 4 块无副作用基础件独立出来是对的：
  - wire
  - PRF / master secret / key block
  - ClientHello builder
  - server-flight parser
- 这样下一波接 `DoConnect` 时，风险就主要集中在握手状态机，而不是“边写状态机边补底层工具”。
- 这波也进一步确认了一个设计现实：
  - `ClientHello` 在 TLS1.2 下根本不需要 key_share
  - `X25519` 的复用点会在后面的 `ClientKeyExchange / ServerKeyExchange`，不是 hello builder
- 再往前推一层后，可以确认 `ServerKeyExchange` 的 RSA 校验也适合先抽成独立底座：
  - 证书里的 RSA modulus 往往带前导零
  - verify 层必须按 unsigned modulus 长度归一化，而不是直接按 DER 整数字节长度比较
- 这个细节如果不提前收口，后面真正接 TLS1.2 握手时会很容易在“签名明明是对的但 verify 失败”上浪费时间。

## 2026-03-11 Findings (pure Pascal TLS1.2 local OpenSSL handshake)
- TLS1.2 真正把握手接起来后，最先暴露的不是 `ClientKeyExchange`，而是 record layer 细节：
  - TLS1.2 ChaCha20-Poly1305 的 `fixed_iv_length` 是 12，不是 4
  - nonce 不是简单拼接，而是 `write_iv XOR padded_seq`
  - 加密 `Finished` 的外层 record type 仍然是 `handshake`，不是 TLS1.3 式的 `application_data`
- 这些差异如果沿用 TLS1.3 直觉，很容易得到 OpenSSL 的 `bad record mac`。
- 另外，runtime truth 也必须跟着实现前进：
  - 一旦本地 TLS1.2 handshake 已能成功，`IsProtocolSupported(sslProtocolTLS12)` 继续返回 `False` 就又变成了假话
  - 更稳的语义是：
    - capability truth = 支持 TLS1.2
    - default config/context = 仍推荐 TLS1.3-only
    - `KnownIssues` 明确写出 TLS1.2 当前仍只是窄路径

## 2026-03-11 Findings (pure Pascal TLS1.2 application data and connection info)
- TLS1.2 一旦从握手走到 application-data，最容易错用 TLS1.3 直觉的有两处：
  - 记录外层 type 仍然是实际 type（handshake / alert / application_data）
  - app-data record 的 seq 需要从握手阶段消耗的 `Finished` 之后继续计数
- 这意味着 TLS1.2 握手成功后，client/server write sequence 不能从 0 重新开始，而应从 1 进入 app-data。
- 另外 `GetConnectionInfo` 不能继续只认 TLS1.3 suite 名称；否则 TLS1.2 已经能跑通，但 observability 仍像没实现一样。

## 2026-03-11 Findings (pure Pascal TLS1.2 verify-on and shutdown)
- TLS1.2 verify-on 这条线的好消息是：现有 `ValidatePostHandshake(...)` 基本无需改动。
- 一旦 handshake 已把 `FPeerCertificates` 正确填好，trust-store、hostname、pinning、verify callback 这套版本无关逻辑就能直接复用。
- TLS1.2 shutdown 则暴露了另一个典型坑：
  - 即使 handshake 和 app-data 都已可用，`DoShutdown` 继续只走 TLS1.3 alert helper 仍会让用户面 contract 显得“协议只完成了一半”
- 把 `close_notify` 接上后，TLS1.2 client 的行为才更像一个完整连接，而不是一次性握手探针

## 2026-03-11 Findings (pure Pascal TLS1.2 high-level API contract)
- 这条合同很重要，因为它验证的是用户真正会写的路径，而不是内部 probe：
  - `TSSLContextBuilder.WithTLS12`
  - `WithVerifyPeer`
  - `WithCAFile`
  - `TSSLConnector`
  - `TSSLStream`
- 这也说明当前 TLS1.2 client path 已经不只是底层 connection 实验，而是开始进入“普通业务开发者可消费”的阶段。

## 2026-03-11 Findings (pure Pascal TLS1.2 AES128-GCM local coverage)
- TLS1.2 AES-GCM 和 ChaCha20 的最大差异不在握手，而在 record layer：
  - AES-GCM 有 4-byte fixed IV
  - 8-byte explicit nonce 单独出现在 record payload 前缀
  - AAD 用本地 tracked sequence，而 nonce 里的 explicit 部分不应反推 AAD 序号
- 一开始把 explicit nonce 当作 sequence 去算 AAD，会直接得到 `bad record mac` 级别失败。
- 这波之后，TLS1.2 broader cipher coverage 的工作重点就从“密码模式细节”转移到了“更广互操作矩阵”，因为 CHACHA/AES128 两个代表性 AEAD 路径都已打通。

## 2026-03-11 Findings (pure Pascal TLS1.2 session surface truth)
- TLS1.2 这条线上，session surface 已经是“可用但不可复用”：
  - `GetSession` 非空
  - `Clone` 可用
  - `SetSession` 不会破坏第二次连接
  - 但 `IsResumable=False`
  - 且 `IsSessionReused=False`
- 这是一条很合理的中间真相：普通业务代码已经能拿到会话快照做观测/缓存接口对接，但实现层还没有假装 TLS1.2 resumption 已完成。

## 2026-03-11 Findings (pure Pascal TLS1.2 session metadata and AES high-level contract)
- TLS1.2 session surface 还有一个容易忽略的小裂缝：session snapshot 里虽然有 `CipherName`，但内部 suite-id 之前是 `0`。
- 这不会立刻影响普通业务代码，但会影响更底层/更框架化的消费方，因此值得尽早收口。
- 另一个结论是：一旦 low-level CHACHA/AES path 已经打通，高层 `TSSLContextBuilder + TSSLConnector + TSSLStream` 往往也会顺着变绿；这证明当前主入口抽象没有把 TLS1.2 新路径藏丢。

## 2026-03-11 Findings (pure Pascal TLS1.2 resumption truth matrix)
- TLS1.2 这条线最重要的不是“有没有 session object”，而是不要把 `SetSession` 误读成已实现 resumption。
- 现在通过双 cipher matrix 可以更明确地说：
  - CHACHA 路径：`IsResumable=False`、`IsSessionReused=False`
  - AES128-GCM 路径：`IsResumable=False`、`IsSessionReused=False`
- 这说明当前 truth 是 protocol-wide 的，而不是某条 cipher path 的偶然行为。

## 2026-03-11 Findings (pure Pascal TLS1.2 system-roots harness)
- TLS1.2 的真实 system-roots 证据和 TLS1.3 不同，不能轻率写默认公网 host：
  - 是否仍支持 TLS1.2
  - 是否与当前 cipher/path兼容
  都更容易波动。
- 更稳的做法是先把 harness 和 env contract 固定下来：
  - `FAFAFA_RUN_NETWORK_TESTS=1`
  - `FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS`
  - `FAFAFA_TLS12_SYSTEM_ROOTS_HOST`
- 这样后面补真实外网证据时，不会重新把测试结构和 skip 语义从零再搭一遍。

## 2026-03-11 Findings (pure Pascal TLS1.2 real-world RSA interop)
- 把真实外网 RSA/TLS1.2 站点的关键差异扫出来后，收益最高的补点不是马上做 `P-256 ECDHE`，而是：
  - `rsa_pkcs1_sha512`
  - `rsa_pss_rsae_sha256`
  - `AES256-GCM-SHA384`
- 这些能力一补，就能直接吃下多条 `X25519 + RSA` 的真实站点，而不必先做更重的 `P-256` 纯 Pascal ECDHE。

## 2026-03-11 Findings (pure Pascal TLS1.2 P-256 ECDHE)
- 当前最关键的扩展点不是把 `P-256` 算法重写一份，而是把已有 `tls13.ecdsa` 里的 P-256 算法导出成 TLS1.2 可用的 ECDHE helper。
- 这样做的价值很高：
  - 不重复维护两份 P-256 算法
  - TLS1.2 / TLS1.3 的 pure Pascal curve math 真相开始合流
- 本地矩阵通过后，可以确认当前剩下的公网 `badssl` 问题已经不再是“不会 P-256”，而是更细的真实互操作差异。

## 2026-03-11 Findings (pure Pascal TLS1.2 badssl residual closure)
- `rsa2048.badssl.com` 最终证明不是单点问题，而是三类能力叠加后的残差：
  - `P-256 ECDHE`
  - `rsa_pkcs1_sha512`
  - `AES128-GCM`
- 这也说明真正高价值的调试方式不是盲猜公网站点，而是：
  - 先拆站点能力画像
  - 再用本地 OpenSSL 把同组合复现到绿
  - 最后回到公网验证
- 一旦这三层能力都绿，公网残差自然收口。

## 2026-03-11 Findings (pure Pascal ALPN runtime evidence)
- 当前 `ALPN` 的主要残留已经不是 parser / state field，而是证据层：之前只有 scripted 骨架，还缺真正贴近业务入口和真实服务的证明。
- 一旦把 `TSSLContextBuilder.WithHTTP2 + TSSLConnector + TSSLStream` 接到本地 OpenSSL `-alpn h2`，可以确认：
  - convenience API 没有把 ALPN 丢在 builder 层
  - `GetSelectedALPNProtocol` 与 `GetConnectionInfo.ALPNProtocol` 在真实握手后保持一致
- TLS1.2 这条线上，`ServerHello` 的 `ALPN` parse 也已不只是结构代码；真实 OpenSSL `s_server -tls1_2 -alpn h2` 已经通过。
- 外网 runtime 结果表明，现在更值得投入的是扩大 host/service class 覆盖，而不是继续怀疑当前 ALPN 协商主线本身。

## 2026-03-11 Findings (pure Pascal ALPN fallback matrix)
- `ALPN` 真正有价值的 contract 不只是“有没有协商结果”，还包括：
  - offer 列表顺序是否被正确处理
  - 当服务端只提供低优先级协议时，客户端是否能稳定 fallback
- 本地 OpenSSL oracle 已经证明：
  - TLS1.3: `h2,http/1.1` -> `http/1.1`
  - TLS1.2: `h2,http/1.1` -> `http/1.1`
- 外网 multi-host matrix 进一步说明，当前 ALPN runtime harness 值得保留 `expected protocol` 断言能力；否则只能证明“谈成了某个 offered protocol”，证据不够硬。

## 2026-03-11 Findings (pure Pascal ALPN empty negotiation)
- `ALPN` 还有一个容易被漏掉的 service class：服务端根本不返回 ALPN 扩展。
- 这时真正应当锁住的不是“协商失败”，而是：
  - 握手仍成功
  - `GetSelectedALPNProtocol=''`
  - `GetConnectionInfo.ALPNProtocol=''`
- 这类 contract 的价值很高，因为它能防止后续实现把旧值残留、把默认值伪装成协商结果，或者把“无 ALPN”误报成握手错误。

## 2026-03-11 Findings (pure Pascal ALPN client no-offer)
- `server-no-alpn` 和 `client-no-offer` 看起来都得到空协商结果，但它们覆盖的是两类不同风险：
  - 前者防止服务端没发扩展时被误报
  - 后者防止客户端根本没发 ALPN 时，还错误继承或伪造协商结果
- 把这两类都锁住之后，ALPN 的空协商语义才算真正站稳，而不是只覆盖了一半方向。

## 2026-03-11 Findings (pure Pascal TLS1.2 session-id resumption)
- TLS1.2 这条线上，最小可落地的 actual resumption 不是 tickets，而是 session-id abbreviated handshake。
- 真正卡住这条线的根因有三层：
  - session snapshot 没保存 `session_id + master_secret`
  - client 握手路径没有 abbreviated branch
  - resumed `ServerHello` 不一定带 extensions，旧 parser 过度乐观
- 还有一个执行层面的细节：对本地 OpenSSL `s_server -www` 来说，只看两次 `Connect` 并不足以稳定触发第二次 reuse；补最小 GET/read 后，local oracle 就能稳定进入 reused path。

## 2026-03-11 Findings (pure Pascal TLS1.2 resumption entry/runtime evidence)
- 一旦 low-level TLS1.2 session-id resumption 打通，高层 `TSSLConnector.WithSession(...)` 路径也会顺着变绿；这说明主入口抽象没有把 resumption 丢在 wrapper 层。
- 另一个结论是：外部 runtime harness 最好沿用 `system-roots` 那套显式 host 驱动模式，不默认硬绑公网主机，否则 resumption 这类能力很容易变成门禁噪音。

## 2026-03-11 Findings (pure Pascal TLS1.2 external resumption matrix)
- 用当前 pure Pascal client 去跑公网 TLS1.2 resumption，结果已经不是“全失败”：
  - `www.apache.org` => reused
  - `www.perl.org` => reused
  - `rsa2048.badssl.com` => reused
- 再对照 `openssl s_client -tls1_2 -reconnect`，这些 host 本身都支持 resumed handshake。
- 最合理的技术推断是：
  - 当前 pure Pascal 的 TLS1.2 session-id + ticket resumption 已经对一批公网 RSA host 成立
  - 下一步更像是做 breadth，而不是继续追单点残差

## 2026-03-11 Findings (pure Pascal TLS1.2 ticket resumption)
- local `OpenSSL s_server -no_cache` 是把 TLS1.2 ticket 路径单独逼出来的最好 oracle：
  - first handshake 要先把 `NewSessionTicket` 真正吃进 session
  - second handshake 才能在没有 server-side session cache 的情况下复用
- 这条线真正的根因不是单点：
  - 没有 TLS1.2 `NewSessionTicket` parser
  - ClientHello 不会发 extension 35
  - session snapshot 不保存 TLS1.2 ticket
  - resumed session 如果 server 不立刻发新 ticket，会丢掉“继续可复用”状态
- 另外还有一个更隐蔽的执行层面问题：external runtime harness 的请求如果不带 `Host`，像 `rsa2048.badssl.com` 这样的站点会让证据不稳定；补上 `Host` 后，strict reuse 才稳定落地。

## 2026-03-11 Findings (pure Pascal server runtime entry)
- pure Pascal 服务端之前虽已有：
  - server accept skeleton
  - embedded local roundtrip
  但这两类证据都还不是框架作者最常走的入口。
- 一旦把
  - `TSSLContextBuilder.BuildServer`
  - `TSSLConnectionBuilder.BuildServer`
  - 真实 socket
  - `openssl s_client`
  这条链跑通，就能确认当前服务端主入口抽象至少在 TLS1.3 最小路径上已经站稳。

## 2026-03-11 Findings (pure Pascal TLS1.2 server minimum slice RED)
- 当前 pure Pascal 服务端真正还没补上的，不是 TLS1.3 入口，而是 TLS1.2 accept 主线本身。
- 这点现在已经由本地 OpenSSL 客户端合同直接锁住：
  - `BuildServer` 能创建 server context
  - 但 `BuildServer` 后的 `Accept` 仍在 `DoAccept` 里直接拒绝非 TLS1.3
- 这条 RED 很关键，因为它把下一个大缺口从“可能需要做服务端什么”收敛成了“明确要做 TLS1.2 server minimum slice”。

---

## 2026-03-13: digital_signature 私钥密码保护

- `examples/digital_signature` 之前 README 标记“密码保护未实现”，现已通过 `-p/--password` 实现闭环：生成加密私钥 PEM + 读取加密私钥签名。
- 新增 contract 脚本确保：编译产物隔离在 `tmp/`，不会污染 git 工作区；wrong password 必须失败。

## 2026-03-14 Findings (Wave B/B2 manual gate failures)
- Windows WinSSL：Schannel 错误映射中仍引用旧错误码 `sslErrEncryption`，但统一错误码已在 Phase 4 收敛为 `sslErrDecryptionFailed` / `sslErrEncryptionFailed`。
- Windows modules：runner 上 `validate_all_modules.ps1` 编译 OpenSSL 单元时缺 `Contnrs`/`SyncObjs`，更像是 FPC 变体/位数选择导致的 units 缺口；优先 `x86_64-win64` 有助于避免“32-bit 只装 RTL”的环境陷阱。
- macOS modules：P2 模块大面积失败但 Store/TS/CT 仍可过，最可疑的是 OpenSSL 动态库误加载系统 `libcrypto.dylib/libssl.dylib`；优先尝试 `libcrypto.3.dylib/libssl.3.dylib` 能显著降低该风险，同时需要上传 probe JSON 与 module-tests reports 便于证据定位。

## 2026-03-14: Prefer `SSL_get0_verified_chain` for issuer lookup

Problem:
- OCSP issuer selection previously fell back to re-running `X509_verify_cert` to obtain a verified chain.
- This is redundant after a successful handshake (`SSL_get_verify_result = X509_V_OK`) and adds cost/noise.

Decision:
- Bind `SSL_get0_verified_chain` from libssl when available and prefer it as a fallback chain source.
- Keep the existing `X509_verify_cert` fallback for older OpenSSL builds or when the verified chain is unavailable.

Impact:
- Avoids unnecessary verify pass in OCSP paths when the handshake already produced a verified chain.
- Keeps behavior compatible via fallback.

## 2026-03-14: Cert verify cache fingerprint must be fail-safe

Observation:
- `TCertVerifyCache.ComputeFingerprint` previously returned a 32-byte result even when fingerprint computation failed (early Exit paths), which could lead to non-deterministic cache keys.
- Implementation also relied on BIO + fixed 4096-byte DER buffer.

Decision:
- Initialize `Result := nil` and only return 32 bytes on successful digest.
- Prefer `X509_digest(..., EVP_sha256())` when available; fallback to `i2d_X509` DER + EVP digest.
- Ensure core/EVP/X509 modules are attempted to be loaded inside fingerprint computation.

Verification:
- Added `tests/openssl/test_cert_verify_cache_regression.pas` covering nil safety and Put/TryGet determinism.

## 2026-03-20 Findings (context server-mode ServerName isolation)
- 旧 `task_plan.md` 里关于 “connection server-mode SNI isolation 已完成” 的记录不能直接当真相：当前磁盘上既没有对应的 plan/test 文件，5 个 backend 的连接构造路径也仍在无条件复制 context `ServerName`。
- 这说明这条线最可靠的证据源仍然是当前磁盘代码 + 当场回归，而不是历史 planning memory。
- 正确的 owner boundary 分成两层：
  - server context 仍可为了兼容性保留 deprecated `ServerName` 状态
  - 但 server-side connection 不应继承这个 client-only fallback
- 这与上一批 `BuildServer` 修复并不冲突；两者分别解决的是两个不同的问题：
  - builder server path 之前会静默丢字段
  - connection constructor 现在则需要停止把该字段泄漏到 server connection
- 最小安全修复点在 backend connection constructor / allocation path，而不在 builder 或 factory：
  - 仅当 `GetContextType = sslCtxClient` 时，才把 context `ServerName` 注入新连接
- 用 FreePascal-safe regression 锁合同已经足够，因为它能在当前 Linux harness 中直接观测 `GetServerName`；随后再用 `compile_all_modules.py` 覆盖 OpenSSL / WolfSSL / MbedTLS 的编译面。

## 2026-03-20 Findings (context builder server SNI validation alignment)
- 前一批把 runtime owner boundary 收紧后，`ValidateServer` 仍然停留在旧语义：它直接复用 `ValidateClient`，所以 server `.WithSNI(...)` 仍收到“去用 `ISSLClientConnection.SetServerName`”这类 client-only 指南。
- 这不只是措辞问题，而是 validation/runtime drift：
  - runtime 已明确 server-side connection 会忽略 context-level `ServerName`
  - validation 却还在暗示 server path 应继续把它当成 client connection hostname 配置
- 把 shared checks 抽到 common helper 是这波最小稳妥点：
  - 既保留 client 现有 warning 合同
  - 又让 server path 可以给出准确的“deprecated but ignored on server connections”说明
  - 同时顺手消掉 `sslVerifyPeer` 无 CA 时 client/server duplicated warning
- 一个可见副作用是 `BuildServerWithValidation` 的 warning 数从 2 降到 1；这是更准确信号，不是回归，因为去掉的是重复 client wording 而不是实质安全提示。

## 2026-03-24 Findings (cert utils conversion post-success cleanup family)
- `PEMToDER(...)` / `DERToPEM(...)` 这里实际存在两套不同契约，不能混成一个“大而化之的 helper 缺失处理”：
  - 入口 helper 缺失时，conversion helper 走 empty/`False` 降级
  - 输出已经 materialize 之后的 cleanup helper 缺失时，direct/Try 都应保留已产出的结果
- 这批最小正确修复点只在 `finally` cleanup call-site：
  - `PEMToDER(...)` 的 `X509_free(LCert)` 和外层 `BIO_free(LBIO)`
  - `DERToPEM(...)` 的内层 `BIO_free(LBIO)` 和外层 `X509_free(LCert)`
- 直接把 entry helper gate 放宽并不是正确修法；那会破坏已存在的 conversion BIO degrade 合同。
- 新 family 测试暴露了一个 harness 真相：
  - delayed-loss stub 如果不在 direct / Try 每次调用前重装，`BIO_free := nil` 会从前一次 direct 调用泄漏到下一次 Try 调用
  - 那测到的是跨调用全局副作用，不是 Try 包装器自己的 post-success cleanup 契约
- 因此这批除了生产 guard，还需要把测试 harness 改成可重入的 delayed-loss 准备逻辑；这和之前 `GetInfo(...)` cleanup family 的 `Rearm...Stub` 模式一致。

## 2026-03-24 Findings (cert utils fingerprint post-success cleanup family)
- `GetFingerprint(...)` 也和 conversion 一样，存在两段不能混淆的契约边界：
  - 指纹前置 helper 缺失时，direct 仍要走 controlled `ESSLCertError`，`TryGetFingerprint(...)` 仍要走 `False` + clear output
  - 指纹字符串已经 materialize 之后的 cleanup helper 丢失时，不应该再把成功路径翻译成异常或假失败
- 这批最小正确修复点只有两个：
  - `GetFingerprint(...)` 的 `X509_free(LCert)`
  - `GetFingerprint(...)` 的外层 `BIO_free(LBIO)`
- 入口 `BIO_free` gate 不能放宽；否则会破坏已有 `GetFingerprint when BIO_free is unavailable` 的 exception-contract。
- 因为 `BIO_free` 参与 entry gate，这个 family 的 `BIO_free` delayed-loss 场景必须像 `GetInfo(...)` cleanup family 那样重装/rearm：
  - 先让 entry gate 正常通过
  - 再在 `X509_free(...)` 成功后把 `BIO_free` 置空
- 这批验证后，fingerprint 语义更清晰了：
  - “算不出来”仍然是 controlled failure
  - “已经算出来，只是清理 helper 不见了”则是保留结果

## 2026-03-24 Findings (cert utils SignCertificateWithKey EVP_sha256 nil-result family)
- `SignCertificateWithKey(...)` 之前只检查了 `Assigned(EVP_sha256)`，但没有检查 `EVP_sha256()` 的返回值；这让“符号仍存在但 helper 返回 `nil`”这种真实故障形态漏过了入口 guard。
- 这个缺口是共享 helper 级别的，不是单一路径问题：
  - `GenerateSelfSigned(...)`
  - `GenerateSigned(...)`
  都会经由同一个 signing helper 受影响。
- 最小正确修复点不在 public generate flow，而在 helper 内部：
  - 先缓存 `LDigest := EVP_sha256()`
  - `LDigest = nil` 时立即失败
  - 只有拿到有效 digest 后，才允许“先用 SHA-256 签名，失败后再尝试 nil-digest fallback”的旧兼容路径继续存在
- 这样既保住了现有 Ed25519 的 `X509_sign(..., nil)` 特殊路径，也避免把 RSA/ECDSA 的 digest-constructor 故障误翻译成“签名可继续成功”。
- 新 focused family 还暴露了一个 harness 真相：
  - 如果 direct/Try 断言结束后不恢复全局 `EVP_sha256` wrapper，后续 signed warmup 会在修复后提前失败
  - 这测到的是测试夹具泄漏，不是 shared signing helper 的真实 contract
- 因此这批除了生产修复，还必须让 contract test 在每个断言簇结束后恢复原始 `EVP_sha256`，把 nil-result stub 严格限制在目标调用窗口内。

## 2026-03-24 Findings (cert utils GenerateSigned certificate PEM export PEM_write_bio_X509 symbol guard)
- 这批与前一条 `BIO_new` constructor family 的边界不同：这里不是“构造 BIO 失败”，而是 helper gate 已通过、constructor 也已成功后，证书 PEM 写入 helper `PEM_write_bio_X509` 才在局部窗口里消失。
- 因为 `HasCertificatePEMWriteBIOHelpers` 在入口已经检查过 `PEM_write_bio_X509`，正确的 RED 必须用 wrapper 把 helper loss 推迟到 `BIO_new(...)` 成功之后；否则测到的只会是入口 gate，不是目标 call-site。
- 当前磁盘状态表明这一批的 local guard 已经存在于 `GenerateSigned(...)` 证书导出块里；这次工作的关键不是再补同样的代码，而是把“已有实现是否真的闭环”做成 fresh evidence。
- 这批验证后可以确认当前 signed certificate PEM export 成功路径的边界是稳定分层的：
  - `BIO_new` constructor loss => 已有独立 contract 兜住
  - `PEM_write_bio_X509` write loss => direct `ESSLCertError`，Try `False` + clear
- `BIO_free` cleanup loss => 仍由后续 cleanup family 单独约束
- 再加上 self-signed 同 helper contract 也保持绿灯，说明这次 shared helper 名称相同但 owner boundary 仍然是“各自 public flow 局部 guard”，没有相互踩踏。

## 2026-03-24 Findings (factory config ServerName isolation)
- 这条 family 的原始 plan 假设已经过时了：当前 `TSSLFactory.CreateContext(const AConfig: TSSLConfig)` 路径并不会把 `LConfig` 写回 shared library default config。
- 当前源码里的真实边界已经清晰分开：
  - `CreateContext(AContextType, ALibType)` 读取 library singleton 的 default config，并把其中的 `ServerName` 应到新 context
  - `CreateContext(const AConfig)` 只对传入的 one-shot config 做 `NormalizeConfigOptions(...)`，然后直接应用到返回的 context，不污染共享默认值
- FreePascal 后端是这条 contract 的有效探针，因为它会把 context `ServerName` 继承到新建 client connection；因此 factory 级 sticky-state 漏洞能在本地 Linux harness 里直接观测到。
- focused contract 同时锁住了两条边界：
  - 显式 `ISSLLibrary.SetDefaultConfig(...)` 仍然会影响 default-path context / connection
  - one-shot `CreateContext(AConfig)` 只影响当次 context / connection，后续 default-path context / connection 不继承 `sticky.example.com`
- 相邻回归进一步说明这不是偶然通过：
  - FreePascal context-level `ServerName` inheritance 仍正常
  - connection builder precedence 仍保持 `override > context default > empty`
  - full compile 仍是 `181/181`
- 结论：
  - 这批缺的不是运行时代码，而是 working-memory closeout
  - `factory config ServerName isolation` 现在可视为已正式收口，不应再被重新当成待修复 bug

## 2026-03-24 Findings (cert utils GenerateSelfSigned EVP_PKEY_new closeout rerun)
- 这条 family 的 runtime hardening其实已经在 2026-03-23 收口；本轮 fresh rerun 的价值是补齐缺失的 `task_plan.md` 记录，并重新确认当前工作树没有回归。
- 当前源码在 [GenerateRSAKey](/home/dtamade/projects/fafafa.ssl/src/fafafa.ssl.cert.utils.pas#L489) 上仍保持正确边界：
  - `Assigned(EVP_PKEY_new)` 先行检查
  - `EVP_PKEY_new()` 返回 `nil` 时走 controlled `ESSLCertError`
  - 在新失败路径上先释放 `LKey`，不遗留已生成的 RSA 结构
- focused contract 与相邻 RSA/EVP regression 一起说明这条线当前是稳定闭环，而不是偶然的单点通过：
  - `EVP_PKEY_new` missing contract PASS
  - later `EVP_PKEY_assign` contract PASS
  - earlier `RSA_generate_key_ex` contract PASS
  - full compile PASS (`181/181`)
- 结论：
  - 这批不需要新的生产改动
  - `GenerateSelfSigned EVP_PKEY_new` 不应再被当成开放 family，除非后续出现新的 failing contract 或 baseline 回归

## 2026-03-24 Findings (context builder PKCS#11 PIN source serialization contract closeout rerun)
- 这条 family 的原始 RED 在当前工作树里已经不存在；缺口是 working-memory closeout，而不是新的运行时代码。
- 当前源码已经把边界收得比较清楚：
  - `ExportToJSON` / `ExportToINI` 只在 `IsSerializablePKCS11PINSourceMethod(...)` 为真时导出 `pkcs11_pin_method` + `pkcs11_pin`
  - 这个可序列化集合仅包含 `pmEnvironment` / `pmFile`，因此 direct `pmValue` PIN 不会出现在 export surface
  - `ImportFromJSON` / `ImportFromINI` 会先恢复显式 `pkcs11_pin_method`；如果只有 `pkcs11_pin`，则仍按 direct PIN 默认回落到 `pmValue`
  - `Merge(...)` 通过 source JSON 重新注入 `pkcs11_uri`、`pkcs11_pin_method`、`pkcs11_pin`，因此 env/file source 语义不会在 merge 时静默掉回 `pmNone`
- focused round-trip/merge contract 和相邻 `TryBuildServer` 回归共同证明，这些非秘密状态边界现在是真正可观测的：
  - JSON round-trip 之后，缺失环境变量仍报告 environment-variable source error
  - INI round-trip 之后，缺失 PIN 文件仍报告 pin-file source error
  - Merge 之后，缺失环境变量错误语义仍然保留
- 这也说明当前 builder surface 的安全边界保持住了：
  - non-secret env/file source metadata 可以持久化
  - direct PIN value 继续不导出
  - callback / interactive 继续不作为 builder serialization/runtime 支持路径
- 结论：
  - 这条 `pkcs11 pin source serialization` family 现在可视为已正式收口
  - 后续如果继续这条线，应转去看 `Override(...)` parity，而不是重新打开 export/import/merge 本身

## 2026-03-24 Findings (context builder Override PKCS#11 PIN method parity closeout rerun)
- 这条 family 在当前工作树里也已经不再是开放 bug；差的是 fresh evidence 和 working-memory convergence。
- 当前 `Override(...)` 的边界已经与计划目标对齐：
  - 识别 `pkcs11_pin_method`
  - 通过 `TryParsePKCS11PINMethodValue(...)` 接受大小写不敏感的 enum 名和值 ordinal
  - 非法值继续 no-op，保持 override surface 的防御式行为
- 与之前 `pkcs11 pin order sensitivity` family 的修复叠加后，override 现在具备了正确的组合语义：
  - `.Override('pkcs11_pin_method', 'pmEnvironment').Override('pkcs11_pin', ...)` 不会把 method 重置回 `pmValue`
  - `.Override('pkcs11_pin', ...).Override('pkcs11_pin_method', 'pmEnvironment')` 也能在最终状态上得到 env/file source method
  - 只有 `pkcs11_pin` 单独出现时，才继续保持 direct-PIN 默认语义
- focused transformation/export 与 runtime regression 一起说明 override parity 已经不是“只在状态面上看起来对”，而是 runtime 也真的走到了 builder source-resolution contract：
  - exported JSON 会显式带出 `pkcs11_pin_method`
  - override-configured missing env source 仍报 environment-variable resolution error，而不是把 source name 当 direct PIN
- 相邻 `config_import_export` 继续为这个边界兜底：
  - named `pkcs11_pin_method` import 仍可接受
  - `pkcs11_pin`-only import 仍默认回到 `pmValue`
- 结论：
  - `Override PKCS#11 PIN method parity` 现在可视为已正式收口
  - 这条线的下一个合理队列是别的 override parity gap，而不是重新打开 PKCS#11 PIN method 本身

## 2026-03-24 Findings (context builder Override PEM source parity closeout rerun)
- 这条 family 的原始 RED 在当前工作树里同样已经不存在；真正缺的是 fresh verification evidence 和 working-memory closeout。
- 当前 `Override(...)` 在 PEM 方向上的边界已经与 fluent setters 对齐：
  - `Override('certificate_pem', ...)` 会设置 `FCertificatePEM` 并清掉 `FCertificateFile`
  - `Override('private_key_pem', ...)` 会设置 `FPrivateKeyPEM` 并清掉 `FPrivateKeyFile`
  - unknown-field no-op 语义仍保持不变
- 结合当前 build precedence，可以确认这不是“只在导出 JSON 上看起来正确”：
  - 构建路径现在是 certificate `PEM > file`
  - private key 则是 `PKCS#11 > PEM > file`
  - 因此 override PEM 只要清掉 stale file state，就能稳定绕开缺失文件路径并成功构建
- focused transformation/export + runtime precedence 回归共同证明这两个层面都已闭环：
  - export-visible state 会清掉 `certificate_file` / `private_key_file`
  - client/server runtime 不再被 stale missing-file state 拖回错误路径
- 相邻 `Try*` / import-export / validation / compile-all 继续说明这不是局部偶然通过，而是当前 builder surface 的一致行为。
- 结论：
  - `Override PEM source parity` 现在可视为已正式收口
  - file/PEM mutual-exclusion cluster 在当前主要 mutation surfaces 上已基本闭环，后续应转去扫描别的未正式 closeout 的 context-builder family

## 2026-03-25 Findings (cert utils GenerateSelfSigned private-key export post-success cleanup family)
- `GenerateSelfSigned(...)` 这里还剩下一条和 outer cleanup 不同的 successful-path 边界：
  - certificate PEM 已经 materialize
  - private-key PEM 也已经在第二次 `BIO_read(...)` 后 materialize
  - 但 private-key export `finally BIO_free(LBIO)` 仍可能因为 delayed-loss helper 丢失，把成功路径翻成异常或 `False`
- 这条 family 和 entry-missing `BIO_free` 不能混成同一件事：
  - `BIO_free` 在入口就缺失时，`GenerateSelfSigned` 仍应走 controlled failure，`Try*` 仍应返回 `False` + clear outputs
  - 只有当 certificate/key PEM 都已 materialize 之后，cleanup helper 才可以被视为 non-authoritative，不再推翻成功结果
- 最小正确修复点只在一个 call-site：
  - `GenerateSelfSigned(...)` private-key PEM export block 的 `finally BIO_free(LBIO)`
- 旧的 `tests/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract.pas` 不是严格的 entry-missing contract：
  - 它通过 `BIO_read(...)` wrapper 让第二次 private-key read 成功后才清掉 `BIO_free`
  - 所以它本质上已经是 delayed-loss wrapper contract，应该和新的 public contract 一致地保留成功
- 这批修复后的 contract 分层更清晰：
  - broad BIO entry-missing baseline 继续约束 “helper 一开始就缺失”
  - new family + aligned legacy wrapper 约束 “输出已 materialize 后的 cleanup helper 丢失”
  - outer post-success cleanup family 继续约束更晚的 `X509_free` / `EVP_PKEY_free` delayed-loss

## 2026-03-25 Findings (cert utils GenerateSigned Ed25519 keygen family validation closeout)
- 这条 family 的 focused contract 在当前工作树里直接通过，说明此前对共享 `GenerateEd25519Key(...)` 的 hardening 已经同时覆盖：
  - `GenerateSelfSigned(...)`
  - `GenerateSigned(...)` 的 RSA-CA-signed Ed25519 leaf 路径
- new signed-path contract 锁住了三个 delayed-loss helper 窗口：
  - `EVP_PKEY_keygen_init`
  - `EVP_PKEY_keygen`
  - `EVP_PKEY_CTX_free`
  它们现在都保持一致的 public contract：
  - direct `GenerateSigned(...)` 抛 controlled `ESSLCertError`
  - `TryGenerateSigned(...)` 非抛出、返回 `False`、并清空输出
- self-signed sibling 和 baseline Ed25519 contract 同时保持绿灯，说明这不是“signed path 测巧了”，而是 shared Ed25519 keygen helper 的行为真的已经闭环。
- 结论：
  - Milestone 2 里这条 signed Ed25519 keygen delayed-loss family 现在应视为已正式收口
  - 除非后续出现新的 failing contract 或 baseline 回归，否则不要再把它当作待修复代码问题重开

## 2026-03-25 Findings (Milestone 3 context-builder SNI validation/runtime rerun closeout)
- 当前源码、validation 文案与 runtime owner boundary 仍然保持一致：
  - `ValidateClient` 继续把 `.WithSNI(...)` 标记为 deprecated，并明确指向 per-connection `ISSLClientConnection.SetServerName(...)`
  - `ValidateServer` 继续说明 server context 上保留的 legacy `ServerName` 会被 server-side connections 忽略，不再给出 client-only 指南
  - `BuildServer` 仍保留 context-level legacy `ServerName` state
  - server-side connections 仍不会继承这个 client-only fallback
  - FreePascal client-side connections 仍会继承 context `ServerName` fallback
- 这说明 Milestone 3 当前至少在最核心的 SNI cluster 上没有新的 validation/runtime drift：
  - warning wording 是准的
  - server/client owner boundary 是准的
  - client inheritance fallback 也没被前面的兼容性修复误伤
- 结论：
  - 本轮 Milestone 3 fresh discovery 没有给出新的 SNI family RED
  - 后续 discovery 应离开这组已绿基线，转去看 non-SNI 的 context-builder drift；只有拿到新的 failing contract，才值得重新打开这组 SNI family

## 2026-03-25 Findings (auto-backend state serialization/merge rerun closeout)
- 当前工作树上的 `FAutoSelectBackend / FBackendRequirements` family 仍然保持完整闭环：
  - JSON round-trip 继续保留 unmet auto-backend requirements
  - INI round-trip 继续保留 unmet auto-backend requirements
  - `Merge(...)` 继续把 source builder 的 active auto-selection mode 带到 destination
- 相邻行为也仍然一致，没有出现“state 面绿了，但 runtime 回退了”的反向漂移：
  - `TryBuildClient` 仍会在 unmet auto-backend requirements 下返回 `No suitable SSL backend found for requirements`
  - `Override('explicit_backend', ...)` 仍能替换 stale auto-backend requirements
  - selector minimum-score filtering 仍会排除 zero-score backend
  - presets 在当前 builder state surface 上没有被误伤
- 本轮唯一失败来自审查命令本身，而不是产品代码：
  - 第一版 `tests/config` audit 没有先创建 `-FE` 输出目录，FPC 直接报 path missing
  - 修正为 per-file `mkdir -p` 后，整组 `tests/config/*.pas` 全绿
- 结论：
  - 这条 `auto-backend state serialization/merge` family 在当前工作树上可视为已正式收口
  - 后续不应为了“继续”再回到这条 backend-selection state line；除非拿到新的 failing contract

## 2026-03-26 Findings (repo audit / development advice / roadmap review)
- 当前工作树在 Linux 上并不是“失稳状态”：
  - `python3 scripts/compile_all_modules.py` 通过（`182/182`）
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` 通过（compile + P2 modules + Phase 2 dry-run）
- 但 repo-level 最大风险已经转成治理漂移，而不是单点实现失败：
  - `README.md` 仍使用 `Production Ready`、`Tests 100% passed`、`415 个测试文件` 等强宣称
  - `docs/testing/TESTING_README.md` 仍保留 `176` tests / `95.5%` / examples 目录执行等旧口径
  - `docs/DOCUMENTATION_INDEX.md` 仍把已删除的 `DEVELOPMENT_ROADMAP_2026.md` 当成路线图入口
  - `docs/README.md` 仍把 Wave C closeout chain 当“当前工程入口”
  - 而 `task_plan.md` / `docs/plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md` 已明确主线切到 `SSL/TLS backend completeness roadmap`
- 这会带来两个直接后果：
  - 外部读者会高估当前“产品完成度”与“测试闭环范围”
  - 内部推进者会在 Wave C closeout、历史 roadmap、当前 completeness roadmap 之间来回跳转
- 另一个结构性风险是 gate coverage 与当前产品主线不完全匹配：
  - 当前最小门禁主要覆盖 compile、P2 模块和 Phase 2 dry-run
  - 但 pure Pascal backend 当前主线的关键增量是 TLS 1.3 suite parity、client/server resumption/PSK，以及后续 0-RTT / validation hardening
  - 这些更接近产品差异化的路径还没有被提升到默认 gate 层级
- 结论：
  - 当前 repo 最需要的不是“再写一批新计划”，而是先把状态叙事、路线图入口与 gate 分层收敛成同一套真相来源
  - 路线图本身方向是合理的：先做 FreePascal backend completeness，再推进 `0-RTT / OCSP stapling / CT / validation`
  - 但路线图需要从“很多 family-sized plan”升级为“一个外部可导航的 canonical roadmap + 若干实现批次计划”

## 2026-03-27 Findings (repo audit follow-up / roadmap governance confirmation)
- 今天重新跑过当前 Linux 基线后，可以确认“实现层健康、治理层漂移”这个判断仍然成立：
  - `python3 scripts/compile_all_modules.py` 继续通过（`182/182`）
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` 继续通过（compile + `17/17` P2 modules + Phase 2 dry-run）
- 文档入口的主要问题不是缺文档，而是 canonical entry 分裂：
  - `docs/README.md` 仍把 Wave C closeout chain 当作“当前工程入口”
  - `docs/DOCUMENTATION_INDEX.md` 顶部“从这里开始”也仍然优先把读者导向 Wave C closeout 页面
  - 同时它在“路线图与计划”区还继续链接已删除的 `DEVELOPMENT_ROADMAP_2026.md`
  - 这会让新进入仓库的人先读旧 closeout，再撞上不存在的 roadmap，再回头去 working memory 找当前主线
- 架构/API 文档也存在“概念层过时”而不只是链接漂移：
  - `docs/reference/ARCHITECTURE.md` 仍把项目描述为四后端架构，没有把当前已经落地的 `sslFreePascal` backend 放进主叙事
  - 但真实代码里 `src/fafafa.ssl.base.pas` 已公开 `sslFreePascal`，`src/fafafa.ssl.freepascal.lib.pas` 也实现了完整 `ISSLLibrary`
  - `docs/reference/API_REFERENCE.md` 仍挂着 `v0.8 / 2025-10-24` 口径，且接口示例与当前源码签名已发生漂移（例如 `IsFeatureSupported` / capability surface）
- 这意味着当前 repo 的首要开发风险已不再是“代码是否能编”，而是“外部认知是否被旧文档带偏”：
  - README 的 `Production Ready` / `Tests 100% passed` / `415 个测试文件` 这类表述会放大这种偏差
  - 当 gate 绿灯与 public claim 之间没有明确 scope 说明时，外部读者会默认把它理解成“产品功能面已全面闭环”
- 路线图本身方向继续是合理的：
  - 先做 FreePascal backend completeness
  - 再推进 `0-RTT / OCSP stapling / CT / validation hardening`
  - 这个方向与 `src/fafafa.ssl.freepascal.lib.pas` 当前 `KnownIssues` 所暴露的剩余缺口也一致
- 但路线图表达层还需要一次结构升级：
  - 把“当前产品主线 roadmap”与“family-sized implementation plan”分层
  - 对外只保留一个 canonical roadmap/status page
  - 对内继续保留窄 plan 作为批次执行文档
- 另一个值得尽快补上的缺口仍是 gate coverage 与产品主线的距离：
  - 当前默认最小门禁主要覆盖 compile、P2 OpenSSL-related modules 与 Phase 2 dry-run
  - pure Pascal backend 当前差异化主线已经走到 TLS 1.3 suite parity、client/server resumption/PSK
  - 如果这些路径不被提升到默认 gate 或 required CI，路线图就会继续“写得比守得住的更多”

## 2026-03-27 Findings (canonical roadmap/status convergence + FreePascal TLS 1.3 focused gate landing)
- 这次 review 的两条最高优先级建议在当前树上都证明是值得立即落地的：
  - canonical roadmap/status 入口确实需要单点化
  - pure Pascal 主线也确实需要一条比 `run_minimal_ci_gate.sh` 更贴近产品完整度的 focused gate
- 路线图入口层的最小正确做法不是“删掉所有历史 Wave C 页面”，而是分层：
  - `docs/ROADMAP.md` 作为当前稳定入口
  - Wave C closeout / current chain 继续保留，作为收口状态与历史链路参考
  - 这样既不会破坏已有 docs contracts，也不会再让外部读者把旧 closeout 当成当前产品路线图
- readiness 文案的关键风险已经得到收敛：
  - `README.md` 过去的 `Production Ready` / `Tests 100% passed` / `415 个测试文件` 这类表述会让外部读者把局部 gate 误读成“功能面全面闭环”
  - 改成基于当前 build/gate/roadmap 的表述后，public claim 与 repo 真相源更一致
- focused gate 的真实回归根因并不在测试本身，而在脚本执行环境：
  - `scripts/run_freepascal_tls13_completeness_gate.sh` 原先通过 `bash -lc` 跑每个 test group
  - 当前开发机里 `fpc` 来自额外 PATH（`/opt/fpcupdeluxe/...`）
  - login shell 会重写 PATH，导致 gate 自己把可用的 `fpc` 丢掉
- 这也是为什么“先前 compile_all_modules 能过、focused gate 却报 `fpc: command not found`”并不矛盾：
  - 前者直接在当前环境执行
  - 后者切进了被 login shell 改写过的子环境
- 最小正确修法因此不是改测试，也不是强行写死本机路径，而是让 gate 继承当前环境：
  - focused gate contract 现在会用 PATH shim 的 fake `fpc` 验证这一点
  - `run_cmd()` 改成 `bash -c` 后，既保留了“每个命令在独立 shell 中执行”的隔离语义，也修掉了 PATH 丢失
- 结果上，这批改动把 repo review 里的治理建议真正推进了一步：
  - 外部入口现在有稳定 roadmap/status page
  - 默认 CI 现在不仅有 compile/P2 minimal gate，也有一条 pure Pascal TLS 1.3 focused gate
  - 这让路线图不再只是“写在计划里”，而开始有 required verification surface 对齐
- 不过 focused gate 仍然只覆盖当前 completeness 主线的一个截面：
  - 它守住了 client/server resumption、backend basic、capability cache
  - 还没有推进到 `0-RTT / Early Data`、OCSP stapling、CT、validation hardening
  - 所以下一批最有价值的实现工作仍应回到新的 functional RED，而不是继续扩文档 family

## 2026-03-27 Findings (FreePascal TLS 1.3 0-RTT / early-data primitives)
- 这批最重要的路线图判断是：`0-RTT / Early Data` 不应该继续作为一个“完全未开始”的大黑盒，但也绝对不能被表述成“已完成”。
- 当前树上已经补齐的，是后续真正 early-data path 必须依赖的协议原语层：
  - `TLS_EXTENSION_EARLY_DATA`
  - `TLS_HANDSHAKE_TYPE_END_OF_EARLY_DATA`
  - `NewSessionTicket.max_early_data_size`
  - PSK `ClientHello` 的 `early_data` extension
  - `EndOfEarlyData` build/parse
  - resumable session 对 `max_early_data_size` 的持久化
- 这样做的价值在于把 0-RTT 的下一批 scope 从“又大又模糊的 feature”切成了一个更真实的剩余面：
  - 不是再争论 ticket/parser/builder 有没有基础字段
  - 而是直接进入真正的 early application-data transport / accept-reject state / anti-replay contract
- 这批也澄清了一个兼容性边界：
  - 当前 `NewSessionTicket` parser 继续完整保留原始 extensions block
  - 对 `early_data` 只有在扩展长度正确为 4 字节时才结构化暴露 `max_early_data_size`
  - 这让旧的 raw-fixture contract 不会因为 parser 变“过度聪明”而被误伤
- focused tests 证明当前 builder/parser/state surface 已经一致：
  - post-handshake parser 现在能识别 `max_early_data_size` 和 `EndOfEarlyData`
  - PSK `ClientHello` 在显式允许时会带 `early_data`，同时保持 `pre_shared_key` 仍为最后一个 extension
  - client-side session capture/resume 现在会保留并暴露 ticket 的 `max_early_data_size`
- gate 层也因此要同步调整：
  - `test_tls13_posthandshake.pas` 现在必须算进 FreePascal TLS 1.3 completeness gate
  - 否则 0-RTT primitives 仍会停留在 focused-test-only coverage，而不是 mainline completeness surface
- 路线图结论因此需要更细化地写：
  - `0-RTT / Early Data primitives`：当前 worktree 已具备
  - `real early application-data path`：仍未完成
  - `anti-replay / server-side accept-reject policy`：仍未完成
- 结论：
  - pure Pascal backend 现在已经从“双侧 resumption/PSK 可用”推进到“0-RTT 协议 primitives 已落地”
  - 下一条最有价值的 family 不再是继续补 early-data 常量/metadata，而是直接做真实 early-data transport/state 的 fresh RED

## 2026-03-27 Findings (TLS 1.3 ClientHello PSK extension validation)
- 这次 repo review 暴露的不是“builder 会不会生成合法 `ClientHello`”，而是 parser/helper 是否对非法形状 fail-closed。
- 当前 builder 一直保持正确 contract：
  - `pre_shared_key` 作为最后一个 extension 输出
  - `early_data` 只在 PSK path 上生成，且位于 `pre_shared_key` 之前
- 真正的缺口在 parser 面：
  - `TryParseTLS13ClientHelloFromHandshake(...)` 以前只负责“看到字段就解析”，不会拒绝 `pre_shared_key` 后还有 trailing extension 的非法顺序
  - `TryBuildTLS13ClientHelloPSKBinderTranscript(...)` 在遇到 `pre_shared_key` 后直接 `Break`，因此会忽略其后的 trailing extensions
  - `early_data` 也只校验“必须是空扩展”，不会拒绝“没有 `pre_shared_key` 却带 `early_data`”的非法组合
- 这会导致两个具体风险：
  - parser 会把非法 `ClientHello` 误判为可接受
  - binder transcript helper 与真实 `ClientHello` 后缀字节脱节，无法保证 fail-closed
- fresh RED 证据证明这不是理论担忧：
  - 在合法 PSK `ClientHello` 后手工追加一个未知 extension，当前测试先前会直接通过解析
  - 在普通 non-PSK `ClientHello` 后手工追加 `early_data`，当前 parser 先前也会接受
- 最小正确修法只需要收窄在 `src/fafafa.ssl.tls13.clienthello.parser.pas`：
  - 一旦出现 `pre_shared_key` extension，后面不允许再有任何 extension
  - 解析完成后若 `HasEarlyData=True` 但没有 `pre_shared_key`，直接拒绝
  - binder transcript helper 同样对 `pre_shared_key` trailing extensions fail-closed
- 结论：
  - 这批不是新增功能，而是把现有 `PSK / early_data primitives` 从“builder-only correctness”提升到“parser/helper 也守约”
  - 这样后续再进入真正的 early-data transport/state 实现时，底层 `ClientHello` contract 才不会留下协议绕路口

## 2026-04-19 Findings (FreePascal early-data directory-store public opt-in parity)
- 当前最值钱的缺口不是再发明新的 anti-replay persistence 形态，而是把已经稳定的 directory-backed store 从 backend-private 能力薄接成 public opt-in。
- 最小正确边界因此是：
  - `TSSLConfig` 新增一个 server-only directory path 字段
  - builder 增加对应 fluent method
  - factory 作为 config 应用层负责装配
  - `TFreePascalContext` 只通过一个 backend-private installer seam 把目录路径转成现有 store-backed ledger
- 这里最容易返工的点不是 ledger 本身，而是 public path 冲突语义：
  - `server_early_data_replay_store_file`
  - `server_early_data_replay_store_directory`
  - 如果允许 silent precedence，后面持久化形态继续扩展时会把 builder/factory 行为做脏
  - 所以这批必须 fail fast，并把两个 key 与 `not both` 语义写进 builder/factory error contracts
- 另一条关键收敛是不要为了暴露 directory-store 去改 public `ISSLContext`：
  - 复用现有 FreePascal backend 私有分层更稳
  - 新增独立的 `IFreePascalContextEarlyDataReplayDirectoryInstaller` 就够了
  - 这样不会破坏现有 file-installer seam 的 GUID / 契约
- runtime 证据说明这条 public wiring 不是“只把配置字段存下来”：
  - builder-built context 与 factory-built context 指向同一个 directory store 时，first resumed early-data accept 后，cross-context replay 会被 reject
  - 同时 resumed handshake 继续成功，没有把 anti-replay reject 漂移成会话恢复失败
- 这批完成后，public 层的 replay-store 形态已经形成清晰分工：
  - 默认仍是 in-memory single-process ledger
  - file-backed 与 directory-backed 都是 opt-in
  - capability wording 仍保持 experimental，不会误导成“默认已经持久化”
