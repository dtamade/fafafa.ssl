# fafafa.ssl 当前路线图

> **更新**: 2026-04-19
> **用途**: 这是当前对外稳定入口，用来说明“现在项目处于什么状态、接下来最该做什么、从哪里开始验证”。

---

## 当前状态

- engineering_state: CLOSED_OUT_PENDING_APPROVAL
- approval_gate: human decision required before reopening Wave C governance work
- product_mainline: `SSL/TLS backend completeness roadmap`
- current_default_build: `python3 scripts/compile_all_modules.py`
- current_default_gate: `bash scripts/run_minimal_ci_gate.sh --fast-local`
- current_focused_gate: `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- focused_ci_status: `.github/workflows/ci.yml` 已自动运行 `minimal-gate-linux` 与 `freepascal-tls13-completeness`
- freepascal_remaining_capability_caveat: `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

这几个状态需要分开理解：

- `Wave C` 现在主要是治理与审批状态页，不再代表当前产品功能主线。
- 当前真正还在推进的实现主线，是 pure Pascal backend 的 SSL/TLS completeness。
- `KnownIssues` 已不再把 OCSP / CT / validation / resumption 写成剩余 gap；这些线只有 fresh RED 才应重开。

---

## 先看哪里

如果你想快速判断项目现在在哪个阶段，按这个顺序看：

1. [Wave C Closeout Status](test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md)
2. [Wave C Current Chain](test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md)
3. [SSL/TLS backend completeness roadmap](plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md)
4. [GitHub Actions README](../.github/README.md)

如果你只是想开始开发或验证：

Run:

```bash
python3 scripts/compile_all_modules.py
```

Then:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
```

如果你在看 pure Pascal TLS 1.3 主线：

Run:

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local
```

---

## 当前实现主线

当前产品主线不是“继续收口 Wave C”，而是让 pure Pascal backend 的能力声明、实现状态和默认入口持续保持同一套真相源。

已完成到这里：

- TLS 1.3 modern cipher-suite parity
- client-side session resumption / PSK
- server-side session resumption / PSK
- 0-RTT / early-data public transport、policy、anti-replay hardening、config parity、public ergonomics 与 connector convenience
- OCSP / Certificate Transparency / validation closeout
- FreePascal server-side OCSP stapling public closeout

当前这一批已经收口到这里：

1. FreePascal early-data 的 default shipped path 仍然是 in-memory single-process anti-replay ledger；默认 ledger 现在已经收敛到底层 replay-store seam，而不是继续维护一套独立实现。
2. file-backed replay-store opt-in、builder opt-in、`TSSLConfig` / `TSSLFactory` replay-store parity、public-path runtime restart durability、mixed builder/factory shared replay truth、mixed public-path cross-process restart durability、installer-parent -> builder/factory-child mixed cross-process convergence、runtime file-store fail-closed/recovery/isolation、same-file path-representation convergence、cross-process different-store boundary isolation、filesystem failure-shape fail-closed/recovery、以及 `SaveEntries(...)` temp-path preserves-existing-truth / canonical main-path rename-boundary contracts、existing-main replace fallback truth preservation、backup restore failure recovery、`.bak` residue semantics、deterministic temp write-open denied、deterministic backup-promotion rename denied、`.bak` fallback corruption fail-closed、以及 `.bak` 合法头部 + 合法 entry + trailing garbage residue` 这条最后的轻量 corruption sidecar 都已经补齐；现在不仅公共路径能消费 installer-parent materialized 的 replay truth，真实 installer runtime path 也已经直接锁住 corrupt main store、corrupt orphan temp fail-closed、orphan temp recovery、orphan lock ignore、store-boundary/path-swap isolation、同一物理 file 的 relative/absolute alias convergence、不同 replay-store file 在跨进程场景下继续形成独立 truth boundary、`.lock` directory blocker、`.tmp` directory blocker、parent-path file blocker 在 direct provider 与 installer runtime path 上都 fail closed，blocker 移除后同一 session 重新 accept 并 materialize canonical replay truth；同时 fresh RED 还暴露并修复了三个 provider 内部 durability drift：canonical main-path rename-boundary fail-closed 时遗留的 `<store>.tmp` 现在也会被清理；existing-main replace fallback 上新增了 backup-assisted replace；而在 `.bak -> main` restore 也失败时，readable resolution 现在会在 `main` 缺失且 orphan `.tmp` 不存在时，把 `.bak` 作为 restore-failure-only 的 bounded fallback truth source，保证旧 replay truth 仍可被消费直到后续正常写回恢复 canonical main；并且 success-path `.bak` cleanup failure / stale `.bak` undeletable on next save 的 residue semantics、deterministic temp-write-open denied / backup-promotion denied 这组 permission/write-failure shapes、以及 `main` 缺失 + `.tmp` 不存在时读取 corrupt `.bak` fallback 或“合法单 entry 后尾随垃圾字节”的 `.bak` fallback 继续 fail-closed 现在也已经有 fresh direct/runtime evidence；但这仍然是 opt-in seam，不代表默认 durability / distributed readiness。
3. 第二个 backend-private 单机持久化 concrete store 不只 prototype 已落地，第一批 durability hardening 也已经补齐：`TFreePascalDirectoryEarlyDataReplayStore` 现在通过现有 `IFreePascalEarlyDataReplayStore` / `TFreePascalStoreBackedEarlyDataReplayProvider` / `InstallStoreBackedReplayLedger(...)` 接线，已经直接锁住 directory-backed rebuild durability、expired-entry prune、runtime replay reject、corrupt `.entry` fail-closed / cleanup recovery、cross-process lock fail-closed、orphan lock ignore、`main > .tmpdir > .bakdir` 这条 bounded fallback recovery、`.tmpdir` residue 在 repeated replay reject / replay-only restart 下不会被误消费、`.tmpdir` / `.bakdir` fallback 在 `invalid_version` / `trailing_garbage` 两类 corruption 上继续 fail-closed、regular-file canonical `main` / `.tmpdir` / `.bakdir` blocker 在 direct/runtime first acquire 上 fail-closed、以及 `.tmpdir` / `.bakdir` regular-file blocker 在 update path 上 preserve existing replay truth；同时，`main` 缺失且 `corrupt .tmpdir + healthy .bakdir` 同时存在的双 fallback 冲突态现在也已经有 direct/runtime focused evidence：系统会继续 fail closed，不会 silent fallback 到 `.bakdir` 自愈，只有坏 `.tmpdir` 被显式移除后，健康 `.bakdir` replay truth 才恢复并重新 materialize canonical `main`。accepted update-path 的 runtime crash-window restart truth 现在也已经补上 focused evidence：existing replay truth 先 materialize 后，child accept blocked session 并立刻 simulated crash，restart 后 original / just-accepted replay truth 都继续 reject；directory-store `.bakdir` cleanup delete failure residue / stale `.bakdir` undeletable next-save fail-closed 现在也已经有 direct/runtime focused evidence；而更深一层的 update-path write-interruption / rename-denial 家族，`tempdir -> main` failure with restore success / restore failure、deterministic `tempdir -> main` denied、以及 deterministic `main -> .bakdir` denied，也已经有 direct/runtime focused evidence。上一批补的 `RenamePathAt(...)` seam 已经足够表达这组 rename-denial 合同，因此当前批次没有新的生产改动；整个 directory-store line 仍然不需要改 public API、builder / factory / config surface、`TFreePascalContext` 或 `TFreePascalConnection`。
4. managed seam boundary 与 runtime parity evidence 已经补齐：shared in-memory managed lifecycle truth 已锁住，callback / file-backed / directory-backed non-managed providers 不会因为 local enabled / capacity toggle 被隐式 wipe；public-installed file-backed ledger 也继续保持 expired-entry prune 语义。
5. focused gate、compile gate、builder / factory error contracts、runtime parity closeout、mixed public-path durability closeout、mixed public-path cross-process durability closeout、installer-to-public mixed cross-process convergence closeout、runtime file-store fail-closed/recovery/isolation closeout、store-path identity / cross-process boundary closeout、`SaveEntries(...)` boundaries closeout、backup restore failure recovery closeout、`.bak` residue semantics closeout、`.bak` fallback corruption closeout、deterministic permission/write-failure shapes closeout、directory-store durability hardening closeout、directory-store `.tmpdir` residue semantics closeout、directory-store filesystem blocker semantics closeout、directory-store blocker edge closeout、directory-store dual-fallback conflict closeout、directory-store runtime crash-update restart closeout、directory-store `.bakdir` residue semantics closeout、directory-store backup-assisted replace / backup restore failure closeout、以及 directory-store deterministic rename-denial closeout 都已经有 fresh evidence。

下一条最值得开的实现线：

1. 如果还要继续扩 early-data，不再建议无 fresh RED 地继续开 directory-store family；当前 blocker queue 已经收口，剩余只有更深的 crash-window / write-interruption drift 值得在 fresh failing evidence 出现时再重开，而不是现在回头重开现有 file-backed `.bak` family、managed boundary 或现有 parity 接线。
2. capability 等级继续保持 `experimental`，直到默认 shipped path 不再局限于 in-memory single-process ledger。
3. OCSP / CT / validation 只在 fresh failing evidence 出现时重开，不再作为默认 future queue。

相关计划：

- [Backend completeness roadmap](plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md)
- [2026-04-12 root roadmap truth alignment and early-data next wave](plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md)
- [2026-04-13 FreePascal early-data builder opt-in](plans/2026-04-13-freepascal-early-data-antireplay-builder-optin.md)
- [2026-04-13 FreePascal early-data replay-store factory parity and error contracts](plans/2026-04-13-freepascal-early-data-replay-store-factory-parity-and-error-contracts.md)
- [2026-04-14 FreePascal early-data mixed public-path cross-process durability](plans/2026-04-14-freepascal-early-data-mixed-public-path-cross-process-durability.md)
- [2026-04-14 FreePascal early-data installer-to-public mixed cross-process convergence](plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md)
- [2026-04-14 FreePascal early-data runtime file-store fail-closed recovery and isolation](plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md)
- [2026-04-15 FreePascal early-data store-path identity and cross-process boundary](plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md)
- [2026-04-15 FreePascal early-data filesystem failure shapes fail-closed and recovery](plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md)
- [2026-04-15 FreePascal early-data SaveEntries boundaries](plans/2026-04-15-freepascal-early-data-saveentries-boundaries.md)
- [2026-04-15 FreePascal early-data existing-main replace truth preservation](plans/2026-04-15-freepascal-early-data-existing-main-replace-truth-preservation.md)
- [2026-04-15 FreePascal early-data backup restore failure recovery](plans/2026-04-15-freepascal-early-data-backup-restore-failure-recovery.md)
- [2026-04-15 FreePascal early-data `.bak` residue semantics](plans/2026-04-15-freepascal-early-data-bak-residue-semantics.md)
- [2026-04-16 FreePascal early-data `.bak` fallback corruption hardening](plans/2026-04-16-freepascal-early-data-bak-fallback-corruption-hardening.md)
- [2026-04-16 FreePascal early-data `.bak` trailing-garbage fallback closeout](plans/2026-04-16-freepascal-early-data-bak-trailing-garbage-fallback-closeout.md)
- [2026-04-16 FreePascal early-data directory store prototype](plans/2026-04-16-freepascal-early-data-directory-store-prototype.md)
- [2026-04-16 FreePascal early-data directory store durability hardening](plans/2026-04-16-freepascal-early-data-directory-store-durability-hardening.md)
- [2026-04-16 FreePascal early-data directory store crash-window tempdir residue](plans/2026-04-16-freepascal-early-data-directory-store-crash-window-tempdir-residue.md)
- [2026-04-16 FreePascal early-data directory store fallback corruption hardening](plans/2026-04-16-freepascal-early-data-directory-store-fallback-corruption-hardening.md)
- [2026-04-16 FreePascal early-data directory store filesystem blocker semantics](plans/2026-04-16-freepascal-early-data-directory-store-filesystem-blocker-semantics.md)
- [2026-04-16 FreePascal early-data directory store blocker edge closeout](plans/2026-04-16-freepascal-early-data-directory-store-blocker-edge-closeout.md)
- [2026-04-16 FreePascal early-data directory store dual fallback conflict](plans/2026-04-16-freepascal-early-data-directory-store-dual-fallback-conflict.md)
- [2026-04-16 FreePascal early-data directory store runtime crash-update restart](plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md)
- [2026-04-19 FreePascal early-data directory store `.bakdir` residue semantics](plans/2026-04-19-freepascal-early-data-directory-store-bakdir-residue-semantics.md)
- [2026-04-19 FreePascal early-data directory store backup-assisted replace and restore failure](plans/2026-04-19-freepascal-early-data-directory-store-backup-assisted-replace-and-restore-failure.md)
- [2026-04-19 FreePascal early-data directory store deterministic rename-denial](plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md)
- [2026-04-16 FreePascal early-data permission / write-failure shapes](plans/2026-04-16-freepascal-early-data-permission-write-failure-shapes.md)
- [FreePascal validation next-wave roadmap closeout](plans/2026-04-11-freepascal-validation-next-wave-roadmap.md)
- [FreePascal server-side OCSP stapling public closeout](plans/2026-04-11-freepascal-server-side-ocsp-stapling-public-closeout.md)

---

## 这页不是什么

这页不是：

- 历史周报的替代品
- 每个 family-sized implementation batch 的详细执行记录
- 对所有功能都已“production complete”的承诺

详细历史证据继续保留在 `docs/test_reports/` 和 `docs/plans/`。这页只负责给出稳定入口、当前真相和下一条最值得开的实现线。
