# Findings

# 2026-05-24 MbedTLS OCSP Capability Doc Truth Resync
- `tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh` 之前的 false-red 来自过宽的负向检查：它把任何 `sslCertVerifyCheckOCSP` 词面都当成在线 OCSP 能力发布。
- `src/fafafa.ssl.mbedtls.certificate.pas` 里的实际行为是 fail-closed：遇到 OCSP / CRL flags 时直接返回 “Certificate revocation verification is unavailable”。
- 这次修复把 contract 收窄成两件事：保留 fail-closed VerifyEx 拒绝路径，并禁止在线 OCSP helper 的实际发布。
- 当前 MbedTLS capability 事实仍然是：online OCSP 和 OCSP stapling 都不发布。

## Current Session
- Current audit found one real false-red in `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`: the script still expected the pre-`GetContext` phrase `连接信息 / ALPN / 状态字符串`, while `docs/reference/API_REFERENCE.md` already carried the expanded `连接信息 / 上下文引用 / ALPN / 状态字符串` owner-family guidance.
- The fix was a contract expectation resync, not another API doc wording change and not a runtime change.
- The remaining unclosed-looking `ISSLConnection` / `ISSLConnectionInfo` plans now have verified `Execution Result` sections.
- `tests/contract/test_backend_contract.pas` compiled and ran through `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc`; result was 135 total, 111 passed, 0 failed, 24 skipped.
- `ISSLConnectionInfo` is now closed across migration map, active guidance, source classification, owner-primacy, residual classification, and backend contract wording.
- The previous `ISSLConnectionInfo` owner-primacy batch is complete and the worktree is clean.
- The next high-value architecture batch is `TSSLConfig`, with likely focus on `TSSLLibraryDefaults` adoption and any remaining public-surface scope drift around logging, buffer size, handshake timeout, server name, and option-bridge booleans.
- Before editing, we should recheck the live contracts and docs that already name `TSSLConfig` so we can keep the next change narrow and evidence-backed.
- The repo already has dedicated `TSSLConfig` plan and contract files, including the roadmap, library-defaults adoption, scope-bucket truth, logging truth, migration-targets, and option-bridge truth checks.
- `TSSLLibraryDefaults` adoption looks already completed in the plan docs, so the remaining value is likely in verifying the migration-target docs/contracts and checking for any small drift instead of inventing new structure.
- Current contract sweep: migration targets, logging surface, scope buckets, option-bridge surface/default/precedence, `ServerName`, and active guidance are all passing; the only immediate snag was a missing timeout contract script name.
- After checking the real timeout-related contracts, the current `TSSLConfig` slice is already at closeout truth; no source or docs edits were needed beyond keeping the session records current.
- The `ISSLConnection` convenience-surface classification contract is also already aligned with shipped truth; the pass only needed a plan/records closeout, not code changes.
- `ISSLConnectionControl` owner-path adoption is likewise already aligned with shipped truth; the contract passed and only the plan records needed closing.
- `ISSLConnection` text owner-path adoption is also already aligned with shipped truth; the contract passed and only the plan records needed closing.
- `GetContext` active guidance had one real doc drift; the one-line API reference note fix made the focused contract pass again.
- `GetContext` compiler deprecation and source/class split contracts are also green; they only needed closeout markers, not code changes.
- `GetStateString` and `GetSelectedALPNProtocol` residual contracts are also green; these are now just plan closeouts, not code changes.

## Conclusions
- `ISSLConnection` 当前 shipped surface 可以稳定冻结成一张 41-method taxonomy：`17` core、`6` convenience mirrors、`18` compatibility-core mirrors。
- `ReadString` / `WriteString` 和 timeout / blocking 已经是明确的 owner-mapped convenience buckets，分别对应 `ISSLConnectionTextIO` 和 `ISSLConnectionControl`。
- 其余 18 个 compatibility-core mirrors 现在可以按 `ISSLConnectionInfo`、`ISSLDiagnostics`、`ISSLSessionResumption`、`ISSLCertificateVerification`、`ISSLOCSPStapling` 五个 owner family 来读。
- `INTERFACE_DESIGN_V2` 是承载这张 current-shipped taxonomy 的正确位置，因为它既能保留 v2 目标 core，又能不遮蔽当前 source truth。
- taxonomy 这批做完后，下一条更自然的 implementation batch 很可能就是剩余 `ISSLConnectionInfo` family 的进一步收口。
- `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md` 已经把下一条推荐批次前移到 remaining `ISSLConnectionInfo` family，而不再把 taxonomy 当作未完成事项。
- `ISSLConnectionInfo` family 在 backend contract 中原本只剩半条 owner-first 语义：`GetConnectionInfo` / `GetContext` 已完成，但 `GetSelectedALPNProtocol` / `GetStateString` 仍残留 core-first wording；这一处现在也已补齐。
- 验证顺手暴露出 `tests/test_freepascal_client_session_resumption.pas` 与 `tests/test_freepascal_server_accept_skeleton.pas` 又把 negotiated ALPN 读回了 direct core getter；按既有计划 truth，这两处都应该优先通过 `ISSLConnectionInfo.GetSelectedALPNProtocol` 读取，现已对齐并恢复 `GetSelectedALPNProtocol` 的 4-hit residual allowlist。
- The repo now has an explicit excellence-level architecture anchor: `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md` is the new top-level place to reason about north star, principles, and evolution order.
- The highest-value unfinished design debt remains the same three families, but they now sit inside a clearer global route:
  - `ISSLConnection` core-too-fat / owner taxonomy
  - `TSSLConfig` mixed-scope public model
  - facade historical-path simplification
- The next implementation batch should not reopen closed early-data / OCSP / CT / connection-scope families without fresh RED; it should first build a whole-surface taxonomy for `ISSLConnection`.

## Notes
- `src/fafafa.ssl.base.pas` now makes the current source truth easy to split without ambiguity; the current whole-surface partition is a good candidate for shell-contract guarding.
- The current work should stay doc- and contract-focused; runtime signature churn would only re-open a family that is already stable enough to classify cleanly.
- A useful pattern emerged in this batch: when a residual allowlist contract fails, prefer checking whether an ordinary proof regressed back to the core mirror before broadening the allowlist.
- The key strategic decision in this batch is to treat `ReadString` / `WriteString` and timeout/blocking as explicit `v1.x` convenience mirrors, not as owner-less clutter and not as the immediate first removal target.
- Another key decision is to delay `ISSLServerConnection` symmetry work until after connection-core clarity and config-scope clarity are stronger; fake symmetry would make the public model worse, not better.
