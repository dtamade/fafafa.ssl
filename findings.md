# Findings

# 2026-05-25 TLS13 ServerCertVerify Range-Check Cleanup
- `tests/test_tls13_servercertverify.pas` hit a concentrated
  `Warning: Range check error while evaluating constants` batch around the
  `TestBigIntQWordVectorSuiteWaveD` and `TestBigIntQWordVectorSuiteWaveF`
  QWord vector suites.
- The warnings came from untyped 64-bit hex literals being evaluated as signed
  constants before reaching `QWord`-typed helper parameters.
- Wrapping those literals in explicit `QWord(...)` casts kept the same test
  values and semantics while removing the range-check warning family.
- Focused compile for `tests/test_tls13_servercertverify.pas` is clean for the
  target warning family.
- The target test binary passed.
- The full TLS 1.3 completeness gate passed with `18` passed and `0` failed.
- Residual gate warnings are separate families and should be handled in later
  waves, not folded back into this batch.

# 2026-05-24 Module Test Unreachable-Code Warning Cleanup
- The broad module-test `Unreachable code` warnings were caused by direct
  compile-time constant comparisons in test assertion branches, not by dead
  runtime code.
- Wrapping those constant operands in local `RuntimeInteger` helpers preserves
  the expected-value checks while preventing FPC from folding the failure branch
  away at compile time.
- Focused compile for the 9 affected module tests found no remaining
  `Warning: Unreachable code`.
- `run_all_module_tests.sh --fast-local` passed with `22` passed, `0` failed,
  and `0` skipped.
- The new broad module-test compile logs contain no `Warning:` entries.

# 2026-05-24 Managed Result Init Safety Post-Wave6 Residual Discovery
- Post-wave6 broad module tests passed: `run_all_module_tests.sh --fast-local`
  reported `22` passed, `0` failed, and `0` skipped.
- Broad module-test compile-log grep found no
  `Warning: Function result variable of a managed type does not seem to be initialized`.
- Post-wave6 source compile remained clean: `compile_all_modules.py --rebuild`
  compiled `186/186` source modules with `0` warnings.
- No managed-result wave7 is justified on current evidence.
- The next warning-family candidate should be tracked separately; the broad
  module logs expose test `Unreachable code` warnings.

# 2026-05-24 Managed Result Init Safety Wave 6
- Residual discovery showed source modules were already clean for this warning
  family: `compile_all_modules.py --rebuild` compiled `186/186` source modules
  with `0` warnings.
- The remaining managed-result warnings came from the TLS 1.3 completeness test
  harness, not production units.
- Replacing empty `TBytes` result initialization with `Result := nil` preserves
  helper semantics while avoiding FreePascal's managed-result initialization
  warning.
- Helpers that resize `Result` directly now initialize it first where the gate
  exposed the same warning class.
- Full TLS 1.3 completeness gate passed with `18` tests passed and `0` failed.
- The wave6 gate log contains no
  `Warning: Function result variable of a managed type does not seem to be initialized`.
- Remaining gate warnings are different families and should be handled in
  separately named follow-up batches, not folded into wave6.

# 2026-05-24 Managed Result Init Safety Wave 5
- Wave5 production and test targets were already type-safe on current head:
  `TLS13ComputeResumptionMasterSecretFromTranscriptHash`,
  `TLS13DeriveResumptionPSKFromTranscriptHash`,
  `HashTranscriptForSuite`, `HKDFExtractForSuite`,
  `HKDFExpandLabelForSuite`, `BuildExtensionHeader`,
  `BuildTLS13ServerHelloBody`, `BuildTLS13ServerHelloHandshake`,
  `BuildTLS13ServerHelloHandshakeWithSelectedPSK`, and `HexToBytes` all passed
  the focused managed-result contract.
- Focused compile/run passed for `tests/test_tls13_appschedule.pas`,
  `tests/test_tls13_serverhello_builder.pas`, and
  `tests/test_tls13_resumption.pas`.
- The wave5 compile logs rebuilt `tls13.appschedule`, `tls13.serverhello`, and
  `tests/test_tls13_resumption.pas` and contain no
  `Warning: Function result variable of a managed type`.
- No production edits were needed for this batch.
- There is no `wave6` plan or contract on current head; the next safe step is
  a residual-discovery pass before creating any follow-up wave.

# 2026-05-24 Managed Result Init Safety Wave 4
- Wave4 production targets were already type-safe on current head:
  `HashTranscriptForSuite`, `HKDFExtractForSuite`,
  `HKDFExpandLabelForSuite`, `TLS13ComputePSKBinderForCipherSuite`,
  `BuildExtensionServerName`, `BuildExtensionALPN`,
  `BuildExtensionPreSharedKey`, `BuildTLS13ClientHelloBody`,
  `BuildTLS13ClientHelloBodyWithPSKCore`,
  `BuildTLS13ClientHelloHandshake`,
  `BuildTLS13ClientHelloHandshakeWithPSK`, and
  `BuildTLS13ClientHelloHandshakeWithComputedPSKBinder` all passed the
  focused managed-result contract.
- Focused compile/run passed for both `tests/test_tls13_foundation.pas` and
  `tests/test_tls13_resumption.pas`, rebuilding the `tls13.keyschedule` and
  `tls13.clienthello` units.
- The wave4 compile logs contain no
  `Warning: Function result variable of a managed type`.
- No production edits were needed for this batch; the next planned cleanup is
  wave 5:
  `src/fafafa.ssl.tls13.appschedule.pas`,
  `src/fafafa.ssl.tls13.serverhello.pas`, and
  `tests/test_tls13_resumption.pas`.

# 2026-05-24 Managed Result Init Safety Wave 3
- Wave3 production targets were already type-safe on current head:
  `CopyBytes`, `ConcatBytes`, `BuildTLS13HKDFLabel`,
  `HKDF_Expand_SHA256`, `HKDF_Expand_SHA384`, and
  `TConstantTime.Select` all passed the focused managed-result contract.
- `tests/unit/test_constant_time.pas` failed in `TestTimingConsistency` because
  it measured very short loops with `GetTickCount64`; the average time was near
  zero, so normal scheduler noise inflated the percentage deviation.
- The fix keeps deterministic equal/different compare sanity loops and removes
  low-resolution wall-clock variance as a pass/fail signal.
- Focused compile logs for wave3 no longer contain
  `Warning: Function result variable of a managed type`.
- The next likely managed-result cleanup is wave 4.

# 2026-05-24 Managed Result Init Safety Wave 2
- Wave 2 production functions were already type-safe on current head:
  `BuildTLSPlaintext(...)`, `ReadVector16(...)`, and
  `TFreePascalSession.Serialize(...)` all initialize empty `TBytes` results
  with `Result := nil`.
- The focused `tests/test_freepascal_client_session_resumption.pas` compile
  exposed the live residual warning class in its TLS 1.3 handshake-message
  helper functions.
- Replacing the helper `SetLength(Result, 0)` calls with `Result := nil`
  preserves append/build semantics while removing the managed-result warning
  class from the verification harness.
- The wave2 contract now guards both the production functions and those
  session-resumption helper functions.
- The next likely managed-result cleanup is wave 3:
  `src/fafafa.ssl.tls13.primitives.pas` and
  `src/fafafa.ssl.crypto.constant_time.pas`.

# 2026-05-24 Managed Result Init Safety Harness Extension
- The source contract on `src/fafafa.ssl.pas` and `src/fafafa.ssl.connection.base.pas`
  was already in the type-safe form the batch wanted, so the live warning source
  we had to close was the verification harness.
- `tests/test_connection_builder_hostname_precedence.pas` also used the same
  empty `TBytes` result pattern in `TMockCertificate.SaveToDER` and
  `TMockSession.Serialize`.
- Switching those helpers to `Result := nil` keeps the behavior identical while
  removing the managed-result warning class from the verification harness.
- `tests/scripts/test_managed_result_init_safety_contract.sh` now covers the
  harness helpers in addition to the source-managed-result contract.
- If we continue, the next likely managed-result cleanup is wave 2:
  `src/fafafa.ssl.tls13.wire.pas` and `src/fafafa.ssl.freepascal.session.pas`.

# 2026-05-24 ISSLCertificateVerification Root Residual Campaign Closeout
- `tests/test_openssl_connection_verify_result_contract.pas` was rechecked and remains a backend/core mirror proof.
- `tests/test_wolfssl_framework.pas` was rechecked and remains a backend framework proof with direct core verify-result coverage intentionally preserved.
- The current root-test direct-core verify-result residual subgroup is now fully intentional; the residual campaign is complete.

# 2026-05-24 ISSLCertificateVerification Client Chain Trust Owner Path
- `tests/test_freepascal_client_chain_trust_runtime.pas` 的 trust-status 与 not-verified / OK 断言已迁到本地 helper `GetCertificateVerifyResult` / `GetCertificateVerifyResultString`。
- 两个 helper 都通过 `ISSLCertificateVerification` 读取 owner surface，保留 CA-signed / self-signed / trust-store parity 语义不变。
- 该文件已移出 root residual、broad residual、compiler-deprecated quarantine allowlist；root-test direct-core verify-result residual set 从 3 个文件缩到 2 个文件。
- 目标测试编译运行通过；编译输出仍有 4 个既有 managed result initialization warning，没有引入新的失败。

# 2026-05-24 ISSLCertificateVerification Client Cert Verify Flags Owner Path
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas` 的 mixed numeric/text verify-result 断言已迁到本地 helper `GetCertificateVerifyResult` / `GetCertificateVerifyResultString`。
- 两个 helper 都通过 `ISSLCertificateVerification` 读取 owner surface，保留 hostname / expiry / strict-chain / revocation / CRL fail-closed 语义不变。
- 该文件已移出 root residual、broad residual、compiler-deprecated quarantine allowlist；root-test direct-core verify-result residual set 从 4 个文件缩到 3 个文件。
- 目标测试编译运行通过；编译输出仍有 4 个既有 managed result initialization warning，没有引入新的失败。

# 2026-05-24 ISSLCertificateVerification Server Accept Skeleton Owner Path
- `tests/test_freepascal_server_accept_skeleton.pas` 的 accept-failure verify-result 文本读取已迁到本地 helper `GetCertificateVerifyResultString`。
- helper 通过 `ISSLCertificateVerification.GetVerifyResultString` 读取失败文本，保留 TLS 1.3 server accept skeleton / ALPN / connection-info 断言不变。
- 该文件已移出 root residual、broad residual、compiler-deprecated quarantine allowlist；root-test direct-core verify-result residual set 从 5 个文件缩到 4 个文件。

# 2026-05-24 ISSLCertificateVerification Server Accept Skeleton Planning
- `tests/test_freepascal_server_accept_skeleton.pas` 的 direct `GetVerifyResultString` 只用于 accept 失败后的诊断文本，主要判断 `client finished` / `certificateverify signer` 一类失败原因。
- 该文件的 `GetConnectionInfo` / `GetSelectedALPNProtocol` 相关逻辑是独立 owner surface，不需要一起改。
- 预编辑分类：`owner-migrate`。
- Starting contracts 通过：root residual allowlist、broad residual classification、compiler-deprecated quarantine 当前都仍与 5-file residual truth 对齐。

# 2026-05-24 ISSLCertificateVerification Online OCSP Owner Path
- `tests/test_freepascal_client_online_ocsp_runtime.pas` 的 revoked、signature verification、responder/delegated responder failure 文本断言已迁到本地 helper `GetCertificateVerifyResultString`。
- helper 通过 `ISSLCertificateVerification.GetVerifyResultString` 读取失败文本，保留 online OCSP fail-closed 语义不变。
- 该文件已移出 root residual、broad residual、compiler-deprecated quarantine allowlist；root-test direct-core verify-result residual set 从 6 个文件缩到 5 个文件。

# 2026-05-24 ISSLCertificateVerification Online OCSP Planning
- `tests/test_freepascal_client_online_ocsp_runtime.pas` 的 remaining direct `GetVerifyResultString` 命中只用于 revoked、signature verification、responder/delegated responder 三类 fail-closed 文本断言。
- 这些命中不是 core mirror 等价 proof；下一轮预分类为 `owner-migrate`。
- Starting contracts 通过：root residual allowlist、broad residual classification、compiler-deprecated quarantine 当前都仍与 6-file residual truth 对齐。

# 2026-05-24 ISSLCertificateVerification Campaign Control
- The campaign is now bounded to 6 root-test residual files, not an open-ended sweep.
- Each future round must classify exactly one target as `owner-migrate` or `freeze`, then verify and commit.
- `tests/test_openssl_connection_verify_result_contract.pas` is already likely a real backend/core mirror proof and should not be rewritten unless the public compatibility strategy changes.

# 2026-05-24 ISSLCertificateVerification OCSP Stapling Owner Path
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas` 的 direct verify-result usage 都是 required OCSP stapling fail-closed 文本检查，不是 core mirror 等价 proof。
- 新增本地 helper `GetCertificateVerifyResultString`，通过 `ISSLCertificateVerification.GetVerifyResultString` 读取失败文本，并移除该文件的 deprecated-warning quarantine。
- 目标 OCSP stapling 测试编译运行通过；编译输出仍有该文件既有的 6 个 managed result initialization warning。root-test direct-core verify-result residual set 从 7 个文件缩到 6 个文件。

# 2026-05-24 ISSLCertificateVerification CT/SCT Owner Path
- `tests/test_freepascal_client_ct_sct_surface.pas` 的 verify-result direct core usage 都是 CT/SCT fail-closed 文本检查，不是 core mirror 等价 proof。
- 新增本地 helper `GetCertificateVerifyResultString`，通过 `ISSLCertificateVerification.GetVerifyResultString` 读取失败文本，避免在多个断言处重复 `Supports` 代码。
- 目标 CT/SCT 测试编译运行通过；编译输出仍有该文件既有的 10 个 managed result initialization warning。root-test direct-core verify-result residual set 从 8 个文件缩到 7 个文件。

# 2026-05-24 ISSLCertificateVerification Certificate-Flight Owner Path
- `tests/test_freepascal_client_certificate_flight_requirements.pas` 只有一处 direct `GetVerifyResultString`，用途是断言完整 TLS 1.3 握手缺少 Certificate/CertificateVerify flight 时失败文本包含 `certificate`；这不是 core mirror 等价 proof。
- 该断言已迁到 `ISSLCertificateVerification.GetVerifyResultString`，root-test direct-core verify-result residual set 从 9 个文件缩到 8 个文件。
- 目标测试编译运行通过；编译输出仍有该文件既有的 3 个 managed result initialization warning，没有引入失败。

# 2026-05-24 ISSLCertificateVerification FreePascal Basic Owner Path
- `tests/test_freepascal_backend_basic.pas` 的 TLS 1.2 client/server failure text checks 不是 core mirror 等价 proof；它们只需要读取验证失败文本，因此适合迁到 `ISSLCertificateVerification.GetVerifyResultString`。
- 本批保留 FreePascal backend fail-closed / unsupported 语义不变，只把读取路径从 deprecated `ISSLConnection.GetVerifyResultString` compatibility mirror 切到 owner interface。
- 迁移后该文件不再需要 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释，也不再需要 verify-result deprecated-warning quarantine。
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh` 和 broad residual classification 现在把 root-test direct-core verify-result residual set 从 10 个文件缩到 9 个文件。
- `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh` 的 suppressed file list 同步移除了 `tests/test_freepascal_backend_basic.pas`。

# 2026-05-24 ISSLSessionResumption Residual Slimming
- `tests/test_mbedtls_connection_session_reused_contract.pas` and `tests/test_openssl_connection_session_reused_contract.pas` now prove their session-reuse semantics through `ISSLSessionResumption`.
- The migration exposed an interface/manual-free lifetime issue in both tests; fixing it by letting the interface reference own the connection removed `EInvalidPointer` teardown failures.
- `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh` now treats `tests/contract/test_backend_contract.pas` as the only remaining direct-core session-resumption residual.
- `src/fafafa.ssl.connection.base.pas` now records session-resumption direct-core usage as contract-mirror-only.

# 2026-05-24 ISSLDiagnostics Residual Slimming
- `tests/winssl/test_winssl_monitoring.pas` and the diagnostics blocks in `tests/winssl/test_winssl_connection_edge_cases.pas` now use `ISSLDiagnostics` owner path instead of direct core getters.
- `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh` now treats `tests/contract/test_backend_contract.pas` as the only remaining direct-core diagnostics residual.
- `src/fafafa.ssl.connection.base.pas` now describes diagnostics as contract-mirror-only instead of carrying a stale WinSSL residual note.
- `fpc` compile attempts for the WinSSL test units on this Linux host stop in `fafafa.ssl.winssl.certificate.pas` because the `Windows` unit is unavailable; the diagnostics contract and text checks still passed.

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
