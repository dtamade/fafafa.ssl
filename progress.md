# Progress - Interface Design And Backend Implementation Verification

## 2026-05-18

### Context Recovery

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch is `master...origin/master`
    - current worktree started clean before this interface/backend verification batch

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - script produced no recovery output
    - there was no extra unsynced session context to merge before starting the new goal

- `sed -n '1,220p' docs/AGENTS.md`
  - result: PASS
  - summary:
    - repo conventions confirm this batch should keep scope tight, prefer focused verification, and update planning files as part of done criteria

- `sed -n '1,220p' docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - result: PASS
  - summary:
    - previous static audit already identified six design-smell families
    - current batch needs to verify whether those smells still map to live implementation truth across backends

- `sed -n '1,160p' task_plan.md`
  - result: PASS
  - summary:
    - previous plan was still anchored on the older WinSSL capability-truth batch
    - a new plan entrypoint is required to avoid reopening the wrong lane next time

### Interface And Backend Truth Cross-Check

- `rg -n "ISSLConnection = interface|ISSLClientConnection = interface|ISSLServerConnection|SetServerName|TSSLConfig = record|Supports[A-Z][A-Za-z]+: Boolean|[A-Za-z]+Support: TSSLSupportLevel" src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.pas docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md docs/ARCHITECTURE.md docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - confirmed live source still lacks any `ISSLServerConnection` declaration
    - confirmed docs still promise `ISSLServerConnection`
    - confirmed context-level `SetServerName` remains deprecated in base but actively used in factory/builder

- `rg -n "ServerName|SetServerName\\(|GetServerName\\(|CreateConnection\\(" src/fafafa.ssl.factory.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.base.pas src/fafafa.ssl.*connection*.pas tests`
  - result: PASS
  - summary:
    - all major client-capable backends still copy `AContext.GetServerName` into connection state
    - tests also codify context-to-connection `ServerName` fallback as expected behavior

- `rg -n "HandshakeTimeout" src tests`
  - result: PASS
  - summary:
    - request/default factory paths explicitly reject custom `HandshakeTimeout`
    - this is a scoped-design constraint, not a silent no-op

- `rg -n "BufferSize" src tests`
  - result: PASS
  - summary:
    - request/default factory paths explicitly reject custom `BufferSize`
    - `BufferSize` currently remains a public config field mainly for defaults/debug/compatibility surface

- `rg -n "EnableSessionTickets|EnableOCSPStapling" src/fafafa.ssl.factory.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.pas src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas tests/test_factory_logic.pas`
  - result: PASS
  - summary:
    - confirmed `EnableSessionTickets` / `EnableOCSPStapling` are normalized into `Options`
    - this part is compatibility-heavy but still has a live normalization path

- `sed -n '922,1088p' src/fafafa.ssl.openssl.backed.pas`
  - result: PASS
  - summary:
    - OpenSSL `GetCapabilities` still publishes both legacy booleans and v1.2 support-level fields

- `sed -n '1450,1515p' src/fafafa.ssl.freepascal.lib.pas`
  - result: PASS
  - summary:
    - FreePascal backend still marks several legacy booleans as `True` while publishing the corresponding features as `experimental`

- `sed -n '510,575p' src/fafafa.ssl.winssl.lib.pas`
  - result: PASS
  - summary:
    - WinSSL capability source is now internally more truthful after the previous batch, but it still participates in the dual boolean/support-level model

- `sed -n '470,515p' src/fafafa.ssl.mbedtls.lib.pas`
  - result: PASS
  - summary:
    - MbedTLS publishes legacy booleans and support-levels separately, with `OCSPStaplingSupport` locked to `none`

- `sed -n '419,470p' src/fafafa.ssl.wolfssl.lib.pas`
  - result: PASS
  - summary:
    - WolfSSL does the same dual publication, including experimental OCSP/early-data grades

- `sed -n '340,390p' src/fafafa.ssl.backend.selector.pas`
  - result: PASS
  - summary:
    - selector feature matching already trusts support-level fields rather than legacy booleans

- `sed -n '260,305p' src/fafafa.ssl.capability.serializer.pas; sed -n '480,535p' src/fafafa.ssl.capability.serializer.pas; sed -n '665,690p' src/fafafa.ssl.capability.serializer.pas; sed -n '820,868p' src/fafafa.ssl.capability.serializer.pas; sed -n '228,252p' src/fafafa.ssl.capability.diff.pas`
  - result: PASS
  - summary:
    - serializer and diff still round-trip and compare both the legacy boolean surface and the new support-level surface
    - this confirms the dual-truth model is systemic, not a one-file leftover

### Focused Fix And Verification

- update `docs/ARCHITECTURE.md`
  - change:
    - remove nonexistent `ISSLServerConnection` from the active public interface graph
    - clarify that current server-specific capability surfaces mainly live on optional context interfaces

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - remove `ISSLServerConnection` from the active hierarchy
    - restate current truth instead of promising a missing public interface

- add `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - purpose:
    - ensure active docs do not draw `ISSLServerConnection` into the shipped public interface graph while source still lacks the declaration
    - keep the script portable by using `grep`, not a hard `rg` dependency

- `bash -n tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh && bash tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - result: PASS
  - summary:
    - active interface docs no longer promise nonexistent `ISSLServerConnection`

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - custom `HandshakeTimeout` and `BufferSize` are explicitly rejected in factory paths
    - confirms these fields are scope-gated, not silently ignored

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - client-side context `ServerName` remains officially supported as a compatibility path
    - server-side use remains rejected

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - FreePascal / OpenSSL / WolfSSL / MbedTLS all still inherit context-level `ServerName` fallback on dual-context stream paths
    - FreePascal socket path does the same
    - this is now well-proved implementation truth, not just a documentation smell

- `git diff --check`
  - result: PASS

### Residual Context SNI Classification And WinSSL mTLS Skeleton Cleanup

- add `docs/plans/2026-05-18-residual-context-sni-classification-and-mtls-skeleton-cleanup.md`
  - purpose:
    - define the bounded residual classification batch after the first WinSSL client-flow migration cut
    - separate intentional compatibility / API-surface coverage from the last small ordinary handshake path

- add `tests/scripts/test_residual_context_sni_classification_contract.sh`
  - purpose:
    - require explicit `INTENTIONAL_*` labels in the residual ambiguous files
    - fail if `tests/winssl/test_winssl_mtls_skeleton.pas` still uses `Ctx.SetServerName(ServerHost)` in the real handshake path

- `bash -n tests/scripts/test_residual_context_sni_classification_contract.sh && bash tests/scripts/test_residual_context_sni_classification_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `tests/winssl/test_winssl_mtls_skeleton.pas` still lacked explicit `INTENTIONAL_API_SURFACE` classification
    - the residual batch was still real work, not duplicate governance

- update residual classification files:
  - `tests/test_tls_connector_early_data_contract.pas`
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
  - change:
    - add explicit `INTENTIONAL_COMPAT` / `INTENTIONAL_API_SURFACE` markers to the residual ambiguous coverage files
    - move the real `TestMTLSHandshake` flow from context-level `SetServerName(ServerHost)` to per-connection `ISSLClientConnection.SetServerName(ServerHost)`

- `bash -n tests/scripts/test_residual_context_sni_classification_contract.sh && bash tests/scripts/test_residual_context_sni_classification_contract.sh`
  - result: RED -> GREEN
  - summary:
    - residual files are now explicitly classified
    - `test_winssl_mtls_skeleton.pas` no longer uses context-level SNI in the real handshake path

- `mkdir -p tmp/test_tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_early_data_contract -FEtmp/test_tls_connector_early_data_contract -otmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the new `INTENTIONAL_COMPAT` marker only produced the expected deprecated context-level SNI warning at the labeled coverage site

- `mkdir -p tmp/test_mbedtls_context_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_context_contract -FEtmp/test_mbedtls_context_contract -otmp/test_mbedtls_context_contract/test_mbedtls_context_contract tests/mbedtls/test_mbedtls_context_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the labeled context contract still only emits the expected deprecated setter/getter warnings

- `mkdir -p tmp/test_wolfssl_context_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_context_contract -FEtmp/test_wolfssl_context_contract -otmp/test_wolfssl_context_contract/test_wolfssl_context_contract tests/wolfssl/test_wolfssl_context_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the labeled context contract still only emits the expected deprecated setter/getter warnings

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_library_basic.exe tests/winssl/test_winssl_library_basic.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile succeeded after adding the explicit API-surface label

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_mtls_skeleton.exe tests/winssl/test_winssl_mtls_skeleton.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile succeeded after migrating the real handshake path to per-connection SNI
    - the remaining context-level setter use in the file is now limited to the labeled configuration smoke coverage

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the residual classification cut as Phase E delivered second cut
    - move the next recommended batch from residual classification to behavior-migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated residual-classification closeout section
    - refresh the next-step recommendation so future sessions continue from behavior-migration RED selection

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the residual classification closeout into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the residual classification batch

### BuildServer WithSNI Ignore Behavior Migration

- add `docs/plans/2026-05-18-buildserver-withsni-ignore-behavior-migration.md`
  - purpose:
    - define the first true behavior-migration cut after residual classification closed
    - keep scope bounded to the server-side builder dead-compat path instead of reopening client fallback

- update focused RED tests:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
  - `tests/config/test_config_validation.pas`
  - change:
    - expect `BuildServer.WithSNI(...)` to stop retaining `ServerName` on the built server context
    - expect warning / validation wording to say `BuildServer ignores it and server-side connections ignore it`

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED
  - summary:
    - initial run failed 1 assertion
    - `BuildServer` still retained the deprecated client-only `ServerName` on the built server context

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 2 assertions
    - warning wording still described the old apply/ignore split and did not match the desired runtime truth

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: RED
  - summary:
    - initial run failed 1 assertion
    - validation wording still described server-side ignore semantics without the new `BuildServer ignores it` truth

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - `BuildServer` no longer calls `Result.SetServerName(FServerName)`
    - builder server warning now says `BuildServer ignores it and server-side connections ignore it`
    - `ValidateServer` warning wording now follows the same ignore semantics

- update `docs/reference/API_REFERENCE.md`
  - change:
    - clarify that `BuildClient` applies `WithSNI(...)` with warning, while `BuildServer` warns and ignores it

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED -> GREEN
  - summary:
    - final run finished `6 passed, 0 failed`
    - built server contexts no longer retain the deprecated client-only `ServerName`

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - final run finished `14 passed, 0 failed`
    - builder warning text now matches the actual ignore behavior

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: RED -> GREEN
  - summary:
    - final run finished `53 passed, 0 failed`
    - validation wording is aligned with the new runtime truth

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the first server-side behavior-migration cut
    - move the next recommended batch to client-side behavior-migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated BuildServer dead-compat closeout section
    - refresh the next-step recommendation toward client-side fallback migration

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the first behavior-migration cut into the persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the BuildServer ignore batch

### sslCtxBoth Context SNI Ambiguity Cut

- add `docs/plans/2026-05-18-sslctxboth-context-sni-ambiguity-cut.md`
  - purpose:
    - define the first bounded client-side fallback migration cut
    - keep the scope on `sslCtxBoth` role ambiguity instead of reopening all client fallback paths

- update `tests/test_sslctxboth_client_capability_clarification.pas`
  - change:
    - move the dual-context stream/socket expectations from inherited context fallback to explicit no-fallback semantics
    - keep the `ISSLClientConnection` exposure checks and early-data role-gate checks intact

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_sslctxboth_client_capability_clarification.pas` from the intentional-compat label set
    - this file is no longer expected to preserve legacy inherited fallback

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED
  - summary:
    - initial run failed 5 assertions
    - FreePascal / OpenSSL / WolfSSL / MbedTLS dual-context stream paths and the FreePascal socket path all still inherited `both.example.com`

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional-compat label set stayed stable after removing the `sslCtxBoth` file

- `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
  - result: PASS
  - summary:
    - adjacent roleless-handshake boundary was already green before the shim change
    - this confirmed the intended semantic anchor for the ambiguity cut

- update `src/fafafa.ssl.context.compat.pas`
  - change:
    - `GetContextLevelServerNameCompatibilityValue(...)` now returns empty for `sslCtxBoth`
    - add a short comment tying this to the existing explicit-role handshake rule

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED -> GREEN
  - summary:
    - final run finished `28 passed, 0 failed, 1 skipped`
    - dual-role contexts no longer inherit deprecated context-level `ServerName` fallback

- `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
  - result: PASS
  - summary:
    - roleless-handshake fail-fast behavior remained intact after the ambiguity cut

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional-compat label set stayed green after the `sslCtxBoth` removal

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the `sslCtxBoth` ambiguity cut under the shared-compatibility-shim track
    - move the next recommended batch to `sslCtxClient` behavior migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated `sslCtxBoth` ambiguity-cut closeout section
    - refresh the next-step recommendation toward `sslCtxClient` fallback migration

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the first client-side fallback migration cut into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the `sslCtxBoth` ambiguity cut

### Context ServerName Shared Compatibility Shim

- add `docs/plans/2026-05-18-context-servername-shared-compatibility-shim.md`
  - purpose:
    - define the bounded Phase C batch before code changes
    - keep the next execution order anchored on shared seam extraction instead of broader migration

- add `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - purpose:
    - force a RED on missing shared helper adoption
    - guard both helper presence and backend source migration away from local direct context `GetServerName` reads

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `src/fafafa.ssl.context.compat.pas` did not exist yet
    - shared compatibility seam had not been extracted

- add `src/fafafa.ssl.context.compat.pas`
  - change:
    - introduce `GetContextLevelServerNameCompatibilityValue(...)`
    - centralize client-role gate, deprecated read, and warning suppression in one place

- update backend constructors:
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - replace local direct context `GetServerName` fallback reads with shared helper usage
    - preserve each backend's original side effect path (`SetServerName(...)` vs field assignment)

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED
  - summary:
    - first compile failed because the new helper referenced `ContextTypeSupportsClientConnectionRole` from the wrong unit
    - fixed by importing `fafafa.ssl.connection.base` inside `src/fafafa.ssl.context.compat.pas`

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: PASS
  - summary:
    - shared helper now exists
    - all five backend constructor paths route fallback through the shared seam
    - backend-local direct context `GetServerName` reads are gone from the targeted files

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - cross-backend context-to-connection ServerName fallback remains intact after seam extraction
    - final run finished `28 passed, 0 failed, 1 skipped`

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - factory/client compatibility behavior remains intact after backend shim extraction
    - final run finished `6 passed, 0 failed`

### Builder ServerName Compatibility Warning

- add `docs/plans/2026-05-18-builder-servername-compatibility-warning.md`
  - purpose:
    - define the next bounded builder-surface batch after the shared shim landed
    - keep the repo-level route anchored on runtime compatibility warning alignment instead of broader surface redesign

- add `tests/test_context_builder_server_name_compatibility_warning.pas`
  - purpose:
    - prove builder runtime path still silently applies `WithSNI(...)`
    - lock the exact warning expectations for `BuildClient`, `BuildServer`, and the no-SNI quiet path

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 8 assertions
    - `BuildClient` / `BuildServer` both still silently applied `WithSNI(...)`
    - no runtime warning named `WithSNI`, no compatibility-only phrasing, and no builder callsite evidence existed yet

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - add `LogBuilderContextLevelServerNameCompatibilityWarning(...)`
    - emit runtime warning before `BuildClient` / `BuildServer` apply `FServerName` to the context
    - align validation wording so client/server `WithSNI(...)` warnings follow the same compatibility terminology
    - add a short interface comment marking `WithSNI(...)` as compatibility-only

- update `docs/reference/API_REFERENCE.md`
  - change:
    - extend the `Client SNI Compatibility Note` so it explicitly includes `TSSLContextBuilder.WithSNI(...)`
    - point new code toward `TSSLConnectionBuilder.WithHostname(...)` in addition to the per-connection APIs already documented

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - final run passed all 12 assertions
    - builder runtime path no longer stays silent when `WithSNI(...)` is applied
    - the quiet path without `WithSNI(...)` remains quiet

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - validation warning semantics stayed aligned after the builder wording update
    - final run finished `53 passed, 0 failed`

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - builder client/server compatibility behavior remained intact after adding runtime warnings
    - final run finished `6 passed, 0 failed`

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the builder warning batch

### WinSSL Client Flow SNI Guidance Cleanup

- add `docs/plans/2026-05-18-winssl-client-flow-sni-guidance-cleanup.md`
  - purpose:
    - define a bounded batch over a small set of ordinary WinSSL client-flow tests
    - separate normal client-flow guidance from intentional compatibility/API-surface coverage

- add `tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - purpose:
    - fail if selected WinSSL client-flow tests still teach context-level SNI through local context variables

- `bash -n tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh && bash tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `tests/winssl/test_winssl_error_mapping_online.pas` still used `Ctx.SetServerName('expired.badssl.com')`
    - the selected WinSSL client-flow files were still carrying deprecated context-level SNI guidance

- update selected WinSSL client-flow tests:
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - change:
    - replace local context-level `SetServerName(...)` with per-connection `ISSLClientConnection.SetServerName(...)`
    - preserve existing protocol/verification/handshake assertions

- `bash -n tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh && bash tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - the selected WinSSL client-flow tests no longer use context-level SNI guidance

- `mkdir -p tmp/test_winssl_https_client && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_winssl_https_client -FEtmp/test_winssl_https_client -otmp/test_winssl_https_client/test_winssl_https_client tests/winssl/test_winssl_https_client.pas`
  - result: EXPECTED PLATFORM FAILURE
  - summary:
    - direct Linux-target compile still fails in `src/fafafa.ssl.winssl.lib.pas` because the WinSSL library depends on the `Windows` unit
    - this confirms the selected files should be verified through Win64 cross-compile or Windows runtime evidence, not native Linux-target compile

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_error_mapping_online.exe tests/winssl/test_winssl_error_mapping_online.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_revocation_online.exe tests/winssl/test_winssl_revocation_online.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_mtls_e2e_local.exe tests/winssl/test_winssl_mtls_e2e_local.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_https_client.exe tests/winssl/test_winssl_https_client.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the WinSSL client-flow cleanup batch

### Context ServerName Compatibility Roadmap Freeze

- `rg -n "SetServerName\\(|GetServerName\\(|WithSNI\\(|ServerName\\b" src tests docs | sed -n '1,320p'`
  - result: PASS
  - summary:
    - mapped the remaining `context-level ServerName` write paths, backend fallback read paths, active docs guidance, and focused tests that still lock compatibility semantics

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the real migration map across factory, builder, connector, five backend constructors, and intentional compatibility tests
    - define the next execution order as builder surface narrowing -> shared compatibility shim -> final surface cleanup
    - include a route-level progress report so future sessions resume from the current main line instead of reopening finished capability work

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - add `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - add `tests/test_sslctxboth_client_capability_clarification.pas`
    - accept the unified `INTENTIONAL_COMPAT:` label across the curated compatibility-locking tests

- update `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - change:
    - align the direct server-context compatibility note to the shared `INTENTIONAL_COMPAT:` label family

- update `tests/test_sslctxboth_client_capability_clarification.pas`
  - change:
    - label the dual-context fallback checks as explicit intentional compatibility coverage

- `bash -n tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the curated context-level SNI compatibility tests are now explicitly labeled, including the newly mapped builder-server and sslCtxBoth fallback regressions

- `git diff --check`
  - result: PASS

### Capability Serialization Truth Projection

- add `tests/test_capability_serialization_truth_projection.pas`
  - purpose:
    - directly assert JSON/XML emitted payload truth instead of relying on deserialize round-trip
    - catch cases where serializer leaks contradictory `supports*` and `*Support` fields

- `mkdir -p tmp/test_capability_serialization_truth_projection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_serialization_truth_projection -FEtmp/test_capability_serialization_truth_projection -otmp/test_capability_serialization_truth_projection/test_capability_serialization_truth_projection tests/test_capability_serialization_truth_projection.pas && ./tmp/test_capability_serialization_truth_projection/test_capability_serialization_truth_projection`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `CapabilitiesToJSON(...)` still emitted `"supportsSNI": false` while `sniSupport` was already `"stable"`
    - after the fix, JSON/XML serialization now projects legacy boolean output from support-level truth whenever the record already carries v1.2 support-level signals

- update `src/fafafa.ssl.capability.serializer.pas`
  - change:
    - add `HasAnySupportLevelTruth(...)` and `PrepareCapabilitiesForSerialization(...)`
    - normalize a local copy before JSON/XML emission when the record is already support-level-aware
    - keep pure legacy-only in-memory records untouched because serializer still has no presence bits to distinguish default `none` from explicit `none`

- `mkdir -p tmp/test_capability_deserialization_roundtrip && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_roundtrip -FEtmp/test_capability_deserialization_roundtrip -otmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas && ./tmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip`
  - result: PASS
  - summary:
    - existing JSON/XML round-trip compatibility remained green after the serializer projection fix

- `git diff --check`
  - result: PASS

### Capability Runtime Truth Alignment

- `git diff -- src/fafafa.ssl.base.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas`
  - result: PASS
  - summary:
    - confirmed this batch adds one shared normalization helper in `fafafa.ssl.base`
    - confirmed all five live capability sources now normalize legacy boolean truth from the v1.2 support-level fields before caching/returning

- `git diff -- tests/contract/test_capabilities_contract.pas tests/contract/test_backend_contract.pas tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
  - result: PASS
  - summary:
    - confirmed the new source contract guards helper adoption across all major backends
    - confirmed contract assertions now trust `*Support` as runtime truth and also require bool/support-level projection consistency

- `bash -n tests/scripts/test_capability_legacy_bool_normalization_contract.sh && bash tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
  - result: PASS
  - summary:
    - the shared normalization helper is declared in `src/fafafa.ssl.base.pas`
    - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL all invoke `NormalizeLegacyCapabilityBooleans(Result);` in `GetCapabilities`

- `mkdir -p tmp/test_capabilities_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capabilities_contract -FEtmp/test_capabilities_contract -otmp/test_capabilities_contract/test_capabilities_contract tests/contract/test_capabilities_contract.pas && ./tmp/test_capabilities_contract/test_capabilities_contract`
  - result: PASS
  - summary:
    - focused capability contract finished `63 passed, 0 failed, 1 skipped`
    - major backends now pass support-level-first truth checks and all bool/support-level consistency assertions
    - compile emitted only pre-existing repo warning families; no new normalization-related failures appeared

- `mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract finished `111 passed, 0 failed, 24 skipped`
    - optional interface alignment for SNI / CT / OCSP now follows the support-level truth and remains green across available backends
    - Windows Schannel remains intentionally skipped on this Linux host, consistent with the repo's current platform boundary

### Serializer / Deserializer / Diff Truth Alignment

- `mkdir -p tmp/test_capability_deserialization_truth_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_truth_precedence -FEtmp/test_capability_deserialization_truth_precedence -otmp/test_capability_deserialization_truth_precedence/test_capability_deserialization_truth_precedence tests/test_capability_deserialization_truth_precedence.pas && ./tmp/test_capability_deserialization_truth_precedence/test_capability_deserialization_truth_precedence`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `JSONToCapabilities(...)` kept `supportsSNI=true` even when `sniSupport="none"` was present in the same payload
    - after the fix, JSON/XML deserialization now lets v1.2 `*Support` fields override conflicting legacy boolean inputs while preserving legacy-only input compatibility

- `mkdir -p tmp/test_capability_diff_support_level_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_diff_support_level_truth -FEtmp/test_capability_diff_support_level_truth -otmp/test_capability_diff_support_level_truth/test_capability_diff_support_level_truth tests/test_capability_diff_support_level_truth.pas && ./tmp/test_capability_diff_support_level_truth/test_capability_diff_support_level_truth`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `CompareCapabilities(...)` completely missed `SNISupport` / `EarlyDataSupport` changes when legacy boolean values did not change
    - after the fix, diff now compares support-level truth first and uses legacy boolean only as a compatibility fallback

- `mkdir -p tmp/test_capability_deserialization_roundtrip && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_roundtrip -FEtmp/test_capability_deserialization_roundtrip -otmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas && ./tmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip`
  - result: PASS
  - summary:
    - existing JSON/XML round-trip test remained green after the precedence fix
    - confirms this batch tightened truth precedence without regressing the current serialization/deserialization compatibility path

### Internal Context ServerName Warning Quarantine

- `mkdir -p tmp/internal_context_servername_warning_probe && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/internal_context_servername_warning_probe -FEtmp/internal_context_servername_warning_probe -otmp/internal_context_servername_warning_probe/test_capabilities_contract tests/contract/test_capabilities_contract.pas 2>&1 | tee tmp/internal_context_servername_warning_probe/compile.log`
  - result: RED
  - summary:
    - live compile probe emitted deprecated `ISSLContext.GetServerName` warnings from `src/fafafa.ssl.wolfssl.connection.pas` and `src/fafafa.ssl.mbedtls.connection.pas`
    - this confirmed the old `test_builder_integration`-based warning contract had drifted away from the current noise source

- update `tests/scripts/test_internal_context_servername_warning_contract.sh`
  - change:
    - switch the compile probe from `tests/test_builder_integration.pas` to `tests/contract/test_capabilities_contract.pas`
    - check that `wolfssl.connection` / `mbedtls.connection` no longer emit deprecated `GetServerName` warnings
    - add a static `WinSSL` source guard by requiring local warning quarantine markers in `src/fafafa.ssl.winssl.connection.pas`
    - run the compiled `test_capabilities_contract` binary as part of the contract

- update `src/fafafa.ssl.wolfssl.connection.pas`
  - change:
    - add local deprecated-warning quarantine around the two constructor fallback reads of `AContext.GetServerName`

- update `src/fafafa.ssl.mbedtls.connection.pas`
  - change:
    - add local deprecated-warning quarantine around the internal SNI fallback read from `FContext.GetServerName`

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - add local deprecated-warning quarantine around both constructor fallback reads of `AContext.GetServerName`

- `bash -n tests/scripts/test_internal_context_servername_warning_contract.sh`
  - result: PASS

- `bash tests/scripts/test_internal_context_servername_warning_contract.sh`
  - result: GREEN
  - summary:
    - internal warning contract passed after the local quarantines landed
    - the compiled `test_capabilities_contract` binary still executed successfully inside the contract

- `rg -n "deprecated" tmp/internal_context_servername_warning_contract/build.log`
  - result: PASS
  - summary:
    - no remaining deprecated-warning matches were left in the focused compile log after the quarantine change

### Context Builder ServerName Compatibility Marker

- resumed compile/run session `74931` for `tests/config/test_context_builder_server_name_compat_marker.pas`
  - result: RED
  - summary:
    - initial run failed 5 assertions
    - builder export lacked any explicit compatibility marker for `server_name`
    - legacy JSON import failure also exposed a brittle substring-style assertion against pretty-printed JSON

- add `tests/config/test_context_builder_server_name_compat_marker.pas`
  - purpose:
    - lock builder JSON/INI export behavior so `server_name` remains backward compatible but is visibly marked as deprecated context-level SNI compatibility
    - ensure legacy JSON/INI payloads with bare `server_name` still import and re-export with the new marker

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - add `CONTEXT_SERVER_NAME_COMPAT_MODE = 'deprecated_context_sni'`
    - emit `server_name_mode` in JSON/INI export whenever `server_name` is non-empty
    - explicitly accept/ignore `server_name_mode` during JSON/INI import so compatibility metadata does not affect runtime state

- update `tests/config/test_context_builder_server_name_compat_marker.pas`
  - change:
    - parse JSON for the legacy-import assertions instead of substring-matching formatted output
    - keep the INI assertions string-based because INI export is line-oriented and stable

- `mkdir -p tmp/test_context_builder_server_name_compat_marker && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compat_marker -FEtmp/test_context_builder_server_name_compat_marker -otmp/test_context_builder_server_name_compat_marker/test_context_builder_server_name_compat_marker tests/config/test_context_builder_server_name_compat_marker.pas && ./tmp/test_context_builder_server_name_compat_marker/test_context_builder_server_name_compat_marker`
  - result: RED -> GREEN
  - summary:
    - all 8 assertions passed after the builder export/import compatibility marker patch
    - compile emitted only pre-existing repo warning families

- `mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export`
  - result: PASS
  - summary:
    - focused config import/export suite finished `96 passed, 0 failed`
    - the new `server_name_mode` field did not break existing JSON/INI round-trip coverage

- `mkdir -p tmp/test_context_builder_merge_advanced_option_snapshot_semantics && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_merge_advanced_option_snapshot_semantics -FEtmp/test_context_builder_merge_advanced_option_snapshot_semantics -otmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && ./tmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics`
  - result: PASS
  - summary:
    - merge snapshot semantics stayed green (`13 passed, 0 failed`)
    - additive compatibility metadata did not alter empty-field or option-clearing behavior

- `mkdir -p tmp/test_config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_snapshot_clone -FEtmp/test_config_snapshot_clone -otmp/test_config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone/test_config_snapshot_clone`
  - result: PASS
  - summary:
    - clone/reset/merge suite stayed green (`57 passed, 0 failed`)
    - builder snapshots continue to round-trip after the compatibility marker addition

- update `docs/plans/2026-05-18-context-builder-servername-compatibility-marker.md`
  - change:
    - record the bounded Phase B first-cut plan, touched files, command sequence, and expected outputs for the builder compatibility marker batch

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark Phase B builder surface first cut as delivered
    - move the next recommended batch to `factory/config surface narrowing`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated builder-surface compatibility-marker closeout section
    - refresh the "next batch" recommendation so future sessions do not restart from discovery

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new builder-surface result into the persistent repo-level working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained at batch closeout

### Factory Config ServerName Compatibility Warning

- add `tests/test_factory_server_name_compatibility_warning.pas`
  - purpose:
    - lock the second Phase B cut so factory/client `TSSLConfig.ServerName` compatibility no longer stays silent
    - prove both default-config and one-shot factory paths emit an explicit deprecation warning while preserving current behavior

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 8 assertions
    - both factory client paths still silently applied `TSSLConfig.ServerName`
    - no warning named `TSSLConfig.ServerName`, no compatibility-only phrasing, and no explicit callsite evidence existed yet

- update `src/fafafa.ssl.factory.pas`
  - change:
    - add `LogContextLevelServerNameCompatibilityWarning(...)`
    - emit `TSecurityLog.Warning('Factory', ...)` right before client-side compatibility writes in both `CreateContext` overloads
    - message explicitly names `TSSLConfig.ServerName`, marks it as deprecated context-level SNI compatibility, and points callers at `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`

- update `src/fafafa.ssl.base.pas`
  - change:
    - mark `TSSLConfig.ServerName` field comment as deprecated compatibility-only context-level SNI

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `Client SNI Compatibility Note`
    - document that factory still applies `TSSLConfig.ServerName` only for compatibility and now emits a warning

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - all 12 assertions passed after the warning patch
    - default-config client path and one-shot config path now both emit the expected compatibility warning
    - client config without `ServerName` remains quiet

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - client default-config and one-shot `ServerName` compatibility behavior remains intact
    - server-side rejection behavior remains intact

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: PASS
  - summary:
    - one-shot `ServerName` still does not leak into shared defaults
    - explicit default-config compatibility inheritance remains intact

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - request config still rejects `LogLevel` / `LogCallback`
    - library default logging round-trip and dispatch behavior stayed green after the new factory warning hook

- `bash tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs still do not teach deprecated context-level SNI as the recommended path

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the factory/config warning batch

- `git diff --check`
  - result: PASS
