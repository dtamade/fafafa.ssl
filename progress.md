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

### Shared Client Context SNI Fallback Cut

- add `docs/plans/2026-05-18-shared-client-context-sni-fallback-cut.md`
  - purpose:
    - define the bounded cross-backend alignment batch after the FreePascal-only no-inheritance cut
    - keep scope on the shared seam instead of reopening unrelated release or Windows lanes

- add `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - purpose:
    - prove that all currently available client-capable backends preserve deprecated context state on the context object itself
    - but no longer auto-inherit that state into new client connections

- update `src/fafafa.ssl.context.compat.pas`
  - change:
    - keep `GetContextLevelServerNameCompatibilityValue(...)` as the shared control seam
    - stop reading deprecated context-level `GetServerName`
    - return `''` for any non-nil context so shared-helper backends also follow the no-inheritance rule

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED
  - summary:
    - stale source contract still required `src/fafafa.ssl.freepascal.connection.pas` to use the shared helper
    - this contradicted the earlier FreePascal no-inheritance runtime cut and blocked the current batch for the wrong reason

- update `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - change:
    - require the shared helper only in OpenSSL / WolfSSL / MbedTLS / WinSSL
    - fail if FreePascal reintroduces the shared helper
    - fail if the helper itself or any backend source reintroduces direct `(AContext|FContext).GetServerName` fallback reads

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the shared client fallback cut, the stale-contract correction, and the new next-route recommendation into repo working memory

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the shared client fallback cut as delivered
    - move the next recommended batch back to the final direct server-context legacy-state control case

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the shared client fallback cut
    - refresh the route summary so the next session does not reopen the already-closed cross-backend fallback divergence

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED -> GREEN
  - summary:
    - updated source contract now matches current truth
    - shared-helper backends still route through one seam, FreePascal stays off the seam, and direct context getter fallback stays forbidden everywhere

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend contract finished `20 passed, 0 failed, 1 skipped`
    - FreePascal / OpenSSL / WolfSSL / MbedTLS all keep deprecated context state on the context but no longer inherit it into new client connections
    - WinSSL stayed source-covered and runtime-skipped on Linux because the backend is unavailable on this host

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - focused builder/runtime consistency suite finished `6 passed, 0 failed`
    - the shared seam cut did not regress the remaining direct server-context control assertions

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - focused factory scope suite finished `6 passed, 0 failed`
    - client default-config / one-shot `ServerName` remains context-only state on FreePascal after the shared seam cut

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: PASS
  - summary:
    - focused factory isolation suite finished `6 passed, 0 failed`
    - one-shot/default config isolation remains green while FreePascal connections stay no-inheritance

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused dual-role clarification suite finished `28 passed, 0 failed, 1 skipped`
    - the shared seam cut did not reopen the already-closed `sslCtxBoth` no-inheritance boundary

- `git diff --check`
  - result: PASS
  - summary:
    - current shared client fallback cut batch has no whitespace or patch-format issues

### FreePascal Client Context SNI Fallback Cut

- add `docs/plans/2026-05-18-freepascal-client-context-sni-fallback-cut.md`
  - purpose:
    - define the first dedicated `sslCtxClient` behavior-migration batch after the cross-backend contract cleanup
    - keep scope on FreePascal runtime constructors instead of reopening all backends or shared shim consumers

- update `tests/test_freepascal_context_server_name_inheritance.pas`
  - change:
    - flip the dedicated FreePascal regression from inherited-fallback expectations to explicit no-inheritance expectations
    - locally suppress the deprecated direct-context setter warning at the negative-coverage callsite

- add `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - purpose:
    - fail if `src/fafafa.ssl.freepascal.connection.pas` still reads `GetContextLevelServerNameCompatibilityValue(AContext)`
    - keep the new FreePascal runtime cut guarded by a cheap source contract

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_freepascal_context_server_name_inheritance.pas`
    - keep the intentional label set aligned with the smaller remaining compatibility boundary

- `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - result: RED
  - summary:
    - initial failure proved the two FreePascal client constructors still read shared context-level `ServerName` compatibility fallback

- `mkdir -p tmp/test_freepascal_context_server_name_inheritance && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_context_server_name_inheritance -FEtmp/test_freepascal_context_server_name_inheritance -otmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance`
  - result: RED
  - summary:
    - both negative assertions failed
    - builder `WithSNI(...)` and direct context `SetServerName(...)` were still being inherited by new FreePascal client connections

- update `src/fafafa.ssl.freepascal.connection.pas`
  - change:
    - remove `GetContextLevelServerNameCompatibilityValue(AContext)` reads from the socket and stream client constructors
    - leave `FServerName` empty until callers explicitly set per-connection hostname/SNI

- `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - result: RED -> GREEN
  - summary:
    - FreePascal client constructors no longer read context-level `ServerName` compatibility fallback

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the dedicated FreePascal runtime regression

- `mkdir -p tmp/test_freepascal_context_server_name_inheritance && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_context_server_name_inheritance -FEtmp/test_freepascal_context_server_name_inheritance -otmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance`
  - result: RED -> GREEN
  - summary:
    - dedicated FreePascal regression now proves both socket and stream client connections no longer inherit context-level `ServerName`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - remaining intentional mock precedence contract stayed green
    - no production change in this batch accidentally rewrote the next planned compatibility surface

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - remaining connector override precedence contract stayed green

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_freepascal_context_server_name_inheritance.pas` from the intentional compatibility set
    - record the new FreePascal client runtime cut and move the next recommendation to `tests/test_connection_builder_hostname_precedence.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the FreePascal client fallback cut
    - shrink the intentional compatibility set and refresh the next recommended batch

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the dedicated FreePascal runtime cut into persistent repo working memory

### Connection Builder Explicit Hostname Cut

- add `docs/plans/2026-05-18-connection-builder-explicit-hostname-cut.md`
  - purpose:
    - define the next bounded client-side behavior-migration batch after the FreePascal runtime cut
    - keep scope on `TSSLConnectionBuilder.TryBuildClient` instead of reopening connector or shared backend compatibility shims

- update `tests/test_connection_builder_hostname_precedence.pas`
  - change:
    - flip case 1 from “preserve context fallback” to “clear context fallback”
    - keep case 2 explicit override and case 3 explicit empty clear intact
    - locally suppress the deprecated context setter warning at the mock setup callsite

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_connection_builder_hostname_precedence.pas`
    - keep the intentional compatibility label set aligned with the smaller remaining boundary

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: RED
  - summary:
    - only case 1 failed
    - `TryBuildClient` was still preserving inherited context fallback when no explicit hostname was provided

- update `src/fafafa.ssl.connection.builder.pas`
  - change:
    - when the built client connection supports `ISSLClientConnection`, `TryBuildClient` now always owns per-connection hostname state
    - if `WithHostname(...)` was not called, it explicitly clears `ServerName` to `''`
    - explicit override / explicit empty clear behavior remains unchanged

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: RED -> GREEN
  - summary:
    - all 9 assertions passed
    - client builder path no longer preserves inherited context fallback

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - connector override precedence stayed green
    - the builder-path cut did not regress the next higher-level client override surface

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the builder precedence test

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_connection_builder_hostname_precedence.pas` from the intentional compatibility set
    - record the builder explicit-hostname cut and move the next recommendation to `tests/test_tls_connector_hostname_override_precedence.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connection-builder explicit-hostname cut
    - shrink the remaining client-side intentional compatibility surface again

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the builder explicit-hostname cut into persistent repo working memory

### TLS Connector Override Without Context Fallback

- add `docs/plans/2026-05-18-tls-connector-override-no-context-fallback.md`
  - purpose:
    - define the bounded contract-cleanup batch that removes inherited context fallback from the connector override precedence test
    - keep production `TSSLConnector` code untouched because it already uses pure per-connection `SetServerName(...)`

- add `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if `tests/test_tls_connector_hostname_override_precedence.pas` still teaches `Ctx.SetServerName(...)`

- update `tests/test_tls_connector_hostname_override_precedence.pas`
  - change:
    - remove the mock context-level `SetServerName('ctx.example.com')` setup
    - rename the empty case text so it no longer talks about clearing inherited fallback

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_tls_connector_hostname_override_precedence.pas`
    - keep the intentional compatibility label set aligned with the smaller remaining boundary

- `bash -n tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - connector override precedence test no longer teaches context-level SNI

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the connector override test

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - connector override precedence behavior stayed green without the inherited context fallback input

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_tls_connector_hostname_override_precedence.pas` from the intentional compatibility set
    - move the next recommendation to `tests/test_tls_connector_early_data_contract.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connector override contract cleanup
    - shrink the remaining client-side intentional compatibility surface again

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the connector override contract cleanup into persistent repo working memory

### TLS Connector Early-Data Without Context Fallback

- add `docs/plans/2026-05-18-tls-connector-early-data-no-context-fallback.md`
  - purpose:
    - define the bounded contract-cleanup batch that removes inherited context fallback from the connector early-data contract
    - keep production `TSSLConnector` code untouched because it already applies explicit per-connection hostname before early-data queueing

- add `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if `tests/test_tls_connector_early_data_contract.pas` still teaches `Ctx.SetServerName(...)`

- update `tests/test_tls_connector_early_data_contract.pas`
  - change:
    - remove the mock context-level `SetServerName('ctx.example.com')` setup
    - rename the server-name assertion so it describes explicit hostname application instead of overriding inherited fallback

- `bash -n tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - connector early-data contract no longer teaches context-level SNI

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green while shrinking to the remaining server-side control case

- `mkdir -p tmp/test_tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_early_data_contract -FEtmp/test_tls_connector_early_data_contract -otmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas && ./tmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract`
  - result: PASS
  - summary:
    - connector early-data ordering and unsupported-path behavior stayed green without the inherited context fallback input

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - add the connector early-data contract cleanup as the fifth cut
    - move the next recommendation to `tests/test_context_builder_server_servername_runtime_consistency.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connector early-data contract cleanup
    - record that the remaining intentional compatibility label set is now only the server-side control case

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the connector early-data contract cleanup into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - current early-data contract cleanup batch has no whitespace or patch-format issues

### FreePascal Client Context ServerName Expectation Sync

- add `docs/plans/2026-05-18-freepascal-client-context-servername-expectation-sync.md`
  - purpose:
    - define the bounded sync batch that fixes stale FreePascal-focused contracts after the earlier client runtime fallback cut
    - keep the work on truth-sync instead of reopening unrelated server-side or release lanes

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - current intentional compatibility label set is now only the direct server-context control case

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED
  - summary:
    - `BuildClient.WithSNI(...)` still preserved context state
    - but FreePascal client connections no longer inherited that state
    - the focused contract was still asserting pre-cut behavior

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: RED
  - summary:
    - client default-config / one-shot config still preserved context state
    - but FreePascal client connections no longer inherited that state
    - factory focused contract was still asserting pre-cut connection fallback

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: RED
  - summary:
    - default-path / one-shot isolation contract showed the same stale inherited-connection expectation

- update FreePascal-focused contracts:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - change:
    - keep context-state assertions intact
    - replace inherited-connection assertions with explicit empty-ServerName expectations on FreePascal connections

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record that live retest exposed stale FreePascal-focused expectations
    - move the next recommendation to the remaining shared client fallback backends instead of the old server-side control case

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the FreePascal expectation-sync batch
    - restate the main unresolved seam as cross-backend shared client fallback divergence

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the FreePascal expectation correction and the corrected next route into persistent repo working memory

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED -> GREEN
  - summary:
    - client-side assertion now matches live FreePascal runtime truth
    - server-side control assertions remained green

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: RED -> GREEN
  - summary:
    - FreePascal factory client contract now correctly treats `ServerName` as context-only state, not inherited connection fallback

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: RED -> GREEN
  - summary:
    - default-path / one-shot isolation contract now reflects the same context-only boundary on FreePascal

- `git diff --check`
  - result: PASS
  - summary:
    - current FreePascal expectation-sync batch has no whitespace or patch-format issues

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

### Cross-Backend Network Contracts Per-Connection SNI

- add `docs/plans/2026-05-18-cross-backend-network-contracts-per-connection-sni.md`
  - purpose:
    - define the bounded batch that removes deprecated context-level SNI guidance from the two cross-backend network contracts
    - separate real cross-backend result/error contracts from intentional compatibility coverage

- add `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if the two cross-backend integration contracts still teach `Ctx.SetServerName(...)`
    - require an explicit `SetServerName(...)` call to remain, so the SNI step does not disappear silently

- `bash -n tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - both cross-backend network contracts now use explicit per-connection SNI instead of `Ctx.SetServerName(...)`

- update cross-backend network contracts:
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
  - change:
    - remove `Ctx.SetServerName(...)`
    - require `ISSLClientConnection`
    - move SNI setup to `ClientConn.SetServerName(...)` before `Connect`
    - migrate the `www.google.com:80` handshake-failure branch to the same per-connection path

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `test_cross_backend_consistency_contract`
    - remove `test_cross_backend_errors_contract`
    - keep the intentional-compat label set aligned with the smaller remaining real compatibility boundary

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the two cross-backend network contracts

- `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
  - result: PASS
  - summary:
    - compile/run shape stayed green after the per-connection SNI migration
    - runtime network probe remained skipped on this host because `FAFAFA_RUN_NETWORK_TESTS!=1`

- `mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract`
  - result: PASS
  - summary:
    - compile/run shape stayed green after the per-connection SNI migration
    - runtime network probe remained skipped on this host because `FAFAFA_RUN_NETWORK_TESTS!=1`

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove the two cross-backend network contracts from the intentional compatibility set
    - record the new Phase E cut that migrates them to per-connection SNI

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - remove the old claim that the two cross-backend network contracts must carry `INTENTIONAL_COMPAT`
    - add a dedicated closeout section for the per-connection SNI migration
    - refresh the next recommended batch toward `tests/test_freepascal_context_server_name_inheritance.pas`

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync this batch into persistent repo working memory so future sessions do not reopen the old misclassification

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the cross-backend per-connection SNI batch
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

### High-Level Context ServerName Ignore Cut

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - recovery script produced no extra unsynced context to merge
    - current session could continue directly from the live worktree and planning files

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: PASS
  - summary:
    - focused factory warning suite finished `16 passed, 0 failed`
    - default-config and one-shot client paths both emit the compatibility warning
    - built client contexts no longer retain deprecated `ServerName` state

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - focused config validation suite finished `53 passed, 0 failed`
    - builder validation wording and compatibility guidance stayed green after the high-level ignore cut

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend contract finished `20 passed, 0 failed, 1 skipped`
    - direct context API still keeps deprecated `ServerName` observable on the context itself
    - new client connections across available backends still do not inherit that state

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - sync current truth so builder/factory high-level paths are `warning + ignore`
    - move the next recommended batch from the old direct-state control case to final public surface cleanup prep

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the high-level context `ServerName` ignore cut
    - refresh the route summary so future sessions do not reopen the already-closed builder/factory legacy-state question

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that deprecated context-level `ServerName` no longer enters new contexts through builder/factory high-level paths
    - record that direct `ISSLContext.SetServerName/GetServerName` is now the last remaining observable compatibility surface

- `git diff --check`
  - result: PASS
  - summary:
    - current high-level ignore cut batch has no whitespace or patch-format issues

- `git status --short`
  - result: PASS
  - summary:
    - worktree contains the expected builder/factory/test/doc updates for the current batch
    - new plan file `docs/plans/2026-05-18-high-level-context-servername-ignore-cut.md` is ready to be added at commit time

### OpenSSL Library Default-Config ServerName Alignment

- add `docs/plans/2026-05-18-openssl-library-default-config-servername-alignment.md`
  - purpose:
    - define the bounded backend-specific alignment batch for the remaining OpenSSL direct-library default-config `ServerName` drift
    - keep scope on `ISSLLibrary.SetDefaultConfig + TOpenSSLLibrary.CreateContext(...)` instead of reopening the whole public-surface family

- add `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - purpose:
    - prove the OpenSSL direct-library client default-config path still preserved deprecated `ServerName`
    - prove the OpenSSL direct-library server default-config path was not rejecting client-scoped `ServerName` yet

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: RED
  - summary:
    - initial run failed `8` assertions
    - OpenSSL direct-library client path still preserved deprecated default `ServerName`
    - OpenSSL direct-library server path still created a context instead of rejecting the client-scoped field
    - no direct-library warning existed yet

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - move the server-scope validation into a true fail-fast check before context creation
    - stop applying `FDefaultConfig.ServerName` to new client contexts
    - emit an OpenSSL library warning through the library log callback when client default-config still carries deprecated `ServerName`

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: RED -> GREEN
  - summary:
    - focused OpenSSL direct-library clarification suite finished `13 passed, 0 failed`
    - client default-config `ServerName` is now warning + ignore
    - server default-config `ServerName` now fails fast before context creation

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - adjacent cross-backend contract stayed green (`20 passed, 0 failed, 1 skipped`)
    - the OpenSSL direct-library alignment did not regress the current no-inheritance truth on new client connections

- update `docs/reference/API_REFERENCE.md`
  - change:
    - extend the client SNI compatibility note so it also covers the direct OpenSSL library default-config path

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the OpenSSL direct-library alignment as the last remaining high-level write-surface closeout
    - keep the next recommended batch on final public surface cleanup prep

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the OpenSSL direct-library default-config alignment
    - refresh the route summary so future sessions do not re-open this backend-specific leak

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that builder, generic factory, and direct OpenSSL library paths no longer inject deprecated `ServerName` into newly created contexts

### Deprecated Builder/Config ServerName Surface Classification

- add `docs/plans/2026-05-18-deprecated-context-servername-compat-surface-classification.md`
  - purpose:
    - define the first static cleanup cut inside final public surface cleanup prep
    - keep scope on ordinary-test de-guidance plus explicit compatibility classification

- update selected tests under `tests/` and `tests/config/`
  - change:
    - remove ordinary `.WithSNI(...)` usage from `tests/test_quick.pas`
    - remove stale `LConfig.ServerName := ...` setup from `tests/winssl/test_winssl_connection_edge_cases.pas`
    - add `INTENTIONAL_COMPAT` labels to remaining builder/config compatibility coverage files
    - clarify `tests/test_data_structures.pas` and `tests/test_factory_logic.pas` messages so `ServerName` is framed as a compatibility field, not recommended flow guidance

- add `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - purpose:
    - confine deprecated builder/config ServerName surface to an explicit allowlist
    - fail if active ordinary tests reintroduce `.WithSNI(...)` or builder-config `ServerName :=`

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - remaining deprecated builder/config ServerName usage is confined to explicitly labeled compatibility tests
    - ordinary active tests no longer leak deprecated builder/config guidance

- `mkdir -p tmp/test_quick && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_quick -FEtmp/test_quick -otmp/test_quick/test_quick tests/test_quick.pas && ./tmp/test_quick/test_quick`
  - result: PASS
  - summary:
    - normal builder smoke still builds client and server contexts without `.WithSNI(...)`
    - quick smoke output stayed green after removing deprecated builder guidance from the ordinary path

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the public compatibility-surface classification cut complete
    - move the next recommended batch from test-surface cleanup prep to final API-shape decisions

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for deprecated builder/config ServerName surface classification
    - record that ordinary smoke/edge-case tests no longer teach deprecated builder/config guidance

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that remaining deprecated builder/config ServerName usage is now explicitly classified
    - record that the next highest-value work is final API-shape decisions, not more ordinary-test cleanup

### Active Direct Context ServerName Surface Classification

- add `docs/plans/2026-05-18-active-direct-context-servername-surface-classification.md`
  - purpose:
    - define the second static cleanup cut inside final public surface cleanup prep
    - keep scope on active direct-context `SetServerName(...)` classification only

- update selected compatibility tests
  - change:
    - add explicit `INTENTIONAL_COMPAT` labels to:
      - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      - `tests/test_sslctxboth_client_capability_clarification.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
    - keep runtime semantics unchanged; the batch is classification-only

- add `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - purpose:
    - classify every active real direct-context `SetServerName(...)` hit
    - fail if an active ordinary test reintroduces an unclassified direct-context ServerName setter

- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - all active real direct-context `SetServerName(...)` tests are now explicitly classified
    - no unexpected active ordinary test still uses a direct-context ServerName setter

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend clarification stayed green (`20 passed, 0 failed, 1 skipped`)
    - direct context state is still observable while new client connections stay no-inheritance across available backends

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder precedence contract stayed green (`9 passed, 0 failed`)
    - explicit hostname override/clear rules remain correct after the classification-only batch

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused `sslCtxBoth` clarification stayed green (`28 passed, 0 failed, 1 skipped`)
    - dual-role contexts still expose client capability without reintroducing implicit ServerName inheritance

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the active direct-context surface classification cut complete
    - keep the next recommended batch on final API-shape decisions

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for active direct-context ServerName surface classification
    - record that the next blocker is final public API shape, not more test-surface triage

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that all active real direct-context `SetServerName(...)` hits are now explicitly classified
    - record that the next highest-value work is final API-shape decisions, not more direct-context surface census

- update selected intentional compatibility tests
  - change:
    - add local deprecated getter/setter warning suppression to:
      - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      - `tests/test_sslctxboth_client_capability_clarification.pas`
      - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - keep runtime semantics unchanged; the batch is warning-noise cleanup only

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend clarification stayed green after local warning quarantine (`20 passed, 0 failed, 1 skipped`)
    - compile output no longer emits the direct-context `GetServerName` deprecated warnings from this test

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused `sslCtxBoth` clarification stayed green after local warning quarantine (`28 passed, 0 failed, 1 skipped`)
    - compile output no longer emits the direct-context `SetServerName` deprecated warnings from this test

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - focused builder/direct-context consistency contract stayed green after local warning quarantine (`6 passed, 0 failed`)
    - compile output no longer emits the direct-context `GetServerName` deprecated warnings from this test

### WithSNI Compiler Deprecation Alignment

- add `docs/plans/2026-05-18-withsni-compiler-deprecation-alignment.md`
  - purpose:
    - define the bounded source-truth batch that upgrades `WithSNI(...)` from documentation/runtime-only deprecation to compiler-level deprecation
    - keep runtime behavior unchanged while making the public builder surface tell the truth at compile time

- add `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - purpose:
    - fail if `ISSLContextBuilder.WithSNI(...)` or `TSSLContextBuilderImpl.WithSNI(...)` loses its compiler `deprecated` marker

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - mark both public `WithSNI(...)` declarations as compiler `deprecated`
    - reuse the same per-connection-hostname migration message already used by the runtime warnings

- update selected intentional compatibility tests under `tests/` and `tests/config/`
  - change:
    - add local warning suppression around intentional `.WithSNI(...)` callsites
    - keep behavior assertions unchanged; the batch is source-truth alignment plus compile-noise quarantine

- update `docs/reference/API_REFERENCE.md`
  - change:
    - record that `WithSNI(...)` is now also compiler deprecated, not only runtime warning + ignore

- `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - both builder `WithSNI(...)` declarations are now compiler deprecated
    - the dedicated source contract now guards this declaration-level truth

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - remaining deprecated builder/config compatibility usage stays confined to the existing allowlist after the compiler-deprecation alignment

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: PASS
  - summary:
    - focused builder warning suite finished `16 passed, 0 failed`
    - intentional `.WithSNI(...)` coverage stayed green after the compiler-level deprecation change

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - focused config validation suite finished `53 passed, 0 failed`
    - compatibility validation wording stayed aligned while compile output remained free of repeated known `.WithSNI(...)` deprecation noise

- `git diff --check`
  - result: PASS
  - summary:
    - current `WithSNI` compiler-deprecation batch has no whitespace or patch-format issues

### TSSLConfig ServerName Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-servername-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for `TSSLConfig.ServerName`
    - keep runtime behavior unchanged while preventing the record field from drifting back into ordinary client-path guidance

- add `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - purpose:
    - fail if the `TSSLConfig.ServerName` source comment, warning wording, or active-doc confinement drifts away from the current compatibility-only truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - repeat the client-side warning + ignore truth next to `Use TSSLConfig with TSSLFactory.CreateContext(...)`
    - explicitly redirect callers back to `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`

- `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new TSSLConfig surface-truth contract script is syntactically valid

- `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - result: RED -> GREEN
  - summary:
    - first run failed because markdown backticks inside double-quoted `rg` patterns triggered shell command substitution
    - the script was corrected to use fixed-string matching for the API reference bullets
    - `TSSLConfig.ServerName` source comment, warning wording, and active-doc confinement all match the intended compatibility-only truth
    - active docs currently mention `TSSLConfig.ServerName` only in `docs/reference/API_REFERENCE.md`

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the existing builder/config compatibility allowlist stays green after the TSSLConfig source/doc freeze batch

- `git diff --check`
  - result: PASS
  - summary:
    - current TSSLConfig surface-freeze batch has no whitespace or patch-format issues

### Direct Context ServerName Surface Truth Freeze

- add `docs/plans/2026-05-18-direct-context-servername-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for direct `ISSLContext.SetServerName/GetServerName`
    - keep runtime behavior unchanged while preventing deprecated direct context APIs from drifting back into ordinary client-path guidance

- add `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - purpose:
    - fail if the direct-context deprecation messages, production-source caller boundary, or active-doc guidance drift away from the current compatibility-only truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - explicitly classify `ISSLContext.SetServerName(...)` / `GetServerName(...)` as deprecated direct context compatibility APIs

- `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new direct-context surface-truth contract script is syntactically valid

- `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - direct `ISSLContext` ServerName deprecation messages remain correct
    - production `src/` contains no real direct context caller
    - active docs contain no `Ctx.SetServerName(...)`-style guidance

- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - active direct-context test coverage remains explicitly classified and confined after the source/doc freeze batch

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional direct-context compatibility control case stays explicitly labeled

- `git diff --check`
  - result: PASS
  - summary:
    - current direct-context surface-freeze batch has no whitespace or patch-format issues

### WithSNI Surface Truth Freeze

- add `docs/plans/2026-05-18-withsni-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for `TSSLContextBuilder.WithSNI(...)`
    - keep runtime behavior unchanged while preventing the deprecated fluent method from drifting back into ordinary builder guidance

- add `tests/scripts/test_withsni_surface_truth_contract.sh`
  - purpose:
    - fail if the WithSNI source comment, active-doc confinement, or source-hit boundary drifts away from the current compatibility-only truth

- `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new WithSNI surface-truth contract script is syntactically valid

- `bash tests/scripts/test_withsni_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - `WithSNI(...)` remains fenced to its current declaration/implementation boundary in `src/`
    - active docs currently mention `WithSNI(...)` only in `docs/reference/API_REFERENCE.md`
    - the source comment still classifies it as compatibility-only context-level SNI

- `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the dedicated compiler-deprecation contract still proves both public WithSNI declarations remain deprecated

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the existing builder/config compatibility allowlist stays green after the WithSNI surface-freeze batch

- `git diff --check`
  - result: PASS
  - summary:
    - current WithSNI surface-freeze batch has no whitespace or patch-format issues

### Post-SNI Interface Debt Triage

- add `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - purpose:
    - capture the next recommended route after the entire context-level SNI compatibility family was frozen for `v1.x`
    - avoid reopening old SNI cleanup when the broader interface-design debt should now move to `TSSLConfig` vs `ISSLConnection`

- read-only evidence triage:
  - summary:
    - `TSSLConfig` already has multiple field-scope truths on disk:
      - `BufferSize` / `HandshakeTimeout` = connection-scoped and rejected by factory
      - `LogLevel` / `LogCallback` = library-scoped and rejected by factory
      - several option-style fields still normalize into `Options`
    - `ISSLConnection` slimming remains larger-risk because it would touch every backend connection implementation plus many tests/helpers
    - next recommended bounded batch is therefore `TSSLConfig` cross-layer slimming roadmap, not immediate `ISSLConnection` surgery

### TSSLConfig Scope Buckets

- add `docs/plans/2026-05-18-tsslconfig-scope-buckets.md`
  - purpose:
    - define the first bounded post-SNI `TSSLConfig` truth batch
    - freeze mixed-scope field buckets before any larger slimming or backend refactor

- update `src/fafafa.ssl.base.pas`
  - change:
    - rewrite mixed-scope field comments so `BufferSize` / `HandshakeTimeout` / `Session*` / `ALPN` / early-data / logging / option-bridge fields now carry explicit scope truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `TSSLConfig Scope Buckets`
    - align the replay-store note so it explicitly says `context-scoped, server-only opt-in`

- add `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - purpose:
    - fail if the new source/doc bucket truth drifts away from current factory / OpenSSL direct-path evidence

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - the new TSSLConfig scope bucket contract script is syntactically valid

- `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - first result: FAIL
  - summary:
    - shell interpreted backtick-bearing double-quoted fixed-string assertions as command substitution
    - fix:
      - switch those fixed-string assertions to single-quoted literals
      - add `--` to `rg` invocations so bullet-like patterns are not parsed as flags

- `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - rerun result: PASS
  - summary:
    - source comments, API reference bucket section, factory scope checks, and OpenSSL direct-path apply points stay aligned

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - focused factory connection-scope clarification test remains green

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - focused factory logging-scope clarification test remains green

### Cross-backend Direct-Library Default-Config Parity Audit

- read-only static audit:
  - summary:
    - OpenSSL direct-library `CreateContext(AType)` explicitly applies `SessionCacheSize` / `SessionTimeout` / `ALPNProtocols` and handles deprecated `ServerName`
    - WinSSL direct-library `CreateContext(AType)` currently only applies `Options`
    - FreePascal / MbedTLS / WolfSSL direct-library `CreateContext(AType)` currently just create contexts
    - those same libraries still store `FDefaultConfig`, while their context classes expose `SessionCacheSize` / `SessionTimeout` / `ALPNProtocols`
    - this is the next highest-value parity risk to verify/fix before broader interface slimming

### Direct-Library Default-Config Parity Fix

- add `docs/plans/2026-05-18-direct-library-default-config-parity.md`
  - purpose:
    - define a bounded TDD batch for `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AType)` parity

- add `tests/test_direct_library_default_config_parity.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- `bash -n tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the new direct-library default-config parity contract script is syntactically valid

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.freepascal.lib.pas` was not normalizing `SetDefaultConfig(...)`

- `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library `CreateContext(sslCtxClient)` failed to reflect default-config:
      - `ProtocolVersions`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - normalized option-bridge `Options`

- update:
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - normalize `SetDefaultConfig(...)` via `TSSLFactory.NormalizeConfig(...)`
    - apply context-safe default fields in direct-library `CreateContext(AType)`

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - GREEN result: PASS
  - summary:
    - all targeted backend library units now keep the same direct-library default-config apply skeleton

- `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library client context now reflects:
      - `ProtocolVersions`
      - `PreferredVersion`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - normalized option-bridge `Options`

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - connection-scoped rejection truth on factory paths remains green after the direct-library parity fix

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - library-scoped logging truth on factory paths remains green after the direct-library parity fix

### Direct-Library ServerName Compatibility Parity

- add `docs/plans/2026-05-18-direct-library-servername-compatibility-parity.md`
  - purpose:
    - define the bounded TDD batch for direct-library `ServerName` compatibility warning/reject parity

- add `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- update `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - change:
    - allowlist the new intentional direct-library compatibility test

- `bash -n tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - result: PASS
  - summary:
    - the new direct-library ServerName parity contract script is syntactically valid

- `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.freepascal.lib.pas` was still missing server reject / client warning logic

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library path was still:
      - client silent ignore
      - server non-reject

- update:
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - align direct-library deprecated `ServerName` compatibility behavior to OpenSSL:
      - client default-config warning + ignore
      - server default-config reject

- `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - GREEN result: PASS
  - summary:
    - direct-library `ServerName` compatibility source truth is now aligned across all targeted backend library units

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library path now:
      - warns and ignores client default-config `ServerName`
      - rejects server default-config `ServerName`
      - stays quiet when `ServerName` is empty

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the new intentional direct-library compatibility test is properly confined in the allowlist

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the previous direct-library default-config parity batch remains intact after adding the `ServerName` special-case parity

### Direct-Library Early-Data And Replay-Store Parity

- add `docs/plans/2026-05-18-direct-library-early-data-replay-store-parity.md`
  - purpose:
    - define the bounded TDD batch for the last remaining direct-library special-case parity lane

- add `src/fafafa.ssl.context.config.pas`
  - purpose:
    - hold shared internal helper logic for replay-store scope validation, early-data apply, and replay-store installer apply
    - avoid re-copying the same logic into five backend library units

- add `tests/test_direct_library_early_data_replay_store_parity.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- `bash -n tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh && bash tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.openssl.backed.pas` still had no replay-store scope validation / early-data apply / replay-store apply on the direct-library path

- `mkdir -p tmp/test_direct_library_early_data_replay_store_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_early_data_replay_store_parity -FEtmp/test_direct_library_early_data_replay_store_parity -otmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity tests/test_direct_library_early_data_replay_store_parity.pas && ./tmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library path was still missing:
      - client `ClientEarlyDataEnabled` apply
      - server `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` apply
      - replay-store file / directory install
      - client replay-store rejection
      - conflicting replay-store file + directory rejection

- update:
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - connect all five backend library `CreateContext(AType)` paths to the shared helper
    - align direct-library early-data / replay-store behavior to the factory/context truth
    - update API reference so the direct-library note no longer says early-data / replay-store is still pending

- `bash -n tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh && bash tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - GREEN result: PASS
  - summary:
    - all targeted backend library units now validate replay-store scope and apply early-data / replay-store defaults on the direct-library path

- `mkdir -p tmp/test_direct_library_early_data_replay_store_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_early_data_replay_store_parity -FEtmp/test_direct_library_early_data_replay_store_parity -otmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity tests/test_direct_library_early_data_replay_store_parity.pas && ./tmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library path now:
      - applies `ClientEarlyDataEnabled`
      - applies `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
      - installs replay-store file / directory at the configured path
      - rejects client replay-store config
      - rejects conflicting replay-store file + directory

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh && bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - result: PASS
  - summary:
    - the earlier direct-library default-config and `ServerName` parity batches remain intact after adding early-data / replay-store parity

### TSSLConfig Option-Bridge Default Truth Parity

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-default-truth-parity.md`
  - purpose:
    - define a bounded batch for fresh default-config surface truth on the three option-bridge compatibility booleans

- add `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - purpose:
    - prove a real runtime RED across:
      - direct-library `GetDefaultConfig(...)`
      - factory-held `GetDefaultConfig(...)`
      - `CreateDefaultConfig(...)`
      - `SetDefaultConfig(GetDefaultConfig)` round-trip

- add `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - purpose:
    - keep constructor-level normalization and backend registration truth cheap to re-verify

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - result: PASS
  - summary:
    - constructor normalization and the API-reference truth note were present before the runtime narrowing continued

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - RED result: FAIL
  - summary:
    - direct `CreateFreePascalSSLLibrary` default-config truth was already green
    - `SetDefaultConfig(GetDefaultConfig)` direct-library round-trip was already green
    - only the factory-held / auto-detect / `CreateDefaultConfig(...)` lane still dropped `EnableSessionTickets`

- update `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - change:
    - add narrowing assertions for:
      - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
      - `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
  - summary:
    - this isolated the real source from `CreateDefaultConfig(...)` down to the factory-held backend instance itself

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - RED result: FAIL
  - summary:
    - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
      and `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
      were already stale before `CreateDefaultConfig(...)` ran
    - this proved the root cause lived in production backend instantiation, not only in the helper surface

- update:
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - change:
    - add explicit backend creator-function registration to `TSSLFactory`
    - prefer `CreateFunc` over raw registered-class instantiation in `CreateLibraryInstance(...)`
    - switch real backend registrations to `@Create*SSLLibrary`
    - extend the contract so creator-function registration truth is also guarded

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - GREEN result: PASS
  - summary:
    - constructor normalization is still present
    - real backend registrations now go through explicit creator functions

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - GREEN result: PASS
  - summary:
    - factory-held `GetDefaultConfig(...)`, auto-detect `GetDefaultConfig(...)`,
      and `CreateDefaultConfig(...)` now all preserve the FreePascal session-ticket truth
    - full focused suite finished `20 passed, 0 failed`

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the earlier direct-library default-config parity batch remains intact after the creator-path fix

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - the existing `CreateDefaultConfig(...)` baseline suite remains green after the factory creator-path change
    - logging-safe default behavior was not regressed

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge default-truth batch has no whitespace or patch-format issues

### TSSLConfig Option-Bridge Precedence Freeze

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-precedence-freeze.md`
  - purpose:
    - define a bounded batch for freezing the conflict-precedence truth between `Options` and option-bridge compatibility booleans

- add `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - purpose:
    - prove runtime truth across:
      - `TSSLFactory.NormalizeConfig(...)`
      - `TSSLFactory.CreateContext(const AConfig)`
      - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`

- add `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - purpose:
    - keep the precedence rule synchronized across source comments, docs, and backend normalization paths

- update:
  - `src/fafafa.ssl.factory.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - document the current `v1.x` precedence truth explicitly:
      - legacy booleans remain the compatibility write surface
      - conflicting option bits yield to the legacy booleans
      - final `Options` truth is then projected back to the booleans

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - RED result: FAIL
  - summary:
    - the first failure was only a shell-quoting bug in the new contract script
    - the script string containing backticks was accidentally interpreted by bash before any real repo truth was checked

- update `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - change:
    - switch the API-reference needles containing backticks to single-quoted shell strings
  - summary:
    - this removed the shell parser noise so the contract can verify actual repo truth

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - GREEN result: PASS
  - summary:
    - source/doc truth now explicitly records the option-bridge precedence rule
    - backend `SetDefaultConfig(...)` normalization paths remain aligned

- `mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas && ./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze`
  - result: PASS
  - summary:
    - full focused suite finished `16 passed, 0 failed`
    - `NormalizeConfig(...)`, one-shot factory path, and direct-library default-config path all follow the same precedence truth

- `bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier default-truth and scope-bucket batches remain intact after freezing precedence

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge precedence-freeze batch has no whitespace or patch-format issues

### TSSLConfig Option-Bridge Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-surface-truth-freeze.md`
  - purpose:
    - define a bounded batch for freezing the remaining public-surface truth of the three option-bridge booleans
    - keep the scope on source/doc/test guidance instead of reopening runtime semantics

- update:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - `tests/test_direct_library_default_config_parity.pas`
  - `tests/security/test_session_security.pas`
  - change:
    - tighten the three `TSSLConfig` option-bridge booleans to explicit `compatibility-only` source/doc truth
    - label the remaining dedicated compatibility tests
    - move active session-security coverage away from legacy boolean writes

- add `tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
  - purpose:
    - fail if source/docs/tests drift back toward treating the option-bridge booleans as ordinary primary inputs

- `bash -n tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new source/doc/test contract holds the narrowed compatibility-only truth

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - the first failure was only wording drift against the new narrowed API text
    - the scope-bucket contract was updated to the new compatibility-only phrasing instead of reopening runtime verification

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier fresh default-config contract now points at the new API wording and remains green

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - result: PASS
  - summary:
    - the earlier precedence-freeze contract now points at the new API wording and remains green

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - result: PASS
  - summary:
    - focused default-truth suite finished `20 passed, 0 failed`
    - the new compatibility labels/comments did not disturb the earlier runtime truth batch

- `mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas && ./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze`
  - result: PASS
  - summary:
    - focused precedence suite finished `16 passed, 0 failed`
    - narrowed public-surface wording did not disturb the earlier precedence contract

- `mkdir -p tmp/test_session_security && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_session_security -FEtmp/test_session_security -otmp/test_session_security/test_session_security tests/security/test_session_security.pas && ./tmp/test_session_security/test_session_security`
  - RED -> GREEN result: PASS
  - summary:
    - the first attempt incorrectly tried to prove session-ticket configurability by writing only `Options` through `NormalizeConfig(...)`
    - that failed because the already-frozen legacy-boolean precedence intentionally overrides conflicting option bits during normalization
    - the final fix moved the active security test to direct context `SetOptions(...)` / `GetOptions(...)`, finishing with `35 passed, 0 failed`

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge surface-truth batch has no whitespace or patch-format issues

### TSSLConfig Active Guidance Cleanup

- add `docs/plans/2026-05-18-tsslconfig-active-guidance-cleanup.md`
  - purpose:
    - define a bounded batch for cleaning up high-visibility TSSLConfig guidance drift in active example/reference surfaces

- update:
  - `examples/example_factory_usage.pas`
  - `docs/reference/ARCHITECTURE.md`
  - change:
    - remove `BufferSize` / `HandshakeTimeout` from the factory/config example path
    - redirect timeout/buffering guidance to connection / transport-level APIs
    - replace the stale pseudo-`TSSLConfig` record in architecture docs with current scope buckets

- add `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - purpose:
    - keep active example usage and architecture reference aligned with the current TSSLConfig scope truth
    - also keep the example-surface direct-context API coverage explicitly labeled

- `bash -n tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh && bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - the first failure was only an over-broad contract needle that accidentally matched an unrelated `ProtocolVersion` symbol elsewhere in the architecture doc
    - the final contract now stays focused on the real guidance truth instead of creating false reds

- `mkdir -p tmp/example_factory_usage && fpc -B -Fu./src -Fu./examples -FUtmp/example_factory_usage -FEtmp/example_factory_usage -otmp/example_factory_usage/example_factory_usage examples/example_factory_usage.pas`
  - result: PASS
  - summary:
    - active factory-usage example still compiles after removing the mixed-scope guidance drift
    - compile finished with existing repo warnings only; no new example breakage was introduced

- `git diff --check`
  - result: PASS
  - summary:
    - current active-guidance-cleanup batch has no whitespace or patch-format issues

### TSSLConfig Public-Surface Slimming Roadmap

- add `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
  - purpose:
    - turn the already-proved TSSLConfig scope truth into a field-level migration matrix that can drive future implementation batches

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `TSSLConfig Migration Targets`
    - map:
      - `LogLevel` / `LogCallback` -> library defaults surface
      - `HandshakeTimeout` / `BufferSize` -> connection / transport surface
      - `ServerName` -> per-connection SNI surface
      - option-bridge booleans -> `Options` / `WithOption(...)`
    - record the current `v1.x` status and the intended `v2` direction for each family

- add `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - purpose:
    - keep the API migration map and the dedicated slimming roadmap synchronized

- `bash -n tests/scripts/test_tsslconfig_migration_targets_contract.sh && bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - the migration matrix holds across both the API reference and the dedicated roadmap doc

- `git diff --check`
  - result: PASS
  - summary:
    - current slimming-roadmap batch has no whitespace or patch-format issues

### TSSLConfig Logging Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-logging-surface-truth-freeze.md`
  - purpose:
    - define a bounded batch for freezing the remaining active logging guidance truth around `TSSLConfig.LogLevel` / `LogCallback`
    - keep scope on docs/reference/examples + focused contracts, not runtime redesign

- add `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - purpose:
    - fail if active docs drift back toward teaching callback-only logging as a complete way to see info/debug output
    - keep API/reference/guides synchronized on the split between log-level defaults and callback installation

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new docs contract is syntactically valid before repo truth checks

- `bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - RED result: FAIL
  - summary:
    - first failure proved `docs/reference/API_REFERENCE.md` still lacked the explicit split between:
      - `LogLevel` via `GetDefaultConfig(...)` / `SetDefaultConfig(...)`
      - `LogCallback` via `SetLogCallback(...)`
    - the same active-doc drift also still existed in `USER_GUIDE` / `TROUBLESHOOTING`, where callback-only snippets immediately emitted `sslLogInfo` even though the default runtime threshold is still `sslLogError`

- update:
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - change:
    - make the library-default logging truth explicit in reference docs
    - require guide snippets to raise `LLogConfig.LogLevel` through `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` before showing `sslLogInfo` / `sslLogDebug` dispatch
    - keep callback installation on `ISSLLibrary.SetLogCallback(...)`

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - GREEN result: PASS
  - summary:
    - active docs/reference now agree on the logging owner boundary
    - callback-only examples no longer pretend info/debug output works under the default `sslLogError` threshold

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - focused logging scope suite finished `12 passed, 0 failed`
    - request-path rejection, library-default round-trip, and callback dispatch gating all stayed green after the doc truth cleanup

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - focused default-config suite kept the logging baseline truth green
    - `CreateDefaultConfig(...)` still returns request-safe `LogLevel = sslLogError` and `LogCallback = nil`

- `git diff --check`
  - result: PASS
  - summary:
    - current logging-surface-truth batch has no whitespace or patch-format issues

### Direct-Library Connection-Scope Clarification

- add `docs/plans/2026-05-18-direct-library-connection-scope-clarification.md`
  - purpose:
    - define a bounded batch for aligning direct-library `SetDefaultConfig(...)` + `CreateContext(AType)` with the existing connection-scope truth of `HandshakeTimeout` / `BufferSize`

- add:
  - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
  - `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - purpose:
    - prove the remaining direct-library silent-ignore drift with one runtime-focused FreePascal test and one cross-backend source/docs contract

- `bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - result: PASS
  - summary:
    - new direct-library connection-scope contract is syntactically valid before repo truth checks

- `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - RED result: FAIL
  - summary:
    - first failure proved `docs/reference/API_REFERENCE.md` still described `HandshakeTimeout` / `BufferSize` only in factory terms
    - the same contract would also have failed because no shared direct-library connection-scope validator existed yet

- `mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_connection_scope_clarification -FEtmp/test_freepascal_library_default_config_connection_scope_clarification -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification tests/test_freepascal_library_default_config_connection_scope_clarification.pas && ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification`
  - RED result: FAIL
  - summary:
    - initial FreePascal direct-library runtime proof showed both custom `HandshakeTimeout` and custom `BufferSize` were silently accepted on `Lib.CreateContext(sslCtxClient)` instead of raising `ESSLConfigurationException`

- update:
  - `src/fafafa.ssl.context.config.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - change:
    - add shared `ValidateDirectLibraryConnectionScope(...)`
    - wire all five backend `CreateContext(AType)` paths through that shared validator
    - update reference wording so direct-library path is explicitly covered by the same connection-scope truth

- `bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh && bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - first post-fix failure was only a false red from line-oriented grep against a multiline helper call
    - after tightening the contract to match the real helper invocation semantics, source/docs truth stayed green across all backend library paths

- `mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_connection_scope_clarification -FEtmp/test_freepascal_library_default_config_connection_scope_clarification -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification tests/test_freepascal_library_default_config_connection_scope_clarification.pas && ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification`
  - GREEN result: PASS
  - summary:
    - focused direct-library runtime suite finished `9 passed, 0 failed`
    - custom `HandshakeTimeout` / `BufferSize` now fail-fast on `ISSLLibrary.CreateContext(AType)` and request-safe defaults still build

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - existing factory connection-scope suite finished `12 passed, 0 failed`
    - the new shared direct-library validator did not disturb the already-frozen factory reject path

- `git diff --check`
  - result: PASS
  - summary:
    - current direct-library connection-scope clarification batch has no whitespace or patch-format issues

### Library-Default LogCallback Detachment

- add `docs/plans/2026-05-18-library-default-logcallback-detachment.md`
  - purpose:
    - define the first runtime/source implementation slice under the `LogLevel` / `LogCallback` slimming route
    - keep scope on callback ownership between `SetDefaultConfig(...)` and `SetLogCallback(...)`

- add `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - purpose:
    - fail if any backend still lets `SetDefaultConfig(...)` install the runtime callback
    - keep `SetLogCallback(...)` as the only source-guarded callback owner

- update `tests/test_factory_logging_scope_clarification.pas`
  - change:
    - strengthen the focused runtime proof so it now requires:
      - `SetDefaultConfig(LogCallback)` does not install the callback
      - `SetLogCallback(...)` remains the only owner
      - later `SetDefaultConfig(LogLevel)` updates filtering without clearing the installed callback

- `bash -n tests/scripts/test_library_default_logcallback_detachment_contract.sh && bash tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - RED result: FAIL
  - summary:
    - first source-contract failure immediately proved `src/fafafa.ssl.openssl.backed.pas` still let `SetDefaultConfig(...)` install `FLogCallback`
    - the same drift existed across the other backend library units as well

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - RED result: FAIL
  - summary:
    - strengthened runtime proof showed two concrete failures:
      - `SetDefaultConfig(...)` still visibleized callback input in `GetDefaultConfig(...)`
      - `SetDefaultConfig(LogCallback)` alone already made `Log(...)` dispatch

- update:
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `tests/test_factory_logging_scope_clarification.pas`
  - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `src/fafafa.ssl.base.pas`
  - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - change:
    - `SetDefaultConfig(...)` now preserves the current callback snapshot instead of installing/replacing it from `LConfig.LogCallback`
    - `SetLogCallback(...)` stays the only callback owner
    - direct-library warning tests now install callbacks through `SetLogCallback(...)`
    - docs/source comments now explicitly state that `SetDefaultConfig(...)` no longer installs or replaces callbacks

- `bash -n tests/scripts/test_library_default_logcallback_detachment_contract.sh && bash tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - GREEN result: PASS
  - summary:
    - all five backend library paths now keep callback ownership detached from `SetDefaultConfig(...)`

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - GREEN result: PASS
  - summary:
    - focused logging scope suite finished `17 passed, 0 failed`
    - callback installation, visibility, filtering, and ownership are now aligned around the dedicated setter path

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - result: PASS
  - summary:
    - direct-library FreePascal warning/reject suite finished `13 passed, 0 failed`
    - moving warning capture to `SetLogCallback(...)` did not regress the existing ServerName parity truth

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: PASS
  - summary:
    - direct-library OpenSSL warning/reject suite finished `13 passed, 0 failed`
    - the callback-owner cut did not disturb the existing OpenSSL ServerName compatibility path

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active docs now state the stronger truth that `SetDefaultConfig(...)` no longer installs or replaces callbacks

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - source comments, scope buckets, factory wording, and backend source still agree after the callback-owner cut

- `bash -n tests/scripts/test_tsslconfig_migration_targets_contract.sh && bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - the public slimming roadmap still agrees with the updated callback detachment truth

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - default-config suite remained green
    - `CreateDefaultConfig(...)` still returns request-safe `LogLevel = sslLogError` and `LogCallback = nil`

- `git diff --check`
  - result: PASS
  - summary:
    - current library-default callback detachment batch has no whitespace or patch-format issues

### Noninteractive Core Compat Tests

- add `docs/plans/2026-05-18-noninteractive-core-compat-tests.md`
  - purpose:
    - define a bounded cleanup batch for turning two core compat/record-shape tests into real noninteractive test programs
    - keep scope on `tests/test_factory_logic.pas` and `tests/test_data_structures.pas`

- `rg -n "ReadLn\\;|按回车键退出" tests/test_factory_logic.pas tests/test_data_structures.pas`
  - result: PASS
  - summary:
    - both files still contained interactive exit prompts and `ReadLn`

- `zsh -lc "mkdir -p tmp/test_factory_logic && fpc ... && printf '\\n' | ./tmp/test_factory_logic/test_factory_logic"`
  - result: PASS
  - summary:
    - pre-fix direct run finished only after feeding stdin
    - output ended with `按回车键退出...`, confirming the remaining manual-exit tail in the core factory test

- `zsh -lc "mkdir -p tmp/test_data_structures && fpc ... && printf '\\n' | ./tmp/test_data_structures/test_data_structures"`
  - result: PASS
  - summary:
    - pre-fix direct run finished only after feeding stdin
    - output ended with `按回车键退出...`, confirming the remaining manual-exit tail in the core data-structure test

- `timeout 2 ./tmp/test_factory_logic/test_factory_logic`
  - result: PASS
  - summary:
    - headless run did not hard-hang on this host
    - but it still printed the interactive exit prompt, proving the test binary remained automation-noisy even when stdin was absent

- `timeout 2 ./tmp/test_data_structures/test_data_structures`
  - result: PASS
  - summary:
    - same result for the core data-structure test: no hard hang here, but the interactive exit tail still polluted automated output

- update:
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - change:
    - remove `按回车键退出...` + `ReadLn`
    - extend the `INTENTIONAL_COMPAT` header note so it explicitly includes mixed-scope record-shape fields such as `BufferSize` / `HandshakeTimeout`

- `zsh -lc "mkdir -p tmp/test_factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logic -FEtmp/test_factory_logic -otmp/test_factory_logic/test_factory_logic tests/test_factory_logic.pas >/tmp/test_factory_logic.build.log && timeout 2 ./tmp/test_factory_logic/test_factory_logic"`
  - GREEN result: PASS
  - summary:
    - core factory logic suite finished `80 passed, 0 failed`
    - output now ends cleanly at the test summary without the old interactive-exit tail

- `zsh -lc "mkdir -p tmp/test_data_structures && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_data_structures -FEtmp/test_data_structures -otmp/test_data_structures/test_data_structures tests/test_data_structures.pas >/tmp/test_data_structures.build.log && timeout 2 ./tmp/test_data_structures/test_data_structures"`
  - GREEN result: PASS
  - summary:
    - core data-structure suite finished `102 passed, 0 failed`
    - output now ends cleanly at the test summary without the old interactive-exit tail

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive core compat test batch has no whitespace or patch-format issues

### Noninteractive Top-Level Core Tests

- `rg -n "ReadLn|按回车键退出" tests/test_exceptions.pas tests/test_base_interface_contract.pas`
  - result: PASS
  - summary:
    - both top-level core tests still contained interactive exit prompts and `ReadLn`

- `mkdir -p tmp/test_exceptions && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_exceptions -FEtmp/test_exceptions -otmp/test_exceptions/test_exceptions tests/test_exceptions.pas && timeout 2 ./tmp/test_exceptions/test_exceptions`
  - result: PASS
  - summary:
    - headless run did not hard-hang on this host because stdin EOF let the program exit
    - but the output still ended with `按回车键退出...`, proving the test remained automation-noisy

- `mkdir -p tmp/test_base_interface_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_base_interface_contract -FEtmp/test_base_interface_contract -otmp/test_base_interface_contract/test_base_interface_contract tests/test_base_interface_contract.pas && timeout 2 ./tmp/test_base_interface_contract/test_base_interface_contract`
  - result: PASS
  - summary:
    - same result for the base-interface core test: no hard hang here, but the interactive-exit tail still polluted automated output

- `rg -n "ReadLn|按回车键退出" tests`
  - result: PASS
  - summary:
    - repo-wide scan showed more `ReadLn` hits remain
    - the residual set is mainly examples, diagnostics, benchmarks/file readers, and WinSSL-specialized programs rather than top-level core tests

- add `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
  - purpose:
    - define a bounded cleanup batch for turning the remaining top-level core interactive tests into real noninteractive test programs
    - keep scope on `tests/test_exceptions.pas` and `tests/test_base_interface_contract.pas`

- add `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - purpose:
    - guard the two top-level core tests against reintroducing `ReadLn` or `按回车键退出...`

- `bash -n tests/scripts/test_top_level_core_tests_noninteractive_contract.sh && bash tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - result: RED
  - summary:
    - new contract immediately failed on `tests/test_exceptions.pas`
    - the failure proved the remaining interactive exit tail was still real at source level

- update:
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - change:
    - remove `按回车键退出...` + `ReadLn`
    - keep all assertions and coverage targets unchanged

- `bash tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - GREEN result: PASS
  - summary:
    - the new source contract now confirms both top-level core tests are noninteractive

- `mkdir -p tmp/test_exceptions && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_exceptions -FEtmp/test_exceptions -otmp/test_exceptions/test_exceptions tests/test_exceptions.pas && timeout 2 ./tmp/test_exceptions/test_exceptions`
  - GREEN result: PASS
  - summary:
    - exception core suite finished `64 passed, 0 failed`
    - output now ends cleanly at the summary without the old interactive-exit tail

- `mkdir -p tmp/test_base_interface_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_base_interface_contract -FEtmp/test_base_interface_contract -otmp/test_base_interface_contract/test_base_interface_contract tests/test_base_interface_contract.pas && timeout 2 ./tmp/test_base_interface_contract/test_base_interface_contract`
  - GREEN result: PASS
  - summary:
    - base-interface core suite finished `89 passed, 0 failed`
    - output now ends cleanly at the summary without the old interactive-exit tail

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive top-level core test batch has no whitespace or patch-format issues

### Noninteractive WinSSL Active Tests

- `sed -n '80,110p' run_winssl_tests.ps1`
  - result: PASS
  - summary:
    - current Windows-focused runner explicitly classifies `tests\\unit\\test_winssl_comprehensive.pas` as `Minimal, non-network, non-interactive tests`
    - this proved the remaining interactive tail in that file was a real workflow contradiction, not just a cosmetic annoyance

- `sed -n '55,80p' scripts/run_tests_windows.ps1`
  - result: PASS
  - summary:
    - legacy Windows run script still attempts to auto-run WinSSL unit-level tests
    - this further confirmed the batch should stay on active WinSSL test programs, not examples/diagnostics

- `rg -n "ReadLn|按回车键退出" tests/unit/*.pas tests/winssl/*.pas tests/examples/*.pas tests/diagnostic/*.pas`
  - result: PASS
  - summary:
    - after the top-level core cleanup, remaining interactive tails were concentrated in WinSSL-specialized tests plus examples/diagnostics
    - this narrowed the next bounded batch to WinSSL active tests only

- `tail -n 35 tests/unit/test_winssl_comprehensive.pas`
  - result: PASS
  - summary:
    - both the Windows main path and the non-Windows fallback still ended with `Press Enter to exit...` + `ReadLn`

- `tail -n 35 tests/winssl/test_winssl_context_comprehensive.pas`
- `tail -n 35 tests/winssl/test_winssl_errors_comprehensive.pas`
- `tail -n 35 tests/winssl/test_winssl_monitoring.pas`
- `tail -n 35 tests/winssl/test_winssl_connection_edge_cases.pas`
- `tail -n 35 tests/winssl/test_winssl_certstore.pas`
- `tail -n 35 tests/winssl/test_winssl_session_management.pas`
- `tail -n 35 tests/winssl/test_winssl_library_basic.pas`
- `tail -n 35 tests/winssl/test_winssl_certificate_loading.pas`
  - result: PASS
  - summary:
    - each active WinSSL test still carried the same interactive exit tail
    - this confirmed the issue was systematic across the active WinSSL automation layer

- add `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
  - purpose:
    - define a bounded cleanup batch for active WinSSL test programs only
    - keep examples / diagnostics / benchmark out of scope

- add `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - purpose:
    - guard the active WinSSL test set against reintroducing `ReadLn` / `Press Enter to exit...` / `按回车键退出...`

- `bash -n tests/scripts/test_winssl_active_tests_noninteractive_contract.sh && bash tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - result: RED
  - summary:
    - new contract immediately failed on `tests/unit/test_winssl_comprehensive.pas`
    - the failure proved the active WinSSL noninteractive drift was still real at source level

- update:
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`
  - change:
    - remove the interactive exit tail
    - keep all assertions and test bodies unchanged

- `bash tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - GREEN result: PASS
  - summary:
    - active WinSSL tests are now source-guarded as noninteractive

- `mkdir -p tmp/test_unit_winssl_comprehensive_nonwindows && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_unit_winssl_comprehensive_nonwindows -FEtmp/test_unit_winssl_comprehensive_nonwindows -otmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive tests/unit/test_winssl_comprehensive.pas && timeout 2 ./tmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive`
  - GREEN result: PASS
  - summary:
    - the non-Windows fallback branch compiled and exited cleanly on Linux
    - output no longer ended with `Press Enter to exit...`

- `mkdir -p tmp/winssl_unit_comp_win64 && fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_unit_comp_win64 -FEtmp/winssl_unit_comp_win64 -otmp/winssl_unit_comp_win64/test_winssl_comprehensive.exe tests/unit/test_winssl_comprehensive.pas`
  - GREEN result: PASS
  - summary:
    - Win64 cross-compile succeeded and linked `tmp/winssl_unit_comp_win64/test_winssl_comprehensive.exe`
    - warnings were pre-existing compile noise unrelated to the interactive-tail cleanup

- `mkdir -p tmp/winssl_session_mgmt_win64 && fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas`
  - GREEN result: PASS
  - summary:
    - Win64 cross-compile succeeded and linked `tmp/winssl_session_mgmt_win64/test_winssl_session_management.exe`
    - this gave a second Windows-side syntax proof on a dedicated WinSSL test program

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive WinSSL active test batch has no whitespace or patch-format issues

### Backend Optional-Surface Completion-Audit Revalidation

- `for f in docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md; do ...; done`
  - result: PASS
  - summary:
    - all 6 targeted backend completion-audit plans were confirmed to be missing execution-result sections
    - this proved the next gap was documentation/evidence completeness, not missing contract code

- `rg -n "Contract [0-9]+:|ISSLHttpHooksAccess|ISSLDiagnostics|ISSLNativeHandleAccess|ISSLEarlyDataContext|ISSLServerOCSPStaplingContext" tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - `tests/contract/test_backend_contract.pas` already contains Contracts 12-18 for the targeted optional surfaces
    - the repo therefore already had the right focused verifier; it just lacked current execution receipts in the plan docs

- add `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
  - purpose:
    - define a bounded evidence-closeout batch for backend optional public surfaces already covered by `test_backend_contract`
    - keep scope on focused revalidation instead of reopening broader design work

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - GREEN result: PASS
  - summary:
    - focused contract suite finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - OpenSSL / WolfSSL / MbedTLS / FreePascal all passed:
      - Contract 12: context optional interface alignment
      - Contract 13: context native-handle interface alignment
      - Contract 14: context HTTP hooks interface alignment
      - Contract 15: session native-handle interface alignment
      - Contract 17: certificate-store native-handle interface alignment
      - Contract 18: diagnostics interface alignment
    - WinSSL continued to skip on the current Linux host, and session native-handle kept the dedicated Windows-batch boundary

- update:
  - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
  - change:
    - add `Focused Revalidation Result (2026-05-18)` sections
    - record the live `test_backend_contract` outcome without falsely claiming the heavy compile/minimal-ci gates were rerun in this batch

- `git diff --check`
  - result: PASS
  - summary:
    - current backend optional-surface completion-audit revalidation batch has no whitespace or patch-format issues

### ISSLConnection Surface Truth Freeze

- `git status --short --branch && git log -1 --oneline --decorate`
  - result: PASS
  - summary:
    - worktree was clean at batch start
    - latest synced commit was `992382d docs/audit: record backend optional-surface revalidation`

- `rg -n "ISSLConnection|GetCipherBits|VerifyPeerCertificate|GetSessionID|IsSessionResumed|GetSessionData|SetSessionData|GetSelectedALPNProtocol|GetSession\\b|SetSession\\b|IsSessionReused|GetVerifyResult|GetOCSPResponseStatus|GetNativeHandle" src/fafafa.ssl.base.pas docs/reference/API_REFERENCE.md docs/reference/INTERFACE_DESIGN_V2.md docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - result: PASS
  - summary:
    - source and active docs were confirmed to be out of sync in the `ISSLConnection` area
    - `API_REFERENCE.md` still documented obsolete methods while the source exposed a larger current surface plus optional-owner splits

- `sed -n '1122,1555p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - established the current source truth for `ISSLConnection`, `ISSLClientConnection`, `ISSLDiagnostics`, `ISSLSessionResumption`, `ISSLCertificateVerification`, `ISSLOCSPStapling`, and `ISSLConnectionInfo`

- `sed -n '1663,1684p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - established the current source truth for `ISSLSession`
    - confirmed the active session surface is `GetID` / `Serialize` / `Clone`, not `GetSessionData` / `GetLastAccessTime`

- `nl -ba docs/reference/API_REFERENCE.md | sed -n '413,930p'`
  - result: PASS
  - summary:
    - confirmed the active docs still promised stale `ISSLConnection` and `ISSLSession` methods
    - example code still used `GetSessionID` and `IsSessionResumed`

- add `docs/plans/2026-05-18-isslconnection-surface-truth-freeze.md`
  - purpose:
    - define a bounded doc/contract truth-freeze batch before any public-interface slimming work

- update `docs/reference/API_REFERENCE.md`
  - change:
    - replace stale `ISSLConnection` signature block with the current source truth
    - add `v1.x` compatibility-core / optional-owner notes
    - rewrite session examples to use `GetID`, `Serialize`, and `IsSessionReused`

- add `tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - purpose:
    - fail if active docs reintroduce stale `ISSLConnection` / `ISSLSession` names
    - require current source-truth methods and optional-owner notes to remain visible

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - correct the stale route priority
    - make `ISSLConnection surface truth freeze` the immediate next batch instead of defaulting back to `TSSLConfig`

- `bash -n tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new focused contract script is syntactically valid

- `bash tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active `ISSLConnection` / `ISSLSession` docs now match current source truth
    - stale names no longer appear in the guarded active-doc section

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnection surface truth freeze` batch has no whitespace or patch-format issues

### Backend Connection-Surface Completion-Audit Revalidation

- `for f in docs/plans/2026-05-04-backend-*.md; do ...; done`
  - result: PASS
  - summary:
    - re-scan confirmed only 3 targeted connection-layer plans were still missing current execution receipts:
      - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
      - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
      - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
    - `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification` plans already had execution results and were not the next gap

- `rg -n "Contract [0-9]+: .*SNI|Contract [0-9]+: .*native-handle|Contract [0-9]+: .*OCSP|ISSLClientConnection|ISSLNativeHandleAccess|ISSLOCSPStapling" tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - confirmed the repo already has the right focused verifier for the three missing plan receipts:
      - Contract 8: client connection SNI interface alignment
      - Contract 10: client connection OCSP interface alignment
      - Contract 11: connection native-handle interface alignment

- add `docs/plans/2026-05-18-backend-connection-surface-completion-audit-revalidation.md`
  - purpose:
    - define a bounded evidence-closeout batch for the remaining connection-layer plans missing current execution receipts

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused contract suite finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - direct connection-layer truth relevant to this batch:
      - Contract 8:
        - OpenSSL / WolfSSL / MbedTLS / FreePascal PASS
        - WinSSL SKIP on the current Linux host
      - Contract 10:
        - OpenSSL / WolfSSL / FreePascal OCSP-capable connection surfaces PASS
        - MbedTLS absent-path PASS
        - WinSSL SKIP
      - Contract 11:
        - OpenSSL / WolfSSL / MbedTLS native-handle surfaces PASS
        - FreePascal absent-path PASS
        - WinSSL SKIP

- update:
  - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
  - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
  - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
  - change:
    - add `Focused Revalidation Result (2026-05-18)` sections
    - record current live `test_backend_contract` evidence without falsely claiming heavy compile/minimal-ci gates were rerun in this batch

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the remaining connection-layer execution-receipt gap as closed
    - move the next route forward to a real `ISSLConnection` slimming slice

### ISSLConnectionInfo Mirror Demotion Migration Map

- `rg -n "ISSLConnectionInfo|GetConnectionInfo|GetContext\\b|GetSelectedALPNProtocol|GetStateString" src/fafafa.ssl.base.pas src/fafafa.ssl.connection.base.pas tests/contract/test_backend_contract.pas docs/reference/API_REFERENCE.md docs/reference/INTERFACE_DESIGN_V2.md docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - result: PASS
  - summary:
    - confirmed current source truth, active API docs, contract proof, and v2 design doc were no longer aligned on the `ISSLConnectionInfo` mirror group
    - the design doc still omitted `ISSLConnectionInfo` from the hierarchy and misrouted part of the migration table

- `sed -n '1,220p' docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - confirmed `INTERFACE_DESIGN_V2.md` still used the empty `ISSLAdvanced` bucket
    - confirmed the class example omitted `ISSLConnectionInfo`
    - confirmed the migration table still mapped `GetConnectionInfo` to `ISSLDiagnostics`

- add `docs/plans/2026-05-18-isslconnectioninfo-mirror-demotion-migration-map.md`
  - purpose:
    - define a bounded design-only batch that freezes the Stage-A demotion route for the `ISSLConnectionInfo` mirrors before any source-facing slimming work

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - add `ISSLConnectionInfo` to the hierarchy and extension definitions
    - remove the stale `ISSLAdvanced` bucket
    - correct the implementation example and migration snippet
    - freeze `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString` to `ISSLConnectionInfo` as the Stage-A demotion target

- add `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - purpose:
    - fail if `INTERFACE_DESIGN_V2.md` reintroduces stale owner targets or the empty `ISSLAdvanced` bucket
    - require the Stage-A `ISSLConnectionInfo` demotion map to remain visible

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the migration-map batch as delivered
    - move the next route to source-facing slimming prep

- `bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - new migration-target contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - `INTERFACE_DESIGN_V2.md` now keeps the Stage-A `ISSLConnectionInfo` demotion map consistent
    - stale owner targets and the empty `ISSLAdvanced` bucket are no longer present

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo mirror demotion / migration-map` batch has no whitespace or patch-format issues

### ISSLConnectionInfo Active Guidance De-emphasis

- `rg -n -F "GetSelectedALPNProtocol" ...` / `rg -n -F "GetStateString" ...` / `rg -n -F "GetConnectionInfo" ...`
  - result: PASS
  - summary:
    - confirmed active docs still taught direct core mirror usage in `API_REFERENCE.md` and `INTEGRATION_GUIDE.md`
    - this remained misaligned with the just-frozen Stage-A `ISSLConnectionInfo` demotion map

- add `docs/plans/2026-05-18-isslconnectioninfo-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded user-facing doc batch that switches connection-info mirrors from core teaching to `ISSLConnectionInfo`-first guidance

- update:
  - `docs/reference/API_REFERENCE.md`
  - `docs/INTEGRATION_GUIDE.md`
  - change:
    - replace direct `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString` example guidance
    - switch examples to `Supports(..., ISSLConnectionInfo, ...)`
    - add an explicit note that new code should prefer `ISSLConnectionInfo` for this mirror group

- add `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - purpose:
    - fail if active docs reintroduce direct core mirror teaching for connection-info mirrors
    - require `ISSLConnectionInfo`-first guidance in the guarded active docs

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark active-guidance de-emphasis as delivered
    - keep the next route on source-facing slimming prep

- `bash -n tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - new active-guidance contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs now prefer `ISSLConnectionInfo` for the connection-info mirror group
    - direct core mirror teaching no longer appears in the guarded examples

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo active guidance de-emphasis` batch has no whitespace or patch-format issues

### ISSLConnectionInfo Source Classification Freeze

- `sed -n '1188,1295p' src/fafafa.ssl.base.pas` / `sed -n '1520,1548p' src/fafafa.ssl.base.pas` / `sed -n '36,72p' src/fafafa.ssl.connection.base.pas`
  - result: PASS
  - summary:
    - confirmed source comments still lacked an explicit Stage-A classification note for the `ISSLConnectionInfo` mirror group
    - confirmed the next source-facing gap was classification truth, not implementation behavior

- add `docs/plans/2026-05-18-isslconnectioninfo-source-classification-freeze.md`
  - purpose:
    - define a bounded source-facing prep batch that freezes the `compatibility-core duplicate` classification in source comments before any implementation cut

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add Stage-A classification notes for `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
    - clarify that `ISSLConnectionInfo` is the current owner used to carry these `v1.x` compatibility-core duplicates

- add `tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - purpose:
    - fail if source comments lose the Stage-A classification notes
    - keep source-facing truth aligned with the roadmap and active docs

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark source classification freeze as delivered
    - move the next route to the first real implementation slice decision

- `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - result: PASS
  - summary:
    - new source-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - result: PASS
  - summary:
    - source comments now keep the `ISSLConnectionInfo` mirror group aligned with the Stage-A roadmap
    - source-facing duplicate-owner truth no longer depends only on external docs

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo source classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the focused source-classification proof during final commit preparation
    - current batch is ready to commit without reopening heavier verification lanes

### GetContext Active Guidance De-emphasis

- `sed -n '388,410p' docs/CAPABILITY_MATRIX_GUIDE.md` / `sed -n '548,586p' docs/reference/API_REFERENCE.md` / `rg -n "\\.GetContext\\b|GetContext\\(" src tests docs`
  - result: PASS
  - summary:
    - confirmed the last active-doc example still teaching direct core `GetContext` was in `CAPABILITY_MATRIX_GUIDE.md`
    - confirmed `API_REFERENCE.md` had not yet explicitly grouped `GetContext` into the `ISSLConnectionInfo`-first guidance sentence
    - confirmed production source had no extra live callers beyond the base implementation and mirror-equality contract coverage

- add `docs/plans/2026-05-18-getcontext-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded `GetContext` batch that keeps the work on active guidance and route selection instead of prematurely changing the public signature

- update:
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - switch the capability example from `Conn.GetContext` to `ISSLConnectionInfo.GetContext`
    - explicitly include `GetContext` in the API reference's `ISSLConnectionInfo`-first note

- add `tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - purpose:
    - fail if active docs reintroduce direct core `GetContext` teaching
    - keep `GetContext` aligned with the current `ISSLConnectionInfo` owner route

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the `GetContext` active-guidance cut as delivered
    - record `GetContext` as the current first-priority mirror for the next real implementation slice

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` active-guidance contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs no longer teach `Conn.GetContext` as the preferred path
    - `GetContext` is now explicitly aligned with `ISSLConnectionInfo`-first guidance

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext active guidance de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the focused `GetContext` guidance proof during final commit preparation
    - current batch is ready to commit without reopening heavier verification lanes

### GetContext Contract Owner Primacy

- `sed -n '1788,1856p' tests/contract/test_backend_contract.pas` / `rg -n \"test_backend_contract\\.pas|GetContext\" progress.md docs/plans tests/scripts`
  - result: PASS
  - summary:
    - confirmed the remaining live `GetContext` coupling had shrunk to the contract layer
    - confirmed the contract still narrated `ISSLConnection.GetContext` and `ISSLConnectionInfo.GetContext` as a dual-owner pair

- add `docs/plans/2026-05-18-getcontext-contract-owner-primacy.md`
  - purpose:
    - define a bounded contract-semantics batch that promotes `ISSLConnectionInfo.GetContext` to the primary owner without touching runtime implementation

- update `tests/contract/test_backend_contract.pas`
  - change:
    - check `ISSLConnectionInfo.GetContext` against the creation context before consulting the core mirror
    - keep `ISSLConnection.GetContext` only as a mirror-equality proof after optional-owner truth is established

- add `tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - purpose:
    - fail if the backend contract drifts back to legacy dual-owner `GetContext` wording
    - keep the new optional-owner/core-mirror semantics cheap to revalidate

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetContext` contract owner primacy as delivered
    - move the next route to stronger `GetContext` feasibility / deprecation discussion

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` contract-owner guard script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - backend contract source now treats `ISSLConnectionInfo.GetContext` as the primary owner
    - legacy dual-owner `GetContext` wording is no longer present in the guarded contract block

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract suite finished `135 total / 111 passed / 0 failed / 24 skipped`
    - OpenSSL / WolfSSL / MbedTLS / FreePascal kept `Contract 19: Connection-info interface alignment` green after the owner-primacy change
    - WinSSL continued to follow the current Linux-host skip truth

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext contract owner primacy` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetContext` contract-owner proof after the final planning-file sync
    - no heavy recompile was needed because only planning files changed after the last focused Pascal contract run

### GetContext Source/Class Split Feasibility Freeze

- `rg -n "\\.GetContext\\b|GetContext\\(" src tests docs --glob '!docs/archive/**' --glob '!docs/plans/**'` / `rg -n "function .*GetContext: ISSLContext" src`
  - result: PASS
  - summary:
    - confirmed the remaining live `GetContext` surface had shrunk to interface declarations, one shared base implementation, one active-doc `ConnInfo.GetContext` path, and one backend-contract core mirror proof
    - confirmed production source had no extra direct `GetContext` call dependency to block a future class/surface split discussion

- add `docs/plans/2026-05-18-getcontext-source-class-split-feasibility-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so the next route decision no longer depends on repeating the same `GetContext` source archaeology

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner / mirror notes for `GetContext`
    - spell out that the shared base implementation now mainly exists to support the compatibility mirror plus the current `ISSLConnectionInfo` owner

- add `tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - purpose:
    - freeze the current `GetContext` remaining live surface into a cheap allowlist contract
    - fail if active docs, source, or non-script tests reintroduce new direct core `GetContext` dependencies

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the source/class split feasibility freeze as delivered
    - move the next route decision to public deprecation wording vs. the next mirror

- first run of `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: RED
  - summary:
    - the initial script exited early because zero-hit `rg` pipelines still returned status `1` under `set -euo pipefail`
    - adjusted the counting branches to tolerate zero-hit scans explicitly before re-running

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` source/class split contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: PASS
  - summary:
    - `GetContext` live surface is now frozen to the expected allowlist
    - no new active-doc, source, or non-script test dependency escaped the guarded boundary

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext source/class split feasibility freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetContext` allowlist proof after the final planning-file sync
    - no heavy Pascal contract rerun was needed because only planning files changed after the source/class split freeze passed

### GetStateString Active Test De-emphasis

- `rg -n "GetStateString|ISSLConnectionInfo" tests/connection/test_connection_basic.pas tests/integration/test_real_https_connection.pas`
  - result: PASS
  - summary:
    - confirmed the highest-value remaining ordinary `GetStateString` usage lived in the generic connection smoke test and the real HTTPS integration suite
    - confirmed the next batch could stay on active-test de-emphasis without reopening backend-specific runtime surfaces

- add `docs/plans/2026-05-18-getstatestring-active-test-deemphasis.md`
  - purpose:
    - define a bounded `GetStateString` batch that moves ordinary generic/integration tests off the core getter before touching backend-specific runtime files

- update:
  - `tests/connection/test_connection_basic.pas`
  - `tests/integration/test_real_https_connection.pas`
  - change:
    - route generic/integration state-string reads through `ISSLConnectionInfo`
    - add an integration helper so handshake-failure reporting no longer directly calls the core getter

- add `tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - purpose:
    - fail if ordinary generic/integration tests reintroduce direct core `GetStateString`
    - keep this first `GetStateString` route change cheap to verify

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetStateString` active-test de-emphasis as delivered
    - move the next route to residual runtime classification vs. `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: RED
  - summary:
    - the initial contract expected an exact `Result := LConnInfo.GetStateString;` token, but the integration helper used a semicolon-free `if/else` form
    - relaxed the check to the real source shape before re-running

- `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: PASS
  - summary:
    - new `GetStateString` active-test contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: PASS
  - summary:
    - active generic/integration tests now prefer `ISSLConnectionInfo.GetStateString`
    - direct core `GetStateString` no longer appears in the guarded ordinary test paths

- first run of `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
  - result: RED
  - summary:
    - compile/run exposed a pre-existing companion drift in `tests/connection/test_connection_basic.pas`
    - the file still treated `GetNativeHandle` as core `ISSLConnection` surface and used `FillChar` to build `TSSLConfig`, which triggered `LogLevel is library-scoped` at runtime

- update `tests/connection/test_connection_basic.pas`
  - change:
    - switch the native-handle check to `ISSLNativeHandleAccess`
    - replace `FillChar` config initialization with `CreateDefaultConfig(sslCtxClient)` so the test follows the current factory/config truth

- `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
  - result: PASS
  - summary:
    - generic connection smoke suite finished `11 passed, 0 failed`
    - the state-string path now goes through `ISSLConnectionInfo`, and the same file no longer drifts on native-handle/config initialization truth

- `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
  - result: PASS
  - summary:
    - integration suite compiled successfully and finished green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetStateString active-test de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetStateString` active-test proof after the final planning-file sync
    - no extra compile rerun was needed because only planning files changed after the focused tests passed

### GetStateString Residual Classification Freeze

- `rg -n "\\.GetStateString\\b|GetStateString\\(" tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetStateString`
    - confirmed the remaining direct-core residuals had shrunk to backend-contract mirror proof plus OpenSSL/WolfSSL backend-specific runtime tests

- add `docs/plans/2026-05-18-getstatestring-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetStateString` no longer requires repeated residual-hit archaeology

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetStateString`
    - spell out the current residual direct-core surface at the shared base-class comment level

- add `tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetStateString` direct-core residual file set into a cheap allowlist contract
    - fail if ordinary docs/tests or new files reintroduce direct core `GetStateString`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger `GetStateString` wording vs. `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the initial comment-pattern check was too strict for the wrapped base-class comment layout
    - relaxed the residual-note matching to the real multiline source shape before re-running

- second run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the allowlist count initially expected 8 direct core hits, but the real residual set is 9 including the contract mirror-proof hit
    - corrected the expected residual count before the final re-run

- `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetStateString` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetStateString` residual direct-core surface now matches the expected allowlist
    - ordinary docs/tests no longer reintroduce direct core `GetStateString`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetStateString residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetStateString` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetSelectedALPNProtocol Active Test De-emphasis

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b' tests --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed the highest-value remaining ordinary `GetSelectedALPNProtocol` usage lived in the real HTTPS integration suite and the cross-backend consistency contract
    - confirmed the next batch could stay on active-test de-emphasis without reopening backend-specific runtime ALPN files

- add `docs/plans/2026-05-18-getselectedalpn-active-test-deemphasis.md`
  - purpose:
    - define a bounded `GetSelectedALPNProtocol` batch that moves ordinary integration/contract tests off the core getter before touching backend-specific runtime files

- add `tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - purpose:
    - fail if ordinary integration/contract tests reintroduce direct core `GetSelectedALPNProtocol`
    - keep this first ALPN route change cheap to verify

- first run of `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: RED
  - summary:
    - the new contract correctly caught the first residual ordinary-path use in `tests/integration/test_real_https_connection.pas`
    - this confirmed the batch boundary before any Pascal edits landed

- update:
  - `tests/integration/test_real_https_connection.pas`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - change:
    - add `ISSLConnectionInfo`-first ALPN helpers
    - replace direct core `GetSelectedALPNProtocol` reads in the guarded ordinary integration/contract paths

- `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: PASS
  - summary:
    - new `GetSelectedALPNProtocol` active-test contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: PASS
  - summary:
    - ordinary integration/contract tests now prefer `ISSLConnectionInfo.GetSelectedALPNProtocol`
    - direct core `GetSelectedALPNProtocol` no longer appears in the guarded test paths

- `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
  - result: PASS
  - summary:
    - integration suite compiled successfully and finished green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
  - result: PASS
  - summary:
    - cross-backend consistency contract compiled successfully and stayed green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b' tests --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed the remaining direct-core ALPN surface had shrunk to backend contract mirror proof plus MbedTLS/WinSSL backend-specific runtime files

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetSelectedALPNProtocol` active-test de-emphasis as delivered
    - move the next route decision to residual runtime classification vs. stronger client-owner wording

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetSelectedALPNProtocol active-test de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetSelectedALPNProtocol` active-test proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the focused tests passed

### GetSelectedALPNProtocol Residual Classification Freeze

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b|GetSelectedALPNProtocol\(' tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetSelectedALPNProtocol`
    - confirmed the remaining direct-core residuals had shrunk to backend-contract mirror proof plus MbedTLS/WinSSL backend-specific runtime ALPN files

- add `docs/plans/2026-05-18-getselectedalpn-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetSelectedALPNProtocol` no longer requires repeated residual-hit archaeology

- add `tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetSelectedALPNProtocol` direct-core residual file set into a cheap allowlist contract
    - fail if ordinary docs/tests or new files reintroduce direct core `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the new residual contract correctly caught the missing source-level preferred-access note in `src/fafafa.ssl.base.pas`
    - this confirmed the batch still had real source-facing truth drift before comment updates

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetSelectedALPNProtocol`
    - spell out the current residual direct-core surface at the shared base-class comment level

- `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetSelectedALPNProtocol` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetSelectedALPNProtocol` residual direct-core surface now matches the expected allowlist
    - ordinary docs/tests no longer reintroduce direct core `GetSelectedALPNProtocol`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger `GetSelectedALPNProtocol` wording vs. `GetConnectionInfo`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetSelectedALPNProtocol residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetSelectedALPNProtocol` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetConnectionInfo Residual Classification Freeze

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetConnectionInfo\b|GetConnectionInfo\(' tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed active docs and ordinary tests no longer used direct core `GetConnectionInfo`
    - confirmed the remaining direct-core residuals were already limited to backend-contract mirror proof plus OpenSSL/WinSSL backend-specific files

- add `docs/plans/2026-05-18-getconnectioninfo-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetConnectionInfo` no longer requires repeated residual-hit archaeology

- add `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetConnectionInfo` direct-core residual file set into a cheap allowlist contract
    - fail if active docs/tests or new files reintroduce direct core `GetConnectionInfo`

- first run of `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the new residual contract correctly caught the missing source-level preferred-access note in `src/fafafa.ssl.base.pas`
    - this confirmed the batch still had real source-facing truth drift before comment updates

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetConnectionInfo`
    - spell out the current residual direct-core surface at the shared base-class comment level

- `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetConnectionInfo` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetConnectionInfo` residual direct-core surface now matches the expected allowlist
    - active docs/tests no longer reintroduce direct core `GetConnectionInfo`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger wording vs. backend implementation-completeness review

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetConnectionInfo` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetConnectionInfo Base Enrichment From Residual Audit

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - add shared `DoGetConnectionInfoServerName` hook on `TBaseSSLConnection`
    - enrich shared `GetConnectionInfo` with `ServerName`
    - enrich shared `GetConnectionInfo` with `SessionId` when connected/handshake-complete and session metadata is available
    - extend the focused hostname-precedence mock test with `ConnectionInfo.ServerName` / `ConnectionInfo.SessionId` behavior coverage
    - narrow the active API wording from “all fields are fully populated” to shared-minimum + best-effort backend detail truth

- implementation note:
  - the final shared-layer design intentionally avoids `Supports(Self, ISSLClientConnection, ...)` inside `TBaseSSLConnection.GetConnectionInfo`
  - summary:
    - a prior attempt had already shown that the naive self-cast route could destabilize OpenSSL fresh-connection access
    - the landed design uses backend overrides of `DoGetConnectionInfoServerName` instead, which is safe for direct concrete-object test construction paths

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `13 passed, 0 failed`
    - new `ConnectionInfo.ServerName` and `ConnectionInfo.SessionId` checks both stayed green

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info cipher guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` no longer reproduced the prior `EAccessViolation`
    - shared `ServerName` enrichment preserved the cipher-guard baseline fields

- update `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - change:
    - raise the expected direct-core hit count from `7` to `9`
    - add `tests/test_connection_builder_hostname_precedence.pas` to the intentional direct-core allowlist
    - rationale:
      - the new mock test intentionally reads core `Conn.GetConnectionInfo` to verify shared-layer mirror truth

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - updated residual contract now matches the expanded intentional allowlist
    - no unexpected direct-core `GetConnectionInfo` files were introduced

### GetConnectionInfo PeerCertificate Base Enrichment

- add `docs/plans/2026-05-18-getconnectioninfo-peercertificate-base-enrichment.md`
  - purpose:
    - define the next bounded implementation-completeness batch after `ServerName` / `SessionId`
    - keep scope on the shared `PeerCertificate` field instead of prematurely diving into backend-specific cipher detail mapping

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - enrich shared `GetConnectionInfo` with `PeerCertificate` when `GetPeerCertificate` returns a current certificate
    - extend the focused mock contract so the existing shared `GetConnectionInfo` read also proves `PeerCertificate.Subject` / `Issuer` mirror truth
    - narrow the active API wording so `PeerCertificate` is now documented as a shared-layer field when the connection can expose the current peer certificate

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `15 passed, 0 failed`
    - the existing `GetConnectionInfo` proof still covered `ServerName` / `SessionId`
    - the same intentional direct-core read now also proved `PeerCertificate.Subject` / `Issuer` mirror truth without expanding the residual allowlist

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` remained safe after the new shared `GetPeerCertificate` path was introduced

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface stayed unchanged at the current allowlist
    - no new residual archaeology was needed for this batch

### GetConnectionInfo Crypto Detail Name-Derived First Slice

- add `docs/plans/2026-05-18-getconnectioninfo-crypto-detail-name-derived-first-slice.md`
  - purpose:
    - define the first bounded shared-crypto-detail batch after `PeerCertificate`
    - keep scope on name-derived `Cipher` / `Hash` / `KeySize` normalization instead of reopening backend-specific ID/MAC detail

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - add a shared cipher-suite-name normalization helper for `GetConnectionInfo`
    - derive `Cipher`, `Hash`, and `KeySize` from the negotiated cipher-suite name
    - derive `KeyExchange` when the cipher-suite name still carries a legacy prefix such as `ECDHE-RSA`
    - update the focused mock proof to use a real parseable suite name: `ECDHE-RSA-AES128-GCM-SHA256`
    - narrow the active API wording so these fields are now documented as shared best-effort derivations when the backend already exposes a stable cipher-suite name

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `19 passed, 0 failed`
    - the existing intentional direct-core `GetConnectionInfo` proof still covered `ServerName`, `SessionId`, and `PeerCertificate`
    - the same read now also proved shared name-derived `KeyExchange`, `Cipher`, `Hash`, and `KeySize` truth

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` remained safe after the new shared cipher-suite-name parser was introduced

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface stayed unchanged at the current allowlist
    - this batch did not require any new residual file or hit-count changes

### GetConnectionInfo CipherSuiteId First Slice

- add `docs/plans/2026-05-18-getconnectioninfo-ciphersuiteid-first-slice.md`
  - purpose:
    - define the next bounded `GetConnectionInfo` completeness batch after the shared name-derived crypto-detail slice
    - keep scope on `CipherSuiteId` instead of reopening `MacSize` or broader backend runtime refactors

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.openssl.api.ssl.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - keep the shared TLS 1.3 standard-name derivation for `CipherSuiteId`
    - export and load `SSL_CIPHER_get_protocol_id` from the active OpenSSL SSL API unit
    - let `TOpenSSLConnection.GetConnectionInfo` prefer `SSL_CIPHER_get_protocol_id` and fall back to `SSL_CIPHER_get_id and $FFFF`
    - extend the focused OpenSSL contract with explicit `CipherSuiteId` truth checks
    - sync the residual allowlist count after adding one more intentional direct-core `GetConnectionInfo` proof site

- error encountered:
  - the carry-over uncommitted implementation initially did not compile because `SSL_CIPHER_get_protocol_id` was not exported from the active `fafafa.ssl.openssl.api.ssl` loader path
  - resolution:
    - add the missing type / var / nil-reset / loader assignment in `src/fafafa.ssl.openssl.api.ssl.pas`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `21 passed, 0 failed`
    - the existing intentional direct-core `GetConnectionInfo` proof still covered `ServerName`, `SessionId`, `PeerCertificate`, and legacy `KeyExchange`
    - the same read now also proved shared TLS 1.3 `CipherSuiteId` truth on `TLS_AES_128_GCM_SHA256`

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - first result:
    - compile became green after the loader/export fix
    - runtime contract exposed one new failure:
      - `GetConnectionInfo when SSL_CIPHER_get_name is unavailable should not raise`
      - `EAccessViolation: Access violation`
  - diagnosis:
    - the old guard uses a fake non-nil cipher pointer to model “current cipher exists but helpers are unavailable”
    - once `CipherSuiteId` low-level helpers were added, leaving real `SSL_CIPHER_get_protocol_id` assigned made the test exercise an invalid-pointer artifact instead of a real product path
  - follow-up fix:
    - extend the degrade branch to nil both `SSL_CIPHER_get_protocol_id` and `SSL_CIPHER_get_id`
    - add a separate truth contract that proves:
      - `SSL_CIPHER_get_protocol_id` is preferred
      - `SSL_CIPHER_get_id` low word is the fallback
  - final result: PASS
  - summary:
    - focused OpenSSL connection-info suite finished `14 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` still degrades safely when cipher helpers are unavailable
    - low-level `CipherSuiteId` backfill now has explicit contract coverage

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - first result:
    - FAIL: expected exactly `9` direct core `GetConnectionInfo` test hits, found `10`
  - resolution:
    - update the expected count to `10`
    - rationale:
      - the new OpenSSL focused `CipherSuiteId` truth proof intentionally adds one direct-core `GetConnectionInfo` site in an already-allowlisted test file
  - final result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface remains controlled
    - this batch did require a small allowlist count sync, but no new residual file family

- `git diff --check`
  - result: PASS
  - summary:
    - current batch has no whitespace or patch-format issues

### GetConnectionInfo Contract Owner Primacy

- add `docs/plans/2026-05-18-getconnectioninfo-contract-owner-primacy.md`
  - purpose:
    - close the stale residual-allowlist drift on the `GetConnectionInfo` route
    - turn `Contract 19` into explicit `ISSLConnectionInfo` owner primacy instead of implicit dual-owner comparison

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED
  - summary:
    - stale source contract still expected exactly `10` direct core `GetConnectionInfo` test hits
    - live repo truth had already drifted to `15` hits across:
      - shared builder proof
      - OpenSSL / WolfSSL / MbedTLS completeness proof
      - FreePascal completion proof
    - this confirmed a real route/workflow gap instead of just a missing note

- implementation:
  - `tests/contract/test_backend_contract.pas`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - change:
    - `Contract 19` now reads `ISSLConnectionInfo.GetConnectionInfo` first, then checks direct core `GetConnectionInfo` as a mirror
    - new shell contract freezes the owner-primacy wording
    - completeness / proof tests now read connection info through `ISSLConnectionInfo`
    - residual allowlist now shrinks to the true remaining direct-core files:
      - `tests/contract/test_backend_contract.pas`
      - `tests/winssl/test_winssl_connection_info.pas`
      - `tests/winssl/test_winssl_connection_edge_cases.pas`

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - backend contract now treats `ISSLConnectionInfo.GetConnectionInfo` as the primary owner

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED -> GREEN
  - summary:
    - after shrinking ordinary proof/test usage, residual direct-core surface now matches the expected `5`-hit allowlist

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared builder proof stayed green at `29 passed, 0 failed`
    - moving the proof to `ISSLConnectionInfo` did not change the shared truth already covered in this test

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Contract 19` stayed green on OpenSSL / WolfSSL / MbedTLS / FreePascal
    - overall backend contract result remained:
      - `Total Tests: 135`
      - `Passed: 111`
      - `Failed: 0`
      - `Skipped: 24`

- `mkdir -p tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_server_accept_skeleton -FEtmp/test_freepascal_server_accept_skeleton -otmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
  - result: PASS
  - summary:
    - FreePascal server completion proof remained green after switching to `ISSLConnectionInfo`

- `mkdir -p tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_session_resumption -FEtmp/test_freepascal_client_session_resumption -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - result: PASS
  - summary:
    - FreePascal session-resumption completion proof remained green after switching to `ISSLConnectionInfo`

- `mkdir -p tmp/test_mbedtls_connection_info_ciphersuite_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_info_ciphersuite_contract -FEtmp/test_mbedtls_connection_info_ciphersuite_contract -otmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract tests/test_mbedtls_connection_info_ciphersuite_contract.pas && ./tmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract`
  - result: PASS
  - summary:
    - MbedTLS completeness proof remained green at `15 passed, 0 failed`

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: RED -> GREEN
  - summary:
    - first owner-path conversion exposed a real test-lifetime bug:
      - concrete `TOpenSSLConnection` was still manually freed after `ISSLConnectionInfo` had taken over lifetime
      - test failed with `EInvalidPointer` / `EAccessViolation`
    - after switching the helper to interface-owned lifetime, final result returned to:
      - `20 passed, 0 failed`

- `mkdir -p tmp/test_wolfssl_connection_info_macsize_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_info_macsize_contract -FEtmp/test_wolfssl_connection_info_macsize_contract -otmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract tests/test_wolfssl_connection_info_macsize_contract.pas && ./tmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract`
  - result: RED -> GREEN
  - summary:
    - the same lifecycle pit also existed in the WolfSSL helper after the owner-path conversion
    - after aligning it to interface-owned lifetime, final result returned to:
      - `3 passed, 0 failed`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetConnectionInfo` contract owner primacy as delivered
    - move the default mainline to stronger wording / slimming discussion instead of more residual allowlist cleanup

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the owner-primacy closeout, the residual shrink, and the concrete-object/interface lifetime pit into persistent repo working memory

### GetConnectionInfo WinSSL Direct-Core Classification

- add `docs/plans/2026-05-18-getconnectioninfo-winssl-direct-core-classification.md`
  - purpose:
    - settle the last residual-classification question on the current `GetConnectionInfo` route
    - decide whether the remaining WinSSL direct-core files are intentional core-surface proof or stale owner-path drift

- static audit result:
  - `tests/winssl/test_winssl_connection_info.pas`
    intentionally verifies:
    - direct core `GetConnectionInfo`
    - direct core `GetProtocolVersion`
    - direct core `GetCipherName`
    - consistency between `GetConnectionInfo` and the individual core getters
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
    keeps one direct core `GetConnectionInfo` path inside the broader WinSSL edge-case suite
  - this means the remaining WinSSL residuals are core-surface proof, not ordinary completeness tests that were forgotten during the owner-path migration

- implementation:
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
  - change:
    - marked the two WinSSL residual files as `INTENTIONAL_CORE_SURFACE`
    - added a focused source guard that freezes the WinSSL residual file set and the classification marker

- `bash tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
  - result: PASS
  - summary:
    - the remaining WinSSL direct-core `GetConnectionInfo` files are explicitly classified and confined

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the global residual direct-core `GetConnectionInfo` allowlist stayed green after the WinSSL classification closeout

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark WinSSL residual classification as delivered
    - move the default mainline fully onto stronger owner / deprecation wording work

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the WinSSL residual classification closeout into persistent repo working memory

### FreePascal GetConnectionInfo Completion Audit

- add `docs/plans/2026-05-18-freepascal-getconnectioninfo-completion-audit.md`
  - purpose:
    - close the last open question on the current `GetConnectionInfo` implementation-completeness route
    - prove whether `FreePascal` still needs a backend-local low-level truth helper or can now close on shared TLS 1.3 truth

- update focused FreePascal runtime proofs:
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - change:
    - add explicit `GetConnectionInfo` assertions on the server skeleton path for:
      - `CipherSuiteId`
      - `KeySize`
      - `MacSize`
    - add explicit `GetConnectionInfo` assertions on the client initial/resumed paths for:
      - `ProtocolVersion`
      - `CipherSuiteId`
      - `KeySize`
      - `MacSize`
      - `ServerName`
      - `IsResumed`
      - `SessionId`

- add `tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
  - purpose:
    - fail if `FreePascal` grows a dedicated `GetConnectionInfo` override
    - guard that client/server TLS 1.3 paths still feed standard suite-name truth into shared `GetConnectionInfo`
    - guard that session/resumption state still carries `FCipherSuite: Word`

- `bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
  - result: PASS
  - summary:
    - confirmed `TFreePascalConnection` does not implement a dedicated `GetConnectionInfo` override
    - confirmed the active truth path still depends on:
      - `FCipherName := TLS13CipherSuiteToString(...)`
      - `FCipherSuite: Word`

- `mkdir -p tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_server_accept_skeleton -FEtmp/test_freepascal_server_accept_skeleton -otmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
  - result: PASS
  - summary:
    - server skeleton proof now covers:
      - `GetConnectionInfo.CipherSuiteId = TLS13_CIPHER_AES_128_GCM_SHA256`
      - `KeySize = 128`
      - `MacSize = 16`

- `mkdir -p tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_session_resumption -FEtmp/test_freepascal_client_session_resumption -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - result: PASS
  - summary:
    - initial and resumed client proofs now cover:
      - `ProtocolVersion = TLS 1.3`
      - `CipherSuiteId = TLS13_CIPHER_CHACHA20_POLY1305_SHA256`
      - `KeySize = 256`
      - `MacSize = 16`
      - `ServerName = 'example.com'`
      - `IsResumed` false/true truth
      - `SessionId` mirror truth

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `FreePascal` completion audit as delivered
    - move the default mainline from backend-helper hunting back to owner / deprecation wording route

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the `FreePascal` completion-audit conclusion into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - current FreePascal completion-audit batch has no whitespace or patch-format issues

### WolfSSL GetConnectionInfo Legacy MacSize Truth

- add `docs/plans/2026-05-18-wolfssl-connectioninfo-macsize-legacy-truth-feasibility.md`
  - purpose:
    - capture the next bounded `MacSize` batch after OpenSSL legacy truth landed
    - keep scope on WolfSSL low-level HMAC truth instead of reopening shared parser guesses

- implementation:
  - `src/fafafa.ssl.wolfssl.api.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`
  - `tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
  - change:
    - active WolfSSL API export/binding chain now includes:
      - `wolfSSL_GetHmacSize`
    - WolfSSL `GetConnectionInfo` now fills `MacSize` from HMAC truth only when:
      - shared path still leaves `MacSize = 0`
    - AEAD `MacSize` remains owned by the shared suite-name derivation path

- `bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the new WolfSSL API export chain and the guarded HMAC-truth `MacSize` write path

- `mkdir -p tmp/test_wolfssl_connection_info_macsize_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_info_macsize_contract -FEtmp/test_wolfssl_connection_info_macsize_contract -otmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract tests/test_wolfssl_connection_info_macsize_contract.pas && ./tmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a focused contract harness precondition:
      - optional WolfSSL backend tests must define `ENABLE_WOLFSSL`
      - and must pull in `fafafa.ssl.wolfssl.lib` so factory registration is active
    - after aligning the test harness, final result was:
      - `3 passed, 0 failed`
    - the suite now explicitly proves:
      - helper unavailable safe degrade
      - legacy non-AEAD HMAC truth -> `MacSize = 32`
      - AEAD HMAC truth does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the WolfSSL legacy `MacSize` addition did not disturb the earlier shared AEAD semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL legacy `MacSize` batch has no whitespace or patch-format issues

### MbedTLS GetConnectionInfo Ciphersuite Truth

- add `docs/plans/2026-05-18-mbedtls-connectioninfo-ciphersuite-truth-feasibility.md`
  - purpose:
    - capture the MbedTLS batch that finishes the remaining high-value backend truth source on the current `GetConnectionInfo` route
    - keep scope on ciphersuite-info runtime truth and a blocking MD-constant correction

- implementation:
  - `src/fafafa.ssl.mbedtls.base.pas`
  - `src/fafafa.ssl.mbedtls.api.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
  - `tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
  - change:
    - fixed `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` constant truth
    - active MbedTLS API export/binding chain now includes:
      - `mbedtls_ssl_get_ciphersuite_id`
      - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
      - `mbedtls_ssl_ciphersuite_from_id`
      - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
    - MbedTLS `GetConnectionInfo` now fills:
      - direct or fallback `CipherSuiteId`
      - `KeySize` from ciphersuite info
      - legacy/non-AEAD `MacSize` from digest truth only when shared AEAD truth still leaves `MacSize = 0`
    - shared cipher-suite parser now recognizes MbedTLS-style hyphenated AES / TLS-RSA names

- `bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the corrected MD constants
    - verified the new MbedTLS ciphersuite-info export chain
    - verified the runtime write path for `CipherSuiteId` / `KeySize` / `MacSize`
    - verified the shared hyphenated-name compatibility guard

- `mkdir -p tmp/test_mbedtls_connection_info_ciphersuite_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_info_ciphersuite_contract -FEtmp/test_mbedtls_connection_info_ciphersuite_contract -otmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract tests/test_mbedtls_connection_info_ciphersuite_contract.pas && ./tmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a real shared baseline gap:
      - MbedTLS-style hyphenated AES suite names were not fully parsed by the shared cipher-suite derivation path
    - after aligning the shared parser, final result was:
      - `15 passed, 0 failed`
    - the suite now explicitly proves:
      - corrected runtime SHA1 constant truth against canonical SHA1(`abc`)
      - helper unavailable safe degrade
      - direct ciphersuite-id truth
      - name-based ciphersuite-id fallback
      - legacy non-AEAD digest truth -> `MacSize = 32` / `20`
      - AEAD digest truth does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the MbedTLS truth additions and shared hyphenated-name support did not regress earlier semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS ciphersuite-truth batch has no whitespace or patch-format issues

### GetConnectionInfo MacSize Semantics Matrix

- add `docs/plans/2026-05-18-getconnectioninfo-macsize-semantics-matrix.md`
  - purpose:
    - capture the bounded follow-up after the WinSSL cipher-truth correction
    - turn `MacSize` from an ambiguous one-backend field into a reusable shared/backend matrix with a clear next-step boundary

- implementation:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - shared connection-info derivation now fills AEAD `MacSize` from recognized suite names
    - `GCM` / `POLY1305` / `OCB` / `CCM` map to `16`
    - `CCM_8` maps to `8`
    - WinSSL `GetConnectionInfo` now starts from inherited shared truth
    - WinSSL only falls back to `ConnInfo.dwHashStrength div 8` when shared derivation still leaves `MacSize = 0`
    - focused mock proof now explicitly checks:
      - TLS 1.3 AEAD suite -> `MacSize = 16`
      - legacy GCM suite -> `MacSize = 16`
      - legacy non-AEAD suite -> `MacSize = 0`

- `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier WinSSL cipher-suite truth correction still holds after the new `MacSize` batch

- `bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
  - result: PASS
  - summary:
    - verified WinSSL now starts from inherited shared connection-info truth
    - verified `dwHashStrength div 8` is guarded behind a missing shared `MacSize`
    - verified shared source contains the new AEAD-first `MacSize` derivation rules

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/connection-info suite finished `26 passed, 0 failed`
    - the shared `MacSize` derivation is now covered on:
      - TLS 1.3 GCM
      - legacy GCM
      - legacy non-AEAD no-guess behavior

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info suite remained green at `14 passed, 0 failed`
    - the new shared `MacSize` derivation did not regress the existing safe-degrade or `CipherSuiteId` truth coverage

- `git diff --check`
  - result: PASS
  - summary:
    - current `MacSize` semantics batch has no whitespace or patch-format issues

### OpenSSL GetConnectionInfo Legacy MacSize Truth

- add `docs/plans/2026-05-18-openssl-connectioninfo-macsize-legacy-truth-feasibility.md`
  - purpose:
    - capture the next bounded `MacSize` batch after the shared AEAD semantics matrix
    - keep the scope on OpenSSL low-level truth instead of spreading legacy `MacSize` guesses into the shared parser

- implementation:
  - `src/fafafa.ssl.openssl.api.ssl.pas`
  - `src/fafafa.ssl.openssl.api.evp.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
  - change:
    - active SSL API export/binding chain now includes:
      - `SSL_CIPHER_is_aead`
      - `SSL_CIPHER_get_digest_nid`
    - active EVP export/binding chain now includes:
      - `EVP_get_digestbynid`
    - OpenSSL `GetConnectionInfo` now fills `MacSize` from digest truth only when:
      - shared path still leaves `MacSize = 0`
      - current cipher is explicitly non-AEAD
    - AEAD `MacSize` remains owned by the shared suite-name derivation path

- `bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the new SSL/EVP export chain and the OpenSSL digest-truth `MacSize` write path

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a stale contract assumption:
      - old fake-cipher-pointer scenarios only nulled `protocol_id` / `get_id`
      - after the new `MacSize` path landed, `is_aead` / `digest_nid` / `EVP_get_digestbynid` also had to be nulled in those fake-pointer branches
    - after aligning the contract, final result was:
      - `20 passed, 0 failed`
    - the expanded suite now explicitly proves:
      - helper unavailable safe degrade
      - legacy non-AEAD digest truth -> `MacSize = 32`
      - AEAD digest size does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the OpenSSL legacy `MacSize` addition did not disturb the earlier shared AEAD semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current OpenSSL legacy `MacSize` batch has no whitespace or patch-format issues

### WinSSL GetConnectionInfo Cipher Truth Correction

- add `docs/plans/2026-05-18-winssl-connectioninfo-cipher-truth-correction.md`
  - purpose:
    - capture the WinSSL truth-correction batch that was discovered while auditing `MacSize`
    - keep scope on the deterministic `CipherSuiteId` source bug before reopening broader field-completeness work

- static audit result:
  - `SecPkgContext_ConnectionInfo.aiCipher` in `src/fafafa.ssl.winssl.base.pas` is explicitly documented as an encryption algorithm ID
  - the same WinSSL unit uses it to derive algorithm-level cipher names and enums
  - therefore the previous `Result.CipherSuiteId := Word(ConnInfo.aiCipher)` path was a wrong truth source, not a benign best-effort approximation

- implementation:
  - `src/fafafa.ssl.winssl.base.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - add `SECPKG_ATTR_CIPHER_INFO`
    - add a minimal WinSSL cipher-info helper that reads Schannel `dwCipherSuite`
    - let WinSSL `GetConnectionInfo` stop writing `CipherSuiteId` from `ConnInfo.aiCipher`
    - let WinSSL `DoGetCipherName` prefer the real suite name when Schannel exposes it
    - narrow the active docs so `MacSize` is explicitly described as still best-effort and not guaranteed to equal the AEAD auth tag length

- `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
  - result: PASS
  - summary:
    - verified `SECPKG_ATTR_CIPHER_INFO` is defined
    - verified WinSSL now queries `SECPKG_ATTR_CIPHER_INFO`
    - verified the old `ConnInfo.aiCipher -> CipherSuiteId` write is gone

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite remained green at `21 passed, 0 failed`
    - the WinSSL correction did not disturb the shared `GetConnectionInfo` truth already established on other backends

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info suite remained green at `14 passed, 0 failed`
    - the WinSSL correction did not regress the OpenSSL `CipherSuiteId` truth or safe-degrade guard

- `gh auth status`
  - result: PASS
  - summary:
    - GitHub CLI is installed
    - authenticated account has `workflow` scope
    - Windows gate can be dispatched from this environment after the batch lands

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=\"winssl_cipher_truth_20260518_152020\" -f strict_closure=false`
  - result: PASS
  - summary:
    - dispatched GitHub workflow `Wave B B2 Manual Gate (Template)` against pushed commit `dcde2ff`

- `gh run watch 26019296095`
  - result: PASS
  - summary:
    - workflow `https://github.com/dtamade/fafafa.ssl/actions/runs/26019296095` finished `success`
    - `windows-gate` finished `success`
    - `Run quick WinSSL smoke` finished `success`
    - `Run Windows Wave B gate` finished `success`
    - `Run broader WinSSL runtime suite` finished `success`
    - `linux-gate`, `macos-gate`, and final `summary` job also finished `success`

- `git diff --check`
  - result: PASS
  - summary:
    - current batch has no whitespace or patch-format issues

### GetConnectionInfo Public Wording De-emphasis

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - script produced no recovery output
    - there was no extra unsynced session context before the wording batch

- `sed -n '1198,1220p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access wording
    - but it still lacked a stronger owner/de-emphasis note for `ISSLConnection.GetConnectionInfo`

- `sed -n '430,590p' docs/reference/API_REFERENCE.md`
  - result: PASS
  - summary:
    - active API reference still declared `GetConnectionInfo` in the core interface without an inline de-emphasis marker
    - the connection-info example still taught `LConn.GetProtocolVersion` / `LConn.GetCipherName` right beside `ISSLConnectionInfo.GetConnectionInfo`

- `sed -n '90,180p' docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - migration example still said `LConn.GetConnectionInfo;  // 仍然存在`
    - this was weaker than the current owner/mirror truth already established elsewhere

- add `docs/plans/2026-05-18-getconnectioninfo-public-wording-deemphasis.md`
  - purpose:
    - capture the bounded source/doc wording batch after owner-primacy and WinSSL residual classification were already closed
    - keep scope on public-facing truth instead of reopening runtime or backend implementation work

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - add an explicit owner-note plus a stronger compatibility note to `ISSLConnection.GetConnectionInfo`
    - mark the active API declaration as compatibility-only and move the connection-info example to `LInfo.ProtocolVersion` / `LInfo.CipherSuite`
    - strengthen the v2 migration wording so `GetConnectionInfo` is explicitly treated as a compatibility mirror rather than merely "still exists"

- `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - result: PASS
  - summary:
    - source comment, active API docs, and v2 migration doc all carry the stronger owner/de-emphasis wording
    - stale `LConn.GetConnectionInfo;  // 仍然存在` wording is gone

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo public wording de-emphasis` batch has no whitespace or patch-format issues

### GetConnectionInfo Compiler Deprecation Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch started from clean `master...origin/master`
    - the new batch could be scoped on top of the already-pushed public-wording closeout

- `rg -n "deprecated '|'deprecated;|SYMBOL_DEPRECATED|WARN 6058|WithSNI|SetServerName\\(|GetConnectionInfo" src tests docs/reference docs/plans task_plan.md findings.md progress.md --glob '!docs/archive/**'`
  - result: PASS
  - summary:
    - existing `.WithSNI(...)` and direct-context `ServerName` work already showed a stable compiler-deprecation + local-warning-quarantine pattern
    - `GetConnectionInfo` residual direct-core use was now small enough to evaluate the same route safely

- `sed -n '1188,1222p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access, owner-note, and stronger compatibility wording
    - but the declaration itself still was not compiler deprecated yet

- `rg -n '\\b(?:Conn|LConn|LConnection)\\.GetConnectionInfo\\b|\\.GetConnectionInfo\\(' tests src docs --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - production source did not show new direct-core `GetConnectionInfo` callers
    - the remaining direct-core residual set stayed confined to backend contract and WinSSL intentional core-surface tests

- add `docs/plans/2026-05-18-getconnectioninfo-compiler-deprecation-alignment.md`
  - purpose:
    - capture the bounded source-truth batch that upgrades `ISSLConnection.GetConnectionInfo` from source/doc de-emphasis to compiler-level deprecation
    - keep runtime behavior unchanged while aligning the public core mirror surface with current owner truth

- add `tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
  - purpose:
    - fail if the core `GetConnectionInfo` declaration loses its compiler `deprecated` marker
    - guard the new doc wording and intentional warning-quarantine boundary

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/contract/test_backend_contract.pas`
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - change:
    - mark `ISSLConnection.GetConnectionInfo` as compiler `deprecated 'Use ISSLConnectionInfo.GetConnectionInfo'`
    - upgrade active docs to say the core getter is now compiler deprecated
    - add local warning suppression around the remaining intentional direct-core `GetConnectionInfo` callsites

- `bash tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the core declaration is compiler deprecated
    - active docs and intentional residual tests all match the expected source-truth boundary

- `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - result: PASS
  - summary:
    - the earlier wording contract stayed green after the compiler-deprecation upgrade
    - source/doc de-emphasis and compiler deprecation now tell the same story

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - residual direct-core `GetConnectionInfo` surface stayed confined to the existing allowlist
    - compiler deprecation did not re-expand direct-core usage

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: RED -> GREEN
  - summary:
    - first compile failed with `test_backend_contract.pas(... ) Fatal: Syntax error, ";" expected but "ELSE" found`
    - root cause was a stray semicolon before the fallback `else` branch in the new session-resumption mirror-proof restructuring
    - after removing that stray semicolon, the focused backend contract finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - intentional direct-core mirror proof stayed green after local deprecation-warning quarantine

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo` compiler-deprecation batch has no whitespace or patch-format issues
