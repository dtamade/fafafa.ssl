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
