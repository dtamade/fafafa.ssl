# Context Builder Server ServerName Runtime Consistency Plan

**Goal:** Restore `TSSLContextBuilder.BuildServer` so an explicit `.WithSNI(...)` is not silently dropped on the server build path. Keep this as a narrow runtime-consistency fix between builder client/server paths and the existing factory config path.

**Architecture:** Add a focused FreePascal regression because it is Linux-safe and already inherits context `ServerName` into newly created connections. Lock one passing control case for `BuildClient.WithSNI(...)`, then a failing builder server case that shows the same builder field is lost on `BuildServer`. Apply the smallest possible fix in `src/fafafa.ssl.context.builder.pas`: when `FServerName <> ''`, call `Result.SetServerName(FServerName)` in `BuildServer`, matching `BuildClient`.

**Tech Stack:** Free Pascal, no network handshake required

## Files
- Add: `tests/test_context_builder_server_servername_runtime_consistency.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`

## Steps
1. RED
   - Add a focused FreePascal regression:
     - `BuildClient.WithSNI(...)` remains a passing control
     - builder server with `.WithSNI(...)` should preserve `Ctx.GetServerName` and inherited connection `GetServerName`
   - Run the test and confirm the builder server case fails.

2. GREEN
   - Update `TSSLContextBuilderImpl.BuildServer` to apply `Result.SetServerName(FServerName)` when configured.
   - Preserve existing validation warning behavior and all certificate/key logic.

3. Regression
   - Re-run the new focused test.
   - Re-run adjacent server-name regressions:
     - `tests/test_factory_config_server_name_isolation.pas`
     - `tests/test_freepascal_context_server_name_inheritance.pas`
     - `tests/test_connection_builder_hostname_precedence.pas`
     - `tests/test_tls_connector_hostname_override_precedence.pas`

4. Planning writeback
   - Update `task_plan.md`, `findings.md`, and `progress.md`.

## Expected Output
- RED: builder server case fails because `BuildServer` currently never applies `FServerName`.
- GREEN: focused test and adjacent server-name regressions pass.
