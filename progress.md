# Progress - CI Runtime Gate Repair

## 2026-05-15

### Context Recovery

- `git status --short --branch`
  - result: `## master...origin/master` with only local repair changes / generated reports in progress
- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: no output

### Remote Failure Revalidation

- `gh run view 25893971783 --json databaseId,displayTitle,headSha,conclusion,jobs`
  - result: PASS
  - summary:
    - run=`25893971783`
    - head=`2eb563f`
    - `Minimal Gate (Linux)` PASS
    - `FreePascal TLS 1.3 Completeness` FAIL
    - `Code Quality (Light)` PASS

- `gh run view 25893971783 --log-failed | tail -n 80`
  - result: PASS
  - summary:
    - failure lands in `WolfSSL KnownIssues 运行时对齐测试`
    - key error: `Failed to initialize WolfSSL library ... Failed to load WolfSSL library: libwolfssl.so`

- `gh run view 25901035350 --json databaseId,displayTitle,headSha,conclusion,jobs`
  - result: PASS
  - summary:
    - run=`25901035350`
    - head=`2eb563f`
    - `tls13-signer-gate` job failed in bundle step + append-step-summary step

- `gh run view 25901035350 --log-failed | tail -n 120`
  - result: PASS
  - summary:
    - bundle step shows `signer_gate_ci exit=1`
    - bundle report ends `overall=FAIL overall_state=ATTENTION`
    - summary step shows broken here-doc terminator and `syntax error: unexpected end of file`

### RED Contracts Before Fix

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: FAIL
  - summary: `ci.yml completeness workflow must install libwolfssl-dev`

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: FAIL
  - summary: `release.yml installs WolfSSL runtime dependencies for completeness coverage`

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: FAIL
  - summary: extracted append-step-summary shell did not parse cleanly because `PY` terminator was indented

- `bash tests/scripts/test_tls13_servercertverify_bench_contract.sh`
  - result: FAIL
  - summary: bench script still forced `-Criot` / hid compile diagnostics

### Production Fixes Applied

- update `.github/workflows/ci.yml`
  - change: completeness job install line now includes `libwolfssl-dev`
- update `.github/workflows/release.yml`
  - change: release workflow install line now includes `libwolfssl-dev`
- update `.github/workflows/release.yml.disabled`
  - change: disabled release template kept in sync with active workflow
- update `.github/workflows/tls13-signer-gate.yml`
  - change: heredoc terminator `PY` is flush-left in the extracted shell script
- update `scripts/run_freepascal_tls13_servercertverify_bench.sh`
  - change: remove `-Criot`
  - change: stop redirecting compile output to `/dev/null`

### Local Revalidation After Fix

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: PASS

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_servercertverify_bench_contract.sh`
  - result: PASS

- `bash scripts/run_freepascal_tls13_servercertverify_bench.sh`
  - result: PASS
  - metrics:
    - `CRT_avg_ms=120.1000`
    - `D_avg_ms=567.1000`
    - `Speedup_D_over_CRT=4.72x`

- `bash scripts/run_tls13_signer_gate_ci.sh`
  - result: PASS
  - run_id: `20260515_131250`

- `bash scripts/run_tls13_signer_gate_bundle.sh --run-id local_bundle_repair_20260515 --reports-dir test-reports --strict`
  - result: PASS
  - summary: `overall=PASS overall_state=HEALTHY`

- `git diff --check`
  - result: PASS

### Fourth-Order Remote Revalidation

- `gh run watch 25902644127 --exit-status`
  - result: FAIL
  - summary:
    - `Minimal Gate (Linux)` PASS
    - `Code Quality (Light)` PASS
    - `FreePascal TLS 1.3 Completeness` FAIL in 2m28s

- `gh run view 25902644127 --json databaseId,displayTitle,headSha,conclusion,jobs,url`
  - result: PASS
  - summary:
    - run=`25902644127`
    - head=`8d052dd`
    - only job failure is `FreePascal TLS 1.3 Completeness`

- `gh run view 25902644127 --log-failed | tail -n 260`
  - result: PASS
  - summary:
    - `WolfSSL KnownIssues runtime alignment` now passes on GitHub runner
    - failure has moved to `MbedTLS KnownIssues runtime alignment`
    - key error: `Failed to initialize MbedTLS library (LastError=-1, Details=Failed to load MbedTLS libraries)`

### Fourth-Order RED Contracts

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result before fourth fix: FAIL
  - summary:
    - completeness job install step still lacked `libmbedtls-dev`

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result before fourth fix: FAIL
  - summary:
    - release workflows still lacked `libmbedtls-dev`

### Fourth-Order Repairs

- update `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - change: completeness job install-step contract now also requires `libmbedtls-dev`

- update `tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - change: release workflow contract now also requires `libmbedtls-dev`

- update `.github/workflows/ci.yml`
  - change: `freepascal-tls13-completeness` install step now includes `libmbedtls-dev`
  - note: the first attempt accidentally hit `minimal-gate-linux`; the strengthened job-local contract caught the mis-target and the final patch was narrowed to the completeness job

- update `.github/workflows/release.yml`
  - change: install step now includes `libmbedtls-dev`

- update `.github/workflows/release.yml.disabled`
  - change: disabled release template kept in sync with the active workflow

### Local Revalidation After Fourth Fix

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: PASS

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - result: PASS

- `git diff --check`
  - result: PASS

### First Push Revalidation

- `git commit -m "fix: repair ci runtime gate blockers"`
  - result: PASS
  - commit: `d3ebeee`

- `git push origin master`
  - result: PASS
  - remote update: `2eb563f..d3ebeee`

- `gh run list --branch master --limit 8 --json ...`
  - result: PASS
  - summary:
    - signer run=`25901775672`
    - ci run=`25901775676`
    - both runs target head=`d3ebeee`

- `gh run view 25901775672 --log-failed | tail -n 160`
  - result: PASS
  - summary:
    - bundle main step is now green
    - append-step-summary still fails
    - current error: `IndentationError: unexpected indent`

- `gh run view 25901775676 --log-failed | tail -n 160`
  - result: PASS
  - summary:
    - completeness job still fails at `Failed to load WolfSSL library: libwolfssl.so`

### Second-Order Repairs

- update `tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - change: contract now executes the extracted summary shell against a fake JSON payload instead of only checking `bash -n`

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result before second fix: FAIL
  - summary: reproduced `IndentationError` from indented Python heredoc body

- update `.github/workflows/tls13-signer-gate.yml`
  - change: Python heredoc body now renders without extra leading spaces in the executed shell script

- add `tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - purpose: force the WolfSSL loader source to include Linux fallback search paths / versioned soname scanning

- `bash tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - result before second fix: FAIL
  - summary: `src/fafafa.ssl.wolfssl.api.pas` only attempted `LoadLibrary(WOLFSSL_LIB_NAME)`

- update `src/fafafa.ssl.wolfssl.api.pas`
  - change: on Linux, loader now:
    - tries the canonical bare name first
    - then tries explicit common library directories
    - then scans versioned `libwolfssl.so*` candidates

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result after second fix: PASS

- `bash tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - result after second fix: PASS

### Long-Run Local Completeness Revalidation

- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_ci_runtime_repair_20260515`
  - result: PASS
  - summary:
    - FreePascal capability-cache test compiled and ran successfully
    - `FreePascal KnownIssues 运行时对齐测试` PASS
    - `WolfSSL KnownIssues 运行时对齐测试` PASS
    - `MbedTLS KnownIssues 运行时对齐测试` PASS
    - final line: `[PASS] freepascal tls13 completeness gate finished`

### Fifth-Order Remote Revalidation

- `gh run list --branch master --limit 6 --json databaseId,workflowName,status,conclusion,headSha,displayTitle,url,createdAt,updatedAt`
  - result: PASS
  - summary:
    - latest run is CI `25902932655` on head `30467e4`
    - latest signer success remains `25902255923`
    - no newer remote run has superseded the shutdown-crash evidence yet

- `gh run view 25902932655 --json databaseId,displayTitle,headSha,conclusion,jobs,url`
  - result: PASS
  - summary:
    - only `FreePascal TLS 1.3 Completeness` failed
    - `Minimal Gate (Linux)` PASS
    - `Code Quality (Light)` PASS

- `gh run view 25902932655 --log-failed | tail -n 120`
  - result: PASS
  - summary:
    - `FreePascal KnownIssues runtime alignment` PASS
    - `WolfSSL KnownIssues runtime alignment` PASS
    - `MbedTLS KnownIssues runtime alignment` PASS
    - the job prints `所有测试完成！`
    - immediately afterward the process throws two `EAccessViolation` exceptions and exits 1

### Fifth-Order RED Contract

- `bash tests/scripts/test_optional_backend_shutdown_unregister_contract.sh`
  - result before fifth fix: FAIL
  - summary:
    - factory lacked a shutdown-safe unregister helper
    - optional backend units still unregistered through the normal `Finalize` path during `finalization`

### Fifth-Order Repairs

- add `tests/scripts/test_optional_backend_shutdown_unregister_contract.sh`
  - purpose: lock in the shutdown-safe unregister design for optional backends

- update `src/fafafa.ssl.factory.pas`
  - change: add `TSSLFactory.UnregisterLibraryForProcessShutdown`
  - change: process-shutdown helper now removes factory-held library references and registration entries without re-entering backend `Finalize`

- update `src/fafafa.ssl.mbedtls.lib.pas`
  - change: add sticky `GSkipFinalizeOnDestroy` guard for shutdown-time destroy
  - change: destructor now skips `Finalize` when process-shutdown unregister is active
  - change: `finalization` now calls `UnregisterMbedTLSBackendForProcessShutdown`

- update `src/fafafa.ssl.wolfssl.lib.pas`
  - change: add sticky `GSkipFinalizeOnDestroy` guard for shutdown-time destroy
  - change: destructor now skips `Finalize` when process-shutdown unregister is active
  - change: `finalization` now calls `UnregisterWolfSSLBackendForProcessShutdown`

### Local Revalidation After Fifth Fix

- `bash tests/scripts/test_optional_backend_shutdown_unregister_contract.sh`
  - result: PASS

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: PASS

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - result: PASS

- `git diff --check`
  - result: PASS

- `python3 scripts/compile_all_modules.py`
  - result: PASS
  - summary:
    - compiled 185/185 core Pascal modules successfully
    - both `src/fafafa.ssl.wolfssl.lib.pas` and `src/fafafa.ssl.mbedtls.lib.pas` compiled cleanly after the shutdown-safe changes

- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_shutdown_unregister_20260515`
  - result: PASS
  - summary:
    - `FreePascal KnownIssues 运行时对齐测试` PASS
    - `WolfSSL KnownIssues 运行时对齐测试` PASS
    - `MbedTLS KnownIssues 运行时对齐测试` PASS
    - final line: `[PASS] freepascal tls13 completeness gate finished`
    - local run did not reproduce the remote shutdown-time `EAccessViolation`

### Fifth Push Success Revalidation

- `gh run watch 25903921296 --exit-status`
  - result: PASS
  - summary:
    - `FreePascal TLS 1.3 Completeness` SUCCESS in 2m36s
    - `Minimal Gate (Linux)` SUCCESS in 3m11s
    - `Code Quality (Light)` SUCCESS

- `gh run view 25903921296 --json databaseId,displayTitle,headSha,conclusion,jobs,url`
  - result: PASS
  - summary:
    - run=`25903921296`
    - head=`45dabb4`
    - overall conclusion: `success`
    - previous shutdown-time `EAccessViolation` no longer reproduced on GitHub runner

### Sixth-Order RED Contract

- `bash tests/scripts/test_workflow_checkout_node24_contract.sh`
  - result before sixth fix: FAIL
  - summary:
    - `.github/workflows/basic-checks.yml.disabled` still used `actions/checkout@v4`
    - contract then expanded to cover all workflow files, not just active ones

### Sixth-Order Repairs

- add `tests/scripts/test_workflow_checkout_node24_contract.sh`
  - purpose: ensure `.github/workflows` no longer keeps `actions/checkout@v3/v4` and active/synced templates use `actions/checkout@v5`

- update `.github/workflows/*.yml` and `.github/workflows/*.yml.disabled`
  - change: upgrade every `actions/checkout@v3` / `actions/checkout@v4` reference to `actions/checkout@v5`
  - note: active workflows updated include `ci.yml`, `release.yml`, `tls13-signer-gate.yml`, `wave-b-b2-manual.yml`
  - note: synchronized templates updated include `release.yml.disabled` and `wave-b-b2-manual.yml.disabled`
  - note: dormant templates were also upgraded to prevent future re-enable drift

### Local Revalidation After Sixth Fix

- `bash tests/scripts/test_workflow_checkout_node24_contract.sh`
  - result: PASS

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: PASS

- `cmp -s .github/workflows/release.yml .github/workflows/release.yml.disabled`
  - result: PASS
  - summary: release workflow templates remain synchronized

- `cmp -s .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`
  - result: PASS
  - summary: wave-b manual workflow templates remain synchronized

- `git diff --check`
  - result: PASS

### Third-Order Remote Revalidation

- `gh run list --branch master --limit 8 --json databaseId,workflowName,status,conclusion,headSha,displayTitle,url,createdAt,updatedAt`
  - result: PASS
  - summary:
    - signer run=`25902255923` on head=`18f154f` => `success`
    - ci run=`25902255941` on head=`18f154f` => `failure`

- `gh run view 25902255941 --json databaseId,displayTitle,headSha,conclusion,jobs,url`
  - result: PASS
  - summary:
    - only `FreePascal TLS 1.3 Completeness` failed
    - `Minimal Gate (Linux)` PASS
    - `Code Quality (Light)` PASS

- `gh run view 25902255941 --log-failed | tail -n 220`
  - result: PASS
  - summary:
    - failure still lands in `WolfSSL KnownIssues 运行时对齐测试`
    - key error still reads `Failed to load WolfSSL library: libwolfssl.so`

### Third-Order RED/Process Gap

- `nl -ba .github/workflows/ci.yml | sed -n '1,260p'`
  - result: PASS
  - summary:
    - line `29`: minimal gate install step includes `libwolfssl-dev`
    - line `93`: completeness job install step still omitted `libwolfssl-dev`

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result before third fix: FAIL
  - summary:
    - upgraded contract extracts the `freepascal-tls13-completeness` job's install step
    - reproduced real gap: `sudo apt-get install -y fpc libssl-dev python3`

### Third-Order Repairs

- update `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - change: extract the `freepascal-tls13-completeness` job and its install step with `python3`, then assert `libwolfssl-dev` exists inside that specific block rather than anywhere in `ci.yml`

- update `.github/workflows/ci.yml`
  - change: completeness job install line now includes `libwolfssl-dev`

### Local Revalidation After Third Fix

- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - result: PASS

- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS

- `bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wolfssl_loader_fallback_contract.sh`
  - result: PASS

- `git diff --check`
  - result: PASS
