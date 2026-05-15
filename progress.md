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
