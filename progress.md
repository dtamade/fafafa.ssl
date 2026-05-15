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
