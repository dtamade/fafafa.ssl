# Standalone LPR OpenSSL Unit Name Refresh Plan

**Goal:** Restore standalone `.lpr` smoke/module entrypoints that have drifted behind the current OpenSSL unit layout and API/runtime contract so they compile and run again under the current source tree.

**Architecture:** Keep this batch narrow:

- audit standalone `.lpr` compile status
- fix only source-level stale unit/API/runtime-expectation drift inside the standalone entrypoints
- do not redesign test runners or global compile scripts
- record compile-contract-only findings separately when a failure is caused by missing `-Fu` paths rather than stale source

The current source tree exposes OpenSSL types through:
- `fafafa.ssl.openssl.api.types`

Therefore this batch should update stale standalone programs that still import:
- `fafafa.ssl.openssl.types`

When evidence shows the standalone entrypoint has also drifted from its sibling reference program under `tests/certificate` or `tests/crypto`, this batch may refresh that local test logic as well, while keeping the change scoped to the standalone `.lpr`.

## Task 1: Confirm failing entrypoints

**Files:**
- Reference: `tests/test_p2_ct.lpr`
- Reference: `tests/test_p2_srp.lpr`
- Reference: `tests/unit/test_mock.lpr`

**Steps:**
- Run a standalone `.lpr` compile audit with isolated `-FU/-FE` output dirs.
- Confirm:
  - `test_p2_ct.lpr` fails because it imports `fafafa.ssl.openssl.types`
  - `test_p2_srp.lpr` fails because it imports `fafafa.ssl.openssl.types`
  - `test_mock.lpr` only fails when `tests/mocks` is absent from the compile search path

## Task 2: Minimal standalone source refresh

**Files:**
- Modify: `tests/test_p2_ct.lpr`
- Modify: `tests/test_p2_srp.lpr`

**Steps:**
- Replace the stale unit import:
  - `fafafa.ssl.openssl.types`
- With the current unit:
  - `fafafa.ssl.openssl.api.types`
- If the standalone program still fails after the unit-name refresh, compare it against the current sibling reference program and make the smallest local refresh needed to match current API/runtime contracts:
  - `test_p2_ct.lpr`: refresh stale CT symbol names (`i2o/o2i_*`, `X509_get_ext_d2i` path)
  - `test_p2_srp.lpr`: stop treating OpenSSL 3.x-optional `SRP_user_pwd_*` symbols as required

## Task 3: Verification

**Run:**
- `mkdir -p tmp/lpr_refresh/test_p2_ct && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/unit -FUtmp/lpr_refresh/test_p2_ct -FEtmp/lpr_refresh/test_p2_ct -otmp/lpr_refresh/test_p2_ct/test_p2_ct tests/test_p2_ct.lpr && ./tmp/lpr_refresh/test_p2_ct/test_p2_ct`
- `mkdir -p tmp/lpr_refresh/test_p2_srp && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/unit -FUtmp/lpr_refresh/test_p2_srp -FEtmp/lpr_refresh/test_p2_srp -otmp/lpr_refresh/test_p2_srp/test_p2_srp tests/test_p2_srp.lpr && ./tmp/lpr_refresh/test_p2_srp/test_p2_srp`
- `mkdir -p tmp/lpr_refresh/test_mock && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/unit -Fu./tests/mocks -FUtmp/lpr_refresh/test_mock -FEtmp/lpr_refresh/test_mock -otmp/lpr_refresh/test_mock/test_mock tests/unit/test_mock.lpr`
- `git diff --check -- docs/plans/2026-03-20-standalone-lpr-openssl-unit-name-refresh.md tests/test_p2_ct.lpr tests/test_p2_srp.lpr task_plan.md findings.md progress.md`

**Expected:**
- `test_p2_ct.lpr` and `test_p2_srp.lpr` compile and run again with current OpenSSL unit names.
- `test_mock.lpr` compiles when its required mock unit path is supplied, confirming it is a compile-contract issue rather than stale source.
