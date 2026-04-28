# SRP Helper Optional User-Pwd Guard Plan

**Goal:** Make `SRPCreateUser(...)` degrade safely on OpenSSL 3.x when deprecated optional `SRP_user_pwd_*` setter symbols are unavailable, instead of risking a nil-function call.

**Architecture:** Keep this batch narrow:

- add one focused regression for the missing-optional-symbol path
- change only `src/fafafa.ssl.openssl.api.srp.pas`
- do not redesign broader SRP support or standalone smoke programs
- preserve existing behavior when the setter symbols are available

## Task 1: RED - Reproduce the helper contract gap

**Files:**
- Add: `tests/test_srp_helper_optional_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.srp.pas`

**Steps:**
- Write a minimal standalone contract test that:
  - loads the OpenSSL core and SRP module
  - detects whether `SRP_user_pwd_set_salt` / `SRP_user_pwd_set_verifier` are missing
  - when they are missing, asserts `SRPCreateUser(...)` returns `nil` and does not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal runtime guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.srp.pas`

**Steps:**
- In `SRPCreateUser(...)`, before the helper reaches the deprecated setter calls:
  - detect whether `SRP_user_pwd_set_salt` and `SRP_user_pwd_set_verifier` are assigned
  - if either is missing, free the partially created user and return `nil`
- Keep the pre-existing success path unchanged when both setters are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/srp_helper_contract && fpc -B -Fu./src -FUtmp/srp_helper_contract -FEtmp/srp_helper_contract -otmp/srp_helper_contract/test_srp_helper_optional_symbol_contract tests/test_srp_helper_optional_symbol_contract.pas && ./tmp/srp_helper_contract/test_srp_helper_optional_symbol_contract`
- `mkdir -p tmp/lpr_refresh/test_p2_srp && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/unit -FUtmp/lpr_refresh/test_p2_srp -FEtmp/lpr_refresh/test_p2_srp -otmp/lpr_refresh/test_p2_srp/test_p2_srp tests/test_p2_srp.lpr && ./tmp/lpr_refresh/test_p2_srp/test_p2_srp`
- `git diff --check -- docs/plans/2026-03-20-srp-helper-optional-user-pwd-guard.md src/fafafa.ssl.openssl.api.srp.pas tests/test_srp_helper_optional_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused helper contract test passes on OpenSSL 3.x without raising an exception
- existing standalone SRP smoke program remains green
