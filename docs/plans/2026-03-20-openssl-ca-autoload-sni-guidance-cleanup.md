# OpenSSL CA Autoload SNI Guidance Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove stale context-level SNI setup from the OpenSSL CA auto-loading test and its active documentation summary, aligning both with the project’s per-connection SNI guidance.

**Architecture:** Treat this as a narrow test/docs cleanup batch, not an API-removal batch. Add a focused shell contract for the selected test/doc pair, then update the test to configure SNI on `ISSLClientConnection` created from a dummy socket and update the doc wording to match. Do not change CA auto-loading semantics or broaden scope beyond this pair.

**Tech Stack:** Pascal test, Markdown doc, shell contract, focused compile/run verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_openssl_ca_autoload_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/openssl/test_openssl_ca_autoload.pas`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
- Fail if the test still uses:
  - `LCtx.SetServerName(...)`
  - `Ctx.SetServerName(...)`
- Fail if the doc still says:
  - `SNI hostname properly set on context`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_openssl_ca_autoload_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current test/doc drift

### Task 2: GREEN - Update test and doc

**Files:**
- Modify: `tests/openssl/test_openssl_ca_autoload.pas`
- Modify: `docs/CA_CERTIFICATE_AUTO_LOADING.md`

**Step 1: Replace stale guidance**

- In the selected test:
  - create the connection first
  - cast to `ISSLClientConnection`
  - set `ServerName` on the connection
  - assert via the connection interface
- In the doc:
  - update the test-case wording to say SNI is set on the client connection

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_openssl_ca_autoload_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_openssl_ca_autoload_no_context_level_sni_guidance_contract.sh`
- Test: `tests/openssl/test_openssl_ca_autoload.pas`
- Test: `docs/CA_CERTIFICATE_AUTO_LOADING.md`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_openssl_ca_autoload_no_context_level_sni_guidance_contract.sh`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_openssl_ca_autoload tests/openssl/test_openssl_ca_autoload.pas && ./tmp/test_openssl_ca_autoload`
- `rg -n 'SetServerName|GetServerName|client connection' tests/openssl/test_openssl_ca_autoload.pas docs/CA_CERTIFICATE_AUTO_LOADING.md`

Expected:
- contract passes
- test still compiles and runs
- selected file/doc now reflect per-connection SNI wording

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that this file was not intentional compatibility coverage.
- Note that CA auto-loading tests should not keep teaching context-level SNI as part of normal client setup.

**Step 2: Roll the next queue**

- Continue reviewing remaining ambiguous active uses with the same rule:
  - API-surface coverage => label
  - normal client flow/tests/docs => move to per-connection SNI
