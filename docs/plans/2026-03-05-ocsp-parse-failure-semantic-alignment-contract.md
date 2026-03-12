# OCSP Parse-Failure Semantic Alignment Contract

## Goal
Lock cross-path semantic alignment for parse-failure in OpenSSL connection OCSP handling:
- `DoGetOCSPResponseStatus` must return parse-failure wording when DER decode fails;
- required-stapling validation must fail closed and map verify result to `X509_V_ERR_OCSP_VERIFY_FAILED` under the same parse-failure condition.

## Architecture
- Contract-only hardening (no production behavior change expected):
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a single scenario that drives both paths in one fixture context.
  - Inject `CountingD2IOCSPResponse` so decode deterministically fails (`d2i` returns `nil`).
  - Keep `OCSP_RESPONSE_free` and `OCSP_RESPONSE_status` assigned to isolate parse-failure semantics only.
  - Assert:
    - status path returns `Failed to parse OCSP response`;
    - required-stapling returns fail-closed and `verify_result = X509_V_ERR_OCSP_VERIFY_FAILED`.

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`

## Steps
1. Add contract scenario and register in test entry.
2. Run focused OCSP connection regression.
3. Run compile gate (`compile_all_modules.py`).
4. Update `task_plan.md`, `findings.md`, `progress.md` with evidence.

## Execution Log (2026-03-05)

### Contract
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added scenario: `TestOCSPParseFailureSemanticAlignment`.
  - Scenario drives both paths under the same deterministic decode-failure stub.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- Output (key):
  - `=== OCSP parse-failure semantic alignment: status string vs required-stapling verify_result ===`
  - `[PASS] Parse-failure semantics are aligned: status string + required-stapling verify_result`
  - `Passed: 15`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
