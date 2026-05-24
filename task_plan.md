# Task Plan: ISSLCertificateVerification Residual Campaign

## Objective

Finish the current `ISSLCertificateVerification` residual campaign with a
bounded goal: move ordinary runtime tests off deprecated direct-core
`GetVerifyResult` / `GetVerifyResultString`, and freeze only real mirror/backend
proofs as intentional residuals.

## Current State

- Root-test direct-core verify-result residual set: 6 files.
- Known owner-migrated root files in this campaign:
  - `tests/test_freepascal_backend_basic.pas`
  - `tests/test_freepascal_client_certificate_flight_requirements.pas`
  - `tests/test_freepascal_client_ct_sct_surface.pas`
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas`

## Remaining Queue

1. `tests/test_freepascal_client_online_ocsp_runtime.pas`
   - Likely failure-text runtime checks.
   - Action: inspect, then migrate to `ISSLCertificateVerification` if not a mirror proof.
2. `tests/test_freepascal_server_accept_skeleton.pas`
   - Likely simple failure-text check.
   - Action: inspect, then migrate if safe.
3. `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
   - Mixed numeric result + text assertions.
   - Action: migrate to owner only if semantics stay clear; otherwise freeze as intentional runtime proof.
4. `tests/test_freepascal_client_chain_trust_runtime.pas`
   - Mixed numeric result + text assertions.
   - Action: migrate to owner only if semantics stay clear; otherwise freeze as intentional runtime proof.
5. `tests/test_openssl_connection_verify_result_contract.pas`
   - Backend/core mirror contract.
   - Action: keep frozen unless public API compatibility strategy changes.
6. `tests/test_wolfssl_framework.pas`
   - Backend framework contract.
   - Action: inspect last; migrate only non-mirror failure text if obviously safe.

## Per-Round Contract

Each round must have:

- One named target file or one explicit freeze decision.
- A short pre-edit classification: `owner-migrate` or `freeze`.
- Code/test changes only for that target, plus required allowlist updates.
- Verification:
  - `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - relevant Pascal compile/run when a Pascal test is changed
  - `git diff --check`
- Brief review conclusion before commit.
- Git commit after the round.

## Stop Conditions

Stop this campaign when one of these is true:

- Root-test residual set contains only intentional mirror/backend proofs.
- A target would require changing public API or runtime semantics.
- A focused test fails in a way that changes the classification.

## Next Round

Target `tests/test_freepascal_client_online_ocsp_runtime.pas`.
