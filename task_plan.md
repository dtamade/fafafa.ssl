# Task Plan: ISSLConnectionInfo Backend Contract Owner Primacy Completion

## Goal
Finish the remaining `ISSLConnectionInfo` backend-contract wording drift by making `GetSelectedALPNProtocol` and `GetStateString` follow the same owner-first contract semantics already used for `GetConnectionInfo` and `GetContext`.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-24-isslconnectioninfo-backend-contract-owner-primacy-completion.md](docs/plans/2026-05-24-isslconnectioninfo-backend-contract-owner-primacy-completion.md)

## Done
- Confirmed the current worktree is clean after the whole-surface taxonomy batch.
- Re-checked the `ISSLConnectionInfo` family and found the real remaining drift is inside `tests/contract/test_backend_contract.pas`, not active docs or source declarations.
- Verified `GetConnectionInfo` and `GetContext` already use owner-first failure wording, while `GetSelectedALPNProtocol` and `GetStateString` still describe the optional owner as if it drifted from the core mirror.
- Add a focused shell contract for ALPN / state-string backend-contract owner primacy.
- Flip the backend contract wording/comments to owner-first semantics for those two mirrors.
- During verification, discovered two FreePascal TLS1.3 runtime proofs had reintroduced direct core `GetSelectedALPNProtocol`; migrated them back to `ISSLConnectionInfo.GetSelectedALPNProtocol` and restored the 4-hit residual allowlist.

## Verification
- `bash -n tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_alpn_statestring_contract_owner_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
- `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `fpc -B -Fu./src -Fu./tests -FUtmp/test_fp_alpn_owner/units -FEtmp/test_fp_alpn_owner/bin tests/test_freepascal_client_session_resumption.pas`
- `tmp/test_fp_alpn_owner/bin/test_freepascal_client_session_resumption`
- `fpc -B -Fu./src -Fu./tests -FUtmp/test_fp_alpn_owner_server/units -FEtmp/test_fp_alpn_owner_server/bin tests/test_freepascal_server_accept_skeleton.pas`
- `tmp/test_fp_alpn_owner_server/bin/test_freepascal_server_accept_skeleton`
- `git diff --check`
