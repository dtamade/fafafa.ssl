# fafafa.ssl v1.5.0 Release Notes

Release date: 2026-05-12

## Release Status

`v1.5.0` is the stable release candidate for the current API surface. Do not publish this release until `docs/test_reports/RELEASE_READINESS_V1.5.0.md` records a passing Windows `WinSSL` runtime proof and the tag has explicit human approval.

## Version Truth

- `FAFAFA_SSL_VERSION_STRING = '1.5.0'`
- `FAFAFA_SSL_INTERFACE_VERSION = 10500`
- Lazarus package version: `1.5.0`

## Public API Changes

Deprecated helper entrypoints were removed from `fafafa.ssl.factory.pas` and `fafafa.ssl.pas`.

Removed helpers:

- `SSLFactory`
- `SSLHelper`
- `CreateSSLLibrary`
- `CreateSSLContext`
- `CreateSSLCertificate`
- `CreateSSLConnection`

Use the explicit `TSSLFactory.*` APIs instead:

- `TSSLFactory.GetLibraryInstance(...)`
- `TSSLFactory.CreateContext(...)`
- `TSSLFactory.CreateCertificate(...)`
- `TSSLFactory.CreateConnection(...)`

The context-level SNI compatibility methods remain deprecated but available until the next major release.

## Security And Contract Hardening

- File-based certificate, private-key, and CA-chain loading now enforces size limits.
- Empty file paths are rejected consistently across the five backends.
- Replay-store file writes use stricter sharing rules.
- OpenSSL and WolfSSL early-data max-size setters now update internal state only after the backend API succeeds.
- Cross-backend error-mapping, capability, MbedTLS, and WolfSSL contract coverage was expanded.
- Expired CT/SCT and CRL test fixtures were refreshed while preserving the fixture semantics used by the FreePascal TLS 1.3 gate.

## Backend Notes

- OpenSSL: production early-data path remains the stable default for early-data users.
- FreePascal: TLS 1.3 coverage is broader, but early-data remains experimental. The default shipped path uses a local persistent replay store and fails closed when the replay-store path is unavailable.
- WolfSSL: early-data exposure is gated by build/runtime helper availability.
- MbedTLS: early-data, OCSP stapling, and CT remain unsupported in the current wrapper truth.
- WinSSL: source contracts and validation bundle contracts are in place, but stable release signoff still requires real Windows-host runtime artifacts.

## Verification Snapshot

- `python3 scripts/compile_all_modules.py`: PASS, 185/185 compiled.
- `bash scripts/run_minimal_ci_gate.sh --fast-local`: PASS.
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`: PASS, 17 passed / 0 failed.
- `python3 scripts/check_code_style.py src`: PASS after 44-file / 369-line mechanical indentation cleanup.
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`: PASS.
- Release workflow and Windows validation bundle contracts: PASS locally.
- Wave B/B2 Windows runtime proof: pending GitHub Actions artifact collection.

The final truth source for publish/no-publish is `docs/test_reports/RELEASE_READINESS_V1.5.0.md`.
