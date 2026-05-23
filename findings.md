# Findings

## Conclusions
- The remaining FreePascal completeness-gate failure was not a crypto or parser redesign issue; it was an error-propagation bug in the embedded SCT fallback path.
- `TFreePascalConnection.TryCachePeerCertificatesFromHandshake` used to clear `AError` when `TryLoadEmbeddedSignedCertificateTimestampList(...)` failed, which turned malformed embedded SCT data into a fail-open/no-SCT path.
- The fix is intentionally minimal: preserve an SCT-related error, clear cached peer-cert state, and exit early so the handshake stays fail-closed.
- The fast-local completeness gate is now fully green: `18/18 PASS` in `tmp/test-reports/freepascal_tls13_completeness_20260523_191741.md`.
- A full module compile probe still ends at the same pre-existing boundary: `fafafa.ssl.pkcs11.engine.pas` is the only failing unit (`185/186` compiled).

## Notes
- This batch reopens CT/SCT work only because a fresh RED surfaced in the completeness gate; no broader CT policy or parser redesign was needed.
