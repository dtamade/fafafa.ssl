# ICertificateEx Source-Level Final Sweep

## Goal
Confirm `ICertificateEx` migration cleanup is complete at source level and deprecated warning suppression remains only where technically necessary.

## Scope
- Verification only (no production behavior changes expected)
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Sweep migration-related symbols and suppression markers:
   - `rg -n "WARN SYMBOL_DEPRECATED OFF|ICertificateEx is deprecated|IPrivateKeyEx is deprecated|fafafa\\.ssl\\.cert\\.builder\\.ICertificateEx|fafafa\\.ssl\\.cert\\.builder\\.IPrivateKeyEx" src tests`
2. Assert only bridge-local hits remain in:
   - `src/fafafa.ssl.openssl.cert.builder.pas`
