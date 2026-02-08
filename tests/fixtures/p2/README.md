# P2 Fixtures

This directory stores offline fixture files for P2 modules:

- `pkcs7/`
- `cms/`
- `pkcs12/`
- `ocsp/`
- `ct/`
- `ts/`
- `store/`

## Naming convention

Use `<module>_<scenario>_<version>.<ext>`.

Examples:

- `pkcs12_valid_default_v1.p12`
- `ocsp_response_revoked_v1.der`
- `ts_response_invalid_signature_v1.der`

## Rules

- Keep fixtures deterministic and offline.
- Add one success and one failure sample per scenario.
- Update `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md` when adding new fixtures.
