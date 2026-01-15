# WinSSL Error Mapping (Draft)

Date: 2025-10-31
Scope: Map common Schannel (SSPI) error codes to fafafa.ssl project error categories and user-facing messages.

## Goals
- Normalize error handling between WinSSL and OpenSSL backends
- Provide actionable messages and categories (Handshake, Certificate, IO, Timeout, Internal)

## Initial Map (seed)

| Schannel / SSPI Code | Symbol | Category | Suggested Mapping | Notes |
|---|---|---|---|---|
| 0x80090326 | SEC_E_ILLEGAL_MESSAGE | Handshake | sslErrProtocol | Malformed or unexpected message |
| 0x80090327 | SEC_E_INVALID_TOKEN   | Handshake | sslErrProtocol | Token invalid/expired |
| 0x80090325 | SEC_E_UNTRUSTED_ROOT  | Certificate | sslErrCertUntrusted | Root CA not trusted |
| 0x80090324 | SEC_E_INCOMPLETE_MESSAGE | IO | sslErrIO | Need more data / partial record |
| 0x80090328 | SEC_E_QOP_NOT_SUPPORTED | Feature | sslErrNotSupported | Cipher/feature unsupported |
| 0x80090322 | SEC_E_WRONG_PRINCIPAL | Certificate | sslErrHostnameMismatch | Hostname mismatch |
| 0x80090321 | SEC_E_BUFFER_TOO_SMALL | Internal | sslErrInternal | Resize buffers |
| 0x8009030E | SEC_E_NO_CREDENTIALS  | Credential | sslErrCredential | No client cert/credential |
| 0x8009030C | SEC_E_LOGON_DENIED    | Credential | sslErrCredential | Bad credentials |
| 0x80090311 | SEC_E_MESSAGE_ALTERED | Handshake | sslErrProtocol | Integrity check failed |

## Next Steps
- Add exhaustive list used in our WinSSL code path (`src/fafafa.ssl.winssl.*`).
- Back tests in `tests/test_winssl_integration_multi.pas` negative scenarios to validate mapping.
- Document retries and user guidance for common errors (system time, CA store, proxy MITM).

