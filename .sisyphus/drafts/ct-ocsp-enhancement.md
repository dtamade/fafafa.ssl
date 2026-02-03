# Draft: CT and OCSP Stapling Enhancement

## User Request Summary
Implement Certificate Transparency (CT) and OCSP Stapling enhancements for the fafafa.ssl Free Pascal SSL/TLS library.

## Project Context (Confirmed)
- **Project**: fafafa.ssl - Free Pascal SSL/TLS library
- **Language**: Free Pascal (Object Pascal)
- **Architecture**: Rust-style (Result<T,E>, Builder pattern, interface abstraction)
- **Multi-backend**: OpenSSL, WinSSL, MbedTLS, WolfSSL
- **Test coverage**: 95.8% (60,621 lines of code, 109 modules)

## Current Status (From User)

### CT Module (src/fafafa.ssl.openssl.api.ct.pas)
- Test pass rate: 100% (36/36 tests)
- Core functions implemented: SCT_new, SCT_free, SCT_validate, SCT_LIST_validate
- SCT serialization: i2o_SCT, o2i_SCT
- X509 CT extension integration exists
- **MISSING**: Production-grade SCT verification, CT log server integration

### OCSP Module (src/fafafa.ssl.openssl.api.ocsp.pas)
- Test pass rate: 88% (22/25 tests)
- Basic OCSP request/response implemented
- **MISSING**: OCSP Stapling (RFC 6066), response caching

## Implementation Goals

### Issue #2: Implement SCT Verification (RFC 6962)
- Parse SCT from X.509 v3 extensions
- Verify SCT signatures using CT log public keys
- Implement SCT timestamp verification
- Support 3 SCT sources: TLS extension, OCSP Stapling, certificate extension

### Issue #3: Integrate CT Log Servers
- Implement CT log server API client
- Support Google CT log list
- Add CT policy configuration (required SCT count)
- Implement CT log caching mechanism

## Deliverables (Confirmed)
- src/fafafa.ssl.ct.sct.pas - SCT verification module
- src/fafafa.ssl.ct.log.pas - CT log client
- tests/ct/test_sct_verification.pas - SCT verification tests
- docs/CT_IMPLEMENTATION_GUIDE.md - CT implementation guide

## Acceptance Criteria (Confirmed)
- CT module test pass rate: 100%
- Support at least 3 mainstream CT log servers
- SCT verification performance < 50ms (single certificate)

## Constraints (Confirmed)
- Must follow existing Free Pascal coding patterns
- Must integrate with existing certificate verification flow
- Must use OpenSSL CT APIs correctly
- Must maintain backward compatibility

## Open Questions (To Be Clarified)

### 1. OCSP Stapling Scope
**Question**: Should we implement client-side OCSP Stapling, server-side OCSP Stapling, or both?
- Client-side: Parse and validate stapled OCSP responses from servers
- Server-side: Fetch and staple OCSP responses for clients
- Both: Full implementation

### 2. CT Log List Source
**Question**: What should be the source for CT log lists?
- Google's official CT log list (https://www.gstatic.com/ct/log_list/v3/log_list.json)
- Custom/configurable log lists
- Both with fallback mechanism

**Question**: How should CT log lists be updated?
- Manual update by user
- Automatic periodic updates
- On-demand refresh API

### 3. Test Infrastructure
**Question**: What testing framework is used in the project?
- From README: mentions "tests/" directory with unit/, fuzz/, benchmarks/, integration/
- Need to confirm: FPCUnit? Custom framework?

**Question**: Should we follow TDD approach?
- Write tests first (RED-GREEN-REFACTOR)
- Write tests after implementation
- Manual verification procedures

### 4. Performance Requirements
**Question**: Beyond the 50ms SCT verification requirement, are there other performance constraints?
- CT log server query timeout?
- OCSP response caching duration?
- Maximum memory usage for CT log cache?

### 5. Integration Points
**Question**: Where exactly should CT verification integrate into the certificate verification flow?
- During TLS handshake?
- Post-handshake verification?
- Optional verification API?

### 6. Error Handling
**Question**: What should happen when CT verification fails?
- Hard fail (reject connection)?
- Soft fail (log warning, continue)?
- Configurable policy?

## Research Findings

### CT Module Analysis (COMPLETED)

**Current Implementation** (`src/fafafa.ssl.openssl.api.ct.pas` - 619 lines):
- ✅ Complete OpenSSL CT API bindings (SCT lifecycle, validation, serialization)
- ✅ Helper functions: `EnableCertificateTransparency()`, `ValidateSCTList()`, `LoadCTLogStore()`
- ✅ SCT sources supported: TLS extension, X.509v3 extension, OCSP stapled response
- ✅ Test coverage: 100% (36/36 tests in `tests/certificate/test_p2_ct.pas`)

**What's Missing**:
1. **CT Log Query Client**: No HTTP client for querying CT logs (get-sth, get-entries)
2. **SCT Verification in Pure Pascal**: Pure Pascal X.509 parser doesn't extract/verify SCTs
3. **CT Policy Enforcement**: No high-level API for CT policy (e.g., "require 2 SCTs from different logs")
4. **CT Log List Management**: No built-in CT log list (Google's log list, etc.)
5. **Precertificate Handling**: No support for creating/verifying precertificates

**Integration Points**:
- During TLS handshake: `SSL_CTX_set_ct_validation_callback()`, `SSL_get0_peer_scts()`
- During certificate parsing: Add OID `1.3.6.1.4.1.11129.2.4.2` to `ProcessKnownExtensions()`
- Post-handshake: Use `ValidateSCTList()` helper

**Existing Patterns to Follow**:
- Result<T,E> pattern: `src/fafafa.ssl.result.utils.pas` (467 lines)
- Builder pattern: `src/fafafa.ssl.connection.builder.pas` (415 lines)
- HTTP client: `examples/https_client_production.pas` (462 lines) - THTTPSClient class
- Test framework: Custom test framework with `StartTest()`, `PassTest()`, `FailTest()`

### OCSP Module Analysis (COMPLETED)

**IMPORTANT CORRECTION**: Test pass rate is **100% (25/25 tests)**, NOT 88% as stated in requirements!

**Current Implementation** (`src/fafafa.ssl.openssl.api.ocsp.pas` - 1069 lines):
- ✅ Complete OpenSSL OCSP API bindings (84 functions)
- ✅ Helper functions: `CheckCertificateStatus()`, `CreateOCSPRequest()`, `SendOCSPRequest()`, `VerifyOCSPResponse()`
- ✅ HTTP/HTTPS transport with TLS verification
- ✅ Nonce support for replay protection
- ✅ Fail-closed security model
- ✅ Test coverage: 100% (25/25 tests in `tests/certificate/test_p2_ocsp.pas`)

**OCSP Integration Points**:
- Post-handshake verification: `ValidatePostHandshake()` in `src/fafafa.ssl.openssl.connection.pas` (Lines 422-546)
- Certificate verification: `VerifyEx()` in `src/fafafa.ssl.openssl.certificate.pas` (Lines 858-960)
- Context setup: `src/fafafa.ssl.openssl.context.pas` (Lines 1497-1498)

**TLS Extension APIs** (DECLARED BUT NOT USED):
- `SSL_CTX_set_tlsext_status_type` - Enable OCSP stapling
- `SSL_set_tlsext_status_ocsp_resp` - Attach OCSP response (server-side)
- `SSL_get_tlsext_status_ocsp_resp` - Retrieve stapled response (client-side)
- `SSL_CTX_set_tlsext_status_cb` - Stapling callback
- Extension type: `TLSEXT_TYPE_status_request = 5` (RFC 6066)

**What's Missing for OCSP Stapling**:
1. **OCSP Response Cache**: No cache for OCSP responses (each verification = fresh request)
2. **Server-Side Stapling Callback**: TLS extension APIs declared but not implemented
3. **Client-Side Stapled Response Verification**: No code to retrieve/verify stapled responses
4. **Background OCSP Fetcher**: No pre-fetching or refresh mechanism
5. **Cache Expiry Pattern**: Need time-based expiry (can follow CRL pattern from `src/fafafa.ssl.crl.pas`)

**Existing Patterns to Follow**:
- Callback pattern: Password callback in `src/fafafa.ssl.openssl.context.pas` (Lines 1510-1540)
- Time-based expiry: CRL expiry in `src/fafafa.ssl.crl.pas`
- Session cache: `SSL_CTX_sess_*` functions available but not used for OCSP

## Technical Decisions (Auto-Resolved with Defaults)

### 1. OCSP Stapling Scope: **Both Client and Server** (Default)
**Rationale**: 
- User's requirements mention "OCSP Stapling (RFC 6066)" without specifying direction
- TLS extension APIs are already declared for both directions
- Production-ready library should support both use cases
- Can be implemented incrementally (client-side first, then server-side)

### 2. CT Log List Source: **Both with Fallback** (Default)
**Rationale**:
- Google's official list (https://www.gstatic.com/ct/log_list/v3/log_list.json) as primary source
- Allow custom log lists for enterprise/testing scenarios
- Follows existing pattern of "system defaults + user override" (like certificate stores)

### 3. CT Log List Updates: **On-Demand Refresh API** (Default)
**Rationale**:
- Manual update: Too inflexible for production use
- Automatic periodic updates: Adds complexity (background threads, lifecycle management)
- On-demand refresh: Best balance - user controls when to update, no background threads
- Can be upgraded to automatic later if needed

### 4. CT Verification Failure Policy: **Configurable Policy** (Default)
**Rationale**:
- Existing codebase has `sslCertVerifyCheckOCSP` flag pattern
- Different use cases need different policies (strict for production, permissive for testing)
- Follows OpenSSL's `SSL_CT_VALIDATION_PERMISSIVE` vs `SSL_CT_VALIDATION_STRICT` pattern
- Default to permissive mode (log warnings), allow opt-in to strict mode

### 5. RFC 9162 Support: **RFC 6962 Only (for now)** (Default)
**Rationale**:
- RFC 6962 logs shutting down Feb 28, 2026 (28 days away) is concerning BUT:
- User's requirements explicitly mention "RFC 6962" implementation
- RFC 9162 support can be added later as enhancement (backward compatible)
- Focus on delivering working RFC 6962 implementation first
- Plan includes architecture that allows RFC 9162 addition without major refactoring
- **Note in plan**: RFC 9162 support recommended as future enhancement

### RFC Specifications (COMPLETED)

**CRITICAL UPDATE**: RFC 6962 is **OBSOLETED by RFC 9162** (Static CT API)
- Let's Encrypt RFC 6962 logs shut down: **February 28, 2026** (28 days from now!)
- New logs use Static CT API (more scalable, better performance)
- Implementation should support BOTH formats during transition

**SCT Structure** (RFC 6962/9162):
- Version (1 byte): v1 (0x00)
- Log ID (32 bytes): SHA-256 hash of log's public key
- Timestamp (8 bytes): Milliseconds since Unix epoch
- Extensions (variable): Currently empty
- Signature (variable): Digital signature (ECDSA P-256 or RSA)

**SCT Signature Verification Steps**:
1. Extract log's public key from CT log list
2. Reconstruct signed data structure
3. Verify signature using log's public key
4. Validate timestamp (not in future, within certificate validity)

**OCSP Stapling** (RFC 6066):
- Client sends `status_request` extension (type 5) in ClientHello
- Server responds with `CertificateStatus` message containing DER-encoded OCSP response
- Client validates OCSP response signature and freshness

**Chrome CT Policy** (2026):
- Certificates ≤ 180 days: **2 SCTs** from different log operators
- Certificates > 180 days: **3 SCTs** from different log operators
- At least one SCT from Google-operated log (policy evolving)

**Google CT Log List**:
- JSON format: https://www.gstatic.com/ct/log_list/v3/log_list.json
- Signature: https://www.gstatic.com/ct/log_list/v3/log_list.sig
- Public key: https://www.gstatic.com/ct/log_list/v3/log_list_pubkey.pem
- Schema: https://www.gstatic.com/ct/log_list/v3/log_list_schema.json

**Log Entry Fields**:
- `description`: Human-readable name
- `log_id`: Base64-encoded SHA-256 hash of public key
- `key`: Base64-encoded SubjectPublicKeyInfo
- `url`: Log API endpoint
- `mmd`: Maximum Merge Delay (typically 86400 = 24 hours)
- `state`: Log state (Usable, Qualified, Retired, Rejected)
- `temporal_interval`: Start/end timestamps for log shard

**Performance Best Practices**:
- Cache OCSP responses (24-48 hour TTL)
- Preload CT log list at startup, refresh daily
- Cache SCT validation results per certificate
- Parallel SCT verification
- Use permissive mode initially, then strict mode

## Technical Decisions (Auto-Resolved with Defaults)

### 1. OCSP Stapling Scope: **Both Client and Server** (Default)
**Rationale**: 
- User's requirements mention "OCSP Stapling (RFC 6066)" without specifying direction
- TLS extension APIs are already declared for both directions
- Production-ready library should support both use cases
- Can be implemented incrementally (client-side first, then server-side)

### 2. CT Log List Source: **Both with Fallback** (Default)
**Rationale**:
- Google's official list (https://www.gstatic.com/ct/log_list/v3/log_list.json) as primary source
- Allow custom log lists for enterprise/testing scenarios
- Follows existing pattern of "system defaults + user override" (like certificate stores)

### 3. CT Log List Updates: **On-Demand Refresh API** (Default)
**Rationale**:
- Manual update: Too inflexible for production use
- Automatic periodic updates: Adds complexity (background threads, lifecycle management)
- On-demand refresh: Best balance - user controls when to update, no background threads
- Can be upgraded to automatic later if needed

### 4. CT Verification Failure Policy: **Configurable Policy** (Default)
**Rationale**:
- Existing codebase has `sslCertVerifyCheckOCSP` flag pattern
- Different use cases need different policies (strict for production, permissive for testing)
- Follows OpenSSL's `SSL_CT_VALIDATION_PERMISSIVE` vs `SSL_CT_VALIDATION_STRICT` pattern
- Default to permissive mode (log warnings), allow opt-in to strict mode

### 5. RFC 9162 Support: **RFC 6962 Only (for now)** (Default)
**Rationale**:
- RFC 6962 logs shutting down Feb 28, 2026 (28 days away) is concerning BUT:
- User's requirements explicitly mention "RFC 6962" implementation
- RFC 9162 support can be added later as enhancement (backward compatible)
- Focus on delivering working RFC 6962 implementation first
- Plan includes architecture that allows RFC 9162 addition without major refactoring
- **Note in plan**: RFC 9162 support recommended as future enhancement

## Scope Boundaries
**INCLUDE**:
- SCT verification from all 3 sources (TLS extension, OCSP, X.509 extension)
- CT log server integration
- OCSP Stapling support
- Response caching mechanisms
- Test coverage for new functionality
- Documentation

**EXCLUDE** (To be confirmed):
- CT log server implementation (only client)
- OCSP responder implementation (only client)
- GUI/CLI tools (library only)
- Non-OpenSSL backend implementations (focus on OpenSSL first)
