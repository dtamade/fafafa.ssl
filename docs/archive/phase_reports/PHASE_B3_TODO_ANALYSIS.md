# Phase B3: TODO Methods Analysis

**Date**: 2025-10-10

---

## Overview

Analysis of all TODO items in the fafafa.ssl codebase to prioritize implementation for Phase B3.

---

## TODO Categories

### Category 1: OpenSSL Context Callbacks (Priority: LOW)
These are advanced features that are rarely used and require careful implementation.

**Location**: `src/fafafa.ssl.openssl.pas`

1. **SetVerifyCallback** (line 951)
   - Purpose: Custom certificate verification callback
   - Complexity: HIGH
   - Required for: Custom cert validation logic
   - Status: TODO

2. **SetPasswordCallback** (line 1036)
   - Purpose: Password callback for encrypted private keys
   - Complexity: MEDIUM
   - Required for: Encrypted key files
   - Status: TODO (partial implementation exists for default password)

3. **SetInfoCallback** (line 1041)
   - Purpose: SSL state change notification callback
   - Complexity: MEDIUM
   - Required for: Debugging/monitoring
   - Status: TODO

**Recommendation**: SKIP for now - these are advanced features with complex callback mechanisms.

---

### Category 2: Session Management (Priority: MEDIUM)
Session caching and resumption for performance optimization.

**Location**: `src/fafafa.ssl.openssl.pas`

1. **SetSessionCacheMode** (line 976)
   - Purpose: Enable/disable session caching
   - Complexity: LOW
   - Required for: Session resumption
   - Status: TODO

2. **GetSessionCacheMode** (line 980)
   - Purpose: Query session cache status
   - Complexity: LOW
   - Status: Returns hardcoded `False`

3. **SetSessionTimeout** (line 986)
   - Purpose: Configure session lifetime
   - Complexity: LOW
   - Required for: Session management
   - Status: TODO

4. **GetSessionTimeout** (line 990)
   - Purpose: Query session timeout
   - Complexity: LOW
   - Status: Returns hardcoded `0`

5. **SetSessionCacheSize** (line 996)
   - Purpose: Configure session cache size
   - Complexity: LOW
   - Required for: Memory management
   - Status: TODO

6. **GetSessionCacheSize** (line 1000)
   - Purpose: Query session cache size
   - Complexity: LOW
   - Status: Returns hardcoded `0`

**Recommendation**: IMPLEMENT - These are straightforward OpenSSL API calls.

---

### Category 3: Helper Utility Functions (Priority: HIGH)
Standalone helper functions for certificate/key operations.

**Location**: `src/fafafa.ssl.openssl.pas`

1. **LoadCertificateFromFile** (line 3281)
   - Purpose: Load PX509 certificate from file
   - Complexity: LOW
   - Required for: Utility functions
   - Status: TODO (class method exists)

2. **LoadCertificateFromMemory** (line 3286)
   - Purpose: Load PX509 certificate from memory
   - Complexity: LOW
   - Required for: Utility functions
   - Status: TODO (class method exists)

3. **LoadPrivateKeyFromFile** (line 3291)
   - Purpose: Load PEVP_PKEY from file
   - Complexity: LOW
   - Required for: Utility functions
   - Status: TODO (class method exists)

4. **LoadPrivateKeyFromMemory** (line 3296)
   - Purpose: Load PEVP_PKEY from memory
   - Complexity: LOW
   - Required for: Utility functions
   - Status: TODO (class method exists)

5. **VerifyCertificate** (line 3301)
   - Purpose: Verify certificate against CA store
   - Complexity: LOW
   - Required for: Certificate validation
   - Status: TODO (class method exists)

6. **GetCertificateInfo** (line 3306)
   - Purpose: Extract certificate information
   - Complexity: LOW
   - Required for: Certificate inspection
   - Status: TODO (class method exists)

**Recommendation**: IMPLEMENT - These are simple wrappers around existing class methods.

---

### Category 4: WinSSL TODOs (Priority: LOW - Future Work)
WinSSL implementation has many TODO items for Phase 3+ of WinSSL development.

**Location**: Multiple `src/fafafa.ssl.winssl.*` files

**Total**: 70+ TODO items

**Recommendation**: SKIP - These are for future WinSSL Phase 3 development.

---

### Category 5: Other Module TODOs (Priority: VERY LOW)
TODOs in OpenSSL API modules, factory, certchain, utils.

**Locations**:
- `src/fafafa.ssl.factory.pas` - Future backend support, helper functions
- `src/fafafa.ssl.certchain.pas` - Advanced PKI features
- `src/fafafa.ssl.utils.pas` - Hash calculation utilities
- `src/fafafa.ssl.openssl.api.*` - API implementation details

**Recommendation**: SKIP - Not critical for current functionality.

---

## Phase B3 Implementation Plan

### Step 1: Implement Helper Utility Functions (HIGH PRIORITY)
**Estimated effort**: 1-2 hours
**Test coverage**: New test suite

**Functions**:
1. `LoadCertificateFromFile` - Simple wrapper around BIO + PEM_read_bio_X509
2. `LoadCertificateFromMemory` - Simple wrapper around BIO_new_mem_buf + PEM_read_bio_X509
3. `LoadPrivateKeyFromFile` - Simple wrapper around BIO + PEM_read_bio_PrivateKey
4. `LoadPrivateKeyFromMemory` - Simple wrapper around BIO_new_mem_buf + PEM_read_bio_PrivateKey
5. `VerifyCertificate` - Simple wrapper around X509_STORE_CTX + X509_verify_cert
6. `GetCertificateInfo` - Extract common fields into TSSLCertificateInfo record

**Benefit**: Provides standalone utility functions for certificate/key operations without requiring object instantiation.

---

### Step 2: Implement Session Management (MEDIUM PRIORITY)
**Estimated effort**: 2-3 hours
**Test coverage**: New test suite

**Functions**:
1. `SetSessionCacheMode` / `GetSessionCacheMode` - SSL_CTX_set_session_cache_mode
2. `SetSessionTimeout` / `GetSessionTimeout` - SSL_CTX_set_timeout / SSL_CTX_get_timeout
3. `SetSessionCacheSize` / `GetSessionCacheSize` - SSL_CTX_sess_set_cache_size / SSL_CTX_sess_get_cache_size

**Benefit**: Enables session resumption for improved TLS performance.

---

### Step 3: Document Callback Methods (LOW PRIORITY)
**Estimated effort**: 1 hour
**Test coverage**: Not applicable

Document why callback methods are not implemented:
- Complex callback marshaling between Pascal and C
- Require advanced error handling
- Low demand for these features
- Can be added in future if needed

**Benefit**: Clarifies scope and prevents confusion.

---

## Summary

**Total TODOs found**: 130+

**Phase B3 scope**: 12 methods (9.2% of total TODOs)
- 6 helper utility functions (HIGH)
- 6 session management methods (MEDIUM)

**Deferred**: 118+ TODOs
- 3 callback methods (LOW - complex, rarely used)
- 70+ WinSSL TODOs (LOW - future phase)
- 45+ other module TODOs (VERY LOW - not critical)

**Estimated total effort**: 3-5 hours

**Test coverage**: 2 new test suites
- `test_helper_utilities.pas` (6-8 tests)
- `test_session_management.pas` (6-8 tests)

---

## Next Steps

1. ✅ Create this TODO analysis document
2. ⏭️ Implement helper utility functions (Step 1)
3. ⏭️ Create test suite for helper utilities
4. ⏭️ Implement session management methods (Step 2)
5. ⏭️ Create test suite for session management
6. ⏭️ Create Phase B3 completion report

**Status**: Ready to begin implementation
