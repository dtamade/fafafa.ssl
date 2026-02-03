
### Wave 1: Foundation (Start Immediately)

---

#### Task 1: CT Log Client Foundation

**What to do**:
- Create `src/fafafa.ssl.ct.log.pas` with CT log client implementation
- Implement HTTP client for CT log API queries (based on THTTPSClient pattern from `examples/https_client_production.pas`)
- Implement Google CT log list parser (JSON format from https://www.gstatic.com/ct/log_list/v3/log_list.json)
- Implement CT log public key management (Base64-encoded SubjectPublicKeyInfo)
- Add Result<T,E> error handling throughout
- Write test suite `tests/ct/test_ct_log_client.pas` following custom test framework pattern

**Must NOT do**:
- NO automatic periodic updates (on-demand refresh only)
- NO CT log server implementation (client-side only)
- NO RFC 9162 support in this phase

**Recommended Agent Profile**:
- **Category**: `unspecified-high`
  - Reason: Complex HTTP client implementation with JSON parsing, requires high-quality code
- **Skills**: [`playwright`]
  - `playwright`: May need to verify CT log list URL accessibility and response format
- **Skills Evaluated but Omitted**:
  - `git-master`: Not needed yet (no commits in foundation phase)

**Parallelization**:
- **Can Run In Parallel**: YES
- **Parallel Group**: Wave 1 (with Tasks 2, 3)
- **Blocks**: Tasks 4, 5 (SCT verification and CT policy need log client)
- **Blocked By**: None (can start immediately)

**References**:

**Pattern References**:
- `examples/https_client_production.pas:262-372` - THTTPSClient class pattern (HTTP client, URL parsing, request/response handling)
- `src/fafafa.ssl.result.utils.pas:41-85` - Result<T,E> pattern (error handling, TryOperation, FromException)
- `tests/certificate/test_p2_ct.pas:38-509` - Custom test framework pattern (StartTest, PassTest, FailTest)

**API/Type References**:
- `src/fafafa.ssl.openssl.api.ct.pas:64-81` - CT data structures (PCTLOG, PCTLOG_STORE, PSCT)
- `src/fafafa.ssl.openssl.api.ct.pas:142-162` - CTLOG functions (CTLOG_new, CTLOG_get0_public_key, CTLOG_STORE_load_file)

**External References**:
- Google CT Log List: https://www.gstatic.com/ct/log_list/v3/log_list.json - JSON format specification
- RFC 6962 Section 3.2: https://www.rfc-editor.org/rfc/rfc6962.html#section-3.2 - Log server API specification

**Acceptance Criteria**:

**Automated Verification**:
```bash
# Agent runs test suite:
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/ct/test_ct_log_client.pas -o./bin/test_ct_log_client && ./bin/test_ct_log_client
# Assert: Output contains "Total Tests: N"
# Assert: Output contains "Passed: N (100.0%)"
# Assert: Output contains "Failed: 0 (0.0%)"
# Assert: Exit code 0
```

**Test Coverage**:
- [ ] Test: Parse Google CT log list JSON successfully
- [ ] Test: Extract log ID, public key, URL from log entry
- [ ] Test: Handle malformed JSON gracefully (Result<T,E> error)
- [ ] Test: HTTP GET request to CT log list URL (timeout 5s)
- [ ] Test: Base64 decode log public key
- [ ] Test: Create CTLOG from parsed data

**Commit**: NO (groups with Wave 1 completion)

---

#### Task 2: OCSP Response Cache Foundation

**What to do**:
- Create `src/fafafa.ssl.ocsp.cache.pas` with thread-safe OCSP response cache
- Implement cache entry structure (certificate fingerprint, DER-encoded response, timestamps)
- Implement time-based expiry using TDateTime (follow CRL pattern from `src/fafafa.ssl.crl.pas`)
- Implement LRU eviction when cache exceeds 1000 entries
- Use TCriticalSection for thread safety
- Add Result<T,E> error handling
- Write test suite `tests/ocsp/test_ocsp_cache.pas`

**Must NOT do**:
- NO automatic background refresh (on-demand only)
- NO OCSP response generation (caching only)
- NO persistent storage (in-memory cache only)

**Recommended Agent Profile**:
- **Category**: `unspecified-high`
  - Reason: Thread-safe data structure with time-based expiry, requires careful implementation
- **Skills**: []
- **Skills Evaluated but Omitted**:
  - `playwright`: Not needed (no browser interaction)
  - `git-master`: Not needed yet (no commits in foundation phase)

**Parallelization**:
- **Can Run In Parallel**: YES
- **Parallel Group**: Wave 1 (with Tasks 1, 3)
- **Blocks**: Tasks 6, 7 (OCSP stapling needs cache)
- **Blocked By**: None (can start immediately)

**References**:

**Pattern References**:
- `src/fafafa.ssl.crl.pas` - Time-based expiry pattern (IsExpired, IsValid using TDateTime)
- `src/fafafa.ssl.result.utils.pas:41-85` - Result<T,E> pattern
- `tests/certificate/test_p2_ocsp.pas:38-509` - Custom test framework pattern

**API/Type References**:
- `src/fafafa.ssl.openssl.api.ocsp.pas:34-79` - OCSP data structures (POCSP_RESPONSE, POCSP_BASICRESP)
- `src/fafafa.ssl.openssl.api.ocsp.pas:196-208` - OCSP response operations (OCSP_resp_get0_produced_at, OCSP_single_get0_status)

**External References**:
- RFC 6960 Section 4.2.2.1: https://www.rfc-editor.org/rfc/rfc6960.html#section-4.2.2.1 - OCSP response validity period (thisUpdate, nextUpdate)

**Acceptance Criteria**:

**Automated Verification**:
```bash
# Agent runs test suite:
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/ocsp/test_ocsp_cache.pas -o./bin/test_ocsp_cache && ./bin/test_ocsp_cache
# Assert: Output contains "Total Tests: N"
# Assert: Output contains "Passed: N (100.0%)"
# Assert: Output contains "Failed: 0 (0.0%)"
# Assert: Exit code 0
```

**Test Coverage**:
- [ ] Test: Put and Get cache entry successfully
- [ ] Test: Expired entry returns cache miss
- [ ] Test: LRU eviction when exceeding 1000 entries
- [ ] Test: Thread-safe concurrent Put operations
- [ ] Test: Thread-safe concurrent Get operations
- [ ] Test: Cleanup removes expired entries

**Commit**: NO (groups with Wave 1 completion)

---

#### Task 3: SCT Data Structures

**What to do**:
- Create `src/fafafa.ssl.ct.sct.pas` with SCT data structure helpers
- Implement SCT parsing from TLS wire format (o2i_SCT wrapper)
- Implement SCT serialization to TLS wire format (i2o_SCT wrapper)
- Implement SCT field accessors (version, log ID, timestamp, extensions, signature)
- Add Result<T,E> error handling
- Write test suite `tests/ct/test_sct_data_structures.pas`

**Must NOT do**:
- NO signature verification in this task (Task 4)
- NO CT policy enforcement (Task 5)
- NO precertificate handling

**Recommended Agent Profile**:
- **Category**: `unspecified-high`
  - Reason: Binary data parsing and serialization, requires careful handling
- **Skills**: []
- **Skills Evaluated but Omitted**:
  - `playwright`: Not needed (no browser interaction)
  - `git-master`: Not needed yet (no commits in foundation phase)

**Parallelization**:
- **Can Run In Parallel**: YES
- **Parallel Group**: Wave 1 (with Tasks 1, 2)
- **Blocks**: Tasks 4, 8 (SCT verification and X.509 parsing need data structures)
- **Blocked By**: None (can start immediately)

**References**:

**Pattern References**:
- `src/fafafa.ssl.openssl.api.ct.pas:532-605` - PrintSCTInfo helper (SCT field access pattern)
- `src/fafafa.ssl.result.utils.pas:41-85` - Result<T,E> pattern
- `tests/certificate/test_p2_ct.pas:38-509` - Custom test framework pattern

**API/Type References**:
- `src/fafafa.ssl.openssl.api.ct.pas:64-81` - SCT data structures (PSCT, PSCT_LIST)
- `src/fafafa.ssl.openssl.api.ct.pas:108-131` - SCT functions (SCT_new, SCT_get_*, SCT_set_*)
- `src/fafafa.ssl.openssl.api.ct.pas:165-168` - SCT serialization (i2o_SCT, o2i_SCT)

**External References**:
- RFC 6962 Section 3.2: https://www.rfc-editor.org/rfc/rfc6962.html#section-3.2 - SCT structure specification

**Acceptance Criteria**:

**Automated Verification**:
```bash
# Agent runs test suite:
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/ct/test_sct_data_structures.pas -o./bin/test_sct_data_structures && ./bin/test_sct_data_structures
# Assert: Output contains "Total Tests: N"
# Assert: Output contains "Passed: N (100.0%)"
# Assert: Output contains "Failed: 0 (0.0%)"
# Assert: Exit code 0
```

**Test Coverage**:
- [ ] Test: Parse SCT from TLS wire format
- [ ] Test: Serialize SCT to TLS wire format
- [ ] Test: Get SCT version (v1 = 0)
- [ ] Test: Get SCT log ID (32 bytes)
- [ ] Test: Get SCT timestamp (milliseconds since epoch)
- [ ] Test: Get SCT signature
- [ ] Test: Handle malformed SCT data (Result<T,E> error)

**Commit**: YES (Wave 1 foundation complete)
- Message: `feat(ct): implement CT foundation (log client, OCSP cache, SCT structures)`
- Files: `src/fafafa.ssl.ct.log.pas`, `src/fafafa.ssl.ocsp.cache.pas`, `src/fafafa.ssl.ct.sct.pas`, `tests/ct/*.pas`, `tests/ocsp/test_ocsp_cache.pas`
- Pre-commit: Run all Wave 1 tests

---

### Wave 2: Core Features (After Wave 1)

---

#### Task 4: SCT Signature Verification

**What to do**:
- Extend `src/fafafa.ssl.ct.sct.pas` with SCT signature verification
- Implement signature verification using CT log public keys (ECDSA P-256 or RSA)
- Implement SCT timestamp validation (not in future, within certificate validity)
- Integrate with CT log client (Task 1) to fetch log public keys
- Use SCT data structures (Task 3) for parsing
- Add Result<T,E> error handling
- Write test suite `tests/ct/test_sct_verification.pas` (36+ tests to match existing CT module)

**Must NOT do**:
- NO CT policy enforcement (Task 5)
- NO precertificate handling
- NO CT log submission

**Recommended Agent Profile**:
- **Category**: `unspecified-high`
  - Reason: Cryptographic signature verification, requires high-quality implementation
- **Skills**: []
- **Skills Evaluated but Omitted**:
  - `playwright`: Not needed (no browser interaction)
  - `git-master`: Will be needed for commit after Wave 2

**Parallelization**:
- **Can Run In Parallel**: YES
- **Parallel Group**: Wave 2 (with Tasks 5, 6, 7)
- **Blocks**: Task 9 (TLS handshake integration needs verification)
- **Blocked By**: Tasks 1, 3 (needs CT log client and SCT structures)

**References**:

**Pattern References**:
- `src/fafafa.ssl.openssl.api.ct.pas:434-476` - ValidateSCTList helper (validation pattern)
- `src/fafafa.ssl.ct.log.pas` - CT log client (from Task 1, fetch log public keys)
- `src/fafafa.ssl.ct.sct.pas` - SCT data structures (from Task 3)
- `src/fafafa.ssl.result.utils.pas:41-85` - Result<T,E> pattern

**API/Type References**:
- `src/fafafa.ssl.openssl.api.ct.pas:134-136` - SCT validation (SCT_validate, SCT_LIST_validate, SCT_get_validation_status)
- `src/fafafa.ssl.openssl.api.ct.pas:94-105` - CT_POLICY_EVAL_CTX (validation context)
- `src/fafafa.ssl.openssl.api.evp.pas` - EVP functions for signature verification

**External References**:
- RFC 6962 Section 3.2: https://www.rfc-editor.org/rfc/rfc6962.html#section-3.2 - SCT signature verification algorithm
- Chrome CT Policy: https://googlechrome.github.io/CertificateTransparency/ct_policy.html - SCT validation requirements

**Acceptance Criteria**:

**Automated Verification**:
```bash
# Agent runs test suite:
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/ct/test_sct_verification.pas -o./bin/test_sct_verification && ./bin/test_sct_verification
# Assert: Output contains "Total Tests: 36" (or more)
# Assert: Output contains "Passed: 36 (100.0%)"
# Assert: Output contains "Failed: 0 (0.0%)"
# Assert: Exit code 0
```

**Test Coverage** (36+ tests):
- [ ] Test: Verify SCT with valid ECDSA P-256 signature
- [ ] Test: Verify SCT with valid RSA signature
- [ ] Test: Reject SCT with invalid signature
- [ ] Test: Reject SCT with future timestamp
- [ ] Test: Reject SCT with timestamp outside certificate validity
- [ ] Test: Verify SCT list with multiple valid SCTs
- [ ] Test: Handle unknown log ID gracefully
- [ ] Test: Performance < 50ms per certificate (acceptance criteria)

**Commit**: NO (groups with Wave 2 completion)

---

