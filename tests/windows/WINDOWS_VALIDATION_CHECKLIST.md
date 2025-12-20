# Windows Validation Checklist

**Version**: 1.0  
**Date**: 2025-10-29  
**Target**: fafafa.ssl Windows Production Readiness  
**Validator**: [Your Name]  
**Environment**: Windows 10/11 x64

---

## 📋 Pre-Validation Setup

### Environment Requirements
- [ ] Windows 10/11 x64 (Build 19041 or later)
- [ ] Lazarus IDE installed (3.0+ recommended)
- [ ] OpenSSL 3.x installed (check with `openssl version`)
- [ ] PowerShell 5.1+ available
- [ ] Administrator privileges available (if needed)

### File Verification
- [ ] All source files present in `src/` directory
- [ ] Test programs present in `tests/windows/` directory
- [ ] `.lpi` project files configured correctly
- [ ] PowerShell scripts executable

### OpenSSL Configuration
- [ ] `libssl-3-x64.dll` in PATH or test directory
- [ ] `libcrypto-3-x64.dll` in PATH or test directory
- [ ] OpenSSL config file accessible (if required)
- [ ] No conflicting OpenSSL versions in PATH

---

## 🧪 Validation Test Matrix

### Phase 1: Quick Smoke Test (2 minutes)

**Script**: `Run-QuickValidation.ps1`

| Test | Expected Result | Status | Notes |
|------|----------------|--------|-------|
| OpenSSL library load | ✅ Success | [ ] | libssl-3-x64.dll found and loaded |
| Version check | ✅ 3.x.x | [ ] | OpenSSL_version returns 3.x.x |
| Basic initialization | ✅ Success | [ ] | OPENSSL_init_ssl succeeds |
| Cleanup | ✅ No leaks | [ ] | Clean shutdown without errors |

**Success Criteria**: All 4 tests pass in < 2 minutes

---

### Phase 2: Certificate Loading Tests (5-10 minutes)

**Program**: `test_cert_load.exe`

#### Scenario 1: Basic Certificate Operations
- [ ] Load single PEM certificate
- [ ] Load certificate chain
- [ ] Load from memory buffer
- [ ] Verify certificate attributes (subject, issuer, dates)

#### Scenario 2: Multiple Format Support
- [ ] Load PEM format certificates
- [ ] Load DER format certificates
- [ ] Load PKCS#12 bundles
- [ ] Handle invalid format gracefully

#### Scenario 3: Chain Validation
- [ ] Load root CA certificate
- [ ] Load intermediate CA certificate
- [ ] Load end-entity certificate
- [ ] Verify chain of trust

#### Scenario 4: Error Handling
- [ ] Invalid file path → proper error
- [ ] Corrupted certificate → proper error
- [ ] Expired certificate → detection works
- [ ] Missing dependencies → clear error message

#### Scenario 5: Resource Management
- [ ] No memory leaks after 100 operations
- [ ] Proper cleanup on error paths
- [ ] Handle large certificate files (>1MB)
- [ ] Concurrent certificate loading (if applicable)

#### Scenario 6: Real-World Scenarios
- [ ] Load Let's Encrypt certificate chain
- [ ] Load self-signed certificate
- [ ] Load wildcard certificate
- [ ] Load certificate with extensions (SAN, etc.)

**Success Criteria**: All scenarios pass with 0 errors

---

### Phase 3: Factory Mode Validation (3-5 minutes)

**Program**: `test_factory_mode.exe`

#### Core Factory Operations
- [ ] Create certificate factory instance
- [ ] Configure factory with custom parameters
- [ ] Batch certificate generation (10 certs)
- [ ] Factory cleanup and resource release

#### Factory Features
- [ ] Custom certificate templates
- [ ] Serial number management
- [ ] Validity period configuration
- [ ] Key type selection (RSA/ECC)

#### Production Scenarios
- [ ] Generate 100 certificates in sequence
- [ ] Generate certificates with different key sizes
- [ ] Generate certificates with extensions
- [ ] Performance: < 1s per certificate

**Success Criteria**: Factory creates valid certificates efficiently

---

### Phase 4: Integration Tests (10-15 minutes)

**Script**: `Run-WindowsValidation.ps1`

#### OpenSSL Core Integration
- [ ] All 65 OpenSSL modules load successfully
- [ ] No DLL conflicts or version mismatches
- [ ] Error handling works across all modules
- [ ] Memory management correct in all paths

#### Cross-Module Integration
- [ ] Certificate + Key generation pipeline
- [ ] Certificate + TLS connection
- [ ] Certificate + File I/O
- [ ] Certificate + Error reporting

#### Real-World Workflows
- [ ] TLS server certificate setup
- [ ] TLS client certificate authentication
- [ ] Certificate revocation checking
- [ ] Certificate chain export/import

**Success Criteria**: All integration tests pass

---

### Phase 5: Performance Validation (5 minutes)

#### Benchmarks
- [ ] Certificate load time: < 10ms per cert
- [ ] Memory usage: < 50MB for 100 certs
- [ ] Startup time: < 500ms
- [ ] Cleanup time: < 100ms

#### Stress Tests
- [ ] Load 1000 certificates sequentially
- [ ] Handle 10 concurrent operations
- [ ] Process 100MB certificate bundle
- [ ] 24-hour stability test (optional)

**Success Criteria**: Performance within acceptable ranges

---

## 🔧 Troubleshooting Guide

### Common Issues and Solutions

#### Issue 1: OpenSSL DLL Not Found
**Symptoms**: "libssl-3-x64.dll not found" error

**Solutions**:
1. Copy DLLs to test executable directory
2. Add OpenSSL bin directory to PATH
3. Install OpenSSL for Windows from official source
4. Check DLL architecture matches (x64 vs x86)

#### Issue 2: Access Violation / Crash
**Symptoms**: Program crashes with AV or segfault

**Solutions**:
1. Check OpenSSL version compatibility (must be 3.x)
2. Verify no mixing of OpenSSL 1.1 and 3.x DLLs
3. Run in debug mode to identify specific function
4. Check for null pointer dereferences

#### Issue 3: Certificate Validation Fails
**Symptoms**: Valid certificates reported as invalid

**Solutions**:
1. Check system time is correct
2. Verify root CA certificates installed
3. Check certificate chain order
4. Ensure intermediate certificates included

#### Issue 4: Memory Leaks Detected
**Symptoms**: Growing memory usage over time

**Solutions**:
1. Check all X509_free() calls present
2. Verify SSL_CTX cleanup
3. Check error path cleanup
4. Use HeapMemView or similar tool to identify leaks

#### Issue 5: Performance Issues
**Symptoms**: Tests run very slowly

**Solutions**:
1. Check antivirus not scanning DLLs repeatedly
2. Disable debug logging if enabled
3. Ensure running Release build, not Debug
4. Check disk I/O not bottleneck (SSD preferred)

---

## 📊 Validation Results Summary

### Overall Status
- [ ] **PASS** - All tests passed, production ready
- [ ] **CONDITIONAL PASS** - Minor issues, documented workarounds
- [ ] **FAIL** - Critical issues, not ready for production

### Test Summary
| Phase | Total Tests | Passed | Failed | Duration |
|-------|-------------|--------|--------|----------|
| Phase 1: Smoke Test | 4 | | | |
| Phase 2: Certificate Loading | 24 | | | |
| Phase 3: Factory Mode | 12 | | | |
| Phase 4: Integration | 16 | | | |
| Phase 5: Performance | 8 | | | |
| **TOTAL** | **64** | | | |

### Issues Found
| ID | Severity | Description | Status | Workaround |
|----|----------|-------------|--------|------------|
| | | | | |

### Performance Results
| Metric | Expected | Actual | Status |
|--------|----------|--------|--------|
| Cert Load Time | < 10ms | | |
| Memory Usage | < 50MB | | |
| Startup Time | < 500ms | | |
| Cleanup Time | < 100ms | | |

---

## ✅ Sign-Off

### Validator Certification
```
I certify that I have:
- Executed all validation tests as specified
- Documented all issues encountered
- Verified all success criteria met
- Reviewed the codebase for Windows-specific concerns

Validator: _________________________
Date: _____________________________
Signature: ________________________
```

### Production Readiness Decision
- [ ] **APPROVED** for Windows production deployment
- [ ] **APPROVED WITH CONDITIONS** (see notes)
- [ ] **NOT APPROVED** (critical issues found)

**Notes**:
```




```

---

## 📝 Next Steps

### If APPROVED
1. [ ] Tag release version
2. [ ] Update documentation with Windows specifics
3. [ ] Create deployment package
4. [ ] Notify stakeholders

### If CONDITIONAL PASS
1. [ ] Document all workarounds
2. [ ] Create issue tracking tickets
3. [ ] Schedule follow-up validation
4. [ ] Communicate limitations to users

### If FAILED
1. [ ] Execute rollback plan (see ROLLBACK_PLAN.md)
2. [ ] Create detailed bug reports
3. [ ] Assign priority for fixes
4. [ ] Schedule re-validation after fixes

---

## 📚 Reference Documents
- `VALIDATION_BUNDLE.md` - Complete file listing
- `ROLLBACK_PLAN.md` - Rollback procedures
- `WINDOWS_VALIDATION_REPORT.md` - Final report template
- `Run-WindowsValidation.ps1` - Main test runner
- `Run-QuickValidation.ps1` - Quick smoke test

---

**Document Version**: 1.0  
**Last Updated**: 2025-10-29  
**Maintained By**: fafafa.ssl Team
