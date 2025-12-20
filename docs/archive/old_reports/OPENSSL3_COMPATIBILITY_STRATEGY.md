# OpenSSL 3.x Compatibility Strategy

**Date**: 2025-09-30  
**Version**: 1.0  
**Status**: Strategic Planning Document

---

## 📋 Executive Summary

OpenSSL 3.x introduced significant architectural changes that deprecated many low-level cryptographic APIs. This document outlines our strategy for achieving full compatibility with OpenSSL 3.x while maintaining backward compatibility with OpenSSL 1.1.x.

---

## 🎯 Current Status

### ✅ Working Modules (15/20 tested)

**Core Algorithms** - Use legacy APIs still available in OpenSSL 3.x:
- AES, DES, ChaCha20
- RSA, DSA, DH, EC, ECDH
- SHA-1/2, MD5, MD4, BLAKE2
- HMAC, BN, BIO, EVP (basic)

**Success Rate**: 95.1% for legacy-compatible modules

### ⚠️ Problematic Modules (2/20 tested)

| Module | Issue | OpenSSL 3.x Status | Priority |
|--------|-------|-------------------|----------|
| **SHA3** | Low-level API not exported | Must use EVP_MD_fetch | **HIGH** |
| **CMAC** | CMAC_* functions deprecated | Must use EVP_MAC | **MEDIUM** |

### ❓ Untested Modules (Likely Issues)

Based on OpenSSL 3.x deprecation patterns, these modules likely have similar issues:

| Module | Risk | Reason |
|--------|------|--------|
| **MODES** (GCM/CCM/XTS/OCB) | High | Low-level mode functions typically internal |
| **PKCS7/PKCS12** | Medium | May use deprecated APIs |
| **CMS** | Medium | Related to PKCS7 |
| **ENGINE** | High | **Entirely deprecated in 3.x** |
| **Poly1305** | Medium | May require EVP interface |
| **SipHash** | Medium | May require EVP interface |

---

## 📊 OpenSSL 3.x Architecture Changes

### Provider-Based Architecture

```
OpenSSL 1.1.x:                    OpenSSL 3.x:
┌─────────────┐                   ┌──────────────┐
│  Algorithm  │                   │  Algorithm   │
│   Direct    │                   │  via EVP_*   │
│   Access    │                   │   _fetch()   │
├─────────────┤                   ├──────────────┤
│   libssl    │                   │   libssl     │
│  libcrypto  │                   │  libcrypto   │
└─────────────┘                   ├──────────────┤
                                  │  Providers   │
                                  │  (default,   │
                                  │   legacy,    │
                                  │   fips)      │
                                  └──────────────┘
```

### API Migration Paths

| Old API (1.1.x) | New API (3.x) | Status |
|-----------------|---------------|--------|
| `SHA3_256(...)` | `EVP_MD_fetch("SHA3-256", ...)` | **Required** |
| `CMAC_Init(...)` | `EVP_MAC_fetch("CMAC", ...)` | **Required** |
| `AES_encrypt(...)` | Still works (legacy) | **OK** |
| `RSA_sign(...)` | Still works (legacy) | **OK** |
| `ENGINE_*` | N/A (removed) | **Deprecated** |

---

## 🔧 Compatibility Strategies

### Strategy 1: Runtime Detection (Recommended)

Detect OpenSSL version at runtime and use appropriate API:

```pascal
function GetOpenSSLMajorVersion: Integer;
var
  Ver: Cardinal;
begin
  Ver := OpenSSL_version_num();
  Result := (Ver shr 28) and $F;
end;

function SHA3_256Hash(const Data: TBytes): TBytes;
begin
  if GetOpenSSLMajorVersion >= 3 then
    Result := SHA3_256Hash_EVP(Data)    // Modern API
  else
    Result := SHA3_256Hash_Legacy(Data); // Legacy API
end;
```

**Pros**:
- ✅ Single binary supports both versions
- ✅ Automatic fallback
- ✅ Best user experience

**Cons**:
- ⚠️ More code complexity
- ⚠️ Both code paths must be maintained

### Strategy 2: Compile-Time Selection

Use conditional compilation:

```pascal
{$IFDEF OPENSSL3}
  // Modern EVP-based implementation
{$ELSE}
  // Legacy direct implementation
{$ENDIF}
```

**Pros**:
- ✅ Simpler code per build
- ✅ Optimized for target version

**Cons**:
- ❌ Need separate binaries
- ❌ More build complexity

### Strategy 3: EVP-Only (Future)

Migrate entirely to EVP APIs:

```pascal
// Always use EVP APIs
function SHA3_256Hash(const Data: TBytes): TBytes;
begin
  Result := SHA3_256Hash_EVP(Data);
end;
```

**Pros**:
- ✅ Simplest code
- ✅ Future-proof
- ✅ Works on both versions (EVP available in 1.1.x)

**Cons**:
- ❌ Requires more upfront work
- ❌ Some performance overhead

---

## 🎯 Recommended Implementation Plan

### Phase 1: Critical Fixes (Priority: HIGH)

**Goal**: Make SHA3 and CMAC work on OpenSSL 3.x

**Tasks**:
1. Implement EVP-based SHA3 wrapper functions
   - `EVP_MD_fetch("SHA3-256", ...)` 
   - `EVP_MD_fetch("SHA3-512", ...)`
   - Runtime version detection
   - Fallback to legacy API for 1.1.x

2. Implement EVP-based CMAC wrapper functions
   - `EVP_MAC_fetch("CMAC", ...)`
   - Runtime version detection
   - Fallback to legacy API for 1.1.x

**Estimated Time**: 8-16 hours  
**Priority**: HIGH  
**Impact**: Restores SHA3 and CMAC functionality

### Phase 2: AEAD Modes (Priority: MEDIUM)

**Goal**: Ensure GCM, CCM, XTS, OCB work properly

**Approach**: These modes are **already** EVP-based in our high-level wrappers. Test to confirm:

```pascal
// We likely already do this:
EVP_aes_256_gcm()  // Returns EVP_CIPHER
EVP_aes_256_ccm()
```

**Tasks**:
1. Audit MODES module usage
2. Verify all wrappers use EVP interface
3. Test on OpenSSL 3.x
4. Document any limitations

**Estimated Time**: 4-8 hours  
**Priority**: MEDIUM

### Phase 3: Comprehensive Testing (Priority: MEDIUM)

**Goal**: Test all remaining modules on OpenSSL 3.x

**Modules to Test**:
- PKCS7, PKCS12, CMS
- POLY1305, SIPHASH
- ENGINE (document as unavailable)
- X509, PEM, ASN1
- OCSP, CT, TS

**Tasks**:
1. Create test programs for each module
2. Run on OpenSSL 3.x
3. Document issues
4. Implement fixes where needed

**Estimated Time**: 16-24 hours  
**Priority**: MEDIUM

### Phase 4: Documentation & Migration Guide (Priority: HIGH)

**Goal**: Help users migrate to OpenSSL 3.x

**Deliverables**:
1. **Migration Guide** - How to update applications
2. **API Reference** - Document version-specific APIs
3. **Best Practices** - Recommend modern patterns
4. **Breaking Changes** - Clear list of incompatibilities

**Estimated Time**: 8-12 hours  
**Priority**: HIGH (for release)

---

## 📝 Specific Module Plans

### SHA3 Module

**Current Status**: Functions not available in OpenSSL 3.x

**Implementation**:

```pascal
// Add to fafafa.ssl.openssl.evp.pas
type
  TEVP_MD_fetch = function(ctx: Pointer; const algorithm: PAnsiChar;
                           const properties: PAnsiChar): PEVP_MD; cdecl;
  TEVP_MD_free = procedure(md: PEVP_MD); cdecl;

var
  EVP_MD_fetch: TEVP_MD_fetch = nil;
  EVP_MD_free: TEVP_MD_free = nil;

// Add to fafafa.ssl.openssl.sha3.pas
function SHA3_256Hash_EVP(const Data: TBytes): TBytes;
var
  MD: PEVP_MD;
  Ctx: PEVP_MD_CTX;
  HashLen: Cardinal;
begin
  SetLength(Result, SHA3_256_DIGEST_LENGTH);
  
  MD := EVP_MD_fetch(nil, 'SHA3-256', nil);
  if MD = nil then
    raise Exception.Create('SHA3-256 not available');
  
  try
    Ctx := EVP_MD_CTX_new();
    try
      if EVP_DigestInit_ex(Ctx, MD, nil) <> 1 then
        raise Exception.Create('EVP_DigestInit_ex failed');
        
      if EVP_DigestUpdate(Ctx, @Data[0], Length(Data)) <> 1 then
        raise Exception.Create('EVP_DigestUpdate failed');
        
      if EVP_DigestFinal_ex(Ctx, @Result[0], HashLen) <> 1 then
        raise Exception.Create('EVP_DigestFinal_ex failed');
    finally
      EVP_MD_CTX_free(Ctx);
    end;
  finally
    EVP_MD_free(MD);
  end;
end;

function SHA3_256Hash(const Data: TBytes): TBytes;
begin
  // Try modern API first
  if Assigned(EVP_MD_fetch) then
    Result := SHA3_256Hash_EVP(Data)
  // Fall back to legacy
  else if Assigned(SHA3_256) then
    Result := SHA3_256Hash_Legacy(Data)
  else
    raise Exception.Create('SHA3-256 not available');
end;
```

**Testing**: Verify on both OpenSSL 1.1.x and 3.x

### CMAC Module

**Current Status**: CMAC_* functions deprecated in OpenSSL 3.x

**Implementation**:

```pascal
// Add to fafafa.ssl.openssl.mac.pas (new file)
type
  TEVP_MAC_fetch = function(ctx: Pointer; const algorithm: PAnsiChar;
                            const properties: PAnsiChar): PEVP_MAC; cdecl;
  TEVP_MAC_free = procedure(mac: PEVP_MAC); cdecl;
  TEVP_MAC_CTX_new = function(mac: PEVP_MAC): PEVP_MAC_CTX; cdecl;
  TEVP_MAC_CTX_free = procedure(ctx: PEVP_MAC_CTX); cdecl;
  TEVP_MAC_init = function(ctx: PEVP_MAC_CTX; const key: PByte;
                           keylen: size_t; params: Pointer): Integer; cdecl;
  TEVP_MAC_update = function(ctx: PEVP_MAC_CTX; const data: PByte;
                             datalen: size_t): Integer; cdecl;
  TEVP_MAC_final = function(ctx: PEVP_MAC_CTX; out_: PByte; outl: Psize_t;
                            outsize: size_t): Integer; cdecl;

// Wrapper function
function CMAC_Compute_EVP(const Key: TBytes; const Data: TBytes;
                          const Cipher: string): TBytes;
var
  MAC: PEVP_MAC;
  Ctx: PEVP_MAC_CTX;
  OutLen: size_t;
begin
  SetLength(Result, 16); // AES block size
  
  MAC := EVP_MAC_fetch(nil, 'CMAC', nil);
  if MAC = nil then
    raise Exception.Create('CMAC not available');
  
  try
    Ctx := EVP_MAC_CTX_new(MAC);
    try
      // Set cipher parameter
      // ...
      
      if EVP_MAC_init(Ctx, @Key[0], Length(Key), nil) <> 1 then
        raise Exception.Create('EVP_MAC_init failed');
        
      if EVP_MAC_update(Ctx, @Data[0], Length(Data)) <> 1 then
        raise Exception.Create('EVP_MAC_update failed');
        
      if EVP_MAC_final(Ctx, @Result[0], @OutLen, Length(Result)) <> 1 then
        raise Exception.Create('EVP_MAC_final failed');
        
      SetLength(Result, OutLen);
    finally
      EVP_MAC_CTX_free(Ctx);
    end;
  finally
    EVP_MAC_free(MAC);
  end;
end;
```

**Testing**: Verify against test vectors

### ENGINE Module

**Status**: **Completely deprecated in OpenSSL 3.x**

**Recommendation**: 
- Mark as **not supported** in OpenSSL 3.x
- Document migration to Provider API
- Provide clear error messages

```pascal
function LoadENGINEFunctions: Boolean;
begin
  if GetOpenSSLMajorVersion >= 3 then
  begin
    // ENGINE API deprecated in OpenSSL 3.x
    // Use Provider API instead
    Result := False;
    Exit;
  end;
  
  // Load legacy ENGINE functions for OpenSSL 1.1.x
  // ...
end;
```

---

## 📚 Key Resources

### OpenSSL 3.x Documentation

- **Migration Guide**: https://www.openssl.org/docs/man3.0/man7/migration_guide.html
- **Provider Concept**: https://www.openssl.org/docs/man3.0/man7/provider.html
- **EVP_MD API**: https://www.openssl.org/docs/man3.0/man3/EVP_MD_fetch.html
- **EVP_MAC API**: https://www.openssl.org/docs/man3.0/man3/EVP_MAC_fetch.html
- **Deprecated APIs**: https://www.openssl.org/docs/man3.0/man7/openssl-deprecated.html

### Testing Resources

- **Test Vectors**: NIST CAVP test vectors
- **OpenSSL Tests**: OpenSSL's own test suite as reference

---

## ✅ Success Criteria

### Minimum Viable Product (MVP)

- [x] Core SSL/TLS works on OpenSSL 3.x ✅
- [x] Common hash functions work ✅  
- [x] Common ciphers work ✅
- [x] RSA/EC signing works ✅
- [ ] SHA3 works on OpenSSL 3.x
- [ ] CMAC works on OpenSSL 3.x
- [ ] All AEAD modes work

### Complete Solution

- [ ] All modules tested on OpenSSL 3.x
- [ ] Comprehensive documentation
- [ ] Migration guide for users
- [ ] Performance benchmarks
- [ ] CI/CD tests for both versions

---

## 📊 Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| More modules fail on 3.x | High | Medium | Test early, document |
| Performance regression | Low | Low | Benchmark EVP calls |
| Breaking API changes | Medium | High | Version wrappers |
| User migration issues | High | High | Good documentation |

---

## 🎯 Conclusion

**OpenSSL 3.x compatibility is achievable** with focused effort on:

1. **Immediate**: Fix SHA3 and CMAC (HIGH priority)
2. **Short-term**: Test and fix other problematic modules
3. **Medium-term**: Create comprehensive migration guide
4. **Long-term**: Consider EVP-only architecture

**Estimated Total Effort**: 40-60 hours for complete OpenSSL 3.x compatibility

**Recommended Approach**: 
- Use **Runtime Detection** strategy (Strategy 1)
- Focus on **high-priority modules first** (SHA3, CMAC)
- Test incrementally, document as we go
- Maintain backward compatibility with OpenSSL 1.1.x

---

**Next Action**: Implement Phase 1 (SHA3 and CMAC EVP wrappers) to restore critical functionality.
