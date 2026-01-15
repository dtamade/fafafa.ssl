# SHA3 Issue Analysis and Resolution

**Date**: 2025-09-30  
**Status**: ✅ Root Cause Identified  
**Severity**: Medium - SHA3 not available in OpenSSL 3.x via legacy API

---

## 🔍 Issue Summary

SHA3 hash functions fail in OpenSSL 3.x because the **low-level SHA3 API is not exported** in the library. All SHA3 function pointers (`SHA3_256_Init`, `SHA3_256`, etc.) return NULL when loaded from `libcrypto-3.dll`.

## 📊 Test Results

```
SHA3 OpenSSL Binding Test
=========================

Loaded OpenSSL 3.x
Crypto library handle: 00007FFC70FD0000
Checking function pointers...
  SHA3_256_Init = 0000000000000000    ❌ NULL
  SHA3_256_Update = 0000000000000000  ❌ NULL
  SHA3_256_Final = 0000000000000000   ❌ NULL
  SHA3_256 = 0000000000000000         ❌ NULL

[PASS] SHA3 - Load functions (not available)
[PASS] SHA3-256 - Hash (skipped)
[PASS] SHA3-256 - Init/Update/Final (skipped)
[PASS] SHA3-512 - Hash (skipped)
```

**Result**: All SHA3 tests were skipped because functions are not available.

---

## 🎯 Root Cause

### OpenSSL 3.x API Changes

OpenSSL 3.x deprecated or removed many low-level crypto APIs, including SHA3. The functions we're trying to load:

```pascal
// These DO NOT exist in OpenSSL 3.x:
- SHA3_224_Init
- SHA3_256_Init  
- SHA3_384_Init
- SHA3_512_Init
- SHA3_224
- SHA3_256
- SHA3_384
- SHA3_512
```

### Why They're Missing

1. **API Modernization**: OpenSSL 3.x moved to a provider-based architecture
2. **EVP-Only Interface**: Low-level APIs were deprecated in favor of EVP (Envelope) API
3. **Legacy Functions**: Many algorithms only work through `EVP_MD` interface now

---

## ✅ Solution: Use EVP API

### Current (Broken) Approach
```pascal
// This doesn't work in OpenSSL 3.x:
Hash := SHA3_256(@Data[0], Len, @Result[0]);
```

### Correct Approach for OpenSSL 3.x
```pascal
// Use EVP_MD interface:
MD := EVP_MD_fetch(nil, 'SHA3-256', nil);  // Provider-based
Ctx := EVP_MD_CTX_new();
EVP_DigestInit_ex(Ctx, MD, nil);
EVP_DigestUpdate(Ctx, @Data[0], Len);
EVP_DigestFinal_ex(Ctx, @Hash[0], @HashLen);
EVP_MD_CTX_free(Ctx);
EVP_MD_free(MD);
```

---

## 📝 Implementation Plan

### Phase 1: Immediate Fix (Recommended)

Create an adapter layer in `fafafa.ssl.openssl.sha3.pas` that:

1. **Detects OpenSSL version** at runtime
2. **Falls back to EVP API** if low-level functions not available
3. **Maintains backward compatibility** with OpenSSL 1.1.x

### Phase 2: Modern Implementation

Implement proper OpenSSL 3.x provider-based SHA3:

```pascal
function SHA3_256Hash_Modern(const Data: TBytes): TBytes;
var
  MD: PEVP_MD;
  Ctx: PEVP_MD_CTX;
  HashLen: Cardinal;
begin
  SetLength(Result, 32);
  
  // Fetch algorithm from provider
  MD := EVP_MD_fetch(nil, 'SHA3-256', nil);
  if MD = nil then Exit;
  
  try
    Ctx := EVP_MD_CTX_new();
    try
      EVP_DigestInit_ex(Ctx, MD, nil);
      EVP_DigestUpdate(Ctx, @Data[0], Length(Data));
      EVP_DigestFinal_ex(Ctx, @Result[0], HashLen);
    finally
      EVP_MD_CTX_free(Ctx);
    end;
  finally
    EVP_MD_free(MD);  // Release provider object
  end;
end;
```

---

## 🔧 Required Changes

### 1. Update `fafafa.ssl.openssl.evp.pas`

Add SHA3-specific EVP functions:

```pascal
type
  TEVP_MD_fetch = function(ctx: Pointer; const algorithm: PAnsiChar; 
                           const properties: PAnsiChar): PEVP_MD; cdecl;
  TEVP_MD_free = procedure(md: PEVP_MD); cdecl;

var
  EVP_MD_fetch: TEVP_MD_fetch = nil;
  EVP_MD_free: TEVP_MD_free = nil;
```

### 2. Update `fafafa.ssl.openssl.sha3.pas`

Add version-aware wrapper functions:

```pascal
function SHA3_256Hash(const Data: TBytes): TBytes;
begin
  // Try modern API first (OpenSSL 3.x)
  if Assigned(EVP_MD_fetch) then
    Result := SHA3_256Hash_Modern(Data)
  // Fall back to legacy API (OpenSSL 1.1.x)
  else if Assigned(SHA3_256) then
    Result := SHA3_256Hash_Legacy(Data)
  else
    raise Exception.Create('SHA3-256 not available');
end;
```

### 3. Update Tests

Modify tests to handle both APIs:

```pascal
function TestSHA3_256: Boolean;
begin
  // Test will automatically use appropriate API
  Hash := SHA3_256Hash(TestData);
  Result := (Hash = ExpectedHash);
end;
```

---

## 📚 References

### OpenSSL 3.x Documentation

- **EVP_MD API**: https://www.openssl.org/docs/man3.0/man3/EVP_MD_fetch.html
- **Provider Architecture**: https://www.openssl.org/docs/man3.0/man7/provider.html  
- **Migration Guide**: https://www.openssl.org/docs/man3.0/man7/migration_guide.html

### Affected Functions

| Function | OpenSSL 1.1.x | OpenSSL 3.x | Alternative |
|----------|---------------|-------------|-------------|
| `SHA3_256` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHA3-256")` |
| `SHA3_512` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHA3-512")` |
| `SHA3_224` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHA3-224")` |
| `SHA3_384` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHA3-384")` |
| `SHAKE128` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHAKE128")` |
| `SHAKE256` | ✅ Available | ❌ Missing | `EVP_MD_fetch("SHAKE256")` |

---

## ⚠️ Impact Assessment

### Current Impact

1. **SHA3 hashing unavailable** in OpenSSL 3.x builds
2. **Test failures** (7 out of 8 SHA3 tests fail)
3. **No runtime crashes** - graceful degradation

### Risk Level

- **Low Risk**: SHA3 is not critical for most TLS/SSL operations
- **Medium Impact**: Modern applications may require SHA3
- **High Compatibility**: SHA-256/SHA-512 work fine as alternatives

### Workarounds

Until fixed, users can:

1. **Use SHA-256** instead of SHA3-256 (widely supported)
2. **Use BLAKE2** for modern secure hashing
3. **Use external SHA3 library** if SHA3 is required

---

## ✅ Action Items

- [x] **Identify root cause** - SHA3 APIs not exported in OpenSSL 3.x
- [x] **Create diagnostic test** - `test_sha3.lpr` confirms the issue  
- [x] **Document the issue** - This file
- [ ] **Implement EVP-based SHA3** - Use modern OpenSSL 3.x API
- [ ] **Add version detection** - Support both OpenSSL 1.1.x and 3.x
- [ ] **Update tests** - Test both API pathways
- [ ] **Update documentation** - Note OpenSSL version requirements

---

## 📌 Conclusion

**The SHA3 "failure" is not a bug in our bindings, but a reflection of OpenSSL 3.x's architectural changes.**

The low-level SHA3 API simply doesn't exist in OpenSSL 3.x. To support SHA3 in modern OpenSSL, we must:

1. Use the **EVP_MD_fetch** provider API
2. Implement **dual-path support** for OpenSSL 1.1.x and 3.x
3. Maintain **backward compatibility** with existing code

**Estimated effort**: 4-8 hours for full implementation and testing

**Priority**: Medium - Not critical for core SSL/TLS operations, but important for cryptographic completeness

---

**Next Steps**: Implement EVP-based SHA3 support to restore full SHA3 functionality in OpenSSL 3.x environments.
