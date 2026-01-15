# Migration Guide

Guide for migrating to fafafa.ssl from other SSL/TLS libraries.

---

## 📋 Table of Contents

1. [From Direct OpenSSL Usage](#from-direct-openssl-usage)
2. [From Other Pascal SSL Libraries](#from-other-pascal-ssl-libraries)
3. [From Indy (Internet Direct)](#from-indy-internet-direct)
4. [Platform Migration (Windows → Zero Dependency)](#platform-migration)
5. [Version Migration (OpenSSL 1.1 → 3.x)](#version-migration)

---

## 🔄 From Direct OpenSSL Usage

If you're currently using OpenSSL API directly in Pascal, fafafa.ssl provides a drop-in replacement with benefits.

### Before (Direct OpenSSL)

```pascal
uses
  OpenSSL_lib, // Some third-party OpenSSL binding
  ssl_lib;

var
  Ctx: PSSL_CTX;
  SSL: PSSL;
begin
  SSL_library_init();
  Ctx := SSL_CTX_new(TLS_client_method());
  // ... manual setup
end;
```

### After (fafafa.ssl)

```pascal
uses
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  // Automatic initialization and best backend selection
  Lib := CreateSSLLibrary(sslAutoDetect);
  Lib.Initialize;
  Ctx := Lib.CreateContext(sslCtxClient);
  // Clean, high-level API
end;
```

### Benefits
- ✅ **Simpler API**: Less boilerplate code
- ✅ **Auto-detection**: Automatically chooses best backend
- ✅ **Multi-backend**: Can switch to WinSSL on Windows (zero dependencies!)
- ✅ **Better error handling**: Structured error management
- ✅ **Resource management**: Automatic cleanup via interfaces

---

## 🔄 From Other Pascal SSL Libraries

### Common Scenario: Switching from synopse/mORMot

#### Before

```pascal
uses
  SynCrypto, SynOpenSSL;

var
  Hash: THash256;
begin
  SHA256Weak('data', Hash);
end;
```

#### After

```pascal
uses
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

function SHA256Hash(const Data: AnsiString): TBytes;
var
  MD: PEVP_MD;
  Ctx: PEVP_MD_CTX;
  Hash: array[0..31] of Byte;
  HashLen: Cardinal;
begin
  SetLength(Result, 32);
  MD := EVP_MD_fetch(nil, 'SHA256', nil);
  Ctx := EVP_MD_CTX_new;
  
  EVP_DigestInit_ex(Ctx, MD, nil);
  EVP_DigestUpdate(Ctx, PAnsiChar(Data), Length(Data));
  EVP_DigestFinal_ex(Ctx, @Hash, @HashLen);
  
  Move(Hash, Result[0], 32);
  
  EVP_MD_CTX_free(Ctx);
  EVP_MD_free(MD);
end;
```

---

## 🔄 From Indy (Internet Direct)

### HTTPS Client Migration

#### Before (Indy)

```pascal
uses
  IdHTTP, IdSSLOpenSSL;

var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Response: string;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    HTTP.IOHandler := SSLHandler;
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    Response := HTTP.Get('https://api.example.com');
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;
```

#### After (fafafa.ssl + Your HTTP Library)

```pascal
uses
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // Initialize SSL
  Lib := CreateSSLLibrary(sslAutoDetect);
  Lib.Initialize;
  
  Ctx := Lib.CreateContext(sslCtxClient);
  Ctx.SetServerName('api.example.com');
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  
  // Create connection (integrate with your socket library)
  Conn := Ctx.CreateConnection(YourSocket);
  Conn.Connect;
  
  // Send/receive data
  // (HTTP protocol handling with your preferred library)
end;
```

**Note**: fafafa.ssl provides SSL/TLS layer. You still need an HTTP library for protocol handling.

---

## 🚀 Platform Migration

### Windows: Eliminate OpenSSL Dependency

One of fafafa.ssl's killer features: **Zero-dependency deployment on Windows**.

#### Before (Deploying with OpenSSL)

```
MyApp.exe             (500 KB)
libcrypto-3-x64.dll   (5.2 MB)
libssl-3-x64.dll      (1.1 MB)
-----------------------------------
Total: 6.8 MB
```

#### After (Using WinSSL)

```
MyApp.exe             (520 KB)  ← No DLLs needed!
-----------------------------------
Total: 520 KB (92% smaller!)
```

#### Code Change

```pascal
// Just change the backend selection
{$IFDEF WINDOWS}
Lib := CreateSSLLibrary(sslWinSSL);  // Windows native!
{$ELSE}
Lib := CreateSSLLibrary(sslOpenSSL); // OpenSSL on Linux/macOS
{$ENDIF}

// Rest of code stays the same!
```

**Benefits**:
- ✅ **No DLL dependencies**
- ✅ **Smaller deployment** (92% size reduction)
- ✅ **Automatic security updates** (via Windows Update)
- ✅ **Enterprise CA trust** (automatic)
- ✅ **FIPS compliance** (if enabled in Windows)

---

## 🔄 Version Migration

### OpenSSL 1.1.x to 3.x

fafafa.ssl supports both, but 3.x is recommended for new projects.

#### API Differences Handled Automatically

| Feature | OpenSSL 1.1.x | OpenSSL 3.x | fafafa.ssl |
|---------|---------------|-------------|------------|
| MD Fetch | `EVP_get_digestbyname` | `EVP_MD_fetch` | ✅ Both |
| Cipher Fetch | `EVP_get_cipherbyname` | `EVP_CIPHER_fetch` | ✅ Both |
| Cleanup | Manual | Automatic | ✅ Managed |

#### Code Example

```pascal
uses
  fafafa.ssl.openssl.api.evp;

var
  MD: PEVP_MD;
begin
  // Works with both 1.1.x and 3.x!
  MD := EVP_MD_fetch(nil, 'SHA256', nil);
  // On 1.1.x, automatically falls back to EVP_get_digestbyname
  
  // Always clean up (works on both versions)
  EVP_MD_free(MD);
end;
```

---

## 📚 Migration Strategies

### Strategy 1: Gradual Migration

**Best for**: Large existing codebases

1. **Phase 1**: Parallel implementation
   - Keep existing SSL code
   - Add fafafa.ssl alongside
   - Test in non-critical paths

2. **Phase 2**: Partial replacement
   - Replace non-critical SSL usage
   - Monitor for issues
   - Gain confidence

3. **Phase 3**: Full migration
   - Replace all SSL usage
   - Remove old dependencies
   - Clean up

### Strategy 2: Clean Break

**Best for**: New features or rewrites

1. Remove old SSL dependencies
2. Add fafafa.ssl
3. Rewrite SSL code with new API
4. Comprehensive testing

---

## 🐛 Common Migration Issues

### Issue 1: Missing CA Certificates

**Symptom**: Certificate verification fails after migration

**Solution**:
```pascal
// fafafa.ssl auto-loads system CA on Linux
// But you can specify custom CA if needed:
Ctx.LoadCAFile('/path/to/ca-bundle.crt');
// Or use Windows certificate store (WinSSL)
```

### Issue 2: Protocol Version Changes

**Symptom**: Handshake failures after migration

**Solution**:
```pascal
// Explicitly set protocol versions
Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### Issue 3: Different Error Codes

**Symptom**: Error handling breaks

**Solution**:
```pascal
// Use fafafa.ssl's unified error interface
try
  Conn.Connect;
except
  on E: Exception do
    WriteLn('SSL Error: ', E.Message);
end;
```

---

## ✅ Migration Checklist

### Pre-Migration
- [ ] Review current SSL usage
- [ ] Identify all SSL-related code
- [ ] Check OpenSSL version requirements
- [ ] Plan testing strategy

### During Migration
- [ ] Install fafafa.ssl
- [ ] Update unit clauses
- [ ] Convert API calls
- [ ] Test each component
- [ ] Handle errors properly

### Post-Migration
- [ ] Run full test suite
- [ ] Performance testing
- [ ] Security audit
- [ ] Document changes
- [ ] Remove old dependencies

---

## 💡 Best Practices

### DO ✅
- Use factory pattern for backend selection
- Let fafafa.ssl manage resource cleanup (interfaces)
- Use high-level APIs (`ISSLLibrary`, `ISSLContext`) when possible
- Test on all target platforms
- Use WinSSL on Windows for zero-dependency deployment

### DON'T ❌
- Mix fafafa.ssl with other SSL libraries in same code
- Manually manage OpenSSL resources if using high-level API
- Hardcode backend selection (use auto-detect)
- Forget to test certificate validation
- Skip error handling

---

## 📞 Getting Help

### Resources
- **Documentation**: [docs/](docs/)
- **Examples**: [examples/](examples/)
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **GitHub Issues**: Report migration problems

### Community
- GitHub Discussions
- Issue tracker
- Project maintainers

---

## 🎯 Success Stories

### Example: Enterprise Migration

**Company**: [Example Corp]
**Migration**: Indy → fafafa.ssl
**Results**:
- ✅ 40% code reduction
- ✅ Zero OpenSSL DLLs on Windows (using WinSSL)
- ✅ Improved error handling
- ✅ Faster development
- ✅ Better maintenance

---

**Last Updated**: 2025-10-28  
**Version**: 1.0.0-rc.1

