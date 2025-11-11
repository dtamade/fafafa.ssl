# Troubleshooting Guide

Common issues and solutions for fafafa.ssl.

---

## 🔧 Compilation Issues

### Issue: "Fatal: Can't find unit fafafa.ssl.XXX"

**Cause**: Unit search path not configured

**Solutions**:

1. **Command Line**: Add `-Fu` paths
   ```bash
   fpc -Fu./src -Fu./src/openssl your_program.pas
   ```

2. **Lazarus IDE**: 
   - Project → Project Options → Compiler Options
   - Paths → Other Unit Files
   - Add: `src;src/openssl`

3. **Using lazbuild**:
   ```bash
   lazbuild your_project.lpi
   ```

---

### Issue: "Error loading OpenSSL"

**Symptoms**:
```
Error: Could not load OpenSSL library
Error: libcrypto.so.3 not found
```

**Cause**: OpenSSL not installed or wrong version

**Solutions**:

#### Linux
```bash
# Ubuntu/Debian
sudo apt install libssl3 libssl-dev

# Fedora/RHEL
sudo dnf install openssl openssl-devel

# Check installation
ldconfig -p | grep libssl
```

#### Windows
**Option 1**: Use WinSSL (recommended)
```pascal
Lib := CreateSSLLibrary(sslWinSSL);
```

**Option 2**: Install OpenSSL
- Download from https://slproweb.com/products/Win32OpenSSL.html
- Install to default location (C:\Program Files\OpenSSL-Win64)
- Or set library path in code

#### macOS
```bash
brew install openssl@3
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl@3/lib
```

---

### Issue: "Invalid type conversion"

**Symptoms**:
```
Error: Incompatible type for arg no. 1: Got "Pointer", expected "PEVP_MD"
```

**Cause**: Strict type safety in fafafa.ssl

**Solution**: Use explicit type casts
```pascal
// Wrong
EVP_DigestInit_ex(Ctx, MD, nil);

// Correct
EVP_DigestInit_ex(Ctx, MD, PENGINE(nil));
```

---

## 🧪 Testing Issues

### Issue: "Test execution failed"

**Symptoms**:
```
[FAIL] Test xxx failed
```

**Debugging Steps**:

1. **Run test directly**:
   ```bash
   ./tests/bin/test_xxx
   ```

2. **Check prerequisites**:
   ```bash
   # Ensure OpenSSL is loaded
   ldd ./tests/bin/test_xxx
   ```

3. **Enable verbose output**:
   Edit test file and add:
   ```pascal
   {$DEFINE DEBUG}
   ```

---

### Issue: "39/40 tests passed - OPENSSL_free not loaded"

**Status**: Known issue (non-critical)

**Cause**: `OPENSSL_free` symbol name varies by version

**Impact**: Minimal - `CRYPTO_free` is used as fallback

**Fix** (optional): 
See `docs/testing/KNOWN_ISSUES.md`

---

## 🔒 SSL/TLS Issues

### Issue: "Certificate verification failed"

**Symptoms**:
```
SSL_ERROR: Certificate verify failed
```

**Causes & Solutions**:

1. **Missing CA certificates**:
   ```pascal
   // Option 1: Use system CA store (automatic in OpenSSL backend)
   Ctx := CreateSSLContext(sslCtxClient);
   
   // Option 2: Load specific CA file
   Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
   
   // Option 3: Disable verification (NOT for production!)
   Ctx.SetVerifyMode([]);  // No verification
   ```

2. **Wrong hostname**:
   ```pascal
   // Ensure SNI matches server
   Ctx.SetServerName('www.example.com');
   ```

3. **Self-signed certificate**:
   ```pascal
   // Load custom CA
   Ctx.LoadCAFile('my_ca.crt');
   ```

---

### Issue: "TLS handshake failed"

**Symptoms**:
```
SSL_ERROR: Handshake failure
Error code: 0x1408F10B
```

**Common Causes**:

1. **Protocol mismatch**:
   ```pascal
   // Allow TLS 1.2 and 1.3
   Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
   ```

2. **Cipher suite mismatch**:
   ```pascal
   // Use modern ciphers
   Ctx.SetCipherList('HIGH:!aNULL:!MD5');
   ```

3. **Server doesn't support requested protocol**:
   - Check server TLS version support
   - Try different protocol versions

---

## 🪟 Windows-Specific Issues

### Issue: "WinSSL backend not available"

**Symptoms**:
```
Error: WinSSL backend not supported on this platform
```

**Cause**: Trying to use WinSSL on non-Windows platform

**Solution**: Use platform detection
```pascal
{$IFDEF WINDOWS}
Lib := CreateSSLLibrary(sslWinSSL);
{$ELSE}
Lib := CreateSSLLibrary(sslOpenSSL);
{$ENDIF}
```

---

### Issue: "TLS 1.3 not available on Windows"

**Cause**: Windows version too old

**Requirements**:
- Windows 11 or Windows 10 build 20348+
- Windows Server 2022+

**Solution**: Use TLS 1.2
```pascal
Ctx.SetProtocolVersions([sslProtocolTLS12]);
```

---

## 🐧 Linux-Specific Issues

### Issue: "libssl.so.3: cannot open shared object file"

**Cause**: OpenSSL 3.x not installed

**Solution**:
```bash
# Install OpenSSL 3.x
sudo apt install libssl3

# Or use OpenSSL 1.1.x (supported)
sudo apt install libssl1.1
```

---

### Issue: "Permission denied" when running tests

**Cause**: Test executables not marked as executable

**Solution**:
```bash
chmod +x tests/bin/*
# Or
chmod +x run_tests_linux.sh
./run_tests_linux.sh
```

---

## 📦 Deployment Issues

### Issue: "Application requires OpenSSL DLLs"

**Problem**: Need to distribute large DLL files

**Solutions**:

1. **Windows**: Use WinSSL backend (zero dependencies!)
   ```pascal
   Lib := CreateSSLLibrary(sslWinSSL);
   ```
   - No DLLs needed
   - Smaller deployment size
   - See [docs/ZERO_DEPENDENCY_DEPLOYMENT.md](docs/ZERO_DEPENDENCY_DEPLOYMENT.md)

2. **Bundle OpenSSL**:
   - Include `libcrypto-3-x64.dll` and `libssl-3-x64.dll`
   - Place in same directory as executable

3. **Use system OpenSSL** (Linux):
   - Rely on system package manager
   - Document minimum version requirement

---

## 💡 Performance Issues

### Issue: "Slow hash/encryption operations"

**Check**:
1. **Debug vs Release build**:
   ```bash
   fpc -O3 your_program.pas  # Optimization level 3
   ```

2. **Using correct API**:
   ```pascal
   // Slow: Using low-level API incorrectly
   
   // Fast: Using EVP high-level API
   MD := EVP_MD_fetch(nil, 'SHA256', nil);
   ```

3. **Buffer sizes**:
   ```pascal
   // Use larger buffers for bulk operations
   const BUFFER_SIZE = 65536;  // 64KB
   ```

---

## 🔍 Debugging Tips

### Enable Verbose Logging

```pascal
// Add to your program
{$DEFINE DEBUG}

uses
  fafafa.ssl.log;

begin
  SetLogLevel(llDebug);
  // Your code
end.
```

### Check OpenSSL Version

```pascal
program CheckVersion;
uses fafafa.ssl.openssl.api.core;
begin
  LoadOpenSSLCore;
  WriteLn('OpenSSL: ', GetOpenSSLVersionString);
end.
```

### Verify Function Loading

```pascal
if not Assigned(EVP_MD_fetch) then
  WriteLn('ERROR: EVP_MD_fetch not loaded')
else
  WriteLn('OK: EVP_MD_fetch is available');
```

---

## 📚 Additional Resources

### Documentation
- [README.md](README.md) - Project overview
- [QUICK_START.md](QUICK_START.md) - Detailed examples
- [docs/](docs/) - Full documentation

### Test Examples
- `tests/test_openssl_simple.pas` - Basic usage
- `tests/test_hash_utils.pas` - Hash functions
- `examples/` - 50+ working examples

### Known Issues
- [docs/testing/KNOWN_ISSUES.md](docs/testing/KNOWN_ISSUES.md)
- GitHub Issues: https://github.com/yourusername/fafafa.ssl/issues

---

## 🆘 Getting More Help

If your issue isn't covered here:

1. **Search**: Check [closed GitHub issues](https://github.com/yourusername/fafafa.ssl/issues?q=is%3Aissue)

2. **Minimum Reproducible Example**:
   Create a small program that demonstrates the issue

3. **Include Information**:
   - Free Pascal version (`fpc -iV`)
   - OpenSSL version (`openssl version`)
   - Operating system
   - Full error message
   - Code snippet

4. **Open Issue**: 
   https://github.com/yourusername/fafafa.ssl/issues/new

---

**Last Updated**: 2025-10-28  
**Version**: 1.0.0-rc.1

