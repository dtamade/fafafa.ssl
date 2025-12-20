# OpenSSL Pascal Bindings - Testing Guide

## Quick Start

### Run All Tests

```powershell
powershell -ExecutionPolicy Bypass -File run_all_tests.ps1
```

### Expected Output

```
========================================
OpenSSL Pascal Bindings Test Suite
========================================

Testing: aes (test_openssl_aes.pas)
  [PASS] Tests: 7, Passed: 7, Failed: 0 (100%)

Testing: dh (test_openssl_dh.pas)
  [PASS] Tests: 6, Passed: 6, Failed: 0 (100%)

... (more modules) ...

========================================
Test Summary
========================================

Modules Tested: 12
Modules Passed: 11
Total Test Cases: 111
Tests Passed: 110
Tests Failed: 1
Overall Pass Rate: 99.1%
```

---

## Current Status

✅ **12 Modules Tested** (16.7% coverage)  
✅ **111 Test Cases**  
✅ **99.1% Pass Rate**  
✅ **Automated Testing**

### Fully Tested Modules

- AES - AES symmetric encryption
- BIO - I/O abstraction
- DH - Diffie-Hellman key exchange  
- ERR - Error handling
- EVP - High-level crypto API
- HMAC - Message authentication
- MD - MD4/MD5 digests
- MD5 - MD5 hash
- RAND - Random numbers
- RSA - RSA encryption
- SHA - SHA hash family

### Partially Tested

- BN - Big numbers (97.2% - 1 test failing)

---

## Project Structure

```
fafafa.ssl/
├── src/                          # Source code
│   ├── fafafa.ssl.openssl.*.pas  # OpenSSL module bindings
│   └── openssl/                  # Additional bindings
├── examples/                     # Test programs
│   └── test_openssl_*.pas        # Module tests
├── run_all_tests.ps1            # Automated test runner
├── TEST_PLAN.md                 # Testing strategy
├── TESTING.md                   # Testing guide
├── PROGRESS_REPORT.md           # Current status
└── README_TESTING.md            # This file
```

---

## Documentation

📋 **TEST_PLAN.md** - Overall testing strategy and progress  
📖 **TESTING.md** - How to create and run tests  
📊 **PROGRESS_REPORT.md** - Detailed status report  
📝 **WORK_SESSION_2025-09-30.md** - Session notes

---

## Testing Individual Modules

### Compile a Test

```bash
fpc -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src \
    -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src\openssl \
    test_openssl_<module>.pas
```

### Run a Test

```bash
./test_openssl_<module>.exe
```

---

## Adding New Tests

1. **Copy Template** (use test_openssl_dh.pas as template)
2. **Update Module Name** in uses clause
3. **Write Test Procedures**
4. **Run Automated Suite** to verify

See **TESTING.md** for detailed instructions.

---

## Known Issues

### BN Module
- 1 modular exponentiation test failing
- All other operations working (97.2% pass)
- Investigating OpenSSL version compatibility

### DSA Module
- Has reserved keyword issue (&type)
- Fix pending

---

## Requirements

- **Free Pascal Compiler** 3.x or higher
- **Lazarus** (optional, for IDE)
- **OpenSSL** 1.1.1 or higher (DLLs in PATH)
- **PowerShell** 5.0+ (for test runner)
- **Windows** (current focus, Linux/macOS planned)

---

## Contributing

1. Run all tests before submitting changes
2. Ensure 100% pass rate for your modules
3. Follow established patterns (see TESTING.md)
4. Update TEST_PLAN.md with new tests
5. Document any issues discovered

---

## Support

- **Issues**: Create GitHub issue
- **Questions**: See TESTING.md
- **Status**: Check PROGRESS_REPORT.md

---

## License

See main project LICENSE file

---

**Last Updated**: 2025-09-30  
**Version**: 1.0  
**Status**: Active Development