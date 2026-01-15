# OpenSSL Pascal Bindings Testing Guide

## Automated Test Suite

### Quick Start

Run all tests with a single command:

```powershell
powershell -ExecutionPolicy Bypass -File run_all_tests.ps1
```

### Test Structure

All test programs are located in the `examples/` directory and follow the naming convention:
```
test_openssl_<module>.pas
```

### Current Test Coverage

**Module Status:** 11/72 modules tested (15.3%)

| Module | Tests | Pass | Fail | Rate |
|--------|-------|------|------|------|
| AES    | 7     | 7    | 0    | 100% |
| BIO    | 9     | 9    | 0    | 100% |
| BN     | 36    | 35   | 1    | 97.2%|
| ERR    | ✓     | ✓    | 0    | 100% |
| EVP    | 3     | 3    | 0    | 100% |
| HMAC   | 3     | 3    | 0    | 100% |
| MD     | 14    | 14   | 0    | 100% |
| MD5    | 8     | 8    | 0    | 100% |
| RAND   | ✓     | ✓    | 0    | 100% |
| RSA    | 15    | 15   | 0    | 100% |
| SHA    | 8     | 8    | 0    | 100% |

**Overall:** 105 test cases, 104 passed, 1 failed (99.0% pass rate)

### Known Issues

1. **BN Module** - 1 test failing (modular exponentiation test)
   - This may be due to OpenSSL version compatibility
   - All other BN functions working correctly

### Creating New Tests

To create a new test module:

1. Copy an existing test template (e.g., `test_openssl_evp.pas`)
2. Follow this structure:

```pascal
program test_openssl_<module>;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.<module>;

var
  TestsPassed, TestsFailed: Integer;

procedure RunTest(const TestName: string; TestProc: TProcedure);
begin
  Write('Testing: ', TestName, '...');
  try
    TestProc();
    WriteLn(' [PASS]');
    Inc(TestsPassed);
  except
    on E: Exception do
    begin
      WriteLn(' [FAIL] ', E.Message);
      Inc(TestsFailed);
    end;
  end;
end;

begin
  TestsPassed := 0;
  TestsFailed := 0;
  
  WriteLn('OpenSSL <Module> Test');
  WriteLn('=====================');
  WriteLn;
  
  // Load OpenSSL
  LoadOpenSSLCore;
  LoadOpenSSL<Module>;
  
  // Run your tests
  RunTest('Test1', @Test1Procedure);
  RunTest('Test2', @Test2Procedure);
  
  // Summary
  WriteLn;
  WriteLn('Tests Passed: ', TestsPassed);
  WriteLn('Tests Failed: ', TestsFailed);
  WriteLn('Total Tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then
    WriteLn('SUCCESS: All tests passed!')
  else
    Halt(1);
end.
```

3. Add your test to the `examples/` directory
4. Run the automated test script to verify

### Manual Testing

To test a single module:

```bash
# Compile
fpc -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src \
    -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src\openssl \
    test_openssl_<module>.pas

# Run
./test_openssl_<module>.exe
```

### Test Output Format

For automated parsing, tests should output in this format:

```
Tests Passed: X
Tests Failed: Y
Total Tests:  Z
```

Alternatively, for simple tests:
```
Test completed successfully.
```

### Best Practices

1. **Test Independence** - Each test should be independent
2. **Error Handling** - Use try-except blocks to catch failures
3. **Clear Output** - Print descriptive test names
4. **Standard Format** - Follow the output format for automation
5. **Test Coverage** - Test both success and failure cases
6. **OpenSSL Versions** - Be aware of version differences

### Next Steps

Priority modules for testing (based on importance and dependencies):

1. **DES** - DES/3DES encryption
2. **X509** - X.509 certificates (requires completion of loading functions)
3. **PEM** - PEM format
4. **ASN1** - ASN.1 encoding
5. **EC** - Elliptic curves

### Continuous Integration

The automated test script can be integrated into CI/CD pipelines:

```yaml
# Example for GitHub Actions
- name: Run OpenSSL Tests
  shell: pwsh
  run: |
    .\run_all_tests.ps1
    if ($LASTEXITCODE -ne 0) { exit 1 }
```

### Troubleshooting

**Issue:** Tests fail with "OpenSSL library not found"
- **Solution:** Ensure OpenSSL DLLs are in PATH or system32

**Issue:** Compilation errors
- **Solution:** Check that `-Fu` paths are correct for your system

**Issue:** Script execution policy error
- **Solution:** Run with `-ExecutionPolicy Bypass` flag

### Contributing

When adding new tests:
1. Follow the test template structure
2. Run the automated suite to verify
3. Update TEST_PLAN.md with results
4. Document any known issues

---

Last Updated: 2025-09-30