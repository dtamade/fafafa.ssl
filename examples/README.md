# fafafa.ssl - OpenSSL Pascal Binding Examples & Tests

This directory contains comprehensive test programs and examples for the fafafa.ssl OpenSSL Pascal binding library.

## Prerequisites

- **Free Pascal Compiler (FPC)** 3.2.2 or higher
- **Lazarus IDE** (optional, for using .lpi project files)
- **OpenSSL Library** 1.1.x or 3.x installed on your system
  - Windows: `libcrypto-1_1-x64.dll` or `libcrypto-3.dll`
  - Linux: `libcrypto.so.1.1` or `libcrypto.so.3`

## Quick Start

### Building Tests

#### Using Lazarus IDE
1. Open any `.lpi` file in Lazarus
2. Press F9 to compile and run

#### Using lazbuild (Command Line)
```bash
# Build a specific test
lazbuild test_openssl_sha.lpi

# Run the test
./test_openssl_sha
```

#### Using FPC directly
```bash
# Set include and unit paths
fpc -Fu../src test_openssl_sha.pas

# Run the test
./test_openssl_sha
```

### Running All Tests

On Windows (PowerShell):
```powershell
.\run_all_tests.ps1
```

## Available Test Programs

### Core Functionality
| Program | Description | Test Count |
|---------|-------------|------------|
| `test_openssl_load` | OpenSSL library loading | - |
| `test_openssl_simple` | Basic OpenSSL operations | - |
| `test_core_modules` | Core modules integration | - |

### Hash Algorithms
| Program | Description | Test Count |
|---------|-------------|------------|
| `test_openssl_md` | MD4, MD5, RIPEMD160 | 14 |
| `test_openssl_md5` | MD5 and MD4 hashing | 8 |
| `test_openssl_sha` | SHA-1, SHA-224, SHA-256, SHA-384, SHA-512 | 8 |

### Message Authentication
| Program | Description | Test Count |
|---------|-------------|------------|
| `test_openssl_hmac` | HMAC-SHA1, HMAC-SHA256, HMAC-SHA512 | 3 |

### Encryption
| Program | Description | Test Count |
|---------|-------------|------------|
| `test_openssl_aes` | AES-128/256 ECB, CBC, Key Wrap | 7 |

### Other Modules
| Program | Description | Test Count |
|---------|-------------|------------|
| `test_openssl_rand` | Random number generation | - |
| `test_openssl_bn` | Big number arithmetic | 35 |
| `test_openssl_bio` | Basic I/O operations | 9 |
| `test_openssl_err` | Error handling | - |

## Test Results

See [TEST_REPORT.md](TEST_REPORT.md) for comprehensive test results and analysis.

**Quick Summary**: 99.5% success rate with 101+ test cases passed.

## Example Usage

### Simple MD5 Hash
```pascal
program simple_md5;
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.md;

var
  Digest: array[0..15] of Byte;
  Input: AnsiString;
begin
  LoadOpenSSLCore;
  LoadOpenSSLMD;
  
  Input := 'Hello, World!';
  MD5(@Input[1], Length(Input), @Digest[0]);
  
  // Digest now contains the MD5 hash
  
  UnloadOpenSSLMD;
  UnloadOpenSSLCore;
end.
```

### Simple SHA-256 Hash
```pascal
program simple_sha256;
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.sha;

var
  Digest: array[0..31] of Byte;
  Input: AnsiString;
begin
  LoadOpenSSLCore;
  LoadOpenSSLSHA;
  
  Input := 'Hello, World!';
  SHA256(@Input[1], Length(Input), @Digest[0]);
  
  // Digest now contains the SHA-256 hash
  
  UnloadOpenSSLSHA;
  UnloadOpenSSLCore;
end.
```

### HMAC-SHA256
```pascal
program simple_hmac;
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.hmac;

var
  Digest: array[0..31] of Byte;
  Key: AnsiString;
  Data: AnsiString;
begin
  LoadOpenSSLCore;
  LoadOpenSSLHMAC;
  
  Key := 'secret_key';
  Data := 'message to authenticate';
  HMAC_SHA256(@Key[1], Length(Key), @Data[1], Length(Data), @Digest[0]);
  
  // Digest now contains the HMAC
  
  UnloadOpenSSLHMAC;
  UnloadOpenSSLCore;
end.
```

### AES-128 CBC Encryption
```pascal
program simple_aes;
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.aes;

var
  Key: array[0..15] of Byte;   // 128-bit key
  IV: array[0..15] of Byte;    // Initialization vector
  Input: array[0..15] of Byte; // Must be multiple of block size
  Output: array[0..15] of Byte;
begin
  LoadOpenSSLCore;
  LoadOpenSSLAES;
  
  // Fill key, IV, and input with data
  // ...
  
  AES_set_encrypt_key(@Key[0], 128, @AESKey);
  AES_cbc_encrypt(@Input[0], @Output[0], 16, @AESKey, @IV[0], AES_ENCRYPT);
  
  // Output now contains encrypted data
  
  UnloadOpenSSLAES;
  UnloadOpenSSLCore;
end.
```

## Directory Structure

```
examples/
├── README.md                  # This file
├── TEST_REPORT.md            # Comprehensive test report
├── run_all_tests.ps1         # PowerShell script to run all tests
│
├── test_openssl_load.pas     # Library loading test
├── test_openssl_simple.pas   # Simple operations test
├── test_core_modules.pas     # Core modules test
│
├── test_openssl_md.pas       # MD algorithms test
├── test_openssl_md5.pas      # MD5/MD4 test
├── test_openssl_sha.pas      # SHA algorithms test
├── test_openssl_hmac.pas     # HMAC test
│
├── test_openssl_aes.pas      # AES encryption test
├── test_openssl_rand.pas     # Random number test
├── test_openssl_bn.pas       # Big number test
├── test_openssl_bio.pas      # BIO operations test
├── test_openssl_err.pas      # Error handling test
│
└── *.lpi                     # Lazarus project files
```

## Troubleshooting

### OpenSSL Library Not Found
If you get "library not found" errors:

**Windows**:
- Ensure `libcrypto-3.dll` or `libcrypto-1_1-x64.dll` is in your PATH or application directory
- Download OpenSSL from: https://slproweb.com/products/Win32OpenSSL.html

**Linux**:
```bash
# Install OpenSSL development package
sudo apt-get install libssl-dev  # Debian/Ubuntu
sudo yum install openssl-devel   # RHEL/CentOS
```

### Compilation Errors
- Ensure the `../src` directory contains all the fafafa.ssl unit files
- Check that your FPC version is 3.2.2 or higher
- Verify that you're compiling for the correct architecture (x64/x86)

### Function Not Available
Some OpenSSL functions may not be available in older versions:
- `HMAC_CTX_new` - Not in OpenSSL 1.1.1h (use one-shot HMAC functions instead)
- `BN_mod` - Not in OpenSSL 1.1.1h (use BN_nnmod or BN_mod_word)

These limitations are documented in the test report.

## Contributing

If you add new test programs:
1. Follow the existing test structure
2. Include proper error handling
3. Update this README and TEST_REPORT.md
4. Create a corresponding .lpi file for Lazarus users

## License

See the main project LICENSE file.

## Support

For issues and questions:
- Check the TEST_REPORT.md for known issues
- Review the source code in ../src for API reference
- Consult OpenSSL documentation: https://www.openssl.org/docs/