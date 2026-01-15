# Requirements Document: Windows Full Validation

## Introduction

本需求文档定义了 fafafa.ssl 库在 Windows 环境下对 OpenSSL 和 WinSSL 两个后端的全量验证功能。该验证套件将系统性地测试所有模块的编译、功能和集成，确保库在 Windows 平台上的生产就绪状态。

## Glossary

- **Validation_Suite**: 完整的验证测试套件，包含编译验证、单元测试、集成测试
- **Test_Runner**: 自动化测试执行器，负责编译和运行所有测试程序
- **OpenSSL_Backend**: 基于 OpenSSL 3.x 的加密后端实现 (65+ API 模块)
- **WinSSL_Backend**: 基于 Windows SChannel 的加密后端实现 (10+ 模块)
- **Test_Report**: 测试执行结果的结构化报告
- **FPC**: Free Pascal Compiler，用于编译 Pascal 源代码

## Requirements

### Requirement 1: 源代码编译验证

**User Story:** As a developer, I want to verify that all source modules compile successfully on Windows, so that I can ensure the codebase is buildable.

#### Acceptance Criteria

1. WHEN the validation suite runs, THE Validation_Suite SHALL compile all 115+ source modules in `src/` directory
2. WHEN a module fails to compile, THE Validation_Suite SHALL record the error message and continue with remaining modules
3. WHEN compilation completes, THE Validation_Suite SHALL report the total number of modules compiled successfully vs failed
4. THE Validation_Suite SHALL use FPC with flags `-Mobjfpc -Sh -Fu"src" -FE"bin"`
5. THE Validation_Suite SHALL target Windows x64 platform

### Requirement 2: OpenSSL API 模块全量测试

**User Story:** As a developer, I want to verify that all OpenSSL API bindings work correctly on Windows, so that I can ensure OpenSSL functionality is complete.

#### Acceptance Criteria

1. WHEN OpenSSL DLLs (libssl-3-x64.dll, libcrypto-3-x64.dll) are available, THE Validation_Suite SHALL verify successful library loading
2. THE Validation_Suite SHALL test all OpenSSL API modules:
   - fafafa.ssl.openssl.api.aead.pas (AEAD 加密)
   - fafafa.ssl.openssl.api.aes.pas (AES 加密)
   - fafafa.ssl.openssl.api.aria.pas (ARIA 加密)
   - fafafa.ssl.openssl.api.asn1.pas (ASN.1 编解码)
   - fafafa.ssl.openssl.api.async.pas (异步操作)
   - fafafa.ssl.openssl.api.bio.pas (BIO I/O)
   - fafafa.ssl.openssl.api.blake2.pas (BLAKE2 哈希)
   - fafafa.ssl.openssl.api.bn.pas (大数运算)
   - fafafa.ssl.openssl.api.buffer.pas (缓冲区)
   - fafafa.ssl.openssl.api.chacha.pas (ChaCha20)
   - fafafa.ssl.openssl.api.cmac.evp.pas (CMAC)
   - fafafa.ssl.openssl.api.cms.pas (CMS)
   - fafafa.ssl.openssl.api.comp.pas (压缩)
   - fafafa.ssl.openssl.api.conf.pas (配置)
   - fafafa.ssl.openssl.api.core.pas (核心功能)
   - fafafa.ssl.openssl.api.crypto.pas (加密基础)
   - fafafa.ssl.openssl.api.ct.pas (证书透明度)
   - fafafa.ssl.openssl.api.des.pas (DES 加密)
   - fafafa.ssl.openssl.api.dh.pas (DH 密钥交换)
   - fafafa.ssl.openssl.api.dsa.pas (DSA 签名)
   - fafafa.ssl.openssl.api.ec.pas (椭圆曲线)
   - fafafa.ssl.openssl.api.ecdh.pas (ECDH)
   - fafafa.ssl.openssl.api.ecdsa.pas (ECDSA)
   - fafafa.ssl.openssl.api.engine.pas (引擎)
   - fafafa.ssl.openssl.api.err.pas (错误处理)
   - fafafa.ssl.openssl.api.evp.pas (EVP 高级接口)
   - fafafa.ssl.openssl.api.hmac.pas (HMAC)
   - fafafa.ssl.openssl.api.kdf.pas (密钥派生)
   - fafafa.ssl.openssl.api.md.pas (消息摘要)
   - fafafa.ssl.openssl.api.modes.pas (加密模式)
   - fafafa.ssl.openssl.api.ocsp.pas (OCSP)
   - fafafa.ssl.openssl.api.pem.pas (PEM 格式)
   - fafafa.ssl.openssl.api.pkcs.pas (PKCS)
   - fafafa.ssl.openssl.api.pkcs12.pas (PKCS#12)
   - fafafa.ssl.openssl.api.pkcs7.pas (PKCS#7)
   - fafafa.ssl.openssl.api.provider.pas (Provider)
   - fafafa.ssl.openssl.api.rand.pas (随机数)
   - fafafa.ssl.openssl.api.rsa.pas (RSA)
   - fafafa.ssl.openssl.api.sha.pas (SHA)
   - fafafa.ssl.openssl.api.sha3.evp.pas (SHA-3)
   - fafafa.ssl.openssl.api.ssl.pas (SSL/TLS)
   - fafafa.ssl.openssl.api.store.pas (存储)
   - fafafa.ssl.openssl.api.ts.pas (时间戳)
   - fafafa.ssl.openssl.api.x509.pas (X.509)
   - fafafa.ssl.openssl.api.x509v3.pas (X.509v3 扩展)
3. WHEN an OpenSSL function fails, THE Validation_Suite SHALL capture and report the OpenSSL error code and message
4. THE Validation_Suite SHALL verify OpenSSL version is 3.x
5. WHEN OpenSSL is not available, THE Validation_Suite SHALL skip OpenSSL tests and report the skip reason

### Requirement 3: OpenSSL 高级功能测试

**User Story:** As a developer, I want to verify that OpenSSL high-level wrappers work correctly, so that I can use the convenient API.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test fafafa.ssl.openssl.loader.pas (动态加载)
2. THE Validation_Suite SHALL test fafafa.ssl.openssl.base.pas (基础类型)
3. THE Validation_Suite SHALL test fafafa.ssl.openssl.errors.pas (错误处理)
4. THE Validation_Suite SHALL test fafafa.ssl.openssl.certificate.pas (证书操作)
5. THE Validation_Suite SHALL test fafafa.ssl.openssl.certstore.pas (证书存储)
6. THE Validation_Suite SHALL test fafafa.ssl.openssl.context.pas (SSL 上下文)
7. THE Validation_Suite SHALL test fafafa.ssl.openssl.connection.pas (SSL 连接)
8. THE Validation_Suite SHALL test fafafa.ssl.openssl.session.pas (会话管理)
9. THE Validation_Suite SHALL test fafafa.ssl.openssl.x509.chain.pas (证书链)
10. THE Validation_Suite SHALL test fafafa.ssl.openssl.cert.builder.pas (证书构建器)

### Requirement 4: WinSSL 后端全量测试

**User Story:** As a developer, I want to verify that the Windows SChannel backend works correctly, so that I can use native Windows TLS without OpenSSL dependency.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test all WinSSL modules:
   - fafafa.ssl.winssl.api.pas (SChannel API 绑定)
   - fafafa.ssl.winssl.base.pas (基础类型)
   - fafafa.ssl.winssl.lib.pas (库加载)
   - fafafa.ssl.winssl.errors.pas (错误处理)
   - fafafa.ssl.winssl.certificate.pas (证书操作)
   - fafafa.ssl.winssl.certstore.pas (证书存储)
   - fafafa.ssl.winssl.context.pas (SSL 上下文)
   - fafafa.ssl.winssl.connection.pas (SSL 连接)
   - fafafa.ssl.winssl.enterprise.pas (企业功能)
   - fafafa.ssl.winssl.utils.pas (工具函数)
2. WHEN testing WinSSL, THE Validation_Suite SHALL verify Windows certificate store access
3. WHEN testing WinSSL, THE Validation_Suite SHALL verify TLS 1.2 and TLS 1.3 connection establishment
4. THE Validation_Suite SHALL test WinSSL error handling and Windows error code mapping
5. THE Validation_Suite SHALL test WinSSL certificate chain validation

### Requirement 5: 加密功能测试

**User Story:** As a developer, I want to verify that all cryptographic operations work correctly on both backends.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test hash algorithms: MD5, SHA-1, SHA-256, SHA-384, SHA-512, SHA-3, BLAKE2
2. THE Validation_Suite SHALL test symmetric ciphers: AES-128/192/256, DES, 3DES, Blowfish, ChaCha20
3. THE Validation_Suite SHALL test cipher modes: ECB, CBC, CTR, GCM, CCM
4. THE Validation_Suite SHALL test asymmetric algorithms: RSA (2048/3072/4096), ECDSA (P-256/P-384/P-521), Ed25519
5. THE Validation_Suite SHALL test key derivation: PBKDF2, HKDF, scrypt
6. THE Validation_Suite SHALL test HMAC with all supported hash algorithms
7. THE Validation_Suite SHALL test digital signatures and verification

### Requirement 6: 证书功能测试

**User Story:** As a developer, I want to verify that certificate operations work correctly on both backends.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test certificate loading from PEM format
2. THE Validation_Suite SHALL test certificate loading from DER format
3. THE Validation_Suite SHALL test certificate loading from PKCS#12 format
4. THE Validation_Suite SHALL test certificate chain validation
5. THE Validation_Suite SHALL test certificate attribute extraction (subject, issuer, validity, extensions)
6. THE Validation_Suite SHALL test certificate generation and self-signing
7. THE Validation_Suite SHALL test CRL (Certificate Revocation List) checking
8. THE Validation_Suite SHALL test OCSP (Online Certificate Status Protocol) checking

### Requirement 7: TLS 连接测试

**User Story:** As a developer, I want to verify that TLS connections work correctly on both backends.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test TLS client connection to public HTTPS servers (e.g., https://www.google.com)
2. THE Validation_Suite SHALL test TLS 1.2 protocol
3. THE Validation_Suite SHALL test TLS 1.3 protocol
4. THE Validation_Suite SHALL test SNI (Server Name Indication)
5. THE Validation_Suite SHALL test ALPN (Application-Layer Protocol Negotiation)
6. THE Validation_Suite SHALL test certificate verification during handshake
7. THE Validation_Suite SHALL test connection timeout handling
8. THE Validation_Suite SHALL test graceful connection shutdown

### Requirement 8: SSH 密钥测试

**User Story:** As a developer, I want to verify that SSH key operations work correctly.

#### Acceptance Criteria

1. THE Validation_Suite SHALL test SSH public key parsing (ssh-rsa, ssh-ed25519, ecdsa-sha2-*)
2. THE Validation_Suite SHALL test SSH private key parsing (OpenSSH format, PEM format)
3. THE Validation_Suite SHALL test encrypted SSH private key parsing
4. THE Validation_Suite SHALL test SSH key generation (RSA, Ed25519, ECDSA)
5. THE Validation_Suite SHALL test SSH key format conversion (OpenSSH ↔ PEM)
6. THE Validation_Suite SHALL test SSH fingerprint calculation (SHA-256, MD5)
7. THE Validation_Suite SHALL test authorized_keys file parsing and writing

### Requirement 9: 测试报告生成

**User Story:** As a developer, I want to receive a comprehensive test report, so that I can understand the validation results.

#### Acceptance Criteria

1. WHEN validation completes, THE Validation_Suite SHALL generate a summary report
2. THE Test_Report SHALL include:
   - 测试执行时间
   - 环境信息 (Windows 版本, FPC 版本, OpenSSL 版本)
   - 总测试数、通过数、失败数、跳过数
   - 每个模块的详细测试结果
   - 失败测试的错误信息
3. THE Test_Report SHALL be saved in Markdown format for human reading
4. THE Test_Report SHALL be saved in JSON format for CI/CD integration
5. THE Validation_Suite SHALL output real-time progress to console

### Requirement 10: 自动化执行

**User Story:** As a developer, I want to run the validation suite automatically via PowerShell.

#### Acceptance Criteria

1. THE Validation_Suite SHALL be executable via `.\run_windows_validation.ps1`
2. THE Validation_Suite SHALL support command-line arguments:
   - `-OpenSSLOnly` 仅测试 OpenSSL 后端
   - `-WinSSLOnly` 仅测试 WinSSL 后端
   - `-SkipCompile` 跳过编译步骤
   - `-Verbose` 详细输出
3. THE Validation_Suite SHALL return exit code 0 for all tests passed
4. THE Validation_Suite SHALL return exit code 1 for any test failed
5. WHEN a test crashes, THE Validation_Suite SHALL catch the exception and continue with remaining tests
6. THE Validation_Suite SHALL support timeout configuration (default 60 seconds per test)

### Requirement 11: 现有测试集成

**User Story:** As a developer, I want to integrate existing test programs into the validation suite.

#### Acceptance Criteria

1. THE Validation_Suite SHALL execute all existing test programs in `tests/` directory
2. THE Validation_Suite SHALL execute tests in `tests/openssl/` for OpenSSL validation
3. THE Validation_Suite SHALL execute tests in `tests/winssl/` for WinSSL validation
4. THE Validation_Suite SHALL execute tests in `tests/crypto/` for cryptographic validation
5. THE Validation_Suite SHALL execute tests in `tests/certificate/` for certificate validation
6. THE Validation_Suite SHALL execute tests in `tests/connection/` for TLS connection validation
7. THE Validation_Suite SHALL execute tests in `tests/integration/` for integration validation
8. THE Validation_Suite SHALL parse test output to extract pass/fail counts

### Requirement 12: 后端对比测试

**User Story:** As a developer, I want to compare OpenSSL and WinSSL backends to ensure consistent behavior.

#### Acceptance Criteria

1. THE Validation_Suite SHALL run identical tests on both OpenSSL and WinSSL backends
2. THE Validation_Suite SHALL compare TLS handshake results between backends
3. THE Validation_Suite SHALL compare certificate validation results between backends
4. THE Validation_Suite SHALL report any behavioral differences between backends
5. WHEN backends produce different results, THE Validation_Suite SHALL flag it for review
