# Requirements Document

## Introduction

本功能为 fafafa.ssl 库添加 SSH 密钥支持，使开发者能够读取、生成和转换 SSH 密钥对。SSH (Secure Shell) 密钥是现代开发和运维中最常用的身份验证方式之一，广泛用于 Git 仓库访问、服务器登录和自动化部署。

本功能将支持 OpenSSH 格式的密钥文件，包括传统格式和新的 OpenSSH 私钥格式，以及与 PEM/PKCS#8 格式之间的转换。

## Glossary

- **SSH_Key_Manager**: SSH 密钥管理器，负责密钥的读取、生成和转换操作
- **SSH_Public_Key**: SSH 公钥，用于身份验证的公开部分
- **SSH_Private_Key**: SSH 私钥，用于签名和解密的私密部分
- **SSH_Key_Pair**: SSH 密钥对，包含公钥和私钥
- **OpenSSH_Format**: OpenSSH 原生密钥格式，以 "ssh-rsa"、"ssh-ed25519" 等开头
- **PEM_Format**: Privacy-Enhanced Mail 格式，Base64 编码的密钥格式
- **Key_Fingerprint**: 密钥指纹，用于快速识别和验证密钥的哈希值
- **Passphrase**: 密码短语，用于加密保护私钥

## Requirements

### Requirement 1: SSH 公钥解析

**User Story:** As a developer, I want to parse SSH public key files, so that I can extract key information and use them for verification.

#### Acceptance Criteria

1. WHEN a valid OpenSSH public key string is provided, THE SSH_Key_Manager SHALL parse it and return the key type, key data, and optional comment
2. WHEN a public key file path is provided, THE SSH_Key_Manager SHALL read and parse the file contents
3. WHEN an invalid or malformed public key is provided, THE SSH_Key_Manager SHALL return a descriptive error indicating the parsing failure
4. THE SSH_Key_Manager SHALL support RSA public keys (ssh-rsa)
5. THE SSH_Key_Manager SHALL support Ed25519 public keys (ssh-ed25519)
6. THE SSH_Key_Manager SHALL support ECDSA public keys (ecdsa-sha2-nistp256, ecdsa-sha2-nistp384, ecdsa-sha2-nistp521)

### Requirement 2: SSH 私钥解析

**User Story:** As a developer, I want to parse SSH private key files, so that I can use them for signing and authentication.

#### Acceptance Criteria

1. WHEN a valid OpenSSH private key file is provided, THE SSH_Key_Manager SHALL parse it and extract the private key data
2. WHEN a PEM-formatted private key (PKCS#1 or PKCS#8) is provided, THE SSH_Key_Manager SHALL parse it as an SSH-compatible private key
3. WHEN an encrypted private key is provided with the correct passphrase, THE SSH_Key_Manager SHALL decrypt and parse the key
4. WHEN an encrypted private key is provided with an incorrect passphrase, THE SSH_Key_Manager SHALL return an authentication error
5. WHEN an unencrypted private key is provided, THE SSH_Key_Manager SHALL parse it without requiring a passphrase
6. THE SSH_Key_Manager SHALL support the new OpenSSH private key format (openssh-key-v1)
7. THE SSH_Key_Manager SHALL support legacy PEM private key formats

### Requirement 3: SSH 密钥对生成

**User Story:** As a developer, I want to generate new SSH key pairs, so that I can create secure authentication credentials programmatically.

#### Acceptance Criteria

1. WHEN generating an RSA key pair, THE SSH_Key_Manager SHALL create a valid RSA key with the specified bit size (2048, 3072, or 4096)
2. WHEN generating an Ed25519 key pair, THE SSH_Key_Manager SHALL create a valid Ed25519 key
3. WHEN generating an ECDSA key pair, THE SSH_Key_Manager SHALL create a valid ECDSA key with the specified curve (P-256, P-384, or P-521)
4. WHEN a comment is specified, THE SSH_Key_Manager SHALL include it in the generated public key
5. WHEN a passphrase is specified, THE SSH_Key_Manager SHALL encrypt the private key with the passphrase
6. THE SSH_Key_Manager SHALL use cryptographically secure random number generation for key creation

### Requirement 4: SSH 密钥导出

**User Story:** As a developer, I want to export SSH keys to various formats, so that I can use them with different tools and systems.

#### Acceptance Criteria

1. THE SSH_Key_Manager SHALL export public keys in OpenSSH format (single-line format with key type, base64 data, and comment)
2. THE SSH_Key_Manager SHALL export private keys in OpenSSH format (openssh-key-v1)
3. THE SSH_Key_Manager SHALL export private keys in PEM format (PKCS#8)
4. WHEN exporting an encrypted private key, THE SSH_Key_Manager SHALL use AES-256-CTR encryption for OpenSSH format
5. WHEN saving keys to files, THE SSH_Key_Manager SHALL set appropriate file permissions (600 for private keys on Unix-like systems)

### Requirement 5: 密钥指纹计算

**User Story:** As a developer, I want to calculate key fingerprints, so that I can verify and identify SSH keys.

#### Acceptance Criteria

1. THE SSH_Key_Manager SHALL calculate SHA-256 fingerprints in the format "SHA256:base64hash"
2. THE SSH_Key_Manager SHALL calculate MD5 fingerprints in the format "MD5:xx:xx:xx:..." for legacy compatibility
3. WHEN calculating a fingerprint, THE SSH_Key_Manager SHALL use the public key blob as input
4. THE SSH_Key_Manager SHALL support fingerprint calculation for all supported key types

### Requirement 6: 格式转换

**User Story:** As a developer, I want to convert SSH keys between different formats, so that I can use them with various tools and libraries.

#### Acceptance Criteria

1. THE SSH_Key_Manager SHALL convert OpenSSH public keys to PEM format (SubjectPublicKeyInfo)
2. THE SSH_Key_Manager SHALL convert PEM public keys to OpenSSH format
3. THE SSH_Key_Manager SHALL convert OpenSSH private keys to PEM format (PKCS#8)
4. THE SSH_Key_Manager SHALL convert PEM private keys to OpenSSH format
5. WHEN converting encrypted keys, THE SSH_Key_Manager SHALL preserve the encryption or allow re-encryption with a new passphrase

### Requirement 7: 密钥验证

**User Story:** As a developer, I want to validate SSH keys, so that I can ensure they are correctly formatted and usable.

#### Acceptance Criteria

1. THE SSH_Key_Manager SHALL verify that a public key and private key form a valid pair
2. THE SSH_Key_Manager SHALL validate key parameters (e.g., RSA modulus size, curve parameters)
3. WHEN a key fails validation, THE SSH_Key_Manager SHALL return a specific error describing the validation failure
4. THE SSH_Key_Manager SHALL detect and report weak or deprecated key configurations

### Requirement 8: authorized_keys 文件处理

**User Story:** As a developer, I want to read and write authorized_keys files, so that I can manage SSH access programmatically.

#### Acceptance Criteria

1. THE SSH_Key_Manager SHALL parse authorized_keys files containing multiple public keys
2. THE SSH_Key_Manager SHALL preserve key options (e.g., command=, from=, no-pty) when parsing
3. THE SSH_Key_Manager SHALL write authorized_keys files with proper formatting
4. WHEN parsing an authorized_keys file with invalid entries, THE SSH_Key_Manager SHALL skip invalid entries and report them as warnings
