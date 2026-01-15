# HMAC Message Authentication Tool

A command-line tool for generating and verifying HMAC (Hash-based Message Authentication Code) for files using OpenSSL's EVP API.

## Features

- **Multiple Hash Algorithms**: Supports MD5, SHA-1, SHA-256, SHA-384, SHA-512
- **Flexible Key Input**: Accept keys as text or from files
- **Multiple Output Formats**: Base64 (default) or hexadecimal
- **Secure Verification**: Uses constant-time comparison to prevent timing attacks
- **Format Auto-detection**: Automatically detects hex vs Base64 format during verification
- **Production Ready**: Built with security best practices

## Building

```bash
fpc -Mobjfpc -Sh -O3 -XX -CX hmac_tool.lpr -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src -FED:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples\hmac_tool\bin -FUD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples\hmac_tool\lib
```

Or use the build script:

```bash
# Windows
build.bat

# Linux/macOS
chmod +x build.sh
./build.sh
```

## Usage

### Generate HMAC

Generate HMAC for a file with a text key:

```bash
hmac_tool generate -i document.txt -k "my_secret_key" -o document.hmac
```

Generate with key from file:

```bash
hmac_tool generate -i data.bin -k @keyfile.key -o data.hmac
```

Generate with specific algorithm and hex output:

```bash
hmac_tool generate -i message.txt -k "secret" -a sha512 --hex
```

Output to stdout:

```bash
hmac_tool generate -i file.dat -k "key123"
```

### Verify HMAC

Verify HMAC with text key:

```bash
hmac_tool verify -i document.txt -k "my_secret_key" -m document.hmac
```

Verify with key from file:

```bash
hmac_tool verify -i data.bin -k @keyfile.key -m data.hmac
```

Quiet mode (only exit code):

```bash
hmac_tool verify -i file.dat -k "key123" -m file.hmac -q
echo $?  # 0 = success, 1 = failure
```

## Command-Line Options

### Operations

- `generate` - Generate HMAC for a file
- `verify` - Verify HMAC of a file

### Options

- `-i <file>` - Input file to authenticate (required)
- `-o <file>` - Output file for HMAC (default: stdout for generate)
- `-k <key>` - Secret key as text, or `@<file>` to read from file (required)
- `-m <file>` - HMAC file to verify against (required for verify)
- `-a <algorithm>` - Hash algorithm: `md5`, `sha1`, `sha256`, `sha384`, `sha512` (default: `sha256`)
- `--hex` - Output HMAC in hexadecimal format (default: base64)
- `-q` - Quiet mode (minimal output)
- `-h, --help` - Show help message

## Supported Algorithms

| Algorithm | Output Size | Recommended Key Size |
|-----------|-------------|---------------------|
| MD5       | 16 bytes    | 16 bytes (not recommended for new applications) |
| SHA-1     | 20 bytes    | 20 bytes (deprecated) |
| SHA-256   | 32 bytes    | 32 bytes ✓ (recommended) |
| SHA-384   | 48 bytes    | 48 bytes ✓ |
| SHA-512   | 64 bytes    | 64 bytes ✓ |

**Default**: SHA-256 (recommended for most applications)

## Output Formats

### Base64 (Default)

Compact, URL-safe representation:

```
XlQFfX0N0xJ3vKWrjVXdOJGhvZYXN7r9yGMqPqKXVYo=
```

### Hexadecimal

Human-readable hex representation:

```
5e54057d7d0dd31277bca5ab8d55dd3891a1bd96173bbfdcd8632a3ea29755
```

## Security Considerations

### Key Management

1. **Key Length**: Use keys that are at least as long as the hash output
   - For SHA-256: minimum 32 bytes
   - For SHA-512: minimum 64 bytes

2. **Key Generation**: Always use cryptographically secure random generators:
   ```bash
   # Generate a 32-byte key
   openssl rand -base64 32 > key.txt
   ```

3. **Key Storage**: 
   - Never store keys in source code
   - Use environment variables or secure key management systems
   - Set appropriate file permissions (e.g., `chmod 600 key.txt`)

4. **Key Rotation**: Regularly rotate keys in production environments

### Timing Attack Protection

The verification operation uses constant-time comparison to prevent timing attacks. This ensures that the time taken to verify an HMAC doesn't leak information about the correct value.

### Algorithm Selection

- **Recommended**: SHA-256, SHA-384, or SHA-512
- **Deprecated**: SHA-1 (vulnerable to collision attacks)
- **Not Recommended**: MD5 (cryptographically broken)

For new applications, use SHA-256 or SHA-512.

## Use Cases

### 1. API Authentication

Generate HMAC for API request bodies:

```bash
# Generate signature
hmac_tool generate -i request.json -k @api_secret.key -o signature.txt

# Send request with signature
curl -X POST -H "X-Signature: $(cat signature.txt)" \
     --data @request.json https://api.example.com/endpoint
```

### 2. File Integrity Verification

Verify downloaded files haven't been tampered with:

```bash
# Sender generates HMAC
hmac_tool generate -i data.zip -k "shared_secret" -o data.hmac

# Receiver verifies
hmac_tool verify -i data.zip -k "shared_secret" -m data.hmac
```

### 3. Message Authentication in Distributed Systems

Authenticate messages between services:

```bash
# Service A sends message
hmac_tool generate -i message.json -k @service_key.txt -o message.sig

# Service B verifies
if hmac_tool verify -i message.json -k @service_key.txt -m message.sig -q; then
    echo "Message authenticated successfully"
    # Process message
else
    echo "Authentication failed - possible tampering"
    exit 1
fi
```

### 4. Webhook Verification

Verify webhook signatures from third-party services:

```bash
# Save webhook payload
echo "$WEBHOOK_PAYLOAD" > webhook.json

# Verify signature
echo "$WEBHOOK_SIGNATURE" > expected.sig
hmac_tool generate -i webhook.json -k "$WEBHOOK_SECRET" | \
    diff - expected.sig && echo "Valid webhook" || echo "Invalid signature"
```

## Exit Codes

- `0` - Success
- `1` - Error (invalid parameters, verification failed, etc.)

## Technical Details

### HMAC Implementation

This tool uses OpenSSL's EVP (Envelope) API for HMAC computation:

1. **Key Setup**: `EVP_PKEY_new_mac_key()` with `EVP_PKEY_HMAC`
2. **Initialization**: `EVP_DigestSignInit()` with selected hash algorithm
3. **Update**: `EVP_DigestSignUpdate()` for streaming file data
4. **Finalization**: `EVP_DigestSignFinal()` to get HMAC

Benefits of using EVP API:
- Hardware acceleration support
- FIPS-compliant implementations
- Consistent interface across algorithms
- Better error handling

### Performance

- Processes files in 8KB chunks for efficient memory usage
- Suitable for files of any size
- No temporary files created

## Comparison with OpenSSL Command Line

This tool vs `openssl dgst`:

| Feature | hmac_tool | openssl dgst |
|---------|-----------|--------------|
| HMAC Generation | ✓ | ✓ |
| HMAC Verification | ✓ Built-in | Manual comparison |
| Constant-time Compare | ✓ | ✗ |
| Key from File | ✓ | Requires `-mac_opt key:` |
| Auto-format Detection | ✓ | ✗ |
| User-friendly CLI | ✓ | Complex syntax |

Example comparison:

```bash
# hmac_tool
hmac_tool generate -i file.txt -k @key.bin -a sha256

# openssl equivalent
openssl dgst -sha256 -mac HMAC -macopt hexkey:$(xxd -p key.bin | tr -d '\n') file.txt
```

## Examples

### Basic Usage

```bash
# Generate HMAC with SHA-256 (default)
$ hmac_tool generate -i message.txt -k "secret_key"
Generating HMAC...
XlQFfX0N0xJ3vKWrjVXdOJGhvZYXN7r9yGMqPqKXVYo=

Algorithm: sha256
HMAC size: 32 bytes

# Verify HMAC
$ echo "XlQFfX0N0xJ3vKWrjVXdOJGhvZYXN7r9yGMqPqKXVYo=" > message.hmac
$ hmac_tool verify -i message.txt -k "secret_key" -m message.hmac
Verifying HMAC...
HMAC verification: SUCCESS

Algorithm: sha256
Format: Base64
```

### Advanced Usage

```bash
# Generate with SHA-512 and hex output
$ hmac_tool generate -i data.bin -k @secure.key -a sha512 --hex -o data.hmac
Generating HMAC...
Key loaded from file: 64 bytes
HMAC written to: data.hmac

Algorithm: sha512
HMAC size: 64 bytes

# Verify in quiet mode (for scripts)
$ hmac_tool verify -i data.bin -k @secure.key -m data.hmac -q
$ echo $?
0
```

### Batch Processing

```bash
#!/bin/bash
# Generate HMACs for all files in a directory

KEY="my_shared_secret"

for file in *.txt; do
    hmac_tool generate -i "$file" -k "$KEY" -o "${file}.hmac"
    echo "Generated HMAC for $file"
done
```

### Integration with Git Hooks

```bash
#!/bin/bash
# Pre-commit hook to verify file integrity

if ! hmac_tool verify -i config.json -k "$CONFIG_SECRET" -m config.json.hmac -q; then
    echo "ERROR: config.json has been modified without valid HMAC"
    exit 1
fi
```

## Troubleshooting

### Error: "Failed to load OpenSSL EVP functions"

- Ensure OpenSSL DLL is in system PATH
- Windows: `libcrypto-3-x64.dll` or `libcrypto-1_1-x64.dll`
- Linux: `libcrypto.so.3` or `libcrypto.so.1.1`

### Error: "Unknown digest algorithm"

- Check algorithm name spelling
- Use one of: `md5`, `sha1`, `sha256`, `sha384`, `sha512`
- Algorithm names are case-insensitive

### Verification Always Fails

- Ensure you're using the same key for generation and verification
- Check that the algorithm matches (`-a` option)
- Verify file hasn't been modified since HMAC generation
- Check file permissions (key file readable)

### Key File Issues

- Use `@` prefix to read key from file: `-k @keyfile.txt`
- Ensure key file exists and is readable
- Key file should contain raw bytes (not base64 or hex)

## License

This tool is part of the fafafa.ssl project and uses the same license.

## See Also

- [File Encryption Tool](../file_encrypt/README.md) - Encrypt/decrypt files with AES-GCM
- [Digital Signature Tool](../digital_sign/README.md) - RSA signature generation/verification
- [Password Hashing Tool](../password_hash/README.md) - PBKDF2 password hashing
- [OpenSSL HMAC Documentation](https://www.openssl.org/docs/man3.0/man3/HMAC.html)

## References

- [RFC 2104](https://tools.ietf.org/html/rfc2104) - HMAC: Keyed-Hashing for Message Authentication
- [NIST FIPS 198-1](https://csrc.nist.gov/publications/detail/fips/198/1/final) - The Keyed-Hash Message Authentication Code (HMAC)
- [OWASP - Cryptographic Storage Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Cryptographic_Storage_Cheat_Sheet.html)
