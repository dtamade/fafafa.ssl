# Password Hash Tool

Secure password hashing tool using PBKDF2-HMAC-SHA256.

## Features

- ✅ **PBKDF2-HMAC-SHA256** - Industry-standard password hashing
- ✅ **Random Salt Generation** - Cryptographically secure salts
- ✅ **Configurable Iterations** - Adjustable security level
- ✅ **Constant-Time Comparison** - Protection against timing attacks
- ✅ **Base64 Encoding** - Easy storage and transmission

## Algorithm Details

- **Algorithm**: PBKDF2-HMAC-SHA256
- **Salt Length**: 128 bits (16 bytes)
- **Hash Length**: 256 bits (32 bytes)
- **Default Iterations**: 100,000 (OWASP recommendation)

## Compilation

```bash
fpc -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src ^
    -FED:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples\password_hash\lib ^
    password_hash.pas
```

## Usage

### 1. Hash a Password

```bash
# Using default iterations (100,000)
password_hash -hash "MySecurePassword123"

# Custom iterations (higher = more secure but slower)
password_hash -hash "MySecurePassword123" 200000
```

**Output:**
```
========================================
Password Hash:
========================================
pbkdf2:sha256:100000:vK7hF9xQ2mP...:n8R4tY1pL3oX...
========================================
```

### 2. Verify a Password

```bash
password_hash -verify "MySecurePassword123" "pbkdf2:sha256:100000:vK7hF9xQ2mP...:n8R4tY1pL3oX..."
```

## Hash Format

The tool uses a standardized format:

```
pbkdf2:sha256:<iterations>:<salt_base64>:<hash_base64>
```

**Example:**
```
pbkdf2:sha256:100000:R2FtWjR5SHJlQzNMeGM=:aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789ABCD=
```

- **Algorithm**: pbkdf2
- **Hash Function**: sha256
- **Iterations**: 100000
- **Salt**: Base64-encoded random salt
- **Hash**: Base64-encoded password hash

## Security Features

### 1. Cryptographically Secure Random Salt

Uses OpenSSL's `RAND_bytes()` for generating salts:
- 128-bit random salt per password
- Unique salt for each password
- Prevents rainbow table attacks

### 2. Configurable Iteration Count

- Default: 100,000 iterations (OWASP recommendation)
- Adjustable based on security requirements
- Higher iterations = more secure but slower

### 3. Constant-Time Comparison

Prevents timing attacks during verification:
```pascal
// XOR all bytes and check if result is zero
for i := 0 to Length(hash) - 1 do
  diff := diff or (computed[i] xor stored[i]);
  
Result := (diff = 0);
```

### 4. SHA-256 Based

- Uses HMAC-SHA-256 for key derivation
- 256-bit output size
- Resistant to collisions

## Use Cases

### 1. User Authentication System

```pascal
// Registration: Hash password
var
  hash_string: string;
begin
  hash_string := HashPassword(user_password, 100000);
  // Store hash_string in database
  SaveToDatabase(user_id, hash_string);
end;

// Login: Verify password
var
  hash_string: string;
  authenticated: Boolean;
begin
  hash_string := LoadFromDatabase(user_id);
  authenticated := VerifyPassword(entered_password, hash_string);
  
  if authenticated then
    // Allow access
  else
    // Deny access
end;
```

### 2. Password Manager

```bash
# Store master password hash
password_hash -hash "MyMasterPassword" 200000 > master.hash

# Verify master password on unlock
password_hash -verify "MyMasterPassword" "$(cat master.hash)"
```

### 3. API Key Verification

```bash
# Generate API key hash
password_hash -hash "api_key_12345"

# Verify API key
password_hash -verify "api_key_12345" <stored_hash>
```

## Best Practices

### ✅ DO

1. **Use High Iteration Counts**
   - Minimum: 100,000 for PBKDF2-HMAC-SHA256
   - Increase as hardware gets faster
   - Balance security vs. performance

2. **Store Hash Components**
   - Store the complete hash string
   - Includes algorithm, iterations, salt, hash
   - Allows algorithm upgrades

3. **Use Unique Salts**
   - Generate new random salt for each password
   - Never reuse salts
   - Prevents rainbow tables

4. **Constant-Time Comparison**
   - Always use timing-safe comparison
   - Prevents timing attacks
   - This tool implements it correctly

### ❌ DON'T

1. **Never Store Plain Passwords**
   - Always hash passwords
   - Never log passwords
   - Never transmit plain passwords

2. **Don't Use Weak Iterations**
   - < 100,000 is too weak
   - Adjust for your threat model
   - Monitor OWASP recommendations

3. **Don't Use MD5 or SHA-1**
   - These are cryptographically broken
   - Use SHA-256 or better
   - This tool uses SHA-256

4. **Don't Roll Your Own Crypto**
   - Use proven algorithms (PBKDF2, Argon2, bcrypt)
   - Use established libraries (OpenSSL)
   - Follow standards

## Iteration Count Guidelines

| Use Case | Iterations | Notes |
|----------|-----------|-------|
| Low Security | 100,000 | Minimum acceptable |
| Standard | 200,000 | Recommended for most |
| High Security | 500,000+ | Sensitive systems |
| Maximum | 1,000,000+ | Very slow, maximum protection |

**Benchmark** (on typical modern CPU):
- 100,000 iterations: ~100-200ms
- 200,000 iterations: ~200-400ms
- 500,000 iterations: ~500-1000ms

## Upgrading Password Hashes

When increasing iteration counts:

```pascal
// During login, check if upgrade needed
if VerifyPassword(password, stored_hash) then
begin
  // Check iteration count
  if NeedsUpgrade(stored_hash) then
  begin
    // Re-hash with new iterations
    new_hash := HashPassword(password, NEW_ITERATIONS);
    UpdateDatabase(user_id, new_hash);
  end;
  
  // Allow access
end;
```

## Comparison with Other Algorithms

| Algorithm | Speed | Security | Notes |
|-----------|-------|----------|-------|
| **PBKDF2-SHA256** | Medium | Good | Industry standard, widely supported |
| Argon2 | Slow | Excellent | Winner of PHC, memory-hard |
| bcrypt | Slow | Good | Popular, limited to 72 bytes |
| scrypt | Very Slow | Excellent | Memory-hard, less supported |

## Error Handling

The tool provides clear error messages:

```
[ERROR] Insufficient parameters
[ERROR] Failed to load OpenSSL  
[ERROR] PBKDF2 derivation failed
[ERROR] Invalid hash format
[ERROR] Unknown algorithm
```

## Troubleshooting

### Problem: "Failed to load OpenSSL"

**Solution:**
- Ensure `libcrypto-3-x64.dll` is in PATH
- Or copy DLL to same directory as exe

### Problem: "PBKDF2 derivation failed"

**Solution:**
- Check OpenSSL installation
- Verify EVP module loaded
- Check available memory

### Problem: Verification always fails

**Solution:**
- Ensure exact password match (case-sensitive)
- Copy hash string correctly (no line breaks)
- Check character encoding (UTF-8)

## Dependencies

- Free Pascal Compiler (FPC) 3.2.0+
- fafafa.ssl library
- OpenSSL 3.0+ (libcrypto-3-x64.dll)

## Technical Implementation

### PBKDF2 Process

```
1. Input: password, salt, iterations, key_length
2. Initialize: U_1 = HMAC(password, salt || counter)
3. Iterate: U_i = HMAC(password, U_(i-1))
4. XOR all: result = U_1 ⊕ U_2 ⊕ ... ⊕ U_iterations
5. Output: derived key
```

### Security Properties

- **Preimage Resistance**: Can't reverse hash to get password
- **Collision Resistance**: Hard to find two passwords with same hash
- **Slowness**: Intentionally slow to resist brute-force
- **Salt**: Prevents rainbow table attacks
- **Iterations**: Increases computational cost

## Standards Compliance

- ✅ NIST SP 800-132 (PBKDF)
- ✅ RFC 2898 (PKCS #5)
- ✅ OWASP Password Storage Cheat Sheet

## License

This tool is part of the fafafa.ssl project.

## References

- [OWASP Password Storage Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
- [NIST SP 800-132](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-132.pdf)
- [RFC 2898 - PKCS #5](https://datatracker.ietf.org/doc/html/rfc2898)
- [PBKDF2 on Wikipedia](https://en.wikipedia.org/wiki/PBKDF2)
