# PKCS#11 Architecture Documentation

## Overview

This document describes the architecture of the PKCS#11 integration in the fafafa.ssl library. The implementation provides seamless support for hardware security modules (HSMs) and smart cards through the PKCS#11 standard interface.

## Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                         │
│  (TOpenSSLContext, TSSLContextBuilder)                      │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   High-Level API Layer                       │
│  • LoadPrivateKeyFromPKCS11                                 │
│  • Builder API (UsePKCS11, WithPKCS11PIN)                   │
│  • Utility Functions (EnumerateTokens, FindKey)             │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   Backend Abstraction Layer                  │
│  • IPKCS11Backend (interface)                               │
│  • TProviderBackend (OpenSSL 3.x)                           │
│  • TEngineBackend (OpenSSL 1.1.1)                           │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   PKCS#11 Module Layer                       │
│  • TPKCS11Loader (dynamic library loading)                  │
│  • PKCS#11 API bindings (C_* functions)                     │
│  • PIN Management (5 methods)                               │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   Hardware/Software HSM                      │
│  (SoftHSM, YubiKey, Luna HSM, etc.)                         │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Type System (`fafafa.ssl.pkcs11.types.pas`)

Defines all PKCS#11 data structures and types:

- **TPKCS11URI**: RFC 7512 URI representation
  - Path attributes: token, object, id, type
  - Query attributes: pin-value, pin-source, module-path
- **TPKCS11Config**: Configuration for PKCS#11 operations
  - Module path, token label, key label
  - PIN method and value
  - Backend selection (auto, provider, engine)

- **TPKCS11KeyInfo**: Key metadata
  - Label, ID, type (RSA/EC)
  - Key size, usage flags
  - Certificate association

- **TPKCS11TokenInfo**: Token information
  - Label, manufacturer, model, serial
  - Flags (login required, protected auth path)
  - Memory information

- **TPKCS11PINMethod**: PIN acquisition methods
  - `pmNone`: No PIN required
  - `pmValue`: Direct PIN value
  - `pmEnvironment`: From environment variable
  - `pmFile`: From file
  - `pmCallback`: User callback
  - `pmInteractive`: Interactive prompt

### 2. URI Parser (`fafafa.ssl.pkcs11.uri.pas`)

RFC 7512 compliant URI parser:

```pascal
class function TPKCS11URIParser.Parse(const AURI: string): TPKCS11URI;
class function TPKCS11URIParser.Generate(const AConfig: TPKCS11URI): string;
class function TPKCS11URIParser.IsPKCS11URI(const AURI: string): Boolean;
class function TPKCS11URIParser.Validate(const AURI: TPKCS11URI): Boolean;
```

**Features**:

- Percent-encoding/decoding
- Path and query attribute parsing
- Validation against RFC 7512
- URI generation from config

**Example URIs**:

```
pkcs11:token=MyToken;object=MyKey
pkcs11:token=MyToken;object=MyKey?pin-value=1234
pkcs11:token=MyToken;object=MyKey?pin-source=file:/path/to/pin.txt
pkcs11:token=MyToken;object=MyKey?module-path=/usr/lib/softhsm/libsofthsm2.so
```

### 3. Backend Abstraction (`fafafa.ssl.pkcs11.backend.pas`)

Unified interface for different OpenSSL versions:

```pascal
IPKCS11Backend = interface
  function LoadPrivateKey(const AConfig: TPKCS11Config): PEVP_PKEY;
  function LoadCertificate(const AConfig: TPKCS11Config): PX509;
  function IsAvailable: Boolean;
  function GetName: string;
  function GetVersion: string;
end;
```

**Backend Selection**:

1. **Auto-detection**: Checks OpenSSL version and availability
2. **Provider Backend**: OpenSSL 3.x (preferred)
3. **ENGINE Backend**: OpenSSL 1.1.1 (fallback)

### 4. Provider Backend (`fafafa.ssl.pkcs11.provider.pas`)

OpenSSL 3.x Provider API implementation:

```pascal
TProviderBackend = class(TBasePKCS11Backend)
  function LoadPrivateKey(const AConfig: TPKCS11Config): PEVP_PKEY;
  function LoadCertificate(const AConfig: TPKCS11Config): PX509;
  function IsAvailable: Boolean;
  function GetName: string;
  function GetVersion: string;
end;
```

**Features**:

- Uses `OSSL_PROVIDER_load` for pkcs11 provider
- Constructs provider-specific URI format
- Handles PIN via OSSL_PARAM
- Automatic provider cleanup

**URI Format**:

```
pkcs11:token=MyToken;object=MyKey
```

### 5. ENGINE Backend (`fafafa.ssl.pkcs11.engine.pas`)

OpenSSL 1.1.1 ENGINE API implementation:

```pascal
TEngineBackend = class(TBasePKCS11Backend)
  function LoadPrivateKey(const AConfig: TPKCS11Config): PEVP_PKEY;
  function LoadCertificate(const AConfig: TPKCS11Config): PX509;
  function IsAvailable: Boolean;
  function GetName: string;
  function GetVersion: string;
end;
```

**Features**:

- Uses `ENGINE_by_id('pkcs11')` for engine loading
- Supports ENGINE control commands
- PIN handling via ENGINE_ctrl_cmd_string
- Automatic engine cleanup

**URI Format**:

```
pkcs11:token=MyToken;object=MyKey
```

### 6. PKCS#11 Loader (`fafafa.ssl.pkcs11.loader.pas`)

Dynamic library loading and PKCS#11 API access:

```pascal
TPKCS11Loader = class
  class function LoadModule(const AModulePath: string): CK_FUNCTION_LIST_PTR;
  class procedure UnloadModule;
  class function GetFunctionList: CK_FUNCTION_LIST_PTR;
end;
```

**Features**:

- Cross-platform library loading (Windows/Linux/macOS)
- Function list caching
- Thread-safe module management
- Automatic cleanup on finalization

### 7. Utility Functions (`fafafa.ssl.pkcs11.utils.pas`)

High-level convenience functions:

```pascal
TPKCS11Utils = class
  // Enumeration
  class function EnumerateSlots(const AModulePath: string): TPKCS11SlotInfoArray;
  class function EnumerateTokens(const AModulePath: string): TPKCS11TokenInfoArray;
  class function EnumerateKeys(const AModulePath, ATokenLabel, APIN: string): TPKCS11KeyInfoArray;

  // Search
  class function FindTokenByLabel(const AModulePath, ALabel: string): TPKCS11TokenInfo;
  class function FindSlotByID(const AModulePath: string; ASlotID: CK_SLOT_ID): TPKCS11SlotInfo;
  class function FindKeyByLabel(const AModulePath, ATokenLabel, AKeyLabel, APIN: string): TPKCS11KeyInfo;

  // Information
  class function GetModuleInfo(const AModulePath: string): TPKCS11ModuleInfo;
end;
```

### 8. PIN Management (`fafafa.ssl.pkcs11.pin.pas`)

Secure PIN handling with multiple acquisition methods:

```pascal
TPKCS11PINManager = class
  class function GetPIN(AMethod: TPKCS11PINMethod;
                       const AValue: string;
                       ACallback: TPKCS11PINCallback;
                       const ATokenLabel: string): string;
  class function ValidatePIN(const APIN: string;
                            AMinLength: Integer = 4;
                            AMaxLength: Integer = 32): Boolean;
  class procedure SecureZeroPIN(var APIN: string);
end;
```

**PIN Methods**:

1. **pmNone**: No PIN (for unprotected tokens)
2. **pmValue**: Direct PIN value (use with caution)
3. **pmEnvironment**: Read from environment variable
4. **pmFile**: Read from file (with permission check)
5. **pmCallback**: User-provided callback function
6. **pmInteractive**: Interactive console prompt

### 9. Context Integration (`fafafa.ssl.openssl.context.pas`)

TOpenSSLContext integration:

```pascal
TOpenSSLContext = class(TInterfacedObject, ISSLContext)
  procedure LoadPrivateKeyFromPKCS11(const AURI: string;
                                     const APIN: string = '';
                                     APINMethod: TPKCS11PINMethod = pmNone);
end;
```

**Features**:

- Automatic URI detection in LoadPrivateKey
- Backend selection and initialization
- Error handling and reporting
- Resource cleanup

### 10. Builder API (`fafafa.ssl.context.builder.pas`)

Fluent API for PKCS#11 configuration:

```pascal
ISSLContextBuilder = interface
  function UsePKCS11(const AURI: string): ISSLContextBuilder;
  function WithPKCS11PIN(const APIN: string): ISSLContextBuilder;
  function WithPKCS11PINMethod(AMethod: TPKCS11PINMethod): ISSLContextBuilder;
end;
```

**Usage Example**:

```pascal
Context := TSSLContextBuilder.Create
  .WithCertificate('server.crt')
  .UsePKCS11('pkcs11:token=MyToken;object=MyKey;type=private')
  .WithPKCS11PIN('1234')
  .BuildServer;
```

**Builder Runtime Contract**:

- Supported in builder runtime:
  - `pmNone`
  - `pmValue`
  - `pmEnvironment`
  - `pmFile`
- Builder callers can switch source modes with `WithPKCS11PINMethod(...)`
- `pmCallback` and `pmInteractive` remain lower-level `TPKCS11Config` / backend integrations, not builder runtime paths

## Data Flow

### Private Key Loading Flow

```
1. Application calls LoadPrivateKeyFromPKCS11
   ↓
2. URI Parser validates and parses PKCS#11 URI
   ↓
3. Backend selector chooses Provider or ENGINE
   ↓
4. PIN Manager acquires PIN (if needed)
   ↓
5. Backend loads private key from HSM
   ↓
6. Key is associated with SSL context
   ↓
7. Application uses context for TLS operations
```

### Token Enumeration Flow

```
1. Application calls EnumerateTokens
   ↓
2. PKCS#11 Loader loads module
   ↓
3. C_Initialize initializes PKCS#11 library
   ↓
4. C_GetSlotList retrieves all slots
   ↓
5. C_GetTokenInfo retrieves token information
   ↓
6. Results returned as array of TPKCS11TokenInfo
   ↓
7. C_Finalize cleans up PKCS#11 library
```

## Design Principles

### 1. Abstraction

- **Backend Independence**: Application code doesn't need to know about Provider vs ENGINE
- **URI-based Configuration**: Standard RFC 7512 URIs for portability
- **Interface-based Design**: Easy to add new backends

### 2. Security

- **Secure PIN Handling**: Multiple methods, secure zeroing
- **No PIN Logging**: PINs never appear in logs
- **Resource Cleanup**: Automatic cleanup of sensitive data
- **Permission Checks**: File-based PIN requires proper permissions

### 3. Compatibility

- **OpenSSL Version Support**: Both 3.x and 1.1.1
- **Cross-platform**: Windows, Linux, macOS
- **Standard Compliance**: RFC 7512, PKCS#11 v2.40

### 4. Usability

- **Fluent API**: Builder pattern for easy configuration
- **Automatic Detection**: URI detection, backend selection
- **Rich Utilities**: Enumeration, search, validation
- **Clear Error Messages**: Detailed error reporting

## Error Handling

### Error Categories

1. **Configuration Errors**
   - Invalid URI format
   - Missing required attributes
   - Invalid PIN method

2. **Module Errors**
   - Module not found
   - Module load failure
   - Function list unavailable

3. **PKCS#11 Errors**
   - Token not found
   - Key not found
   - Authentication failure
   - Session errors

4. **Backend Errors**
   - Provider/ENGINE not available
   - Key load failure
   - OpenSSL errors

### Error Reporting

All errors are reported through:

- Exception raising (ESSLException)
- Backend and factory exceptions during selection or key loading
- Detailed error messages with context

## Performance Considerations

### Optimization Strategies

1. **Module Caching**: PKCS#11 module loaded once per process
2. **Function List Caching**: Function pointers cached after first load
3. **Backend Selection**: Auto-detection cached per context
4. **Session Management**: Efficient session open/close

### Resource Management

1. **Automatic Cleanup**: All resources cleaned up in destructors
2. **Reference Counting**: Interface-based lifetime management
3. **Session Pooling**: (Future enhancement)
4. **Connection Reuse**: (Future enhancement)

## Security Considerations

### PIN Security

1. **Never Log PINs**: PINs excluded from all logging
2. **Secure Zeroing**: Memory zeroed after PIN use
3. **File Permissions**: PIN files must have restricted permissions (0600)
4. **Environment Variables**: Use with caution in production

### Key Security

1. **Private Keys Stay in HSM**: Keys never leave hardware
2. **No Key Export**: Implementation doesn't support key export
3. **Session Security**: Proper session cleanup
4. **Authentication**: Proper PIN/authentication handling

### Best Practices

1. **Use Hardware HSM**: For production environments
2. **Protect PIN Files**: Restrict file permissions
3. **Use Callbacks**: For interactive PIN entry
4. **Monitor Access**: Log token access (not PINs)
5. **Regular Audits**: Review PKCS#11 usage

## Testing Strategy

### Unit Tests

1. **URI Parser Tests**: Valid/invalid URIs, encoding/decoding
2. **PIN Manager Tests**: All PIN methods, validation
3. **Backend Tests**: Provider and ENGINE functionality
4. **Utility Tests**: Enumeration, search functions

### Integration Tests

1. **SoftHSM Tests**: Full workflow with SoftHSM
2. **Key Loading Tests**: Load keys from HSM
3. **TLS Tests**: Complete TLS handshake with HSM keys
4. **Error Tests**: Error handling and recovery

### Hardware Tests

1. **YubiKey Tests**: Real hardware token testing
2. **Luna HSM Tests**: Enterprise HSM testing
3. **Performance Tests**: Throughput and latency
4. **Stress Tests**: Concurrent access, long-running

## Future Enhancements

### Planned Features

1. **Certificate Loading**: Load certificates from PKCS#11
2. **Session Pooling**: Reuse PKCS#11 sessions
3. **Async Operations**: Non-blocking key operations
4. **Key Generation**: Generate keys in HSM
5. **Multi-token Support**: Use multiple tokens simultaneously

### Potential Improvements

1. **Caching**: Cache token/key information
2. **Monitoring**: Metrics and health checks
3. **Failover**: Automatic failover between tokens
4. **Load Balancing**: Distribute load across multiple HSMs

## References

- [RFC 7512: PKCS#11 URI Scheme](https://tools.ietf.org/html/rfc7512)
- [PKCS#11 v2.40 Specification](http://docs.oasis-open.org/pkcs11/pkcs11-base/v2.40/)
- [OpenSSL 3.x Provider Documentation](https://www.openssl.org/docs/man3.0/man7/provider.html)
- [OpenSSL 1.1.1 ENGINE Documentation](https://www.openssl.org/docs/man1.1.1/man3/ENGINE_by_id.html)

## Glossary

- **HSM**: Hardware Security Module - dedicated cryptographic hardware
- **PKCS#11**: Public-Key Cryptography Standards #11 - Cryptographic Token Interface Standard
- **Provider**: OpenSSL 3.x plugin architecture
- **ENGINE**: OpenSSL 1.1.1 plugin architecture
- **Token**: PKCS#11 cryptographic device (physical or virtual)
- **Slot**: Physical or logical reader for tokens
- **PIN**: Personal Identification Number for token authentication
- **URI**: Uniform Resource Identifier - standard way to reference PKCS#11 objects
