# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project snapshot
- Language/tooling: FreePascal (FPC) + Lazarus.
- Core API is interface-based and backend-agnostic (OpenSSL + Windows Schannel/WinSSL backends).
- Primary source lives in `src/` (units named `fafafa.ssl.*`).

## Common commands

### Quick “does it work?” (build + tests + benchmarks)
```bash
chmod +x ci_pipeline.sh
./ci_pipeline.sh all
```
Notes:
- `./ci_pipeline.sh test` includes network-dependent tests (some failures may be expected without network access).

### Build
```bash
# CI-style build (compiles key test binaries under examples/ and tests/)
./ci_pipeline.sh build

# Linux compile gate (compiles core modules via python)
chmod +x build_linux.sh
./build_linux.sh

# Compile most sources/tests into tests/**/bin using fpc
chmod +x scripts/compile_all.sh
./scripts/compile_all.sh
```

### Lint / style checks
```bash
python3 scripts/check_code_style.py src
```

### Run tests
```bash
# CI-style test run (integration-ish)
./ci_pipeline.sh test

# Linux: compile+run tests/*.pas (skips WinSSL tests)
chmod +x run_all_tests.sh
./run_all_tests.sh

# Pure-Pascal module tests (compile first)
./scripts/compile_all.sh
./scripts/run_pure_pascal_tests.sh

# Security tests (compile first)
./scripts/compile_all.sh
./scripts/run_security_tests.sh
```

### Run a single test

#### Lazarus project (.lpi)
Many tests have a Lazarus project file that defines output paths.

```bash
# Build
lazbuild tests/test_x509.lpi

# Run
./tests/bin/test_x509
```

#### Standalone .pas (Linux-style)
```bash
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/test_x509.pas -o./bin/test_x509
./bin/test_x509
```

### Local TLS endpoint for manual testing
`scripts/local_tls_server.sh` wraps `openssl s_server`.

```bash
export FAFAFA_TLS_CERT=/path/to/cert.pem
export FAFAFA_TLS_KEY=/path/to/key.pem
./scripts/local_tls_server.sh start
./scripts/local_tls_server.sh status
./scripts/local_tls_server.sh stop
```

### Script portability notes
Some scripts are machine-specific and may need path adjustments:
- `scripts/lazbuild_all.sh` hard-codes a `lazbuild` path.
- `run_all_tests.ps1` (repo root) contains absolute `D:\...` paths.
- Some benchmark/coverage scripts under `scripts/` hard-code `/home/dtamade/freePascal/...` tool paths.
Prefer the repo-relative runners (e.g. `ci_pipeline.sh`, `run_all_tests.sh`, `tests/run_core_tests.ps1`) when possible.

## Architecture (big picture)

### 1) Stable, backend-agnostic API
- `src/fafafa.ssl.base.pas` defines:
  - core enums/records/constants
  - the main interfaces (`ISSLLibrary`, `ISSLContext`, `ISSLConnection`, `ISSLCertificate`, `ISSLCertificateStore`, `ISSLSession`)
- Error model:
  - `src/fafafa.ssl.exceptions.pas` (typed exception hierarchy with context/native error support)
  - `src/fafafa.ssl.errors.pas` (helpers to raise consistent exceptions)

### 2) Factory entry point
- `src/fafafa.ssl.factory.pas` is the main entry point.
  - Registers/chooses backends (`sslAutoDetect` selects the “best available” backend).
  - Creates contexts/certificates/stores via interfaces.
  - Normalizes default config (protocol/option defaults, session cache defaults, etc.).

### 3) Backend implementations

#### OpenSSL backend
- Low-level dynamic bindings:
  - `src/fafafa.ssl.openssl.api.*.pas` (function-pointer bindings)
  - `src/fafafa.ssl.openssl.loader.pas` (`TOpenSSLLoader` manages `libcrypto`/`libssl`, function lookup, and module load flags)
- High-level interface implementations:
  - `src/fafafa.ssl.openssl.context.pas` (`TOpenSSLContext`)
  - `src/fafafa.ssl.openssl.connection.pas` (`TOpenSSLConnection`)
  - plus certificate/session/store units under `src/fafafa.ssl.openssl.*`
- Initialization helper:
  - `src/fafafa.ssl.init.pas` provides `InitializeOpenSSL` which loads core + key modules.

When working at the binding layer, ensure the relevant OpenSSL module is loaded before calling into it.

#### WinSSL backend (Windows Schannel)
- `src/fafafa.ssl.winssl.*` implements the same interfaces using Schannel:
  - `src/fafafa.ssl.winssl.lib.pas` (`TWinSSLLibrary`)
  - `src/fafafa.ssl.winssl.context.pas` (`TWinSSLContext`)
  - `src/fafafa.ssl.winssl.connection.pas` (`TWinSSLConnection`)
- Non-Windows environments should skip WinSSL-specific builds/tests.

### 4) Convenience layers built on top
- Fluent connection builder:
  - `src/fafafa.ssl.connection.builder.pas` (`TSSLConnectionBuilder`, with non-throwing `TryBuild*` returning `TSSLOperationResult`).
- Quick API:
  - `src/fafafa.ssl.quick.pas` provides one-liners for common certificate generation flows via the certificate/context builders.

## Repo conventions that affect edits
- Pascal units should use `{$mode ObjFPC}{$H+}`.
- Windows-facing units typically include `{$CODEPAGE UTF8}` (also enforced by `scripts/check_code_style.py`).
- Build artifacts:
  - `lib/` and `tests/lib/$(TargetCPU)-$(TargetOS)` contain compiled units.
  - `bin/` and `tests/bin/` contain executables.
Avoid hand-editing generated artifacts unless you are intentionally adjusting build outputs.

## Pointers
- `README.md` (overview + example snippets + CI pipeline commands)
- `docs/README.md` (documentation index)
- `docs/testing/README_TESTING.md` and `tests/integration/README.md` (testing notes)
