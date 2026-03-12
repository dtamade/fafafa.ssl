# 2026-03-11 pure Pascal TLS1.2 client minimum slice

## Goal
- 为 pure Pascal / FreePascal backend 定义一条可执行的 `TLS 1.2 client-only` 最小实现路线。
- 目标不是一次性“补全 TLS 1.2 全家桶”，而是先打通一条可验证、可迭代、尽量贴近“无原生依赖”方向的窄轨道。

## Current Truth
- 当前 pure Pascal runtime 真相已经收口到 `TLS1.3-only`：
  - `IsProtocolSupported(sslProtocolTLS12)=False`
  - `MinTLSVersion=MaxTLSVersion=sslProtocolTLS13`
  - default config / context = `TLS1.3-only`
- 若调用方显式把 `ProtocolVersions` 收窄到 `TLS12`，连接阶段仍会收到 unsupported contract。
- 这意味着下一波应该直接实现新的 TLS 1.2 client handshake path，而不是再补真相文档。

## Reusable Pieces
- **Transport / buffering / IO retry**
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `RecvTLSRecord(...)`
  - `SendBufferedRecord(...)`
  - `TryPopHandshakeMessage(...)`
- **Certificate / hostname / trust / pinning**
  - `src/fafafa.ssl.freepascal.connection.pas`
  - 当前验证主线可在 TLS 1.2 握手完成后复用
- **ClientHello extensions (可提取成通用 helper)**
  - `src/fafafa.ssl.tls13.clienthello.pas`
  - SNI / ALPN / supported_groups / signature_algorithms 编码器可重用
- **Math / crypto primitives**
  - `src/fafafa.ssl.tls13.x25519.pas`：纯 Pascal X25519
  - `src/fafafa.ssl.tls13.bigint.pas`：BigInt / modexp
  - `src/fafafa.ssl.x509.pas`：RSA modulus/exponent、EC 点解析
- **Generic TLS constants / config surface**
  - `src/fafafa.ssl.base.pas`

## Hard Blockers / Gaps
- 当前不存在任何 `src/fafafa.ssl.tls12.*` 单元。
- `DoConnect` / `DoAccept` 在 FreePascal 连接层仍然直接拒绝任何非 TLS 1.3 握手。
- `SetCipherList(...)` 对 pure Pascal 握手路径仍是死字段；当前只消费 TLS 1.3 `CipherSuites`。
- TLS 1.3 key schedule / Finished / record protection 都不能直接套到 TLS 1.2。
- AES-GCM 当前包装层对 pure Pascal backend 仍带外部实现痕迹；这与“无原生依赖”目标存在张力。

## Recommended First Slice
- **形态**：client-only
- **key exchange**：`ECDHE_RSA`
- **group**：先只支持 `X25519`
- **certificate type**：先只支持 `RSA` leaf
- **record protection**：优先评估 `TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256`
  - 原因：更贴近“无原生依赖”目标
  - 代价：与 `AES_128_GCM_SHA256` 相比，TLS 1.2 互操作面可能更窄
- **若互操作不足的备选**：`TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256`
  - 只建议作为 bootstrap / 过渡，不应成为纯 Pascal 终点

## Non-Goals for v1
- server mode
- renegotiation
- resumption / session tickets
- CBC suites
- static RSA key exchange
- ECDSA leaf / ECDSA `ServerKeyExchange` verify
- TLS 1.0 / 1.1

## Implementation Phases

### Phase A: TLS 1.2 wire / parser foundation
- Add:
  - `src/fafafa.ssl.tls12.wire.pas`
  - `src/fafafa.ssl.tls12.clienthello.pas`
  - `src/fafafa.ssl.tls12.serverhello.parser.pas`
  - `src/fafafa.ssl.tls12.prf.pas`
- Scope:
  - record / handshake constants
  - cipher suite IDs
  - ClientHello builder
  - ServerHello / Certificate / ServerKeyExchange / ServerHelloDone parser
  - PRF / master secret / key block helpers

### Phase B: client handshake in `freepascal.connection`
- Branch `DoConnect` on `sslProtocolTLS12`.
- New TLS 1.2 client path should:
  - send ClientHello
  - parse server flight
  - verify `ServerKeyExchange` RSA signature
  - derive master secret + traffic keys
  - send `ChangeCipherSpec` + `Finished`
  - receive server `ChangeCipherSpec` + `Finished`

### Phase C: post-handshake reuse
- Reuse existing:
  - trust / hostname / pinning / verify callback
  - blocking/nonblocking IO and timeout surfaces
  - `TSSLStream` / connector wrappers
- Add only TLS 1.2-specific record AAD / nonce / Finished handling where required.

## Test Strategy

### RED
- Add focused contract proving current TLS1.2 client path still unsupported.
- Add first executable TLS1.2 oracle test against local OpenSSL `s_server`:
  - `-tls1_2`
  - fixed cipher
  - local cert fixture

### GREEN
- First green target:
  - pure Pascal client completes local TLS 1.2 handshake
  - verify result = success
  - negotiated protocol = `sslProtocolTLS12`
  - cipher / key-exchange fields are populated

### Regression
- Re-run:
  - `tests/test_freepascal_backend_basic.pas`
  - `tests/test_factory_backend_default_config_initialization.pas`
  - system-roots / resumption / stream semantics focused gates as needed
  - `python3 -u scripts/compile_all_modules.py`

## Files Expected Next
- `src/fafafa.ssl.tls12.wire.pas`
- `src/fafafa.ssl.tls12.clienthello.pas`
- `src/fafafa.ssl.tls12.serverhello.parser.pas`
- `src/fafafa.ssl.tls12.prf.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_tls12_client_foundation.pas`
- `tests/scripts/test_freepascal_tls12_local_openssl_contract.sh`

## Verification Target
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_factory_backend_default_config_initialization.pas -otmp/test_factory_backend_default_config_initialization && ./tmp/test_factory_backend_default_config_initialization`
- `python3 -u scripts/compile_all_modules.py`
