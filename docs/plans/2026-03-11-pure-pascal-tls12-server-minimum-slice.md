# 2026-03-11 pure Pascal TLS1.2 server minimum slice

## Goal
- 把 pure Pascal 服务端从“TLS1.3-only accept path”推进到最小 TLS1.2 server accept。
- 这波目标不是补完整 TLS1.2 服务器，而是先打通一条可验证、可扩展的最小线：
  - local socket
  - OpenSSL client oracle
  - RSA leaf
  - ECDHE_RSA
  - CHACHA20/AES128/AES256 GCM 中的最小起点

## Current Truth
- 当前 `TFreePascalConnection.DoAccept` 明确只支持 TLS1.3。
- `tests/test_freepascal_backend_basic.pas` 还把 TLS1.2 server accept 定义成 unsupported。

## First RED
- `tests/scripts/test_freepascal_tls12_server_accept_openssl_contract.sh`
- 当前预期：FAIL

## Expected Implementation Direction
- 需要补的最小基础件：
  - TLS1.2 ClientHello parser（server side）
  - TLS1.2 server flight builder
  - TLS1.2 server-side key schedule / Finished
  - 最小 app-data / shutdown 复用现有 record crypto
