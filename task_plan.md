# Task Plan - OpenSSL Server OCSP Stapling Alignment

## Goal
收口 `OpenSSL` 服务端 OCSP stapling issuance 路径里“context 已存 DER / builder 已喂文件，但 native 握手没有真正发出 stapled response”的缺口，让 `ISSLServerOCSPStaplingContext` 的 server path 不再停留在只存不发。

## Current Batch
1. 用 focused contract 锁住 `OpenSSL` status callback / arg 注册、callback 注入响应、clear 注销，以及 `BuildServer + WithServerOCSPStapledResponseFile(...)` 的 file-load 行为。
2. 在 `TOpenSSLContext` 内补 `ApplyServerOCSPStaplingConfiguration`，把 server stapling 配置统一收口到一个 native registration helper。
3. 让 `ClearServerStapledOCSPResponse`、`SetServerStapledOCSPResponse`、`LoadServerStapledOCSPResponseFile` 都同步更新 callback 注册状态。
4. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`。

## Status
- [completed] RED focused contract
- [completed] GREEN implementation
- [completed] Verification
- [pending] Review and commit

## Risks
- 这批 focused contract 直接 stub callback seam，没有覆盖真实 OpenSSL server/client 握手交换；如果后续要把这条线从“结构闭合”提升到“runtime 证实”，还需要补真实握手用例。
- `OpenSSLServerOCSPStaplingStatusCallback` 依赖 `CRYPTO_malloc` / `OPENSSL_free` 兼容分配路径；本批没有扩大到更通用的 allocator 封装。
- 这批只收口 server issuance，不重开 `TOpenSSLConnection` 的 client request / required-policy，那条线已有独立实现。

## Follow-up Queue
1. 补一条真实 OpenSSL server/client 握手测试，验证 `status_request + stapled DER` 在 runtime 上确实被消费。
2. 如果未来要支持自动刷新 stapled response，再单开 batch 处理 online fetch / refresh / responder scheduling，而不是继续堆在 context 层。
