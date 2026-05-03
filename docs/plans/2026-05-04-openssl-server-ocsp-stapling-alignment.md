# OpenSSL Server OCSP Stapling Alignment Plan

**Goal:** 收口 `OpenSSL` 服务端 OCSP stapling issuance 路径里“builder/context 已存 DER，但 native 握手没有真正发出 stapled response”的 public/runtime 漂移。

**Architecture:** 这批只做 `OpenSSL` server-side stapling callback 接线，不重开 client request、online OCSP fetch、refresh、CT 或其他 backend 主线。
- `tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas`：focused contract 直接 stub OpenSSL callback seam，锁住 callback 注册、arg 绑定、response 注入、clear 注销，以及 `BuildServer + WithServerOCSPStapledResponseFile(...)` 的 file-load 行为。
- `src/fafafa.ssl.openssl.context.pas`：补 `TOpenSSLContext` 的 server stapling 配置闭环，让 `Set/Clear/Load` 都回到同一个 native registration helper。
- `src/fafafa.ssl.openssl.api.ssl.pas`：本批只复用既有 binding，不再扩 native API 面。

**Files:**
- Add: `tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas`
- Modify: `src/fafafa.ssl.openssl.context.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - lock callback registration gap

Run:

```bash
fpc -Fu./src tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas -otmp/test_openssl_server_ocsp_stapling_callback_contract
./tmp/test_openssl_server_ocsp_stapling_callback_contract
```

Checks:
- `SetServerStapledOCSPResponse(...)` must register both `SSL_CTX_set_tlsext_status_cb` and `SSL_CTX_set_tlsext_status_arg`
- manual callback invocation must call `SSL_set_tlsext_status_ocsp_resp(...)` with the configured DER bytes
- `ClearServerStapledOCSPResponse` must unregister the callback
- `BuildServer + WithServerOCSPStapledResponseFile(...)` must both load the file bytes and register the callback

## Task 2: GREEN - close the context/native seam

Change:
- implement `TOpenSSLContext.ApplyServerOCSPStaplingConfiguration`
- server context only:
  - `HasServerStapledOCSPResponse=True` => register `arg=Self` and `cb=@OpenSSLServerOCSPStaplingStatusCallback`
  - otherwise => unregister both arg and callback
- call the helper from:
  - `ClearServerStapledOCSPResponse`
  - `SetServerStapledOCSPResponse`
  - `LoadServerStapledOCSPResponseFile`

Constraint:
- keep the callback buffer ownership compatible with OpenSSL allocator semantics
- do not widen the batch into client OCSP or online responder logic

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas -otmp/test_openssl_server_ocsp_stapling_callback_contract && ./tmp/test_openssl_server_ocsp_stapling_callback_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `TOpenSSLContext` no longer only stores stapled-response bytes; it wires them into the OpenSSL status callback seam
- focused contract passes for direct context mutation and builder file-load path
- repo compile gate passes
- minimal CI gate passes

## Execution Result

- RED:
  - `fpc -Fu./src tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas -otmp/test_openssl_server_ocsp_stapling_callback_contract`
  - 初次结果：编译失败在 `Forward declaration not solved "ApplyServerOCSPStaplingConfiguration;"`，证明当前 batch 确实停在 context/native seam 未闭合。
- GREEN:
  - `TOpenSSLContext.ApplyServerOCSPStaplingConfiguration` 已补齐
  - `Clear/Set/LoadServerStapledOCSPResponse*` 现在都会同步注册/注销 OpenSSL stapling callback
  - 手工调用 callback 时，已能把配置好的 DER bytes 注入 `SSL_set_tlsext_status_ocsp_resp(...)`
- Verification:
  - focused contract：`Passed: 11 / Failed: 0 / Skipped: 0`
  - `python3 scripts/compile_all_modules.py` => PASS (`185/185`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS（compile gate + `17/17` module tests + phase2 baseline dry-run）
