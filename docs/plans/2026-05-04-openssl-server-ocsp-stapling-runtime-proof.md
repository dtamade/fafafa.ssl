# OpenSSL Server OCSP Stapling Runtime Proof Plan

**Goal:** 为 `OpenSSL` 服务端 OCSP stapling issuance 补一条真实 client/server 握手证据，验证当前 callback wiring 不只是结构闭合，而是真的能把 caller-provided stapled OCSP response 发到客户端连接 surface。

**Architecture:** 这批不扩新接口、不重开 online OCSP、也不改 FreePascal/WolfSSL 其他主线。只在 `OpenSSL` 上加一个 focused runtime test，用本地 socket 建立真实 TLS 握手，覆盖最小正负场景。
- `tests/openssl/*`：新增 runtime contract，使用 `OpenSSL server <-> OpenSSL client` 真正握手。
- `src/fafafa.ssl.openssl.*`：只有在 runtime test 暴露真实缺口时才做最小修复。
- `task_plan.md` / `findings.md` / `progress.md`：记录这批 runtime proof 结论，避免后续再把“只有 callback 契约”误当成完整 runtime 证据。

**Files:**
- Add: `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
- Modify: `src/fafafa.ssl.openssl.*` (only if runtime proof exposes a real bug)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Status

- 状态：已完成
- 真实 runtime 缺口：`src/fafafa.ssl.openssl.api.ssl.pas` 的 `SSL_CTX_set_tlsext_status_cb_impl` 之前错误使用 `SSL_CTX_ctrl(...)`；修复为按 OpenSSL 宏语义调用 `SSL_CTX_callback_ctrl(...)` 后，server handshake 才会实际进入 stapled-response injection
- 额外 runtime enablement：
  - `src/fafafa.ssl.openssl.context.pas`：server stapled material 存在时，同步设置 `SSL_CTX_set_tlsext_status_type(..., TLSEXT_STATUSTYPE_ocsp)`
  - `src/fafafa.ssl.openssl.connection.pas`：server pre-handshake path 上也按 material truth 设置 `SSL_set_tlsext_status_type(..., TLSEXT_STATUSTYPE_ocsp)`
- runtime harness 说明：本地执行环境无法可靠创建 listen socket，所以最终证明使用 scripted `TStream` TLS 1.3 对端，而不是 localhost TCP
- builder 诊断结论：`BuildServer` runtime 中途失败不是 file-load seam 回退，而是测试若不显式 `WithVerifyNone`，builder 默认 `verify-peer` 会要求客户端证书；最终 helper 已按 direct smoke 基线显式关闭 peer verify

## Task 1: RED - prove the missing runtime coverage

Run:

```bash
fpc -Fu./src tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas -otmp/test_openssl_server_ocsp_stapling_runtime
./tmp/test_openssl_server_ocsp_stapling_runtime
```

Cover:
- configured stapled response + client requested stapling => client `ISSLOCSPStapling` should surface the DER bytes
- configured stapled response + client did not request stapling => client surface should stay empty
- no configured stapled response + client requested stapling => client surface should stay empty
- builder `WithServerOCSPStapledResponseFile(...)` => server should load bytes and client should receive them when requested

## Task 2: GREEN - fix only if runtime proof fails

Constraint:
- keep fixes bounded to `OpenSSL` server stapling runtime seam
- do not widen into client verification policy, CT, online responder fetch, or other backend work

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas -otmp/test_openssl_server_ocsp_stapling_runtime && ./tmp/test_openssl_server_ocsp_stapling_runtime
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `OpenSSL` server stapling has real handshake evidence, not only callback stubs
- client surface receives stapled DER only when request/material preconditions are both met
- builder file-load path is covered by runtime proof
- repo compile gate passes
- minimal CI gate passes

## Verification Result

- focused runtime test：`PASS: OpenSSL server OCSP stapling runtime checks passed`
- `python3 scripts/compile_all_modules.py`：`185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`：compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run PASS
