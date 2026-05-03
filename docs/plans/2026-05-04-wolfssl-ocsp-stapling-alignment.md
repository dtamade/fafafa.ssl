# WolfSSL OCSP Stapling Alignment Plan

**Goal:** 收口 `WolfSSL` 在 OCSP stapling 上的 public/runtime 漂移，补齐 client request 与 server stapled-response issuance 的最小接线，并把 capability / 文档 truth 收紧到“实验性但已真实接线”的状态。

**Architecture:** 这批只处理 `WolfSSL` 的 OCSP stapling，不重开 OpenSSL / FreePascal / CT / online OCSP 其他主线。
- `tests/*`：focused contract 锁住 `WolfSSL` 的 capability、`ISSLServerOCSPStaplingContext`、builder file-load 语义，以及 connection surface 的最小 truth。
- `src/fafafa.ssl.wolfssl.*`：修正错误的 native binding，接通 server-side status callback / response injection，以及 client-side stapling request。
- `docs/*`：把能力矩阵和 OCSP guide 调整为 bounded experimental wording，避免继续把 WolfSSL 写成“不支持”或“稳定支持”。

**Files:**
- Add: `tests/test_wolfssl_ocsp_stapling_contract.pas`
- Modify: `src/fafafa.ssl.wolfssl.base.pas`
- Modify: `src/fafafa.ssl.wolfssl.api.pas`
- Modify: `src/fafafa.ssl.wolfssl.context.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - lock WolfSSL OCSP stapling truth

Run:

```bash
fpc -Fu./src tests/test_wolfssl_ocsp_stapling_contract.pas -otmp/test_wolfssl_ocsp_stapling_contract
./tmp/test_wolfssl_ocsp_stapling_contract
```

Add checks:
- `WolfSSL` capability should not report `sslSupportStable` for OCSP stapling
- server context should expose `ISSLServerOCSPStaplingContext`
- `SetServerStapledOCSPResponse` / `LoadServerStapledOCSPResponseFile` / `GetServerStapledOCSPResponse` should round-trip caller-provided DER bytes
- connection `GetOCSPStaplingEnabled` should not default to `True` merely because an API symbol exists

Constraint:
- runtime may skip on hosts without `libwolfssl.so`; keep the contract valuable even when it only compiles locally

## Task 2: GREEN - implement missing WolfSSL wiring

Change:
- fix `wolfSSL_UseOCSPStapling(...)` binding to the real `(status_type, options)` signature
- add missing bindings for:
  - `wolfSSL_CTX_UseOCSPStapling`
  - `wolfSSL_set_tlsext_status_ocsp_resp`
  - `wolfSSL_CTX_set_tlsext_status_cb`
  - `wolfSSL_CTX_set_tlsext_status_arg`
- `TWolfSSLContext`: register/unregister server stapling callback based on configured stapled-response bytes
- `TWolfSSLConnection`: request client stapling before handshake when `ssoEnableOCSPStapling` / `ssoRequireOCSPStapling` is enabled
- tighten `DoGetOCSPStaplingEnabled` to actual response presence instead of “symbol exists”
- downgrade capability from stable to experimental, but stop reporting `none` once real wiring exists

Constraint:
- keep behavior bounded to caller-provided stapled-response issuance
- do not add online fetch / refresh / responder scheduling

## Task 3: Docs truth alignment

Update:
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`

Requirements:
- stop saying `WolfSSL` server stapling is simply “not supported”
- stop implying it is production-stable
- explicitly call out that current path is manual material + experimental verification evidence

## Task 4: Verification

Run:

```bash
fpc -Fu./src tests/test_wolfssl_ocsp_stapling_contract.pas -otmp/test_wolfssl_ocsp_stapling_contract && ./tmp/test_wolfssl_ocsp_stapling_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `WolfSSL` client path really requests stapled OCSP when configured
- `WolfSSL` server context no longer just stores stapled-response bytes; it wires them into native callback issuance
- capability / docs both converge to `experimental`, not `none` and not `stable`
- focused contract passes or gives bounded dependency skips
- repo compile gate passes
- minimal CI gate passes

## Execution Result

- RED:
  - `bash tests/scripts/test_wolfssl_ocsp_stapling_source_contract.sh`
  - 初次结果：失败在 `wolfSSL_UseOCSPStapling` 绑定签名与本地 header 不一致，命中真实结构性缺口
- GREEN:
  - `wolfSSL_UseOCSPStapling` 已修正为 `(status_type, options)` 签名
  - 已补 `wolfSSL_CTX_UseOCSPStapling`、`wolfSSL_set_tlsext_status_ocsp_resp`、`wolfSSL_CTX_set_tlsext_status_cb`、`wolfSSL_CTX_set_tlsext_status_arg`
  - `TWolfSSLContext.CreateConnection(...)` 已改走现代 `fafafa.ssl.wolfssl.connection.TWolfSSLConnection`
  - `TWolfSSLContext` 已注册 server stapling callback，`TWolfSSLConnection` 已在 client 握手前请求 `status_request`
  - `TWolfSSLConnection.DoGetOCSPStaplingEnabled` 已改成按实际 response 判断
  - `TWolfSSLLibrary.GetCapabilities.OCSPStaplingSupport` 已收敛为 `sslSupportExperimental`
- Verification:
  - `bash -n tests/scripts/test_wolfssl_ocsp_stapling_source_contract.sh` => PASS
  - `bash tests/scripts/test_wolfssl_ocsp_stapling_source_contract.sh` => PASS
  - `fpc -Fu./src tests/test_wolfssl_ocsp_stapling_contract.pas -otmp/test_wolfssl_ocsp_stapling_contract && ./tmp/test_wolfssl_ocsp_stapling_contract` => 编译通过；runtime 因 `WolfSSL` backend 不可用而 `[SKIP]`
  - `python3 scripts/compile_all_modules.py` => PASS (`185/185`)
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS（compile gate + `17/17` module tests + phase2 baseline dry-run）
