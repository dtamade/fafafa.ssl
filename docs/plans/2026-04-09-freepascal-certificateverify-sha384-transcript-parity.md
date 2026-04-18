# FreePascal CertificateVerify SHA384 Transcript Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 FreePascal TLS 1.3 `CertificateVerify` 在 `TLS_AES_256_GCM_SHA384` path 上的 transcript-input parity，确保 client verify 和 server-side signer 都不再把 48-byte transcript hash 误当成 SHA256-only 输入。

**Architecture:** 这批继续保持 validation hardening 的窄边界：不扩到新的 signature scheme family，不动 OCSP/CT，不改更大的 TLS 1.3 state machine。先用两个 RED 把缺口钉住：
- `tests/test_tls13_servercertverify.pas`：共享 helper 现在仍把 48-byte transcript hash 拒掉
- `tests/test_freepascal_client_certificateverify_runtime.pas`：AES256/SHA384 scripted full-handshake 上，valid `CertificateVerify` 仍过不去

若 RED 成立，最小生产修复只收敛到：
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `src/fafafa.ssl.freepascal.connection.pas`

**Guardrails:**
- 不新增新的 signature scheme 支持面
- 不重写 `TryVerifyTLS13CertificateVerifySignature(...)` 的 SHA256-only scheme 语义
- 只修 transcript-input builder 与连接层 call site 的 SHA384 parity
- 不碰 CT / OCSP / resumption / early-data

---

## Task 1: RED - Prove SHA384 transcript input still fails

**Files:**
- Modify: `tests/test_tls13_servercertverify.pas`
- Modify: `tests/test_freepascal_client_certificateverify_runtime.pas`

**Step 1: Add shared-helper RED**
- 在 `tests/test_tls13_servercertverify.pas` 增加一个 48-byte transcript-hash case：
  - 当前仍通过 `BuildTLS13ServerCertificateVerifyInputSHA256(...)` 进入共享 helper
  - 期望行为：48-byte transcript hash 也能被接受并拼成合法 input
  - 当前实际：会命中 `TLS13TranscriptHashSHA256`

**Step 2: Add runtime RED**
- 在 `tests/test_freepascal_client_certificateverify_runtime.pas`：
  - 让 scripted server 可按参数切换 cipher suite
  - 新增 `TLS_AES_256_GCM_SHA384` + valid `CertificateVerify` 成功用例
  - 当前实际应失败，证明 shared helper 缺口已经外溢到 runtime path

**Commands (RED):**
```bash
mkdir -p tmp/tls13_servercertverify_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_red \
  -FEtmp/tls13_servercertverify_red \
  -otmp/tls13_servercertverify_red/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_red/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_runtime_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_runtime_red \
  -FEtmp/freepascal_client_certificateverify_runtime_red \
  -otmp/freepascal_client_certificateverify_runtime_red/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_runtime_red/test_freepascal_client_certificateverify_runtime
```

## Task 2: GREEN - Add the smallest SHA384-parity fix

**Files:**
- Modify: `src/fafafa.ssl.tls13.servercertverify.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Generalize the CertificateVerify input builder**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - 增加一个接受 32/48-byte transcript hash 的共享 builder
  - 保留 `BuildTLS13ServerCertificateVerifyInputSHA256(...)` 作为兼容入口，但不再把 48-byte hash 直接拒掉
  - 如有必要，补一个显式 `BuildTLS13ServerCertificateVerifyInputSHA384(...)`

**Step 2: Make connection call sites suite-aware**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - `ValidateServerCertificateVerify(...)` 改为按实际 transcript-hash 长度 / suite 生成 input
  - server-side outgoing `CertificateVerify` build path 同样改成共享 builder

## Task 3: Verification and closeout

**Commands:**
```bash
mkdir -p tmp/tls13_servercertverify_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_green \
  -FEtmp/tls13_servercertverify_green \
  -otmp/tls13_servercertverify_green/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_green/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_runtime_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_runtime_green \
  -FEtmp/freepascal_client_certificateverify_runtime_green \
  -otmp/freepascal_client_certificateverify_runtime_green/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_runtime_green/test_freepascal_client_certificateverify_runtime
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-sha384-transcript-parity.md src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- Task 1 RED 已确认：
  - `tests/test_tls13_servercertverify.pas` 命中 `ESSLInvalidArgument: Invalid parameter "TLS13TranscriptHashSHA256"`
  - `tests/test_freepascal_client_certificateverify_runtime.pas` 在 `TLS_AES_256_GCM_SHA384` path 上命中同一根因
- Task 2 GREEN 采用最小修复：
  - `BuildTLS13ServerCertificateVerifyInputSHA256(...)` 从只接受 32-byte transcript hash 放宽到接受 32/48-byte transcript hash
  - `src/fafafa.ssl.freepascal.connection.pas` 的 client verify / server signer call site 统一改为走 `HashTLS13TranscriptForSuite(...)`
  - `ValidateServerCertificateVerify(...)` 显式接收 `ACipherSuite`，由 `ProcessEncryptedServerFlight(...)` 传入实际已协商的 suite，消除一处本地编译错误与 suite 漂移风险
- 执行过程中有一次命令输出路径 typo：`-otmp/tmp/...`，已立即改正；未涉及代码行为。

## Final Verification

- `tests/test_tls13_servercertverify.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182` 核心模块编译成功)
- `git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-sha384-transcript-parity.md src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md` => PASS
