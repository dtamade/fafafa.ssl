# FreePascal Revocation Evidence Material Plumbing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 给 FreePascal client runtime trust path 接入调用方提供的 CRL revocation material，让 `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 不再只能 fail-closed 为 unavailable，而是能对 caller-provided CRL material 给出 good / revoked / unavailable 的更细 truth。

**Architecture:** 这批不改公共 `ISSLContext`，只新增 FreePascal 私有 optional context interface 存放 revocation material，并让连接层把材料传给现有 `TSSLCertificateChainVerifier`。chain verifier 使用已有纯 Pascal `TX509CRL` parser 做 bounded CRL-backed revocation check：有匹配且有效的 CRL 时给出 good / revoked truth；没有材料、没有匹配 issuer、CRL 过期或不可解析时，继续 fail-closed 为 unavailable，而不是静默通过。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalContext`, `TFreePascalConnection`, `TSSLCertificateChainVerifier`, `TX509CRL`, focused TLS 1.3 runtime test harness, file-based working memory.

---

## Scope

- 收这 5 件事：
  1. FreePascal 私有 revocation material context seam
  2. CRL PEM fixture
  3. focused RED/GREEN runtime tests
  4. chain verifier 的 CRL-backed revocation truth
  5. ledgers closeout
- 明确不做：
  - 公共 `ISSLContext` API 扩展
  - browser-grade revocation stack
  - online CRL 下载 / 自动刷新
  - CRL signature cryptographic verification 扩面

## Task 1: RED - runtime contracts for caller-provided CRL material

**Files:**
- Modify: `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- Create: `tests/certificate/test_certs/revocation_revoked_crl.pem`
- Create: `tests/certificate/test_certs/revocation_nonmatching_crl.pem`

**Step 1: 扩 focused runtime cases**

- 在现有 `cert_verify_flags` runtime harness 上新增：
  - caller-provided CRL material 存在且 serial 不命中时，`sslCertVerifyCheckCRL` 可以放行
  - caller-provided CRL material 命中 leaf serial 时，连接必须 fail-closed，并 surface revoked truth
- 测试直接要求 FreePascal context 暴露 backend-private revocation material interface。

**Step 2: 跑 RED**

Run:

```bash
mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime
```

Expected:
- FAIL，失败点指向 revocation material interface 缺失或 CRL material 尚未接入 runtime trust path。

## Task 2: GREEN - add FreePascal-private revocation material seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: 增加 optional context interface**

- 只给 FreePascal backend 内部使用：
  - 写入 CRL PEM / file
  - 清空材料
  - 构建只读材料副本给连接层

**Step 2: 连接层把 CRL material 传给 verifier**

- `ValidateClientPeerCertificateTrust(...)` 在已有 trusted/intermediate store 之外，再把 caller-provided CRL material 传进 chain verifier。
- 保持 verify-mode / resumed-session gate 不变。

## Task 3: GREEN - make chain verifier consume CRL material

**Files:**
- Modify: `src/fafafa.ssl.certchain.pas`

**Step 1: 用 `TX509CRL` 做 bounded CRL-backed revocation check**

- 对当前被验 leaf：
  - 解析 certificate DER，提取 issuer + serial
  - 遍历 caller-provided CRL material
  - 只接受 issuer 匹配且当前有效的 CRL
  - serial 命中 => revoked
  - 有有效匹配 CRL 但 serial 不命中 => good
  - 无材料 / 无匹配 issuer / CRL 过期 / parse 失败 => unavailable

**Step 2: richer result surface**

- `TChainVerifyResult` 补齐 revocation status。
- 连接层对 `revoked` 优先映射到 `sslErrCertificateRevoked`；其它 revocation material 失败继续保留为 `sslErrCertificateUntrusted` + 明确错误文本。

## Task 4: Verification and ledgers

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

Run:

```bash
mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime
```

```bash
mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id revocation_material_exec_20260411
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-11-freepascal-revocation-evidence-material-plumbing.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.certchain.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas tests/certificate/test_certs/revocation_revoked_crl.pem tests/certificate/test_certs/revocation_nonmatching_crl.pem task_plan.md findings.md progress.md
```

Expected:
- focused runtime PASS
- adjacent trust runtime PASS
- focused gate PASS
- compile gate PASS
- diff hygiene PASS

## Definition Of Done

- FreePascal client runtime path 能消费 caller-provided CRL material。
- `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 对 CRL-backed path 不再只能报 unavailable。
- 有效 CRL + non-revoked serial => PASS。
- 有效 CRL + revoked serial => fail-closed，并 surface revoked truth。
- ledgers 已记录 RED / GREEN / verification evidence。
