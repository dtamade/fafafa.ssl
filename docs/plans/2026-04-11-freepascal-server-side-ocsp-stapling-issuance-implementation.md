# FreePascal Server-Side OCSP Stapling Issuance Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 给 FreePascal TLS 1.3 server accept path 增加有界的 OCSP stapling issuance：只发送 caller-provided DER stapled response，并在客户端请求 `status_request` 时把它带进 leaf `CertificateEntry`。

**Architecture:** 这批继续保持 bounded design。server context 通过一个 FreePascal backend 私有可选接口持有 stapled OCSP response bytes；`TFreePascalConnection.DoAccept` 只在 full handshake、client 明确请求 `status_request`、且 context 持有 stapled response 时，把 OCSP response 挂到 leaf certificate entry。测试先用离线 scripted client 驱动真实 server `DoAccept` 做 RED，再做最小实现，最后决定把 focused runtime 纳入 completeness gate。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalContext`, `TFreePascalConnection`, `fafafa.ssl.tls13.servercertificate`, TLS 1.3 offline scripted handshake tests, bash gate contract, file-based working memory.

---

## Scope

- 收这 5 件事：
  1. 新增 server-side stapling runtime RED contract
  2. 增加 FreePascal backend 私有 server stapling material seam
  3. 让 server `CertificateEntry` 在请求 + 配置同时满足时带出 stapled OCSP response
  4. focused runtime 稳定后纳入 completeness gate
  5. 回填 ledger / next-stage closeout
- 明确不做：
  - online OCSP fetch / responder
  - OCSP cache / refresh policy
  - broader client-side revocation / CT 改动

## Task 1: RED - Add server-side stapling issuance runtime contract

**Files:**
- Create: `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.tls13.servercertificate.pas`

**Step 1: Build a scripted client that drives real `DoAccept`**

- 复用 `tests/test_freepascal_server_session_resumption.pas` 的 offline client 模式：
  - client 发送真实 TLS 1.3 `ClientHello`
  - 解密 server handshake flight
  - 解析 `Certificate` 消息
  - 回送 client `Finished`
- 观测面至少保存：
  - 是否看到了 `Certificate`
  - `HasLeafOCSPStapledResponse`
  - `LeafOCSPStapledResponse`

**Step 2: Write the failing tests**

- 新增 3 个 focused contract：
  - configured stapled response + client requested `status_request` => leaf stapled response must be emitted
  - configured stapled response + client did not request `status_request` => leaf stapled response must stay absent
  - no configured stapled response + client requested `status_request` => leaf stapled response must stay absent

**Step 3: Run RED**

Run:

```bash
mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime
```

Expected:
- FAIL，失败点指向真实 server accept path 还不会发出 stapled response。

## Task 2: GREEN - Add bounded server stapling material seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Add a backend-private optional interface**

- 新增 `IFreePascalContextServerStaplingMaterial`
- 只提供最小能力：
  - clear
  - set bytes
  - load file
  - has/get bytes

**Step 2: Store stapled-response bytes on the context**

- `TFreePascalContext` 新增 server stapled-response 存储字段
- 保持和现有 certificate/private-key/raw material seam 一样的 backend-private 范围
- 不扩全局 `ISSLContext` 标准接口

## Task 3: GREEN - Emit stapled response on the server certificate flight

**Files:**
- Modify: `src/fafafa.ssl.tls13.clienthello.parser.pas`
- Modify: `src/fafafa.ssl.tls13.servercertificate.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Detect whether the client requested `status_request`**

- 让 server accept path 能判断 ClientHello 是否带了 `status_request`
- 只要能稳定区分 requested / not requested 即可；不要顺手扩更多 extension policy

**Step 2: Add a bounded certificate-handshake builder for stapled OCSP**

- 在 `fafafa.ssl.tls13.servercertificate` 增加生产用 helper：
  - 仅给 leaf `CertificateEntry` 附加 `status_request`
  - body 使用 `status_type=ocsp + OCSPResponse<1..2^24-1>`
- 旧无扩展 builder 保持兼容

**Step 3: Rewire `DoAccept`**

- 只在这些条件同时成立时发 stapled response：
  - full handshake
  - client requested `status_request`
  - context 持有 stapled OCSP response bytes
- 不命中条件时保持现有 `Certificate` 消息不变

**Step 4: Run GREEN**

Run Task 1 command again.

Expected:
- PASS。

## Task 4: Gate integration

**Files:**
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

**Step 1: Add the new focused runtime to the gate**

- 在 server-side runtime 足够轻量、稳定后，把 `tests/test_freepascal_server_ocsp_stapling_runtime.pas` 纳入 focused gate
- 放在 fast revocation / client validation lanes 附近即可，不打散现有主序

**Step 2: Tighten the shell contract**

- dry-run 必须提到 `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- fake `fpc` 调用次数按新增测试提升
- summary report 必须出现新 runtime PASS row

**Step 3: Run contract verification**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --dry-run --fast-local --run-id fp_server_stapling_dryrun_20260411
```

Expected:
- PASS。

## Task 5: Final verification and ledger write-back

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused verification**

Run:

```bash
mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime
```

```bash
mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime
```

**Step 2: Compile gate**

Run:

```bash
python3 scripts/compile_all_modules.py
```

**Step 3: Full focused gate**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_server_stapling_exec_20260411
```

**Step 4: Diff hygiene**

Run:

```bash
git diff --check -- docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-implementation.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.tls13.clienthello.parser.pas src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_server_ocsp_stapling_runtime.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md
```

**Step 5: Write back ledgers**

- `task_plan.md` 顶部新增本轮批次
- `findings.md` 顶部记录 server-side stapling issuance 的真实边界与 request-gating 决策
- `progress.md` 顶部记录 RED/GREEN/验证命令与结果

## Definition Of Done

- FreePascal server path 能在 caller 配置 stapled response 且 client 请求 `status_request` 时发出 stapled OCSP response。
- 没配置或未请求时，`CertificateEntry` 保持无 stapled response。
- 新增 focused runtime 为绿，client stapling runtime 未回归。
- 如已接入 gate，则 gate contract / focused gate / compile gate / diff hygiene 全绿。
