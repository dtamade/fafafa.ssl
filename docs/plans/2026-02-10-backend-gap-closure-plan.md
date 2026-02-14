# Backend Gap Closure (Code-First, No CI/DI) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 关闭 TLS 后端与能力层的高风险未完成项，使核心 API（能力反序列化、证书轮转、TLS1.3 关键路径）达到“可用且可验证”。

**Architecture:** 采用“P0 → P1 → P2”分层收敛：先修复会抛异常/返回假阳性的功能，再补协议能力缺口，最后清理测试占位债务。每个任务严格执行 TDD 小步快跑（先失败测试，再最小实现，再回归验证）。

**Tech Stack:** FreePascal (ObjFPC), fafafa.ssl 多后端架构（OpenSSL/WolfSSL/MbedTLS/FreePascal）, 现有 `tests/` 程序级测试。

---

## Priority Order

1. **P0:** 运行正确性与异常风险（serializer / cert rotation / wolfssl metadata）
2. **P1:** 协议能力缺口（TLS1.3 AES-GCM 与 SHA384 key schedule / backend API parity）
3. **P2:** 测试占位清理（enterprise TODO 与 skip 降噪）

---

### Task 1 (P0): Lock serializer failures with failing tests

**Files:**
- Modify: `tests/test_capability_serialization.pas`
- Create: `tests/test_capability_deserialization_roundtrip.pas`
- Test Target: `src/fafafa.ssl.capability.serializer.pas`

**Step 1: Write failing test**
- 在 `test_capability_deserialization_roundtrip.pas` 添加：
  - JSON round-trip：`CapabilitiesToJSON -> JSONToCapabilities`
  - XML round-trip：`CapabilitiesToXML -> XMLToCapabilities`
  - 断言关键字段（`BackendType`, `SupportsTLS13`, `KnownIssues`）一致。

**Step 2: Run test to verify failure**
- Run:
  - `fpc -Fu./src -Fu./src/openssl tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_deser`
  - `./tmp/test_cap_deser`
- Expected: FAIL，异常包含 `not implemented yet`。

**Step 3: Write minimal implementation hooks**
- 在 `src/fafafa.ssl.capability.serializer.pas` 预留解析入口与字段映射函数（不一次性写全）。

**Step 4: Run test to verify still failing with narrower error**
- Run 同 Step 2。
- Expected: 从“直接 not implemented”变为“字段解析失败/未映射字段”级别失败。

**Step 5: Commit**
- `git add tests/test_capability_deserialization_roundtrip.pas tests/test_capability_serialization.pas src/fafafa.ssl.capability.serializer.pas`
- `git commit -m "test: add failing roundtrip tests for capability deserialization"`

**Acceptance Criteria:**
- 新测试稳定重现当前缺口，失败原因准确定位到字段解析实现。

---

### Task 2 (P0): Implement JSON/XML capability deserialization

**Files:**
- Modify: `src/fafafa.ssl.capability.serializer.pas`
- Modify: `tests/test_capability_deserialization_roundtrip.pas`

**Step 1: Implement JSONToCapabilities minimally**
- 支持当前 `CapabilitiesToJSON` 输出字段集合，不做扩展解析。
- 未识别字段忽略（前向兼容）。

**Step 2: Run JSON round-trip test**
- Run: `./tmp/test_cap_deser`
- Expected: JSON round-trip PASS，XML 仍可能 FAIL。

**Step 3: Implement XMLToCapabilities minimally**
- 支持当前 `CapabilitiesToXML` 输出字段集合。

**Step 4: Run full deserialization tests**
- Run:
  - `fpc -Fu./src -Fu./src/openssl tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_deser`
  - `./tmp/test_cap_deser`
- Expected: PASS。

**Step 5: Commit**
- `git add src/fafafa.ssl.capability.serializer.pas tests/test_capability_deserialization_roundtrip.pas`
- `git commit -m "feat: implement capability JSON/XML deserialization"`

**Acceptance Criteria:**
- 不再抛出 `JSON/XML deserialization not implemented yet`。
- 关键字段 round-trip 一致。

---

### Task 3 (P0): Replace certificate rotation placeholder with real expiry logic

**Files:**
- Modify: `src/fafafa.ssl.cert.rotation.pas`
- Create: `tests/test_cert_rotation_expiry_check.pas`

**Step 1: Write failing test**
- 使用已知证书样本，校验 `CheckExpiry` 返回值与 `DaysRemaining` 随真实 `NotAfter` 变化。
- 增加“证书文件不存在”失败断言。

**Step 2: Run test to verify failure**
- Run:
  - `fpc -Fu./src -Fu./src/openssl tests/test_cert_rotation_expiry_check.pas -otmp/test_cert_rotation`
  - `./tmp/test_cert_rotation`
- Expected: FAIL（当前实现固定 `True/90`）。

**Step 3: Implement minimal real check**
- 在 `CheckCertificateExpiry` 中加载证书并读取 `NotAfter`。
- 计算 `DaysBetween(Now, NotAfter)`，按结果返回成功/失败与天数。

**Step 4: Re-run test**
- Run 同 Step 2。
- Expected: PASS。

**Step 5: Commit**
- `git add src/fafafa.ssl.cert.rotation.pas tests/test_cert_rotation_expiry_check.pas`
- `git commit -m "fix: use real certificate expiry in rotation manager"`

**Acceptance Criteria:**
- 删除固定 90 天占位逻辑。
- 过期证书不会被误判为有效。

---

### Task 4 (P0): Hardening WolfSSL certificate/session metadata correctness

**Files:**
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas`
- Modify: `src/fafafa.ssl.wolfssl.session.pas`
- Modify: `tests/test_wolfssl_framework.pas`
- Create: `tests/wolfssl/test_wolfssl_metadata_accuracy.pas`

**Step 1: Write failing tests**
- 指纹不能为空（至少 SHA256）。
- `NotBefore/NotAfter` 不应退化为 `Now±365` 默认值。
- 会话 `ProtocolVersion/CipherName` 应从真实连接提取（若 API 可用）。

**Step 2: Run failing tests**
- Run:
  - `fpc -Fu./src -Fu./src/wolfssl tests/wolfssl/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_meta`
  - `./tmp/test_wolfssl_meta`
- Expected: FAIL。

**Step 3: Implement minimal metadata extraction**
- 优先使用 wolfSSL 可用 API；不可用时返回显式 `unknown`/错误码，不返回误导性默认值。

**Step 4: Re-run tests**
- Run 同 Step 2。
- Expected: PASS 或按后端能力明确 SKIP（非 silent fallback）。

**Step 5: Commit**
- `git add src/fafafa.ssl.wolfssl.certificate.pas src/fafafa.ssl.wolfssl.session.pas tests/wolfssl/test_wolfssl_metadata_accuracy.pas tests/test_wolfssl_framework.pas`
- `git commit -m "fix: improve wolfssl certificate/session metadata accuracy"`

**Acceptance Criteria:**
- 核心元数据不再依赖误导性默认值。
- 测试输出能区分“能力缺失”与“实现错误”。

---

### Task 5 (P1): Enable TLS1.3 AES-GCM in pure TLS13 AEAD path

**Files:**
- Modify: `src/fafafa.ssl.tls13.aead.pas`
- Modify: `tests/test_tls13_aead.pas`

**Step 1: Convert existing “unsupported expected” tests to failing success tests**
- 将 AES-GCM case 从“应失败”改为“应成功并可解密还原”。

**Step 2: Run test to confirm failure**
- Run:
  - `fpc -Fu./src -Fu./src/openssl tests/test_tls13_aead.pas -otmp/test_tls13_aead`
  - `./tmp/test_tls13_aead`
- Expected: FAIL（当前 AES 分支返回 unsupported）。

**Step 3: Implement AES-128/256-GCM encrypt/decrypt**
- 在 AEAD 路径按 cipher suite 选择 key size。
- 确保 tag 验证失败时返回明确错误。

**Step 4: Re-run test**
- Run 同 Step 2。
- Expected: PASS（CHACHA + AES 两路径均通过）。

**Step 5: Commit**
- `git add src/fafafa.ssl.tls13.aead.pas tests/test_tls13_aead.pas`
- `git commit -m "feat: add tls13 aes-gcm support in pure aead path"`

**Acceptance Criteria:**
- TLS13 AEAD 不再对 AES-GCM 直接返回 unsupported。

---

### Task 6 (P1): Implement SHA384 TLS1.3 key schedule/app schedule path

**Files:**
- Modify: `src/fafafa.ssl.tls13.keyschedule.pas`
- Modify: `src/fafafa.ssl.tls13.appschedule.pas`
- Modify: `tests/test_tls13_keyschedule.pas`
- Modify: `tests/test_tls13_appschedule.pas`

**Step 1: Rewrite SHA384 rejection tests to failing success tests**
- 把 `TLS_AES_256_GCM_SHA384` 从“应拒绝”改为“应派生成功”。

**Step 2: Run tests to confirm failure**
- Run:
  - `fpc -Fu./src tests/test_tls13_keyschedule.pas -otmp/test_tls13_keys`
  - `./tmp/test_tls13_keys`
  - `fpc -Fu./src tests/test_tls13_appschedule.pas -otmp/test_tls13_app`
  - `./tmp/test_tls13_app`
- Expected: FAIL（当前提示 not implemented）。

**Step 3: Implement SHA384 derivation branch**
- 扩展 hash size/key length/transcript hash 逻辑到 SHA384。

**Step 4: Re-run tests**
- Run 同 Step 2。
- Expected: PASS。

**Step 5: Commit**
- `git add src/fafafa.ssl.tls13.keyschedule.pas src/fafafa.ssl.tls13.appschedule.pas tests/test_tls13_keyschedule.pas tests/test_tls13_appschedule.pas`
- `git commit -m "feat: add tls13 sha384 key schedule support"`

**Acceptance Criteria:**
- SHA384 套件不再命中 “not implemented yet”。

---

### Task 7 (P1): FreePascal backend capability parity (cert/store + cipher negotiation)

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_freepascal_server_accept_skeleton.pas`

**Step 1: Add failing tests for certificate/store creation and AES suite negotiation**
- `CreateCertificate/CreateCertificateStore` 不应直接 unsupported。
- server accept 不应硬编码仅 CHACHA 交集。

**Step 2: Run tests to confirm failure**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: FAIL。

**Step 3: Implement minimal parity**
- 提供可用的 certificate/store 创建路径（先对接已有抽象实现）。
- 协商逻辑允许 AES/CHACHA 按优先级选择。

**Step 4: Re-run tests**
- Run 同 Step 2。
- Expected: PASS。

**Step 5: Commit**
- `git add src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_backend_basic.pas tests/test_freepascal_server_accept_skeleton.pas`
- `git commit -m "feat: improve freepascal backend parity for cert/store and cipher negotiation"`

**Acceptance Criteria:**
- FreePascal backend 从 skeleton 向可用实现推进，关键 API 不再立即 unsupported。

---

### Task 8 (P2): Reduce enterprise test placeholders into executable assertions

**Files:**
- Modify: `tests/connection/test_ssl_enterprise.pas`
- Modify: `tests/certificate/test_x509_enterprise.pas`
- Modify: `tests/test_stream_connection.pas`

**Step 1: Pick first TODO batch (<=5 assertions)**
- 将纯输出 TODO 转为可执行断言（优先证书解析/握手基础路径）。

**Step 2: Run targeted tests**
- Run:
  - `fpc -Fu./src -Fu./src/openssl tests/connection/test_ssl_enterprise.pas -otmp/test_ssl_enterprise`
  - `fpc -Fu./src -Fu./src/openssl tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise`
- Expected: 部分 FAIL（新断言生效）。

**Step 3: Implement/adjust minimal support or clear skip contracts**
- 优先将“可实现项”改为 PASS。
- 对“后端暂不支持项”使用统一、可统计的 skip contract。

**Step 4: Re-run targeted tests**
- Expected: 新增断言稳定 PASS/可解释 SKIP。

**Step 5: Commit**
- `git add tests/connection/test_ssl_enterprise.pas tests/certificate/test_x509_enterprise.pas tests/test_stream_connection.pas`
- `git commit -m "test: convert enterprise placeholders into executable assertions"`

**Acceptance Criteria:**
- TODO 输出数量显著下降；测试结果可用于真实回归决策。

---

## Execution Notes
- 严格遵循：不写脚本、不改 CI/DI。
- 每个任务只改最小文件集合。
- 每完成一个任务即执行对应测试，不跨任务累计风险。

## Suggested Immediate Start
- 从 **Task 1 + Task 2 + Task 3** 开始（P0 闭环），预计可最快把“运行时异常 + 错误成功判定”风险降到可控范围。
