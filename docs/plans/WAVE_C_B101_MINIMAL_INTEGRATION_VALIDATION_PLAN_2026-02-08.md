# Wave C B101 Minimal Integration Validation Plan（2026-02-08）

## 目标

将 B99/B100 的 cert verify cache 性能信号，转换为“业务链路可验证”的最小接入验证计划，且不破坏既有回归门禁。

---

## 候选接入点（源码定位）

- `src/fafafa.ssl.openssl.connection.pas:853`
  - `X509_STORE_CTX_init` + `X509_verify_cert` 链路（OCSP 响应验证路径）。
- `src/fafafa.ssl.openssl.connection.pas:1400`
  - `VerifyCertificateOCSP` 中的证书链构建与 `X509_verify_cert`。

说明：当前缓存单元 `src/fafafa.ssl.cert.verify.cache.pas` 尚未在上述业务链路出现直接调用。

---

## 最小验证步骤

### Step 1 — 基线快照（未接入前）

```bash
bash scripts/run_wave_c_b101_validation_playbook.sh \
  --run-id <RUN_ID> \
  --strict \
  --output test-reports/wave_c_b101_validation_<RUN_ID>.md
```

### Step 2 — 业务链路最小接入（后续 B102 执行）

- 在不改变外部 API 的前提下，增加“可开关”的缓存命中路径（建议仅包裹 `X509_verify_cert` 的结果读取与写回）。
- 先限定在单一路径，避免双点同时改动造成排障困难。

### Step 3 — 回归 + 性能复核

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
bash scripts/run_wave_c_b101_validation_playbook.sh --run-id <RUN_ID> --strict --full-gate
```

---

## 验收标准

1. 回归门禁无新增失败（compile/modules 均通过）。
2. `wave_c_b101_validation_<run_id>.md` 中 benchmark 仍显示正向 speedup（>1x）。
3. 若引入开关，默认行为与当前版本一致（可通过配置开启优化路径）。

---

## 风险与缓解

- 风险：证书缓存误命中导致验证语义偏差。
  - 缓解：缓存 key 必须绑定证书指纹 + TTL，失败结果同样记录并受 TTL 管控。
- 风险：在 OCSP/链路分支双点同时接入时定位困难。
  - 缓解：B102 仅选择单点接入，另一路径保留原行为作为对照。
