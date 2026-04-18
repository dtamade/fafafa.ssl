**Goal:** 更新 OCSP guide，让文档覆盖当前真实可用的 FreePascal client runtime OCSP stapling surface 与 `required` 失败策略，而不是只停留在 OpenSSL 在线 OCSP 工作流。

**Why This Batch:** 当前文档存在明显落差：
- `docs/guides/OCSP_USAGE_GUIDE.md` 仍然只面向 OpenSSL backend
- 但 FreePascal client 已经有：
  - `status_request` request path
  - `ISSLOCSPStapling` runtime surface
  - `WithOCSPStaplingRequired(...)` fail-closed gate

这会让 guide 和真实使用路径脱节：读者看不到连接对象上的 surface API，也不知道哪些是当前 FreePascal path 已经可用、哪些仍然是明确未覆盖范围。

**Guardrails:**
- 这批只更新 `docs/guides/OCSP_USAGE_GUIDE.md`
- 只写已经被代码和测试证明的 FreePascal runtime 行为
- 不把 FreePascal OCSP 说成完整 revocation parity
- 不顺手改 `API_DOCUMENTATION.md`、其他 guides、或生产代码

---

## Task 1: Reconfirm runtime truth and docs scope

**Files:**
- Reference: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `docs/plans/2026-04-09-freepascal-client-ocsp-stapling-surface-required-policy.md`

**Step 1: Lock what the guide may claim**
- 当前可以写进文档的 FreePascal runtime truth：
  - `WithOCSPStapling(True)` 会请求 `status_request`
  - 连接可通过 `ISSLOCSPStapling` surface 读取 raw stapled response / verification bit / status text
  - `WithOCSPStaplingRequired(True)` 在缺失或未通过当前有界校验的 stapled response 时 fail-closed
- 当前不能写成已支持的范围：
  - online AIA OCSP fetch parity
  - responder signature / issuer-chain cryptographic verification parity
  - server-side stapling issuance

---

## Task 2: Update the guide

**Files:**
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`

**Step 1: Lead with the shortest working path**
- 在文档开头补一个 FreePascal client runtime section：
  - builder 配置示例：`WithVerifyPeer` + `WithOCSPStapling(...)` + 可选 `WithOCSPStaplingRequired(...)`
  - connection surface 示例：`ISSLOCSPStapling`
  - 写清 optional / required 两种语义和当前 scope

**Step 2: Preserve OpenSSL online workflow**
- 保留现有 OpenSSL 在线 OCSP 内容，但把它明确写成“另一条路径”
- 避免让读者误以为 FreePascal client runtime 仍然只能走 OpenSSL OCSP API

---

## Task 3: Verification / Closeout

**Commands:**
```bash
rg -n "FreePascal client runtime|WithOCSPStaplingRequired|ISSLOCSPStapling|status_request|online AIA OCSP fetch|OpenSSL 在线 OCSP" docs/guides/OCSP_USAGE_GUIDE.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-guide-runtime-required.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md
```

---

## Execution Result

- 这批只更新了 `docs/guides/OCSP_USAGE_GUIDE.md`，没有碰生产代码：
  - 文档标题从 OpenSSL-only 视角改成双路径说明
  - 在开头新增 FreePascal client runtime OCSP stapling section
  - 给出 `WithOCSPStapling(...)` / `WithOCSPStaplingRequired(...)` + `ISSLOCSPStapling` 的用法示例
  - 写清 optional / required 的当前语义与未覆盖范围
  - 现有 OpenSSL 在线 OCSP 工作流保留为“另一条路径”

## Final Verification

- `rg -n "FreePascal client runtime|WithOCSPStaplingRequired|ISSLOCSPStapling|status_request|online AIA OCSP fetch|OpenSSL 在线 OCSP" docs/guides/OCSP_USAGE_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-guide-runtime-required.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md` => PASS
