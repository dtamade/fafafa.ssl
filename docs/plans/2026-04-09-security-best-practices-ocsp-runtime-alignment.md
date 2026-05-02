**Goal:** 更新 `security-best-practices` 里的 OCSP 建议，让它与当前 FreePascal client runtime stapling truth 对齐，而不是只给一个裸 `.WithOCSPStapling` 示例。

**Why This Batch:** 当前 `docs/guides/security-best-practices.md` 的 OCSP section 过于薄，容易产生误读：
- 只有 `.WithOCSPStapling` 的启用片段
- 没写 `WithVerifyPeer`
- 没写 `WithOCSPStaplingRequired(...)`
- 没写 `ISSLOCSPStapling`
- 没写“stapled response path 不等于完整在线 revocation parity”

这会让安全建议和当前真实 runtime 语义脱节。

**Guardrails:**
- 这批只更新 `docs/guides/security-best-practices.md`
- 只写已经被代码和测试证明的 FreePascal runtime 行为
- 不顺手改 `API_DOCUMENTATION.md`、其他 guides、或生产代码

---

## Task 1: Reconfirm the runtime and docs gap

**Files:**
- Reference: `docs/guides/OCSP_USAGE_GUIDE.md`
- Reference: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Lock the claims**
- 文档可以写：
  - `WithOCSPStapling(True)` 会请求 stapled OCSP response
  - `WithOCSPStaplingRequired(True)` 会在缺失或未通过当前有界校验时 fail-closed
  - `ISSLOCSPStapling` 可读 raw bytes / verified bit / status text
- 文档不能写：
  - online AIA OCSP fetch parity
  - responder signature / issuer-chain cryptographic verification parity
  - server-side stapling issuance

---

## Task 2: Update the security guidance

**Files:**
- Modify: `docs/guides/security-best-practices.md`

**Step 1: Tighten the OCSP section**
- 把当前裸 `.WithOCSPStapling` 示例改成：
  - `WithVerifyPeer`
  - `WithOCSPStapling(True)`
  - 可选 `WithOCSPStaplingRequired(True)` 的说明
- 补一句如何通过 `ISSLOCSPStapling` 读取状态

**Step 2: Keep the advice honest**
- 明确说明这里只覆盖 stapled response path
- 不把它写成完整在线 revocation strategy
- 安全检查清单里的 OCSP 项也同步收紧 wording

---

## Task 3: Verification / Closeout

**Commands:**
```bash
rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|stapled response|在线 revocation|VerifyPeer" docs/guides/security-best-practices.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md
```

```bash
git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-runtime-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md
```

---

## Execution Result

- 这批只更新了 `docs/guides/security-best-practices.md`，没有碰生产代码：
  - OCSP section 不再只是一个裸 `.WithOCSPStapling` 片段
  - 文档现在明确建议 `WithVerifyPeer` + `WithOCSPStapling(...)`
  - 追加了 `WithOCSPStaplingRequired(...)` 的风险分层说明
  - 给出 `ISSLOCSPStapling` 的最小状态读取示例
  - 同时写清这条路径只覆盖 stapled response，不等于完整在线 revocation strategy
  - 安全检查清单里的 OCSP 项也同步收紧为 client-path + risk-based wording

## Final Verification

- `rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|stapled OCSP response|在线 revocation|WithVerifyPeer|required" docs/guides/security-best-practices.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md` => PASS
- `git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-runtime-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md` => PASS
