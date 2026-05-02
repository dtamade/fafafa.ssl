**Goal:** 更新 `docs/reference/API_DOCUMENTATION.md` 里的 OCSP 相关条目，让 API reference 与当前 runtime truth 对齐，去掉过头的 server-side 自动 stapling 表述。

**Why This Batch:** `API_DOCUMENTATION.md` 目前有几类明显 drift：
- 把 `WithOCSPStapling(True)` 的 server-side 语义写成“自动获取和附加响应”
- `OCSP Stapling` 主节没有写当前 FreePascal client/runtime 边界
- 最佳实践和排障段落也还停在旧模型

这比普通 guide 风险更高，因为它是 reference 文档，读者容易把里面的例子当作准确 contract。

**Guardrails:**
- 这批只更新 `docs/reference/API_DOCUMENTATION.md`
- 只写已经被代码和测试证明的行为
- 不把 server-side stapling issuance 写成通用保证
- 不改生产代码，不顺手扩其他 reference docs

---

## Task 1: Reconfirm the true public surface

**Files:**
- Reference: `src/fafafa.ssl.base.pas`
- Reference: `src/fafafa.ssl.connection.base.pas`
- Reference: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `docs/guides/OCSP_USAGE_GUIDE.md`

**Step 1: Lock the claims**
- 文档可以写：
  - `WithOCSPStapling(True)` / `WithOCSPStaplingRequired(True)` 是公开 builder API
  - 连接层公开了 `GetOCSP*` 方法，并且也可通过 `ISSLOCSPStapling` 访问
  - 当前最可验证的主路径是 client-side stapled-response request/consume + optional/required semantics
- 文档不能写：
  - `WithOCSPStapling(True)` 在所有 server/backend 上都会自动获取并附加响应
  - 当前已经有完整在线 revocation parity

---

## Task 2: Update the OCSP sections

**Files:**
- Modify: `docs/reference/API_DOCUMENTATION.md`

**Step 1: Tighten builder docs and main OCSP section**
- 更新 `WithOCSPStapling` / `WithOCSPStaplingRequired` 条目
- 更新 `OCSP Stapling` 主节：
  - 先写 client-side runtime path
  - 保留 `GetOCSP*` 访问方式
  - 增加 `ISSLOCSPStapling` capability-gated 示例
  - 把 server-side 说明收紧成 backend-specific caveat

**Step 2: Tighten best practices and troubleshooting**
- 最佳实践里的 client config 增加 risk-based `required` 语义
- 服务端示例不再写“自动获取 OCSP 响应”
- 排障段改成更符合当前 surface 的表述

---

## Task 3: Verification / Closeout

**Commands:**
```bash
rg -n "ISSLOCSPStapling|WithOCSPStaplingRequired|backend-specific|自动获取 OCSP 响应|完整在线 revocation|GetOCSPResponseStatus" docs/reference/API_DOCUMENTATION.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md
```

```bash
git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-runtime-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md
```

---

## Execution Result

- 这批只更新了 `docs/reference/API_DOCUMENTATION.md`，没有碰生产代码：
  - `WithOCSPStapling` / `WithOCSPStaplingRequired` 条目补上了当前 client/runtime truth 和边界
  - `OCSP Stapling` 主节改成先写 client-side request/consume path
  - 保留 `GetOCSP*` public methods，同时补了 `ISSLOCSPStapling` 的 capability-gated 示例
  - server-side 说明收紧成 backend-specific caveat，不再宣称“自动获取和附加响应”
  - best practices 与 troubleshooting 也同步改成更贴近当前语义的 wording

## Final Verification

- `rg -n "ISSLOCSPStapling|WithOCSPStaplingRequired|backend-specific|自动获取 OCSP 响应|完整在线 revocation|GetOCSPResponseStatus" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-runtime-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS
