**Goal:** 更新 `docs/reference/API_DOCUMENTATION.md` 的 CT section，把它从旧的通用 `TSCTValidator` 叙事收紧到当前 FreePascal client/runtime CT surface 与 `required` boundary truth。

**Why This Batch:** 当前 `API_DOCUMENTATION.md` 的 OCSP section 已经对齐到 runtime truth，但 CT section 还停在旧的低层 validator 示例：
- 没有写出 `WithCertificateTransparencyRequired(True)` builder 入口
- 没有写出 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` runtime surface
- 没有写出 `verify-peer` / `verify-none` / resumed 的 `required` boundary
- 容易让读者把 `TSCTValidator` 当成默认主路径，而不是底层 API

**Guardrails:**
- 这批只更新 `docs/reference/API_DOCUMENTATION.md`
- 只写 fresh code/tests 已证明的 CT runtime truth
- 不碰生产代码
- 不顺手扩到其他 guides/reference 文档

---

## Task 1: Add the missing builder/runtime contract

**Files:**
- Modify: `docs/reference/API_DOCUMENTATION.md`

**Step 1: Add builder entry**
- 在 builder 方法区补 `WithCertificateTransparencyRequired(...)`
- 说明收紧到：
  - 只影响当前 FreePascal client/runtime 的 required gate
  - `verify-peer` 的 non-resumed full-handshake 才会执行 fail-closed
  - `verify-none` / resumed path 不触发 required enforcement

**Step 2: Rewrite the CT section around runtime truth**
- 把 CT section 主叙事改成：
  - 优先使用 FreePascal client/runtime surface
  - 连接上通过 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` 读取状态
  - `WithCertificateTransparencyRequired(True)` 的 fail-closed 条件：
    - missing SCT
    - validation unavailable
    - policy failed
- 保留低层 `TSCTValidator` / policy enum，但降到“需要底层 API 时再用”

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|ISSLCertificateTransparencyValidation|verify-none|resumed|full-handshake|missing SCT|validation unavailable|policy failed|TSCTValidator" docs/reference/API_DOCUMENTATION.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md
```

```bash
git diff --check -- docs/plans/2026-04-09-api-documentation-ct-runtime-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `API_DOCUMENTATION.md` 已补上缺失的 builder/runtime contract：
  - 新增 `WithCertificateTransparencyRequired(...)` 方法条目
  - CT section 主叙事改成 FreePascal client/runtime surface
  - 低层 `TSCTValidator` 保留为次级路径，而不是默认主路径
- `required` boundary 也已经写回当前测试锁定的 truth：
  - `verify-peer` 的 non-resumed full-handshake 才执行 gate
  - `verify-none` / resumed path 不触发 required enforcement
  - fail-closed 条件收紧成 missing SCT / validation unavailable / policy failed

## Final Verification

- `rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|ISSLCertificateTransparencyValidation|verify-none|resumed|full-handshake|SCT list|validation 结果不可用|policy 不满足|TSCTValidator" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ct-runtime-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS
