**Goal:** 更新 `docs/guides/security-best-practices.md` 的 OCSP 建议，把 `required OCSP` 的 `verify-none` / resumed boundary 写实。

**Why This Batch:** 之前的 security guide 对齐已经补了 `WithVerifyPeer`、`WithOCSPStaplingRequired(...)` 和 `ISSLOCSPStapling`，但现在 fresh runtime truth 又更细了一层：
- `verify-none` 不触发 required fail-closed
- resumed TLS 1.3 path 不因缺少新的 stapled response 被 `required` 阻断

当前文案还没把这两个 boundary 写出来。

**Guardrails:**
- 这批只更新 `docs/guides/security-best-practices.md`
- 只写 fresh code/tests 已证明的 runtime 行为
- 不顺手改 `OCSP_USAGE_GUIDE.md` 以外的其他 reference / API 文档

---

## Task 1: Tighten the OCSP best-practice wording

**Files:**
- Modify: `docs/guides/security-best-practices.md`

**Step 1: Update the OCSP semantics**
- 在 OCSP section 里把 `WithOCSPStaplingRequired(True)` 的表述收紧成：
  - verify-peer 的 non-resumed full-handshake path 上，missing/unaccepted stapled response 才会 fail-closed
- 同时补上两个 boundary：
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 也跳过 required enforcement

**Step 2: Keep the surrounding advice stable**
- 保留：
  - `WithVerifyPeer`
  - `ISSLOCSPStapling`
  - “stapled response path != 完整在线 revocation strategy”
- 不新增新的能力承诺

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|verify-none|resumed|fail-closed|WithVerifyPeer" docs/guides/security-best-practices.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md
```

```bash
git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-required-boundary-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `docs/guides/security-best-practices.md` 已补齐 `required OCSP` 的两个 boundary：
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 不因缺少新的 stapled response 被 `required` 阻断
- 其余已对齐内容保持不变：
  - `WithVerifyPeer`
  - `ISSLOCSPStapling`
  - “stapled response path != 完整在线 revocation strategy”

## Final Verification

- `rg -n "WithOCSPStaplingRequired|ISSLOCSPStapling|verify-none|resumed|fail-closed|WithVerifyPeer" docs/guides/security-best-practices.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/security-best-practices.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-security-best-practices-ocsp-required-boundary-alignment.md docs/guides/security-best-practices.md task_plan.md findings.md progress.md` => PASS
