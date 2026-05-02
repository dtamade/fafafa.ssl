**Goal:** 收紧 `docs/DOCUMENTATION_INDEX.md` 与 `docs/MIGRATION_GUIDE_V1.1.md` 的 OCSP / capability wording，让入口描述与示例代码都对齐当前 FreePascal capability truth。

**Why This Batch:** 当前 OCSP required boundary 文档已经对齐，但还有两处高可见度 drift：
- `docs/DOCUMENTATION_INDEX.md` 仍把 `OCSP_USAGE_GUIDE.md` 描述成 “OpenSSL” 指南，和当前 guide 已覆盖的 FreePascal runtime stapling path 不一致。
- `docs/MIGRATION_GUIDE_V1.1.md` 的功能成熟度示例仍把 `CertTransparencySupport` 当作 deprecated 示例，和当前 FreePascal capability truth（CT/OCSP 都是 experimental/usable，不是 deprecated）不一致。

**Guardrails:**
- 这批只更新两份文档：
  - `docs/DOCUMENTATION_INDEX.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
- 不改生产代码
- 不顺手扩到其他 docs/reference/guides
- 只写当前代码与 capability tests 已证明的 truth

---

## Task 1: Tighten the OCSP guide entrypoint description

**Files:**
- Modify: `docs/DOCUMENTATION_INDEX.md`

**Step 1: Update the guide summary**
- 把 `OCSP_USAGE_GUIDE.md` 的条目从 “OpenSSL” 单一路径改成当前真实范围：
  - FreePascal runtime stapling path
  - OpenSSL 在线 OCSP workflow

## Task 2: Tighten the migration example wording

**Files:**
- Modify: `docs/MIGRATION_GUIDE_V1.1.md`

**Step 1: Keep the capability example truthful**
- 保留 “stable / usable / deprecated” 三类 helper 的示例结构
- 但不要再把 `CertTransparencySupport` 写成 deprecated 示例
- 改成更诚实的 deprecated 示例：
  - `RenegotiationSupport`
  - 输出 wording 改成 backend-conditional 的弃用提示
- `OCSPStaplingSupport` 的 usable 示例也收紧到和 capability guide 一致的 wording

## Task 3: Verify and close out

**Commands:**
```bash
rg -n "OCSP 使用指南|OpenSSL|FreePascal|OCSPStaplingSupport|RenegotiationSupport|deprecated" docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md /home/dtamade/projects/fafafa.ssl/docs/MIGRATION_GUIDE_V1.1.md
```

```bash
git diff --check -- docs/plans/2026-04-09-ocsp-doc-entrypoint-and-migration-wording-alignment.md docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `docs/DOCUMENTATION_INDEX.md` 已经不再把 `OCSP_USAGE_GUIDE.md` 描述成 OpenSSL-only，而是写回当前双路径：
  - FreePascal runtime stapling
  - OpenSSL 在线 OCSP
- `docs/MIGRATION_GUIDE_V1.1.md` 的功能成熟度示例也已收紧到当前 capability truth：
  - `OCSPStaplingSupport` 的 usable 示例改成和 capability guide 一致的 wording
  - deprecated 示例改成 `RenegotiationSupport`
  - 不再把 `CertTransparencySupport` 写成 deprecated

## Final Verification

- `rg -n "OCSP 使用指南|OpenSSL|FreePascal|OCSPStaplingSupport|RenegotiationSupport|deprecated" docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md /home/dtamade/projects/fafafa.ssl/docs/MIGRATION_GUIDE_V1.1.md` => PASS
  - result:
    - `docs/DOCUMENTATION_INDEX.md` => `unchanged`
    - `docs/MIGRATION_GUIDE_V1.1.md` => formatted
- `git diff --check -- docs/plans/2026-04-09-ocsp-doc-entrypoint-and-migration-wording-alignment.md docs/DOCUMENTATION_INDEX.md docs/MIGRATION_GUIDE_V1.1.md task_plan.md findings.md progress.md` => PASS
