**Goal:** 把 OCSP / CT 指南补进顶层 `README.md` 与 `docs/README.md` 的高可见度导航区，让用户从仓库首页也能直接发现这两条当前主路径。

**Why This Batch:** 当前文档体系里：
- `OCSP_USAGE_GUIDE.md` 已对齐 FreePascal stapling + OpenSSL 在线 OCSP
- `CT_IMPLEMENTATION_GUIDE.md` 已对齐 FreePascal runtime CT + 底层 validator

但两个顶层入口仍有导航缺口：
- `README.md` 的文档表里没有 OCSP / CT 指南
- `docs/README.md` 的功能表里有 CT，但没有 OCSP

**Guardrails:**
- 这批只更新：
  - `README.md`
  - `docs/README.md`
- 只补入口，不改 guide 正文
- 不碰生产代码

---

## Task 1: Add README entrypoints

**Files:**
- Modify: `README.md`
- Modify: `docs/README.md`

**Step 1: Update the top-level README docs table**
- 补两条入口：
  - `OCSP 指南`
  - `CT 指南`
- 描述收紧到当前真实范围，不写成完整 revocation / 全 backend 完整支持

**Step 2: Update docs/README feature table**
- 在 CT 相邻位置补 `OCSP` 行
- 描述写成：
  - stapled response + 在线 OCSP 工作流

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "OCSP 指南|CT 指南|OCSP_USAGE_GUIDE|CT_IMPLEMENTATION_GUIDE|\\| OCSP \\|" README.md docs/README.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/README.md /home/dtamade/projects/fafafa.ssl/docs/README.md
```

```bash
git diff --check -- docs/plans/2026-04-09-readme-ocsp-ct-guide-entrypoints.md README.md docs/README.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `README.md` 的文档表已补上：
  - `OCSP 指南`
  - `CT 指南`
- `docs/README.md` 的功能表已补上：
  - `OCSP | Stapled response + 在线 OCSP 工作流`
- 这样仓库首页、docs 首页、Documentation Index 三个高可见度入口对 CT/OCSP guide 的可发现性已经更一致。

## Final Verification

- `rg -n "OCSP 指南|CT 指南|OCSP_USAGE_GUIDE|CT_IMPLEMENTATION_GUIDE|\\| OCSP \\|" README.md docs/README.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/README.md /home/dtamade/projects/fafafa.ssl/docs/README.md` => PASS
  - `README.md` => formatted
  - `docs/README.md` => formatted
- `git diff --check -- docs/plans/2026-04-09-readme-ocsp-ct-guide-entrypoints.md README.md docs/README.md task_plan.md findings.md progress.md` => PASS
