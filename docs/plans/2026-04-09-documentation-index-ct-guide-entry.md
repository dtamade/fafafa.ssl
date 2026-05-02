**Goal:** 把 `docs/guides/CT_IMPLEMENTATION_GUIDE.md` 补进 `docs/DOCUMENTATION_INDEX.md`，让 CT guide 进入当前高可见度文档入口。

**Why This Batch:** 当前 CT guide 已经写实了 FreePascal runtime surface 与底层 validator 的双路径，但 `DOCUMENTATION_INDEX.md` 的“使用与集成”列表里只有：
- OCSP guide
- TS guide

却没有 CT guide。这样会让已经存在的 CT 主指南在入口层继续“隐身”。

**Guardrails:**
- 这批只更新 `docs/DOCUMENTATION_INDEX.md`
- 只补导航入口，不改 guide 正文
- 不碰生产代码

---

## Task 1: Add the CT guide entry

**Files:**
- Modify: `docs/DOCUMENTATION_INDEX.md`

**Step 1: Insert the guide in the usage/integration section**
- 在 OCSP / TS 一组相邻指南旁边补上：
  - `guides/CT_IMPLEMENTATION_GUIDE.md`
- 描述收紧到当前真实范围：
  - FreePascal runtime CT surface
  - 底层 validator API

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "CT_IMPLEMENTATION_GUIDE|CT 实现指南|OCSP 使用指南|TS 使用指南" docs/DOCUMENTATION_INDEX.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md
```

```bash
git diff --check -- docs/plans/2026-04-09-documentation-index-ct-guide-entry.md docs/DOCUMENTATION_INDEX.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `DOCUMENTATION_INDEX.md` 已经把 `CT_IMPLEMENTATION_GUIDE.md` 补进 “使用与集成” 区：
  - 现在 OCSP / CT / TS 三条相邻 guide 已经并列可见
  - CT 条目描述也收紧到当前真实范围：FreePascal runtime + 底层 validator

## Final Verification

- `rg -n "CT_IMPLEMENTATION_GUIDE|CT 实现指南|OCSP 使用指南|TS 使用指南" docs/DOCUMENTATION_INDEX.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/DOCUMENTATION_INDEX.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-documentation-index-ct-guide-entry.md docs/DOCUMENTATION_INDEX.md task_plan.md findings.md progress.md` => PASS
