**Goal:** 修复 `docs/reference/API_DOCUMENTATION.md` 的“更多资源”断链，把它们改成当前仓库里实际存在的入口。

**Why This Batch:** 在刚完成 CT runtime alignment 后，`API_DOCUMENTATION.md` 的资源区还保留了 3 条旧路径：
- `docs/OCSP_PERFORMANCE_REPORT.md` 不存在
- `docs/CT_IMPLEMENTATION_GUIDE.md` 不存在
- `examples/` 对当前文件位置来说也是错误相对路径

这会直接破坏文档导航，即使正文已经对齐，读者点到资源区仍然会落到断链。

**Guardrails:**
- 这批只更新 `docs/reference/API_DOCUMENTATION.md`
- 只修实际断链，不改正文叙事
- 链接目标必须是仓库中现有文件/目录
- 不碰生产代码

---

## Task 1: Repair the resource links

**Files:**
- Modify: `docs/reference/API_DOCUMENTATION.md`

**Step 1: Point each resource at an existing target**
- 把 “性能测试报告” 改成更诚实的当前入口：
  - `OCSP 模块测试报告` -> `../test_reports/P2_OCSP_MODULE_REPORT.md`
- 把 `CT 实现指南` 改成当前真实路径：
  - `../guides/CT_IMPLEMENTATION_GUIDE.md`
- 把 `示例代码` 改成当前真实相对路径：
  - `../../examples/`

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "\]\((docs/|examples/)|P2_OCSP_MODULE_REPORT|CT_IMPLEMENTATION_GUIDE|../../examples/)" docs/reference/API_DOCUMENTATION.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md
```

```bash
git diff --check -- docs/plans/2026-04-09-api-documentation-resource-link-repair.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `API_DOCUMENTATION.md` 的“更多资源”区已修正为现有入口：
  - `OCSP 模块测试报告` -> `../test_reports/P2_OCSP_MODULE_REPORT.md`
  - `CT 实现指南` -> `../guides/CT_IMPLEMENTATION_GUIDE.md`
  - `示例代码` -> `../../examples/`
- 初次验证时组合 `rg` 正则写错，随后改成固定字符串检查，避免重复在同一个坏正则上浪费时间。

## Final Verification

- 固定字符串检查 => PASS
  - 旧坏路径未再命中
  - 新路径命中：
    - `../test_reports/P2_OCSP_MODULE_REPORT.md`
    - `../guides/CT_IMPLEMENTATION_GUIDE.md`
    - `../../examples/`
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-resource-link-repair.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS
