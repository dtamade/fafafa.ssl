**Goal:** 更新 `docs/reference/API_DOCUMENTATION.md` 的 OCSP section，把 `required OCSP` 的 `verify-none` / resumed boundary 写实。

**Why This Batch:** 当前 API reference 已经比旧版更接近 runtime truth，但 `WithOCSPStaplingRequired(True)` 这一条仍然写成一个无条件 fail-closed 结论，没有把刚落地的两个 boundary 写进去：
- `verify-none` 不触发 required fail-closed
- resumed TLS 1.3 path 不因缺少新的 stapled response 被 `required` 阻断

**Guardrails:**
- 这批只更新 `docs/reference/API_DOCUMENTATION.md`
- 只写 fresh code/tests 已证明的 runtime 行为
- 不顺手改其他 reference/guides
- 不碰生产代码

---

## Task 1: Tighten the OCSP reference wording

**Files:**
- Modify: `docs/reference/API_DOCUMENTATION.md`

**Step 1: Update the client/runtime wording**
- 在 OCSP client config section 把 `WithOCSPStaplingRequired(True)` 的说明收紧成：
  - verify-peer 的 non-resumed full-handshake path 上，missing/unaccepted stapled response 才会 fail-closed
- 同时补上两个 boundary：
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 也跳过 required enforcement

**Step 2: Keep the rest of the section stable**
- 保留：
  - `WithOCSPStapling(True)` request path
  - client/runtime wording
  - server-side backend-specific caveat
- 不新增新的能力承诺

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "WithOCSPStaplingRequired|verify-none|resumed|fail-closed|stapled response|client/runtime path" docs/reference/API_DOCUMENTATION.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md
```

```bash
git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-required-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `docs/reference/API_DOCUMENTATION.md` 已补齐 `required OCSP` 的两个 boundary：
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 不因缺少新的 stapled response 被 `required` 阻断
- 同时把同一文档里剩余的两个旧 wording 一并收紧：
  - `WithOCSPStaplingRequired(...)` builder 条目
  - troubleshooting 里的握手失败说明
- 其余已对齐内容保持不变：
  - `WithOCSPStapling(True)` request path
  - client/runtime wording
  - server-side backend-specific caveat

## Final Verification

- `rg -n "WithOCSPStaplingRequired|verify-none|resumed|fail-closed|stapled response|client/runtime path|required enforcement" docs/reference/API_DOCUMENTATION.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/reference/API_DOCUMENTATION.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-api-documentation-ocsp-required-boundary-alignment.md docs/reference/API_DOCUMENTATION.md task_plan.md findings.md progress.md` => PASS
